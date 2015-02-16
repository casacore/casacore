//# LatticeUtilities.cc: defines the Lattice Utilities global functions//# Copyright (C) 1995,1996,1997,1999,2000,2001,2002,2003,2004
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This library is free software; you can redistribute it and/or modify it
//# under the terms of the GNU Library General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or (at your
//# option) any later version.
//#
//# This library is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
//# License for more details.
//#
//# You should have received a copy of the GNU Library General Public License
//# along with this library; if not, write to the Free Software Foundation,
//# Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

#ifndef LATTICES_LATTICEUTILITIES_TCC
#define LATTICES_LATTICEUTILITIES_TCC

#include <casacore/lattices/Lattices/LatticeUtilities.h>

#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/MaskedArray.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/lattices/Lattices/Lattice.h>
#include <casacore/lattices/Lattices/ArrayLattice.h>
#include <casacore/lattices/Lattices/ExtendLattice.h>
#include <casacore/lattices/Lattices/SubLattice.h>
#include <casacore/lattices/Lattices/MaskedLatticeIterator.h>
#include <casacore/lattices/Lattices/LatticeStepper.h>
#include <casacore/lattices/Lattices/MaskedLattice.h>
#include <casacore/lattices/LatticeMath/LatticeStatistics.h>
#include <casacore/lattices/Lattices/RebinLattice.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>

namespace casacore {  //# namespace casacore begin

// LatticeUtilities

template <class T>
void LatticeUtilities::copyDataAndMask(LogIO& os, MaskedLattice<T>& out,
                                       const MaskedLattice<T>& in, 
                                       Bool zeroMasked)
//
// This function coould be implemented with LEL
// but requires two passes if zeroMask=True so
// we leave it as it is
{  

// Do we need to stuff about with masks ?  Even if the input
// does not have a mask, it has a 'virtual' mask of all True.
// Therefore we need to transfer those mask values to the
// output if an output mask exists.

   Bool doMask = out.isMasked() && out.hasPixelMask();
   Lattice<Bool>* pMaskOut = 0;
   if (doMask) {
      pMaskOut = &out.pixelMask();
      if (!pMaskOut->isWritable()) {
         doMask = False;
         os << LogIO::WARN << "The output image has a mask but it is not writable" << endl;
         os << LogIO::WARN << "So the mask will not be transferred to the output" << LogIO::POST;
      }
   }                        
   if (!doMask) zeroMasked = False;

// Use the same stepper for input and output.
                      
   IPosition cursorShape = out.niceCursorShape(); 
   LatticeStepper stepper (out.shape(), cursorShape, LatticeStepper::RESIZE);

// Create input lattice iterator 

   RO_MaskedLatticeIterator<T> iter(in, stepper);
   for (iter.reset(); !iter.atEnd(); iter++) {

// Put the pixels

      IPosition cursorShape = iter.cursorShape();
      if (zeroMasked) {
         Array<T> pixels = iter.cursor().copy();
         const Array<Bool>& mask = iter.getMask();
//
         typename Array<Bool>::const_iterator mIt;
         typename Array<T>::iterator dIt;
	 typename Array<T>::iterator dItend = pixels.end();
         for (dIt=pixels.begin(),mIt=mask.begin(); dIt!=dItend; ++dIt,++mIt) {
            if (!(*mIt)) *dIt = 0.0;
         }
         out.putSlice(pixels, iter.position());
      } else {
         out.putSlice(iter.cursor(), iter.position());
      }

// Put the mask

      if (doMask) {
         pMaskOut->putSlice(iter.getMask(), iter.position());
      }
   }
}


template <class T>
void LatticeUtilities::replicate (Lattice<T>& lat,
                                  const Slicer& region,
                                  const Array<T>& pixels)
{
   SubLattice<T> subLattice(lat, region, True); 
   const IPosition shapePixels = pixels.shape();
   const IPosition shapeLattice = subLattice.shape();
   AlwaysAssert(shapePixels.nelements()<=shapeLattice.nelements(),AipsError);
//
   LatticeStepper stepper(shapeLattice, shapePixels, LatticeStepper::RESIZE);
   LatticeIterator<T> iter(subLattice, stepper);
   for (iter.reset(); !iter.atEnd(); iter++) {
      subLattice.putSlice(pixels, iter.position()); 
   }
}


template <class T>
void  LatticeUtilities::addDegenerateAxes (Lattice<T>*& pLatOut, const Lattice<T>& latIn, 
                                           uInt nDim)
{
   delete pLatOut; pLatOut = 0;
   const uInt dimIn = latIn.ndim();
   if (nDim < dimIn ) {
      throw (AipsError ("Input Lattice has more dimensions than desired output Lattice"));
   } else if (nDim == dimIn) {
      pLatOut = new SubLattice<T>(latIn);
   } else {
      IPosition newShape(nDim,1);
      newShape.setFirst (latIn.shape());
      IPosition tPath = IPosition::makeAxisPath(newShape.nelements());
      IPosition newAxes = tPath.getLast(nDim-dimIn);
      IPosition stretchAxes;
//
      pLatOut = new ExtendLattice<T>(latIn, newShape, newAxes, stretchAxes);
   }
}

template <typename T> 
void LatticeUtilities::bin (MaskedArray<T>& out, const MaskedArray<T>& in, 
                            uInt axis, uInt bin)
{

// Check

   const uInt nDim = in.ndim();
   AlwaysAssert(axis<nDim, AipsError);

// Make input MaskedLattice

   ArrayLattice<T> data(in.getArray());
   ArrayLattice<Bool> mask(in.getMask());
//
   SubLattice<T> mLat(data);
   mLat.setPixelMask(mask, False);

// Create binner

   IPosition factors(nDim,1);
   factors(axis) = bin;
   RebinLattice<T> binLat(mLat, factors);

// Assign output MA

   MaskedArray<T> tmp(binLat.get(), binLat.getMask());
   out = tmp;
}

} //# End namespace casacore

#endif
