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

#include <trial/Lattices/LatticeUtilities.h>

#include <aips/aips.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/Slicer.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/IPosition.h>
#include <aips/Lattices/Lattice.h>
#include <trial/Lattices/ExtendLattice.h>
#include <trial/Lattices/SubLattice.h>
#include <trial/Lattices/MaskedLatticeIterator.h>
#include <aips/Lattices/LatticeStepper.h>
#include <trial/Lattices/MaskedLattice.h>
#include <trial/Lattices/LatticeStatistics.h>
#include <aips/Lattices/TempLattice.h>
#include <aips/Logging/LogIO.h>
#include <aips/Mathematics/Math.h>
#include <aips/Exceptions/Error.h>
#include <aips/Utilities/Assert.h>
#include <aips/iostream.h>


template <class T> 
void minMax(T & globalMin, T & globalMax, 
	    IPosition & globalMinPos, IPosition & globalMaxPos, 
	    const Lattice<T> & lat) 
{
  //check if IPositions are conformant
  IPosition zeroPos = IPosition( lat.shape().nelements(), 0); 
  DebugAssert((zeroPos.nelements() == globalMinPos.nelements()), AipsError);
  DebugAssert((zeroPos.nelements() == globalMaxPos.nelements()), AipsError);
  
  IPosition cursorShape(lat.niceCursorShape());
  RO_LatticeIterator<T> latIter(lat, cursorShape);
  
  globalMin = lat.getAt( zeroPos );
  globalMinPos = zeroPos;
  globalMax = lat.getAt( zeroPos );
  globalMaxPos = zeroPos;
  
  for(latIter.reset(); !latIter.atEnd(); latIter++) {

    T localMin;
    IPosition localMinPos( latIter.cursor().ndim() );
    T localMax;
    IPosition localMaxPos( latIter.cursor().ndim() );

    Array<T>  arr = latIter.cursor();

    minMax(localMin, localMax, localMinPos, localMaxPos, arr);

    IPosition loc (latIter.position());
    
    if (localMin < globalMin) {
      globalMin = localMin;
      globalMinPos = loc + localMinPos;
    }
    if (localMax > globalMax) {
      globalMax = localMax;
      globalMaxPos = loc + localMaxPos;
    }
  }
}


// LatticeUtilities

template <class T>
void LatticeUtilities::collapse (Array<T>& out, const IPosition& axes,
                                 const MaskedLattice<T>& in,
                                 Bool dropDegenerateAxes) 
{ 
   LatticeStatistics<T> stats(in, False, False);
   AlwaysAssert(stats.setAxes(axes.asVector()), AipsError);
   stats.getMean(out, dropDegenerateAxes);
}

template <class T>
void LatticeUtilities::collapse(Array<T>& data, Array<Bool>& mask,
                                const IPosition& axes, 
                                const MaskedLattice<T>& in,
                                Bool dropDegenerateAxes,
                                Bool getPixels, Bool getMask)
{ 
   
// These lattice are all references so should be reasonably
// fast.  I can't do it the otherway around, i.e. drop degenerate
// axes first with an axes specifier because then the 'axes'
// argument won't match one to one with the lattice axes and
// that would be confusing.  Pity.
                      
   LatticeStatistics<T> stats(in, False, False);
   stats.setAxes(axes.asVector());
//
   if (getPixels) {
      stats.getMean(data, dropDegenerateAxes);
   } else {
      data.resize(IPosition(0,0));
   }

// CLumsy way to get mask.  I should add it to LS

   if (getMask) {
      Array<T> n;
      stats.getNPts(n, dropDegenerateAxes);
      mask.resize(n.shape());
//
      T lim(0.5);
      typename Array<T>::const_iterator it;
      typename Array<Bool>::iterator mIt;
      for (it=n.begin(),mIt=mask.begin(); it!=n.end(); it++,mIt++) {
         *mIt = True;
         if (*it < lim) *mIt = False;
      }
   } else {
      mask.resize();
   }
 }


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

// Create an iterator for the output to setup the cache.

   LatticeIterator<T> dummyIter(out);
   RO_MaskedLatticeIterator<T> iter(in, stepper);
   for (iter.reset(); !iter.atEnd(); iter++) {

// Put the pixels

      typename Array<Bool>::const_iterator mIt;
      typename Array<T>::iterator dIt;
      IPosition cursorShape = iter.cursorShape();
      if (zeroMasked) {
         Array<T> pixels = iter.cursor().copy();
         const Array<Bool>& mask = iter.getMask();
//
         for (dIt=pixels.begin(),mIt=mask.begin(); dIt!=pixels.end(); dIt++,mIt++) {
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
   LatticeStepper stepper(subLattice.shape(), pixels.shape(),
                          LatticeStepper::RESIZE);
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
