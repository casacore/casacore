//# LatticeUtilities.cc: defines the Lattice Utilities global functions//# Copyright (C) 1995,1996,1997,1999,2000,2001
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
#include <aips/Lattices/Lattice.h>
#include <aips/Lattices/LatticeIterator.h>
#include <aips/Lattices/LatticeStepper.h>
#include <trial/Lattices/MaskedLattice.h>
#include <aips/Logging/LogIO.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Mathematics/Math.h>
#include <aips/Exceptions/Error.h>
#include <aips/Utilities/Assert.h>


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

template <class T>
void LatticeUtilities::copyAndZero(LogIO& os,
                                   MaskedLattice<T>& out,
                                   MaskedLattice<T>& in)
//
// Copy the data and the mask from in to out.
// The output pixels are zeroed if the pixel is masked
//
{
   Bool doMask = in.isMasked() && out.hasPixelMask();
   Lattice<Bool>* pMaskOut = 0;
   if (doMask) {
      pMaskOut = &out.pixelMask();
      if (!pMaskOut->isWritable()) {
         doMask = False;
         os << LogIO::WARN << "The output image has a mask but it is not writable" << endl;
         os << LogIO::WARN << "So the mask will not be transferred to the output" << LogIO::POST;
      }
   }
//  
   if (doMask) {
      LatticeIterator<T> outIter(out);
      Bool deleteDataIn, deleteMaskIn, deleteDataOut;
      IPosition shape = outIter.woCursor().shape();
      Array<T> dataIn(shape);
      Array<Bool> maskIn(shape);
//
      for (outIter.reset(); !outIter.atEnd(); outIter++) {
         shape = outIter.woCursor().shape();
         if (!dataIn.shape().isEqual(shape)) dataIn.resize(shape);
         if (!maskIn.shape().isEqual(shape)) maskIn.resize(shape);
//
         in.getSlice(dataIn, outIter.position(), shape);
         in.getMaskSlice(maskIn, outIter.position(), shape);
//
         const T* pDataIn = dataIn.getStorage(deleteDataIn);
         const Bool* pMaskIn = maskIn.getStorage(deleteMaskIn);
//
         Array<T>& dataOut = outIter.woCursor();
         T* pDataOut = dataOut.getStorage(deleteDataOut);
//
         for (Int i=0; i<shape.product(); i++) {
            pDataOut[i] = pDataIn[i];
            if (!pMaskIn[i]) pDataOut[i] = 0.0;
         }
         pMaskOut->putSlice(maskIn, outIter.position());
//
         dataIn.freeStorage(pDataIn, deleteDataIn);
         maskIn.freeStorage(pMaskIn, deleteMaskIn);
         dataOut.putStorage(pDataOut, deleteDataOut);
      }
   } else {
      out.copyData(in);
   }
}

template <class T>
void LatticeUtilities::copyDataAndMask(LogIO& os, MaskedLattice<T>& out,
                                       MaskedLattice<T>& in)
//
// Copy the data and mask from an input ML to the output ML.
// If the input is masked, the output must already be masked
// and ready
//
{  
// Do we need to stuff about with masks ?
   
   Bool doMask = in.isMasked() && out.hasPixelMask();
   Lattice<Bool>* pMaskOut = 0;
   if (doMask) {
      pMaskOut = &out.pixelMask();
      if (!pMaskOut->isWritable()) {
         doMask = False;
         os << LogIO::WARN << "The output image has a mask but it is not writable" << endl;
         os << LogIO::WARN << "So the mask will not be transferred to the output" << LogIO::POST;
      }
   }                        

// Use the same stepper for input and output.
                      
   IPosition cursorShape = out.niceCursorShape(); 
   LatticeStepper stepper (out.shape(), cursorShape, LatticeStepper::RESIZE);

// Create an iterator for the output to setup the cache.
// It is not used, because using putSlice directly is faster and as easy.

   LatticeIterator<T> dummyIter(out);
   RO_LatticeIterator<T> iter(in, stepper);
   for (iter.reset(); !iter.atEnd(); iter++) {
      out.putSlice(in.getSlice(iter.position(),
                   iter.cursorShape()), iter.position());
      if (doMask) {
         pMaskOut->putSlice(in.getMaskSlice(iter.position(),
                            iter.cursorShape()), iter.position());
      }
   }
}

