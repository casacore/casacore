 //# LatticeApply.cc: optimally iterate through lattices and apply supplied function
//# Copyright (C) 1996,1997
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
//
#include <aips/aips.h>
#include <aips/Arrays/ArrayPosIter.h>
#include <aips/Arrays/Vector.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Mathematics/Math.h>
#include <aips/Utilities/Assert.h>
#include <trial/Images/ImageUtilities.h>
#include <trial/Lattices/Lattice.h>
#include <trial/Lattices/LatticeStepper.h>
#include <trial/Lattices/VectorCollapser.h>
#include <trial/Lattices/TiledStepper.h>
#include <trial/Lattices/LatticeApply.h>
#include <trial/Lattices/LatticeIterator.h>
#include <trial/Images/PagedImage.h>
#include <trial/Tasking/ProgressMeter.h>

#include <iostream.h>

template <class T>
void LatticeApply<T>::vectorApply(Lattice<T> &latticeOut,
                                  Lattice<T> &latticeIn,
                                  VectorCollapser<T> &collapser,
                                  const Int profileAxis,
                                  const IPosition blcU,
                                  const IPosition trcU,
                                  const Bool dropAxis,
                                  const Bool showProgress,
                                  const String progressTitle)
//
// This function iterates through a Lattice and applies a user given
// function to profiles along the specified axis.  The result of the
// user supplied function is written into the output Lattice at the
// location of the collapsed profile.  The output lattice must be supplied
// with the correct shape; it must have the shape of the supplied region
// 
// Inputs
//  latticeIn  The input Lattice
//  collapser  The user supplies an object of a class derived from
//             the base class VectorCollapser.  The user provides the
//             implementation of the method "collapse" which takes
//             a profile and returns a value from it.
//  profileAxis     
//             The profile axis
//  blc,trc    The region of interest of the input Lattice.  Will be
//             filled in (0 & shape-1)if values not given or illegal
//  dropAxis   If False, then the output Lattice must have as many 
//             dimensions as the input Lattice, but its shape for
//             axis "index" must be unity.   E.g. if
//               in.shape() = [2,4,6]  and profileAxis=1,  then
//              out.shape() = [2,1,6]
//          
//             If True, then the output Lattice must have one less 
//             dimension than the input Lattice; the axis "index" is 
//             thus dropped from the output Lattice.    E.g. if
//               in.shape() = [2,4,6]  and profileAxis=1,  then
//              out.shape() = [2,6]
//  showProgress 
//             Show ProgressMeter
//  progressTitle
//             title for progress meter
// Output
//  latticeOut The output lattice.  It must be the correct shape to account
//             for the desired region (i.e. shape = trc - blc + 1)
//  
{
// Verify region

   IPosition blc(blcU);
   IPosition trc(trcU);
   IPosition ioMap;
   prepare(ioMap, blc, trc, latticeIn, latticeOut, profileAxis, 
           dropAxis);

// Input profiles are extarcted with the TiledStepper.

   IPosition inTileShape = latticeIn.niceCursorShape(latticeIn.maxPixels());
   TiledStepper inNav(latticeIn.shape(), inTileShape, profileAxis);
   inNav.subSection (blc, trc);
   RO_LatticeIterator<T> inIter(latticeIn, inNav);
   IPosition latticeShape = inNav.subLatticeShape();

// An output buffer matching the input tile shape is manually created
// as there is no navigator to do this.

   Int outDim = latticeOut.ndim();
   IPosition outPos(outDim);
   IPosition outShape(outDim);

// Set up ProgressMeter

   ProgressMeter* pClock;
   Double meterValue = 0.0;
   if (showProgress) {
      Double nProfiles = Double(latticeOut.shape().product());
      pClock = new ProgressMeter(0.0, nProfiles, progressTitle,
                     "Vectors extracted", "", "", True, max(1,Int(nProfiles/20)));
   }


// Iterate

   while (!inIter.atEnd()) {

// Create output buffer shape. Has to be done inside the loop
// as the tile shape may not fit integrally into the lattice

      for (Int j=0; j<outDim; j++) {
	 uInt i = ioMap(j);
         outPos(j) = inIter.position()(i);
	 Int sz = inTileShape(i) - outPos(j) % inTileShape(i);
	 outShape(j) = min (sz, 1 + trc(i) - outPos(j));
         outPos(j) -= blc(i);
      }
      if (outDim == blc.nelements()) {
	  outShape(profileAxis) = 1;
      }
//      cout << outShape << " put at " << outPos << endl;

// Put the collapsed vectors into the output buffer

      Array<T> array(outShape);
      Bool deleteIt;
      T* data = array.getStorage (deleteIt);
      uInt n = array.nelements();
      for (uInt i=0; i<n; i++) {
	  data[i] = collapser.collapse(inIter.vectorCursor(),
                                  inIter.position());
	  inIter++;
      }
      array.putStorage (data, deleteIt);
      latticeOut.putSlice (array, outPos);

// Update progress meter

      if (showProgress) {
         meterValue += 1.0;
         pClock->update(meterValue);
      }
   }
   if (showProgress) delete pClock;
}



template <class T>
void LatticeApply<T>::vectorMultiApply(PtrBlock<Lattice<T> *> &latticeOut,
                                       Lattice<T> &latticeIn,
                                       VectorCollapser<T>& collapser,
                                       const Int profileAxis,
                                       const IPosition blcU,
                                       const IPosition trcU,
                                       const Bool dropAxis,
                                       const Bool showProgress,
                                       const String progressTitle)
//
// This function iterates through a Lattice and applies a user given
// function to profiles along the specified axis.  The result of the
// user supplied function is written into the output Lattices at the
// location of the collapsed profile.  The output lattices must be supplied
// with the correct shape; it must have the shape of the supplied region
//
// The collapser member function "collapse" must return a vector of numbers.
// These are assigned to the output lattices, one to one.
// 
// Inputs
//  latticeIn  The input Lattice
//  collapser  The user supplies an object of a class derived from
//             the base class VectorCollapser.  The user provides the
//             implementation of the method "multiCollapse" which takes
//             a profile and returns a vector of values from it.
//  profileAxis     
//             The profile axis
//  blc,trc    The region of interest of the input Lattice.  Will be
//             filled in (0 & shape-1)if values not given or illegal
//  dropAxis   If False, then the output Lattice must have as many 
//             dimensions as the input Lattice, but its shape for
//             axis "index" must be unity.   E.g. if
//               in.shape() = [2,4,6]  and profileAxis=1,  then
//              out.shape() = [2,1,6]
//          
//             If True, then the output Lattice must have one less 
//             dimension than the input Lattice; the axis "index" is 
//             thus dropped from the output Lattice.    E.g. if
//               in.shape() = [2,4,6]  and profileAxis=1,  then
//              out.shape() = [2,6]
//  showProgress 
//             Show ProgressMeter
//  progressTitle
//             title for progress meter
// Output
//  latticeOut The output lattices.  They must be the correct shape to account
//             for the desired region (i.e. shape = trc - blc + 1)
//  
{

// First verify that all the output lattices have the same shape and tile shape

   const Int nOut = latticeOut.nelements();
   AlwaysAssert(nOut > 0, AipsError);
   const IPosition outShape(latticeOut[0]->shape());
   const IPosition outTileShape = latticeOut[0]->niceCursorShape(latticeOut[0]->maxPixels());
   if (nOut > 1) {
      for (Int i=1; i<nOut; i++) {
         AlwaysAssert(latticeOut[i]->shape() == outShape, AipsError);
         AlwaysAssert(latticeOut[i]->niceCursorShape(latticeOut[i]->maxPixels()) ==
                      outTileShape, AipsError);
      }
   }

// Make veracity check now on input and first output lattices
// and work out map to translate input and output axes

   IPosition blc(blcU);
   IPosition trc(trcU);
   IPosition ioMap;
   prepare(ioMap, blc, trc, latticeIn, *(latticeOut[0]), profileAxis, dropAxis);

   Int i, j;
   const Int inDim = latticeIn.ndim();
   const Int outDim = latticeOut[0]->ndim();

// Make output Lattice iterators

   IPosition outCursorShape(outDim);
   outCursorShape = outTileShape;
   LatticeStepper outNav(outShape, outCursorShape, LatticeStepper::RESIZE);

   PtrBlock<LatticeIterator<T> *> outIter(nOut);
   for (i=0; i<nOut; i++) {
      outIter[i] = new LatticeIterator<T>(*(latticeOut[i]), outNav);
   }

// Prepare input slice

   IPosition inPos(inDim);
   inPos(profileAxis) = blc(profileAxis);

   IPosition stride(inDim,1);
   IPosition profileShape(inDim,1);
   profileShape(profileAxis) = trc(profileAxis) - blc(profileAxis) + 1;
   Vector<T> profileRef;


// Set up ProgressMeter

   ProgressMeter* pClock;
   Double meterValue = 0.0;
   if (showProgress) {
      Double nProfiles = Double(outShape.product());
      pClock = new ProgressMeter(0.0, nProfiles, progressTitle,
                     "Vectors extracted", "", "", True, max(1,Int(nProfiles/20)));
   }

// Iterate through first output lattice

   while (!outIter[0]->atEnd()) {

// Iterate through all the profiles in the current output cursor.

      ArrayPositionIterator posIterator(outIter[0]->cursor().shape(), 
                                        IPosition(outDim,0), 0);
      j =0;
      for (posIterator.origin(); !posIterator.pastEnd(); posIterator.next()) {
         j++;
         for (i=0; i<outDim; i++) {
            inPos(ioMap(i)) = posIterator.pos()(i) + 
                              outIter[0]->position()(i) + blc(ioMap(i));
         }

// Get the input profile

         Array<T> profile;
         latticeIn.getSlice(profile, inPos, profileShape, stride, True);
         profileRef.reference(profile);

// Fill the output iterator cursors

         for (i=0; i<nOut; i++) {
            outIter[i]->cursor()(posIterator.pos()) = 
                collapser.multiCollapse(profileRef,inPos)(i);
         }

// Update progress meter

         if (showProgress) {
            meterValue += 1.0;
            pClock->update(meterValue);
         }
      }


// Increment the output lattice iterators

      for (i=0; i<nOut; i++) (*(outIter[i]))++; 
   }  

// Delete memory

   for (i=0; i<nOut; i++) delete outIter[i];
   if (showProgress) delete pClock;
}





template <class T>
void LatticeApply<T>::prepare(IPosition& ioMap, 
                           IPosition& inBlc,
                           IPosition& inTrc,
                           Lattice<T>& latticeIn,
                           Lattice<T>& latticeOut,
                           const Int profileAxis,
                           const Bool dropAxis)
{   
   IPosition inc(latticeIn.ndim(),1);
   ImageUtilities::verifyRegion (inBlc, inTrc, inc, latticeIn.shape());

   IPosition inTileShape = latticeIn.niceCursorShape(latticeIn.maxPixels());
   IPosition outTileShape = latticeOut.niceCursorShape(latticeOut.maxPixels());
//   cout << "in, out tile shapes =" << inTileShape << ", " << outTileShape << endl;

   IPosition outShape = latticeOut.shape();
   IPosition inShape = inTrc - inBlc + 1;


// Setup

   const Int inDim = latticeIn.ndim();
   const Int outDim = latticeOut.ndim();
   ioMap.resize(outDim);
   Int i, j;
    
   if (dropAxis) {

// Make a little map of the input to the output axes.  ioMap(i)
// is the axis of the input that goes on output axis i

      for (i=0,j=0; i<inDim; i++) {
         if (i != profileAxis) {
            ioMap(j) = i;
            j++;
         }
      }            

// Check conformancy.   The collapsed axis is discarded and
// everything shifted down one.

      AlwaysAssert(outDim == inDim-1, AipsError);
      AlwaysAssert(profileAxis >= 0 && profileAxis <= inDim-1, AipsError);
      for (i=0; i<outDim; i++) AlwaysAssert(outShape(i) == inShape(ioMap(i)), AipsError);
   } else {

// Axis map is just one-to-one

      for (i=0; i<inDim; i++) ioMap(i) = i;

// Check conformancy

      AlwaysAssert(outDim == inDim, AipsError);
      AlwaysAssert(outShape(profileAxis) == 1, AipsError);
      AlwaysAssert(profileAxis >= 0 && profileAxis <= inDim-1, AipsError);

      for (i=0; i<outDim; i++) {
         if (i != profileAxis) AlwaysAssert(outShape(i) == inShape(i), AipsError);
      }
   }
}


