 //# LatticeApply.cc: Optimally iterate through lattices and apply supplied function
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
#include <trial/Lattices/TiledLineStepper.h>
#include <trial/Lattices/LatticeApply.h>
#include <trial/Lattices/LatticeIterator.h>
#include <trial/Images/PagedImage.h>
#include <trial/Tasking/ProgressMeter.h>

#include <iostream.h>

template <class T>
void LatticeApply<T>::vectorApply (Lattice<T>& latticeOut,
				   const Lattice<T>& latticeIn,
				   VectorCollapser<T>& collapser,
				   const Int profileAxis,
				   const IPosition& blcU,
				   const IPosition& trcU,
////				   const IPosition& incU,
				   const Bool dropAxis,
				   const Bool showProgress,
				   const String& progressTitle)
{
// Verify region

    IPosition blc(blcU);
    IPosition trc(trcU);
    IPosition inc;
////    IPosition inc(incU);
    IPosition ioMap;
    prepare (ioMap, blc, trc, inc, latticeIn, latticeOut, profileAxis, 
	     dropAxis);

// Input profiles are extracted with the TiledLineStepper.

    IPosition inTileShape = latticeIn.niceCursorShape (latticeIn.maxPixels());
    TiledLineStepper inNav(latticeIn.shape(), inTileShape, profileAxis);
    inNav.subSection (blc, trc);
    RO_LatticeIterator<T> inIter(latticeIn, inNav);
    IPosition latticeShape = inNav.subLatticeShape();
    
// An output buffer matching the input tile shape is manually created
// as there is no navigator to do this.

    const uInt outDim = latticeOut.ndim();
    IPosition outPos(outDim);
    IPosition outShape(outDim);

// Set up ProgressMeter

    ProgressMeter* pClock;
    Double meterValue = 0.0;
    if (showProgress) {
	Double nProfiles = latticeOut.shape().product();
	pClock = new ProgressMeter(0.0, nProfiles, progressTitle,
				   "Vectors extracted", "", "",
				   True, max(1,Int(nProfiles/20)));
    }


// Iterate

    while (!inIter.atEnd()) {

// Create output buffer shape. Has to be done inside the loop
// as the tile shape may not fit integrally into the lattice

	for (uInt j=0; j<outDim; j++) {
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
	    data[i] = collapser.collapse (inIter.vectorCursor(),
					  inIter.position());
	    inIter++;

// Update progress meter
	
	    if (showProgress) {
		meterValue += 1.0;
		pClock->update (meterValue);
	    }
	}
	array.putStorage (data, deleteIt);
	latticeOut.putSlice (array, outPos);
    }
    if (showProgress) delete pClock;
}



template <class T>
void LatticeApply<T>::vectorMultiApply (PtrBlock<Lattice<T>*>& latticeOut,
					const Lattice<T>& latticeIn,
					VectorCollapser<T>& collapser,
					const Int profileAxis,
					const IPosition& blcU,
					const IPosition& trcU,
////					const IPosition& incU,
					const Bool dropAxis,
					const Bool showProgress,
					const String& progressTitle)
{

// First verify that all the output lattices have the same shape and tile shape

    const Int nOut = latticeOut.nelements();
    AlwaysAssert(nOut > 0, AipsError);
    const IPosition shape(latticeOut[0]->shape());
    const uInt outDim = shape.nelements();
    for (Int i=1; i<nOut; i++) {
	AlwaysAssert(latticeOut[i]->shape() == shape, AipsError);
    }

// Make veracity check now on input and first output lattices
// and work out map to translate input and output axes

    IPosition blc(blcU);
    IPosition trc(trcU);
    IPosition inc;
////    IPosition inc(incU);
    IPosition ioMap;
    prepare (ioMap, blc, trc, inc, latticeIn, *(latticeOut[0]),
	     profileAxis, dropAxis);

// Input profiles are extracted with the TiledLineStepper.

    IPosition inTileShape = latticeIn.niceCursorShape (latticeIn.maxPixels());
    TiledLineStepper inNav(latticeIn.shape(), inTileShape, profileAxis);
    inNav.subSection (blc, trc);
    RO_LatticeIterator<T> inIter(latticeIn, inNav);
    IPosition latticeShape = inNav.subLatticeShape();

// An output buffer matching the input tile shape is manually created
// as there is no navigator to do this.

    IPosition outPos(outDim);
    IPosition outShape(outDim);

// Set up ProgressMeter
   
    ProgressMeter* pClock;
    Double meterValue = 0.0;
    if (showProgress) {
	Double nProfiles = shape.product();
	pClock = new ProgressMeter(0.0, nProfiles, progressTitle,
				   "Vectors extracted", "", "",
				   True, max(1,Int(nProfiles/20)));
    }
    
// Iterate

    while (!inIter.atEnd()) {

// Create output buffer shape. Has to be done inside the loop
// as the tile shape may not fit integrally into the lattice

	for (uInt j=0; j<outDim; j++) {
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
// The buffer contains nOut arrays (and is filled that way).
	
	Bool deleteIt;
	uInt n = outShape.product();
	Block<T> block(n*nOut);
	T* data = block.storage();
	for (uInt i=0; i<n; i++) {
	    Vector<T>& result = collapser.multiCollapse (inIter.vectorCursor(),
							 inIter.position());
	    DebugAssert (result.nelements() == nOut, AipsError);
	    T* datap = data+i;
	    for (uInt j=0; j<nOut; j++) {
		*datap = result(j);
		datap += n;
	    }
	    inIter++;
	}

// Write the arrays (one in each output lattice).

	for (uInt k=0; k<nOut; k++) {
	    Array<T> tmp (outShape, data + k*n, SHARE);
	    latticeOut[k]->putSlice (tmp, outPos);
	}
	
// Update progress meter
	
	if (showProgress) {
	    meterValue += 1.0;
	    pClock->update (meterValue);
	}
    }
    if (showProgress) delete pClock;
}





template <class T>
void LatticeApply<T>::prepare (IPosition& ioMap, 
			       IPosition& inBlc,
			       IPosition& inTrc,
			       IPosition& inInc,
			       const Lattice<T>& latticeIn,
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
	for (i=0; i<outDim; i++) {
	    AlwaysAssert(outShape(i) == inShape(ioMap(i)), AipsError);
	}
    } else {

// Axis map is just one-to-one

	for (i=0; i<inDim; i++) ioMap(i) = i;

// Check conformancy

	AlwaysAssert(outDim == inDim, AipsError);
	AlwaysAssert(outShape(profileAxis) == 1, AipsError);
	AlwaysAssert(profileAxis >= 0 && profileAxis <= inDim-1, AipsError);
	
	for (i=0; i<outDim; i++) {
	    if (i != profileAxis) {
		AlwaysAssert(outShape(i) == inShape(i), AipsError);
	    }
	}
    }
}
