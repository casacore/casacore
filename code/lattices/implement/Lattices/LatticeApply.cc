//# LatticeApply.cc: Optimally iterate through lattices and apply supplied function
//# Copyright (C) 1997
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
#include <trial/Lattices/LineCollapser.h>
#include <trial/Lattices/TiledCollapser.h>
#include <trial/Lattices/LatticeProgress.h>
#include <trial/Lattices/TiledLineStepper.h>
#include <trial/Lattices/TileStepper.h>
#include <trial/Lattices/LatticeApply.h>
#include <trial/Lattices/LatticeIterator.h>
#include <trial/Lattices/PixelBox.h>
#include <trial/Lattices/PixelRegion.h>
#include <iostream.h>


template <class T>
void LatticeApply<T>::lineApply (Lattice<T>& latticeOut,
				 const Lattice<T>& latticeIn,
				 LineCollapser<T>& collapser,
				 uInt collapseAxis,
				 LatticeProgress* tellProgress)
{
    lineApply (latticeOut, latticeIn,
	       PixelBox(IPosition(latticeIn.ndim(), 0),
			latticeIn.shape() - 1,
			latticeIn.shape()),
	       collapser, collapseAxis, tellProgress);
}

template <class T>
void LatticeApply<T>::lineMultiApply (PtrBlock<Lattice<T>*>& latticeOut,
				      const Lattice<T>& latticeIn,
				      LineCollapser<T>& collapser,
				      uInt collapseAxis,
				      LatticeProgress* tellProgress)
{
    lineMultiApply (latticeOut, latticeIn,
		    PixelBox(IPosition(latticeIn.ndim(), 0),
			     latticeIn.shape() - 1,
			     latticeIn.shape()),
		    collapser, collapseAxis, tellProgress);
}

template <class T>
void LatticeApply<T>::tiledApply (Lattice<T>& latticeOut,
				  const Lattice<T>& latticeIn,
				  TiledCollapser<T>& collapser,
				  const IPosition& collapseAxes,
				  LatticeProgress* tellProgress)
{
    tiledApply (latticeOut, latticeIn,
		PixelBox(IPosition(latticeIn.ndim(), 0),
			 latticeIn.shape() - 1,
			 latticeIn.shape()),
		collapser, collapseAxes, tellProgress);
}



template <class T>
void LatticeApply<T>::lineApply (Lattice<T>& latticeOut,
				 const Lattice<T>& latticeIn,
				 const PixelRegion& region,
				 LineCollapser<T>& collapser,
				 uInt collapseAxis,
				 LatticeProgress* tellProgress)
{
// Make veracity check on input and output lattice
// and work out map to translate input and output axes.

    IPosition ioMap = prepare (region.box().length(), latticeOut.shape(),
			       IPosition(1,collapseAxis));

// Input lines are extracted with the TiledLineStepper.

    const IPosition& inShape = latticeIn.shape();
    IPosition inTileShape = latticeIn.niceCursorShape();
    TiledLineStepper inNav(inShape, inTileShape, collapseAxis);
    inNav.subSection (region.box().start(), region.box().end(),
		      region.box().stride());
    RO_LatticeIterator<T> inIter(latticeIn, inNav);

    const IPosition& blc = region.box().start();
    const IPosition& trc = region.box().end();
    const IPosition& inc = region.box().stride();
    const IPosition& len = region.box().length();
    const uInt outDim = latticeOut.ndim();
    IPosition outPos(outDim, 0);
    IPosition outShape(outDim, 1);
    for (uInt i=0; i<outDim; i++) {
	if (ioMap(i) >= 0) {
	    outShape(i) = len(ioMap(i));
	}
    }

// Set the number of expected steps.
// This is the number of lines to process.
// Also give the number of resulting output pixels per line, so the
// collapser can check it.

    Int nLine = outShape.product();
    Int nResult = latticeOut.shape().product() / nLine;
    AlwaysAssert (nResult==1, AipsError);
    collapser.init (nResult);
    if (tellProgress != 0) tellProgress->init (nLine);

// Iterate through all the lines.
// Per tile the lines (in the collapseAxis direction) are
// assembled into a single array, which is put thereafter.

    while (! inIter.atEnd()) {

// Calculate output buffer shape. Has to be done inside the loop
// as the tile shape may not fit integrally into the lattice.
// It takes care of blc, trc, and inc.

	IPosition pos = inIter.position();
	for (uInt j=0; j<outDim; j++) {
	    if (ioMap(j) >= 0) {
		uInt i = ioMap(j);
		uInt stPos = (pos(j) - blc(j)) % inc(j); 
		if (stPos != 0) {
		    stPos = inc(j) - stPos;
		}
		Int sz = inTileShape(i) - pos(i) % inTileShape(i);
		sz = min (sz, 1 + trc(i) - pos(i)) - stPos;
		AlwaysAssert (sz > 0, AipsError);
		outShape(j) = (sz + inc(i) - 1) / inc(i);
		outPos(j) = (pos(i) - blc(i)) / inc(i);
	    }
	}
//      cout << outShape << " put at " << outPos << endl;
	
// Put the collapsed lines into an output buffer
	
	Array<T> array(outShape);
	Bool deleteIt;
	T* data = array.getStorage (deleteIt);
	uInt n = array.nelements() / nResult;
	for (uInt i=0; i<n; i++) {
	    DebugAssert (! inIter.atEnd(), AipsError);
	    data[i] = collapser.process (inIter.vectorCursor(),
					 inIter.position());
	    inIter++;
	    if (tellProgress != 0) tellProgress->nstepsDone (inIter.nsteps());
	}
	array.putStorage (data, deleteIt);
	latticeOut.putSlice (array, outPos);
    }
    if (tellProgress != 0) tellProgress->done();
}



template <class T>
void LatticeApply<T>::lineMultiApply (PtrBlock<Lattice<T>*>& latticeOut,
				      const Lattice<T>& latticeIn,
				      const PixelRegion& region,
				      LineCollapser<T>& collapser,
				      uInt collapseAxis,
				      LatticeProgress* tellProgress)
{
// First verify that all the output lattices have the same shape and tile shape

    uInt i;
    const uInt nOut = latticeOut.nelements();
    AlwaysAssert(nOut > 0, AipsError);
    const IPosition shape(latticeOut[0]->shape());
    const uInt outDim = shape.nelements();
    for (i=1; i<nOut; i++) {
	AlwaysAssert(latticeOut[i]->shape() == shape, AipsError);
    }

// Make veracity check on input and first output lattice
// and work out map to translate input and output axes.

    IPosition ioMap = prepare (region.box().length(), shape,
			       IPosition(1,collapseAxis));

// Input lines are extracted with the TiledLineStepper.

    const IPosition& inShape = latticeIn.shape();
    IPosition inTileShape = latticeIn.niceCursorShape();
    TiledLineStepper inNav(inShape, inTileShape, collapseAxis);
    inNav.subSection (region.box().start(), region.box().end(),
		      region.box().stride());
    RO_LatticeIterator<T> inIter(latticeIn, inNav);

    const IPosition& blc = region.box().start();
    const IPosition& trc = region.box().end();
    const IPosition& inc = region.box().stride();
    const IPosition& len = region.box().length();
    IPosition outPos(outDim, 0);
    IPosition outShape(outDim, 1);
    for (i=0; i<outDim; i++) {
	if (ioMap(i) >= 0) {
	    outShape(i) = len(ioMap(i));
	}
    }

// Set the number of expected steps.
// This is the number of lines to process.
// Also give the number of resulting output pixels per line, so the
// collapser can it.

    Int nLine = outShape.product();
    Int nResult = shape.product() / nLine;
    AlwaysAssert (nResult==1, AipsError);
    collapser.init (nResult);
    if (tellProgress != 0) tellProgress->init (nLine);

// Iterate through all the lines.
// Per tile the lines (in the collapseAxis) direction are
// assembled into a single array, which is put thereafter.

    while (!inIter.atEnd()) {

// Calculate output buffer shape. Has to be done inside the loop
// as the tile shape may not fit integrally into the lattice.
// It takes care of blc, trc, and inc.

	IPosition pos = inIter.position();
	for (uInt j=0; j<outDim; j++) {
	    if (ioMap(j) >= 0) {
		uInt i = ioMap(j);
		uInt stPos = (pos(j) - blc(j)) % inc(j); 
		if (stPos != 0) {
		    stPos = inc(j) - stPos;
		}
		Int sz = inTileShape(i) - pos(i) % inTileShape(i);
		sz = min (sz, 1 + trc(i) - pos(i)) - stPos;
		AlwaysAssert (sz > 0, AipsError);
		outShape(j) = (sz + inc(i) - 1) / inc(i);
		outPos(j) = (pos(i) - blc(i)) / inc(i);
	    }
	}
//      cout << outShape << " put at " << outPos << endl;
	
// Put the collapsed lines into the output buffer
// The buffer contains nOut arrays (and is filled that way).
	
	uInt n = outShape.product();
	Block<T> block(n*nOut);
	T* data = block.storage();
	for (uInt i=0; i<n; i++) {
	    DebugAssert (! inIter.atEnd(), AipsError);
	    Vector<T>& result = collapser.multiProcess (inIter.vectorCursor(),
							inIter.position());
	    DebugAssert (result.nelements() == nOut, AipsError);
	    T* datap = data+i;
	    for (uInt j=0; j<nOut; j++) {
		*datap = result(j);
		datap += n;
	    }
	    inIter++;
	    if (tellProgress != 0) tellProgress->nstepsDone (inIter.nsteps());
	}

// Write the arrays (one in each output lattice).

	for (uInt k=0; k<nOut; k++) {
	    Array<T> tmp (outShape, data + k*n, SHARE);
	    latticeOut[k]->putSlice (tmp, outPos);
	}
    }
    if (tellProgress != 0) tellProgress->done();
}


template <class T>
void LatticeApply<T>::tiledApply (Lattice<T>& latticeOut,
				  const Lattice<T>& latticeIn,
				  const PixelRegion& region,
				  TiledCollapser<T>& collapser,
				  const IPosition& collapseAxes,
				  LatticeProgress* tellProgress)
{
// Make veracity check on input and first output lattice
// and work out map to translate input and output axes.

    uInt i,j;
    IPosition ioMap = prepare (region.box().length(), latticeOut.shape(),
			       collapseAxes);

// The input is traversed using a TileStepper.

    const IPosition& inShape = latticeIn.shape();
    const uInt inDim = inShape.nelements();
    IPosition inTileShape = latticeIn.niceCursorShape();
    TileStepper inNav(inShape, inTileShape, collapseAxes);
    inNav.subSection (region.box().start(), region.box().end(),
		      region.box().stride());
    RO_LatticeIterator<T> inIter(latticeIn, inNav);

// Precalculate various variables.

    const IPosition& blc = region.box().start();
    const IPosition& trc = region.box().end();
    const IPosition& inc = region.box().stride();
    const uInt collDim = collapseAxes.nelements();
    const uInt iterDim = inDim - collDim;
    IPosition iterAxes(iterDim);
    IPosition outShape(latticeOut.shape());
    const uInt outDim = outShape.nelements();
    j = 0;
    for (i=0; i<outDim; i++) {
	if (ioMap(i) >= 0) {
	    outShape(i) = 1;
	    iterAxes(j++) = i;
	}
    }

//    cout << "ioMap      " << ioMap << endl;
//    cout << "iterAxes   " << iterAxes << endl;
//    cout << "outShape   " << outShape << endl;

// Set the number of expected steps.
// This is the number of tiles to process.
// Also give the number of resulting output pixels per line, so the
// collapser can check it.

    uInt nsteps = 1;
    for (j=0; j<inDim; j++) {
	nsteps *= 1 + trc(j)/inTileShape(j) - blc(j)/inTileShape(j);
    }
    collapser.init (outShape.product());
    if (tellProgress != 0) tellProgress->init (nsteps);
//    cout << "nsteps     " << nsteps << endl;

// Determine the axis where the collapsed values are stored in the output.
// This is the first unmapped axis (the first axis when all axes are mapped).
    uInt resultAxis = 0;
    for (j=0; j<outDim; j++) {
	if (ioMap(j) < 0) {
	    resultAxis = j;
	    break;
	}
    }

// Iterate through all the tiles.
// TileStepper is set up in such a way that the collapse axes are iterated
// fastest. When all collapse axes are handled, thus when the iter axes
// position changes, we have to write that part.

    Bool firstTime = True;
    IPosition outPos(outDim, 0);
    IPosition iterPos(outDim, 0);
    while (! inIter.atEnd()) {

// Calculate the size of each chunk of output data.
// Each chunk contains the data of a tile in each IterAxis.
// Determine the index of the first element to take from the cursor.

	const Array<T>& cursor = inIter.cursor();
	const IPosition& cursorShape = cursor.shape();
	IPosition pos = inIter.position();
	for (j=0; j<outDim; j++) {
	    if (ioMap(j) >= 0) {
		uInt axis = ioMap(j);
		iterPos(j) = (pos(axis) - blc(axis)) / inc(axis);
	    }
	}
	if (firstTime  ||  outPos != iterPos) {
	    if (!firstTime) {
		latticeOut.putSlice (collapser.endAccumulator(outShape),
				     outPos);
	    }
	    firstTime = False;
	    outPos = iterPos;
	    uInt n1 = 1;
	    uInt n3 = 1;
	    for (j=0; j<outDim; j++) {
		if (ioMap(j) >= 0) {
		    outShape(j) = cursorShape(ioMap(j));
		    if (j < resultAxis) {
		        n1 *= outShape(j);
		    } else {
		        n3 *= outShape(j);
		    }
		}
	    }
	    collapser.initAccumulator (n1, n3);
	}

// Put the collapsed lines into an output buffer
// Initialize the cursor position needed in the loop.

	IPosition curPos (inDim, 0);

// Determine the increment for the first collapse axes.
// This is done by taking the difference between the adresses of two pixels
// in the cursor (if there are 2 pixels).

	const uInt axis = collapseAxes(0);
	uInt nval = cursorShape(axis);
	uInt incr = 1;
	for (uint i=0; i<axis; i++) {
	    incr *= cursorShape(i);
	}
	
//	cout << " cursorShape " << cursorShape << endl;
//	cout << " incr        " << incr << endl;
//	cout << " nval        " << nval << endl;

// Iterate in the outer loop through the iterator axes.
// Iterate in the inner loop through the collapse axes.

	uInt index1 = 0;
	uInt index3 = 0;
	for (;;) {
	    for (;;) {
//	        cout << curPos << ' ' << collPos << endl;
		collapser.process (index1, index3,
				   &(cursor(curPos)), incr, nval);
		// Increment a sum axis until all axes are handled.
		for (j=1; j<collDim; j++) {
		    uInt axis = collapseAxes(j);
		    if (++curPos(axis) < cursorShape(axis)) {
			break;
		    }
		    curPos(axis) = 0;               // restart this axis
		}
		if (j == collDim) {
		    break;                          // all axes are handled
		}
	    }
	
// Increment an iteration axis until all iteration axes are handled.
	
	    for (j=0; j<iterDim; j++) {
		uInt arraxis = iterAxes(j);
		uInt axis = ioMap(arraxis);
		if (++curPos(axis) < cursorShape(axis)) {
		    if (arraxis < resultAxis) {
		        index1++;
		    } else {
		        index3++;
			index1 = 0;
		    }
		    break;
		}
		curPos(axis) = 0;
	    }
	    if (j == iterDim) {
		break;
	    }
	}
	inIter++;
	if (tellProgress != 0) tellProgress->nstepsDone (inIter.nsteps());
    }

// Write out the last output array.
    latticeOut.putSlice (collapser.endAccumulator(outShape), outPos);
    if (tellProgress != 0) tellProgress->done();
}



template <class T>
IPosition LatticeApply<T>::prepare (const IPosition& inShape,
				    const IPosition& outShape,
				    const IPosition& collapseAxes)
{
    uInt i;
    // Check if the dimensionality of input and output match.
    const uInt inDim  = inShape.nelements();
    const uInt outDim = outShape.nelements();
    const uInt collDim = collapseAxes.nelements();
    uInt ndim = inDim - collDim;
    if (outDim < ndim) {
	throw (AipsError ("LatticeApply::prepare - dimensionalities mismatch"));
    }
    // Check the collapseAxes specification (using the makeAxisPath logic).
    // Also check if they are ascending.
    IPosition allAxes = IPosition::makeAxisPath (inDim, collapseAxes);
    for (i=1; i<collDim; i++) {
	AlwaysAssert (collapseAxes(i) > collapseAxes(i-1), AipsError);
    }
    IPosition ioMap(outDim, -1);
    // Make a little map of the input to the output axes.
    // ioMap(j) is the axis of the input that goes on output axis j.
    // Also check if the length of these axes match for input and output.
    uInt k=0;
    for (i=collDim; i<inDim; i++) {
	uInt axis = allAxes(i);
	while (k < outDim  &&  inShape(axis) != outShape(k)) {
	    k++;
	}
	if (k == outDim) {
	    throw (AipsError ("LatticeApply::prepare - "
			      "non-collapsed input and output shape mismatch"));
	}
	ioMap(k) = axis;
	k++;
    }
    // Make sure the non-mapped axes are consecutive.
    uInt flag = 0;
    for (i=0; i<outDim; i++) {
	if (ioMap(i) < 0) {
	    if (flag == 2) {
		throw (AipsError ("LatticeApply::prepare - "
				  "new output axes are not consecutive"));
	    }
	    flag = 1;
	} else if (flag == 1) {
	    flag = 2;              // Mapped axes after non-mapped axes
	}
    }
    return ioMap;
}
