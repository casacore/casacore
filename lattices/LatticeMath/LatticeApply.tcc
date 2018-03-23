//# LatticeApply.cc: Optimally iterate through lattices and apply supplied function
//# Copyright (C) 1997,1998,1999,2000,2001
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
//# $Id: LatticeApply.tcc 21563 2015-02-16 07:05:15Z gervandiepen $

#ifndef LATTICES_LATTICEAPPLY_TCC
#define LATTICES_LATTICEAPPLY_TCC

#include <casacore/lattices/Lattices/SubLattice.h>
#include <casacore/lattices/Lattices/LatticeStepper.h>
#include <casacore/lattices/Lattices/MaskedLatticeIterator.h>
#include <casacore/lattices/LatticeMath/LineCollapser.h>
#include <casacore/lattices/LatticeMath/TiledCollapser.h>
#include <casacore/lattices/LatticeMath/LatticeProgress.h>
#include <casacore/lattices/Lattices/TiledLineStepper.h>
#include <casacore/lattices/Lattices/TileStepper.h>
#include <casacore/lattices/LatticeMath/LatticeApply.h>
#include <casacore/lattices/Lattices/LatticeIterator.h>
#include <casacore/lattices/LRegions/LatticeRegion.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/ArrayPosIter.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>

// debug
#include <casacore/casa/BasicSL/STLIO.h>
#include <casacore/casa/Arrays/ArrayIO.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template <class T, class U>
void LatticeApply<T,U>::lineApply (MaskedLattice<U>& latticeOut,
				 const MaskedLattice<T>& latticeIn,
				 const LatticeRegion& region,
				 LineCollapser<T,U>& collapser,
				 uInt collapseAxis,
				 LatticeProgress* tellProgress)
{
    lineApply (latticeOut, SubLattice<T>(latticeIn, region),
	       collapser, collapseAxis, tellProgress);
}

template <class T, class U>
void LatticeApply<T,U>::lineMultiApply (PtrBlock<MaskedLattice<U>*>& latticeOut,
				      const MaskedLattice<T>& latticeIn,
				      const LatticeRegion& region,
				      LineCollapser<T,U>& collapser,
				      uInt collapseAxis,
				      LatticeProgress* tellProgress)
{
    lineMultiApply (latticeOut, SubLattice<T>(latticeIn, region),
		    collapser, collapseAxis, tellProgress);
}

template <class T, class U>
void LatticeApply<T,U>::tiledApply (MaskedLattice<U>& latticeOut,
				  const MaskedLattice<T>& latticeIn,
				  const LatticeRegion& region,
				  TiledCollapser<T,U>& collapser,
				  const IPosition& collapseAxes,
				  Int newOutAxis,
				  LatticeProgress* tellProgress)
{
    tiledApply (latticeOut, SubLattice<T>(latticeIn, region),
		collapser, collapseAxes, newOutAxis, tellProgress);
}



template <class T, class U>
void LatticeApply<T,U>::lineApply (MaskedLattice<U>& latticeOut,
				 const MaskedLattice<T>& latticeIn,
				 LineCollapser<T,U>& collapser,
				 uInt collapseAxis,
				 LatticeProgress* tellProgress)
{
// Make veracity check on input and output lattice
// and work out map to translate input and output axes.

    IPosition ioMap = prepare (latticeIn.shape(), latticeOut.shape(),
			       IPosition(1,collapseAxis), -1);

// Does the input has a mask?
// If not, can the collapser handle a null mask.

    Bool useMask = latticeIn.isMasked();
    if (!useMask) {
	useMask =  (! collapser.canHandleNullMask());
    }

// Input lines are extracted with the TiledLineStepper.

    const IPosition& inShape = latticeIn.shape();
    IPosition inTileShape = latticeIn.niceCursorShape();
    TiledLineStepper inNav(inShape, inTileShape, collapseAxis);
    RO_LatticeIterator<T> inIter(latticeIn, inNav);

    const IPosition blc = IPosition(inShape.nelements(), 0);
    const IPosition trc = inShape - 1;
    const IPosition inc = IPosition(inShape.nelements(), 1);
    const IPosition len = inShape;
    const uInt outDim = latticeOut.ndim();
    IPosition outPos(outDim, 0);
    IPosition outShape(outDim, 1);
    for (uInt i=0; i<outDim; ++i) {
	if (ioMap(i) >= 0) {
	    outShape(i) = len(ioMap(i));
	}
    }

// See if the output lattice has a writable pixelmask.
// If so, it will later be used to write the resulting mask to.

    Lattice<Bool>* maskOut = 0;
    if (latticeOut.hasPixelMask()) {
        maskOut = &(latticeOut.pixelMask());
	if (! maskOut->isWritable()) {
	    maskOut = 0;
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
	for (uInt j=0; j<outDim; ++j) {
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
	
	Array<U> array(outShape);
	Array<Bool> arrayMask(outShape);
	Bool deleteIt, deleteMask;
	U* result = array.getStorage (deleteIt);
	Bool* resultMask = arrayMask.getStorage (deleteMask);
	uInt n = array.nelements() / nResult;
	for (uInt i=0; i<n; ++i) {
	    DebugAssert (! inIter.atEnd(), AipsError);
	    const IPosition pos (inIter.position());
	    Vector<Bool> mask;
	    if (useMask) {
		// Casting const away is innocent.
		// Remove degenerate axes to get a 1D array.
		Array<Bool> tmp;
		((MaskedLattice<T>&)latticeIn).getMaskSlice
                          (tmp, Slicer(pos, inIter.cursorShape()), True);
		mask.reference (tmp);
	    }
	    collapser.process (result[i], resultMask[i],
			       inIter.vectorCursor(), mask, pos);
	    ++inIter;
	    if (tellProgress != 0) tellProgress->nstepsDone (inIter.nsteps());
	}
	array.putStorage (result, deleteIt);
	arrayMask.putStorage (resultMask, deleteMask);
	latticeOut.putSlice (array, outPos);
	if (maskOut != 0) {
	    maskOut->putSlice (arrayMask, outPos);
	}
    }
    if (tellProgress != 0) tellProgress->done();
}

template <class T, class U>
void LatticeApply<T,U>::lineMultiApply (PtrBlock<MaskedLattice<U>*>& latticeOut,
				      const MaskedLattice<T>& latticeIn,
				      LineCollapser<T,U>& collapser,
				      uInt collapseAxis,
				      LatticeProgress* tellProgress)
{
    // First verify that all the output lattices have the same shape and tile shape
    const uInt nOut = latticeOut.nelements();
    AlwaysAssert(nOut > 0, AipsError);
    const IPosition shape(latticeOut[0]->shape());
    const uInt outDim = shape.nelements();
    for (uInt i=1; i<nOut; ++i) {
        AlwaysAssert(latticeOut[i]->shape() == shape, AipsError);
    }
    const IPosition& inShape = latticeIn.shape();
    IPosition outPos(outDim, 0);
    IPosition outShape(outDim, 1);
    // Does the input has a mask?
    // If not, can the collapser handle a null mask.
    Bool useMask = latticeIn.isMasked()
        ? True : (! collapser.canHandleNullMask());
    const uInt inNDim = inShape.size();
    const IPosition displayAxes = IPosition::makeAxisPath(inNDim).otherAxes(
        inNDim, IPosition(1, collapseAxis)
    );
    const uInt nDisplayAxes = displayAxes.size();
    Vector<U> result(nOut);
    Vector<Bool> resultMask(nOut);
    // read in larger chunks than before, because that was very
    // Inefficient and brought NRAO cluster to a snail's pace,
    // and then do the accounting for the input lines in memory
    IPosition chunkSliceStart(inNDim, 0);
    IPosition chunkSliceEnd = chunkSliceStart;
    chunkSliceEnd[collapseAxis] = inShape[collapseAxis] - 1;
    const IPosition chunkSliceEndAtChunkIterBegin = chunkSliceEnd;
    IPosition chunkShapeInit = _chunkShape(collapseAxis, latticeIn);
    LatticeStepper myStepper(inShape, chunkShapeInit, LatticeStepper::RESIZE);
    RO_MaskedLatticeIterator<T> latIter(latticeIn, myStepper);
    IPosition curPos;
    static const Vector<Bool> noMask;
    if (tellProgress) {
        uInt nExpectedIters = inShape.product()/chunkShapeInit.product();
        tellProgress->init(nExpectedIters);
    }
    uInt nDone = 0;
    for (latIter.reset(); ! latIter.atEnd(); ++latIter) {
        const IPosition cp = latIter.position();
        const Array<T>& chunk = latIter.cursor();
        IPosition chunkShape = chunk.shape();
        const Array<Bool> maskChunk = useMask ? latIter.getMask() : Array<Bool>();
        chunkSliceStart = 0;
        chunkSliceEnd = chunkSliceEndAtChunkIterBegin;
        IPosition resultArrayShape = chunkShape;
        resultArrayShape[collapseAxis] = 1;
        std::vector<Array<U> > resultArray(nOut);
        std::vector<Array<Bool> > resultArrayMask(nOut);
        // need to initialize this way rather than doing it in the constructor,
        // because using a single Array in the constructor means that all Arrays
        // in the vector reference the same Array.
        for (uInt k=0; k<nOut; k++) {
            resultArray[k] = Array<U>(resultArrayShape);
            resultArrayMask[k] = Array<Bool>(resultArrayShape);
        }
        Bool done = False;
        while (! done) {
            Vector<T> data(chunk(chunkSliceStart, chunkSliceEnd));
            Vector<Bool> mask = useMask
                ? Vector<Bool>(maskChunk(chunkSliceStart, chunkSliceEnd))
                : noMask;
            curPos = cp + chunkSliceStart;
            collapser.multiProcess(result, resultMask, data, mask, curPos);
            for (uInt k=0; k<nOut; ++k) {
                resultArray[k](chunkSliceStart) = result[k];
                resultArrayMask[k](chunkSliceStart) = resultMask[k];
            }
            done = True;
            for (uInt k=0; k<nDisplayAxes; ++k) {
                uInt dax = displayAxes[k];
                if (chunkSliceStart[dax] < chunkShape[dax] - 1) {
                    ++chunkSliceStart[dax];
                    ++chunkSliceEnd[dax];
                    done = False;
                    break;
                }
                else {
                    chunkSliceStart[dax] = 0;
                    chunkSliceEnd[dax] = 0;
                }
            }
        }
        // put the result arrays in the output lattices
        for (uInt k=0; k<nOut; ++k) {
            IPosition outpos = inNDim == outDim
                ? cp : cp.removeAxes(IPosition(1, collapseAxis));
            Bool keepAxis = resultArray[k].ndim() == latticeOut[k]->ndim();
            if (! keepAxis) {
                resultArray[k].removeDegenerate(displayAxes);
            }
            latticeOut[k]->putSlice(resultArray[k], outpos);
            if (latticeOut[k]->hasPixelMask()) {
                Lattice<Bool>& maskOut = latticeOut[k]->pixelMask();
                if (maskOut.isWritable()) {
                    if (! keepAxis) {
                        resultArrayMask[k].removeDegenerate(displayAxes);
                    }
                    maskOut.putSlice (resultArrayMask[k], outpos);
                }
            }
        }
        if (tellProgress != 0) {
            ++nDone;
            tellProgress->nstepsDone(nDone);
        }
    }
    if (tellProgress != 0) {
        tellProgress->done();
    }
}

template <class T, class U>
IPosition LatticeApply<T,U>::_chunkShape(
    uInt axis, const MaskedLattice<T>& latticeIn
) {
    uInt ndim = latticeIn.ndim();
    IPosition chunkShape(ndim, 1);
    IPosition latShape = latticeIn.shape();
    uInt nPixColAxis = latShape[axis];
    chunkShape[axis] = nPixColAxis;
    // arbitrary, but reasonable, max memory limit in bytes for storing arrays in bytes
    static const uInt limit = 2e7;
    static const uInt sizeT = sizeof(T);
    static const uInt sizeBool = sizeof(Bool);
    uInt chunkMult = latticeIn.isMasked() ? sizeT + sizeBool : sizeT;
    uInt subChunkSize = chunkMult*nPixColAxis;
    // integer division
    const uInt maxChunkSize = limit/subChunkSize;
    if (maxChunkSize <= 1) {
        // can only go row by row
        return chunkShape;
    }
    uInt x = maxChunkSize;
    for (uInt i=0; i<ndim; ++i) {
        if (i != axis) {
            chunkShape[i] = min(x, latShape[i]);
            // integer division
            x /= chunkShape[i];
            if (x == 0) {
                break;
            }
        }
    }
    return chunkShape;
}



template <class T, class U>
void LatticeApply<T,U>::tiledApply (
    MaskedLattice<U>& latticeOut,
	const MaskedLattice<T>& latticeIn,
	TiledCollapser<T,U>& collapser,
	const IPosition& collapseAxes,
	Int newOutAxis,
	LatticeProgress* tellProgress
) {
    // Make veracity check on input and first output lattice
    // and work out map to translate input and output axes.

    uInt i,j;
    IPosition ioMap = prepare (
        latticeIn.shape(), latticeOut.shape(),
		collapseAxes, newOutAxis
    );

    // Does the input has a mask?
    // If not, can the collapser handle a null mask.

    Bool useMask = latticeIn.isMasked();
    if (!useMask) {
	    useMask =  (! collapser.canHandleNullMask());
    }

    // The input is traversed using a TileStepper.

    const IPosition& inShape = latticeIn.shape();
    const uInt inDim = inShape.nelements();
    IPosition inTileShape = latticeIn.niceCursorShape(1024*1024);
    TileStepper inNav(inShape, inTileShape, collapseAxes);
    RO_LatticeIterator<T> inIter(latticeIn, inNav);

    // Precalculate various variables.

    const IPosition blc = IPosition(inShape.nelements(), 0);
    const IPosition trc = inShape - 1;
    const IPosition inc = IPosition(inShape.nelements(), 1);
    const uInt collDim = collapseAxes.nelements();
    const uInt iterDim = inDim - collDim;
    IPosition iterAxes(iterDim);
    IPosition outShape(latticeOut.shape());
    const uInt outDim = outShape.nelements();
    j = 0;
    for (i=0; i<outDim; ++i) {
	    if (ioMap(i) >= 0) {
	        outShape(i) = 1;
	        iterAxes(j++) = i;
	    }
    }

    // Find the first collapse axis which is not immediately after
    // the previous collapse axis.
    uInt collStart;
    for (collStart=1; collStart<collDim; ++collStart) {
	    if (collapseAxes(collStart) != 1+collapseAxes(collStart-1)) {
	        break;
	    }
    }
	
    // See if the output lattice has a writable pixelmask.
    // If so, it will later be used to write the resulting mask to.

    Lattice<Bool>* maskOut = 0;
    if (latticeOut.hasPixelMask()) {
        maskOut = &(latticeOut.pixelMask());
	    if (! maskOut->isWritable()) {
	        maskOut = 0;
	    }
    }

    // Set the number of expected steps.
    // This is the number of tiles to process.
    // Also give the number of resulting output pixels per line, so the
    // collapser can check it.

    uInt nsteps = 1;
    for (j=0; j<inDim; ++j) {
	    nsteps *= 1 + trc(j)/inTileShape(j) - blc(j)/inTileShape(j);
    }
    collapser.init (outShape.product());
    if (tellProgress != 0) {
        tellProgress->init (nsteps);
    }

    // Determine the axis where the collapsed values are stored in the output.
    // This is the first unmapped axis (the first axis when all axes are mapped).
    uInt resultAxis = 0;
    for (j=0; j<outDim; ++j) {
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

	    const Array<T>& iterCursor = inIter.cursor();
	    // In order to use the pointers-to-array-data below, the array *must*
	    // be contiguous or the results will in general be incorrect.
	    // Ditto for the mask
	    const Array<T>& cursor = iterCursor.contiguousStorage()
	    	? iterCursor : iterCursor.copy();
	    ThrowIf(
	    	! cursor.contiguousStorage(), "cursor array is not contiguous"
	    );
	    const IPosition& cursorShape = cursor.shape();
	    IPosition pos = inIter.position();
	    IPosition latPos = pos;
	    Array<Bool> mask;
	    if (useMask) {
	        // Casting const away is innocent.
	        ((MaskedLattice<T>&)latticeIn).getMaskSlice(mask, Slicer(pos, cursorShape));
	        if (! mask.contiguousStorage()) {
	        	mask = mask.copy();
	        	ThrowIf(
	        		! mask.contiguousStorage(), "mask array is not contiguous"
	        	);
	        }
	    }
	    for (j=0; j<outDim; ++j) {
	        if (ioMap(j) >= 0) {
		        uInt axis = ioMap(j);
		        iterPos(j) = pos(axis);
	        }
	    }
	    if (firstTime  ||  outPos != iterPos) {
	        if (!firstTime) {
		        Array<U> result;
		        Array<Bool> resultMask;
		        collapser.endAccumulator (result, resultMask, outShape);
		        latticeOut.putSlice (result, outPos);
		        if (maskOut != 0) {
		            maskOut->putSlice (resultMask, outPos);
		        }
	        }
	        firstTime = False;
	        outPos = iterPos;
	        uInt64 n1 = 1;
	        uInt64 n3 = 1;
	        for (j=0; j<outDim; ++j) {
		        if (ioMap(j) >= 0) {
		            outShape(j) = cursorShape(ioMap(j));
		            if (j < resultAxis) {
		                n1 *= outShape(j);
		            }
                    else {
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

	    IPosition chunkShape (inDim, 1);
	    for (j=0; j<collStart; ++j) {
	        const uInt axis = collapseAxes(j);
	        chunkShape(axis) = cursorShape(axis);
	    }
	    uInt nval = chunkShape.product();
	    const uInt axis = collapseAxes(0);

    	IPosition p0(inDim, 0);
	    IPosition p1(inDim, 0);
	    p1[axis] = 1;
	    // general for Arrays with contiguous or non-contiguous storage.
	    uInt dataIncr = &(cursor(p1)) - &(cursor(p0));
	    uInt maskIncr = useMask ? &(mask(p1)) - &(mask(p0)) : 0;

        // Iterate in the outer loop through the iterator axes.
        // Iterate in the inner loop through the collapse axes.

	    uInt index1 = 0;
	    uInt index3 = 0;
	    for (;;) {
	        for (;;) {
		        if (useMask) {
		            collapser.process (
                        index1, index3, &(cursor(curPos)), &(mask(curPos)),
				        dataIncr, maskIncr, nval, latPos, chunkShape
                    );
		        }
                else {
		            collapser.process(
                        index1, index3,
				        &(cursor(curPos)), 0,
				        dataIncr, maskIncr, nval, latPos, chunkShape
                    );
		        }
		        // Increment a collapse axis until all axes are handled.
		        for (j=collStart; j<collDim; ++j) {
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
	
	        for (j=0; j<iterDim; ++j) {
		        uInt arraxis = iterAxes(j);
		        uInt axis = ioMap(arraxis);
		        ++latPos(axis);
		        if (++curPos(axis) < cursorShape(axis)) {
		            if (arraxis < resultAxis) {
		                ++index1;
		            }
                    else {
		                ++index3;
			            index1 = 0;
		            }
		            break;
		        }
		        curPos(axis) = 0;
		        latPos(axis) = pos(axis);
	        }
	        if (j == iterDim) {
		        break;
	        }
	    }
	    ++inIter;
	    if (tellProgress != 0) {
            tellProgress->nstepsDone (inIter.nsteps());
        }
    }

    // Write out the last output array.
    Array<U> result;
    Array<Bool> resultMask;
    collapser.endAccumulator (result, resultMask, outShape);
    latticeOut.putSlice (result, outPos);
    if (maskOut != 0) {
        maskOut->putSlice (resultMask, outPos);
    }
    if (tellProgress != 0) tellProgress->done();
}



template <class T, class U>
IPosition LatticeApply<T,U>::prepare (const IPosition& inShape,
				    const IPosition& outShape,
				    const IPosition& collapseAxes,
				    Int newOutAxis)
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
    for (i=1; i<collDim; ++i) {
	AlwaysAssert (collapseAxes(i) > collapseAxes(i-1), AipsError);
    }
    // Get the first new output axis (i.e. the axis containing
    // the collapsed values). If not given, it is the first axis
    // for which input and output length mismatch.
    if (newOutAxis < 0) {
	newOutAxis = 0;
	for (i=collDim; i<inDim; ++i) {
	    uInt axis = allAxes(i);
	    if (inShape(axis) != outShape(newOutAxis)) {
		break;
	    }
	    ++newOutAxis;
	}
    }
    if (newOutAxis > Int(ndim)) {
	throw (AipsError ("LatticeApply::prepare - newOutAxis too high"));
    }
    // Make a little map of the input to the output axes.
    // ioMap(j) is the axis of the input that goes on output axis j.
    // -1 indicates that an output axis is a new axis (containing the
    // result of the collapse).
    // It checks if the length of axes match for input and output.
    IPosition ioMap(outDim, -1);
    uInt k=0;
    for (i=collDim; i<inDim; ++i) {
	uInt axis = allAxes(i);
	if (Int(k) == newOutAxis) {
	    k += outDim-ndim;
	}
	if (inShape(axis) != outShape(k)) {
	    throw (AipsError ("LatticeApply::prepare - "
			      "non-collapsed input and output shape mismatch"));
	}
	ioMap(k) = axis;
	++k;
    }
    return ioMap;
}

} //# NAMESPACE CASACORE - END


#endif
