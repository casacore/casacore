//# ArrayIter.cc: Iterate an Array cursor through another Array
//# Copyright (C) 1993,1994,1995,1997,1999,2003
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

#include <casa/Arrays/ArrayIter.h>
#include <casa/Arrays/ArrayError.h>

template<class T> ArrayIterator<T>::ArrayIterator(Array<T> &a, uInt byDim)
: ArrayPositionIterator(a.shape(), byDim),
  ap(0),
  readOnly(False)
{
    init(a);
}

template<class T> ArrayIterator<T>::ArrayIterator(Array<T> &a)
: ArrayPositionIterator(a.shape(), 1),
  ap(0),
  readOnly(False)
{
    init(a);
}


// <thrown>
//     <item> ArrayIteratorError
// </thrown>
template<class T> void ArrayIterator<T>::init(Array<T> &a)
{
    pOriginalArray = new Array<T>(a);
    if (!pOriginalArray) {
	throw(ArrayIteratorError("ArrayIterator<T>::init(a) - "
              " failed to make new Array<t>(a) for pOriginalArray"));
    }
    dataPtr = pOriginalArray->begin_p;

    if (dimIter() < 1)
	throw(ArrayIteratorError("ArrayIterator<T>::ArrayIterator<T> - "
				 " at the moment cannot iterate by scalars"));
    IPosition blc(pOriginalArray->ndim(), 0);
    IPosition trc(blc);
    for (uInt i = 0; i < dimIter(); i++) {
	trc(i) += pOriginalArray->length_p(i) - 1;
    }

    // Calculate what the offset for ap->begin is for each step
    IPosition whereFirstElem(blc);
    IPosition whereLastElem(blc);
    offset.resize (a.ndim());
    offset = 0;
    // The step won't be taken if we are the same dimensionality
    // or if all remaining axes have length 1.
    Bool step=False;
    if (dimIter() < a.ndim()) {
	// See if there is a dimension with more than one element left.
	// If so take a step in that one. Otherwise nothing to do.
        uInt lastOffset = 0;
        for (uInt j=dimIter(); j<a.ndim(); j++) {
	    if (pOriginalArray->length_p(j) > 1) {
		step=True;
		// Determine offset in this dimension, thus the offset
		// from element 0 to 1 in that dimension.
		// For further dimensions we need the offset from the
		// last element in the previous dimension to the first
		// in this one.
		whereFirstElem(j) = 1;
		offset(j) = ArrayIndexOffset(pOriginalArray->ndim(), 
				   pOriginalArray->originalLength_p.storage(),
				   pOriginalArray->inc_p.storage(),
				   whereFirstElem);
		offset(j) -= lastOffset;
		whereLastElem(j) = a.shape()(j)-1;
		lastOffset = ArrayIndexOffset(pOriginalArray->ndim(), 
				   pOriginalArray->originalLength_p.storage(),
				   pOriginalArray->inc_p.storage(),
				   whereLastElem);
		whereFirstElem(j) = 0;
	    }
	}
    }

    // Now diddle with the internal array to ensure that it is the
    // correct shape. We only want to remove the last axes, not all
    // possible degenerate axes).
    if (dimIter() < pOriginalArray->ndim()) {
        ap = new Array<T>((*pOriginalArray)(blc,trc).nonDegenerate(dimIter()));
    } else {
        // Same dimensionality, so no degenerate axes
        ap = new Array<T>(*pOriginalArray);
    }
}

// <thrown>
//     <item> ArrayIteratorError
// </thrown>
template<class T> void ArrayIterator<T>::apSetPointer(Int stepDim)
{
    if (ap == 0)
	throw(ArrayIteratorError("ArrayIterator<T>::apSetPointer()"
				 " - no iteration array!"));
    if (pastEnd()) {
	ap->begin_p = 0;  // Mark it "invalid"
    } else {
        if (stepDim < 0) {
	    dataPtr = pOriginalArray->begin_p;
	} else {
	    dataPtr += offset(stepDim);
	}
	ap->begin_p = dataPtr;
	ap->setEndIter();
    }
}

template<class T> void ArrayIterator<T>::origin()
{
    ArrayPositionIterator::origin();
    apSetPointer(-1);
}

template<class T> void ArrayIterator<T>::next()
{
    Int stepDim = ArrayPositionIterator::nextStep();
    apSetPointer(stepDim);
}

template<class T> ArrayIterator<T>::~ArrayIterator()
{
    if (pOriginalArray) delete pOriginalArray;
    if (ap) delete ap;
}
