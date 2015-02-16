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

#ifndef CASA_ARRAYITER_TCC
#define CASA_ARRAYITER_TCC

#include <casacore/casa/Arrays/ArrayIter.h>
#include <casacore/casa/Arrays/ArrayError.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T> ArrayIterator<T>::ArrayIterator(const Array<T> &a, uInt byDim)
: ArrayPositionIterator(a.shape(), byDim),
  ap_p(0)
{
    init(a);
}

template<class T> ArrayIterator<T>::ArrayIterator(const Array<T> &a,
						  const IPosition &axes,
						  Bool axesAreCursor)
: ArrayPositionIterator(a.shape(), axes, axesAreCursor),
  ap_p(0)
{
    init(a);
}

template<class T> ArrayIterator<T>::~ArrayIterator()
{
    delete ap_p;
}


// <thrown>
//     <item> ArrayIteratorError
// </thrown>
template<class T> void ArrayIterator<T>::init(const Array<T> &a)
{
    pOriginalArray_p.reference (a);
    dataPtr_p = pOriginalArray_p.begin_p;

    if (dimIter() < 1)
	throw(ArrayIteratorError("ArrayIterator<T>::ArrayIterator<T> - "
				 " at the moment cannot iterate by scalars"));
    IPosition blc(pOriginalArray_p.ndim(), 0);
    IPosition trc(pOriginalArray_p.endPosition());

    // Calculate what the offset for ap_p->begin is for each step.
    // The offset is the value that has to be added to dataPtr_p to go
    // to the next chunk.
    // The offset calculation must match the way nextStep is iterating.
    // The iteration is such that shape(i)-1 steps are made for axis i.
    const IPosition& iAxes = iterAxes();
    const IPosition& steps = pOriginalArray_p.steps();
    const IPosition& shape = pOriginalArray_p.shape();
    offset_p.resize (a.ndim());
    offset_p = 0;
    Int lastoff = 0;
    for (uInt i=0; i<iAxes.nelements(); i++) {
        uInt axis = iAxes(i);
	if (trc(axis) > 0) trc(axis) = 0;
	offset_p(axis) = steps(axis) - lastoff;
	lastoff += (shape(axis)-1)*steps(axis);
    }
    // Now diddle with the internal array to ensure that it is the
    // correct shape. We only want to remove the iteration axes, not the
    // possible degenerate axes in the cursor).
    if (dimIter() < pOriginalArray_p.ndim()) {
        ap_p = new Array<T>(pOriginalArray_p(blc,trc).nonDegenerate(cursorAxes()));
    } else {
        // Same dimensionality, so no degenerate axes
        ap_p = new Array<T>(pOriginalArray_p);
    }
}

// <thrown>
//     <item> ArrayIteratorError
// </thrown>
template<class T> void ArrayIterator<T>::apSetPointer(Int stepDim)
{
    if (ap_p == 0)
	throw(ArrayIteratorError("ArrayIterator<T>::apSetPointer()"
				 " - no iteration array!"));
    if (pastEnd()) {
	ap_p->begin_p = 0;  // Mark it "invalid"
    } else {
        if (stepDim < 0) {
	    dataPtr_p = pOriginalArray_p.begin_p;
	} else {
	    dataPtr_p += offset_p(stepDim);
	}
	ap_p->begin_p = dataPtr_p;
	ap_p->setEndIter();
    }
}

template<class T> void ArrayIterator<T>::reset()
{
    ArrayPositionIterator::reset();
    apSetPointer(-1);
}

template<class T> void ArrayIterator<T>::next()
{
    Int stepDim = ArrayPositionIterator::nextStep();
    apSetPointer(stepDim);
}

  
template<class T> void ArrayIterator<T>::set (const IPosition& cursorPos)
{
    ArrayPositionIterator::set (cursorPos);
    if (ap_p == 0)
	throw(ArrayIteratorError("ArrayIterator<T>::apSetPointer()"
				 " - no iteration array!"));
    if (pastEnd()) {
	ap_p->begin_p = 0;  // Mark it "invalid"
    } else {
        dataPtr_p = &(pOriginalArray_p(pos()));
	ap_p->begin_p = dataPtr_p;
	ap_p->setEndIter();
    }  
}

template<class T> ArrayBase& ArrayIterator<T>::getArray()
{
    return array();
}

} //# NAMESPACE CASACORE - END

#endif
