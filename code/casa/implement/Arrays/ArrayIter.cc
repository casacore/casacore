//# ArrayIter.cc: Iterate an Array cursor through another Array
//# Copyright (C) 1993,1994,1995
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

#include <aips/Arrays/ArrayIter.h>
#include <aips/Arrays/ArrayError.h>

template<class T> ArrayIterator<T>::ArrayIterator(Array<T> &a, uInt byDim)
: ArrayPositionIterator(a.shape(), a.origin(), byDim), readOnly(False), ap(0)
{
    init(a);
}

template<class T> ArrayIterator<T>::ArrayIterator(Array<T> &a)
: ArrayPositionIterator(a.shape(), a.origin(), 1), readOnly(False), ap(0)
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

    IPosition blc(pOriginalArray->origin()), trc(pOriginalArray->origin());
    if (dimIter() < 1)
	throw(ArrayIteratorError("ArrayIterator<T>::ArrayIterator<T> - "
				 " at the moment cannot iterate by scalars"));
    IPosition whereNextStep(blc);

    // The step won't be taken if we are the same dimensionality.
    Bool step=False;
    if (dimIter() < a.ndim()) {
	// See if there is a dimension with more than one element left.
	// If so take a step in that one. Otherwise nothing to do.
	for (Int j=dimIter(); j<a.ndim(); j++)
	    if (pOriginalArray->length[j]>1) {
		whereNextStep(j) += 1;
		step=True;
		break;
	    }
    }

    for (Int i = 0; i < dimIter(); i++) {
	trc(i) = blc(i) + pOriginalArray->length[i] - 1;
    }

    // Calculate what the offset for ap->begin is for each step
    offset = ArrayIndexOffset(pOriginalArray->ndim(), 
			      pOriginalArray->originalLength.storage(),
			      pOriginalArray->start.storage(), 
			      pOriginalArray->inc.storage(), whereNextStep);
    if (!step) {
       offset = 9999999; // This should be obvious
    }
    // Now diddle with the internal array to ensure that it is the
    // correct shape. We only want to remove the last axes, not all
    // possible degenerate axes).
    if (dimIter() < pOriginalArray->ndim()) {
        ap = new Array<T>((*pOriginalArray)(blc,trc).nonDegenerate(dimIter()));
    } else {
        // Same dimentionality, no degenerate axes
        ap = new Array<T>(*pOriginalArray);
    }
}

// <thrown>
//     <item> ArrayIteratorError
// </thrown>
template<class T> void ArrayIterator<T>::apSetPointer()
{
    if (ap == 0)
	throw(ArrayIteratorError("ArrayIterator<T>::apSetPointer()"
				 " - no iteration array!"));
    if (pastEnd()) {
	ap->begin = 0;  // Mark it "invalid"
    } else {
	ap->begin = pOriginalArray->begin + nSteps()*offset;
    }
}

template<class T> void ArrayIterator<T>::origin()
{
    ArrayPositionIterator::origin();
    apSetPointer();
}

template<class T> void ArrayIterator<T>::next()
{
    ArrayPositionIterator::next();
    apSetPointer();
}

template<class T> ArrayIterator<T>::~ArrayIterator()
{
    if (pOriginalArray) delete pOriginalArray;
    if (ap) delete ap;
}
