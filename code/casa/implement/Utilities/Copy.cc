//# Copy.cc: Copy objects from one c-array to another, with optional strides.
//# Copyright (C) 1994,1995
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

#include <casa/Utilities/Copy.h>
#include <casa/Exceptions/Error.h>

//#  Note of Ger van Diepen on 7-Nov-1997:
//# For one reason or another the compiler did not compile
//#    objcopy (MPosition*, const MPosition*, 2)
//# correctly when   *from++   was used in e.g.  *to++ = *from++.
//# It appeared that the from pointer was updated incorrectly.
//# Therefore everywhere the from pointer is updated explicitly using
//#    from += 1;


template<class T> void objmove (T* to, const T* from, uInt n)
{
    if (n==0)
	return;
    if (to == 0  ||  from == 0) {
	throw(AipsError("template<class T> void objmove(T* to, const T* from,"
			" uInt n) - to or from is null"));
    }
    if (to < from  ||  to >= from+n) {
	while (n--) {
	    *to++ = *from;
	    from += 1;
	}
    }else{
	to += n;
	from +=n;
	while (n--) {
	    from -= 1;
	    *--to = *from;
	}
    }
}

template<class T> void objmove (T* to, const T* from, uInt n, uInt toStride,
				uInt fromStride)
{
    if (n==0) {
	return;
    }
    if (to == 0 || from == 0 || toStride < 1 || fromStride < 1) {
	throw(AipsError("template<class T> void objmove(T* to, const T* from,"
			"uInt n, uInt toStride, uInt fromStride) - "
			"illegal argument"));

    }

    if (toStride == 1  &&  fromStride == 1) {
	objmove (to, from, n);
	return;
    }

    // It's not a simple block move.
    // The to and from interval may overlap, so determine
    // if we have to start moving from the left or right.
    uInt nLeft = n;
    uInt startLeft = 0;
    uInt startRight = n;

    // First test if the to and from intervals are disjoint.
    // If so, we can move everything from the left.
    if (to + n*toStride <= from  ||  to >= from + n*fromStride) {
	nLeft = n;
    }else{

	// Alas not disjoint.
	// When the strides are equal, we can also move everything
	// from the left when to starts before from.
	// Otherwise everything has to be moved from the right.
	if (toStride == fromStride) {
	    if (to <= from) {
		nLeft = n;
	    }else{
		nLeft = 0;
	    }
	}else{

	    // Hmm, it's getting more complex.
	    // First consider the case toStride > fromStride.
	    // When to starts after from, move everything from the right.
	    // When to ends before the end of from, move from the left.
	    if (toStride > fromStride) {
		if (to >= from) {
		    nLeft = 0;
		}else if (to + n*toStride <= from + n*fromStride) {
		    nLeft = n;
		}else{

		    // The intervals overlap in a way that part has to be
		    // moved from the left, part from the right.
		    // Determine the crosspoint.
		    nLeft = (from-to) / (toStride-fromStride);
		    if (nLeft > n) {
			nLeft = n;
		    }
		}
	    }else{

		// This case is the opposite from the previous one.
		// However, the first part has to be moved from the right
		// and the last part from the left.
		if (from >= to) {
		    nLeft = n;
		}else if (from + n*fromStride <= to + n*toStride) {
		    nLeft = 0;
		}else{
		    startRight = (to-from) / (fromStride-toStride);
		    if (startRight > n) {
			startRight = n;
		    }
		    startLeft = startRight;
		    nLeft = n - startRight;
		}
	    }
	}
    }

    // Now do the actual moves.
    n -= nLeft;
    if (nLeft > 0) {
	const T* fromPtr = from + startLeft*fromStride;
	T* toPtr = to + startLeft*toStride;
	while (nLeft--) {
	    *toPtr = *fromPtr;
	    fromPtr += fromStride;
	    toPtr += toStride;
	}
    }
    // Do the moves from the right.
    if (n > 0) {
	const T* fromPtr = from + startRight*fromStride;
	T* toPtr = to + startRight*toStride;
	while (n--) {
	    fromPtr -= fromStride;
	    toPtr -= toStride;
	    *toPtr = *fromPtr;
	}
    }
}



template<class T> void objcopy (T* to, const T* from, uInt n)
{
    if (n > 0 && (to == 0 || from == 0)) {
	throw(AipsError("template<class T> void objcopy(T* to, const T* from,"
			"uInt n) - to or from is null"));
    }
    while (n--) {
	*to++ = *from;
	from += 1;
    }
}

template<class T> void objcopy (T* to, const T* from, uInt n, uInt toStride,
			        uInt fromStride)
{
    if (n==0) {
	return;
    }
    if (to == 0 || from == 0 || toStride < 1 || fromStride < 1) {
	throw(AipsError("template<class T> void objcopy(T* to, const T* from,"
			"uInt n, uInt toStride, uInt fromStride) - "
			"illegal argument"));
    }

    if (toStride == 1  &&  fromStride == 1) {
	objcopy (to, from, n);
    } else {
	while (n--) {
	    *to = *from;
	    to += toStride;
	    from += fromStride;
	}
    }
}



template<class T> void objset (T* to, T fillValue, uInt n)
{
    if (n > 0 && to == 0) {
	throw(AipsError("template<class T> void objset(T* to, T fillvalue"
			"fillValue, uInt n) - to is null"));
    }
    while (n--) *to++ = fillValue;
}

template<class T> void objset (T* to, T fillValue, uInt n, uInt toStride)
{
    if (toStride < 1 || (n > 0 && to == 0)) {
	throw(AipsError("template<class T> void objset(T* to, T fillValue"
			"fillValue, uInt n, uInt toStride) - "
			"invalid arguments"));
    }
    if (toStride == 1) {
	while(n--) *to++ = fillValue;
    } else {
	while(n--) {
	    *to = fillValue;
	    to += toStride;
	}
    }
}
