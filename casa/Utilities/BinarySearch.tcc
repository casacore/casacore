//# BinarySearch.cc: Binary search through linear, sorted, data structures
//# Copyright (C) 1995,1996,2001
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

#ifndef CASA_BINARYSEARCH_TCC
#define CASA_BINARYSEARCH_TCC

#include <casacore/casa/Utilities/BinarySearch.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//#!!!!! If you change either of the following, make sure you change the other
//#!!!!! similarly.

template<class Container, class ElType>
     Int binarySearch(Bool &found, const Container &container, 
		      const ElType &value, uInt n, Int originalLower)
{
    found = False;
    if (n == 0) {
	return 0;
    }

    Int lower = originalLower;
    Int upper = lower + n - 1;
    Int middle = 0;

    Bool ascending = (! (container(upper) < container(lower)));
    Bool toLeft, toRight;

    ElType midval;

    while (lower <= upper) {
	middle = (upper + lower) / 2;
	midval = container(middle);
	if (ascending) {
	    toLeft = (value < midval);
	} else {
	    toLeft = (value > midval);
	}
        if (toLeft) {
            upper = middle - 1;
	} else {
	    if (ascending) {
		toRight = (value > midval);
	    } else {
		toRight = (value < midval);
	    }
	    if (toRight) {
		middle++;
		lower = middle;
	    } else {
	        // exact match, but still we want to get to the beginning of
	        // sequence
	        upper = middle - 1;
		found = True;
	    }
        }
    }
    
    return middle;
}

template<class Container, class ElType>
     Int binarySearchBrackets(Bool &found, const Container &container, 
			      const ElType &value, uInt n, Int originalLower)
{
    found = False;
    if (n == 0) {
	return 0;
    }

    Int lower = originalLower;
    Int upper = lower + n - 1;
    Int middle = 0;

    Bool ascending = (! (container[upper] < container[lower]));
    Bool toLeft, toRight;

    ElType midval;

    while (lower <= upper) {
	middle = (upper + lower) / 2;
	midval = container[middle];
	if (ascending) {
	    toLeft = (value < midval);
	} else {
	    toLeft = (value > midval);
	}
        if (toLeft) {
            upper = middle - 1;
	} else {
	    if (ascending) {
		toRight = (value > midval);
	    } else {
		toRight = (value < midval);
	    }
	    if (toRight) {
		middle++;
		lower = middle;
	    } else {
	        // exact match, but still we want to get to the beginning of
	        // sequence
	        upper = middle - 1;
		found = True;
	    }
        }
    }
    
    return middle;
}




} //# NAMESPACE CASACORE - END


#endif
