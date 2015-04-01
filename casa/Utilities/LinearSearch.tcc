//# LinearSearch.cc: Linear search through linear data structures
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
//#
//# $Id$

#ifndef CASA_LINEARSEARCH_TCC
#define CASA_LINEARSEARCH_TCC


//# Includes
#include <casacore/casa/Utilities/LinearSearch.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class Container, class ElType>
Int linearSearch (Bool& found, const Container& container, 
		  const ElType& value, uInt n, uInt lower)
{
    n += lower;
    while (lower < n) {
	if (container(lower) == value) {
	    found = True;
	    return lower;
	}
	lower++;
    }
    found = False;
    return -1;
}

template<class Container, class ElType>
Int linearSearch1 (const Container& container, const ElType& value,
		   uInt lower)
{
    uInt n = container.nelements();
    while (lower < n) {
	if (container(lower) == value) {
	    return lower;
	}
	lower++;
    }
    return -1;
}

template<class Container, class ElType>
Int linearSearchBrackets (Bool& found, const Container& container, 
			  const ElType& value, uInt n, uInt lower)
{
    n += lower;
    while (lower < n) {
	if (container[lower] == value) {
	    found = True;
	    return lower;
	}
	lower++;
    }
    found = False;
    return -1;
}

template<class Container, class ElType>
Int linearSearchBrackets1 (const Container& container, const ElType& value,
			   uInt lower)
{
    uInt n = container.nelements();
    while (lower < n) {
	if (container[lower] == value) {
	    return lower;
	}
	lower++;
    }
    return -1;
}

} //# NAMESPACE CASACORE - END


#endif
