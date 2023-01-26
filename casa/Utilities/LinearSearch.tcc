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

#ifndef CASA_LINEARSEARCH_TCC
#define CASA_LINEARSEARCH_TCC


//# Includes
#include <casacore/casa/Utilities/LinearSearch.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class Container, class ElType>
int32_t linearSearch (bool& found, const Container& container, 
		  const ElType& value, uint32_t n, uint32_t lower)
{
    n += lower;
    while (lower < n) {
	if (container(lower) == value) {
	    found = true;
	    return lower;
	}
	lower++;
    }
    found = false;
    return -1;
}

template<class Container, class ElType>
int32_t linearSearch1 (const Container& container, const ElType& value,
		   uint32_t lower)
{
    uint32_t n = container.nelements();
    while (lower < n) {
	if (container(lower) == value) {
	    return lower;
	}
	lower++;
    }
    return -1;
}

template<class Container, class ElType>
int32_t linearSearchBrackets (bool& found, const Container& container, 
			  const ElType& value, uint32_t n, uint32_t lower)
{
    n += lower;
    while (lower < n) {
	if (container[lower] == value) {
	    found = true;
	    return lower;
	}
	lower++;
    }
    found = false;
    return -1;
}

template<class Container, class ElType>
int32_t linearSearchBrackets1 (const Container& container, const ElType& value,
			   uint32_t lower)
{
    uint32_t n = container.nelements();
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
