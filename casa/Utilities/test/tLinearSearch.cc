//# tLinearSearch.cc: This program tests the linear search functions
//# Copyright (C) 1997,1999,2001
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

#if !defined(AIPS_DEBUG)
#define AIPS_DEBUG
#endif

#if !defined(AIPS_ARRAY_INDEX_CHECK)
#define AIPS_ARRAY_INDEX_CHECK
#endif

#include <casacore/casa/Utilities/LinearSearch.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>

#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
int main()
{
    Bool found;
    {
	IPosition ip1(1, 5);
	AlwaysAssertExit(linearSearch(found, ip1, 1, ip1.nelements()) == -1 && 
			 !found);
	AlwaysAssertExit(linearSearch1(ip1, 5) == 0);
	AlwaysAssertExit(linearSearch1(ip1, 10) == -1);
	AlwaysAssertExit(linearSearch(found, ip1, 5, 0u) == -1 && !found);
    }

    {
	IPosition ip1(3, 1, 5, 9);
	AlwaysAssertExit(linearSearch(found, ip1, 0, ip1.nelements()) == -1 && 
			 !found);
	AlwaysAssertExit(linearSearch(found, ip1, 1, ip1.nelements()) == 0 && 
			 found);
	AlwaysAssertExit(linearSearch(found, ip1, 3, ip1.nelements()) == -1 && 
			 !found);
	AlwaysAssertExit(linearSearch(found, ip1, 5, ip1.nelements()) == 1 && 
			 found);
	AlwaysAssertExit(linearSearch(found, ip1, 7, ip1.nelements()) == -1 && 
			 !found);
	AlwaysAssertExit(linearSearch(found, ip1, 9, ip1.nelements()) == 2 && 
			 found);
	AlwaysAssertExit(linearSearch(found, ip1, 10, ip1.nelements()) == -1 && 
			 !found);
    }

    {
	IPosition ip1(3, 9, 5, 1);
	AlwaysAssertExit(linearSearch(found, ip1, 0, ip1.nelements()) == -1 &&
			 !found);
	AlwaysAssertExit(linearSearch(found, ip1, 1, ip1.nelements()) == 2 &&
			 found);
	AlwaysAssertExit(linearSearch(found, ip1, 3, ip1.nelements()) == -1 &&
			 !found);
	AlwaysAssertExit(linearSearch(found, ip1, 5, ip1.nelements()) == 1 && 
			 found);
	AlwaysAssertExit(linearSearch(found, ip1, 7, ip1.nelements()) == -1 &&
			 !found);
	AlwaysAssertExit(linearSearch(found, ip1, 9, ip1.nelements()) == 0 &&
			 found);
	AlwaysAssertExit(linearSearch(found, ip1, 10, ip1.nelements()) == -1 && 
			 !found);
    }

    {
	int ia[4];  // int ia[] = {22,9,5,1}; isn't available on all compilers
	ia[0] = 22; ia[1] = 9; ia[2] = 5; ia[3] = 1;
	int *ip = &ia[0];
	AlwaysAssertExit((linearSearchBrackets(found, ip, 55, 4u) == -1) && 
			 !found);
	AlwaysAssertExit((linearSearchBrackets(found, ip, 22, 4u) == 0) && 
			 found);
	AlwaysAssertExit((linearSearchBrackets(found, ip, 11, 4u) == -1) && 
			 !found);
	AlwaysAssertExit((linearSearchBrackets(found, ip, 9, 4u) == 1) && found);
	AlwaysAssertExit((linearSearchBrackets(found, ip, 7, 4u) == -1) && 
			 
			 !found);
	AlwaysAssertExit((linearSearchBrackets(found, ip, 5, 4u) == 2) && found);
	AlwaysAssertExit((linearSearchBrackets(found, ip, 3, 4u) == -1) && 
			 !found);
	AlwaysAssertExit((linearSearchBrackets(found, ip, 1, 4u) == 3) && found);
	AlwaysAssertExit((linearSearchBrackets(found, ip, -99, 4u) == -1) && 
			 !found);
	AlwaysAssertExit((linearSearchBrackets(found, ip, -99, 0u) == -1) && 
			 !found);
    }

    {
	int ia[4];
	ia[0] = 1; ia[1] = 5; ia[2] = 9; ia[3] = 22;
	int *ip = &ia[0];
	AlwaysAssertExit((linearSearchBrackets(found, ip, 55, 4u) == -1) && 
			 !found);
	AlwaysAssertExit((linearSearchBrackets(found, ip, 22, 4u) == 3) && 
			 found);
	AlwaysAssertExit((linearSearchBrackets(found, ip, 11, 4u) == -1) && 
			 !found);
	AlwaysAssertExit((linearSearchBrackets(found, ip, 9, 4u) == 2) && found);
	AlwaysAssertExit((linearSearchBrackets(found, ip, 7, 4u) == -1) && 
			 !found);
	AlwaysAssertExit((linearSearchBrackets(found, ip, 5, 4u) == 1) && found);
	AlwaysAssertExit((linearSearchBrackets(found, ip, 3, 4u) == -1) && 
			 !found);
	AlwaysAssertExit((linearSearchBrackets(found, ip, 1, 4u) == 0) && found);
	AlwaysAssertExit((linearSearchBrackets(found, ip, -99, 4u) == -1) && 
			 !found);
    
    }

    {
	int ia[4];
	ia[0] = 1; ia[1] = 5; ia[2] = 9; ia[3] = 22;
	int *ip = &ia[0];
	AlwaysAssertExit((linearSearchBrackets(found, ip, 55, 2u, 2) == -1) && 
			 !found);
	AlwaysAssertExit((linearSearchBrackets(found, ip, 22, 2u, 2) == 3) && 
			 found);
	AlwaysAssertExit((linearSearchBrackets(found, ip, 11, 2u, 2) == -1) && 
			 !found);
	AlwaysAssertExit((linearSearchBrackets(found, ip, 9, 2u, 2) == 2) &&
			 found);
	AlwaysAssertExit((linearSearchBrackets(found, ip, 7, 2u, 2) == -1) && 
			 !found);
	AlwaysAssertExit((linearSearchBrackets(found, ip, 5, 2u, 2) == -1) && 
			 !found);
	AlwaysAssertExit((linearSearchBrackets(found, ip, 3, 2u, 2) == -1) && 
			 !found);
	AlwaysAssertExit((linearSearchBrackets(found, ip, 1, 2u, 2) == -1) &&
			 !found);
	AlwaysAssertExit((linearSearchBrackets(found, ip, -99, 2u, 2) == -1) && 
			 !found);
    
    }

    {
        static int ia[4]; // all 0 by language rules
	int *ip = &ia[0];
	AlwaysAssertExit((linearSearchBrackets(found, ip, 0, 4u) == 0) && 
			 found);
	AlwaysAssertExit((linearSearchBrackets(found, ip, -1, 4u) == -1) && 
			 !found);
	AlwaysAssertExit((linearSearchBrackets(found, ip, +1, 4u) == -1) && 
			 !found);
    }

    cout << "OK\n";
    return 0;
}
