//# tPtrHolder.cc: Test the tPtrHolder<T> class.
//# Copyright (C) 1994,1995,2000,2001
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

//# Includes

#include <casacore/casa/Utilities/PtrHolder.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
// This program should not have any memory leaks.

void do_nothing(Int *dummy)
{
    AlwaysAssertExit(dummy == 0);
}

void do_nothing2(const Int *dummy)
{
    AlwaysAssertExit(dummy == 0);
}

void tSPtr()
{
    {
	Int *ip = new Int;
	SPtrHolder<Int> ph(ip);            // PtrHolder(T *, Bool=False)
	AlwaysAssertExit(ph.ptr() == ip);
	Int *ip2 = ph.transfer();
	AlwaysAssertExit(ip2 == ip);
	AlwaysAssertExit(ph.ptr() == 0);
	do_nothing (ph.ptr());
	do_nothing2 (ph.ptr());
	delete ip2;
    }
    {
	// Throw an exception to make sure nothing is leaked
	Bool isCaught = False;
	try {
	    Int *ip = new Int;
	    SPtrHolder<Int> ph(ip);
	    throw(AipsError("testing..."));
	} catch (AipsError x) {
	    isCaught = True;
	} 
	AlwaysAssertExit(isCaught);
    }
}

int main()
{
    {
	PtrHolder<Int> ph;               // PtrHolder()
	AlwaysAssertExit(ph.ptr() == 0);           // ptr()
	do_nothing(ph);                  // operator T*()
	// Check that the conversion operator will also match const T*
	// functions. It does under CFront, if it doesn't under some
	// other compiler it may be necessary to add a constT* conversion
	// operator to PtrHolder.
	do_nothing2(ph);
    }
    {
	Int *ip = new Int[1000];
	PtrHolder<Int> ph(ip, True);      // PtrHolder(T *, Bool=True)
	AlwaysAssertExit(ph.isCArray() == True);    // isCarray()
	AlwaysAssertExit(ph.ptr() == ip);
    }
    {
	Int *ip = new Int;
	PtrHolder<Int> ph(ip);            // PtrHolder(T *, Bool=False)
	AlwaysAssertExit(ph.isCArray() == False);
	AlwaysAssertExit(ph.ptr() == ip);
    }
    {
	Int *ip = new Int[1000];
	PtrHolder<Int> ph(ip, True);
	Int *ip2 = new Int[1000];
	ph.set(ip2, True);           // set(T*,Bool,Bool=True)
	delete [] ip2;
	Int *ip3 = new Int[1000];
	// Shouldn't set off a double deletion
	ph.set(ip3, True, False);    // set(T*,Bool,Bool=False)
	ph.clear();                  // clear(Bool=True);
	AlwaysAssertExit(ph.ptr() == 0);
    }
    {
	// Throw an exception to make sure nothing is leaked
	Bool isCaught = False;
	try {
	    Int *ip = new Int[1000];
	    PtrHolder<Int> ph(ip, True);
	    throw(AipsError("testing..."));
	} catch (AipsError x) {
	    isCaught = True;
	} 
	AlwaysAssertExit(isCaught);
    }

    tSPtr();
    cout << "OK" << endl;
    return 0;
}
