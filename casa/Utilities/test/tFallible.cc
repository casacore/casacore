//# tFallible: Test Fallible class
//# Copyright (C) 1994,1995,2000,2001
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

#include <casacore/casa/Utilities/Fallible.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
void Foo(Int x)
{
    AlwaysAssertExit(x == 5);
}

int main()
{
    // None of the following should cause an exception
    Fallible<Int> fi(5);
    Foo(fi);
    AlwaysAssertExit(fi.value() == 5);
    Fallible<Int> fi2(10);
    fi = fi2;
    AlwaysAssertExit(fi.value() == 10);
    Fallible<Int> fi3(fi2);
    AlwaysAssertExit(fi3.value() == 10);
    AlwaysAssertExit(fi3.isValid() == True);
    Fallible<Int> fi4;
    AlwaysAssertExit(fi4.isValid() == False);
    Fallible<Int> fi5(fi4);

    // All of the following should cause an exception
    Bool caught = False;
    try {
	Foo(fi4);
    } catch (AipsError x) {
	caught = True;
    } 
    AlwaysAssertExit(caught);
    caught = False;
    try {
        fi5.value();
    } catch (AipsError x) {
	caught = True;
    } 
    AlwaysAssertExit(caught);
	
    cout << "OK" << endl;
    return 0;
}
