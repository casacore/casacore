//# tDefaultValue.cc - tests the default value function
//# Copyright (C) 1996,2000,2001
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

#include <casacore/casa/aips.h>

#include <casacore/casa/Utilities/DefaultValue.h>

#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Assert.h>

#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
// a bogus test case of a specializtion for Strings
static void defaultValue(String &val)
{
  val = "defaultval";
}

int main()
{
  try {
    Int foo0;
    defaultValue(foo0);
    AlwaysAssert(foo0 == 0, AipsError);

    Float foo1;
    defaultValue(foo1);
    AlwaysAssert(foo1 == 0.0f, AipsError);

    Double foo2;
    defaultValue(foo2);
    AlwaysAssert(foo2 == 0.0f, AipsError);

    // test specializations
    String foo3;
    defaultValue(foo3);
    AlwaysAssert(foo3 == String("defaultval"), AipsError);

    } catch (AipsError x) {
        cout << "\nCaught an exception: " << x.getMesg() << endl;
    } 
 
    cout << "OK" << endl;
    return 0;
}


    

