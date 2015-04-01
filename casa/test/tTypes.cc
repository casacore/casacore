//# tTypes.cc: Test that fundamental types are valid for Casacore
//# Copyright (C) 1998
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

//# Includes


#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/namespace.h>

int main()
{
    {
	// Make sure that Char is signed, uChar is unsigned.
	Char c = 0;
	c--;
	Int C = c;
	AlwaysAssertExit(C == -1);
	uChar cc = 0;
	cc--;
	C = cc;
	AlwaysAssertExit(C == 255);
    }
    {
	// Make sure the sizes are OK.
      AlwaysAssertExit(sizeof(Int) == 4 && sizeof(uInt) == 4 &&
                       sizeof(Short) == 2 && sizeof(uShort) == 2 &&
                       sizeof(Int64) == 8 && sizeof(uInt64) == 8 &&
                       sizeof(Float) == 4 && sizeof(Double) == 8 &&
                       sizeof(lDouble) >= sizeof(Double));
    }

    return 0;
}
