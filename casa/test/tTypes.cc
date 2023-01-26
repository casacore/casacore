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

//# Includes


#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/namespace.h>

int main()
{
    {
	// Make sure that unsigned char is unsigned.
	unsigned char cc = 0;
	cc--;
	int32_t C = cc;
	AlwaysAssertExit(C == 255);
    }
    {
	// Make sure the sizes are OK.
      AlwaysAssertExit(sizeof(int32_t) == 4 && sizeof(uint32_t) == 4 &&
                       sizeof(int16_t) == 2 && sizeof(uint16_t) == 2 &&
                       sizeof(int64_t) == 8 && sizeof(uint64_t) == 8 &&
                       sizeof(float) == 4 && sizeof(double) == 8 &&
                       sizeof(long double) >= sizeof(double));
    }

    return 0;
}
