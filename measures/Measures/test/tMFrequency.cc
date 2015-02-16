//# Copyright (C) 1998,1999,2000,2001
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
//# $Id: HostInfoDarwin.h 21521 2014-12-10 08:06:42Z gervandiepen $

#include <casacore/measures/Measures/MFrequency.h>

#include <casacore/casa/aips.h>
#include <casacore/casa/namespace.h>

int main() {
	try {
		AlwaysAssert(
			MFrequency::typeFromString("LSRK") == MFrequency::LSRK,
			AipsError
		);
		Bool except = False;
		try {
			MFrequency::typeFromString("J2000");
		}
		catch (const AipsError& x) {
			except = True;
		}
		AlwaysAssert(except, AipsError);
		cout << "ok" << endl;
	}
	catch (const AipsError& x) {
		cout << "tMFrequency failed: " << x.getMesg() << endl;
		return 1;
	}
	return 0;
}
