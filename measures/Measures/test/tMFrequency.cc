//# Copyright (C) 1998,1999,2000,2002
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
//#

#include <measures/Measures/MFrequency.h>

#include <casa/aips.h>
#include <casa/namespace.h>

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
