//# Copyright (C) 1995,1996,1997,1998,1999,2000,2001,2002,2003
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
//# $Id: tArrayLattice.cc 20632 2009-06-14 12:16:13Z gervandiepen $

#include <casacore/casa/aips.h>
#include <casacore/casa/namespace.h>

#include <casacore/lattices/Lattices/ArrayLattice.h> 
#include <casacore/lattices/LatticeMath/LatticeStatsDataProvider.h>

int main() {
	try {
		Array<Float> array1(IPosition(3,1024, 1024, 1024));
		ArrayLattice<Float> lat(array1);
		LatticeStatsDataProvider<Float> dataProvider(lat);
		dataProvider.reset();
		while (! dataProvider.atEnd()) {
			++dataProvider;
			cout << dataProvider.getCount() << endl;
		}
	}
	catch (const AipsError& x) {
		cerr << x.getMesg () << endl;
		cout << "FAIL" << endl;
		return 1;
	}
	cout << "OK" << endl;
	return 0;
}
