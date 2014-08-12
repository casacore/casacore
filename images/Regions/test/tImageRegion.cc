//# Copyright (C) 1998,1999,2000,2001,2003
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

#include <images/Regions/ImageRegion.h>

#include <casa/OS/Directory.h>
#include <coordinates/Coordinates/CoordinateUtil.h>
#include <images/Images/PagedImage.h>

#include <casa/namespace.h>

int main () {
	String myname = "tmp.im";
	int ret = 0;
	try {
		PagedImage<Float> im(
			TiledShape(IPosition(4, 1)),
			CoordinateUtil::defaultCoords4D(), myname
		);
		im.flush();
		vector<String> names;
		names.push_back("tmp.im");
		names.push_back("'tmp.im'");
		names.push_back("'./tmp.im'");
		names.push_back("'$PWD/tmp.im'");
		names.push_back("./tmp.im");
		names.push_back("$PWD/tmp.im");
		// various escaping tests for fromLatticeExpession
		uInt lastGood = 3;
		for (uInt i=0; i<names.size(); i++) {
			for (uInt j=0; j<names.size(); j++) {
				String expr = names[i] + " == " + names[j];
				try {
					ImageRegion *z = ImageRegion::fromLatticeExpression(
						expr
					);
					AlwaysAssert(z && i <=lastGood && j <= lastGood, AipsError);
				}
				catch (AipsError& x) {
					AlwaysAssert(i > lastGood || j > lastGood, AipsError);
				}
			}
		}
		cout << "OK" << endl;
	}
	catch (const AipsError& x) {
		cerr << "Caught exception: " << x.getMesg() << endl;
		cout << "FAIL" << endl;
		ret = 1;
	}
	Directory d(myname);
	if (d.exists()) {
		d.removeRecursive(False);
	}
	return ret;
}
