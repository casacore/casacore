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

#include <casa/aips.h>
#include <casa/Exceptions/Error.h>
#include <measures/Measures/MDirection.h>
#include <casa/namespace.h>

Bool testShiftAngle() {
	Double rav = 30;
	Double decv = 40;
	Quantity ra(rav, "deg");
	Quantity dec(decv, "deg");
	MDirection x(ra, dec);
	Quantity offset(4, "arcmin");
	Quantity pa(0, "deg");
	x.shiftAngle(offset, pa);
	Quantum<Vector<Double> > angle = x.getAngle();
	AlwaysAssert(
		abs((angle.getValue("deg")[0] - rav)/rav) < 1e-6,
		AipsError
	);
	Double exp = (dec + offset).getValue("deg");
	AlwaysAssert(
		abs((angle.getValue("deg")[1] - exp)/exp) < 1e-6,
		AipsError
	);

	x = MDirection(ra, dec);
	pa = Quantity(90, "deg");
	x.shiftAngle(offset, pa);
	exp = rav + offset.getValue("deg")/cos(x.getAngle().getValue("rad")[1]);
	angle = x.getAngle();
	AlwaysAssert(
		abs((angle.getValue("deg")[0] - exp)/exp) < 1e-6,
		AipsError
	);
	exp = (decv);
	cout << (angle.getValue("deg")[1] - exp) << endl;
	AlwaysAssert(
		abs((angle.getValue("deg")[1] - exp)/exp) < 1e-6,
		AipsError
	);

	x = MDirection(ra, dec);
	pa = Quantity(-90, "deg");
	x.shiftAngle(offset, pa);
	exp = rav - offset.getValue("deg")/cos(x.getAngle().getValue("rad")[1]);
	angle = x.getAngle();
	AlwaysAssert(
		abs((angle.getValue("deg")[0] - exp)/exp) < 1e-6,
		AipsError
	);
	exp = (decv);
	AlwaysAssert(
		abs((angle.getValue("deg")[1] - exp)/exp) < 1e-6,
		AipsError
	);

	x = MDirection(ra, dec);
	pa = Quantity(180, "deg");
	x.shiftAngle(offset, pa);
	exp = rav;
	angle = x.getAngle();
	AlwaysAssert(
		abs((angle.getValue("deg")[0] - exp)/exp) < 1e-6,
		AipsError
	);
	exp = decv - offset.getValue("deg");
	AlwaysAssert(
		abs((angle.getValue("deg")[1] - exp)/exp) < 1e-6,
		AipsError
	);

	return True;
}


int main() {
	try {
		Bool success = True;
		success = success && testShiftAngle();

		if (success) {
			cout << "tMDirection succeeded" << endl;
			return 0;
		}
		else {
			cout << "tMDirection failed" << endl;
			return 1;
		}
	}

	catch (AipsError x) {
		cout << "tMDirection failed: " << x.getMesg() << endl;
		return 1;
	}
	return 0;
}
