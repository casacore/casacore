//# Copyright (C) 1994,1995,1996,1998,1999,2000,2002
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

#include <casa/Quanta/QVector.h>

#include <casa/Arrays/ArrayMath.h>

#include <casa/namespace.h>
int main () {
	try {
		QVector<Double> x;
		AlwaysAssert(x.size() == 0, AipsError);

		Vector<Double> vy(2);
		vy.set(4.0);
		String unit = "g";
		QVector<Double> y(vy, unit);
		AlwaysAssert(y.size() == vy.size(), AipsError);
		AlwaysAssert(y[0].getValue() == vy[0], AipsError);
		AlwaysAssert(y.getUnit() == unit, AipsError);

		QVector<Double> t = y/2;
		Vector<Double> expec = y.getValue()/2.0;
		AlwaysAssert(allTrue(t.getValue() == expec), AipsError);
		AlwaysAssert(t.getUnit() == unit, AipsError);

		y.scale(0.5);
		expec = t.getValue();
		AlwaysAssert(allTrue(y.getValue() == expec), AipsError);
		AlwaysAssert(y.getUnit() == unit, AipsError);

		Vector<Double> zx(2);
		zx[0] = 0;
		zx[1] = 1;
		QVector<Double> z(zx, unit);
		Quantity mymin = z.min();
		AlwaysAssert(mymin.getValue() == 0, AipsError);
		AlwaysAssert(mymin.getUnit() == unit, AipsError);

		Vector<Double> aa(2), bb(2);
		aa[0] = 1;
		aa[1] = 2;
		bb[0] = 3;
		bb[1] = 4;
		QVector<Double> a(aa, "g");
		QVector<Double> b(bb, "mg");
		QVector<Double> add = a + b;
		AlwaysAssert(add.getValue()[0] == 1.003, AipsError);
		AlwaysAssert(add.getValue()[1] == 2.004, AipsError);
		AlwaysAssert(add.getUnit() == "g", AipsError);
		add = b + a;
		AlwaysAssert(add.getValue()[0] == 1003, AipsError);
		AlwaysAssert(add.getValue()[1] == 2004, AipsError);
		AlwaysAssert(add.getUnit() == "mg", AipsError);

		QVector<Double> sub = a - b;
		AlwaysAssert(sub.getValue()[0] == 0.997, AipsError);
		AlwaysAssert(sub.getValue()[1] == 1.996, AipsError);
		AlwaysAssert(sub.getUnit() == "g", AipsError);
		sub = b - a;
		AlwaysAssert(sub.getValue()[0] == -997, AipsError);
		AlwaysAssert(sub.getValue()[1] == -1996, AipsError);
		AlwaysAssert(sub.getUnit() == "mg", AipsError);


	}
	catch (const AipsError& x) {
		cerr << x.getMesg() << endl;
		return 1;
	}
	return 0;
    
}
