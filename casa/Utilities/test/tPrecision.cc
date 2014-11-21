//# tRegex.cc: Test program for the Regex class
//# Copyright (C) 1993,1994,1995,1996,1999,2000,2001
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

#include <casacore/casa/iomanip.h>
#include <casacore/casa/Utilities/Precision.h>

#include <casacore/casa/namespace.h>

void testit(
		const Vector<Double>& x, const Vector<Double>& y,
		const uInt expectedPrecision
) {
	ostringstream testStream;
	testStream << "x = " << x[0] << " +/- " << x[1] << ", y ";
	if (y.size() == 0) {
		testStream << "nonexistant, ";

	}
	else {
		testStream << y[0] << " +/- " << y[1] << ", ";
	}
	testStream << "results in a precision of " << expectedPrecision;

	cout << "*** "<< testStream.str() << " ***" << endl;
	uInt precision = precisionForValueErrorPairs(x, y);
        cout << "prec="<<precision<<endl;
	cout << fixed << setprecision(precision) << "x = " << x[0] << " +/- " << x[1] << endl;
	if (y.size() == 2) {
		cout << fixed << setprecision(precision) << "y = " << y[0] << " +/- " << y[1] << endl;
	}
	AlwaysAssert(precision == expectedPrecision, AipsError);
}

int main () {

  try {
	  ostringstream test;
	  Vector<Double> x(2, 0);
	  Vector<Double> y;
	  uInt expected = 3;
	  testit(x, y, expected);

	  x[0] = 5;
	  x[1] = 0;
	  expected = 2;
	  testit(x, y, expected);

	  x[0] = 5.1;
	  x[1] = 0;
	  expected = 2;
	  testit(x, y, expected);

	  x[0] = -5.1;
	  x[1] = 0;
	  expected = 2;
	  testit(x, y, expected);

	  x[0] = 12.25;
	  x[1] = 0;
	  expected = 1;
	  testit(x, y, expected);

	  x[0] = -12.25;
	  x[1] = 0;
	  expected = 1;
	  testit(x, y, expected);

	  x[0] = 12.25;
	  x[1] = 0.003;
	  expected = 4;
	  testit(x, y, expected);

	  x[0] = 12.25;
	  x[1] = 0.009;
	  expected = 4;
	  testit(x, y, expected);

	  x[0] = 12.25;
	  x[1] = 9;
	  expected = 1;
	  testit(x, y, expected);

	  y.resize(2);
	  x[0] = 12.25;
	  x[1] = 9;
	  y[0] = 12.25;
	  y[1] = 0.009;
	  expected = 4;
	  testit(x, y, expected);

	  y.resize(2);
	  x[0] = 1200.25;
	  x[1] = 900;
	  y[0] = 1200.25;
	  y[1] = 90;
	  expected = 0;
	  testit(x, y, expected);


  } catch (AipsError x) {
    cout << x.getMesg() << endl;
    return 1;
  } 
  return 0;                           // exit with success status
}

