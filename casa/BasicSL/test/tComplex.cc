//# tComplex.cc: This program tests the Complex class
//# Copyright (C) 1993,1994,1995,1996,1999,2000,2001,2002
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

#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/BasicSL/IComplex.h>

#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/stdio.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/fstream.h>
#include <unistd.h>

#include <casacore/casa/namespace.h>
int main() {

  Complex f1(23.9,1.8), f2(9.2,8.2), f3(2.7,1.8), fo(237.561,0.9312), fi;
  IComplex i1(5,2), i3(Int(f1.real()),Int(f1.imag()));
  DComplex d1, d2(f1.real(),f1.imag()), d3(0.921,7.812);
  fstream fio("tComplex_tmp.data", ios::out | ios::trunc);

  cout << "Initial value for complex: " << d1 << endl;
  cout << d1 << " :" << endl;
  cout << "    real part:      " << d1.real() << " == " << real(d1) << endl;
  cout << "    imaginary part: " << d1.imag() << " == " << imag(d1) << endl;
  cout << f1 << " == " << d2 << " ~= " << i3 << endl;
  cout << "-" << d3 << " == " << -d3 << endl;
  cout << "conj(" << d3 << ") == " << conj(d3) << endl;
  cout << "norm(" << d3 << ") == " << norm(d3) << endl;
  cout << "arg(" << d3 << ") == " << arg(d3) << endl;
  d1 = DComplex(18.9, -2.31);
  cout << "fabs(" << d1 << ") == " << fabs(d1) << endl;
  fio << fo << endl;
  fio.close();
  fio.open("tComplex_tmp.data", ios::in);
  fio >> fi;
  fio.close();
  unlink("tComplex_tmp.data");
  cout << "out: " << fo << "           in: " << fi << endl;

  
  d1 = DComplex(i1.real(), i1.imag());
  cout << d1 << " == " << i1 << endl;

  cout << "- - - - - - - - - - - - - - - - - - - -" << endl;
  cout << f1 << " + " << f2 << " = " << f1 + f2 << endl;
  cout << f1 << " * " << f2 << " = " << f1 * f2 << endl;
  cout << f1 << " - " << f2 << " = " << f1 - f2 << endl;
  cout << f1 << " / " << f2 << " = " << f1 / f2 << endl;
  cout << endl;

  cout << "- - - - - - - - - - - - - - - - - - - -" << endl;
  cout << f1 << " + " << 7.561 << " = " << f1 + 7.561f << endl;
  cout << f1 << " * " << 7.561 << " = " << f1 * 7.561 << endl;
  cout << f1 << " - " << 7.561 << " = " << f1 - 7.561f << endl;
  cout << f1 << " / " << 7.561 << " = " << f1 / 7.561 << endl;
  cout << endl;

  cout << "- - - - - - - - - - - - - - - - - - - -" << endl;
  cout << f1 << " + " << 13 << " = " << f1 + 13.0f << endl;
  cout << f1 << " * " << "13.0f" << " = " << f1 * 13.0f << endl;
  cout << f1 << " * " << "13" << " = " << f1 * 13 << endl;
  cout << f1 << " * " << "13." << " = " << f1 * 13. << endl;
  cout << "13.0f" << " * " << f1 << " = " << 13.0f * f1 << endl;
  cout << "13" << " * " << f1 << " = " << 13 * f1 << endl;
  cout << "13." << " * " << f1 << " = " << 13. * f1 << endl;
  cout << f1 << " - " << 13 << " = " << f1 - 13.0f << endl;
  cout << f1 << " / " << "13.0f" << " = " << f1 / 13.0f << endl;
  cout << f1 << " / " << "13" << " = " << f1 / 13 << endl;
  cout << f1 << " / " << "13." << " = " << f1 / 13. << endl;
  cout << "13.0f" << " / " << f1 << " = " << 13.0f / f1 << endl;
  cout << "13" << " / " << f1 << " = " << 13 / f1 << endl;
  cout << "13." << " / " << f1 << " = " << 13. / f1 << endl;
  cout << endl;

  cout << "- - - - - - - - - - - - - - - - - - - -" << endl;
  cout << f1 << " +=  4 -> "; cout << (f1 += 4) << endl;
  cout << f1 << " *=  4 -> "; cout << (f1 *= 4) << endl;
  cout << f1 << " -=  4 -> "; cout << (f1 -= 4) << endl;
  cout << f1 << " /=  4 -> "; cout << (f1 /= 4) << endl;
  cout << endl;

  cout << "- - - - - - - - - - - - - - - - - - - -" << endl;
  cout << f1 << " +=  0.896 -> "; cout << (f1 += 0.896) << endl;
  cout << f1 << " *=  0.896 -> "; cout << (f1 *= 0.896) << endl;
  cout << f1 << " -=  0.896 -> "; cout << (f1 -= 0.896) << endl;
  cout << f1 << " /=  0.896 -> "; cout << (f1 /= 0.896) << endl;
  cout << endl;

  char simChar = 2;
  cout << "- - - - - - - - - - - - - - - - - - - -" << endl;
  cout << f1 << " +=  simChar -> "; cout << (f1 += simChar) << endl;
  cout << f1 << " *=  simChar -> "; cout << (f1 *= simChar) << endl;
  cout << f1 << " -=  simChar -> "; cout << (f1 -= simChar) << endl;
  cout << f1 << " /=  simChar -> "; cout << (f1 /= simChar) << endl;
  cout << endl;

  cout << "- - - - - - - - - - - - - - - - - - - -" << endl;
  cout << f1 << " +=  " << f3 << " -> "; cout << (f1 += f3) << endl;
  cout << f1 << " *=  " << f3 << " -> "; cout << (f1 *= f3) << endl;
  cout << f1 << " -=  " << f3 << " -> "; cout << (f1 -= f3) << endl;
  cout << f1 << " /=  " << f3 << " -> "; cout << (f1 /= f3) << endl;
  cout << endl;

  cout << "- - - - - - - - - - - - - - - - - - - -" << endl;
  Complex d3c = Complex(d3.real(), d3.imag());
  cout << f1 << " +=  " << d3 << " -> "; cout << (f1 += d3c) << endl;
  cout << f1 << " *=  " << d3 << " -> "; cout << (f1 *= d3c) << endl;
  cout << f1 << " -=  " << d3 << " -> "; cout << (f1 -= d3c) << endl;
  cout << f1 << " /=  " << d3 << " -> "; cout << (f1 /= d3c) << endl;
  cout << endl;

  i1 = IComplex(36, 14);

  cout << "- - - - - - - - - - - - - - - - - - - -" << endl;
  // reset f1 to nominal values at it will have been corrupted (due to
  // roundoff) by all the earlier calculations.
  f1 = Complex(33.417, 13.412);
  cout << "sin(" << f1 << ") = " << sin(f1) << endl;
  cout << "cos(" << f1 << ") = " << cos(f1) << endl;
  cout << "sinh(" << f1 << ") = " << sinh(f1) << endl;
  cout << "cosh(" << f1 << ") = " << cosh(f1) << endl;
  cout << endl;

  cout << "- - - - - - - - - - - - - - - - - - - -" << endl;
  cout << "log(" << f1 << ") = " << log(f1) << endl;
  cout << "log10(" << f1 << ") = " << log10(f1) << endl;
  cout << "exp(" << f1 << ") = " << exp(f1) << endl;
  cout << endl;

  cout << "- - - - - - - - - - - - - - - - - - - -" << endl;
  if (! near (pow(f1,3.0f), Complex(19283.3,42518.8))) {
    cout << "pow(" << f1 << ",3) = " << pow(f1,3.0f) << endl;
  }
  if (! near (pow(f1,-8.0f), Complex(-3.52463e-13,-3.11743e-14))) {
    cout << "pow(" << f1 << ",-8) = " << pow(f1,-8.0f) << endl;
  }
  if (! near (pow(f1,0.214f), Complex(2.14595,0.175667))) {
    cout << "pow(" << f1 << ",0.214) = " << pow(f1,0.214f) << endl;
  }
  cout << "pow(" << f1 << "," << i1 << " = "
       << pow(f1,Complex(i1.real(), i1.imag())) << endl;
  if (! near (pow(f1,f2), Complex(8.06899e+11,9.07712e+12))) {
    cout << "pow(" << f1 << "," << f2 << " = " << pow(f1,f2) << endl;
  }
  cout << endl;

  cout << "- - - - - - - - - - - - - - - - - - - -" << endl;
  cout << "sqrt(" << f1 << ") = " << sqrt(f1) << endl;
  cout << endl;

  cout << "- - - - - - - - - - - - - - - - - - - -" << endl;
  cout << f1 << " == " << f2 << " -> " << (f1 == f2) << endl;
  cout << f1 << " != " << f2 << " -> " << (f1 != f2) << endl;
  cout << f1 << " >= " << f2 << " -> " << (f1 >= f2) << endl;
  cout << f1 << " > " << f2 << " -> " << (f1 > f2) << endl;
  cout << f1 << " <= " << f2 << " -> " << (f1 <= f2) << endl;
  cout << f1 << " < " << f2 << " -> " << (f1 < f2) << endl;
  cout << endl;

  cout << "- - - - - - - - - - - - - - - - - - - -" << endl;
  cout << f2 << " == " << f1 << " -> " << (f2 == f1) << endl;
  cout << f2 << " != " << f1 << " -> " << (f2 != f1) << endl;
  cout << f2 << " >= " << f1 << " -> " << (f2 >= f1) << endl;
  cout << f2 << " > " << f1 << " -> " << (f2 > f1) << endl;
  cout << f2 << " <= " << f1 << " -> " << (f2 <= f1) << endl;
  cout << f2 << " < " << f1 << " -> " << (f2 < f1) << endl;
  cout << endl;

  cout << "- - - - - - - - - - - - - - - - - - - -" << endl;
  cout << f1 << " == " << f1 << " -> " << (f1 == f1) << endl;
  cout << f1 << " != " << f1 << " -> " << (f1 != f1) << endl;
  cout << f1 << " >= " << f1 << " -> " << (f1 >= f1) << endl;
  cout << f1 << " > " << f1 << " -> " << (f1 > f1) << endl;
  cout << f1 << " <= " << f1 << " -> " << (f1 <= f1) << endl;
  cout << f1 << " < " << f1 << " -> " << (f1 < f1) << endl;
  cout << endl;

  f1 = Complex(33.0, 6.0);

  f1 = 33.021;

  f1 = 0;
  cout << "- - - - - - - - - - - - - - - - - - - -" << endl;

  AlwaysAssertExit(near(Complex(0,10000), Complex(0,10001), 1.01e-4));
  AlwaysAssertExit(!near(Complex(0,10000), Complex(0,10001), 0.99e-4));
  AlwaysAssertExit(!near(Complex(10000,0), Complex(0,10001), 1.01e-4));
  AlwaysAssertExit(nearAbs(Complex(0,10000), Complex(0,10001), 1.01));
  AlwaysAssertExit(!nearAbs(Complex(0,10000), Complex(0,10001), 0.99));
  AlwaysAssertExit(!nearAbs(Complex(10000,0), Complex(0,10001), 1.01));

  AlwaysAssertExit(allNear(Complex(0,10000), Complex(0,10001), 1.01e-4));
  AlwaysAssertExit(!allNear(Complex(0,10000), Complex(0,10001), 0.99e-4));
  AlwaysAssertExit(!allNear(Complex(10000,0), Complex(0,10001), 1.01e-4));
  AlwaysAssertExit(allNearAbs(Complex(0,10000), Complex(0,10001), 1.01));
  AlwaysAssertExit(!allNearAbs(Complex(0,10000), Complex(0,10001), 0.99));
  AlwaysAssertExit(!allNearAbs(Complex(10000,0), Complex(0,10001), 1.01));

  AlwaysAssertExit(near(DComplex(0,10000), DComplex(0,10001), 1.01e-4));
  AlwaysAssertExit(!near(DComplex(0,10000), DComplex(0,10001), 0.99e-4));
  AlwaysAssertExit(!near(DComplex(10000,0), DComplex(0,10001), 1.01e-4));
  AlwaysAssertExit(nearAbs(DComplex(0,10000), DComplex(0,10001), 1.01));
  AlwaysAssertExit(!nearAbs(DComplex(0,10000), DComplex(0,10001), 0.99));
  AlwaysAssertExit(!nearAbs(DComplex(10000,0), DComplex(0,10001), 1.01));

  AlwaysAssertExit(allNear(DComplex(0,10000), DComplex(0,10001), 1.01e-4));
  AlwaysAssertExit(!allNear(DComplex(0,10000), DComplex(0,10001), 0.99e-4));
  AlwaysAssertExit(!allNear(DComplex(10000,0), DComplex(0,10001), 1.01e-4));
  AlwaysAssertExit(allNearAbs(DComplex(0,10000), DComplex(0,10001), 1.01));
  AlwaysAssertExit(!allNearAbs(DComplex(0,10000), DComplex(0,10001), 0.99));
  AlwaysAssertExit(!allNearAbs(DComplex(10000,0), DComplex(0,10001), 1.01));

  Complex c1; DComplex c2;
  setNaN(c1); setNaN(c2);
  AlwaysAssertExit(isNaN(c1) && isNaN(c2));
  c1 = Complex(0.0, c1.imag()); c2 = DComplex(c2.real(), 0.0);
  AlwaysAssertExit(isNaN(c1) && isNaN(c2));
  c1 = Complex(0.0); c2 = DComplex(0.0);
  AlwaysAssertExit((!isNaN(c1)) && (!isNaN(c2)));

  {
      // Test min/max
      Complex c1(0,1), c2(2,0);
      Complex c3 = min(c1,c2);
      AlwaysAssertExit(near(c1,c3));
      Complex c4 = max(c1,c2);
      AlwaysAssertExit(near(c2,c4));
  }

  return(0);
}
