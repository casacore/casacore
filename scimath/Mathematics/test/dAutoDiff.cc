//# dAutoDiff.cc: Demo program for AutoDiff, including 2nd derivative
//# Copyright (C) 2001
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
#include <casacore/scimath/Mathematics/AutoDiff.h>
#include <casacore/scimath/Mathematics/AutoDiffA.h>
#include <casacore/scimath/Mathematics/AutoDiffMath.h>
#include <casacore/scimath/Mathematics/AutoDiffIO.h>

#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
// Define a simple function a^3*b^3*x

template <class T> class f {
public:
  T operator()(const T& x) { return a_p*a_p*a_p*b_p*b_p*x; }
  void set(const T& a, const T& b) { a_p = a; b_p = b; }
private:
  T a_p;
  T b_p;
};

// Specialization

template <> class f<AutoDiffA<double> > {
public:
  AutoDiffA<double> operator()(const AutoDiffA<double>& x) {
    Vector<double> dr(a_p.nDerivatives());
    dr[0] = 3*a_p.value()*a_p.value()*b_p.value()*b_p.value()*x.value();
    dr[1] = 2*a_p.value()*a_p.value()*a_p.value()*b_p.value()*x.value();
 return AutoDiffA<double>(a_p.value()*a_p.value()*a_p.value()*
			  b_p.value()*b_p.value()*x.value(), dr); }
  void set(const AutoDiff<double>& a, const AutoDiff<double>& b) {
    a_p = a; b_p = b; }
private:
  AutoDiffA<double> a_p;
  AutoDiffA<double> b_p;
};

int main() {
  cout << "Test AutoDiff2" << endl;
  cout << "----------------------------------------" << endl;

  // By selecting double a,b,x; f(x) will calculate value
  double a0(2), b0(3), x0(7);
  f<double> f0; f0.set(a0, b0);
  cout << "Value:      " << f0(x0) << endl;

  // By selecting AutoDiff a,b, and x; f(x) will calculate value and
  // partial derivatives wrt a,b
  AutoDiff<double> a1(2,2,0), b1(3,2,1), x1(7);
  f<AutoDiff<double> > f1; f1.set(a1, b1);
  cout << "Diff a,b:   " << f1(x1) << endl;

  // No need to use the function object, just calculate the expression:
  cout << "Same...:    " << (pow(a1,3.0)*pow(b1,2.0)*x1) << endl;

  // Use the specialization:
  f<AutoDiffA<double> > f12; f12.set(a1, b1);
  cout << "Same...:    " << f12(x1) << endl;

  // By selecting AutoDiff x, and a,b; f(x) will calculate value and
  // partial derivative wrt x
  AutoDiff<double> a2(2), b2(3), x2(7,1,0);
  f<AutoDiff<double> > f2; f2.set(a2, b2);
  cout << "Diff x:     " << f2(x2) << endl;

  // By selecting AutoDiff<AutoDiff> a,b, and x; f(x) will calculate value and
  // (first and) 2nd order partial derivatives wrt a,b
  AutoDiff<AutoDiff<double> > a3(AutoDiff<double>(2,2,0),2,0),
    b3(AutoDiff<double>(3,2,1),2,1), x3(AutoDiff<double>(7),2);
  f<AutoDiff<AutoDiff<double> > > f3; f3.set(a3, b3);
  cout << "Diff2 a,b:  " << f3(x3) << endl;

  // By selecting AutoDiff<AutoDiff> x, and a,b; f(x) will calculate value and
  // (first and) 2nd order partial derivatives wrt x
  AutoDiff<AutoDiff<double> > a4(AutoDiff<double>(2),1),
    b4(AutoDiff<double>(3),1),
    x4(AutoDiff<double>(7,1,0),1,0);
  f<AutoDiff<AutoDiff<double> > > f4; f4.set(a4, b4);
  cout << "Diff2 x:    " << f4(x4) << endl;

}


template class f<double>;
template class f<AutoDiff<double> >;
template class f<AutoDiff<AutoDiff<double> > >;
