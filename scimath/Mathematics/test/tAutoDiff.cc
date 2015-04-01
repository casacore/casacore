//# tAutoDiff.cc: test program for AutoDiff
//# Copyright (C) 1995,1996,1999,2000,2001,2004
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
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Arrays/MaskArrLogi.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Arrays/IPosition.h>

#include <casacore/scimath/Mathematics/AutoDiff.h>
#include <casacore/scimath/Mathematics/AutoDiffMath.h>
#include <casacore/scimath/Mathematics/AutoDiffIO.h>

#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
// Made some minor changes, so we don't compare floating point values directly, i.e. use ==
// but rather use nearAbs, as the floating point between chips won't necessarily give the
// same answer. wky 23-aug-2004

int main() {
  uInt nerr = 0;
  // test the constructors
  {
    AutoDiff<Float> a;
    if (a.value() != 0 || (a.derivatives()).nelements() != 0) {
      cerr << "AutoDiff<Float> a; failed a = " << a << endl;
      nerr++;
    }
    
    AutoDiff<Float> b(1.0);
    if (b.value() != 1.0 || b.derivatives().nelements() != 0) {
      cerr << "AutoDiff<Float> b(1.0); failed b = " << b << endl;
      nerr++;
    }
    
    Vector<Float> g(3);
    g = 0.0;
    g(1) = 1.0;
    AutoDiff<Float> x(2.0, 3, 1);
    if (x.value() != 2.0 || ! allEQ(x.derivatives(),g)) {
      cerr << "AutoDiff<Float> x(2.0, 3, 1); failed x = " << x << endl;
      nerr++;
    }
    
    AutoDiff<Float> y(x);
    if (y.value() != x.value() || ! allEQ(y.derivatives(),x.derivatives())) {
      cerr << "AutoDiff<Float> y(x); failed y = " << y << " x = " << x << endl;
      nerr++;
    }
    
    g(0) = 1.0;
    g(1) = -1.0;
    g(2) = 0.5;
    Float val = 5.0;
    AutoDiff<Float> z(val, g);
    if (z.value() != val || ! allEQ(z.derivatives(),g)) {
      cerr << "AutoDiff<Float> z(val, g); failed z = " << z 
	   << " val = " << val << " g = " << g << endl;
      nerr++;
    }
  }
  
  // test the assignment operators
  {
    AutoDiff<Float> x(3.0,1,0);
    x = 1.0;
    if (x.value() != 1.0 || x.derivatives().nelements()!=0) {
      cerr << "assignment to constant failed x : " << x << endl;
      nerr++;
    }
    
    AutoDiff<Float> y(2.0, 3, 1);
    x = y;
    if (x.value() != y.value() || ! allEQ(x.derivatives(), y.derivatives())) {
      cerr << "assignment to other failed x : " << x << " y : " << y << endl;
      nerr++;
    }
  }
  
  // test the class member operators
  {
    AutoDiff<Float> x(3.0,2,0);
    AutoDiff<Float> y(2.0,2,1);
    AutoDiff<Float> z;
    z = x;
    z *= y;
    // verify result
    if (z.value() != (x.value() * y.value()) ||
	z.derivatives()(0) != y.value() ||
	z.derivatives()(1) != x.value()) {
      cerr << "*= operator failed" << endl;
      nerr++;
    }
    
    z = x;
    z /= y;
    // verify result
    if (z.value() != (x.value() / y.value()) ||
	z.derivatives()(0) != (1.0/y.value()) ||
	z.derivatives()(1) != (-x.value()/(y.value()*y.value()))) {
      cerr << "/= operator failed" << endl;
      nerr++;
    }
    
    z = x;
    z += y;
    // verify result
    if (z.value() != (x.value() + y.value()) ||
	z.derivatives()(0) != 1 ||
	z.derivatives()(1) != 1) {
      cerr << "+= operator failed" << endl;
      nerr++;
    }
    
    z = x;
    z -= y;
    // verify result
    if (z.value() != (x.value() - y.value()) ||
	z.derivatives()(0) != 1 ||
	z.derivatives()(1) != -1) {
      cerr << "-= operator failed" << endl;
      nerr++;
    }
  }
  
  // other class members
  {
    AutoDiff<Float> x;
    if (x.nDerivatives() != 0) {
      cerr << "wrong number of elements, should be 0" << endl;
      nerr++;
    }
    if (!x.isConstant()) {
      cerr << "x should be const, isConstant reports False" << endl;
      nerr++;
    }
    AutoDiff<Float> y(1.0,3,0);
    if (y.nDerivatives() != 3) {
      cerr << "resize failed" << endl;
      nerr++;
    }
    Vector<Float> grad(3);
    grad(0) = 1.;
    grad(1) = 2.;
    grad(2) = 3.;
    y = AutoDiff<Float>(y.value(), grad);;;
    if (!allEQ(y.derivatives(),grad)) {
      cerr << "derivatives assignment failed" << endl;
      nerr++;
    }
    y.value() = 4.0;
    if (y.value() != 4.0) {
      cerr << "value assignment failed" << endl;
      nerr++;
    }
    if (y.isConstant()) {
      cerr << "y should not be const, isConstant reports True" << endl;
      nerr++;
    }
  }
  
  // AutoDIffMath tests
  {
    AutoDiff<Float> x(3.0,1,0);
    AutoDiff<Float> y;
    
    y = +x;
    if (y.value() != x.value() ||
	!allEQ(y.derivatives(), x.derivatives())) {
      cerr << "operator+(const AutoDiff<T> &) failed" << endl;
      nerr++;
    }
    
    y = -x;
    if (y.value() != -x.value() ||
	!allEQ(y.derivatives(), -x.derivatives())) {
      cerr << "operator-(const AutoDiff<T> &) failed" << endl;
      nerr++;
    }
    
    y = x + x;
    if (y.value() != (Float(2.0) * x.value()) ||
	!allEQ(y.derivatives(), Float(2.0) * x.derivatives())) {
      cerr << "operator+(const AutoDiff<T> &, const AutoDiff<T> &) failed" << endl;
      nerr++;
    }
    y = x - x;
    if (y.value() != 0.0 ||
	!allEQ(y.derivatives(), Float(0.0))) {
      cerr << "operator-(const AutoDiff<T> &, const AutoDiff<T> &) failed" << endl;
      nerr++;
    }
    y = x * x;
    if (y.value() != (x.value() * x.value()) ||
	!allEQ(y.derivatives(), Float(2.0) * x.value() * x.derivatives())) {
      cerr << "operator*(const AutoDiff<T> &, const AutoDiff<T> &) failed" << endl;
      nerr++;
    }
    
    y = x / x;
    if (!near(y.value(),Float(1)) ||
	!allNearAbs(y.derivatives(), Float(0.0),1.0e-5)) {
      cerr << "operator/(const AutoDiff<T> &, const AutoDiff<T> &) failed" << endl;
      nerr++;
    }

    y = x + Float(1.0);
    if (y.value() != (x.value() + Float(1.0)) ||
	!allEQ(y.derivatives(), x.derivatives())) {
      cerr << "operator+(const AutoDiff<T> &,const T&) failed" << endl;
      nerr++;
    }
    
    y = x - Float(1.0);
    if (y.value() != (x.value() - Float(1.0)) ||
	!allEQ(y.derivatives(), x.derivatives())) {
      cerr << "operator-(const AutoDiff<T> &,const T&) failed" << endl;
      nerr++;
    }
    
    y = x * Float(2.0);
    if (y.value() != (x.value() * Float(2.0)) ||
	!allEQ(y.derivatives(), x.derivatives()*Float(2.0))) {
      cerr << "operator*(const AutoDiff<T> &,const T&) failed" << endl;
      nerr++;
    }
    
    y = x / Float(2.0);
    if (y.value() != (x.value() / Float(2.0)) ||
	!allEQ(y.derivatives(), x.derivatives()/Float(2.0))) {
      cerr << "operator/(const AutoDiff<T> &,const T&) failed" << endl;
      nerr++;
    }
    
    y = Float(1.0) + x;
    if (y.value() != (x.value() + Float(1.0)) ||
	!allEQ(y.derivatives(), x.derivatives())) {
      cerr << "operator+(,const T&, const AutoDiff<T> &) failed" << endl;
      nerr++;
    }

    y = Float(1.0) - x;
    if (y.value() != (Float(1.0) - x.value()) ||
	!allEQ(y.derivatives(), -x.derivatives())) {
      cerr << "operator-(const T&, const AutoDiff<T> &) failed" << endl;
      nerr++;
    }

    y = Float(2.0) * x;
    if (y.value() != (x.value() * Float(2.0)) ||
	!allEQ(y.derivatives(), x.derivatives()*Float(2.0))) {
      cerr << "operator*(const T&, const AutoDiff<T> &) failed" << endl;
      nerr++;
    }
    
    y = Float(2.0) / x;
    if (!near(y.value(),Float(2.0) / x.value()) ||
	!allNearAbs(y.derivatives(), -x.derivatives()*Float(2.0)/(x.value()*x.value()),1.0e-5)) {
      cerr << "operator/(const T&, const AutoDiff<T> &) failed" << endl;
      nerr++;
    }

    // trancendentals
    x.value() = 0.5;
    // acos(x) : derivative = -1/sqrt(1-x*x)
    y = acos(x);
    if (y.value() != Float(acos(x.value())) ||
	!allEQ(y.derivatives(),
	       -x.derivatives()/Float(sqrt(1.0 - x.value()*x.value())))) {
      cerr << "acos(const AutoDiff<T> &) failed" << endl;
      nerr++;
    }
    
    // asin(x) : derivative = 1/sqrt(1-x*x)
    y = asin(x);
    if (y.value() != Float(asin(x.value())) ||
	!allEQ(y.derivatives(),
	       x.derivatives()/Float(sqrt(1.0 - x.value()*x.value())))) {
      cerr << "asin(const AutoDiff<T> &) failed" << endl;
      nerr++;
    }
    
    // atan(x) : derivative = 1/(1+x*x)
    y = atan(x);
    if (!allNearAbs(y.value(), Float(atan(x.value())),1.e-6) ||
	!allNearAbs(y.derivatives(), 
	       x.derivatives()/Float(1.0 + x.value()*x.value()),1.e-6)) {
      cerr << y.value() -  Float(atan(x.value())) << endl;
      cerr << y.derivatives() - x.derivatives()/Float(1.0 + x.value()*x.value()) << endl;
      cerr << "atan(const AutoDiff<T> &) failed" << endl;
      nerr++;
    }
    
    // atan2(x, y) : derivative = d(atan(x/y))
    //                          = (1/(1+(x/y)*(x/y))) * (dx/y - x*dy/y**2)
    AutoDiff<Float> w(3.0, 2, 0);
    AutoDiff<Float> z(2.5, 2, 1);
    y = atan2(w, z);
    if (y.value() != Float(atan2(w.value(), z.value())) ||
	!allEQ(y.derivatives(), 
	       (w.derivatives()/z.value() - 
		w.value()*z.derivatives()/(z.value()*z.value())) / 
	       Float(1.0 + w.value()*w.value()/(z.value()*z.value())))) {
      cerr << "atan2(const AutoDiff<T> &, const AutoDiff<T> &g) failed" <<
	endl;
      nerr++;
    }
    
    // cos(x) : derivative = -sin(x)
    y = cos(x);
    if (!nearAbs(y.value(), Float(cos(x.value())) ) ||
	!allEQ(y.derivatives(),-Float(sin(x.value()))*x.derivatives())) {
      cerr << "cos(const AutoDiff<T> &) failed" << endl;
      nerr++;
    }
    
    // cosh(x) : derivative = sinh(x)
    y = cosh(x);
    if (y.value() != Float(cosh(x.value())) ||
	!allEQ(y.derivatives(), Float(sinh(x.value()))*x.derivatives())) {
      cerr << "cosh(const AutoDiff<T> &) failed" << endl;
      nerr++;
    }
    
    // exp(x) : derivative = exp(x)
    y = exp(x);
    if (y.value() != Float(exp(x.value())) ||
	!allEQ(y.derivatives(), x.derivatives() * Float(exp(x.value())))) {
      cerr << "exp(const AutoDiff<T> &) failed" << endl;
      nerr++;
    }
    
    // log(x) : derivative = 1/x
    y = log(x);
    if (y.value() != Float(log(x.value())) ||
	!allEQ(y.derivatives(), x.derivatives() / x.value())) {
      cerr << "log(const AutoDiff<T> &) failed" << endl;
      nerr++;
    }
    
    // log10(x) : derivative = (1/x) / log(10)
    y = log10(x);
    if (y.value() != Float(log10(x.value())) ||
	!allEQ(y.derivatives(), x.derivatives() /
	       Float((x.value()*log(10.0))))) {
      cerr << "log10(const AutoDiff<T> &) failed" << endl;
      nerr++;
    }
    
    // pow(x,y) : derivative = y*pow(x,y-1)*dx + pow(x,y)*log(x)*dy
    y = pow(w,z);
    if (y.value() != Float(pow(w.value(), z.value()))
	|| !allEQ(y.derivatives(),
		  (Float(z.value()*pow(w.value(),z.value()-1))*
		   w.derivatives() +
		   Float(pow(w.value(),z.value())*log(w.value()))*
		   z.derivatives()))) {
      cerr << "pow(const AutoDiff<T> &, const AutoDiff<T> &) failed" << endl;
      nerr++;
    }

    // pow(x,const) : derivative = const*pow(x,const-1)*dx
    y = pow((AutoDiff<Float>&)x,Float(2.5));
    if (y.value() != Float(pow(x.value(),2.5)) ||
	!allEQ(y.derivatives(), 
	       Float(2.5*pow(x.value(),1.5))*x.derivatives())) {
      cerr << "pow(const AutoDiff<T> &, const double &) failed" << endl;
      nerr++;
    }
    
    // sin(x) : derivative = cos(x)
    y = sin(x);
    if (!allNearAbs(y.value(), Float(sin(x.value())) ) ||
	!allEQ(y.derivatives(), Float(cos(x.value()))*x.derivatives())) {
      cerr << "sin(const AutoDiff<T> &) failed" << endl;
      nerr++;
    }
    
    // sinh(x) : derivative = cosh(x)
    y = sinh(x);
    if (!allNearAbs(y.value(), Float(sinh(x.value()))) ||
	!allEQ(y.derivatives(), Float(cosh(x.value()))*x.derivatives())) {
      cerr << "sinh(const AutoDiff<T> &) failed" << endl;
      nerr++;
    }
    
    // sqrt(x) : derivative = 0.5/sqrt(x)
    y = sqrt(x);
    if (!allNearAbs(y.value(), Float(sqrt(x.value()))) ||
      	!allEQ(y.derivatives(), x.derivatives()*Float(0.5/sqrt(x.value())))) {
      cerr << "sqrt(const AutoDiff<T> &) failed" << endl;
      nerr++;
    }
    
    // tan(x) : derivative = sec(x)*sec(x) = 1/(cos(x)*cos(x))
    y = tan(x);
    if (!allNearAbs(y.value(), Float(tan(x.value())) ) ||
	!allNearAbs(y.derivatives(), 
	       x.derivatives()/Float(cos(x.value())*cos(x.value())),1.e-6)) {
      cerr << "tan(const AutoDiff<T> &) failed" << endl;
      nerr++;
    }
    
    // tanh(x) : derivative = sech(x)*sech(x) = 1/(cosh(x)*cosh(x))
    y = tanh(x);
    if (!allNearAbs(y.value(), Float(tanh(x.value())) ) ||
	!allEQ(y.derivatives(), 
	       x.derivatives()/Float(cosh(x.value())*cosh(x.value())))) {
      cerr << "sinh(const AutoDiff<T> &) failed" << endl;
      nerr++;
    }
    
  }
  if (nerr != 0) cout << "There were " << nerr << " errors" << endl;
  else cout << "ok" << endl;

  return nerr;
}

