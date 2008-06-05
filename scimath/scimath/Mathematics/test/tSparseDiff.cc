//# tSparseDiff.cc: test program for SparseDiff
//# Copyright (C) 2007
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
//# $Id: tSparseDiff.cc,v 1.1 2007/11/16 04:44:13 wbrouw Exp $

//# Includes
#include <casa/BasicMath/Math.h>
#include <casa/Exceptions/Error.h>

#include <scimath/Mathematics/SparseDiff.h>
#include <scimath/Mathematics/SparseDiffMath.h>
#include <scimath/Mathematics/SparseDiffIO.h>

#include <casa/iostream.h>

#include <casa/namespace.h>

Bool testDer(const SparseDiff<Float> y, const SparseDiff<Float> &x, Float f) {
  return !(x.nDerivatives() != y.nDerivatives() ||
    !nearAbs(y.derivative(0).second, f*x.derivative(0).second, 1e-5));
}

int main() {
  uInt nerr = 0;
  // test the constructors
  {
    SparseDiff<Float> a;
    if (a.value() != 0 || a.nDerivatives() != 0) {
      cerr << "SparseDiff<Float> a; failed a = " << a << endl;
      nerr++;
    }
    
    SparseDiff<Float> b(1.0);
    if (b.value() != 1.0 || b.nDerivatives() != 0) {
      cerr << "SparseDiff<Float> b(1.0); failed b = " << b << endl;
      nerr++;
    }
    
    SparseDiff<Float> x(2.0, 1);
    if (x.value() != 2.0 || x.nDerivatives() != 1 ||
	x.derivative(0) != pair<uInt, Float>(1, 1)) {
      cerr << "SparseDiff<Float> x(2.0, 1); failed x = " << x << endl;
      nerr++;
    }
    
    SparseDiff<Float> y(x);
    if (y.value() != x.value() || y.nDerivatives() != x.nDerivatives() ||
	x.derivative(0) != y.derivative(0)) {
      cerr << "SparseDiff<Float> y(x); failed y = " << y << " x = " << x << endl;
      nerr++;
    }
    
    Float val = 5.0;
    SparseDiff<Float> z(val, 2, 73.);
    if (z.value() != val || z.nDerivatives() != 1 ||
	z.derivative(0) != pair<uInt, Float>(2, 73.)) {
      cerr << "SparseDiff<Float> z(val, 2, 73.); failed z = " << z 
	   << " val = " << val << endl;
      nerr++;
      }
  }
  
  // test the assignment operators
  {
    SparseDiff<Float> x(3.0, 1);
    x = 14.0;
    if (x.value() != 14.0 || x.nDerivatives() != 1) {
      cerr << "assignment of value failed x : " << x << endl;
      nerr++;
    }
    
    SparseDiff<Float> y(2.0, 2);
    x = y;
    if (x.value() != y.value() || y.nDerivatives() != x.nDerivatives() ||
	x.derivative(0) != y.derivative(0)) {
      cerr << "assignment of other failed x : " << x << " y : " << y << endl;
      nerr++;
    }

    pair<uInt, Float> z(4, 9);
    x = z;
    if (x.value() != y.value() || x.nDerivatives() != 2 ||
	x.derivative(1) != z) {
      cerr << "assignment of added pair failed x : " << x << endl;
      nerr++;
    }

    pair<uInt, Float> z1(7, 23);
    vector<pair<uInt, Float> > z0, z00;
    z0.push_back(z1);
    z0.push_back(z);
    x = z0;
    if (x.value() != y.value() || x.nDerivatives() != 2 ||
	x.derivative(0) != z || x.derivative(1) !=z1) {
      cerr << "assignment of vector failed x : " << x << endl;
      nerr++;
    }
  }

  // test the class member operators
  {
    SparseDiff<Float> x(3.0, 0);
    SparseDiff<Float> y(2.0, 1);
    SparseDiff<Float> z;
    z = x;
    z *= y;
    // verify result
    if (z.value() != (x.value() * y.value()) ||
	z.derivative(0).second != y.value() ||
	z.derivative(1).second != x.value()) {
      cerr << "*= operator failed" << endl;
      nerr++;
    }
    
    z = x;
    z /= y;
    // verify result
    if (z.value() != (x.value() / y.value()) ||
	z.derivative(0).second != (1.0/y.value()) ||
	z.derivative(1).second != (-x.value()/(y.value()*y.value()))) {
      cerr << "/= operator failed" << endl;
      nerr++;
    }
    
    z = x;
    z += y;
    // verify result
    if (z.value() != (x.value() + y.value()) ||
	z.derivative(0).second != 1 ||
	z.derivative(1).second != 1) {
      cerr << "+= operator failed" << endl;
      nerr++;
    }
    
    z = x;
    z -= y;
    // verify result
    if (z.value() != (x.value() - y.value()) ||
	z.derivative(0).second != 1 ||
	z.derivative(1).second != -1) {
      cerr << "-= operator failed" << endl;
      nerr++;
    }
  }

  // other class members
  {
    SparseDiff<Float> x;
    if (x.nDerivatives() != 0) {
      cerr << "wrong number of elements, should be 0" << endl;
      nerr++;
    }
    if (!x.isConstant()) {
      cerr << "x should be const, isConstant reports False" << endl;
      nerr++;
    }
    SparseDiff<Float> y(1,1);
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
    SparseDiff<Float> x(3.0,0);
    SparseDiff<Float> y;
    
    y = +x;
    if (y.value() != x.value() || y.nDerivatives() != x.nDerivatives() ||
	x.derivative(0) != y.derivative(0)) {
      cerr << "operator+(const SparseDiff<T> &) failed" << endl;
      nerr++;
    }
    
    y = -x;
    if (y.value() != -x.value() || y.nDerivatives() != x.nDerivatives() ||
	-x.derivative(0).second != y.derivative(0).second) {
      cerr << "operator-(const SparseDiff<T> &) failed" << endl;
      nerr++;
    }
    
    y = x + x;
    if (y.value() != (Float(2.0) * x.value()) ||
	y.nDerivatives() != x.nDerivatives() ||
	y.derivative(0).second != Float(2.0) * x.derivative(0).second) {
      cerr << "operator+(const SparseDiff<T> &, const SparseDiff<T> &) failed"
	   << endl;
      nerr++;
    }
    y = x - x;
    if (y.value() != 0.0 || !y.isConstant()) {
      cerr << "operator-(const SparseDiff<T> &, const SparseDiff<T> &) failed"
	   << endl;
      nerr++;
    }
    y = x * x;
    if (y.value() != (x.value() * x.value()) ||
	y.nDerivatives() != x.nDerivatives() ||
	y.derivative(0).second != Float(2.0) * x.value() *
	x.derivative(0).second) {
      cerr << "operator*(const SparseDiff<T> &, const SparseDiff<T> &) failed"
	   << endl;
      nerr++;
    }
    
    y = x / x;
    if (!near(y.value(),Float(1)) || !y.isConstant()) {
      cerr << "operator/(const SparseDiff<T> &, const SparseDiff<T> &) failed"
	   << endl;
      nerr++;
    }

    y = x + Float(1.0);
    if (y.value() != (x.value() + Float(1.0)) ||
	y.nDerivatives() != x.nDerivatives() ||
	y.derivative(0) != x.derivative(0)) {
      cerr << "operator+(const SparseDiff<T> &,const T&) failed" << endl;
      nerr++;
    }
    
    y = x - Float(1.0);
    if (y.value() != (x.value() - Float(1.0)) ||
	y.nDerivatives() != x.nDerivatives() ||
	y.derivative(0) != x.derivative(0)) {
      cerr << "operator-(const SparseDiff<T> &,const T&) failed" << endl;
      nerr++;
    }
    
    y = x * Float(2.0);
    if (y.value() != (x.value() * Float(2.0)) ||
	y.nDerivatives() != x.nDerivatives() ||
	y.derivative(0).second != x.derivative(0).second*Float(2)) {
      cerr << "operator*(const SparseDiff<T> &,const T&) failed" << endl;
      nerr++;
    }
    
    y = x / Float(2.0);
    if (y.value() != (x.value() / Float(2.0)) ||
	y.nDerivatives() != x.nDerivatives() ||
	y.derivative(0).second != x.derivative(0).second/Float(2)) {
      cerr << "operator/(const SparseDiff<T> &,const T&) failed" << endl;
      nerr++;
    }
    
    y = Float(1.0) + x;
    if (y.value() != (x.value() + Float(1.0)) ||
	y.nDerivatives() != x.nDerivatives() ||
	y.derivative(0) != x.derivative(0)) {
      cerr << "operator+(,const T&, const SparseDiff<T> &) failed" << endl;
      nerr++;
    }

    y = Float(1.0) - x;
    if (y.value() != (Float(1.0) - x.value()) ||
	y.nDerivatives() != x.nDerivatives() ||
	y.derivative(0).second != -x.derivative(0).second) {
      cerr << "operator-(const T&, const SparseDiff<T> &) failed" << endl;
      nerr++;
    }

    y = Float(2.0) * x;
    if (y.value() != (x.value() * Float(2.0)) ||
	y.nDerivatives() != x.nDerivatives() ||
	y.derivative(0).second != x.derivative(0).second*Float(2)) {
      cerr << "operator*(const T&, const SparseDiff<T> &) failed" << endl;
      nerr++;
    }
    
    y = Float(2.0) / x;
    if (!near(y.value(),Float(2.0) / x.value()) ||
	!testDer(y,x, -Float(2.0)/(x.value()*x.value()))) {
      cerr << "operator/(const T&, const SparseDiff<T> &) failed" << endl;
      nerr++;
    }

    // transcendentals
    x.value() = 0.5;
    // acos(x) : derivative = -1/sqrt(1-x*x)
    y = acos(x);
    if (y.value() != Float(acos(x.value())) ||
	!testDer(y,x,
		 -Float(1.)/Float(sqrt(1.0 - x.value()*x.value())))) {
      cerr << "acos(const SparseDiff<T> &) failed" << endl;
      nerr++;
    }
    
    // asin(x) : derivative = 1/sqrt(1-x*x)
    y = asin(x);
    if (y.value() != Float(asin(x.value())) ||
	!testDer(y,x,
	      Float(1.)/Float(sqrt(1.0 - x.value()*x.value())))) {
      cerr << "asin(const SparseDiff<T> &) failed" << endl;
      nerr++;
    }
    
    // atan(x) : derivative = 1/(1+x*x)
    y = atan(x);
    if (!allNearAbs(y.value(), Float(atan(x.value())),1.e-6) ||
	!testDer(y,x,
		 Float(1.)/Float(1.0 + x.value()*x.value()))) {
      cerr << y.value() -  Float(atan(x.value())) << endl;
      cerr << y.derivative(0).second -
	x.derivative(0).second/Float(1.0 + x.value()*x.value()) << endl;
      cerr << "atan(const SparseDiff<T> &) failed" << endl;
      nerr++;
    }
    
    // atan2(x, y) : derivative = d(atan(x/y))
    //                          = (1/(1+(x/y)*(x/y))) * (dx/y - x*dy/y**2)
    SparseDiff<Float> w(3.0, 0);
    SparseDiff<Float> z(2.5, 1);
    y = atan2(w, z);
    if (y.value() != Float(atan2(w.value(), z.value())) ||
	!near(y.derivative(0).second,
	      Float(1)/(Float(1)+w*w/z/z).value()*
	      (w/z).derivative(0).second,1e-5) ||
	!near(y.derivative(1).second, 
	      Float(1)/(Float(1)+w*w/z/z).value()*
	      (w/z).derivative(1).second,1e-5)) {
      cerr << "atan2(const SparseDiff<T> &, const SparseDiff<T> &g) failed" <<
	endl;
      nerr++;
    }
    
    // cos(x) : derivative = -sin(x)
    y = cos(x);
    if (!nearAbs(y.value(), Float(cos(x.value())) ) ||
	!testDer(y,x,
		 -Float(sin(x.value())))) {
      cerr << "cos(const SparseDiff<T> &) failed" << endl;
      nerr++;
    }
    
    // cosh(x) : derivative = sinh(x)
    y = cosh(x);
    if (y.value() != Float(cosh(x.value())) ||
	!testDer(y,x,
		 Float(sinh(x.value())))) {
      cerr << "cosh(const SparseDiff<T> &) failed" << endl;
      nerr++;
    }
    
    // exp(x) : derivative = exp(x)
    y = exp(x);
    if (y.value() != Float(exp(x.value())) ||
	!testDer(y,x,
		 Float(exp(x.value())))) {
      cerr << "exp(const SparseDiff<T> &) failed" << endl;
      nerr++;
    }
    
    // log(x) : derivative = 1/x
    y = log(x);
    if (y.value() != Float(log(x.value())) ||
	!testDer(y,x,
		 Float(1.) / x.value())) {
      cerr << "log(const SparseDiff<T> &) failed" << endl;
      nerr++;
    }
    
    // log10(x) : derivative = (1/x) / log(10)
    y = log10(x);
    if (y.value() != Float(log10(x.value())) ||
	!testDer(y,x,
	      Float(1)/ Float((x.value()*log(10.0))))) {
      cerr << "log10(const SparseDiff<T> &) failed" << endl;
      nerr++;
    }
    
    // pow(x,y) : derivative = y*pow(x,y-1)*dx + pow(x,y)*log(x)*dy
    y = pow(w,z);
    if (!near(y.value(), Float(pow(w.value(), z.value())), 1E-7) ||
	!near(y.derivative(0).second,
	      (Float(z.value()*pow(w.value(),z.value()-1))*
	       w.derivative(0).second), 1E-7) ||
	!near(y.derivative(1).second,
	      Float(pow(w.value(),z.value())*log(w.value()))*
	      z.derivative(0).second, 1E-7)) {
      cerr << "pow(const SparseDiff<T> &, const SparseDiff<T> &) failed" << endl;
      nerr++;
    }

    // pow(x,const) : derivative = const*pow(x,const-1)*dx
    y = pow((SparseDiff<Float>&)x,Float(2.5));
    if (y.value() != Float(pow(x.value(),2.5)) ||
	!testDer(y,x,
	       Float(2.5*pow(x.value(),1.5)))) {
      cerr << "pow(const SparseDiff<T> &, const double &) failed" << endl;
      nerr++;
    }
    
    // sin(x) : derivative = cos(x)
    y = sin(x);
    if (!allNearAbs(y.value(), Float(sin(x.value())) ) ||
	!testDer(y,x,
		 Float(cos(x.value())))) {
      cerr << "sin(const SparseDiff<T> &) failed" << endl;
      nerr++;
    }
    
    // sinh(x) : derivative = cosh(x)
    y = sinh(x);
    if (!allNearAbs(y.value(), Float(sinh(x.value()))) ||
	!testDer(y,x,
		 Float(cosh(x.value())))) {
      cerr << "sinh(const SparseDiff<T> &) failed" << endl;
      nerr++;
    }
    
    // sqrt(x) : derivative = 0.5/sqrt(x)
    y = sqrt(x);
    if (!allNearAbs(y.value(), Float(sqrt(x.value()))) ||
	!testDer(y,x,
		 Float(0.5/sqrt(x.value())))) {
      cerr << "sqrt(const SparseDiff<T> &) failed" << endl;
      nerr++;
    }
    
    // tan(x) : derivative = sec(x)*sec(x) = 1/(cos(x)*cos(x))
    y = tan(x);
    if (!allNearAbs(y.value(), Float(tan(x.value())) ) ||
	!testDer(y,x,
		 Float(1)/Float(cos(x.value())*cos(x.value())))) {
      cerr << "tan(const SparseDiff<T> &) failed" << endl;
      nerr++;
    }
    
    // tanh(x) : derivative = sech(x)*sech(x) = 1/(cosh(x)*cosh(x))
    y = tanh(x);
    if (!allNearAbs(y.value(), Float(tanh(x.value())) ) ||
	!testDer(y,x,
		 Float(1)/Float(cosh(x.value())*cosh(x.value())))) {
      cerr << "sinh(const SparseDiff<T> &) failed" << endl;
      nerr++;
    }
  }

  if (nerr != 0) cout << "There were " << nerr << " errors" << endl;
  else cout << "ok" << endl;

  return nerr;
}

