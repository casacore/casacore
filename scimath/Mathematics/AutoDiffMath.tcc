//# AutoDiffMath.cc: Implements all mathematical functions for AutoDiff.
//# Copyright (C) 1995,1996,1999,2001,2002,2004
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
//# $Id$

#ifndef SCIMATH_AUTODIFFMATH_TCC
#define SCIMATH_AUTODIFFMATH_TCC

//# Includes
#include <casacore/scimath/Mathematics/AutoDiffMath.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/BasicSL/Constants.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// Unary arithmetic operators.
template<class T> AutoDiff<T> operator+(const AutoDiff<T> &other) { 
  AutoDiff<T> tmp(other);
  return tmp.ref();
}

template<class T> AutoDiff<T> operator-(const AutoDiff<T> &other) { 
  AutoDiff<T> tmp(other);
  tmp *= T(-1);
  return tmp.ref();
}

// Binary arithmetic operators
template<class T> 
AutoDiff<T> operator+(const AutoDiff<T> &left, const AutoDiff<T> &right) {
  if (left.theRep()->nd_p == 0) return (left.theRep()->val_p + right);
  AutoDiff<T> tmp(left);
  tmp += right;
  return tmp.ref();
}

template<class T> 
AutoDiff<T> operator-(const AutoDiff<T> &left, const AutoDiff<T> &right) {
  if (left.theRep()->nd_p == 0) return (left.theRep()->val_p - right);
  AutoDiff<T> tmp(left);
  tmp -= right;
  return tmp.ref();
}

template<class T> 
AutoDiff<T> operator*(const AutoDiff<T> &left, const AutoDiff<T> &right) {
  if (left.theRep()->nd_p == 0) return (left.theRep()->val_p * right);
  AutoDiff<T> tmp(left);
  tmp *= right;
  return tmp.ref();
}

template<class T> 
AutoDiff<T> operator/(const AutoDiff<T> &left, const AutoDiff<T> &right) {
  if (left.theRep()->nd_p == 0) return (left.theRep()->val_p / right);
  AutoDiff<T> tmp(left);
  tmp /= right;
  return tmp.ref();
}

template<class T> 
AutoDiff<T> operator+(const AutoDiff<T> &left, const T &right) { 
  AutoDiff<T> tmp(left);
  tmp += right;
  return tmp.ref();
}

template<class T> 
AutoDiff<T> operator-(const AutoDiff<T> &left, const T &right) { 
  AutoDiff<T> tmp(left);
  tmp -= right;
  return tmp.ref();
}

template<class T> 
AutoDiff<T> operator* (const AutoDiff<T> &left, const T &right) { 
  AutoDiff<T> tmp(left);
  tmp *= right;
  return tmp.ref();
}

template<class T> 
AutoDiff<T> operator/(const AutoDiff<T> &left, const T &right) {
  AutoDiff<T> tmp(left);
  tmp /= right;
  return tmp.ref();
}

template<class T> 
AutoDiff<T> operator+(const T &left, const AutoDiff<T> &right) { 
  AutoDiff<T> tmp(right);
  tmp += left;
  return tmp.ref();
}

template<class T> 
AutoDiff<T> operator-(const T &left, const AutoDiff<T> &right) { 
  AutoDiff<T> tmp(right);
  tmp *= T(-1);
  tmp += left;
  return tmp.ref();
}

template<class T> 
AutoDiff<T> operator*(const T &left, const AutoDiff<T> &right) { 
  AutoDiff<T> tmp(right);
  tmp *= left;
  return tmp.ref();
}

template<class T> 
AutoDiff<T> operator/(const T &left, const AutoDiff<T> &right) { 
  AutoDiff<T> tmp(right);
  T tv(right.theRep()->val_p);
  tmp.theRep()->val_p = left/tv;
  tmp.theRep()->grad_p *= -tmp.theRep()->val_p/tv;
  return tmp.ref();
}

template<class T> AutoDiff<T> acos(const AutoDiff<T> &ad) {
  AutoDiff<T> tmp(ad);
  T tv = tmp.theRep()->val_p;
  tmp.theRep()->grad_p /= T(-sqrt(T(1) - tv*tv));
  tmp.theRep()->val_p = acos(tv);
  return tmp.ref();
}

template<class T> AutoDiff<T> asin(const AutoDiff<T> &ad) {
  AutoDiff<T> tmp(ad);
  T tv = tmp.theRep()->val_p;
  tmp.theRep()->grad_p /= T(sqrt(T(1) - tv*tv));
  tmp.theRep()->val_p = asin(tv);
  return tmp.ref();
}

template<class T> AutoDiff<T> atan(const AutoDiff<T> &ad) {
  AutoDiff<T> tmp(ad);
  T tv = tmp.theRep()->val_p;
  tmp.theRep()->grad_p /= T(1) + tv*tv;
  tmp.theRep()->val_p = atan(tv);
  return tmp.ref();
}

template<class T>
AutoDiff<T> atan2(const AutoDiff<T> &y, const AutoDiff<T> &x) {  
  // this gets the derivative right, via the chain rule using the already
  // defined / and atan functions, but the value may be wrong
  AutoDiff<T> tmp = atan(y/x);
  // get the value right
  tmp.theRep()->val_p = atan2(y.theRep()->val_p, x.theRep()->val_p);
  return tmp.ref();
}

template<class T> AutoDiff<T> cos(const AutoDiff<T> &ad) { 
  AutoDiff<T> tmp(ad);
  T tv = tmp.theRep()->val_p;
  tmp.theRep()->grad_p *= T(-sin(tv));
  tmp.theRep()->val_p = cos(tv);
  return tmp.ref();
}

template<class T> AutoDiff<T> cosh(const AutoDiff<T> &ad) { 
  AutoDiff<T> tmp(ad);
  T tv = tmp.theRep()->val_p;
  tmp.theRep()->grad_p *= T(sinh(tv));
  tmp.theRep()->val_p = cosh(tv);
  return tmp.ref();
}

template<class T> AutoDiff<T> sin(const AutoDiff<T> &ad) { 
  AutoDiff<T> tmp(ad);
  T tv = tmp.theRep()->val_p;
  tmp.theRep()->grad_p *= T(cos(tv));
  tmp.theRep()->val_p = sin(tv);
  return tmp.ref();
}

template<class T> AutoDiff<T> sinh(const AutoDiff<T> &ad) { 
  AutoDiff<T> tmp(ad);
  T tv = tmp.theRep()->val_p;
  tmp.theRep()->grad_p *= T(cosh(tv));
  tmp.theRep()->val_p = sinh(tv);
  return tmp.ref();
}

template<class T> AutoDiff<T> exp(const AutoDiff<T> &ad) { 
  AutoDiff<T> tmp(ad);
  tmp.theRep()->val_p = exp(ad.theRep()->val_p);
  tmp.theRep()->grad_p *= tmp.theRep()->val_p;
  return tmp.ref();
}

template<class T> AutoDiff<T> log(const AutoDiff<T> &ad) { 
  AutoDiff<T> tmp(ad);
  T tv = tmp.theRep()->val_p;
  tmp.theRep()->grad_p /= tv;
  tmp.theRep()->val_p = log(tv);
  return tmp.ref();
}

template<class T> AutoDiff<T> log10(const AutoDiff<T> &ad) {
  static const T l10 = T(log(10.0));
  AutoDiff<T> tmp(ad);
  T tv = tmp.theRep()->val_p;
  tmp.theRep()->grad_p /= tv*l10;
  tmp.theRep()->val_p = log10(tv);
  return tmp.ref();
}

template<class T> AutoDiff<T> erf(const AutoDiff<T> &ad) { 
  AutoDiff<T> tmp(ad);
  T tv = tmp.theRep()->val_p;
  tmp.theRep()->grad_p *= T(T(C::_2_sqrtpi)*exp(-tv*tv));
  tmp.theRep()->val_p = erf(tv);
  return tmp.ref();
}

template<class T> AutoDiff<T> erfc(const AutoDiff<T> &ad) { 
  AutoDiff<T> tmp(ad);
  T tv = tmp.theRep()->val_p;
  tmp.theRep()->grad_p *= T(T(-C::_2_sqrtpi)*exp(-tv*tv));
  tmp.theRep()->val_p = erfc(tv);
  return tmp.ref();
}

template<class T> 
AutoDiff<T> pow(const AutoDiff<T> &a, const AutoDiff<T> &b) {
  if (b.theRep()->nd_p == 0) return pow(a, b.theRep()->val_p);
  T ta = a.theRep()->val_p;
  T tb = b.theRep()->val_p;
  T value = pow(ta, tb);
  T temp2 = tb * pow(ta, tb - T(1));
  AutoDiff<T> tmp(b);
  tmp.theRep()->grad_p *= value * T(log(ta));
  for (uInt i=0; i<a.theRep()->nd_p; i++) {
    tmp.theRep()->grad_p[i] += a.theRep()->grad_p[i]*temp2;
  }
  tmp.theRep()->val_p = value;
  return tmp.ref();
}

template<class T> AutoDiff<T> pow(const AutoDiff<T> &a, const T &b) {
  AutoDiff<T> tmp(a);
  T ta = a.theRep()->val_p;
  tmp.theRep()->grad_p *= b*pow(ta, b-T(1));
  tmp.theRep()->val_p = pow(ta, b);
  return tmp.ref();
}

template<class T> AutoDiff<T> square(const AutoDiff<T> &ad) {
  AutoDiff<T> tmp(ad);
  tmp.theRep()->val_p = square(tmp.theRep()->val_p);
  tmp.theRep()->grad_p *= T(2)*tmp.theRep()->val_p;
  return tmp.ref();
}

template<class T> AutoDiff<T> cube(const AutoDiff<T> &ad) {
  AutoDiff<T> tmp(ad);
  tmp.theRep()->val_p = cube(tmp.theRep()->val_p);
  tmp.theRep()->grad_p *= T(3)*square(tmp.theRep()->val_p);
  return tmp.ref();
}

template<class T> AutoDiff<T> sqrt(const AutoDiff<T> &ad) { 
  AutoDiff<T> tmp(ad);
  tmp.theRep()->val_p = sqrt(tmp.theRep()->val_p);
  tmp.theRep()->grad_p /= T(2)*tmp.theRep()->val_p;
  return tmp.ref();
}

template<class T> AutoDiff<T> tan(const AutoDiff<T> &ad) { 
  AutoDiff<T> tmp(ad);
  T tv = tmp.theRep()->val_p;
  T temp = cos(tv);
  temp *= temp;
  tmp.theRep()->grad_p /= temp;
  tmp.theRep()->val_p = tan(tv);
  return tmp.ref();
}

template<class T> AutoDiff<T> tanh(const AutoDiff<T> &ad) { 
  AutoDiff<T> tmp(ad);
  T tv = tmp.theRep()->val_p;
  T temp = cosh(tv);
  temp *= temp;
  tmp.theRep()->grad_p /= temp;
  tmp.theRep()->val_p = tanh(tv);
  return tmp.ref();
}

template<class T> AutoDiff<T> abs(const AutoDiff<T> &ad) { 
  // Here we assume that function F represented by ad is continous and 
  // differentiable in a small enough neighborhood where F is 
  // evaluated. So if ad.theRep()->val_p is positive, F is positive in
  // the small neighborhood.
  AutoDiff<T> tmp(ad);
  if (ad.theRep()->val_p < T(0)) tmp *= T(-1);
  return tmp.ref();
}

template<class T> AutoDiff<T> fmod(const AutoDiff<T> &x, const T &c) { 
  // Floating-point remainder of x/c, with the same sign as x, where c is
  // a constant.  Since fmod(x,c) = x - ((int)(x/c))*c and d[(int)(z)]/dz = 0, 
  // d(fmod(x,c))/dx = 1.  At z = integer, (int)(z) is discontinuous, but
  // away from the point (int)(z) is well defined and has derivative (=0)
  // we use the derivative at z = integer+epsilon as the derivative at
  // z = integer.
  AutoDiff<T> tmp(x);
  tmp.theRep()->val_p = fmod(x.theRep()->val_p, c);
  return tmp.ref();
}

template<class T> AutoDiff<T> fmod(const AutoDiff<T> &x,
				   const AutoDiff<T> &c) { 
  AutoDiff<T> tmp(x);
  tmp.theRep()->val_p = fmod(x.theRep()->val_p, c.theRep()->val_p);
  return tmp.ref();
}

template<class T> AutoDiff<T> floor(const AutoDiff<T> &ad) { 
  AutoDiff<T> tmp(ad);
  tmp.theRep()->val_p = floor(ad.theRep()->val_p);
  tmp.theRep()->grad_p = T(0);
  return tmp.ref();
}

template<class T> AutoDiff<T> ceil(const AutoDiff<T> &ad) { 
  AutoDiff<T> tmp(ad);
  tmp.theRep()->val_p = ceil(ad.theRep()->val_p);
  tmp.theRep()->grad_p = T(0);
  return tmp.ref();
}

template<class T>
Bool operator>(const AutoDiff<T> &left, const AutoDiff<T> &right) {
  return (left.theRep()->val_p > right.theRep()->val_p);
}

template<class T>
Bool operator<(const AutoDiff<T> &left, const AutoDiff<T> &right) {
  return (left.theRep()->val_p < right.theRep()->val_p);
}

template<class T>
Bool operator>=(const AutoDiff<T> &left, const AutoDiff<T> &right) {
  return (left.theRep()->val_p >= right.theRep()->val_p);
}

template<class T>
Bool operator<=(const AutoDiff<T> &left, const AutoDiff<T> &right) {
  return (left.theRep()->val_p <= right.theRep()->val_p);
}

template<class T>
Bool operator==(const AutoDiff<T> &left, const AutoDiff<T> &right) {
  return (left.theRep()->val_p == right.theRep()->val_p);
}

template<class T>
Bool operator!=(const AutoDiff<T> &left, const AutoDiff<T> &right) {
  return (left.theRep()->val_p != right.theRep()->val_p);
}

// Compare an AutoDiff and a constant
template<class T> Bool operator>(const AutoDiff<T> &left,const T &right) {
  return (left.theRep()->val_p > right);
}

template<class T> Bool operator<(const AutoDiff<T> &left,const T &right) {
  return (left.theRep()->val_p < right);
}

template<class T> Bool operator>=(const AutoDiff<T> &left,const T &right) {
  return (left.theRep()->val_p >= right);
}

template<class T> Bool operator<=(const AutoDiff<T> &left,const T &right) {
  return (left.theRep()->val_p <= right);
}

template<class T> Bool operator==(const AutoDiff<T> &left,const T &right) {
  return (left.theRep()->val_p == right);
}

template<class T> Bool operator!=(const AutoDiff<T> &left,const T &right) {
  return (left.theRep()->val_p != right);
}

// Compare a constant and an AutoDiff
template<class T> Bool operator>(const T &left, const AutoDiff<T> &right) {
  return (left > right.theRep()->val_p);
}

template<class T> Bool operator<(const T &left, const AutoDiff<T> &right) {
  return (left < right.theRep()->val_p);
}

template<class T> Bool operator>=(const T &left, const AutoDiff<T> &right) {
  return (left >= right.theRep()->val_p);
}

template<class T> Bool operator<=(const T &left, const AutoDiff<T> &right) {
  return (left <= right.theRep()->val_p);
}

template<class T> Bool operator==(const T &left, const AutoDiff<T> &right) {
  return (left == right.theRep()->val_p);
}

template<class T> Bool operator!=(const T &left, const AutoDiff<T> &right) {
  return (left != right.theRep()->val_p);
}

// Near comparisons

template<class T>
Bool near(const AutoDiff<T> &left, const AutoDiff<T> &right) {
  return (near(left.theRep()->val_p, right.theRep()->val_p));
}

template<class T>
Bool near(const T &left, const AutoDiff<T> &right) {
  return near(left, right.theRep()->val_p);
}

template<class T>
Bool near(const AutoDiff<T> &left, const T &right) {
  return near(left.theRep()->val_p, right);
}

template<class T>
Bool near(const AutoDiff<T> &left, const AutoDiff<T> &right,
	  const Double tol) {
  return near(left.theRep()->val_p, right.theRep()->val_p, tol);
}

template<class T>
Bool near(const T &left, const AutoDiff<T> &right, const Double tol) {
  return near(left, right.theRep()->val_p, tol);
}

template<class T>
Bool near(const AutoDiff<T> &left, const T &right, const Double tol) {
  return near(left.theRep()->val_p, right, tol);
}

template<class T>
Bool allnear(const AutoDiff<T> &left, const AutoDiff<T> &right) {
  return (near(left.theRep()->val_p, right.theRep()->val_p));
}

template<class T>
Bool allnear(const T &left, const AutoDiff<T> &right) {
  return near(left, right.theRep()->val_p);
}

template<class T>
Bool allnear(const AutoDiff<T> &left, const T &right) {
  return near(left.theRep()->val_p, right);
}

template<class T>
Bool allnear(const AutoDiff<T> &left, const AutoDiff<T> &right,
	     const Double tol) {
  return near(left.theRep()->val_p, right.theRep()->val_p, tol);
}

template<class T>
Bool allnear(const T &left, const AutoDiff<T> &right, const Double tol) {
  return near(left, right.theRep()->val_p, tol);
}

template<class T>
Bool allnear(const AutoDiff<T> &left, const T &right, const Double tol) {
  return near(left.theRep()->val_p, right, tol);
}

template<class T>
Bool nearAbs(const AutoDiff<T> &left, const AutoDiff<T> &right) {
  return (nearAbs(left.theRep()->val_p, right.theRep()->val_p));
}

template<class T>
Bool nearAbs(const T &left, const AutoDiff<T> &right) {
  return nearAbs(left, right.theRep()->val_p);
}

template<class T>
Bool nearAbs(const AutoDiff<T> &left, const T &right) {
  return nearAbs(left.theRep()->val_p, right);
}

template<class T>
Bool nearAbs(const AutoDiff<T> &left, const AutoDiff<T> &right,
	     const Double tol) {
  return nearAbs(left.theRep()->val_p, right.theRep()->val_p, tol);
}

template<class T>
Bool nearAbs(const T &left, const AutoDiff<T> &right, const Double tol) {
  return nearAbs(left, right.theRep()->val_p, tol);
}

template<class T>
Bool nearAbs(const AutoDiff<T> &left, const T &right, const Double tol) {
  return nearAbs(left.theRep()->val_p, right, tol);
}

template<class T>
Bool allnearAbs(const AutoDiff<T> &left, const AutoDiff<T> &right) {
  return (nearAbs(left.theRep()->val_p, right.theRep()->val_p));
}

template<class T>
Bool allnearAbs(const T &left, const AutoDiff<T> &right) {
  return nearAbs(left, right.theRep()->val_p);
}

template<class T>
Bool allnearAbs(const AutoDiff<T> &left, const T &right) {
  return nearAbs(left.theRep()->val_p, right);
}

template<class T>
Bool allnearAbs(const AutoDiff<T> &left, const AutoDiff<T> &right,
		const Double tol) {
  return nearAbs(left.theRep()->val_p, right.theRep()->val_p, tol);
}

template<class T>
Bool allnearAbs(const T &left, const AutoDiff<T> &right, const Double tol) {
  return nearAbs(left, right.theRep()->val_p, tol);
}

template<class T>
Bool allnearAbs(const AutoDiff<T> &left, const T &right, const Double tol) {
  return nearAbs(left.theRep()->val_p, right, tol);
}

// Test special values
template<class T>
Bool isNaN (const AutoDiff<T> &val) {
  return isNaN(val.theRep()->val_p);
}

template<class T>
Bool isInf(AutoDiff<T> &val) {
  return isInf(val.theRep()->val_p);
}

template<class T>
AutoDiff<T> min(const AutoDiff<T> &left,
		const AutoDiff<T> &right) {
  AutoDiff<T> tmp = (left.theRep()->val_p <= right.theRep()->val_p) ?
	  left : right;
  return tmp.ref();
}

template<class T>
AutoDiff<T> max(const AutoDiff<T> &left,
		const AutoDiff<T> &right) {
  AutoDiff<T> tmp = (left.theRep()->val_p <= right.theRep()->val_p) ?
	  right : left;
  return tmp.ref();
}

} //# NAMESPACE CASACORE - END


#endif
