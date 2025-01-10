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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

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
  return tmp;
}

template<class T> AutoDiff<T> operator-(const AutoDiff<T> &other) { 
  AutoDiff<T> tmp(other);
  tmp *= T(-1);
  return tmp;
}

// Binary arithmetic operators
template<class T> AutoDiff<T> operator+(const AutoDiff<T> &left, const AutoDiff<T> &right) {
  AutoDiff<T> tmp(left);
  tmp += right;
  return tmp;
}

template<class T> 
AutoDiff<T> operator-(const AutoDiff<T> &left, const AutoDiff<T> &right) {
  AutoDiff<T> tmp(left);
  tmp -= right;
  return tmp;
}

template<class T> 
AutoDiff<T> operator*(const AutoDiff<T> &left, const AutoDiff<T> &right) {
  AutoDiff<T> tmp(left);
  tmp *= right;
  return tmp;
}

template<class T> 
AutoDiff<T> operator/(const AutoDiff<T> &left, const AutoDiff<T> &right) {
  AutoDiff<T> tmp(left);
  tmp /= right;
  return tmp;
}

template<class T> 
AutoDiff<T> operator+(const AutoDiff<T> &left, const T &right) { 
  AutoDiff<T> tmp(left);
  tmp += right;
  return tmp;
}

template<class T> 
AutoDiff<T> operator-(const AutoDiff<T> &left, const T &right) { 
  AutoDiff<T> tmp(left);
  tmp -= right;
  return tmp;
}

template<class T> 
AutoDiff<T> operator* (const AutoDiff<T> &left, const T &right) { 
  AutoDiff<T> tmp(left);
  tmp *= right;
  return tmp;
}

template<class T> 
AutoDiff<T> operator/(const AutoDiff<T> &left, const T &right) {
  AutoDiff<T> tmp(left);
  tmp /= right;
  return tmp;
}

template<class T> 
AutoDiff<T> operator+(const T &left, const AutoDiff<T> &right) { 
  AutoDiff<T> tmp(right);
  tmp += left;
  return tmp;
}

template<class T> 
AutoDiff<T> operator-(const T &left, const AutoDiff<T> &right) { 
  AutoDiff<T> tmp(right);
  tmp *= T(-1);
  tmp += left;
  return tmp;
}

template<class T> 
AutoDiff<T> operator*(const T &left, const AutoDiff<T> &right) { 
  AutoDiff<T> tmp(right);
  tmp *= left;
  return tmp;
}

template<class T> 
AutoDiff<T> operator/(const T &left, const AutoDiff<T> &right) { 
  AutoDiff<T> tmp(right);
  T tv = right.value();
  tmp.value() = left/tv;
  tmp.derivatives() *= -tmp.value()/tv;
  return tmp;
}

template<class T> AutoDiff<T> acos(const AutoDiff<T> &ad) {
  AutoDiff<T> tmp(ad);
  T tv = tmp.value();
  tmp.derivatives() /= T(-sqrt(T(1) - tv*tv));
  tmp.value() = acos(tv);
  return tmp;
}

template<class T> AutoDiff<T> asin(const AutoDiff<T> &ad) {
  AutoDiff<T> tmp(ad);
  T tv = tmp.value();
  tmp.derivatives() /= T(sqrt(T(1) - tv*tv));
  tmp.value() = asin(tv);
  return tmp;
}

template<class T> AutoDiff<T> atan(const AutoDiff<T> &ad) {
  AutoDiff<T> tmp(ad);
  T tv = tmp.value();
  tmp.derivatives() /= T(1) + tv*tv;
  tmp.value() = atan(tv);
  return tmp;
}

template<class T>
AutoDiff<T> atan2(const AutoDiff<T> &y, const AutoDiff<T> &x) {  
  // this gets the derivative right, via the chain rule using the already
  // defined / and atan functions, but the value may be wrong
  AutoDiff<T> tmp = atan(y/x);
  // get the value right
  tmp.value() = atan2(y.value(), x.value());
  return tmp;
}

template<class T> AutoDiff<T> cos(const AutoDiff<T> &ad) { 
  AutoDiff<T> tmp(ad);
  T tv = tmp.value();
  tmp.derivatives() *= T(-sin(tv));
  tmp.value() = cos(tv);
  return tmp;
}

template<class T> AutoDiff<T> cosh(const AutoDiff<T> &ad) { 
  AutoDiff<T> tmp(ad);
  T tv = tmp.value();
  tmp.derivatives() *= T(sinh(tv));
  tmp.value() = cosh(tv);
  return tmp;
}

template<class T> AutoDiff<T> sin(const AutoDiff<T> &ad) { 
  AutoDiff<T> tmp(ad);
  T tv = tmp.value();
  tmp.derivatives() *= T(cos(tv));
  tmp.value() = sin(tv);
  return tmp;
}

template<class T> AutoDiff<T> sinh(const AutoDiff<T> &ad) { 
  AutoDiff<T> tmp(ad);
  T tv = tmp.value();
  tmp.derivatives() *= T(cosh(tv));
  tmp.value() = sinh(tv);
  return tmp;
}

template<class T> AutoDiff<T> exp(const AutoDiff<T> &ad) { 
  AutoDiff<T> tmp(ad);
  tmp.value() = exp(ad.value());
  tmp.derivatives() *= tmp.value();
  return tmp;
}

template<class T> AutoDiff<T> log(const AutoDiff<T> &ad) { 
  AutoDiff<T> tmp(ad);
  T tv = tmp.value();
  tmp.derivatives() /= tv;
  tmp.value() = log(tv);
  return tmp;
}

template<class T> AutoDiff<T> log10(const AutoDiff<T> &ad) {
  static const T l10 = T(log(10.0));
  AutoDiff<T> tmp(ad);
  T tv = tmp.value();
  tmp.derivatives() /= tv*l10;
  tmp.value() = log10(tv);
  return tmp;
}

template<class T> AutoDiff<T> erf(const AutoDiff<T> &ad) { 
  AutoDiff<T> tmp(ad);
  T tv = tmp.value();
  tmp.derivatives() *= T(T(M_2_SQRTPI)*exp(-tv*tv));
  tmp.value() = erf(tv);
  return tmp;
}

template<class T> AutoDiff<T> erfc(const AutoDiff<T> &ad) { 
  AutoDiff<T> tmp(ad);
  T tv = tmp.value();
  tmp.derivatives() *= T(T(-M_2_SQRTPI)*exp(-tv*tv));
  tmp.value() = erfc(tv);
  return tmp;
}

template<class T> 
AutoDiff<T> pow(const AutoDiff<T> &a, const AutoDiff<T> &b) {
  if (b.nDerivatives() == 0) return pow(a, b.value());
  T ta = a.value();
  T tb = b.value();
  T value = pow(ta, tb);
  T temp2 = tb * pow(ta, tb - T(1));
  AutoDiff<T> tmp(b);
  tmp.derivatives() *= value * T(log(ta));
  for (uInt i=0; i<a.nDerivatives(); i++) {
    tmp.derivatives()[i] += a.derivatives()[i]*temp2;
  }
  tmp.value() = value;
  return tmp;
}

template<class T> AutoDiff<T> pow(const AutoDiff<T> &a, const T &b) {
  AutoDiff<T> tmp(a);
  T ta = a.value();
  tmp.derivatives() *= b*pow(ta, b-T(1));
  tmp.value() = pow(ta, b);
  return tmp;
}

template<class T> AutoDiff<T> square(const AutoDiff<T> &ad) {
  AutoDiff<T> tmp(ad);
  tmp.value() = square(tmp.value());
  tmp.derivatives() *= T(2)*tmp.value();
  return tmp;
}

template<class T> AutoDiff<T> cube(const AutoDiff<T> &ad) {
  AutoDiff<T> tmp(ad);
  tmp.value() = cube(tmp.value());
  tmp.derivatives() *= T(3)*square(tmp.value());
  return tmp;
}

template<class T> AutoDiff<T> sqrt(const AutoDiff<T> &ad) { 
  AutoDiff<T> tmp(ad);
  tmp.value() = sqrt(tmp.value());
  tmp.derivatives() /= T(2)*tmp.value();
  return tmp;
}

template<class T> AutoDiff<T> tan(const AutoDiff<T> &ad) { 
  AutoDiff<T> tmp(ad);
  T tv = tmp.value();
  T temp = cos(tv);
  temp *= temp;
  tmp.derivatives() /= temp;
  tmp.value() = tan(tv);
  return tmp;
}

template<class T> AutoDiff<T> tanh(const AutoDiff<T> &ad) { 
  AutoDiff<T> tmp(ad);
  T tv = tmp.value();
  T temp = cosh(tv);
  temp *= temp;
  tmp.derivatives() /= temp;
  tmp.value() = tanh(tv);
  return tmp;
}

template<class T> AutoDiff<T> abs(const AutoDiff<T> &ad) { 
  // Here we assume that function F represented by ad is continous and 
  // differentiable in a small enough neighborhood where F is 
  // evaluated. So if ad.value() is positive, F is positive in
  // the small neighborhood.
  AutoDiff<T> tmp(ad);
  if (ad.value() < T(0)) tmp *= T(-1);
  return tmp;
}

template<class T> AutoDiff<T> fmod(const AutoDiff<T> &x, const T &c) { 
  // Floating-point remainder of x/c, with the same sign as x, where c is
  // a constant.  Since fmod(x,c) = x - ((int)(x/c))*c and d[(int)(z)]/dz = 0, 
  // d(fmod(x,c))/dx = 1.  At z = integer, (int)(z) is discontinuous, but
  // away from the point (int)(z) is well defined and has derivative (=0)
  // we use the derivative at z = integer+epsilon as the derivative at
  // z = integer.
  AutoDiff<T> tmp(x);
  tmp.value() = fmod(x.value(), c);
  return tmp;
}

template<class T> AutoDiff<T> fmod(const AutoDiff<T> &x,
				   const AutoDiff<T> &c) { 
  AutoDiff<T> tmp(x);
  tmp.value() = fmod(x.value(), c.value());
  return tmp;
}

template<class T> AutoDiff<T> floor(const AutoDiff<T> &ad) { 
  AutoDiff<T> tmp(ad);
  tmp.value() = floor(ad.value());
  tmp.derivatives() = T(0);
  return tmp;
}

template<class T> AutoDiff<T> ceil(const AutoDiff<T> &ad) { 
  AutoDiff<T> tmp(ad);
  tmp.value() = ceil(ad.value());
  tmp.derivatives() = T(0);
  return tmp;
}

template<class T>
Bool operator>(const AutoDiff<T> &left, const AutoDiff<T> &right) {
  return (left.value() > right.value());
}

template<class T>
Bool operator<(const AutoDiff<T> &left, const AutoDiff<T> &right) {
  return (left.value() < right.value());
}

template<class T>
Bool operator>=(const AutoDiff<T> &left, const AutoDiff<T> &right) {
  return (left.value() >= right.value());
}

template<class T>
Bool operator<=(const AutoDiff<T> &left, const AutoDiff<T> &right) {
  return (left.value() <= right.value());
}

template<class T>
Bool operator==(const AutoDiff<T> &left, const AutoDiff<T> &right) {
  return (left.value() == right.value());
}

template<class T>
Bool operator!=(const AutoDiff<T> &left, const AutoDiff<T> &right) {
  return (left.value() != right.value());
}

// Compare an AutoDiff and a constant
template<class T> Bool operator>(const AutoDiff<T> &left,const T &right) {
  return (left.value() > right);
}

template<class T> Bool operator<(const AutoDiff<T> &left,const T &right) {
  return (left.value() < right);
}

template<class T> Bool operator>=(const AutoDiff<T> &left,const T &right) {
  return (left.value() >= right);
}

template<class T> Bool operator<=(const AutoDiff<T> &left,const T &right) {
  return (left.value() <= right);
}

template<class T> Bool operator==(const AutoDiff<T> &left,const T &right) {
  return (left.value() == right);
}

template<class T> Bool operator!=(const AutoDiff<T> &left,const T &right) {
  return (left.value() != right);
}

// Compare a constant and an AutoDiff
template<class T> Bool operator>(const T &left, const AutoDiff<T> &right) {
  return (left > right.value());
}

template<class T> Bool operator<(const T &left, const AutoDiff<T> &right) {
  return (left < right.value());
}

template<class T> Bool operator>=(const T &left, const AutoDiff<T> &right) {
  return (left >= right.value());
}

template<class T> Bool operator<=(const T &left, const AutoDiff<T> &right) {
  return (left <= right.value());
}

template<class T> Bool operator==(const T &left, const AutoDiff<T> &right) {
  return (left == right.value());
}

template<class T> Bool operator!=(const T &left, const AutoDiff<T> &right) {
  return (left != right.value());
}

// Near comparisons

template<class T>
Bool near(const AutoDiff<T> &left, const AutoDiff<T> &right) {
  return (near(left.value(), right.value()));
}

template<class T>
Bool near(const T &left, const AutoDiff<T> &right) {
  return near(left, right.value());
}

template<class T>
Bool near(const AutoDiff<T> &left, const T &right) {
  return near(left.value(), right);
}

template<class T>
Bool near(const AutoDiff<T> &left, const AutoDiff<T> &right,
	  const Double tol) {
  return near(left.value(), right.value(), tol);
}

template<class T>
Bool near(const T &left, const AutoDiff<T> &right, const Double tol) {
  return near(left, right.value(), tol);
}

template<class T>
Bool near(const AutoDiff<T> &left, const T &right, const Double tol) {
  return near(left.value(), right, tol);
}

template<class T>
Bool allnear(const AutoDiff<T> &left, const AutoDiff<T> &right) {
  return (near(left.value(), right.value()));
}

template<class T>
Bool allnear(const T &left, const AutoDiff<T> &right) {
  return near(left, right.value());
}

template<class T>
Bool allnear(const AutoDiff<T> &left, const T &right) {
  return near(left.value(), right);
}

template<class T>
Bool allnear(const AutoDiff<T> &left, const AutoDiff<T> &right,
	     const Double tol) {
  return near(left.value(), right.value(), tol);
}

template<class T>
Bool allnear(const T &left, const AutoDiff<T> &right, const Double tol) {
  return near(left, right.value(), tol);
}

template<class T>
Bool allnear(const AutoDiff<T> &left, const T &right, const Double tol) {
  return near(left.value(), right, tol);
}

template<class T>
Bool nearAbs(const AutoDiff<T> &left, const AutoDiff<T> &right) {
  return (nearAbs(left.value(), right.value()));
}

template<class T>
Bool nearAbs(const T &left, const AutoDiff<T> &right) {
  return nearAbs(left, right.value());
}

template<class T>
Bool nearAbs(const AutoDiff<T> &left, const T &right) {
  return nearAbs(left.value(), right);
}

template<class T>
Bool nearAbs(const AutoDiff<T> &left, const AutoDiff<T> &right,
	     const Double tol) {
  return nearAbs(left.value(), right.value(), tol);
}

template<class T>
Bool nearAbs(const T &left, const AutoDiff<T> &right, const Double tol) {
  return nearAbs(left, right.value(), tol);
}

template<class T>
Bool nearAbs(const AutoDiff<T> &left, const T &right, const Double tol) {
  return nearAbs(left.value(), right, tol);
}

template<class T>
Bool allnearAbs(const AutoDiff<T> &left, const AutoDiff<T> &right) {
  return (nearAbs(left.value(), right.value()));
}

template<class T>
Bool allnearAbs(const T &left, const AutoDiff<T> &right) {
  return nearAbs(left, right.value());
}

template<class T>
Bool allnearAbs(const AutoDiff<T> &left, const T &right) {
  return nearAbs(left.value(), right);
}

template<class T>
Bool allnearAbs(const AutoDiff<T> &left, const AutoDiff<T> &right,
		const Double tol) {
  return nearAbs(left.value(), right.value(), tol);
}

template<class T>
Bool allnearAbs(const T &left, const AutoDiff<T> &right, const Double tol) {
  return nearAbs(left, right.value(), tol);
}

template<class T>
Bool allnearAbs(const AutoDiff<T> &left, const T &right, const Double tol) {
  return nearAbs(left.value(), right, tol);
}

// Test special values
template<class T>
Bool isNaN (const AutoDiff<T> &val) {
  return isNaN(val.value());
}

template<class T>
Bool isInf(AutoDiff<T> &val) {
  return isInf(val.value());
}

template<class T>
AutoDiff<T> min(const AutoDiff<T> &left,
		const AutoDiff<T> &right) {
  AutoDiff<T> tmp = (left.value() <= right.value()) ?
	  left : right;
  return tmp;
}

template<class T>
AutoDiff<T> max(const AutoDiff<T> &left,
		const AutoDiff<T> &right) {
  AutoDiff<T> tmp = (left.value() <= right.value()) ?
	  right : left;
  return tmp;
}

} //# NAMESPACE CASACORE - END


#endif
