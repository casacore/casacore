//# SparseDiffMath.cc: Implements all mathematical functions for SparseDiff.
//# Copyright (C) 2007
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
//# $Id: SparseDiffMath.cc,v 1.1 2007/11/16 04:34:46 wbrouw Exp $

//# Includes
#include <scimath/Mathematics/SparseDiffMath.h>
#include <scimath/Mathematics/SparseDiffRep.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/BasicSL/Constants.h>

namespace casa { //# NAMESPACE CASA - BEGIN

  // Unary arithmetic operators.
  template<class T> SparseDiff<T> operator+(const SparseDiff<T> &other) { 
    SparseDiff<T> tmp(other);
    return tmp;
  }

  template<class T> SparseDiff<T> operator-(const SparseDiff<T> &other) { 
    SparseDiff<T> tmp(other);
    tmp *= T(-1);
    return tmp;
  }

  // Binary arithmetic operators
  template<class T> 
  SparseDiff<T> operator+(const SparseDiff<T> &left,
			  const SparseDiff<T> &right) {
    SparseDiff<T> tmp(left);
    tmp += right;
    return tmp;
  }

  template<class T> 
  SparseDiff<T> operator-(const SparseDiff<T> &left,
			  const SparseDiff<T> &right) {
    SparseDiff<T> tmp(left);
    tmp -= right;
    return tmp;
  }

  template<class T> 
  SparseDiff<T> operator*(const SparseDiff<T> &left,
			  const SparseDiff<T> &right) {
    SparseDiff<T> tmp(left);
    tmp *= right;
    return tmp;
  }

  template<class T> 
  SparseDiff<T> operator/(const SparseDiff<T> &left,
			  const SparseDiff<T> &right) {
    SparseDiff<T> tmp(left);
    tmp /= right;
    return tmp;
  }

  template<class T> 
  SparseDiff<T> operator+(const SparseDiff<T> &left, const T &right) { 
    SparseDiff<T> tmp(left);
    tmp += right;
    return tmp;
  }

  template<class T> 
  SparseDiff<T> operator-(const SparseDiff<T> &left, const T &right) { 
    SparseDiff<T> tmp(left);
    tmp -= right;
    return tmp;
  }

  template<class T> 
  SparseDiff<T> operator* (const SparseDiff<T> &left, const T &right) { 
    SparseDiff<T> tmp(left);
    tmp *= right;
    return tmp;
  }

  template<class T> 
  SparseDiff<T> operator/(const SparseDiff<T> &left, const T &right) {
    SparseDiff<T> tmp(left);
    tmp /= right;
    return tmp;
  }

  template<class T> 
  SparseDiff<T> operator+(const T &left, const SparseDiff<T> &right) { 
    SparseDiff<T> tmp(right);
    tmp += left;
    return tmp;
  }

  template<class T> 
  SparseDiff<T> operator-(const T &left, const SparseDiff<T> &right) { 
    SparseDiff<T> tmp(left);
    tmp -= right;
    return tmp;
  }

  template<class T> 
  SparseDiff<T> operator*(const T &left, const SparseDiff<T> &right) { 
    SparseDiff<T> tmp(right);
    tmp *= left;
    return tmp;
  }

  template<class T> 
  SparseDiff<T> operator/(const T &left, const SparseDiff<T> &right) { 
    SparseDiff<T> tmp(left);
    tmp /= right;
    return tmp;
  }

  template<class T> SparseDiff<T> acos(const SparseDiff<T> &ad) {
    SparseDiff<T> tmp(ad);
    T tv = tmp.value();
    *tmp.theRep() /= T(-sqrt(T(1) - tv*tv));
    tmp.value() = acos(tv);
    return tmp;
  }

  template<class T> SparseDiff<T> asin(const SparseDiff<T> &ad) {
    SparseDiff<T> tmp(ad);
    T tv = tmp.value();
    *tmp.theRep() /= T(sqrt(T(1) - tv*tv));
    tmp.value() = asin(tv);
    return tmp;
  }

  template<class T> SparseDiff<T> atan(const SparseDiff<T> &ad) {
    SparseDiff<T> tmp(ad);
    T tv = tmp.value();
    *tmp.theRep() /= T(1) + tv*tv;
    tmp.value() = atan(tv);
    return tmp;
  }

  template<class T>
  SparseDiff<T> atan2(const SparseDiff<T> &y, const SparseDiff<T> &x) {  
    // this gets the derivative right, via the chain rule using the already
    // defined / and atan functions, but the value may be wrong
    SparseDiff<T> tmp = atan(y/x);
    // get the value right
    tmp.value() = atan2(y.value(), x.value());
    return tmp;
  }

  template<class T> SparseDiff<T> cos(const SparseDiff<T> &ad) { 
    SparseDiff<T> tmp(ad);
    T tv = tmp.value();
    *tmp.theRep() *= T(-sin(tv));
    tmp.value() = cos(tv);
    return tmp;
  }

  template<class T> SparseDiff<T> cosh(const SparseDiff<T> &ad) { 
    SparseDiff<T> tmp(ad);
    T tv = tmp.value();
    *tmp.theRep() *= T(sinh(tv));
    tmp.value() = cosh(tv);
    return tmp;
  }

  template<class T> SparseDiff<T> sin(const SparseDiff<T> &ad) { 
    SparseDiff<T> tmp(ad);
    T tv = tmp.value();
    *tmp.theRep() *= T(cos(tv));
    tmp.value() = sin(tv);
    return tmp;
  }

  template<class T> SparseDiff<T> sinh(const SparseDiff<T> &ad) { 
    SparseDiff<T> tmp(ad);
    T tv = tmp.value();
    *tmp.theRep() *= T(cosh(tv));
    tmp.value() = sinh(tv);
    return tmp;
  }

  template<class T> SparseDiff<T> exp(const SparseDiff<T> &ad) { 
    SparseDiff<T> tmp(ad);
    tmp.value() = exp(ad.value());
    *tmp.theRep() *= tmp.value();
    return tmp;
  }

  template<class T> SparseDiff<T> log(const SparseDiff<T> &ad) { 
    SparseDiff<T> tmp(ad);
    T tv = tmp.value();
    *tmp.theRep() /= tv;
    tmp.value() = log(tv);
    return tmp;
  }

  template<class T> SparseDiff<T> log10(const SparseDiff<T> &ad) {
    static const T l10 = T(log(10.0));
    SparseDiff<T> tmp(ad);
    T tv = tmp.value();
    *tmp.theRep() /= tv*l10;
    tmp.value() = log10(tv);
    return tmp;
  }

  template<class T> SparseDiff<T> erf(const SparseDiff<T> &ad) { 
    SparseDiff<T> tmp(ad);
    T tv = tmp.value();
    *tmp.theRep() *= T(T(C::_2_sqrtpi)*exp(-tv*tv));
    tmp.value() = erf(tv);
    return tmp;
  }

  template<class T> SparseDiff<T> erfc(const SparseDiff<T> &ad) { 
    SparseDiff<T> tmp(ad);
    T tv = tmp.value();
    *tmp.theRep() *= T(T(-C::_2_sqrtpi)*exp(-tv*tv));
    tmp.value() = erfc(tv);
    return tmp;
  }

  template<class T> 
  SparseDiff<T> pow(const SparseDiff<T> &a, const SparseDiff<T> &b) {
    if (b.isConstant()) return pow(a, b.value());
    T ta = a.value();
    T tb = b.value();
    T val = pow(ta, tb);
    T temp2 = tb * pow(ta, tb - T(1));
    SparseDiff<T> tmp(b);
    *tmp.theRep() *= val * T(log(ta));
    SparseDiff<T> tmpa(a);
    tmpa *= temp2;
    tmp += tmpa;
    tmp.value() = val;
    return tmp;
  }

  template<class T> SparseDiff<T> pow(const SparseDiff<T> &a, const T &b) {
    SparseDiff<T> tmp(a);
    T ta = a.value();
    *tmp.theRep() *= b*pow(ta, b-T(1));
    tmp.value() = pow(ta, b);
    return tmp;
  }

  template<class T> SparseDiff<T> square(const SparseDiff<T> &ad) {
    SparseDiff<T> tmp(ad);
    tmp.value() = square(tmp.value());
    *tmp.theRep() *= T(2)*tmp.value();
    return tmp;
  }

  template<class T> SparseDiff<T> cube(const SparseDiff<T> &ad) {
    SparseDiff<T> tmp(ad);
    tmp.value() = cube(tmp.value());
    *tmp.theRep() *= T(3)*square(tmp.value());
    return tmp;
  }

  template<class T> SparseDiff<T> sqrt(const SparseDiff<T> &ad) { 
    SparseDiff<T> tmp(ad);
    tmp.value() = sqrt(tmp.value());
    *tmp.theRep() /= T(2)*tmp.value();
    return tmp;
  }

  template<class T> SparseDiff<T> tan(const SparseDiff<T> &ad) { 
    SparseDiff<T> tmp(ad);
    T tv = tmp.value();
    T temp = cos(tv);
    temp *= temp;
    *tmp.theRep() /= temp;
    tmp.value() = tan(tv);
    return tmp;
  }

  template<class T> SparseDiff<T> tanh(const SparseDiff<T> &ad) { 
    SparseDiff<T> tmp(ad);
    T tv = tmp.value();
    T temp = cosh(tv);
    temp *= temp;
    *tmp.theRep() /= temp;
    tmp.value() = tanh(tv);
    return tmp;
  }

  template<class T> SparseDiff<T> abs(const SparseDiff<T> &ad) { 
    // Here we assume that function F represented by ad is continous and 
    // differentiable in a small enough neighborhood where F is 
    // evaluated. So if ad.value() is positive, F is positive in
    // the small neighborhood.
    SparseDiff<T> tmp(ad);
    if (ad.value() < T(0)) tmp *= T(-1);
    return tmp;
  }

  template<class T> SparseDiff<T> fmod(const SparseDiff<T> &x, const T &c) { 
    // Floating-point remainder of x/c, with the same sign as x, where c is
    // a constant.  Since fmod(x,c) = x - ((int)(x/c))*c and d[(int)(z)]/dz = 0, 
    // d(fmod(x,c))/dx = 1.  At z = integer, (int)(z) is discontinuous, but
    // away from the point (int)(z) is well defined and has derivative (=0)
    // we use the derivative at z = integer+epsilon as the derivative at
    // z = integer.
    SparseDiff<T> tmp(x);
    tmp.value() = fmod(x.value(), c);
    return tmp;
  }

  template<class T> SparseDiff<T> fmod(const SparseDiff<T> &x,
				       const SparseDiff<T> &c) { 
    SparseDiff<T> tmp(x);
    tmp.value() = fmod(x.value(), c.value());
    return tmp;
  }

  template<class T> SparseDiff<T> floor(const SparseDiff<T> &ad) { 
    SparseDiff<T> tmp(floor(ad.value()));
    return tmp;
  }

  template<class T> SparseDiff<T> ceil(const SparseDiff<T> &ad) { 
    SparseDiff<T> tmp(ceil(ad.value()));
    return tmp;
  }

  template<class T>
  Bool operator>(const SparseDiff<T> &left, const SparseDiff<T> &right) {
    return (left.value() > right.value());
  }

  template<class T>
  Bool operator<(const SparseDiff<T> &left, const SparseDiff<T> &right) {
    return (left.value() < right.value());
  }

  template<class T>
  Bool operator>=(const SparseDiff<T> &left, const SparseDiff<T> &right) {
    return (left.value() >= right.value());
  }

  template<class T>
  Bool operator<=(const SparseDiff<T> &left, const SparseDiff<T> &right) {
    return (left.value() <= right.value());
  }

  template<class T>
  Bool operator==(const SparseDiff<T> &left, const SparseDiff<T> &right) {
    return (left.value() == right.value());
  }

  template<class T>
  Bool operator!=(const SparseDiff<T> &left, const SparseDiff<T> &right) {
    return (left.value() != right.value());
  }

  // Compare an SparseDiff and a constant
  template<class T> Bool operator>(const SparseDiff<T> &left,const T &right) {
    return (left.value() > right);
  }

  template<class T> Bool operator<(const SparseDiff<T> &left,const T &right) {
    return (left.value() < right);
  }

  template<class T> Bool operator>=(const SparseDiff<T> &left,const T &right) {
    return (left.value() >= right);
  }

  template<class T> Bool operator<=(const SparseDiff<T> &left,const T &right) {
    return (left.value() <= right);
  }

  template<class T> Bool operator==(const SparseDiff<T> &left,const T &right) {
    return (left.value() == right);
  }

  template<class T> Bool operator!=(const SparseDiff<T> &left,const T &right) {
    return (left.value() != right);
  }

  // Compare a constant and an SparseDiff
  template<class T> Bool operator>(const T &left, const SparseDiff<T> &right) {
    return (left > right.value());
  }

  template<class T> Bool operator<(const T &left, const SparseDiff<T> &right) {
    return (left < right.value());
  }

  template<class T> Bool operator>=(const T &left, const SparseDiff<T> &right) {
    return (left >= right.value());
  }

  template<class T> Bool operator<=(const T &left, const SparseDiff<T> &right) {
    return (left <= right.value());
  }

  template<class T> Bool operator==(const T &left, const SparseDiff<T> &right) {
    return (left == right.value());
  }

  template<class T> Bool operator!=(const T &left, const SparseDiff<T> &right) {
    return (left != right.value());
  }

  // Near comparisons

  template<class T>
  Bool near(const SparseDiff<T> &left, const SparseDiff<T> &right) {
    return (near(left.value(), right.value()));
  }

  template<class T>
  Bool near(const T &left, const SparseDiff<T> &right) {
    return near(left, right.value());
  }

  template<class T>
  Bool near(const SparseDiff<T> &left, const T &right) {
    return near(left.value(), right);
  }

  template<class T>
  Bool near(const SparseDiff<T> &left, const SparseDiff<T> &right,
	    const Double tol) {
    return near(left.value(), right.value(), tol);
  }

  template<class T>
  Bool near(const T &left, const SparseDiff<T> &right, const Double tol) {
    return near(left, right.value(), tol);
  }

  template<class T>
  Bool near(const SparseDiff<T> &left, const T &right, const Double tol) {
    return near(left.value(), right, tol);
  }

  template<class T>
  Bool allnear(const SparseDiff<T> &left, const SparseDiff<T> &right) {
    return (near(left.value(), right.value()));
  }

  template<class T>
  Bool allnear(const T &left, const SparseDiff<T> &right) {
    return near(left, right.value());
  }

  template<class T>
  Bool allnear(const SparseDiff<T> &left, const T &right) {
    return near(left.value(), right);
  }

  template<class T>
  Bool allnear(const SparseDiff<T> &left, const SparseDiff<T> &right,
	       const Double tol) {
    return near(left.value(), right.value(), tol);
  }

  template<class T>
  Bool allnear(const T &left, const SparseDiff<T> &right, const Double tol) {
    return near(left, right.value(), tol);
  }

  template<class T>
  Bool allnear(const SparseDiff<T> &left, const T &right, const Double tol) {
    return near(left.value(), right, tol);
  }

  template<class T>
  Bool nearAbs(const SparseDiff<T> &left, const SparseDiff<T> &right) {
    return (nearAbs(left.value(), right.value()));
  }

  template<class T>
  Bool nearAbs(const T &left, const SparseDiff<T> &right) {
    return nearAbs(left, right.value());
  }

  template<class T>
  Bool nearAbs(const SparseDiff<T> &left, const T &right) {
    return nearAbs(left.value(), right);
  }

  template<class T>
  Bool nearAbs(const SparseDiff<T> &left, const SparseDiff<T> &right,
	       const Double tol) {
    return nearAbs(left.value(), right.value(), tol);
  }

  template<class T>
  Bool nearAbs(const T &left, const SparseDiff<T> &right, const Double tol) {
    return nearAbs(left, right.value(), tol);
  }

  template<class T>
  Bool nearAbs(const SparseDiff<T> &left, const T &right, const Double tol) {
    return nearAbs(left.value(), right, tol);
  }

  template<class T>
  Bool allnearAbs(const SparseDiff<T> &left, const SparseDiff<T> &right) {
    return (nearAbs(left.value(), right.value()));
  }

  template<class T>
  Bool allnearAbs(const T &left, const SparseDiff<T> &right) {
    return nearAbs(left, right.value());
  }

  template<class T>
  Bool allnearAbs(const SparseDiff<T> &left, const T &right) {
    return nearAbs(left.value(), right);
  }

  template<class T>
  Bool allnearAbs(const SparseDiff<T> &left, const SparseDiff<T> &right,
		  const Double tol) {
    return nearAbs(left.value(), right.value(), tol);
  }

  template<class T>
  Bool allnearAbs(const T &left, const SparseDiff<T> &right, const Double tol) {
    return nearAbs(left, right.value(), tol);
  }

  template<class T>
  Bool allnearAbs(const SparseDiff<T> &left, const T &right, const Double tol) {
    return nearAbs(left.value(), right, tol);
  }

  // Test special values
  template<class T>
  Bool isNaN (const SparseDiff<T> &val) {
    return isNaN(val.value());
  }

  template<class T>
  Bool isInf(SparseDiff<T> &val) {
    return isInf(val.value());
  }

  template<class T>
  SparseDiff<T> min(const SparseDiff<T> &left,
		    const SparseDiff<T> &right) {
    SparseDiff<T> tmp = (left.value() <= right.value()) ?
      left : right;
    return tmp;
  }

  template<class T>
  SparseDiff<T> max(const SparseDiff<T> &left,
		    const SparseDiff<T> &right) {
    SparseDiff<T> tmp = (left.value() <= right.value()) ?
      right : left;
    return tmp;
  }

} //# NAMESPACE CASA - END

