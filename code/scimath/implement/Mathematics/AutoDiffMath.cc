//# AutoDiff.cc: an automatic differential class for  parameterized functions
//# Copyright (C) 1995,1996,1999,2001
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

#include <trial/Mathematics/AutoDiffMath.h>
#include <trial/Mathematics/AutoDiff.h>
#include <aips/Arrays/ArrayMath.h>

// global math functions
// Unary arithmetic operators.
template<class T> AutoDiff<T> operator+(const AutoDiff<T> &other) { 
  return other; 
}

template<class T> AutoDiff<T> operator-(const AutoDiff<T> &other) { 
  AutoDiff<T> tmp(other);
  tmp.value_ *= T(-1.0);
  for (uInt i = 0; i < tmp.nderivs; i++) (*tmp.gradient_)(i) *= T(-1.0);
  return tmp;
}

template<class T> 
AutoDiff<T> operator+(const AutoDiff<T> &left, const AutoDiff<T> &right) {
  if (left.nderivs == 0) return (left.value_ + right);
  AutoDiff<T> tmp(left);
  for (uInt i = 0; i < right.nderivs; i++) {
    (*tmp.gradient_)(i) += (*right.gradient_)(i);
  };
  tmp.value_ += right.value_;
  return tmp;
}

template<class T> 
AutoDiff<T> operator-(const AutoDiff<T> &left, const AutoDiff<T> &right) {
  if (left.nderivs == 0) return (left.value_ - right);
  AutoDiff<T> tmp(left);
  for (uInt i = 0; i < right.nderivs; i++) {
    (*tmp.gradient_)(i) -= (*right.gradient_)(i);
  };
  tmp.value_ -= right.value_;
  return tmp;
}

template<class T> 
AutoDiff<T> operator*(const AutoDiff<T> &left, const AutoDiff<T> &right) {
  if (left.nderivs == 0) return (left.value_ * right);
  AutoDiff<T> tmp(left);
  for (uInt i = 0; i < left.nderivs; i++) {
    (*tmp.gradient_)(i) *= right.value_;
  };
  for (uInt i = 0; i < right.nderivs; i++) {
    (*tmp.gradient_)(i) +=  tmp.value_*(*right.gradient_)(i);
  };
  tmp.value_ *= right.value_;
  return tmp;
}

template<class T> 
AutoDiff<T> operator/(const AutoDiff<T> &left, const AutoDiff<T> &right) {
  if (left.nderivs == 0) return (left.value_ / right);
  T temp = right.value_ * right.value_;
  AutoDiff<T> tmp(left);
  for (uInt i = 0; i < left.nderivs; i++) {
    (*tmp.gradient_)(i) /= right.value_;
  };
  for (uInt i = 0; i < right.nderivs; i++) {
    (*tmp.gradient_)(i) -=  left.value_ * (*right.gradient_)(i)/temp;
  };
  tmp.value_ /= right.value_;
  return tmp;
}

template<class T> 
AutoDiff<T> operator+(const AutoDiff<T> &left, const T &right) { 
  AutoDiff<T> tmp(left);
  tmp.value_ += right;
  return tmp;
}

template<class T> 
AutoDiff<T> operator-(const AutoDiff<T> &left, const T &right) { 
  AutoDiff<T> tmp(left);
  tmp.value_ -= right;
  return tmp;
}

template<class T> 
AutoDiff<T> operator* (const AutoDiff<T> &left, const T &right) { 
  AutoDiff<T> tmp(left);
  for (uInt i = 0; i < tmp.nderivs; i++) {
    (*tmp.gradient_)(i) *= right;
  };
  tmp.value_ *= right;
  return tmp;
}

template<class T> 
AutoDiff<T> operator/(const AutoDiff<T> &left, const T &right) {
  AutoDiff<T> tmp(left);
  for (uInt i = 0; i < tmp.nderivs; i++) {
    (*tmp.gradient_)(i) /= right;
  };
  tmp.value_ /= right;
  return tmp;
}

template<class T> 
AutoDiff<T> operator+(const T &left, const AutoDiff<T> &right) { 
  AutoDiff<T> tmp(right);
  tmp.value_ += left;
  return tmp;
}

template<class T> 
AutoDiff<T> operator-(const T &left, const AutoDiff<T> &right) { 
  AutoDiff<T> tmp(right);
  for (uInt i = 0; i < tmp.nderivs; i++) {
    (*tmp.gradient_)(i) *= T(-1.0);
  };
  tmp.value_ = left - right.value_;
  return tmp;
}

template<class T> 
AutoDiff<T> operator*(const T &left, const AutoDiff<T> &right) { 
  AutoDiff<T> tmp(right);
  for (uInt i = 0; i < tmp.nderivs; i++) {
    (*tmp.gradient_)(i) *= left;
  };
  tmp.value_ *= left;
  return tmp;
}

template<class T> 
AutoDiff<T> operator/(const T &left, const AutoDiff<T> &right) { 
  AutoDiff<T> tmp(right);
  tmp.value_ = left/right.value_;
  for (uInt i = 0; i < tmp.nderivs; i++) {
    (*tmp.gradient_)(i) *= -tmp.value_/right.value_;
  };
  return tmp;
}

template<class T> AutoDiff<T> acos(const AutoDiff<T>& ad) {
  AutoDiff<T> tmp(ad);
  T temp = sqrt(T(1) - tmp.value_*tmp.value_);
  for (uInt i = 0; i < tmp.nderivs; i++) {
    (*tmp.gradient_)(i) /= -temp;
  };
  tmp.value_ = acos(tmp.value_);
  return tmp;
}

template<class T> AutoDiff<T> asin(const AutoDiff<T>& ad) {
  AutoDiff<T> tmp(ad);
  T temp = sqrt(T(1) - tmp.value_*tmp.value_);
  for (uInt i = 0; i < tmp.nderivs; i++) {
    (*tmp.gradient_)(i) /= temp;
  };
  tmp.value_ = asin(tmp.value_);
  return tmp;
}

template<class T> AutoDiff<T> atan(const AutoDiff<T>& ad) {
  AutoDiff<T> tmp(ad);
  T temp = T(1) + tmp.value_*tmp.value_;
  for (uInt i = 0; i < tmp.nderivs; i++) {
    (*tmp.gradient_)(i) /= temp;
  };
  tmp.value_ = atan(tmp.value_);
  return tmp;
}

template<class T>
AutoDiff<T> atan2(const AutoDiff<T>& y, const AutoDiff<T>& x) {  
  // this gets the derivative right, via the chain rule using the already
  // defined / and atan functions, but the value may be wrong
  AutoDiff<T> temp = atan(y/x);
  // get the value right
  temp.value_ = atan2(y.value_, x.value_);
  return temp;
}

template<class T> AutoDiff<T> cos(const AutoDiff<T>& ad) { 
  AutoDiff<T> tmp(ad);
  for (uInt i = 0; i < tmp.nderivs; i++) {
    (*tmp.gradient_)(i) *= -sin(tmp.value_);
  };
  tmp.value_ = cos(tmp.value_);
  return tmp;
}

template<class T> AutoDiff<T> cosh(const AutoDiff<T>& ad) { 
  AutoDiff<T> tmp(ad);
  for (uInt i = 0; i < tmp.nderivs; i++) {
    (*tmp.gradient_)(i) *= sinh(tmp.value_);
  };
  tmp.value_ = cosh(tmp.value_);
  return tmp;
}

template<class T> AutoDiff<T> sin(const AutoDiff<T>& ad) { 
  AutoDiff<T> tmp(ad);
  for (uInt i = 0; i < tmp.nderivs; i++) {
    (*tmp.gradient_)(i) *= cos(tmp.value_);
  };
  tmp.value_ = sin(tmp.value_);
  return tmp;
}

template<class T> AutoDiff<T> sinh(const AutoDiff<T>& ad) { 
  AutoDiff<T> tmp(ad);
  for (uInt i = 0; i < tmp.nderivs; i++) {
    (*tmp.gradient_)(i) *= cosh(tmp.value_);
  };
  tmp.value_ = sinh(tmp.value_);
  return tmp;
}

template<class T> AutoDiff<T> exp(const AutoDiff<T>& ad) { 
  AutoDiff<T> tmp(ad);
  tmp.value_ = exp(ad.value_);
  for (uInt i = 0; i < tmp.nderivs; i++) {
    (*tmp.gradient_)(i) *= tmp.value_;
  };
  return tmp;
}

template<class T> AutoDiff<T> log(const AutoDiff<T>& ad) { 
  AutoDiff<T> tmp(ad);
  for (uInt i = 0; i < tmp.nderivs; i++) {
    (*tmp.gradient_)(i) /= tmp.value_;
  }
  tmp.value_ = log(tmp.value_);
  return tmp;
}

template<class T> AutoDiff<T> log10(const AutoDiff<T>& ad) { 
  AutoDiff<T> tmp(ad);
  for (uInt i = 0; i < tmp.nderivs; i++) {
    (*tmp.gradient_)(i) /= tmp.value_*T(log(10.0));
  };
  tmp.value_ = log10(tmp.value_);
  return tmp;
}

template<class T> 
AutoDiff<T> pow(const AutoDiff<T>& a, const AutoDiff<T>& b) {
  if (b.nderivs == 0) return pow(a, b.value_);
  T value = pow(a.value_, b.value_);
  T temp1 = value * log(a.value_);
  T temp2 = b.value_ * pow(a.value_, b.value_ - T(1));
  AutoDiff<T> tmp(b);
  for (uInt i = 0; i < tmp.nderivs; i++) {
    (*tmp.gradient_)(i) *= temp1;
  };
  for (uInt i=0; i<a.nderivs; i++) {
    (*tmp.gradient_)(i) += (*a.gradient_)(i)*temp2;
  };
  tmp.value_ = value;
  return tmp;
}

template<class T> AutoDiff<T> pow(const AutoDiff<T>& a, const T& b) {
  AutoDiff<T> tmp(a);
  T temp = b*pow(a.value_, b-T(1));
  for (uInt i = 0; i < tmp.nderivs; i++) {
    (*tmp.gradient_)(i) *= temp;
  };
  tmp.value_ = pow(a.value_, b);
  return tmp;
}

template<class T> AutoDiff<T> sqrt(const AutoDiff<T>& ad) { 
  AutoDiff<T> tmp(ad);
  tmp.value_ = sqrt(tmp.value_);
  for (uInt i = 0; i < tmp.nderivs; i++) {
    (*tmp.gradient_)(i) /= T(2)*tmp.value_;
  };
  return tmp;
}

template<class T> AutoDiff<T> tan(const AutoDiff<T>& ad) { 
  AutoDiff<T> tmp(ad);
  T temp = cos(tmp.value_);
  temp *= temp;
  for (uInt i = 0; i < tmp.nderivs; i++) {
    (*tmp.gradient_)(i) /= temp;
  };
  tmp.value_ = tan(tmp.value_);
  return tmp;
}

template<class T> AutoDiff<T> tanh(const AutoDiff<T>& ad) { 
  AutoDiff<T> tmp(ad);
  T temp = cosh(tmp.value_);
  temp *= temp;
  for (uInt i = 0; i < tmp.nderivs; i++) {
    (*tmp.gradient_)(i) /= temp;
  };
  tmp.value_ = tanh(tmp.value_);
  return tmp;
}

template<class T> AutoDiff<T> abs(const AutoDiff<T>& ad) { 
  // Here we assume that function F represented by ad is continous and 
  // differentiable in a small enough neighborhood where F is 
  // evaluated. So if ad.value_ is positive, F is positive in the small
  // neighborhood.
  AutoDiff<T> tmp(ad);
  if (ad.value_ < T(0)) {
    for (uInt i = 0; i < tmp.nderivs; i++) (*tmp.gradient_)(i) *= T(-1);
    tmp.value_ = -ad.value_;
  };
  return tmp;
}


template<class T> AutoDiff<T> fmod(const AutoDiff<T>& x, const T c) { 
  // Floating-point remainder of x/c, with the same sign as x, where c is
  // a constant.  Since fmod(x,c) = x - ((int)(x/c))*c and d[(int)(z)]/dz = 0, 
  // d(fmod(x,c))/dx = 1.  At z = integer, (int)(z) is discontinuous, but
  // away from the point (int)(z) is well defined and has derivative (=0)
  // we use the derivative at z = integer+epsilon as the derivative at
  // z = integer.
  AutoDiff<T> tmp(x);
  tmp.value_ = fmod(x.value_,c);
  return tmp;
}


template<class T>
Bool operator>(const AutoDiff<T>& right, const AutoDiff<T>& left) {
  return (right.value_ > left.value_);
}

template<class T>
Bool operator<(const AutoDiff<T>& right, const AutoDiff<T>& left) {
  return (right.value_ < left.value_);
}

template<class T>
Bool operator>=(const AutoDiff<T>& right, const AutoDiff<T>& left) {
  return (right.value_ >= left.value_);
}

template<class T>
Bool operator<=(const AutoDiff<T>& right, const AutoDiff<T>& left) {
  return (right.value_ <= left.value_);
}

template<class T>
Bool operator==(const AutoDiff<T>& right, const AutoDiff<T>& left) {
  return (right.value_ == left.value_);
}

template<class T>
Bool operator!=(const AutoDiff<T>& right, const AutoDiff<T>& left) {
  return (right.value_ != left.value_);
}

// Compare an AutoDiff and a constant
template<class T> Bool operator>(const AutoDiff<T>& right,const T& left) {
  return (right.value_ > left);
}

template<class T> Bool operator<(const AutoDiff<T>& right,const T& left) {
  return (right.value_ < left);
}

template<class T> Bool operator>=(const AutoDiff<T>& right,const T& left) {
  return (right.value_ >= left);
}

template<class T> Bool operator<=(const AutoDiff<T>& right,const T& left) {
  return (right.value_ <= left);
}

template<class T> Bool operator==(const AutoDiff<T>& right,const T& left) {
  return (right.value_ == left);
}

template<class T> Bool operator!=(const AutoDiff<T>& right,const T& left) {
  return (right.value_ != left);
}

// Compare a constant and an AutoDiff
template<class T> Bool operator>(const T& right, const AutoDiff<T>& left) {
  return (right > left.value_);
}

template<class T> Bool operator<(const T& right, const AutoDiff<T>& left) {
  return (right < left.value_);
}

template<class T> Bool operator>=(const T& right, const AutoDiff<T>& left) {
  return (right >= left.value_);
}

template<class T> Bool operator<=(const T& right, const AutoDiff<T>& left) {
  return (right <= left.value_);
}

template<class T> Bool operator==(const T& right, const AutoDiff<T>& left) {
  return (right == left.value_);
}

template<class T> Bool operator!=(const T& right, const AutoDiff<T>& left) {
  return (right != left.value_);
}

template<class T> Bool near(const T& right, const AutoDiff<T>& left) {
  return near(right,left.value_);
}

template<class T> Bool near(const AutoDiff<T>& right, const T& left) {
  return near(left,right.value_);
}

template<class T>
Bool near(const AutoDiff<T>& right, const AutoDiff<T>& left) {
  return (near(left.value_ , right.value_));
}
