//# AutoDiffMath.cc: Implements all mathematical functions for AutoDiff.
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

//# Includes
#include <trial/Mathematics/AutoDiffMath.h>
#include <aips/Arrays/ArrayMath.h>

// Unary arithmetic operators.
template<class T> AutoDiff<T> operator+(const AutoDiff<T> &other) { 
  AutoDiff<T> tmp(other);
  return tmp.ref();
}

template<class T> AutoDiff<T> operator-(const AutoDiff<T> &other) { 
  AutoDiff<T> tmp(other);
  tmp.theRep()->val_p *= T(-1);
  for (uInt i=0; i<tmp.theRep()->nd_p; i++) tmp.theRep()->grad_p[i] *= T(-1);
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
  tmp.theRep()->val_p += right;
  return tmp.ref();
}

template<class T> 
AutoDiff<T> operator-(const AutoDiff<T> &left, const T &right) { 
  AutoDiff<T> tmp(left);
  tmp.theRep()->val_p -= right;
  return tmp.ref();
}

template<class T> 
AutoDiff<T> operator* (const AutoDiff<T> &left, const T &right) { 
  AutoDiff<T> tmp(left);
  for (uInt i=0; i<tmp.theRep()->nd_p; i++) {
    tmp.theRep()->grad_p[i] *= right;
  };
  tmp.theRep()->val_p *= right;
  return tmp.ref();
}

template<class T> 
AutoDiff<T> operator/(const AutoDiff<T> &left, const T &right) {
  AutoDiff<T> tmp(left);
  for (uInt i=0; i<tmp.theRep()->nd_p; i++) {
    tmp.theRep()->grad_p[i] /= right;
  };
  tmp.theRep()->val_p /= right;
  return tmp.ref();
}

template<class T> 
AutoDiff<T> operator+(const T &left, const AutoDiff<T> &right) { 
  AutoDiff<T> tmp(right);
  tmp.theRep()->val_p += left;
  return tmp.ref();
}

template<class T> 
AutoDiff<T> operator-(const T &left, const AutoDiff<T> &right) { 
  AutoDiff<T> tmp(right);
  for (uInt i=0; i<tmp.theRep()->nd_p; i++) {
    tmp.theRep()->grad_p[i] *= T(-1);
  };
  tmp.theRep()->val_p = left - right.theRep()->val_p;
  return tmp.ref();
}

template<class T> 
AutoDiff<T> operator*(const T &left, const AutoDiff<T> &right) { 
  AutoDiff<T> tmp(right);
  for (uInt i=0; i<tmp.theRep()->nd_p; i++) {
    tmp.theRep()->grad_p[i] *= left;
  };
  tmp.theRep()->val_p *= left;
  return tmp.ref();
}

template<class T> 
AutoDiff<T> operator/(const T &left, const AutoDiff<T> &right) { 
  AutoDiff<T> tmp(right);
  tmp.theRep()->val_p = left/right.theRep()->val_p;
  for (uInt i=0; i<tmp.theRep()->nd_p; i++) {
    tmp.theRep()->grad_p[i] *= -tmp.theRep()->val_p/right.theRep()->val_p;
  };
  return tmp.ref();
}

template<class T> AutoDiff<T> acos(const AutoDiff<T>& ad) {
  AutoDiff<T> tmp(ad);
  T temp = sqrt(T(1) - tmp.theRep()->val_p*tmp.theRep()->val_p);
  for (uInt i=0; i<tmp.theRep()->nd_p; i++) {
    tmp.theRep()->grad_p[i] /= -temp;
  };
  tmp.theRep()->val_p = acos(tmp.theRep()->val_p);
  return tmp.ref();
}

template<class T> AutoDiff<T> asin(const AutoDiff<T>& ad) {
  AutoDiff<T> tmp(ad);
  T temp = sqrt(T(1) - tmp.theRep()->val_p*tmp.theRep()->val_p);
  for (uInt i=0; i<tmp.theRep()->nd_p; i++) {
    tmp.theRep()->grad_p[i] /= temp;
  };
  tmp.theRep()->val_p = asin(tmp.theRep()->val_p);
  return tmp.ref();
}

template<class T> AutoDiff<T> atan(const AutoDiff<T>& ad) {
  AutoDiff<T> tmp(ad);
  T temp = T(1) + tmp.theRep()->val_p*tmp.theRep()->val_p;
  for (uInt i=0; i<tmp.theRep()->nd_p; i++) {
    tmp.theRep()->grad_p[i] /= temp;
  };
  tmp.theRep()->val_p = atan(tmp.theRep()->val_p);
  return tmp.ref();
}

template<class T>
AutoDiff<T> atan2(const AutoDiff<T>& y, const AutoDiff<T>& x) {  
  // this gets the derivative right, via the chain rule using the already
  // defined / and atan functions, but the value may be wrong
  AutoDiff<T> tmp = atan(y/x);
  // get the value right
  tmp.theRep()->val_p = atan2(y.theRep()->val_p, x.theRep()->val_p);
  return tmp.ref();
}

template<class T> AutoDiff<T> cos(const AutoDiff<T>& ad) { 
  AutoDiff<T> tmp(ad);
  for (uInt i=0; i<tmp.theRep()->nd_p; i++) {
    tmp.theRep()->grad_p[i] *= -sin(tmp.theRep()->val_p);
  };
  tmp.theRep()->val_p = cos(tmp.theRep()->val_p);
  return tmp.ref();
}

template<class T> AutoDiff<T> cosh(const AutoDiff<T>& ad) { 
  AutoDiff<T> tmp(ad);
  for (uInt i=0; i<tmp.theRep()->nd_p; i++) {
    tmp.theRep()->grad_p[i] *= sinh(tmp.theRep()->val_p);
  };
  tmp.theRep()->val_p = cosh(tmp.theRep()->val_p);
  return tmp.ref();
}

template<class T> AutoDiff<T> sin(const AutoDiff<T>& ad) { 
  AutoDiff<T> tmp(ad);
  for (uInt i=0; i<tmp.theRep()->nd_p; i++) {
    tmp.theRep()->grad_p[i] *= cos(tmp.theRep()->val_p);
  };
  tmp.theRep()->val_p = sin(tmp.theRep()->val_p);
  return tmp.ref();
}

template<class T> AutoDiff<T> sinh(const AutoDiff<T>& ad) { 
  AutoDiff<T> tmp(ad);
  for (uInt i=0; i<tmp.theRep()->nd_p; i++) {
    tmp.theRep()->grad_p[i] *= cosh(tmp.theRep()->val_p);
  };
  tmp.theRep()->val_p = sinh(tmp.theRep()->val_p);
  return tmp.ref();
}

template<class T> AutoDiff<T> exp(const AutoDiff<T>& ad) { 
  AutoDiff<T> tmp(ad);
  tmp.theRep()->val_p = exp(ad.theRep()->val_p);
  for (uInt i=0; i<tmp.theRep()->nd_p; i++) {
    tmp.theRep()->grad_p[i] *= tmp.theRep()->val_p;
  };
  return tmp.ref();
}

template<class T> AutoDiff<T> log(const AutoDiff<T>& ad) { 
  AutoDiff<T> tmp(ad);
  for (uInt i=0; i<tmp.theRep()->nd_p; i++) {
    tmp.theRep()->grad_p[i] /= tmp.theRep()->val_p;
  }
  tmp.theRep()->val_p = log(tmp.theRep()->val_p);
  return tmp.ref();
}

template<class T> AutoDiff<T> log10(const AutoDiff<T>& ad) { 
  AutoDiff<T> tmp(ad);
  for (uInt i=0; i<tmp.theRep()->nd_p; i++) {
    tmp.theRep()->grad_p[i] /= tmp.theRep()->val_p*T(log(10.0));
  };
  tmp.theRep()->val_p = log10(tmp.theRep()->val_p);
  return tmp.ref();
}

template<class T> 
AutoDiff<T> pow(const AutoDiff<T>& a, const AutoDiff<T>& b) {
  if (b.theRep()->nd_p == 0) return pow(a, b.theRep()->val_p);
  T value = pow(a.theRep()->val_p, b.theRep()->val_p);
  T temp1 = value * log(a.theRep()->val_p);
  T temp2 = b.theRep()->val_p * pow(a.theRep()->val_p, b.theRep()->val_p - T(1));
  AutoDiff<T> tmp(b);
  for (uInt i=0; i<tmp.theRep()->nd_p; i++) {
    tmp.theRep()->grad_p[i] *= temp1;
  };
  for (uInt i=0; i<a.theRep()->nd_p; i++) {
    tmp.theRep()->grad_p[i] += a.theRep()->grad_p[i]*temp2;
  };
  tmp.theRep()->val_p = value;
  return tmp.ref();
}

template<class T> AutoDiff<T> pow(const AutoDiff<T>& a, const T& b) {
  AutoDiff<T> tmp(a);
  T temp = b*pow(a.theRep()->val_p, b-T(1));
  for (uInt i=0; i<tmp.theRep()->nd_p; i++) {
    tmp.theRep()->grad_p[i] *= temp;
  };
  tmp.theRep()->val_p = pow(a.theRep()->val_p, b);
  return tmp.ref();
}

template<class T> AutoDiff<T> sqrt(const AutoDiff<T>& ad) { 
  AutoDiff<T> tmp(ad);
  tmp.theRep()->val_p = sqrt(tmp.theRep()->val_p);
  for (uInt i=0; i<tmp.theRep()->nd_p; i++) {
    tmp.theRep()->grad_p[i] /= T(2)*tmp.theRep()->val_p;
  };
  return tmp.ref();
}

template<class T> AutoDiff<T> tan(const AutoDiff<T>& ad) { 
  AutoDiff<T> tmp(ad);
  T temp = cos(tmp.theRep()->val_p);
  temp *= temp;
  for (uInt i=0; i<tmp.theRep()->nd_p; i++) {
    tmp.theRep()->grad_p[i] /= temp;
  };
  tmp.theRep()->val_p = tan(tmp.theRep()->val_p);
  return tmp.ref();
}

template<class T> AutoDiff<T> tanh(const AutoDiff<T>& ad) { 
  AutoDiff<T> tmp(ad);
  T temp = cosh(tmp.theRep()->val_p);
  temp *= temp;
  for (uInt i=0; i<tmp.theRep()->nd_p; i++) {
    tmp.theRep()->grad_p[i] /= temp;
  };
  tmp.theRep()->val_p = tanh(tmp.theRep()->val_p);
  return tmp.ref();
}

template<class T> AutoDiff<T> abs(const AutoDiff<T>& ad) { 
  // Here we assume that function F represented by ad is continous and 
  // differentiable in a small enough neighborhood where F is 
  // evaluated. So if ad.theRep()->val_p is positive, F is positive in the small
  // neighborhood.
  AutoDiff<T> tmp(ad);
  if (ad.theRep()->val_p < T(0)) {
    for (uInt i=0; i<tmp.theRep()->nd_p; i++) tmp.theRep()->grad_p[i] *= T(-1);
    tmp.theRep()->val_p = -ad.theRep()->val_p;
  };
  return tmp.ref();
}


template<class T> AutoDiff<T> fmod(const AutoDiff<T>& x, const T c) { 
  // Floating-point remainder of x/c, with the same sign as x, where c is
  // a constant.  Since fmod(x,c) = x - ((int)(x/c))*c and d[(int)(z)]/dz = 0, 
  // d(fmod(x,c))/dx = 1.  At z = integer, (int)(z) is discontinuous, but
  // away from the point (int)(z) is well defined and has derivative (=0)
  // we use the derivative at z = integer+epsilon as the derivative at
  // z = integer.
  AutoDiff<T> tmp(x);
  tmp.theRep()->val_p = fmod(x.theRep()->val_p,c);
  return tmp.ref();
}


template<class T>
Bool operator>(const AutoDiff<T>& right, const AutoDiff<T>& left) {
  return (right.theRep()->val_p > left.theRep()->val_p);
}

template<class T>
Bool operator<(const AutoDiff<T>& right, const AutoDiff<T>& left) {
  return (right.theRep()->val_p < left.theRep()->val_p);
}

template<class T>
Bool operator>=(const AutoDiff<T>& right, const AutoDiff<T>& left) {
  return (right.theRep()->val_p >= left.theRep()->val_p);
}

template<class T>
Bool operator<=(const AutoDiff<T>& right, const AutoDiff<T>& left) {
  return (right.theRep()->val_p <= left.theRep()->val_p);
}

template<class T>
Bool operator==(const AutoDiff<T>& right, const AutoDiff<T>& left) {
  return (right.theRep()->val_p == left.theRep()->val_p);
}

template<class T>
Bool operator!=(const AutoDiff<T>& right, const AutoDiff<T>& left) {
  return (right.theRep()->val_p != left.theRep()->val_p);
}

// Compare an AutoDiff and a constant
template<class T> Bool operator>(const AutoDiff<T>& right,const T& left) {
  return (right.theRep()->val_p > left);
}

template<class T> Bool operator<(const AutoDiff<T>& right,const T& left) {
  return (right.theRep()->val_p < left);
}

template<class T> Bool operator>=(const AutoDiff<T>& right,const T& left) {
  return (right.theRep()->val_p >= left);
}

template<class T> Bool operator<=(const AutoDiff<T>& right,const T& left) {
  return (right.theRep()->val_p <= left);
}

template<class T> Bool operator==(const AutoDiff<T>& right,const T& left) {
  return (right.theRep()->val_p == left);
}

template<class T> Bool operator!=(const AutoDiff<T>& right,const T& left) {
  return (right.theRep()->val_p != left);
}

// Compare a constant and an AutoDiff
template<class T> Bool operator>(const T& right, const AutoDiff<T>& left) {
  return (right > left.theRep()->val_p);
}

template<class T> Bool operator<(const T& right, const AutoDiff<T>& left) {
  return (right < left.theRep()->val_p);
}

template<class T> Bool operator>=(const T& right, const AutoDiff<T>& left) {
  return (right >= left.theRep()->val_p);
}

template<class T> Bool operator<=(const T& right, const AutoDiff<T>& left) {
  return (right <= left.theRep()->val_p);
}

template<class T> Bool operator==(const T& right, const AutoDiff<T>& left) {
  return (right == left.theRep()->val_p);
}

template<class T> Bool operator!=(const T& right, const AutoDiff<T>& left) {
  return (right != left.theRep()->val_p);
}

template<class T> Bool near(const T& right, const AutoDiff<T>& left) {
  return near(right, left.theRep()->val_p);
}

template<class T> Bool near(const AutoDiff<T>& right, const T& left) {
  return near(left, right.theRep()->val_p);
}

template<class T>
Bool near(const AutoDiff<T>& right, const AutoDiff<T>& left) {
  return (near(left.theRep()->val_p, right.theRep()->val_p));
}
