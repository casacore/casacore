//# AutoDiff.cc: an automatic differential class for  parameterized functions
//# Copyright (C) 1995,1996,1999
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
template<class T> AutoDiff<T> operator+(const AutoDiff<T> &other)
{ 
  return other; 
}

template<class T> AutoDiff<T> operator-(const AutoDiff<T> &other)
{ 
  AutoDiff<T> tmp;
  tmp.nderivs = other.nderivs;
  if(tmp.nderivs > tmp.gradient_->nelements())
    tmp.gradient_->resize(tmp.nderivs);
  tmp.value_ = -other.value_;
  for(uInt i = 0; i < tmp.nderivs; i++) {
    (*tmp.gradient_)(i) = - (*other.gradient_)(i);
  }
  return tmp;
}

template<class T> 
AutoDiff<T> operator+(const AutoDiff<T> &left, const AutoDiff<T> &right)
{
  AutoDiff<T> tmp;
  uInt i;
  tmp.nderivs = (left.nderivs > right.nderivs) ? left.nderivs : right.nderivs;
  if(tmp.nderivs > tmp.gradient_->nelements())
    tmp.gradient_->resize(tmp.nderivs);
  for(i = 0; i < tmp.nderivs; i++) 
    (*tmp.gradient_)(i) = T(0.0);
  for(i = 0; i < left.nderivs; i++) 
    (*tmp.gradient_)(i) = (*left.gradient_)(i);
  for(i = 0; i < right.nderivs; i++) 
    (*tmp.gradient_)(i) += (*right.gradient_)(i);
  tmp.value_ = left.value_ + right.value_;
  return tmp;
}

template<class T> 
AutoDiff<T> operator-(const AutoDiff<T> &left, const AutoDiff<T> &right)
{
  AutoDiff<T> tmp;
  uInt i;
  tmp.nderivs = (left.nderivs > right.nderivs) ? left.nderivs : right.nderivs;
  if(tmp.nderivs > tmp.gradient_->nelements())
    tmp.gradient_->resize(tmp.nderivs);
  for(i = 0; i < tmp.nderivs; i++) 
    (*tmp.gradient_)(i) = T(0.0);
  for(i = 0; i < left.nderivs; i++) 
    (*tmp.gradient_)(i) = (*left.gradient_)(i);
  for(i = 0; i < right.nderivs; i++) 
    (*tmp.gradient_)(i) -= (*right.gradient_)(i);
  tmp.value_ = left.value_ - right.value_;
  return tmp;
}

template<class T> 
AutoDiff<T> operator* (const AutoDiff<T> &left,const AutoDiff<T> &right)
{
  AutoDiff<T> tmp;
  uInt i;
  tmp.nderivs = (left.nderivs > right.nderivs) ? left.nderivs : right.nderivs;
  if(tmp.nderivs > tmp.gradient_->nelements())
    tmp.gradient_->resize(tmp.nderivs);
  for(i = 0; i < tmp.nderivs; i++) 
    (*tmp.gradient_)(i) = T(0.0);
  for(i = 0; i < left.nderivs; i++) 
    (*tmp.gradient_)(i) = right.value_*(*left.gradient_)(i);
  for(i = 0; i < right.nderivs; i++) 
    (*tmp.gradient_)(i) += left.value_*(*right.gradient_)(i);
  tmp.value_ = left.value_*right.value_;
  return tmp;
}

template<class T> 
AutoDiff<T> operator/(const AutoDiff<T> &left, const AutoDiff<T> &right)
{
  AutoDiff<T> tmp;
  uInt i;
  T temp = right.value_*right.value_;
  tmp.nderivs = (left.nderivs > right.nderivs) ? left.nderivs : right.nderivs;
  if(tmp.nderivs > tmp.gradient_->nelements())
    tmp.gradient_->resize(tmp.nderivs);
  for(i = 0; i < tmp.nderivs; i++) 
    (*tmp.gradient_)(i) = T(0.0);
  for(i = 0; i < left.nderivs; i++) 
    (*tmp.gradient_)(i) = right.value_*(*left.gradient_)(i)/temp;
  for(i = 0; i < right.nderivs; i++) 
    (*tmp.gradient_)(i) -= left.value_*(*right.gradient_)(i)/temp;
  tmp.value_ = left.value_/right.value_;
  return tmp;
}

template<class T> 
AutoDiff<T> operator+(const AutoDiff<T> &left, const T &right)
{ 
  AutoDiff<T> tmp;
  tmp.nderivs = left.nderivs;
  if(tmp.nderivs > tmp.gradient_->nelements())
    tmp.gradient_->resize(tmp.nderivs);  
  for(uInt i = 0; i < tmp.nderivs; i++) {
    (*tmp.gradient_)(i) =(*left.gradient_)(i);
  }
  tmp.value_ = left.value_ + right;
  return tmp;
}

template<class T> 
AutoDiff<T> operator-(const AutoDiff<T> &left, const T &right)
{ 
  AutoDiff<T> tmp;
  tmp.nderivs = left.nderivs;
  if(tmp.nderivs > tmp.gradient_->nelements())
    tmp.gradient_->resize(tmp.nderivs);  
  for(uInt i = 0; i < tmp.nderivs; i++) {
    (*tmp.gradient_)(i) =(*left.gradient_)(i);
  }
  tmp.value_ = left.value_ - right;
  return tmp;
}

template<class T> 
AutoDiff<T> operator* (const AutoDiff<T> &left, const T &right)
{ 
  AutoDiff<T> tmp;
  tmp.nderivs = left.nderivs;
  if(tmp.nderivs > tmp.gradient_->nelements())
    tmp.gradient_->resize(tmp.nderivs);  
  for(uInt i = 0; i < tmp.nderivs; i++) {
    (*tmp.gradient_)(i) = (*left.gradient_)(i)*right;
  }
  tmp.value_ = left.value_*right;
  return tmp;
}

template<class T> 
AutoDiff<T> operator/(const AutoDiff<T> &left, const T &right)
{
  AutoDiff<T> tmp;
  tmp.nderivs = left.nderivs;
  if(tmp.nderivs > tmp.gradient_->nelements())
    tmp.gradient_->resize(tmp.nderivs);  
  for(uInt i = 0; i < tmp.nderivs; i++) {
    (*tmp.gradient_)(i) = (*left.gradient_)(i)/right;
  }
  tmp.value_ = left.value_/right;
  return tmp; 
}

template<class T> 
AutoDiff<T> operator+(const T &left, const AutoDiff<T> &right)
{ 
  AutoDiff<T> tmp;
  tmp.nderivs = right.nderivs;
  if(tmp.nderivs > tmp.gradient_->nelements())
    tmp.gradient_->resize(tmp.nderivs);  
  for(uInt i = 0; i < tmp.nderivs; i++) {
    (*tmp.gradient_)(i) = (*right.gradient_)(i);
  }
  tmp.value_ = left + right.value_;
  return tmp;
}

template<class T> 
AutoDiff<T> operator-(const T &left, const AutoDiff<T> &right)
{ 
  AutoDiff<T> tmp;
  tmp.nderivs = right.nderivs;
  if(tmp.nderivs > tmp.gradient_->nelements())
    tmp.gradient_->resize(tmp.nderivs);  
  for(uInt i = 0; i < tmp.nderivs; i++) {
    (*tmp.gradient_)(i) = -(*right.gradient_)(i);
  }
  tmp.value_ = left - right.value_;
  return tmp;
}

template<class T> 
AutoDiff<T> operator*(const T &left, const AutoDiff<T> &right)
{ 
  AutoDiff<T> tmp;
  tmp.nderivs = right.nderivs;
  if(tmp.nderivs > tmp.gradient_->nelements())
    tmp.gradient_->resize(tmp.nderivs);  
  for(uInt i = 0; i < tmp.nderivs; i++) {
    (*tmp.gradient_)(i) = left*(*right.gradient_)(i);
  }
  tmp.value_ = left*right.value_;
  return tmp;
}

template<class T> 
AutoDiff<T> operator/(const T &left, const AutoDiff<T> &right)
{ 
  AutoDiff<T> tmp;
  tmp.nderivs = right.nderivs;
  if(tmp.nderivs > tmp.gradient_->nelements())
    tmp.gradient_->resize(tmp.nderivs);  
  for(uInt i = 0; i < tmp.nderivs; i++) {
    (*tmp.gradient_)(i) = -left*(*right.gradient_)(i)/
      (right.value_*right.value_);
  }
  tmp.value_ = left/right.value_;
  return tmp;
}

template<class T> AutoDiff<T> acos(const AutoDiff<T>& ad)
{
  T temp = ad.value_;
  temp = sqrt(T(1) - temp*temp);
  AutoDiff<T> tmp;
  tmp.nderivs = ad.nderivs;
  if(tmp.nderivs > tmp.gradient_->nelements())
    tmp.gradient_->resize(tmp.nderivs);  
  for(uInt i = 0; i < tmp.nderivs; i++) {
    (*tmp.gradient_)(i) = -(*ad.gradient_)(i)/temp;
  }
  tmp.value_ = acos(ad.value_);
  return tmp;
}

template<class T> AutoDiff<T> asin(const AutoDiff<T>& ad)
{
  T temp = ad.value_;
  temp = sqrt(T(1) - temp*temp);
  AutoDiff<T> tmp;
  tmp.nderivs = ad.nderivs;
  if(tmp.nderivs > tmp.gradient_->nelements())
    tmp.gradient_->resize(tmp.nderivs);  
  for(uInt i = 0; i < tmp.nderivs; i++) {
    (*tmp.gradient_)(i) = (*ad.gradient_)(i)/temp;
  }
  tmp.value_ = asin(ad.value_);
  return tmp;
}

template<class T> AutoDiff<T> atan(const AutoDiff<T>& ad)
{
  T temp = ad.value_;
  temp = T(1) + temp*temp;
  AutoDiff<T> tmp;
  tmp.nderivs = ad.nderivs;
  if(tmp.nderivs > tmp.gradient_->nelements())
    tmp.gradient_->resize(tmp.nderivs);  
  for(uInt i = 0; i < tmp.nderivs; i++) {
    (*tmp.gradient_)(i) = (*ad.gradient_)(i)/temp;
  }
  tmp.value_ = atan(ad.value_);
  return tmp;
}

template<class T> AutoDiff<T> atan2(const AutoDiff<T>& y, 
				      const AutoDiff<T>& x)
{  
  // this gets the derivative right, via the chain rule using the already
  // defined / and atan functions, but the value may be wrong
  AutoDiff<T> temp = atan(y/x);
  // get the value right
  temp.value() = atan2(y.value(),x.value());
  return temp;
}

template<class T> AutoDiff<T> cos(const AutoDiff<T>& ad)
{ 
  AutoDiff<T> tmp;
  tmp.nderivs = ad.nderivs;
  if(tmp.nderivs > tmp.gradient_->nelements())
    tmp.gradient_->resize(tmp.nderivs);  
  for(uInt i = 0; i < tmp.nderivs; i++) {
    (*tmp.gradient_)(i) = -(*ad.gradient_)(i)*sin(ad.value_);
  }
  tmp.value_ = cos(ad.value_);
  return tmp;
}

template<class T> AutoDiff<T> cosh(const AutoDiff<T>& ad)
{ 
  AutoDiff<T> tmp;
  tmp.nderivs = ad.nderivs;
  if(tmp.nderivs > tmp.gradient_->nelements())
    tmp.gradient_->resize(tmp.nderivs);  
  for(uInt i = 0; i < tmp.nderivs; i++) {
    (*tmp.gradient_)(i) = (*ad.gradient_)(i)*sinh(ad.value_);
  }
  tmp.value_ = cosh(ad.value_);
  return tmp;
}

template<class T> AutoDiff<T> sin(const AutoDiff<T>& ad)
{ 
  AutoDiff<T> tmp;
  tmp.nderivs = ad.nderivs;
  if(tmp.nderivs > tmp.gradient_->nelements())
    tmp.gradient_->resize(tmp.nderivs);  
  for(uInt i = 0; i < tmp.nderivs; i++) {
    (*tmp.gradient_)(i) = (*ad.gradient_)(i)*cos(ad.value_);
  }
  tmp.value_ = sin(ad.value_);
  return tmp;
}

template<class T> AutoDiff<T> sinh(const AutoDiff<T>& ad)
{ 
  AutoDiff<T> tmp;
  tmp.nderivs = ad.nderivs;
  if(tmp.nderivs > tmp.gradient_->nelements())
    tmp.gradient_->resize(tmp.nderivs);  
  for(uInt i = 0; i < tmp.nderivs; i++) {
    (*tmp.gradient_)(i) = (*ad.gradient_)(i)*cosh(ad.value_);
  }
  tmp.value_ = sinh(ad.value_);
  return tmp;
}

template<class T> AutoDiff<T> exp(const AutoDiff<T>& ad)
{ 
  AutoDiff<T> tmp;
  tmp.nderivs = ad.nderivs;
  if(tmp.nderivs > tmp.gradient_->nelements())
    tmp.gradient_->resize(tmp.nderivs);  
  for(uInt i = 0; i < tmp.nderivs; i++) {
    (*tmp.gradient_)(i) = (*ad.gradient_)(i)*exp(ad.value_);
  }
  tmp.value_ = exp(ad.value_);
  return tmp;
}

template<class T> AutoDiff<T> log(const AutoDiff<T>& ad)
{ 
  AutoDiff<T> tmp;
  tmp.nderivs = ad.nderivs;
  if(tmp.nderivs > tmp.gradient_->nelements())
    tmp.gradient_->resize(tmp.nderivs);  
  for(uInt i = 0; i < tmp.nderivs; i++) {
    (*tmp.gradient_)(i) = (*ad.gradient_)(i)/ad.value_;
  }
  tmp.value_ = log(ad.value_);
  return tmp;
}

template<class T> AutoDiff<T> log10(const AutoDiff<T>& ad)
{ 
  AutoDiff<T> tmp;
  tmp.nderivs = ad.nderivs;
  if(tmp.nderivs > tmp.gradient_->nelements())
    tmp.gradient_->resize(tmp.nderivs);  
  for(uInt i = 0; i < tmp.nderivs; i++) {
    (*tmp.gradient_)(i) = (*ad.gradient_)(i)/(ad.value_*T(log(10.0)));
  }
  tmp.value_ = log10(ad.value_);
  return tmp;
}

template<class T> AutoDiff<T> pow(const AutoDiff<T>& a,
				  const AutoDiff<T>& b)
{
  T temp1, temp2, value;
  value = pow(a.value_, b.value_);
  temp1 = value * log(a.value_);
  temp2 = b.value_ * pow(a.value_, b.value_ - T(1));
  AutoDiff<T> tmp;
  tmp.nderivs = a.nderivs;
  if(tmp.nderivs > tmp.gradient_->nelements())
    tmp.gradient_->resize(tmp.nderivs);  
  for(uInt i = 0; i < tmp.nderivs; i++) {
    (*tmp.gradient_)(i) = (*b.gradient_)(i)*temp1+(*a.gradient_)(i)*temp2;
  }
  tmp.value_ = value;
  return tmp;
}

template<class T> AutoDiff<T> pow(const AutoDiff<T>& a, const T& b)
{
  AutoDiff<T> tmp;
  tmp.nderivs = a.nderivs;
  if(tmp.nderivs > tmp.gradient_->nelements())
    tmp.gradient_->resize(tmp.nderivs);  
  for(uInt i = 0; i < tmp.nderivs; i++) {
    (*tmp.gradient_)(i) = b*pow(a.value_,b-T(1))*(*a.gradient_)(i);
  }
  tmp.value_ = pow(a.value_,b);
  return tmp;
}

template<class T> AutoDiff<T> sqrt(const AutoDiff<T>& ad)
{ 
  T value = sqrt(ad.value_);
  AutoDiff<T> tmp;
  tmp.nderivs = ad.nderivs;
  if(tmp.nderivs > tmp.gradient_->nelements())
    tmp.gradient_->resize(tmp.nderivs);  
  for(uInt i = 0; i < tmp.nderivs; i++) {
    (*tmp.gradient_)(i) = (*ad.gradient_)(i)/(T(2)*value);
  }
  tmp.value_ = value;
  return tmp;
}

template<class T> AutoDiff<T> tan(const AutoDiff<T>& ad)
{ 
  T temp = cos(ad.value_);
  AutoDiff<T> tmp;
  tmp.nderivs = ad.nderivs;
  if(tmp.nderivs > tmp.gradient_->nelements())
    tmp.gradient_->resize(tmp.nderivs);  
  for(uInt i = 0; i < tmp.nderivs; i++) {
    (*tmp.gradient_)(i) = (*ad.gradient_)(i)/(temp*temp);
  }
  tmp.value_ = tan(ad.value_);
  return tmp;
}

template<class T> AutoDiff<T> tanh(const AutoDiff<T>& ad)
{ 
  T temp = cosh(ad.value_);
  AutoDiff<T> tmp;
  tmp.nderivs = ad.nderivs;
  if(tmp.nderivs > tmp.gradient_->nelements())
    tmp.gradient_->resize(tmp.nderivs);  
  for(uInt i = 0; i < tmp.nderivs; i++) {
    (*tmp.gradient_)(i) = (*ad.gradient_)(i)/(temp*temp);
  }
  tmp.value_ = tanh(ad.value_);
  return tmp;
}

template<class T> AutoDiff<T> abs(const AutoDiff<T>& ad)
{ 
  // Here we assume that function F represented by ad is continous and 
  // differentiable in a small enough neighborhood where F is 
  // evaluated. So if ad.value_ is positive, F is positive in the small
  // neighborhood.
  uInt i;
  AutoDiff<T> tmp;
  tmp.nderivs = ad.nderivs;
  if(tmp.nderivs > tmp.gradient_->nelements())
    tmp.gradient_->resize(tmp.nderivs);  
  if(ad.value_ >= T(0)) {
    for(i = 0; i < tmp.nderivs; i++) {
      (*tmp.gradient_)(i) = (*ad.gradient_)(i);
    }
    tmp.value_ = ad.value_;
  } else {
    for(i = 0; i < tmp.nderivs; i++) {
      (*tmp.gradient_)(i) = -(*ad.gradient_)(i);
    }
    tmp.value_ = -ad.value_;
  }

  return tmp;
}


template<class T> AutoDiff<T> fmod(const AutoDiff<T>& x,const T c)
{ 
  // Floating-point remainder of x/c, with the same sign as x, where c is
  // a constant.  Since fmod(x,c) = x - ((int)(x/c))*c and d[(int)(z)]/dz = 0, 
  // d(fmod(x,c))/dx = 1.  At z = integer, (int)(z) is discontinuous, but
  // away from the point (int)(z) is well defined and has derivative (=0)
  // we use the derivative at z = integer+epsilon as the derivative at
  // z = integer.

  uInt i;
  AutoDiff<T> tmp;
  tmp.nderivs = x.nderivs;
  if(tmp.nderivs > tmp.gradient_->nelements())
    tmp.gradient_->resize(tmp.nderivs);  
  for(i = 0; i < tmp.nderivs; i++) {
    (*tmp.gradient_)(i) = (*x.gradient_)(i);
    }
  tmp.value_ = fmod(x.value_,c);

  return tmp;
}


template<class T> Bool operator>(const AutoDiff<T>& right,const AutoDiff<T>& left)
{
  return (right.value_ > left.value_? True : False);
}

template<class T> Bool operator<(const AutoDiff<T>& right,const AutoDiff<T>& left)
{
  return (right.value_ < left.value_? True : False);
}

template<class T> Bool operator>=(const AutoDiff<T>& right,const AutoDiff<T>& left)
{
  return (right.value_ >= left.value_? True : False);
}

template<class T> Bool operator<=(const AutoDiff<T>& right,const AutoDiff<T>& left)
{
  return (right.value_ <= left.value_? True : False);
}

template<class T> Bool operator==(const AutoDiff<T>& right,const AutoDiff<T>& left)
{
  if(right.value_ != left.value_) return False;
  if(right.nderivs != left.nderivs) return False;
  for(uInt i = 0; i < right.nderivs; i++) 
    if((*right.gradient_)(i) != (*left.gradient_)(i)) return False;
  return True;
}

template<class T> Bool operator!=(const AutoDiff<T>& right,const AutoDiff<T>& left)
{
  return (!(left == right) ? True : False);
}

// Compare an AutoDiff and a constant
template<class T> Bool operator>(const AutoDiff<T>& right,const T& left)
{
  return (right.value_ > left ? True : False);
}

template<class T> Bool operator<(const AutoDiff<T>& right,const T& left)
{
  return (right.value_ < left ? True : False);
}

template<class T> Bool operator>=(const AutoDiff<T>& right,const T& left)
{
  return (right.value_ >= left ? True : False);
}

template<class T> Bool operator<=(const AutoDiff<T>& right,const T& left)
{
  return (right.value_ <= left ? True : False);
}

template<class T> Bool operator==(const AutoDiff<T>& right,const T& left)
{
  if(right.value_ != left) return False;
  if(right.nderivs != 0) return False;
  return True;
}

template<class T> Bool operator!=(const AutoDiff<T>& right,const T& left)
{
  return (!(right == left) ? True : False);
}

// Compare a constant and an AutoDiff
template<class T> Bool operator>(const T& right,const AutoDiff<T>& left)
{
  return (right > left.value_ ? True : False);
}

template<class T> Bool operator<(const T& right,const AutoDiff<T>& left)
{
  return (right < left.value_ ? True : False);
}

template<class T> Bool operator>=(const T& right,const AutoDiff<T>& left)
{
  return (right >= left.value_ ? True : False);
}

template<class T> Bool operator<=(const T& right,const AutoDiff<T>& left)
{
  return (right <= left.value_ ? True : False);
}

template<class T> Bool operator==(const T& right,const AutoDiff<T>& left)
{
  if(right != left.value_) return False;
  if(left.nderivs != 0) return False;
  return True;
}

template<class T> Bool operator!=(const T& right,const AutoDiff<T>& left)
{
  return (!(right == left) ? True : False);
}

template<class T> Bool near(const T& right,const AutoDiff<T>& left)
{
  if(left.nderivs != 0) return False;
  return near(right,left.value_);
}

template<class T> Bool near(const AutoDiff<T>& right,const T& left)
{
  if(right.nderivs != 0) return False;
  return near(left,right.value_);
}
template<class T> Bool near(const AutoDiff<T>& right,const AutoDiff<T>& left)
{
  if(left.nderivs != right.nderivs) return False;
  if(!near(left.value_ , right.value_)) return False;
  for(Int i = 0; i < left.nderivs; i++) {
    if(!near((*right.gradient_)(i),(*left.gradient_)(i))) return False;
  }
  return True;
}
