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

#include <trial/Mathematics/AutoDiff.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Exceptions/Error.h>

#ifndef __GNUG__
template <class T> VectorPool<T> AutoDiff<T>::pool;
template<class T> Vector<T> AutoDiff<T>::null;
#endif 

template <class T>
AutoDiff<T>::AutoDiff():value_(T(0.0)),gradient_(pool.get()),nderivs(0)
{
  // nothing
}

template <class T>
AutoDiff<T>::AutoDiff(const T &v):value_(v),gradient_(pool.get()),nderivs(0)
{
  // nothing
}

template <class T>
AutoDiff<T>::AutoDiff(const T &v, const uInt ndiffs, const uInt i):value_(v),gradient_(pool.get()),nderivs(ndiffs)
{
  if(i >= ndiffs) {
    throw(AipsError("AutoDiff(const T& v, uInt ndiffs, uInt i): i >= ndiffs"));
  }
  if(ndiffs > gradient_->nelements())
    gradient_->resize(ndiffs);
  for(uInt j = 0; j < ndiffs; j++) {
    (*gradient_)(j) = T(0.0);
  }
  (*gradient_)(i) = 1.0;
}

template <class T>
AutoDiff<T>::AutoDiff(const AutoDiff<T> &other):value_(other.value_),gradient_(pool.get())
{
  nderivs = other.nderivs;
  if(nderivs > 0) {
    if(nderivs > gradient_->nelements())
      gradient_->resize(nderivs);
    for(uInt i = 0; i < nderivs; i++)
      (*gradient_)(i) = (*other.gradient_)(i);
  }
}

template <class T>
AutoDiff<T>::AutoDiff(const T& v, const Vector<T>& derivs):value_(v),gradient_(pool.get())
{
  nderivs = derivs.nelements();
  if(nderivs > gradient_->nelements())
    gradient_->resize(nderivs);
  for(uInt i = 0; i < nderivs; i++) {
    (*gradient_)(i) = derivs(i);
  }
}

template<class T>
AutoDiff<T>::~AutoDiff()
{
  pool.release(gradient_);
}

template <class T>
AutoDiff<T>& AutoDiff<T>::operator=(const T& v)
{
  value_ = v;
  nderivs = 0;
  return *this;
}

template <class T>
AutoDiff<T>& AutoDiff<T>::operator=(const AutoDiff<T> &other)
{ 
  if (this == &other) return *this;

  value_ = other.value_;
  nderivs = other.nderivs;
  
  if(nderivs > gradient_->nelements()) {
    gradient_->resize(nderivs);
  }
  for(uInt i = 0; i < nderivs; i++) {
    (*gradient_)(i) = (*other.gradient_)(i);
  }
  return *this;
}

template <class T>
AutoDiff<T>& AutoDiff<T>::operator*=(const AutoDiff<T> &other)
{ 
  uInt i;

  if(nderivs == 0) {
    gradient_->resize(other.nderivs);
    for(i = 0 ; i < other.nderivs; i++)
      (*gradient_)(i) = T(0.0);
  }    

  for(i = 0 ; i < nderivs; i++)
    (*gradient_)(i) = other.value_*(*gradient_)(i);
  for(i = 0; i < other.nderivs; i++)
    (*gradient_)(i) += value_ * (*other.gradient_)(i);

  nderivs = (nderivs > other.nderivs) ? nderivs : other.nderivs;

  value_ = value_ * other.value_;
  return *this;
}

template <class T>
AutoDiff<T>& AutoDiff<T>::operator/=(const AutoDiff<T> &other)
{ 
  uInt i;
  T temp = other.value_*other.value_;

  if(nderivs == 0) {
    gradient_->resize(other.nderivs);
    for(i = 0 ; i < other.nderivs; i++)
      (*gradient_)(i) = T(0.0);
  }    

  for(i = 0; i < nderivs; i++)
    (*gradient_)(i) = (*gradient_)(i)/other.value_;
  for(i = 0 ; i < other.nderivs; i++)
    (*gradient_)(i) -= value_*(*other.gradient_)(i)/temp;

  nderivs = (nderivs > other.nderivs) ? nderivs : other.nderivs;

  value_ = value_ / other.value_;
  return *this;
}

template <class T>
AutoDiff<T>& AutoDiff<T>::operator+=(const AutoDiff<T> &other)
{
  uInt i;

  if(nderivs == 0) {
    gradient_->resize(other.nderivs);
    for(i = 0 ; i < other.nderivs; i++)
      (*gradient_)(i) = T(0.0);
  }    

  for(i = 0 ; i < other.nderivs; i++) {
    (*gradient_)(i) += (*other.gradient_)(i);
  }

  nderivs = (nderivs > other.nderivs) ? nderivs : other.nderivs;

  value_ = value_ + other.value_;
  return *this;
}

template <class T>
AutoDiff<T>& AutoDiff<T>::operator-=(const AutoDiff<T> &other)
{
  uInt i;

  if(nderivs == 0) {
    gradient_->resize(other.nderivs);
    for(i = 0 ; i < other.nderivs; i++)
      (*gradient_)(i) = T(0.0);
  }    

  for(i = 0 ; i < other.nderivs; i++)
    (*gradient_)(i) -= (*other.gradient_)(i);

  nderivs = (nderivs > other.nderivs) ? nderivs : other.nderivs;

  value_ = value_ - other.value_;
  return *this;
}

template <class T> Vector<T>& AutoDiff<T>::derivatives()
{ 
  uInt i;
  if(nderivs == 0) {
    return null; 
  } else {
    if(gradient_->nelements() != nderivs) {
      Vector<T> tmp(nderivs);
      for(i = 0; i < nderivs; i++) 
	tmp(i) = (*gradient_)(i);
      gradient_->resize(nderivs);
      for(i = 0; i < nderivs; i++) 
	(*gradient_)(i) = tmp(i);
    }
    return *gradient_;
  }
}

template <class T> const Vector<T>& AutoDiff<T>::derivatives() const
{ 
  uInt i;
  if(nderivs == 0) {
    return null; 
  } else {
    if(gradient_->nelements() != nderivs) {
      Vector<T> tmp(nderivs);
      for(i = 0; i < nderivs; i++) 
	tmp(i) = (*gradient_)(i);
      gradient_->resize(nderivs);
      for(i = 0; i < nderivs; i++) 
	(*gradient_)(i) = tmp(i);
    }
    return *gradient_;
  }
}

template <class T> T& AutoDiff<T>::derivative(uInt which)
{ 
  if(which >= nderivs) {
    throw(AipsError("AutoDiff<T>::derivative(uInt which): derivative index out of bound"));
  }
  return (*gradient_)(which);
}

template <class T> uInt AutoDiff<T>::nDerivatives() const
{ 
  return nderivs;
}

template <class T> T& AutoDiff<T>::value()
{
  return value_;
}

template <class T> const T& AutoDiff<T>::value() const
{
  return value_;
}

template <class T> Bool AutoDiff<T>::isConstant() const
{ 
  if(nderivs == 0)
    return True;
  else
    return False;
}

template <class T> void AutoDiff<T>::resize(uInt ndivs)
{ 
  if (ndivs > gradient_->nelements()) {
    gradient_->resize(ndivs); 
  }
  nderivs = ndivs;
  for(uInt i = 0; i < nderivs; i++) 
    (*gradient_)(i) = T(0.0);
}
