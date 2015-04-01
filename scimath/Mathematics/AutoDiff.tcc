//# AutoDiff.cc: An automatic differentiating class for functions
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

#ifndef SCIMATH_AUTODIFF_TCC
#define SCIMATH_AUTODIFF_TCC

//# Includes
#include <casacore/scimath/Mathematics/AutoDiff.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/BasicMath/Math.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Statics
template <class T>
ObjectPool<AutoDiffRep<T>, uInt> AutoDiff<T>::theirPool;
template <class T>
Mutex AutoDiff<T>::theirMutex;


template <class T>
AutoDiff<T>::AutoDiff() : rep_p(0)
{
  ScopedMutexLock locker(theirMutex);
  rep_p = theirPool.get(0);
}

template <class T>
AutoDiff<T>::AutoDiff(const T &v) : rep_p(0)
{
  {
    ScopedMutexLock locker(theirMutex);
    rep_p = theirPool.get(0);
  }
  rep_p->val_p = v; 
}

template <class T>
AutoDiff<T>::AutoDiff(const T &v, const uInt ndiffs, const uInt n) : rep_p(0)
{
  {
    ScopedMutexLock locker(theirMutex);
    rep_p = theirPool.get(ndiffs);
  }
  rep_p->val_p = v;
  rep_p->grad_p = T(0);
  rep_p->grad_p[n] = T(1);
}

template <class T>
AutoDiff<T>::AutoDiff(const T &v, const uInt ndiffs) : rep_p(0)
{
  {
    ScopedMutexLock locker(theirMutex);
    rep_p = theirPool.get(ndiffs);
  }
  rep_p->val_p = v;
  rep_p->grad_p = T(0);
}

template <class T>
AutoDiff<T>::AutoDiff(const AutoDiff<T> &other) : rep_p(0)
{
  if (other.rep_p->nocopy_p) {
    rep_p = other.rep_p;
  } else {
    {
      ScopedMutexLock locker(theirMutex);
      rep_p = theirPool.get(other.rep_p->nd_p);
    }
    rep_p->val_p = other.rep_p->val_p;
    rep_p->grad_p = other.rep_p->grad_p;
  }
}

template <class T>
AutoDiff<T>::AutoDiff(const T &v, const Vector<T> &derivs) : rep_p(0)
{
  {
    ScopedMutexLock locker(theirMutex);
    rep_p = theirPool.get(derivs.nelements());
  }
  rep_p->val_p = v;
  rep_p->grad_p = derivs;
}

template<class T>
AutoDiff<T>::~AutoDiff() {
  release();
}

template <class T>
AutoDiff<T> &AutoDiff<T>::operator=(const T &v) {
  if (rep_p->nd_p != 0) {
    release();
    {
      ScopedMutexLock locker(theirMutex);
      rep_p = theirPool.get(0);
    }
  }
  rep_p->val_p = v;
  return *this;
}

template <class T>
AutoDiff<T> &AutoDiff<T>::operator=(const AutoDiff<T> &other) { 
  if (this != &other) {
    release();
    {
      ScopedMutexLock locker(theirMutex);
      rep_p = theirPool.get(other.rep_p->nd_p);
    }
    rep_p->val_p = other.rep_p->val_p;
    rep_p->grad_p = other.rep_p->grad_p;
  }
  return *this;
}

template <class T>
void AutoDiff<T>::operator*=(const AutoDiff<T> &other) {
  if (other.rep_p->nd_p != 0) {
    if (rep_p->nd_p == 0) {
      T v = rep_p->val_p;
      release();
      {
        ScopedMutexLock locker(theirMutex);
        rep_p = theirPool.get(other.rep_p->nd_p);
      }
      rep_p->grad_p = other.rep_p->grad_p;
      rep_p->grad_p *= v;
      rep_p->val_p = v; 
    } else {
      for (uInt i=0; i<rep_p->nd_p ; i++) {
	rep_p->grad_p[i] = rep_p->val_p*other.rep_p->grad_p[i] +
	  other.rep_p->val_p*rep_p->grad_p[i];
      }
    }
  } else {
    for (uInt i=0; i<rep_p->nd_p ; i++) rep_p->grad_p[i] *= other.rep_p->val_p;
  }
  rep_p->val_p *= other.rep_p->val_p;
}

template <class T>
void AutoDiff<T>::operator/=(const AutoDiff<T> &other) { 
  T temp = other.rep_p->val_p * other.rep_p->val_p;
  if (other.rep_p->nd_p != 0) {
    if (rep_p->nd_p == 0) {
      T v = rep_p->val_p;
      release();
      {
        ScopedMutexLock locker(theirMutex);
        rep_p = theirPool.get(other.rep_p->nd_p);
      }
      rep_p->grad_p = other.rep_p->grad_p;
      rep_p->grad_p *= (-v/temp);
      rep_p->val_p = other.rep_p->val_p;
    } else {
      for (uInt i=0; i<rep_p->nd_p ; i++) {
	rep_p->grad_p[i] = rep_p->grad_p[i]/other.rep_p->val_p -
	  rep_p->val_p*(other.rep_p->grad_p[i])/temp;
      }
    }
  } else {
    rep_p->grad_p /= other.rep_p->val_p;
  }
  rep_p->val_p /= other.rep_p->val_p;
}

template <class T>
void AutoDiff<T>::operator+=(const AutoDiff<T> &other) {
  if (other.rep_p->nd_p != 0) {
    if (rep_p->nd_p == 0) {
      T v = rep_p->val_p;
      release();
      {
        ScopedMutexLock locker(theirMutex);
        rep_p = theirPool.get(other.rep_p->nd_p);
      }
      rep_p->grad_p = other.rep_p->grad_p;
      rep_p->val_p = v;
    } else {
	rep_p->grad_p += other.rep_p->grad_p;
    }
  }
  rep_p->val_p += other.rep_p->val_p;
}

template <class T>
void AutoDiff<T>::operator-=(const AutoDiff<T> &other) {
  if (other.rep_p->nd_p != 0) {
    if (rep_p->nd_p == 0) {
      T v = rep_p->val_p;
      release();
      {
        ScopedMutexLock locker(theirMutex);
        rep_p = theirPool.get(other.rep_p->nd_p);
      }
      rep_p->grad_p = -other.rep_p->grad_p;
      rep_p->val_p = v;
    } else {
      rep_p->grad_p -= other.rep_p->grad_p;
    }
  }
  rep_p->val_p -= other.rep_p->val_p;
}

template <class T>
void AutoDiff<T>::operator*=(const T other) {
  rep_p->grad_p *= other;
  rep_p->val_p *= other;
}

template <class T>
void AutoDiff<T>::operator/=(const T other) { 
  rep_p->grad_p /= other;
  rep_p->val_p /= other;
}

template <class T>
void AutoDiff<T>::operator+=(const T other) {
  rep_p->val_p += other;
}

template <class T>
void AutoDiff<T>::operator-=(const T other) {
  rep_p->val_p -= other;
}

template <class T> Vector<T> AutoDiff<T>::derivatives() const { 
  return rep_p->grad_p;
}

template <class T> void AutoDiff<T>::derivatives(Vector<T> &res) const { 
  res.resize(rep_p->nd_p);
  res = rep_p->grad_p;
}

} //# NAMESPACE CASACORE - END


#endif
