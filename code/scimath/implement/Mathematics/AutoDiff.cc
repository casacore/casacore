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

//# Includes
#include <trial/Mathematics/AutoDiff.h>
#include <aips/Arrays/Vector.h>
#include <aips/Mathematics/Math.h>

//# Statics
template <class T>
ObjectPool<AutoDiffRep<T>, uInt> AutoDiff<T>::pool;

template <class T>
AutoDiff<T>::AutoDiff() : rep_p(pool.get(0)) {}

template <class T>
AutoDiff<T>::AutoDiff(const T &v) : rep_p(pool.get(0)) {
  rep_p->val_p = v; 
}

template <class T>
AutoDiff<T>::AutoDiff(const T &v, const uInt ndiffs, const uInt n) :
  rep_p(pool.get(ndiffs)) {
  rep_p->val_p = v;
  for (uInt i=0; i<ndiffs; i++) rep_p->grad_p[i] = (i==n ? T(1) : T(0));
}

template <class T>
AutoDiff<T>::AutoDiff(const T &v, const uInt ndiffs) :
  rep_p(pool.get(ndiffs)) {
  rep_p->val_p = v;
  for (uInt i=0; i<rep_p->nd_p; i++) rep_p->grad_p[i] = T(0);
}

template <class T>
AutoDiff<T>::AutoDiff(const AutoDiff<T> &other) :
  rep_p(0) {
  if (other.rep_p->nocopy_p) rep_p = other.rep_p;
  else {
    rep_p = pool.get(other.rep_p->nd_p);
    rep_p->val_p = other.rep_p->val_p;
    for (uInt i=0; i<rep_p->nd_p; i++) rep_p->grad_p[i] =
					 other.rep_p->grad_p[i];
  };
}

template <class T>
AutoDiff<T>::AutoDiff(const T &v, const Vector<T> &derivs) :
  rep_p(pool.get(derivs.nelements())) {
  rep_p->val_p = v;
  for (uInt i=0; i<rep_p->nd_p; i++) rep_p->grad_p[i] = derivs(i);
}

template<class T>
AutoDiff<T>::~AutoDiff() {
  release();
}

template <class T>
AutoDiff<T> &AutoDiff<T>::operator=(const T &v) {
  if (rep_p->nd_p != 0) {
    release();
    rep_p = pool.get(0);
  };
  rep_p->val_p = v;
  return *this;
}

template <class T>
AutoDiff<T> &AutoDiff<T>::operator=(const AutoDiff<T> &other) { 
  if (this != &other) {
    release();
    rep_p = pool.get(other.rep_p->nd_p);
    rep_p->val_p = other.rep_p->val_p;
    for (uInt i=0; i<rep_p->nd_p; i++) rep_p->grad_p[i] =
					 other.rep_p->grad_p[i];
  };
  return *this;
}

template <class T>
void AutoDiff<T>::operator*=(const AutoDiff<T> &other) {
  if (other.rep_p->nd_p != 0) {
    if (rep_p->nd_p == 0) {
      T v = rep_p->val_p;
      release();
      rep_p = pool.get(other.rep_p->nd_p);
      for (uInt i=0; i<rep_p->nd_p; i++) {
	rep_p->grad_p[i] = other.rep_p->grad_p[i]*v;
      };
      rep_p->val_p = v; 
    } else {
      for (uInt i=0; i<rep_p->nd_p ; i++) {
	rep_p->grad_p[i] = rep_p->val_p*other.rep_p->grad_p[i] +
	  other.rep_p->val_p*rep_p->grad_p[i];
      };
    };
  } else {
    for (uInt i=0; i<rep_p->nd_p ; i++) rep_p->grad_p[i] *= other.rep_p->val_p;
  };
  rep_p->val_p *= other.rep_p->val_p;
}

template <class T>
void AutoDiff<T>::operator/=(const AutoDiff<T> &other) { 
  T temp = other.rep_p->val_p * other.rep_p->val_p;
  if (other.rep_p->nd_p != 0) {
    if (rep_p->nd_p == 0) {
      T v = rep_p->val_p;
      release();
      rep_p = pool.get(other.rep_p->nd_p);
      for (uInt i=0; i<rep_p->nd_p; i++) {
	rep_p->grad_p[i] = other.rep_p->grad_p[i]*(-v/temp);
      };
      rep_p->val_p = other.rep_p->val_p;
    } else {
      for (uInt i=0; i<rep_p->nd_p ; i++) {
	rep_p->grad_p[i] = rep_p->grad_p[i]/other.rep_p->val_p -
	  rep_p->val_p*(other.rep_p->grad_p[i])/temp;
      };
    };
  } else {
    for (uInt i=0; i<rep_p->nd_p ; i++) rep_p->grad_p[i] /= other.rep_p->val_p;
  };
  rep_p->val_p /= other.rep_p->val_p;
}

template <class T>
void AutoDiff<T>::operator+=(const AutoDiff<T> &other) {
  if (other.rep_p->nd_p != 0) {
    if (rep_p->nd_p == 0) {
      release();
      rep_p = pool.get(other.rep_p->nd_p);
      for (uInt i=0; i<rep_p->nd_p; i++) {
	rep_p->grad_p[i] = other.rep_p->grad_p[i];
      };
    } else {
      for (uInt i=0; i<rep_p->nd_p ; i++) {
	rep_p->grad_p[i] += other.rep_p->grad_p[i];
      };
    };
  };
  rep_p->val_p += other.rep_p->val_p;
}

template <class T>
void AutoDiff<T>::operator-=(const AutoDiff<T> &other) {
  if (other.rep_p->nd_p != 0) {
    if (rep_p->nd_p == 0) {
      T v = rep_p->val_p;
      release();
      rep_p = pool.get(other.rep_p->nd_p);
      for (uInt i=0; i<rep_p->nd_p ; i++) {
	rep_p->grad_p[i] = -other.rep_p->grad_p[i];
      };
      rep_p->val_p = v;
    } else {
      for (uInt i=0; i<rep_p->nd_p ; i++) {
	rep_p->grad_p[i] -= other.rep_p->grad_p[i];
      };
    };
  };
  rep_p->val_p -= other.rep_p->val_p;
}

template <class T>
void AutoDiff<T>::operator*=(const T other) {
  for (uInt i=0; i<rep_p->nd_p ; i++) rep_p->grad_p[i] *= other;
  rep_p->val_p *= other;
}

template <class T>
void AutoDiff<T>::operator/=(const T other) { 
  for (uInt i=0; i<rep_p->nd_p ; i++) rep_p->grad_p[i] /= other;
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
  Vector<T> vval(rep_p->nd_p);
  for (uInt i=0; i<rep_p->nd_p; i++) vval(i) = rep_p->grad_p[i];
  return vval;
}

template <class T> void AutoDiff<T>::derivatives(Vector<T> &res) const { 
  res.resize(rep_p->nd_p);
  for (uInt i=0; i<rep_p->nd_p; i++) res(i) = rep_p->grad_p[i];
}
