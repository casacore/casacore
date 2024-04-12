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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#ifndef SCIMATH_AUTODIFF_TCC
#define SCIMATH_AUTODIFF_TCC

//# Includes
#include <casacore/scimath/Mathematics/AutoDiff.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Utilities/Assert.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN


template <class T>
AutoDiff<T>::AutoDiff() :
  val_p(T(0.0)), nd_p(0), grad_p(0) {}

template <class T>
AutoDiff<T>::AutoDiff(const T &v) :
  val_p(v), nd_p(0), grad_p(0) {}

template <class T>
AutoDiff<T>::AutoDiff(const T &v, const uInt ndiffs, const uInt n) :
  val_p(v), nd_p(ndiffs),
  grad_p(ndiffs) {
  grad_p = T(0);
  grad_p[n] = T(1);
}

template <class T>
AutoDiff<T>::AutoDiff(const T &v, const uInt ndiffs) :
  val_p(v), nd_p(ndiffs),
  grad_p(ndiffs) {
  grad_p = T(0);
}

template <class T>
AutoDiff<T>::AutoDiff(const AutoDiff<T> &other) :
  val_p(other.val_p), nd_p(other.nd_p),
  grad_p(other.nd_p) {
  grad_p = other.grad_p;
}

template <class T>
AutoDiff<T>::AutoDiff(const T &v, const Vector<T> &derivs) :
  val_p(v), nd_p(derivs.nelements()),
  grad_p(nd_p) {
  grad_p = derivs;
}

template<class T>
AutoDiff<T>::~AutoDiff() {}

template <class T>
AutoDiff<T> &AutoDiff<T>::operator=(const T &v) {
  val_p = v;
  nd_p = 0;
  grad_p.resize(nd_p);
  return *this;
}

template <class T>
AutoDiff<T> &AutoDiff<T>::operator=(const AutoDiff<T> &other) { 
  if (this != &other) {
    val_p = other.val_p;
    nd_p = other.nd_p;
    grad_p.resize(nd_p);
    grad_p = other.grad_p;
  }
  return *this;
}


template <class T>
void AutoDiff<T>::operator*=(const AutoDiff<T> &other) {
  if (other.nd_p != 0) {
    if (nd_p == 0) {
      nd_p = other.nd_p;
      grad_p = other.grad_p * val_p;
    } else {
      AlwaysAssert (nd_p == other.nd_p, AipsError);
      for (uInt i=0; i<nd_p ; i++) {
	grad_p[i] = val_p*other.grad_p[i] + other.val_p*grad_p[i];
      }
    }
  } else {
    grad_p *= other.val_p;
  }
  val_p *= other.val_p;
}

template <class T>
void AutoDiff<T>::operator/=(const AutoDiff<T> &other) { 
  T temp = other.val_p * other.val_p;
  if (other.nd_p != 0) {
    if (nd_p == 0) {
      nd_p = other.nd_p;
      grad_p = other.grad_p * (-val_p/temp);
      ///val_p = other.val_p;
    } else {
      AlwaysAssert (nd_p == other.nd_p, AipsError);
      for (uInt i=0; i<nd_p ; i++) {
	grad_p[i] = grad_p[i]/other.val_p - val_p*other.grad_p[i]/temp;
      }
    }
  } else {
    grad_p /= other.val_p;
  }
  val_p /= other.val_p;
}

template <class T>
void AutoDiff<T>::operator+=(const AutoDiff<T> &other) {
  if (other.nd_p != 0) {
    if (nd_p == 0) {
      nd_p = other.nd_p;
      grad_p = other.grad_p;
    } else {
      AlwaysAssert (nd_p == other.nd_p, AipsError);
      grad_p += other.grad_p;
    }
  }
  val_p += other.val_p;
}

template <class T>
void AutoDiff<T>::operator-=(const AutoDiff<T> &other) {
  if (other.nd_p != 0) {
    if (nd_p == 0) {
      nd_p = other.nd_p;
      grad_p = -other.grad_p;
    } else {
      AlwaysAssert (nd_p == other.nd_p, AipsError);
      grad_p -= other.grad_p;
    }
  }
  val_p -= other.val_p;
}

template <class T>
void AutoDiff<T>::operator*=(const T other) {
  grad_p *= other;
  val_p *= other;
}

template <class T>
void AutoDiff<T>::operator/=(const T other) { 
  grad_p /= other;
  val_p /= other;
}

template <class T>
void AutoDiff<T>::operator+=(const T other) {
  val_p += other;
}

template <class T>
void AutoDiff<T>::operator-=(const T other) {
  val_p -= other;
}


template <class T> void AutoDiff<T>::derivatives(Vector<T> &res) const { 
  res.resize(nd_p);
  res = grad_p;
}

} //# NAMESPACE CASACORE - END


#endif
