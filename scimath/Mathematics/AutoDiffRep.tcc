//# AutoDiffRep.cc: Representation of an automatic differential class data
//# Copyright (C) 2001
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

#ifndef SCIMATH_AUTODIFFREP_TCC
#define SCIMATH_AUTODIFFREP_TCC

//# Includes
#include <casacore/scimath/Mathematics/AutoDiffRep.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Constructors
template <class T>
AutoDiffRep<T>::AutoDiffRep() :
  val_p(T(0.0)), nd_p(0), nocopy_p(False), grad_p(0) {}

template <class T>
AutoDiffRep<T>::AutoDiffRep(const T &v) :
  val_p(v), nd_p(0), nocopy_p(False), grad_p(0) {}

template <class T>
AutoDiffRep<T>::AutoDiffRep(const T &v, const uInt ndiffs, const uInt n) :
  val_p(v), nd_p(ndiffs), nocopy_p(False),
  grad_p(ndiffs) {
  grad_p = T(0);
  grad_p[n] = T(1);
}

template <class T>
AutoDiffRep<T>::AutoDiffRep(const T &v, const uInt ndiffs) :
  val_p(v), nd_p(ndiffs), nocopy_p(False),
  grad_p(ndiffs) {
  grad_p = T(0);
}

template <class T>
AutoDiffRep<T>::AutoDiffRep(const uInt ndiffs) :
  val_p(0), nd_p(ndiffs), nocopy_p(False),
  grad_p(ndiffs) {
  grad_p = T(0);
}

template <class T>
AutoDiffRep<T>::AutoDiffRep(const AutoDiffRep<T> &other) :
  val_p(other.val_p), nd_p(other.nd_p), nocopy_p(False),
  grad_p(other.nd_p) {
  grad_p = other.grad_p;
}

template <class T>
AutoDiffRep<T>::AutoDiffRep(const T &v, const Vector<T> &derivs) :
  val_p(v), nd_p(derivs.nelements()), nocopy_p(False),
  grad_p(nd_p) {
  grad_p = derivs;
}

template<class T>
AutoDiffRep<T>::~AutoDiffRep() {}

template <class T>
AutoDiffRep<T> &AutoDiffRep<T>::operator=(const T &v) {
  val_p = v;
  nd_p = 0;
  grad_p.resize(nd_p);
  return *this;
}

template <class T>
AutoDiffRep<T> &AutoDiffRep<T>::operator=(const AutoDiffRep<T> &other) { 
  if (this != &other) {
    val_p = other.val_p;
    nd_p = other.nd_p;
    grad_p.resize(nd_p);
    grad_p = other.grad_p;
  }
  return *this;
}


} //# NAMESPACE CASACORE - END


#endif
