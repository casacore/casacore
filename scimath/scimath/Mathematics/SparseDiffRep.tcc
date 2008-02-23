//# SparseDiffRep.cc: Representation of an automatic differential class data
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
//# $Id: SparseDiffRep.cc,v 1.1 2007/11/16 04:34:46 wbrouw Exp $

//# Includes
#include <scimath/Mathematics/SparseDiffRep.h>

namespace casa { //# NAMESPACE CASA - BEGIN

  //# Constructors
  template <class T>
  SparseDiffRep<T>::SparseDiffRep() :
    val_p(T(0.0)), grad_p(), link_p(1) {}

  //# Member functions
  template <class T>
  SparseDiffRep<T> &SparseDiffRep<T>::operator=(const T &v) {
    val_p = v;
    return *this;
  }

  template <class T>
  SparseDiffRep<T> &SparseDiffRep<T>::
  operator=(const vector<pair<uInt, T> > &grad) {
    grad_p = grad;
    return *this;
  }

  template <class T>
  SparseDiffRep<T> &SparseDiffRep<T>::operator=(const SparseDiffRep<T> &other) { 
    if (this != &other) {
      val_p = other.val_p;
      grad_p = other.grad_p;
    }
    return *this;
  }

  template <class T>
  void SparseDiffRep<T>::operator*=(const T other) {
    for (typename vector<pair<uInt, T> >::iterator i=grad_p.begin();
	 i!=grad_p.end(); ++i) i->second *= other;
  }

  template <class T>
  void SparseDiffRep<T>::operator/=(const T other) {
    for (typename vector<pair<uInt, T> >::iterator i=grad_p.begin();
	 i!=grad_p.end(); ++i) i->second /= other;
  }

} //# NAMESPACE CASA - END

