//# SparseDiff.cc: An automatic differentiating class for functions
//# Copyright (C) 2007,2008
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
//# $Id: SparseDiff.cc,v 1.3 2008/01/10 12:00:42 wbrouw Exp $

//# Includes
#include <scimath/Mathematics/SparseDiff.h>
#include <casa/Containers/ObjectStack.h>
#include <algorithm>

namespace casa { //# NAMESPACE CASA - BEGIN

  //# Constructors

  template <class T>
  SparseDiff<T>::SparseDiff() :
    rep_p(ObjectStack<SparseDiffRep<T> >::stack().get()) {}

  template <class T>
  SparseDiff<T>::SparseDiff(const T &v) :
    rep_p(ObjectStack<SparseDiffRep<T> >::stack().get()) {
    rep_p->val_p = v; 
  }

  template <class T>
  SparseDiff<T>::SparseDiff(const T &v, const uInt n) :
    rep_p(ObjectStack<SparseDiffRep<T> >::stack().get()) {
    rep_p->val_p = v;
    rep_p->grad_p.push_back(std::make_pair(n, T(1)));
  }

  template <class T>
  SparseDiff<T>::SparseDiff(const T &v, const uInt n, const T &der) :
    rep_p(ObjectStack<SparseDiffRep<T> >::stack().get()) {
    rep_p->val_p = v;
    rep_p->grad_p.push_back(std::make_pair(n, der));
  }

  template <class T>
  SparseDiff<T>::SparseDiff(const AutoDiff<T> &other) :
    rep_p(ObjectStack<SparseDiffRep<T> >::stack().get()) {
    rep_p->val_p = other.value();
    for (uInt i=0; i<other.nDerivatives(); ++i)
      if (other.derivative(i) != T(0))
	rep_p->grad_p.push_back(std::make_pair(i, other.derivative(i)));
  }

  template <class T>
  SparseDiff<T>::SparseDiff(const SparseDiff<T> &other) :
    rep_p(ObjectStack<SparseDiffRep<T> >::stack().get()) {
    rep_p->val_p = other.rep_p->val_p;
    rep_p->grad_p = other.rep_p->grad_p;
  }

  template<class T>
  SparseDiff<T>::~SparseDiff() {
    ObjectStack<SparseDiffRep<T> >::stack().put(rep_p); rep_p=0;
  }

  template <class T>
  SparseDiff<T> &SparseDiff<T>::operator=(const T &v) {
    rep_p->val_p = v;
    return *this;
  }

  template <class T>
  SparseDiff<T> &SparseDiff<T>::operator=(const pair<uInt, T> &der) {
    rep_p->grad_p.push_back(der);
    sort();
    return *this;
  }

  template <class T>
  SparseDiff<T> &SparseDiff<T>::operator=(const vector<pair<uInt, T> > &der) {
    rep_p->grad_p = der;
    sort();
    return *this;
  }

  template <class T>
  SparseDiff<T> &SparseDiff<T>::operator=(const AutoDiff<T> &other) { 
    rep_p->val_p = other.value();
    rep_p->grad_p.clear();
    for (uInt i=0; i<other.nDerivatives(); ++i)
      if (other.derivative(i) != T(0))
	rep_p->grad_p.push_back(std::make_pair(i, other.derivative(i)));
    return *this;
  }

  template <class T>
  SparseDiff<T> &SparseDiff<T>::operator=(const SparseDiff<T> &other) { 
    if (this != &other) {
      rep_p->val_p = other.rep_p->val_p;
      rep_p->grad_p = other.rep_p->grad_p;
    }
    return *this;
  }

  template <class T>
  void SparseDiff<T>::operator*=(const SparseDiff<T> &other) {
    T v;
    if (grad().empty()) {
      for (typename vector<pair<uInt, T> >::const_iterator
	     i=other.grad().begin(); i!=other.grad().end(); ++i)
	if (value() != T(0)) 
	  grad().push_back(std::make_pair(i->first, i->second * value())); 
    } else if (other.grad().empty()) {
      for (typename vector<pair<uInt, T> >::iterator i=grad().begin();
	   i!=grad().end(); ++i)
	if (value() != T(0)) i->second *= other.value(); 
    } else {
      SparseDiffRep<T> *tmp = ObjectStack<SparseDiffRep<T> >::stack().get();
      typename vector<pair<uInt, T> >::const_iterator j=other.grad().begin();
      for (typename vector<pair<uInt, T> >::iterator i=grad().begin();
	   i!=grad().end(); ++i) {
	if (j==other.grad().end()) {
	  if (other.value() != T(0)) 
	    tmp->grad_p.push_back(std::make_pair(i->first,
						 i->second * other.value())); 
	} else if (j->first == i->first) {
	  if ((v = i->second*other.value() + j->second*value()) != T(0))
	    tmp->grad_p.push_back(std::make_pair(i->first, v));
	  ++j;
	} else if (j->first > i->first) {
	  if (other.value() != T(0))
	    tmp->grad_p.push_back(std::make_pair(i->first,
						 i->second * other.value()));
	} else {
	  if (value() != T(0))
	    tmp->grad_p.push_back(std::make_pair(j->first, j->second*value()));
	  ++j;
	  --i;
	}
      }
      if (value() != T(0))
	for ( ; j!=other.grad().end(); ++j) 
	  tmp->grad_p.push_back(std::make_pair(j->first, j->second*value()));
      tmp->val_p = value();
      ObjectStack<SparseDiffRep<T> >::stack().put(rep_p);
      rep_p = tmp;
    }
    value() *= other.value();
  }

  template <class T>
  void SparseDiff<T>::operator/=(const SparseDiff<T> &other) {
    T t = other.value()*other.value();
    T v;
    if (grad().empty()) {
      for (typename vector<pair<uInt, T> >::const_iterator
	     i=other.grad().begin(); i!=other.grad().end(); ++i)
	if (value() != T(0)) 
	  grad().push_back(std::make_pair(i->first, -i->second * value()/t)); 
    } else if (other.grad().empty()) {
      for (typename vector<pair<uInt, T> >::iterator i=grad().begin();
	   i!=grad().end(); ++i)
	i->second /= other.value(); 
    } else {
      SparseDiffRep<T> *tmp = ObjectStack<SparseDiffRep<T> >::stack().get();
      typename vector<pair<uInt, T> >::const_iterator j=other.grad().begin();
      for (typename vector<pair<uInt, T> >::iterator i=grad().begin();
	   i!=grad().end(); ++i) {
	if (j==other.grad().end()) {
	  tmp->grad_p.push_back(std::make_pair(i->first,
					       i->second / other.value())); 
	} else if (j->first == i->first) {
	  if ((v = i->second*other.value() - j->second*value()) != T(0))
	    tmp->grad_p.push_back(std::make_pair(i->first, v/t));
	  ++j;
	} else if (j->first > i->first) {
	  tmp->grad_p.push_back(std::make_pair(i->first,
					       i->second / other.value()));
	} else {
	  if (value() != T(0))
	    tmp->grad_p.push_back(std::make_pair(j->first,
						 -j->second*value()/t));
	  ++j;
	  --i;
	}
      }
      if (value() != T(0))
	for ( ; j!=other.grad().end(); ++j) 
	  tmp->grad_p.push_back(std::make_pair(j->first,
					       -j->second * value()/t));
      tmp->val_p = value();
      ObjectStack<SparseDiffRep<T> >::stack().put(rep_p);
      rep_p = tmp;
    }
    value() /= other.value();
  }

  template <class T>
  void SparseDiff<T>::operator+=(const SparseDiff<T> &other) {
    T v;
    if (grad().empty()) {
      for (typename vector<pair<uInt, T> >::const_iterator
	     i=other.grad().begin(); i!=other.grad().end(); ++i)
	grad().push_back(*i); 
    } else if (other.grad().empty()) {
    } else {
      SparseDiffRep<T> *tmp = ObjectStack<SparseDiffRep<T> >::stack().get();
      typename vector<pair<uInt, T> >::const_iterator j=other.grad().begin();
      for (typename vector<pair<uInt, T> >::iterator i=grad().begin();
	   i!=grad().end(); ++i) {
	if (j==other.grad().end()) {
	  tmp->grad_p.push_back(*i);
	} else if (j->first == i->first) {
	  if ((v = i->second + j->second) != T(0))
	    tmp->grad_p.push_back(std::make_pair(i->first, v));
	  ++j;
	} else if (j->first > i->first) {
	  tmp->grad_p.push_back(*i);
	} else {
	  tmp->grad_p.push_back(*j);
	  ++j;
	  --i;
	}
      }
      for ( ; j!=other.grad().end(); ++j) tmp->grad_p.push_back(*j);
      tmp->val_p = value();
      ObjectStack<SparseDiffRep<T> >::stack().put(rep_p);
      rep_p = tmp;
    }
    value() += other.value();
  }

  template <class T>
  void SparseDiff<T>::operator-=(const SparseDiff<T> &other) {
    SparseDiff<T> tmp = other;
    for (typename vector<pair<uInt, T> >::iterator
	   i=tmp.grad().begin(); i!=tmp.grad().end(); ++i)
      i->second = -i->second;
    tmp.value() = -tmp.value();
    SparseDiff<T>::operator+=(tmp);
  }

  template <class T>
  AutoDiff<T> SparseDiff<T>::toAutoDiff(uInt n) const {
    AutoDiff<T> tmp(n);
    for (typename vector<pair<uInt, T> >::const_iterator
	   i=grad().begin(); i!=grad().end(); ++i) {
      if (i->first < n) tmp.derivative(i->first) = i->second;
    }
    return tmp;
  }

  template <class T>
  vector<pair<uInt, T> > &SparseDiff<T>::derivatives() const { 
    return rep_p->grad_p;
  }

  template <class T>
  void SparseDiff<T>::derivatives(vector<pair<uInt, T> > &res) const { 
    res = rep_p->grad_p;
  }

  template <class T>
  Bool SparseDiff<T>::ltSort(pair<uInt, T> &lhs, pair<uInt, T> &rhs) {
    return (lhs.first < rhs.first);
  }

  template <class T>
  void SparseDiff<T>::sort() {
    std::make_heap(grad().begin(), grad().end(), SparseDiff<T>::ltSort);
    std::sort_heap(grad().begin(), grad().end(), SparseDiff<T>::ltSort);
    // Remove empty ones; and add identical ones
    SparseDiffRep<T> *tmp = ObjectStack<SparseDiffRep<T> >::stack().get();
    for (typename vector<pair<uInt, T> >::iterator i=grad().begin();
	 i!=grad().end(); ++i) {
      if (i != grad().begin()) {
	if (i->first == (i-1)->first) {
	  i->second += (i-1)->second;
	  i->second = T(0);
	}
      }
    }
    for (typename vector<pair<uInt, T> >::iterator i=grad().begin();
	 i!=grad().end(); ++i) {
      if (i->second != T(0)) tmp->grad_p.push_back(*i);
    }
    tmp->val_p = value();
    ObjectStack<SparseDiffRep<T> >::stack().put(rep_p);
    rep_p = tmp;
  }
 
} //# NAMESPACE CASA - END

