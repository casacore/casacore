//# ListMap.cc: Map with list ordering/operations
//# Copyright (C) 1993,1994,1995,2000,2001
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

#include <casa/Containers/ListMap.h>


template<class key, class value> value *ListMapRep<key,value>::isDefined(const key &kv) {
  if (!list.atEnd() && list.getRight().x() == kv) {
    value &ret = list.getRight().y();
    return(&ret);
  }
  list.toStart();
  while (list.atEnd() != True){
    if ((list.getRight()).x() == kv) {
//
//      Had to change this because of a **SUN BUG**
//
//    return(&(list.getRight()).y());
//
      OrderedPair<key,value> &opTmp = list.getRight();
      return(&opTmp.y());
    }
    ++list;
  }
  return(0);
}

template<class key, class value> const value *ListMapRep<key,value>::isDefined(const key &kv) const {
  if (!list.atEnd() && list.getRight().x() == kv) {
    const value &ret = list.getRight().y();
    return(&ret);
  }
  ConstListIter<OrderedPair<key,value> > listp = list;
  listp.toStart();
  while (listp.atEnd() != True){
    if ((listp.getRight()).x() == kv) {
//
//      Had to change this because of a **SUN BUG**
//
//    return(&(list.getRight()).y());
//
      const OrderedPair<key,value> &opTmp = listp.getRight();
      return(&opTmp.y());
    }
    ++listp;
  }
  return(0);
}

template<class key, class value> uInt ListMapRep<key,value>::ndefined() const {
  return(list.len());
}

template<class key, class value> void ListMapRep<key,value>::remove(const key &kv) {
  if (!list.atEnd() && list.getRight().x() == kv){
    list.removeRight();
    return;
  }
  list.toStart();
  while (list.atEnd() != True) {
    if (list.getRight().x() == kv){
      list.removeRight();
      return;
    }
    ++list;
  }
}

template<class key, class value> value &ListMapRep<key,value>::define(const key &k, const value &v) {
  list.toStart();
  while (list.atEnd() != True) {
    if (list.getRight().x() == k){
      list.removeRight();
      break;
    }
    ++list;
  }
  if (deforder == Append)
    list.toEnd();
  else
    list.toStart();
  list.addRight(OrderedPair<key,value>(k,v));
  return(list.getRight().y());
}
    
template<class key, class value> void ListMapRep<key,value>::clear() {
  list.toStart();
  while (list.atEnd() != True){
    list.removeRight();
  }
}

template<class key, class value>
MapIterRep<key,value> *ListMapRep<key,value>::getRep(Map<key,value> *container) const {
  ListMapIterRep<key,value> *ret = new ListMapIterRep<key,value>((ListMap<key,value> *)container);
  return(ret);
}

template<class key, class value>
MapRep<key,value> *ListMapRep<key,value>::Clone() const {
  ListMapRep<key,value> *ret = new ListMapRep<key,value>(this->DefaultVal,
							 deforder);
  ConstListIter<OrderedPair<key,value> > listp = list;

  ret->list.toStart();
  for(listp.toStart();listp.atEnd() != True; listp++) {
    ret->list.addRight(listp.getRight());
    ret->list++;
  }
  return ret;
}

template<class key, class value> 
void ListMapIterRep<key,value>::operator++() {
  if (listp.atEnd() == True)
    throw(AipsError("ListMap::operator++ - at end"));
  ++listp;
}

template<class key, class value>
void ListMapIterRep<key,value>::operator++(int) {
  if (listp.atEnd() == True)
    throw(AipsError("ListMap::operator++ - at end"));
  ++listp;
}
