//# ObjectPool.cc: A parameterized stack of re-usable objects
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

//# Includes
/// Move to aips/Containers
#include <trial/Mathematics/ObjectPool.h>

//# Constructors
template <class T, class Key>
ObjectPool<T, Key>::ObjectPool() :
  defKey_p(), defVal_p(new T), defStack_p(new PoolStack<T, Key>),
  cacheKey_p(), cacheStack_p(0),
  map_p(defStack_p, PoolStack<T, Key>::NDEF) {}

template <class T, class Key>
ObjectPool<T, Key>::~ObjectPool() {
}

template <class T, class Key>
T *const ObjectPool<T, Key>::get(const Key key) {
  PoolStack<T, Key> *v;
  if (key == defKey_p) v = defStack_p;
  else if (key == cacheKey_p && cacheStack_p) v = cacheStack_p;
  else {
    PoolStack<T, Key> *const *v0;
    if (!(v0 = map_p.isDefined(key))) {
      v = map_p.define(key, new PoolStack<T, Key>);
    } else v = *v0;
    cacheKey_p = key;
    cacheStack_p = v;
  };
  return v->popVal(key);
}

template <class T, class Key>
void ObjectPool<T, Key>::release(T *obj, const Key key) {
  PoolStack<T, Key> *v;
  if (key == defKey_p) v = defStack_p;
  else if (key == cacheKey_p && cacheStack_p) v = cacheStack_p;
  else {
    PoolStack<T, Key> *const *v0;
    if (!(v0 = map_p.isDefined(key))) {
      v = map_p.define(key, new PoolStack<T, Key>);
    } else v = *v0;
  };
  v->push(obj);
}

template <class T, class Key>
void ObjectPool<T, Key>::clearStacks() {
}

template <class T, class Key>
void ObjectPool<T, Key>::clearStack(const Key key) {
}

template <class T, class Key>
void ObjectPool<T, Key>::clear() {
}

