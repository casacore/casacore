//# PoolStack.cc: A parameterized stack of re-usable objects
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
#include <trial/Mathematics/PoolStack.h>

//# Constructors
template <class T, class Key>
PoolStack<T, Key>::PoolStack() :
  top_p(0), stack_p() {}

template <class T, class Key>
PoolStack<T, Key>::~PoolStack() {
  for (uInt i=0; i<stack_p.nelements(); i++) delete stack_p[i];
}

template <class T, class Key>
T *PoolStack<T, Key>::popVal(const Key &key) {
  // Stack is empty
  if (!top_p) {
    addElements(NDEF);
    for (uInt i=0; i<NDEF; i++) push(new T(key));
  };
  return stack_p[--top_p];
}

template <class T, class Key>
void PoolStack<T, Key>::push(T *obj) {
  stack_p[top_p++] = obj;
}

template <class T, class Key>
void PoolStack<T, Key>::addElements(const uInt n) {
  stack_p.resize(stack_p.nelements() + n);
  for (uInt i=top_p; i<stack_p.nelements(); i++) {
    stack_p[i] = static_cast<T*>(0);
  };
}

template <class T, class Key>
void PoolStack<T, Key>::clear() {
}

