//# PoolStack.cc: A parameterized stack of re-usable objects
//# Copyright (C) 2001,2004
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

#ifndef CASA_POOLSTACK_TCC
#define CASA_POOLSTACK_TCC

//# Includes
#include <casacore/casa/Containers/PoolStack.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Constructors
template <class T, class Key>
PoolStack<T, Key>::PoolStack() :
  top_p(0), stack_p(), key_p() {}

template <class T, class Key>
PoolStack<T, Key>::PoolStack(const Key &key) :
  top_p(0), stack_p(), key_p(key) {}

template <class T, class Key>
PoolStack<T, Key>::~PoolStack() {
  for (uInt i=0; i<stack_p.nelements(); i++) {
    delete stack_p[i]; stack_p[i] = 0;
  }
}

//# Member functions

template <class T, class Key>
void PoolStack<T, Key>::addElements(const uInt n) {
  stack_p.resize(stack_p.nelements() + n);
  for (uInt i=0; i<n; i++) {
    stack_p[stack_p.nelements()-n+i] = 0;
    release(new T(key_p));
  }
}

template <class T, class Key>
void PoolStack<T, Key>::clear() {
  for (uInt i=0; i<top_p; i++) {
    delete stack_p[i]; stack_p[i] = 0;
  }
  stack_p.resize(stack_p.nelements() - top_p, True);
  top_p = 0;
}







} //# NAMESPACE CASACORE - END


#endif
