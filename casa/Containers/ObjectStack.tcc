//# ObjectStack.cc: A stack of re-usable objects
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
//# $Id: ObjectStack.cc,v 1.1 2007/11/16 04:08:17 wbrouw Exp $

//# Includes
#include <casa/Containers/ObjectStack.h>

namespace casa { //# NAMESPACE CASA - BEGIN

  template <class T>
  ObjectStack<T> &ObjectStack<T>::stack() {
    static ObjectStack<T> theStack;
    return theStack;
  }

  //# Destructor
  template <class T>
  ObjectStack<T>::~ObjectStack() {
    for (uInt i=0; i<stack_p.size(); ++i) {
      delete stack_p[i];
      stack_p[i]=0;
    }
  }

  //# Member functions
  template <class T>
  T *ObjectStack<T>::get() {
    ScopedMutexLock lock(mutex_p);
    if (stack_p.empty()) stack_p.push_back(new T);
    stack_p.pop_back();
    return *stack_p.end();
  }

  template <class T>
  void ObjectStack<T>::clear() {
    ScopedMutexLock lock(mutex_p);
    vector<T*>(stack_p).swap(stack_p);
  }

} //# NAMESPACE CASA - END

