//# ObjectStack.h: A stack of re-usable objects
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
//# $Id: ObjectStack.h,v 1.1 2007/11/16 04:08:17 wbrouw Exp $

#ifndef CASA_OBJECTSTACK_H
#define CASA_OBJECTSTACK_H

//# Includes
#include <casa/aips.h>
#include <casa/vector.h>
#include <casa/OS/Mutex.h>

namespace casa { //# NAMESPACE CASA - BEGIN

  //# Forward declarations

  // <summary>
  // A stack of re-usable objects
  // </summary>
  //
  // <use visibility=export>
  //
  // <reviewed reviewer="Ger van Diepen" date="2001/07/03" tests="tObjectStack.cc" demos="">
  // </reviewed>
  //
  // <prerequisite>
  // <li>
  // </prerequisite>
  //
  // <synopsis>
  // An ObjectStack contains a set of pre-allocated Objects of the type
  // <src>T</src>.
  // The stack is a very simple stack, without the
  // linking/unlinking of a normal Stack implementation.
  // This lightweight implementation was especially designed for use
  // with the <linkto class=AutoDiff>AutoDiff</linkto>
  // classes, but can be used independently. The stack works best with small
  // object sizes, or letter/envelope classes.
  //
  // The class is fully thread-safe, thus the same object can be used safely
  // in multiple threads.
  //
  // </synopsis>
  //
  // <example>
  // <srcblock>
  //   {
  //   // Get an element (and create stack!)
  //   SparseDiff<Double> elem;
  //   // Use it
  //   elem.value() = 27;
  //   // Release it (automatic by dtor on elem)
  //   }
  // </srcblock>
  // </example>
  //
  // <motivation>
  // To improve the speed for the auto differentiating classes.
  // </motivation>
  //
  // <templating arg=T>
  //  <li> the class T must have a <em>constructor(T::FULLREFRESH)</em>
  //       for creating new entries and destructor;
  //      and must possess a <em>clear()</em> method to enable element re-use.
  // </templating>
  //
  // <todo asof="2007/11/27">
  // </todo>

  template <class T> class ObjectStack {
  public:

    //# Member functions
    // Destructor
    ~ObjectStack();

    // Create a singleton stack
    static ObjectStack<T> &stack();

    // Get a pointer to an object in the stack. The stack will be extended if
    // no objects left.
    T *get();

    // Return an object to the stack for re-use
    void put(T *obj) { obj->clear(); stack_p.push_back(obj); };

    // Decimate the stack by getting rid of all unused elements in it
    void clear();

    // Test if stack empty
    Bool empty() { return stack_p.empty(); };

    // return the stack extend (for debugging use and checking mainly)
    uInt nelements() const { return stack_p.size(); };

  private:
    //# Data
    // The Stack
    vector<T*> stack_p;
    Mutex      mutex_p;

    //# Constructors
    // All ctor and assignment constructors and assignment (not implemented)
    // <group>
    ObjectStack() : stack_p() {};
    ObjectStack(const ObjectStack<T> &other);
    ObjectStack<T> &operator=(const ObjectStack<T> &other);
    // </group>

    //# Member functions
  };


} //# NAMESPACE CASA - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casa/Containers/ObjectStack.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
