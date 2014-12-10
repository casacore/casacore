//# PoolStack.h: A parameterized stack of re-usable objects
//# Copyright (C) 2001,2002,2004
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

#ifndef CASA_POOLSTACK_H
#define CASA_POOLSTACK_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Containers/Block.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations

// <summary>
// A parameterized stack of re-usable objects
// </summary>
//
// <use visibility=export>
//
// <reviewed reviewer="Ger van Diepen" date="2001/07/03" tests="tPoolStack.cc" demos="">
// </reviewed>
//
// <prerequisite>
// <li>
// </prerequisite>
//
// <synopsis>
// A PoolStack contains a set of pre-allocated Objects of the type
// <src>T</src>, with a parameter <src>Key</src> (e.g. an object could be
// a <src>Vector</src> of <src>T Double</src> with an <src>uInt Key</src>).
// The stack is a very simple stack, without the
// linking/unlinking of a normal Stack implementation.
// This lightweight implementation was especially designed for use
// with the <linkto class=ObjectPool>ObjectPool</linkto>
// class, but can be used independently.
//
// Objects can be obtained with the <src>get()</src> method, and
// returned for re-use with <src>release()</src>.
//
// Objects are not initialised when popped. The user should never delete the
// object returned by get; but return it to the pool.
//
// PoolStack is not thread-safe, but ObjectPool is.
// </synopsis>
//
// <example>
// <srcblock>
//   // Create a pool of length 5 vectors
//   PoolStack<Vector<Double>, uInt> pool5(5);
//   // Get an element
//   Vector<Double> *elem(pool5.get());
//   // Use it
//   (*elem)(2) = 27;
//   // Release it
//   pool5.release(elem);
// </srcblock>
// </example>
//
// <motivation>
// To improve the speed for the auto differentiating class.
// </motivation>
//
// <templating arg=T>
//  <li> the class T must have a constructor with a Key argument
// </templating>
//
// <templating arg=Key>
//  <li> the class Key must be usable as a constructor argument for T
// </templating>
//
// <todo asof="2001/06/07">
// <li> Nothing I know of
// </todo>

template <class T, class Key> class PoolStack {
 public:
  //# Constants
  // Number of default stack entries.
  static const uInt NDEF=8;
  //# Constructors
  // Create the stack with the default Key
  PoolStack();
  // Create the stack for the specified key
  explicit PoolStack(const Key &key);
  // Delete the stack
  ~PoolStack();

  //# Member functions
  // Get a pointer to an object in the stack. The stack will be extended if
  // no objects left. Extension is done with the NDEF number of elements.
  // Different extension can be done manually with the addElements() method.
  T *get() { if (!top_p) addElements(NDEF); T *tmp = stack_p[--top_p];
  stack_p[top_p] = 0; return tmp; };

  // Return an object to the stack for re-use
  void release(T *obj) {if (obj) stack_p[top_p++] = obj; };

  // Add n elements
  void addElements(const uInt n);

  // Decimate the stack by getting rid of all unused elements in it
  void clear();

  // Test if stack empty
  Bool empty() { return top_p == 0; };

  // Return the key belonging to the stack
  const Key &key() const { return key_p; } 
  // return the stack extend (for debugging use and checking mainly)
  uInt nelements() const { return stack_p.nelements(); };

private:
  //# Data
  // Current pointer to top-of-stack
  uInt top_p;
  // The stack
  PtrBlock<T*> stack_p;
  // The key belonging to this stack
  Key key_p;

  //# Constructors
  // Copy and assignment constructors and assignment (not implemented)
  // <group>
  PoolStack(const PoolStack<T, Key> &other);
  PoolStack<T, Key> &operator=(const PoolStack<T, Key> &other);
  // </group>

  //# Member functions
};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/casa/Containers/PoolStack.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
