//# PoolStack.h: A parameterized stack of re-usable objects
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

#if !defined(AIPS_POOLSTACK_H)
#define AIPS_POOLSTACK_H

//# Includes
#include <aips/aips.h>
#include <aips/Containers/Block.h>

//# Forward declarations

// <summary>
// A parameterized stack of re-usable objects
// </summary>
//
// <use visibility=export>
//
// <reviewed reviewer="" date="yyyy/mm/dd" tests="tAutoDiff.cc" demos="">
// </reviewed>
//
// <prerequisite>
// <li> <linkto class=ObjectPool>ObjectPool</linkto>
// </prerequisite>
//
// <synopsis>
// A PoolStack contains a set of pre-allocated Objects of the type
// <src>T</src>. The stack is a very simple stack, without the
// linking/unlinking of a normal Stack implementation.
// This lightweight implementation was especially designed for use
// with the <linkto class=ObjectPool>ObjectPool</linkto>
// class, but can be used independently.
// 
// As an example, a <src><Vector<Double> ></src> PoolStack can
// be used to pre-allocate some vectors of a specified length.
//
// Objects can be obtained with the <src>popVal()</src> method, and
// returned with <src>push()</src>.
// </synopsis>
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
//  <li> the class Key must be sortable to be used as a key in the Map
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
  // Create the stack
  PoolStack();
  // Delete the stack
  ~PoolStack();

  //# Member functions
  // Get a pointer to an object in the stack. The stack will be extended if
  // no objects left.
  // <group>
  T *popVal(const Key &key);
  // </group>

  // Return an object to the stack for re-use
  void push(T *obj);

  // Add n elements
  void addElements(const uInt n);

  // Decimate the stack
  void clear();

  // Test if stack empty
  Bool empty() { return top_p == 0; };

  // return the stack extend (for debugging use only)
  uInt nelements() { return stack_p.nelements(); };

private:
  //# Data
  // Current pointer to top-of-stack
  uInt top_p;
  // The stack
  PtrBlock<T*> stack_p;

  //# Constructors
  // Copy and assignment constructors and assignment (not implemented)
  // <group>
  PoolStack(const PoolStack<T, Key> &other);
  PoolStack<T, Key> &operator=(const PoolStack<T, Key> &other);
  // </group>

  //# Member functions
};

#endif
