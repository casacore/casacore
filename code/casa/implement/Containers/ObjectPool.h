//# ObjectPool.h: A parameterized stack of re-usable objects
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

#if !defined(AIPS_OBJECTPOOL_H)
#define AIPS_OBJECTPOOL_H

//# Includes
#include <aips/aips.h>
#include <trial/Mathematics/PoolStack.h>
#include <aips/Containers/SimOrdMap.h>

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
// <li> <linkto class=SimpleOrderedMap>SimpleOrderedMap</linkto>
// <li> <linkto class=PoolStack>PoolStack</linkto>
// </prerequisite>
//
// <synopsis>
// An ObjectPool contains a set of pre-allocated Objects of the type
// <src>T</src>. A Map based on the <src>Key</src> values contains
// a stack of objects each.
// 
// As an example, a <src><Vector<Double>, uInt></src> ObjectPool can
// contain a <src>SimpleOrderedMap<uInt, Stack<Vector<Double> > ></src>
// map. Each Stack will contain a stack of <src>Vector<Double></src>
// objects, with a length of the key value each.
//
// When an object is asked for with the <src>get</src> method, the
// correct stack is found using the parameter key. If no entry in the map
// exists, a new stack is created. If the relevant stack is empty, new elements
// are added to the stack.
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
// <li> Describe a way to pass references around in an easy way.
// </todo>

template <class T, class Key> class ObjectPool {
 public:
  //# Constructors
  // Create the pool
  ObjectPool();
  // Delete the pool
  ~ObjectPool();

  //# Member functions
  // Get a pointer to an object in the pool with the specified parameter. The
  // object is detached from the stack, and has to be returned with the 
  // <src>release</src> method.
  // <group>
  T *const get(const Key key=Key());
  // </group>

  // Release an object obtained from the pool through <src>get</src> for
  // re-use.
  void release(T *obj, const Key key=Key());

  // Decimate the stacks
  // <group>
  void clearStacks();
  void clearStack(const Key key=Key());
  // </group>

  // Decimate the map and the stacks
  void clear();

private:
  //# Constants
  // Number of default stack entries.
  static const uInt NDEF=8;
  //# Data
  // The default key and stack, and the last referenced one (for caching
  // purposes)
  // <group>
  Key defKey_p;
  T *defVal_p;
  PoolStack<T> *defStack_p;
  Key cacheKey_p;
  PoolStack<T> *cacheStack_p;
  // </group>

  // The pool map
  SimpleOrderedMap<Key, PoolStack<T>* > map_p;

  //# Constructors
  // Copy and assignment constructors and assignment (not implemented)
  // <group>
  ObjectPool(const ObjectPool<T, Key> &other);
  ObjectPool<T, Key> &operator=(const ObjectPool<T, Key> &other);
  // </group>

  //# Member functions
  // Push n new objects onto the appropriate stack.
  void pushStack(PoolStack<T> *v, const uInt n, const Key key=Key());
};

#endif
