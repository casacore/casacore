//# ObjectPool.h: A parameterized stack of re-usable objects
//# Copyright (C) 2001,2002
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

#ifndef CASA_OBJECTPOOL_H
#define CASA_OBJECTPOOL_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Containers/PoolStack.h>
#include <casacore/casa/Containers/SimOrdMap.h>
#include <casacore/casa/OS/Mutex.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations

// <summary>
// A parameterized stack of re-usable objects
// </summary>
//
// <use visibility=export>
//
// <reviewed reviewer="Ger van Diepen" date="2001/07/04" tests="tObjectPool.cc" demos="">
// </reviewed>
//
// <prerequisite>
// <li> <linkto class=PoolStack>PoolStack</linkto>
// </prerequisite>
//
// <synopsis>
// An ObjectPool contains a set of pre-allocated Objects of the type
// <src>T</src>. A Map based on the <src>Key</src> values contains
// a stack of objects for each key value.
// 
// As an example, a <src><Vector<Double>, uInt></src> ObjectPool contains
// a <src>SimpleOrderedMap<uInt,PoolStack<Vector<Double>,uInt>* ></src>
// map. Each Stack will contain a stack of <src>Vector<Double></src>
// objects, with a length of the key value each.
//
// When an object is asked for with the <src>get</src> method, the
// correct stack is found using the parameter key. If no entry in the map
// exists, a new stack is created. If the relevant stack is empty, new elements
// are added to the stack.
// </synopsis>
//
// <example>
// <srcblock>
//   // Create a pool of vectors
//   ObjectPool<Vector<Double>, uInt> pool;
//   // Get a pointer to a pre-defined vector of length 5
//   Vector<Double> *el5(pool.get(5));
//   // and one of length 10
//   Vector<Double> *el10(pool.get(10));
//   ... 
//   // Release the objects for re-use
//   pool.release(el5, 5);
//   pool.release(el10, 10); 
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
//  <li> the class Key must be sortable to be used as a key in the Map
// </templating>
//
// <todo asof="2001/06/07">
// <li> Nothing at the moment
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
  // <src>release</src> method. The object should not be deleted by caller.
  // <group>
  T *get(const Key key=Key()) { return getStack(key).get(); };
  // </group>

  // Get the object stack for the given key
  PoolStack<T, Key> &getStack(const Key key);

  // Release an object obtained from the pool through <src>get</src> for
  // re-use.
  void release(T *obj, const Key key=Key());

  // Get the number of object stacks in the pool
  uInt nelements() const { return map_p.ndefined(); };

  // Decimate the stacks by deleting all unused objects.
  // <group>
  void clearStacks();
  void clearStack(const Key key=Key());
  // </group>

  // Decimate the stacks and remove any map entry that is completely unused
  void clear();

private:
  //# Data
  // The default key and stack, and the last referenced one (for caching
  // purposes)
  // <group>
  Key defKey_p;
  PoolStack<T, Key> *defStack_p;
  Key cacheKey_p;
  PoolStack<T, Key> *cacheStack_p;
  Mutex mutex_p;
  // </group>

  // The pool map
  SimpleOrderedMap<Key, PoolStack<T, Key>* > map_p;

  //# Constructors
  // Copy and assignment constructors and assignment (not implemented)
  // <group>
  ObjectPool(const ObjectPool<T, Key> &other);
  ObjectPool<T, Key> &operator=(const ObjectPool<T, Key> &other);
  // </group>

  //# Member functions
  // Do the actual clearing of the stack (without a lock).
  void doClearStack(const Key key);
};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/casa/Containers/ObjectPool.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
