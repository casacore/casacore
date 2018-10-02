//# Block.h: Simple templated array classes
//# Copyright (C) 1993-1997,2000,2002,2005,2015
//# Associated Universities, Inc. Washington DC, USA.
//# National Astronomical Observatory of Japan
//# 2-21-1, Osawa, Mitaka, Tokyo, 181-8588, Japan.
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

#ifndef CASA_BLOCK_H
#define CASA_BLOCK_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Utilities/Copy.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/casa/Containers/Allocator.h>
#include <cstddef>                  // for ptrdiff_t
#include <algorithm> // for std:min/max
#include <type_traits>

//# For index checking
#if defined(AIPS_ARRAY_INDEX_CHECK)
#include <casacore/casa/Exceptions/Error.h>
#endif

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>simple 1-D array</summary>
// <use visibility=export>
//
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
//
// <etymology>
//     This should be viewed as a <em>block</em> of memory without sophisticated
//     manipulation functions. Thus it is called <src>Block</src>.
// </etymology>
//
// <synopsis>
// <src>Block<T></src> is a simple templated 1-D array class. Indices are always
// 0-based. For efficiency reasons, no index checking is done unless the
// preprocessor symbol <src>AIPS_ARRAY_INDEX_CHECK</src> is defined. 
// <src>Block<T></src>'s may be assigned to and constructed from other
// <src>Block<T></src>'s.
// As no reference counting is done this can be an expensive operation, however.
//
// The net effect of this class is meant to be unsurprising to users who think
// of arrays as first class objects. The name "Block" is  intended to convey
// the concept of a solid "chunk" of things without any intervening "fancy"
// memory management, etc. This class was written to be
// used in the implementations of more functional Vector, Matrix, etc. classes,
// although it is expected <src>Block<T></src> will be useful on its own.
//
// The Block class should be efficient. You should normally use <src>Block</src>.
//
// <note role=warning> If you use the assignment operator on an element of this
// class, you may leave dangling references to pointers released from
// <src>storage()</src>.
// Resizing the array will also have this effect if the underlying storage
// is actually affected.
// </note>
//
// If index checking is turned on, an out-of-bounds index will
// generate an <src>indexError<uInt></src> exception.
// </synopsis>
//
// <example> 
// <srcblock>
// Block<Int> a(100,0);  // 100 ints initialized to 0
// Block<Int> b;         // 0-length Block
// // ...
// b = a;                // resize b and copy a into it
// for (size_t i=0; i < a.nelements(); i++) {
//     a[i] = i;    // Generate a sequence
//                  // with Vectors, could simply say "indgen(myVector);"
// }
// b.set(-1);       // All positions in b have the value -1
// b.resize(b.nelements()*2); // Make b twice as long, by default the old
//                            // elements are copied over, although this can
//                            // be defeated.
// some_c_function(b.storage());  // Use a fn that takes an
//                                // <src>Int *</src> pointer
// </srcblock> 
// </example>
//
class BlockTrace
{
public:
  // Set the trace size. The (de)allocation of Blocks with >= sz elements
  // will be traced using the MemoryTrace class.
  // A value 0 means no tracing.
  static void setTraceSize (size_t sz);
protected:
  // Write alloc and free trace messages.
  static void doTraceAlloc (const void* addr, size_t nelem,
                            DataType type, size_t sz);
  static void doTraceFree (const void* addr, size_t nelem,
                           DataType type, size_t sz);
protected:
  static size_t itsTraceSize;
};

template<typename T> class Block;


template<typename T>
class Block_internal_IsFundamental {
  template<typename U> friend class Block;
  static constexpr int value = static_cast<int>(std::is_fundamental<T>::value);
};

template<typename T>
class Block_internal_IsPointer {
  template<typename U> friend class Block;
  static constexpr int value = static_cast<int>(std::is_pointer<T>::value);
};



// <summary>simple 1-D array</summary>
// <use visibility=export>
//
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
//
// <etymology>
//     This should be viewed as a <em>block</em> of memory without sophisticated
//     manipulation functions. Thus it is called <src>Block</src>.
// </etymology>
//
// <synopsis>
// <src>Block<T></src> is a simple templated 1-D array class. Indices are always
// 0-based. For efficiency reasons, no index checking is done unless the
// preprocessor symbol <src>AIPS_ARRAY_INDEX_CHECK</src> is defined. 
// <src>Block<T></src>'s may be assigned to and constructed from other
// <src>Block<T></src>'s.
// As no reference counting is done this can be an expensive operation, however.
//
// The net effect of this class is meant to be unsurprising to users who think
// of arrays as first class objects. The name "Block" is  intended to convey
// the concept of a solid "chunk" of things without any intervening "fancy"
// memory management, etc. This class was written to be
// used in the implementations of more functional Vector, Matrix, etc. classes,
// although it is expected <src>Block<T></src> will be useful on its own.
//
// The Block class should be efficient. You should normally use <src>Block</src>.
//
// <note role=warning> If you use the assignment operator on an element of this
// class, you may leave dangling references to pointers released from
// <src>storage()</src>.
// Resizing the array will also have this effect if the underlying storage
// is actually affected.
// </note>
//
// If index checking is turned on, an out-of-bounds index will
// generate an <src>indexError<uInt></src> exception.
// </synopsis>
//
// <example> 
// <srcblock>
// Block<Int> a(100,0);  // 100 ints initialized to 0
// Block<Int> b;         // 0-length Block
// // ...
// b = a;                // resize b and copy a into it
// for (size_t i=0; i < a.nelements(); i++) {
//     a[i] = i;    // Generate a sequence
//                  // with Vectors, could simply say "indgen(myVector);"
// }
// b.set(-1);       // All positions in b have the value -1
// b.resize(b.nelements()*2); // Make b twice as long, by default the old
//                            // elements are copied over, although this can
//                            // be defeated.
// some_c_function(b.storage());  // Use a fn that takes an
//                                // <src>Int *</src> pointer
// </srcblock> 
// </example>
//
template<class T> class Block: public BlockTrace
{
public:
  // Create a zero-length Block. Note that any index into this Block
  // is an error.
  // DefaultAllocator<T> is used as an allocator.
  Block() :
      allocator_p(get_allocator<typename DefaultAllocator<T>::type>()), capacity_p(
          0), used_p(0), array(0), destroyPointer(True), keep_allocator_p(False) {
  }
  // Create a zero-length Block. Note that any index into this Block
  // is an error.
  template<typename Allocator>
  explicit Block(AllocSpec<Allocator> const &) :
      allocator_p(get_allocator<typename Allocator::type>()), capacity_p(0), used_p(
          0), array(0), destroyPointer(True), keep_allocator_p(False) {
  }

  // Create a Block with the given number of points. The values in Block
  // are initialized. Note that indices range between 0 and n-1.
  // DefaultAllocator<T> is used as an allocator.
  explicit Block(size_t n) :
      allocator_p(get_allocator<typename DefaultAllocator<T>::type>()), used_p(
          n), destroyPointer(True), keep_allocator_p(False) {
    init(init_anyway() ? ArrayInitPolicies::INIT : ArrayInitPolicies::NO_INIT);
  }

  // Create a Block with the given number of points. The values in Block
  // are initialized. Note that indices range between 0 and n-1.
  template<typename Allocator>
  Block(size_t n, AllocSpec<Allocator> const &) :
      allocator_p(get_allocator<typename Allocator::type>()), used_p(n), destroyPointer(
          True), keep_allocator_p(False) {
    init(init_anyway() ? ArrayInitPolicies::INIT : ArrayInitPolicies::NO_INIT);
  }

  // Create a Block with the given number of points. The values in Block
  // are uninitialized. Note that indices range between 0 and n-1.
  // DefaultAllocator<T> is used as an allocator.
  Block(size_t n, ArrayInitPolicy initPolicy) :
      allocator_p(get_allocator<typename DefaultAllocator<T>::type>()), used_p(
          n), destroyPointer(True), keep_allocator_p(False) {
    init(initPolicy);
  }

  // Create a Block with the given number of points.
  // Note that indices range between 0 and n-1.
  template<typename Allocator>
  Block(size_t n, ArrayInitPolicy initPolicy,
      AllocSpec<Allocator> const &) :
      allocator_p(get_allocator<typename Allocator::type>()), used_p(n), destroyPointer(
          True), keep_allocator_p(False) {
    init(initPolicy);
  }

  // Create a Block of the given length, and initialize (via copy constructor for 
  // objects of type T) with the provided value.
  // DefaultAllocator<T> is used as an allocator.
  Block(size_t n, T const &val) :
      allocator_p(get_allocator<typename DefaultAllocator<T>::type>()), used_p(
          n), destroyPointer(True), keep_allocator_p(False) {
    init(ArrayInitPolicies::NO_INIT);
    try {
      allocator_p->construct(array, get_size(), val);
    } catch (...) {
      dealloc();
      throw;
    }
  }

  // Create a Block of the given length, and initialize (via copy constructor for
  // objects of type T) with the provided value.
  template<typename Allocator>
  Block(size_t n, T const &val, AllocSpec<Allocator> const &) :
      allocator_p(get_allocator<typename Allocator::type>()), used_p(n), destroyPointer(
          True), keep_allocator_p(False) {
    init(ArrayInitPolicies::NO_INIT);
    try {
      allocator_p->construct(array, get_size(), val);
    } catch (...) {
      dealloc();
      throw;
    }
  }

  // Create a <src>Block</src> from a C-array (i.e. pointer). If 
  // <src>takeOverStorage</src> is <src>True</src>, The Block assumes that
  // it owns the pointer, i.e. that it is safe to release it via <src>allocator</src> when
  // the Block is destructed, otherwise the actual storage is not destroyed.
  // If true, <src>storagePointer</src> is set to <src>0</src>.
  // It is strongly recommended to supply an appropriate <src>allocator</src> argument explicitly
  // whenever <src>takeOverStorage</src> == True
  // to let <src>Block</src> to know how to release the <src>storagePointer</src>.
  // The default allocator set by this constructor will be changed from <src>NewDelAllocator<T>::value</src>
  // to <src>DefaultAllocator<T>::value</src> in future.
  Block(size_t n, T *&storagePointer, Bool takeOverStorage = True) :
      allocator_p(get_allocator<typename NewDelAllocator<T>::type>()), capacity_p(
          n), used_p(n), array(storagePointer), destroyPointer(takeOverStorage), keep_allocator_p(
          False) {
    if (destroyPointer)
      storagePointer = 0;
  }
  // Create a <src>Block</src> from a C-array (i.e. pointer). If
  // <src>takeOverStorage</src> is <src>True</src>, The Block assumes that
  // it owns the pointer, i.e. that it is safe to release it via <src>allocator</src> when
  // the Block is destructed, otherwise the actual storage is not destroyed.
  // If true, <src>storagePointer</src> is set to <src>0</src>.
  template<typename Allocator>
  Block(size_t n, T *&storagePointer, Bool takeOverStorage,
      AllocSpec<Allocator> const &) :
      allocator_p(get_allocator<typename Allocator::type>()), capacity_p(n), used_p(
          n), array(storagePointer), destroyPointer(takeOverStorage), keep_allocator_p(
          False) {
    if (destroyPointer)
      storagePointer = 0;
  }

  // Copy the other block into this one. Uses copy, not reference, semantics.
  Block(const Block<T> &other) :
      allocator_p(other.allocator_p), used_p(other.size()), destroyPointer(
          True), keep_allocator_p(False) {
    init(ArrayInitPolicies::NO_INIT);

    try {
      //objcopy(array, other.array, get_size());
      objthrowcp1(array, other.array, get_size());
      allocator_p->construct(array, get_size(), other.array);
    } catch (...) {
      dealloc();
      throw;
    }
  }
  
  // Assign other to this. this resizes itself to the size of other, so after
  // the assignment, this->nelements() == other.nelements() always.
  Block<T> &operator=(const Block<T> &other) {
    if (&other != this) {
      T *old = array;
      this->resize(other.size(), True, False, ArrayInitPolicies::NO_INIT);
      if (array == old) {
        objcopy(array, other.array, get_size());
      } else {
        objthrowcp1(array, other.array, get_size());
        allocator_p->construct(array, get_size(), other.array);
      }
    }
    return *this;
  }
  
  // Frees up the storage pointed contained in the Block.
  ~Block() {
    deinit();
  }

  // Resizes the Block. If <src>n == nelements()</src> resize just returns. If
  // a larger size is requested (<src>n > nelements()</src>) the Block always
  // resizes. If the requested size is smaller (<src>n < nelements()</src>),
  // by default the Block does not resize smaller, although it can be
  // forced to with <src>forceSmaller</src>. The reasoning behind this is that
  // often the user will just want a buffer of at least a certain size,
  // and won't want to pay the cost of multiple resizings.
  // <srcblock>
  // Block<float> bf(100, 0.0);
  // bf.resize(10);        // bf.nelements() == 100
  // bf.resize(10, True)   // bf.nelements() == 10
  // bf.resize(200)        // bf.nelements() == 200
  // </srcblock>
  // Normally the old elements are copied over (although if the
  // Block is lengthened the trailing elements will have undefined
  // values), however this can be turned off by setting copyElements to
  // False.
  //
  // This is written as three functions because default parameters do
  // not always work properly with templates.
  //
  // <src>initPolicy</src> makes sense to determine whether extended elements
  // should be initialized or not when you enlarge Block.
  // <group>
  void resize(size_t n, Bool forceSmaller = False, Bool copyElements = True) {
    resize(n, forceSmaller, copyElements,
        init_anyway() ? ArrayInitPolicies::INIT : ArrayInitPolicies::NO_INIT);
  }
  void resize(size_t n, Bool forceSmaller, Bool copyElements,
      ArrayInitPolicy initPolicy) {
    if (n == get_size()) {
      return;
    }
    if (n < get_size() && forceSmaller == False) {
      if (false) { // to keep get_size() == get_capacity()
        allocator_p->destroy(&array[n], get_size() - n);
        set_size(n);
      }
      return;
    }
    if (get_size() < n  && n <= get_capacity()) {
      allocator_p->construct(&array[get_size()], n - get_size());
      set_size(n);
      return;
    }
    T *tp = n > 0 ? allocator_p->allocate(n) : 0;
    traceAlloc(tp, n);
    if (n > 0) {
      size_t start = 0;
      if (copyElements) {
        size_t nmin = std::min(get_size(), n);  // Don't copy too much!
        if (nmin > 0) {
          try {
            allocator_p->construct(tp, nmin, array);
          } catch (...) {
            traceFree(tp, n);
            allocator_p->deallocate(tp, n);
            throw;
          }
        }
        start = nmin;
      }
      if (initPolicy == ArrayInitPolicies::INIT) {
        try {
          allocator_p->construct(&tp[start], n - start);
        } catch (...) {
          allocator_p->destroy(tp, start);
          traceFree(tp, n);
          allocator_p->deallocate(tp, n);
          throw;
        }
      }
    }
    deinit();
    destroyPointer = True;
    array = tp;                       // ... and update pointer
    set_capacity(n);
    set_size(n);
  }
  // </group>

  // Remove a single element from the Block. If forceSmaller is True this
  // will resize the Block and hence involve new memory allocations. This is
  // relatively expensive so setting forceSmaller to False is preferred. When
  // forceSmaller is False the Block is not resized but the elements with an
  // index above the removed element are shuffled down by one. For backward
  // compatibility forceSmaller is True by default.
  //
  // <src>initPolicy</src> makes sense to determine whether new storage
  // should be initialized or not before copying when <src>forceSmaller</src> is True.
  // <group>
  void remove(size_t whichOne, Bool forceSmaller = True) {
    remove(whichOne, forceSmaller,
        init_anyway() ? ArrayInitPolicies::INIT : ArrayInitPolicies::NO_INIT);
  }
  void remove(size_t whichOne, Bool forceSmaller, ArrayInitPolicy initPolicy) {
    if (whichOne >= get_size()) {
#if defined(AIPS_ARRAY_INDEX_CHECK)
      throw(indexError<uInt>(whichOne, "Block::remove() - "
			     "index out of range"));
#else
      return;
#endif
    }
    size_t n = get_size() - 1;
    if (forceSmaller == True) {
      T *tp = n > 0 ? allocator_p->allocate(n) : 0;
      traceAlloc(array, n);
      if (initPolicy == ArrayInitPolicies::INIT && n > 0) {
        try {
          allocator_p->construct(tp, n);
        } catch (...) {
          traceFree(tp, n);
          allocator_p->deallocate(tp, n);
          throw;
        }
      }
      try {
        objcopy(tp, array, whichOne);
      } catch (...) {
        traceFree(tp, n);
        allocator_p->deallocate(tp, n);
        throw;
      }
      try {
        objcopy(tp + whichOne, array + whichOne + 1, get_size() - whichOne - 1);
      } catch (...) {
        allocator_p->destroy(tp, whichOne);
        traceFree(tp, n);
        allocator_p->deallocate(tp, n);
        throw;
      }
      if (array && destroyPointer) {
        traceFree(array, get_capacity());
        allocator_p->destroy(array, get_size());
        allocator_p->deallocate(array, get_capacity());
        array = 0;
      };
      set_capacity(n);
      set_size(n);
      array = tp;
      destroyPointer = True;
    } else {
      objmove(&array[whichOne], &array[whichOne + 1], get_size() - whichOne - 1);
      if (false) { // to keep get_size() == get_capacity()
        allocator_p->destroy(&array[n], 1);
        set_size(n);
      }
    }
  }
  // </group>

  // Prohibit changing allocator for this instance.
  // <group>
  void prohibitChangingAllocator() {
    keep_allocator_p = True;
  }
  // Permit changing allocator for this instance.
  void permitChangingAllocator() {
    keep_allocator_p = False;
  }
  // </group>

  // Replace the internal storage with a C-array (i.e. pointer).
  // If <src>takeOverStorage</src> is True, The Block assumes that it
  // owns the pointer, i.e. that it is safe to release it via <src>allocator</src> when the
  // <src>Block</src>is destructed, otherwise the actual storage is not destroyed.
  // If true, storagePointer is set to <src>NULL</src>.
  // It is strongly recommended to supply an appropriate <src>allocator</src> argument explicitly
  // whenever <src>takeOverStorage</src> == True
  // to let <src>Block</src> to know how to release the <src>storagePointer</src>.
  // The default parameter of allocator will be changed from <src>AllocSpec<NewDelAllocator<T> >::value</src>
  // to <src>AllocSpec<DefaultAllocator<T> >::value</src> in future.
  // AipsError is thrown if allocator is incompatible with the current allocator of the instance and changing allocator is prohibited,
  // even if takeOverStorage == False.
  // <group>
  void replaceStorage(size_t n, T *&storagePointer, Bool takeOverStorage=True) {
    replaceStorage(n, storagePointer, takeOverStorage, AllocSpec<NewDelAllocator<T> >::value);
	}
  template<typename Allocator>
  void replaceStorage(size_t n, T *&storagePointer, Bool takeOverStorage, AllocSpec<Allocator> const &) {
    if (keep_allocator_p && ! isCompatibleAllocator<Allocator>()) {
      throw AipsError("Block::replaceStorage - Attemption to change allocator of Block");
    }

    if (array && destroyPointer) {
      traceFree (array, get_capacity());
      allocator_p->destroy(array, get_size());
      allocator_p->deallocate(array, get_capacity());
      array = 0;
    };
    set_capacity(n);
    set_size(n);
    allocator_p = get_allocator<typename Allocator::type>();
    array = storagePointer;
    destroyPointer = takeOverStorage;
    if (destroyPointer) storagePointer = 0;
  }
  // </group>

  // Index into the block (0-based). If the preprocessor symbol
  // <src>AIPS_ARRAY_INDEX_CHECK</src> is defined, index checking will be done
  // and an out-of-bounds index will cause an <src>indexError<uInt></src> to be
  // thrown. Note that valid indices range between 0 and <src>nelements()-1</src>.
  // <thrown>
  //    <li> indexError
  // </thrown>
  // <group>
  T &operator[](size_t index) {
#if defined(AIPS_ARRAY_INDEX_CHECK)
    // Write it this way to avoid casts; remember index and get_size() are
    // unsigned.
    if ((get_size() == 0) || (index > get_size() - 1)) {
      throw(indexError<uInt>(index, "Block::operator[] - "
			     "index out of range"));
    };
#endif
    return array[index];
  }
  const T &operator[](size_t index) const {
#if defined(AIPS_ARRAY_INDEX_CHECK)
    if ((get_size() == 0) || (index > get_size() - 1)) {
      throw(indexError<uInt>(index, "Block::operator[] const - "
			     "index out of range"));
    };
#endif
    return array[index];
  }
  // </group>
  
  // Set all values in the block to "val".
  // <group>
  Block<T> &operator=(const T &val)
    { T tmp=val; objset(array, tmp, get_size()); return *this;}
  void set(const T &val) { *this = val; }
  // </group>

  // If you really, really, need a "raw" pointer to the beginning of the
  // storage area this will give it to you. This may leave dangling pointers
  // if the block is destructed or if the assignment operator or resize 
  // is used. Returns a null pointer if <src>nelements() == 0</src>.
  // It is best to only use this if you completely control the extent and
  // lifetime of the <src>Block</src>.
  // <h3> Examples of misuse </h3> <srcblock>
  // Block<Int> *bp = new Block<Int>(100);
  // Int *ip = bp->storage();
  // DefaultAllocator<Int>::value.deallocate(bp, bp->capacity());   // Oops, ip is now dangling
  // Block<Int> a(100),b(100);
  // Int *ip = a.storage();
  // a = b;          // Likewise
  // </srcblock>
  // <group>
  T *storage() {return array;}
  const T *storage() const {return array;}
  // </group>

  // The number of elements contained in this <src>Block<T></src>.
  // <group>
  size_t nelements() const {return size();}
  size_t size() const {return get_capacity();}
  // </group>

  // The capacity in this <src>Block<T></src>.
  // <src>size() <= capacity()</src> is always true.
  size_t capacity() const {return get_capacity();}

  // Is the block empty (i.e. no elements)?
  Bool empty() const {return size() == 0;}

  // Define the STL-style iterators.
  // It makes it possible to iterate through all data elements.
  // <srcblock>
  //  Block<Int> bl(100,0);
  //  for (Block<Int>::iterator iter=bl.begin(); iter!=bl.end(); iter++) {
  //    *iter += 1;
  //  }
  // </srcblock>
  // <group name=STL-iterator>
  // STL-style typedefs.
  // <group>
  typedef T                 value_type;
  typedef T*                iterator;
  typedef const T*          const_iterator;
  typedef value_type*       pointer;
  typedef const value_type* const_pointer; 
  typedef value_type&       reference;
  typedef const value_type& const_reference;
  typedef size_t            size_type;
  typedef ptrdiff_t         difference_type;
  // </group>
  // Get the begin and end iterator object for this block.
  // <group>
  iterator begin()
    { return array; }
  const_iterator begin() const
    { return array; }
  iterator end()
    { return array + size(); }
  const_iterator end() const
    { return array + size(); }
  // </group>
  // </group>

  inline void traceAlloc (const void* addr, size_t sz) const
  {
    if (itsTraceSize>0 && sz>=itsTraceSize) {
      doTraceAlloc (addr, sz, whatType(static_cast<T*>(0)), sizeof(T));
    }
  }
  inline void traceFree (const void* addr, size_t sz) const
  {
    if (itsTraceSize>0 && sz>=itsTraceSize) {
      doTraceFree (addr, sz, whatType(static_cast<T*>(0)), sizeof(T));
    }
  }

 private:
  friend class Array<T>; // to allow access to following constructors.

  Block(size_t n, ArrayInitPolicy initPolicy,
          Allocator_private::BulkAllocator<T> *allocator) :
      allocator_p(allocator), used_p(n), destroyPointer(
          True), keep_allocator_p(False) {
    init(initPolicy);
  }
  Block(size_t n, Allocator_private::AllocSpec<T> allocator) :
      allocator_p(allocator.allocator), used_p(n), destroyPointer(
          True), keep_allocator_p(False) {
    init(init_anyway() ? ArrayInitPolicies::INIT : ArrayInitPolicies::NO_INIT);
  }
  Block(size_t n, T *&storagePointer, Bool takeOverStorage,
          Allocator_private::BulkAllocator<T> *allocator) :
      allocator_p(allocator), capacity_p(n), used_p(
          n), array(storagePointer), destroyPointer(takeOverStorage), keep_allocator_p(
          False) {
    if (destroyPointer)
      storagePointer = 0;
  }
  void construct(size_t pos, size_t n, T const *src) {
    allocator_p->construct(&array[pos], n, src);
  }
  void construct(size_t pos, size_t n,
      T const &initial_value) {
    allocator_p->construct(&array[pos], n, initial_value);
  }
  void construct(size_t pos, size_type n) {
    allocator_p->construct(&array[pos], n);
  }
  void destroy(size_t pos, size_type n) {
    allocator_p->destroy(&array[pos], n);
  }
  Allocator_private::BulkAllocator<T> *get_allocator(){
      return allocator_p;
  }

  static bool init_anyway() {
     return !(Block_internal_IsFundamental<T>::value
         || Block_internal_IsPointer<T>::value);
   }

  // end of friend

  void init(ArrayInitPolicy initPolicy) {
    set_capacity(get_size());
    if (get_capacity() > 0) {
      array = allocator_p->allocate(get_capacity());
      traceAlloc(array, get_capacity());
      if (initPolicy == ArrayInitPolicies::INIT) {
        try {
          allocator_p->construct(array, get_size());
        } catch (...) {
          dealloc();
          throw;
        }
      }
    } else {
      array = 0;
    }
  }

  void deinit() {
    if (array && destroyPointer) {
      allocator_p->destroy(array, get_size());
      dealloc();
    }
  }
  void dealloc() {
    if (array && destroyPointer) {
      traceFree(array, get_capacity());
      allocator_p->deallocate(array, get_capacity());
      array = 0;
    }
  }

  template<typename Allocator>
  static typename Allocator_private::BulkAllocator<
      typename Allocator::value_type> *get_allocator() {
    return Allocator_private::get_allocator<
        Allocator>();
  }

  template<typename Allocator>
  Bool isCompatibleAllocator() {
    typename Allocator_private::BulkAllocator<
        typename Allocator::type::value_type> *other_allocator =
                Allocator_private::get_allocator<typename Allocator::type>();
    return other_allocator == allocator_p;
  }

  // The number of used elements in the vector
  size_t get_size() const { return used_p;}
  // Set the number of used elements in the vector
  void set_size(size_t new_value) {
    AlwaysAssert(new_value <= get_capacity(), AipsError);
    used_p = new_value;
  }
  // The capacity of the vector
  size_t get_capacity() const { return capacity_p;}
  // Set the capacity of the vector
  void set_capacity(size_t new_value) {
    capacity_p = new_value;
    set_size(std::min(get_size(), capacity_p));
  }

  // The allocator
  typename Allocator_private::BulkAllocator<T> *allocator_p;
  // The capacity of the vector
  size_t capacity_p;
  // The number of used elements in the vector
  size_t used_p;
  // The actual storage
  T *array;
  // Can we delete the storage upon destruction?
  Bool destroyPointer;
  // Can we change allocator or not?
  Bool keep_allocator_p;
};


// <summary>
// A drop-in replacement for <src>Block<T*></src>.
// </summary>
 
// <use visibility=export>
// <prerequisite>
//   <li> <linkto class=Block>Block</linkto>
// </prerequisite>
 
// <synopsis>
// <src>PtrBlock<T*></src> has exactly the same interface as <src>Block<T*></src>
// and should be used in preference to the latter. It's purpose is solely to
// reduce the number of template instantiations.
// </synopsis>
 
// <todo asof="1996/05/01">
//   <li> Partial template specialization is another implementation choice that 
//        will be possible eventually.
//   <li> It might be useful to have functions that know the template parameter
//        is a pointer, e.g. that delete all the non-null pointers.
// </todo>
 
 template<class T> class PtrBlock {
 public:
   PtrBlock() : block_p() {}
   explicit PtrBlock(size_t n) : block_p(n) {}
   PtrBlock(size_t n, T val) : block_p(n, (void *)val) {}
   PtrBlock(size_t n, T *&storagePointer, Bool takeOverStorage = True)
     : block_p(n, (void **&)storagePointer, takeOverStorage) {}
   PtrBlock(const PtrBlock<T> &other) : block_p(other.block_p) {}
   PtrBlock<T> &operator=(const PtrBlock<T> &other)
     { block_p = other.block_p; return *this;}
   ~PtrBlock() {}
   void resize(size_t n, Bool forceSmaller, Bool copyElements)
     { block_p.resize(n,forceSmaller, copyElements); }
   void resize(size_t n) {block_p.resize(n);}
   void resize(size_t n, Bool forceSmaller) {block_p.resize(n, forceSmaller);}
   void remove(size_t whichOne, Bool forceSmaller) {
     block_p.remove(whichOne, forceSmaller);}
   void remove(size_t whichOne) {block_p.remove(whichOne);}
   void replaceStorage(size_t n, T *&storagePointer, 
		       Bool takeOverStorage=True)
     {block_p.replaceStorage(n, (void **&)storagePointer, takeOverStorage);}
   T &operator[](size_t index) {return (T &)block_p[index];}
   const T &operator[](size_t index) const {return (const T &)block_p[index];}
   void set(const T &val) {block_p.set((void *const &)val);}
   PtrBlock<T> &operator=(const T &val) {set(val); return *this;}
   T *storage()  {return (T *)block_p.storage();}
   const T *storage() const {return (const T *)block_p.storage();}
   size_t nelements() const {return block_p.nelements();}
   size_t size() const {return block_p.size();}
   Bool empty() const {return block_p.empty();}
 private:
   Block<void*> block_p;
 };


//# Instantiate extern templates for often used types.
  extern template class Block<Bool>;
  extern template class Block<Char>;
  extern template class Block<Short>;
  extern template class Block<uShort>;
  extern template class Block<Int>;
  extern template class Block<uInt>;
  extern template class Block<Int64>;
  extern template class Block<Float>;
  extern template class Block<Double>;
  extern template class Block<Complex>;
  extern template class Block<DComplex>;
  extern template class Block<String>;
  extern template class Block<void*>;


} //# NAMESPACE CASACORE - END

#endif
