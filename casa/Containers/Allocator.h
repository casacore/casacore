//# Allocator.h:
//# Copyright (C) 2015
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

#ifndef CASA_CONTAINERS_ALLOCATOR_H_
#define CASA_CONTAINERS_ALLOCATOR_H_

#include <casacore/casa/aips.h>
#include <casacore/casa/Utilities/DataType.h>

#include <memory>
#include <typeinfo>

#include <cstdlib>
namespace casacore { //# NAMESPACE CASACORE - BEGIN

#if __cplusplus < 201103L && ! defined(noexcept)
# define noexcept throw()
# define CASA_UNDEF_noexcept
#endif

#ifndef CASA_DEFAULT_ALIGNMENT
# define CASA_DEFAULT_ALIGNMENT (16UL) // same alignment as malloc
#endif

// <summary>
// A global enum used by some Array/Block constructors.
// </summary>
// <synopsis>
// ArrayInitPolicy is used in functions where an array is allocated/resized.
// </synopsis>
class ArrayInitPolicy {
public:
  // Don't initialize elements in the array. (The array will be explicitly filled with values other than the default value.)
  static ArrayInitPolicy const NO_INIT;
  // Initialize all elements in the array with the default value.
  static ArrayInitPolicy const INIT;
  Bool operator ==(ArrayInitPolicy const &other) {
    return init == other.init;
  }
  Bool operator !=(ArrayInitPolicy const &other) {
    return init != other.init;
  }
private:
  Bool init;
  explicit ArrayInitPolicy(bool v): init(v) {}
};

template<typename T>
struct IsArrayInitPolicy;

template<>
struct IsArrayInitPolicy<ArrayInitPolicy> {
  typedef ArrayInitPolicy type;
};

template<typename T, size_t ALIGNMENT = CASA_DEFAULT_ALIGNMENT>
struct casacore_allocator: public std::allocator<T> {
  typedef std::allocator<T> Super;
  typedef typename Super::size_type size_type;
  typedef typename Super::difference_type difference_type;
  typedef typename Super::pointer pointer;
  typedef typename Super::const_pointer const_pointer;
  typedef typename Super::reference reference;
  typedef typename Super::const_reference const_reference;
  typedef typename Super::value_type value_type;
  enum {alignment = ALIGNMENT};

  template<typename TOther>
  struct rebind {
    typedef casacore_allocator<TOther> other;
  };
  casacore_allocator() throw () {
  }

  casacore_allocator(const casacore_allocator&other) noexcept
  :Super(other) {
  }

  template<typename TOther>
  casacore_allocator(const casacore_allocator<TOther>&) noexcept {
  }

  ~casacore_allocator() noexcept {
  }

  pointer allocate(size_type elements, const void* = 0) {
    if (elements > this->max_size()) {
      throw std::bad_alloc();
    }
    void *memptr = 0;
    int result = posix_memalign(&memptr, ALIGNMENT, sizeof(T) * elements);
    if (result != 0) {
      throw std::bad_alloc();
    }
    return static_cast<pointer>(memptr);
  }

  void deallocate(pointer ptr, size_type) {
    free(ptr);
  }
};

template<typename T, size_t ALIGNMENT>
inline bool operator==(const casacore_allocator<T, ALIGNMENT>&,
    const casacore_allocator<T, ALIGNMENT>&) {
  return true;
}

template<typename T, size_t ALIGNMENT>
inline bool operator!=(const casacore_allocator<T, ALIGNMENT>&,
    const casacore_allocator<T, ALIGNMENT>&) {
  return false;
}

template<typename T>
struct new_del_allocator: public std::allocator<T> {
  typedef std::allocator<T> Super;
  typedef typename Super::size_type size_type;
  typedef typename Super::difference_type difference_type;
  typedef typename Super::pointer pointer;
  typedef typename Super::const_pointer const_pointer;
  typedef typename Super::reference reference;
  typedef typename Super::const_reference const_reference;
  typedef typename Super::value_type value_type;

  template<typename TOther>
  struct rebind {
    typedef new_del_allocator<TOther> other;
  };
  new_del_allocator() throw () {
  }

  new_del_allocator(const new_del_allocator&other) noexcept
  :Super(other) {
  }

  template<typename TOther>
  new_del_allocator(const new_del_allocator<TOther>&) noexcept {
  }

  ~new_del_allocator() noexcept {
  }

  pointer allocate(size_type elements, const void* = 0) {
    if (elements > this->max_size()) {
      throw std::bad_alloc();
    }
    return new T[elements];
  }

  void deallocate(pointer ptr, size_type) {
    delete[] ptr;
  }
#if __cplusplus < 201103L
    void construct(pointer , const_reference) {} // do nothing because new T[] does
#else
  template<typename U, typename... Args>
  void construct(U *, Args&&... ) {} // do nothing because new T[] does
#endif

  template<typename U>
  void destroy(U *) {} // do nothing because delete[] does
};

template<typename T>
inline bool operator==(const new_del_allocator<T>&,
    const new_del_allocator<T>&) {
  return true;
}

template<typename T>
inline bool operator!=(const new_del_allocator<T>&,
    const new_del_allocator<T>&) {
  return false;
}

template<typename T, typename Sub>
struct BaseAllocator {
  typedef T value_type;
  typedef Sub facade_type;
};

template<typename T>
struct DefaultAllocator: public BaseAllocator<T, DefaultAllocator<T> > {
  typedef std::allocator<T> type;
  static DefaultAllocator<T> const value;
};
template<typename T>
DefaultAllocator<T> const DefaultAllocator<T>::value = DefaultAllocator<T>();

template<typename T>
struct NewDelAllocator: BaseAllocator<T, NewDelAllocator<T> > {
  typedef new_del_allocator<T> type;
  static NewDelAllocator<T> const value;
};
template<typename T>
NewDelAllocator<T> const NewDelAllocator<T>::value = NewDelAllocator<T>();


template<typename T, size_t ALIGNMENT = CASA_DEFAULT_ALIGNMENT>
struct AlignedAllocator: BaseAllocator<T, AlignedAllocator<T, ALIGNMENT> > {
  typedef casacore_allocator<T, ALIGNMENT> type;
  static AlignedAllocator<T, ALIGNMENT> const value;
};
template<typename T, size_t ALIGNMENT>
AlignedAllocator<T, ALIGNMENT> const AlignedAllocator<T, ALIGNMENT>::value = AlignedAllocator<T, ALIGNMENT>();

// <summary>Allocator specifier</summary>
// <synopsis>
// This class is just used to avoid ambiguity between overloaded functions.
// </synopsis>
template<typename T>
struct AllocSpec {
  typedef T type;
  static AllocSpec<T> const value;
};
template<typename T>
AllocSpec<T> const AllocSpec<T>::value = AllocSpec<T>();

#if defined(CASA_UNDEF_noexcept)
# undef noexcept
# undef CASA_UNDEF_noexcept
#endif

} //# NAMESPACE CASACORE - END

#endif /* CASA_CONTAINERS_ALLOCATOR_H_ */
