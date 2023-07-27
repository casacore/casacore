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

#ifndef CASA_CONTAINERS_ALLOCATOR_H_
#define CASA_CONTAINERS_ALLOCATOR_H_

#include <casacore/casa/config.h>
#include <casacore/casa/aips.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/casa/Arrays/ArrayFwd.h>

#include <cstdlib>
#include <memory>
#include <new>
#include <typeinfo>
#include <type_traits>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

#ifndef CASA_DEFAULT_ALIGNMENT
# define CASA_DEFAULT_ALIGNMENT (32UL) // AVX/AVX2 alignment
#endif

// <summary>
// A global enum used by some Array/Block constructors.
// </summary>
// <synopsis>
// ArrayInitPolicy is used in functions where an array is allocated/resized.
// </synopsis>
class ArrayInitPolicy {
public:
  Bool operator ==(ArrayInitPolicy const &other) {
    return init == other.init;
  }
  Bool operator !=(ArrayInitPolicy const &other) {
    return init != other.init;
  }
private:
  Bool init;
  explicit constexpr ArrayInitPolicy(bool v): init(v) {}
  friend struct ArrayInitPolicies;
};

struct ArrayInitPolicies {
    // Don't initialize elements in the array. (The array will be explicitly filled with values other than the default value.)
    static constexpr ArrayInitPolicy NO_INIT = ArrayInitPolicy(false);
    // Initialize all elements in the array with the default value.
    static constexpr ArrayInitPolicy INIT = ArrayInitPolicy(true);
};

template<typename T>
using std11_allocator = std::allocator<T>;


template<typename T, size_t ALIGNMENT = CASA_DEFAULT_ALIGNMENT>
struct casacore_allocator: public std11_allocator<T> {
  typedef std11_allocator<T> Super;
  using size_type = typename Super::size_type;
  using difference_type = typename Super::difference_type;
  using pointer = T*;
  using const_pointer = const T*;
  using reference = T&;
  using const_reference = const T&;
  using value_type = typename Super::value_type;

  static constexpr size_t alignment = ALIGNMENT;

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
    if (elements > std::allocator_traits<casacore_allocator>::max_size(*this)) {
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
struct new_del_allocator: public std11_allocator<T> {
  using Super = std11_allocator<T>;
  using size_type = typename Super::size_type;
  using difference_type = typename Super::difference_type;
  using pointer = T*;
  using const_pointer = const T*;
  using reference = T&;
  using const_reference = const T&;
  using value_type = typename Super::value_type;

  template<typename TOther>
  struct rebind {
    typedef new_del_allocator<TOther> other;
  };
  new_del_allocator() noexcept {
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
    if (elements > std::allocator_traits<new_del_allocator>::max_size(*this)) {
      throw std::bad_alloc();
    }
    return new T[elements];
  }

  void deallocate(pointer ptr, size_type) {
    delete[] ptr;
  }
  template<typename U, typename... Args>
  void construct(U *, Args&&... ) {} // do nothing because new T[] does
  template<typename U>
  void construct(U *ptr, U &&value) {
      *ptr = value;     // because *ptr was already contructed by new[].
  }
  template<typename U>
  void construct(U *ptr, U &value) {
      *ptr = value;     // because *ptr was already contructed by new[].
  }
  template<typename U>
  void construct(U *ptr, U const &value) {
      *ptr = value;     // because *ptr was already contructed by new[].
  }

  template<typename U>
  void destroy(U *) {} // do nothing because delete[] will do.
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

template<typename T> class Block;

class Allocator_private {
  template<typename T> friend class AbstractAllocator;
  template<typename T, typename Sub> friend class BaseAllocator;
  template<typename T> friend class Block;

  template<typename T2>
  struct BulkAllocator {
    using size_type = typename std::allocator<T2>::size_type;
    using value_type = typename std::allocator<T2>::value_type;
    using pointer = T2*;
    using const_pointer = const T2*;

    virtual pointer allocate(size_type elements, const void*ptr = 0) = 0;
    virtual void deallocate(pointer ptr, size_type size) = 0;

    virtual void construct(pointer ptr, size_type n, const_pointer src) = 0;
    virtual void construct(pointer ptr, size_type n, value_type const &initial_value) = 0;
    virtual void construct(pointer ptr, size_type n) = 0;
    virtual void destroy(pointer ptr, size_type n) = 0;
    virtual std::type_info const &allocator_typeid() const = 0;
    virtual ~BulkAllocator() {}
  };

  template<typename Allocator>
  struct BulkAllocatorImpl: public BulkAllocator<typename Allocator::value_type> {
    typedef typename Allocator::size_type size_type;
    typedef typename Allocator::pointer pointer;
    typedef typename Allocator::const_pointer const_pointer;
    typedef typename Allocator::value_type value_type;
    virtual pointer allocate(size_type elements, const void *ptr = 0) override {
      return allocator.allocate(elements, ptr);
    }
    virtual void deallocate(pointer ptr, size_type size) override {
      allocator.deallocate(ptr, size);
    }

    virtual void construct(pointer ptr, size_type n, const_pointer src) override {
      size_type i = 0;
      try {
        for (i = 0; i < n; ++i) {
          ::new(&ptr[i]) value_type(src[i]);
        }
      } catch (...) {
        destroy(ptr, i);  // rollback constructions
        throw;
      }
    }
    virtual void construct(pointer ptr, size_type n,
        value_type const &initial_value) override {
      size_type i = 0;
      try {
        for (i = 0; i < n; ++i) {
          ::new(&ptr[i]) value_type(initial_value);
        }
      } catch (...) {
        destroy(ptr, i);  // rollback constructions
        throw;
      }
    }
    virtual void construct(pointer ptr, size_type n) override {
      size_type i = 0;
      try {
        for (i = 0; i < n; ++i) {
          ::new(&ptr[i]) value_type();
        }
      } catch (...) {
        destroy(ptr, i);  // rollback constructions
        throw;
      }
    }
    virtual void destroy(pointer ptr, size_type n) override {
      for (size_type i = n; i > 0;) {
        --i;
        try {
          ptr[i].~value_type();
        } catch (...) {
          // Destructor should not raise any exception.
        }
      }
    }
    virtual std::type_info const &allocator_typeid() const override {
      return typeid(Allocator);
    }
    virtual ~BulkAllocatorImpl() override {}

  private:
    static Allocator allocator;
  };

  template<typename Allocator>
  static BulkAllocator<typename Allocator::value_type> *get_allocator() {
    return get_allocator_raw<Allocator>();
  }

  template<typename Allocator>
  static BulkAllocatorImpl<Allocator> *get_allocator_raw() {
    // Because this function gets called from destructors of statically allocated objects that get destructed
    // after the program finishes, the allocator is constructed in a static storage space and is never
    // destructed.
    static typename std::aligned_storage<sizeof(BulkAllocatorImpl<Allocator>), alignof(BulkAllocatorImpl<Allocator>)>::type storage;
    static BulkAllocatorImpl<Allocator>* ptr =
      new (reinterpret_cast<BulkAllocatorImpl<Allocator>*>(&storage)) BulkAllocatorImpl<Allocator>();
    return ptr;
  }

  // <summary>Allocator specifier</summary>
  // <synopsis>
  // This class is just used to avoid ambiguity between overloaded functions.
  // </synopsis>
  template<typename T>
  struct AllocSpec {
      BulkAllocator<T> *allocator;
      explicit AllocSpec(BulkAllocator<T> *alloc) : allocator(alloc) {}
  };
};

template<typename Allocator>
Allocator Allocator_private::BulkAllocatorImpl<Allocator>::allocator;

template<typename T>
class AbstractAllocator {
public:
  typedef T value_type;
  virtual ~AbstractAllocator(){}
protected:
  AbstractAllocator(){}
  friend class Array<T>;
  friend class Block<T>;

  virtual Allocator_private::BulkAllocator<T> *getAllocator() const = 0;
};

template<typename T, typename Sub>
class BaseAllocator: public AbstractAllocator<T> {
public:
  typedef T value_type;
  typedef Sub facade_type;
  virtual ~BaseAllocator() {}
protected:
  BaseAllocator() {}

  virtual typename Allocator_private::BulkAllocator<T> *getAllocator() const override {
    return Allocator_private::get_allocator<typename facade_type::type>();
  }
};

// An allocator behaves like operator new[]/delete[].
// Because it is impossible to decouple construction/destruction from allocation/deallocation with this allocator,
// it is discouraged to use this allocator.
// Use <src>DefaultAllocator<T></src> or <src>AlignedAllocator<T, ALIGNMENT></src> as possible.
// This allocator is provided only for compatibility for calling
// <src>Array::takeStorage(), Block::replaceStorage(), Block(size_t, T *&, Bool)</src>  etc.
// with a storage allocated by operator new[].
template<typename T>
class NewDelAllocator: public BaseAllocator<T, NewDelAllocator<T> > {
public:
  typedef new_del_allocator<T> type;
  // an instance of this allocator.
  static NewDelAllocator<T> value;
protected:
  NewDelAllocator(){}
};
template<typename T>
NewDelAllocator<T> NewDelAllocator<T>::value;

// An allocator which allocates aligned memory.
template<typename T, size_t ALIGNMENT = CASA_DEFAULT_ALIGNMENT>
class AlignedAllocator: public BaseAllocator<T, AlignedAllocator<T, ALIGNMENT> > {
public:
  typedef casacore_allocator<T, ALIGNMENT> type;
  // an instance of this allocator.
  static AlignedAllocator<T, ALIGNMENT> value;
protected:
  AlignedAllocator(){}
};
template<typename T, size_t ALIGNMENT>
AlignedAllocator<T, ALIGNMENT> AlignedAllocator<T, ALIGNMENT>::value;

// An aligned allocator with the default alignment.
template<typename T>
class DefaultAllocator: public AlignedAllocator<T> {
public:
  typedef typename AlignedAllocator<T>::type type;
  // an instance of this allocator.
  static DefaultAllocator<T> value;
protected:
  DefaultAllocator(){}
};
template<typename T>
DefaultAllocator<T> DefaultAllocator<T>::value;

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


} //# NAMESPACE CASACORE - END

#endif /* CASA_CONTAINERS_ALLOCATOR_H_ */
