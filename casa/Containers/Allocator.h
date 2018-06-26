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

#include <casacore/casa/config.h>
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

#if __cplusplus < 201103L
template<typename T>
struct std11_allocator: public std::allocator<T> {
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
    typedef std11_allocator<TOther> other;
  };
  void construct(pointer ptr) {
    ::new(static_cast<void *>(ptr)) T();
  }
  void construct(pointer ptr, const_reference val) {
    Super::construct(ptr, val);
  }
};

template<typename T>
inline bool operator==(const std11_allocator<T>&,
    const std11_allocator<T>&) {
  return true;
}

template<typename T>
inline bool operator!=(const std11_allocator<T>&,
    const std11_allocator<T>&) {
  return false;
}

#else
template<typename T>
using std11_allocator = std::allocator<T>;
#endif

template<typename T, size_t ALIGNMENT = CASA_DEFAULT_ALIGNMENT>
struct casacore_allocator: public std11_allocator<T> {
  typedef std11_allocator<T> Super;
  typedef typename Super::size_type size_type;
  typedef typename Super::difference_type difference_type;
  typedef typename Super::pointer pointer;
  typedef typename Super::const_pointer const_pointer;
  typedef typename Super::reference reference;
  typedef typename Super::const_reference const_reference;
  typedef typename Super::value_type value_type;
#if __cplusplus < 201103L
  enum {alignment = ALIGNMENT};
#else
  static constexpr size_t alignment = ALIGNMENT;
#endif

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
    void *memptr = 0;
    try {
        if (elements > this->max_size()) {
            throw std::bad_alloc();
        }
        int result = posix_memalign(&memptr, ALIGNMENT, sizeof(T) * elements);
        if (result != 0) {
            throw std::bad_alloc();
        }
    } 
    catch (const std::bad_alloc& ba) {
        std::cerr << "Couldn't allocate memory. Caught bad_alloc: " << ba.what() <<  std::endl;
        exit (EXIT_FAILURE);
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
  typedef std11_allocator<T> Super;
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
    if (elements > this->max_size()) {
      throw std::bad_alloc();
    }
    return new T[elements];
  }

  void deallocate(pointer ptr, size_type) {
    delete[] ptr;
  }
#if __cplusplus < 201103L
    void construct(pointer ptr, const_reference value) {
        *ptr = value;   // because *ptr was already contructed by new[].
    }
    void construct(pointer) {} // do nothing because new T[] does
#else
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
#endif

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

template<typename T> class Array;
template<typename T> class Block;

class Allocator_private {
  template<typename T> friend class AbstractAllocator;
  template<typename T, typename Sub> friend class BaseAllocator;
  template<typename T> friend class Array;
  template<typename T> friend class Block;

  template<typename T2>
  struct BulkAllocator {
    typedef typename std::allocator<T2>::size_type size_type;
    typedef typename std::allocator<T2>::pointer pointer;
    typedef typename std::allocator<T2>::const_pointer const_pointer;
    typedef typename std::allocator<T2>::value_type value_type;

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
    virtual pointer allocate(size_type elements, const void *ptr = 0) {
      return allocator.allocate(elements, ptr);
    }
    virtual void deallocate(pointer ptr, size_type size) {
      allocator.deallocate(ptr, size);
    }

    virtual void construct(pointer ptr, size_type n, const_pointer src) {
      size_type i = 0;
      try {
        for (i = 0; i < n; ++i) {
          allocator.construct(&ptr[i], src[i]);
        }
      } catch (...) {
        destroy(ptr, i);  // rollback constructions
        throw;
      }
    }
    virtual void construct(pointer ptr, size_type n,
        value_type const &initial_value) {
      size_type i = 0;
      try {
        for (i = 0; i < n; ++i) {
          allocator.construct(&ptr[i], initial_value);
        }
      } catch (...) {
        destroy(ptr, i);  // rollback constructions
        throw;
      }
    }
    virtual void construct(pointer ptr, size_type n) {
      size_type i = 0;
      try {
        for (i = 0; i < n; ++i) {
          allocator.construct(&ptr[i]);
        }
      } catch (...) {
        destroy(ptr, i);  // rollback constructions
        throw;
      }
    }
    virtual void destroy(pointer ptr, size_type n) {
      for (size_type i = n; i > 0;) {
        --i;
        try {
          allocator.destroy(&ptr[i]);
        } catch (...) {
          // Destructor should not raise any exception.
        }
      }
    }
    virtual std::type_info const &allocator_typeid() const {
      return typeid(Allocator);
    }
    virtual ~BulkAllocatorImpl() {}

  private:
    static Allocator allocator;
  };

  template<typename Allocator>
  static BulkAllocator<typename Allocator::value_type> *get_allocator() {
    return get_allocator_raw<Allocator>();
  }
  template<typename Allocator>
  class BulkAllocatorInitializer {
    BulkAllocatorInitializer() {
      get_allocator_raw<Allocator>();
    }
    static BulkAllocatorInitializer<Allocator> instance;
  };
  template<typename Allocator>
  static BulkAllocatorImpl<Allocator> *get_allocator_raw() {
    static union {
      void *dummy;
      char alloc_obj[sizeof(BulkAllocatorImpl<Allocator> )];
    } u;
    static BulkAllocatorImpl<Allocator> *ptr = 0;
    // Probably this method is called from BulkAllocatorInitializer<Allocator> first
    // while static initialization
    // and other threads are not started yet.
    if (ptr == 0) {
      // Use construct below to avoid https://gcc.gnu.org/bugzilla/show_bug.cgi?id=42032 
      ::new (reinterpret_cast<BulkAllocatorImpl<Allocator>*>(u.alloc_obj)) BulkAllocatorImpl<Allocator>(); // this instance will never be destructed.
      //      ::new (u.alloc_obj) BulkAllocatorImpl<Allocator>(); // this instance will never be destructed.
      ptr = reinterpret_cast<BulkAllocatorImpl<Allocator> *>(u.alloc_obj);
    }
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

template<typename Allocator>
Allocator_private::BulkAllocatorInitializer<Allocator> Allocator_private::BulkAllocatorInitializer<Allocator>::instance;

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

  virtual typename Allocator_private::BulkAllocator<T> *getAllocator() const {
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

#if defined(CASA_UNDEF_noexcept)
# undef noexcept
# undef CASA_UNDEF_noexcept
#endif

} //# NAMESPACE CASACORE - END

#endif /* CASA_CONTAINERS_ALLOCATOR_H_ */
