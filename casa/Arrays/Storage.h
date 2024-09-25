#ifndef CASACORE_STORAGE_2_H
#define CASACORE_STORAGE_2_H

#include <cstring>
#include <memory>
  
namespace casacore {

namespace arrays_internal {
  
// This class emplements a static (but run-time) sized array. It is used in the
// Array class, and is necessary because std::vector specializes for bool.
// It holds the same functionality as a normal array, and enables allocation
// through different allocators similar to std::vector.
template<typename T>
class Storage
{
public:
  // Construct an empty Storage
  Storage() :
    _data(nullptr),
    _end(nullptr),
    _isShared(false)
  { }
  
  // Construct Storage with a given size.
  // The elements will be default constructed
  Storage(std::size_t n) :
    _data(construct(n)),
    _end(_data + n),
    _isShared(false)
  { }
  
  // Construct Storage with a given size.
  // The elements will be copy constructed from the given value
  Storage(std::size_t n, const T& val) :
    _data(construct(n, val)),
    _end(_data + n),
    _isShared(false)
  { }
  
  // Construct Storage from a range.
  // The elements will be copy constructed from the given values.
  // Note that this constructor can be chosen over
  // of Storage(std::size_t, const T&, const Alloc) when T=size_t. Therefore,
  // this constructor forwards to the appropriate constructor based on
  // whether T is an integral.
  template<typename InputIterator>
  Storage(InputIterator startIter, InputIterator endIter) :
    Storage(startIter, endIter,
      disjunction<
        std::is_integral<InputIterator>,
        conjunction<
          std::is_same<InputIterator, const char*>,
          std::is_base_of<std::string, T>
        >
      >())
  { }
  
  // Construct Storage from a range by moving.
  // The elements will be move constructed from the given values.
  static std::unique_ptr<Storage<T>> MakeFromMove(T* startIter, T* endIter)
  {
    return std::unique_ptr<Storage<T>>(new Storage(startIter, endIter, std::false_type(), std::true_type()));
  }
  
  // Construct a Storage from existing data.
  // The given pointer will not be owned by this class.
  static std::unique_ptr<Storage<T>> MakeFromSharedData(T* existingData, size_t n)
  {
    std::unique_ptr<Storage<T>> newStorage = std::unique_ptr<Storage>(new Storage<T>());
    newStorage->_data = existingData;
    newStorage->_end = existingData + n;
    newStorage->_isShared = true;
    return newStorage;
  }
  
  // Construct a Storage with uninitialized data.
  // This will skip the constructor of the elements. This is only allowed for
  // trivial types.
  static std::unique_ptr<Storage<T>> MakeUninitialized(size_t n)
  {
    static_assert(std::is_trivial<T>::value, "Only trivial types can be constructed uninitialized");
    std::unique_ptr<Storage<T>> newStorage = std::unique_ptr<Storage>(new Storage<T>());
    if(n == 0)
      newStorage->_data = nullptr;
    else
      newStorage->_data = std::allocator<T>().allocate(n);
    newStorage->_end = newStorage->_data + n;
    return newStorage;
  }
  
  // Destructs the elements and deallocates the data
 ~Storage() noexcept
  {
    if(size() && !_isShared)
    {
      for(size_t i=0; i!=size(); ++i)
        _data[size()-i-1].~T();
      std::allocator<T>().deallocate(_data, size());
    }
  }
    
  // Return a pointer to the storage data.
  // @{
  T* data() { return _data; }
  const T* data() const { return _data; }
  // @}
  
  // Size of the data, zero if empty.
  size_t size() const { return _end - _data; }

  // Whether this Storage owns its data.
  // Returns @c true when this Storage was constructed with MakeFromSharedData().
  bool is_shared() const { return _isShared; }
  
  Storage(const Storage<T>&) = delete;
  Storage(Storage<T>&&) = delete;
  Storage& operator=(const Storage&) = delete;
  Storage& operator=(Storage&&) = delete;
  
private:
  // Moving range constructor implementation. Parameter integral is only a place-holder.
  Storage(T* startIter, T* endIter, std::false_type /*integral*/, std::true_type /*move*/) :
    _data(construct_move(startIter, endIter)),
    _end(_data + (endIter-startIter)),
    _isShared(false) 
  { }
  
  // Copying range constructor implementation for non-integral types
  template<typename InputIterator>
  Storage(InputIterator startIter, InputIterator endIter, std::false_type /*integral*/) :
    _data(construct_range(startIter, endIter)),
    _end(_data + std::distance(startIter, endIter)),
    _isShared(false)
  { }

  // Copying range constructor implementation for integral types
  template<typename Integral>
  Storage(Integral n, Integral val, std::true_type /*integral*/) :
    _data(construct(n, val)),
    _end(_data + n),
    _isShared(false)
  { }

  // These methods allocate the storage and construct the elements.
  // When any element constructor throws, the already constructed elements are destructed in reverse
  // and the allocated storage is deallocated.
  // @{
  
  T* construct(size_t n)
  {
    if(n == 0)
      return nullptr;
    else {
      T* data = std::allocator<T>().allocate(n);
      T* current = data;
       try {
        for (; current != data+n; ++current) {
          new (current) T();
        }
      } catch(...) {
        while(current != data)
        {
          --current;
          current->~T();
        }
        std::allocator<T>().deallocate(data, n);
        throw;
      }
      return data;
    }
  }
  
  T* construct(size_t n, const T& val)
  {
    if(n == 0)
      return nullptr;
    else {
      T* data = std::allocator<T>().allocate(n);
      T* current = data;
      try {
        for (; current != data+n; ++current) {
          new (current) T(val);
        }
      } catch(...) {
        while(current != data)
        {
          --current;
          current->~T();
        }
        std::allocator<T>().deallocate(data, n);
        throw;
      }
      return data;
    }
  }
  
  template<typename InputIterator>
  T* construct_range(InputIterator startIter, InputIterator endIter)
  {
    if(startIter == endIter)
      return nullptr;
    else {
      size_t n = std::distance(startIter, endIter);
      T* data = std::allocator<T>().allocate(n);
      T* current = data;
      try {
        for (; current != data+n; ++current) {
          new (current) T(*startIter);
          ++startIter;
        }
      } catch(...) {
        while(current != data)
        {
          --current;
          current->~T();
        }
        std::allocator<T>().deallocate(data, n);
        throw;
      }
      return data;
    }
  }
  
  T* construct_move(T* startIter, T* endIter)
  {
    if(startIter == endIter)
      return nullptr;
    else {
      size_t n = endIter - startIter;
      T* data = std::allocator<T>().allocate(n);
      T* current = data;
      try {
        for (; current != data+n; ++current) {
          new (current) T(std::move(*startIter));
          ++startIter;
        }
      } catch(...) {
        while(current != data)
        {
          --current;
          current->~T();
        }
        std::allocator<T>().deallocate(data, n);
        throw;
      }
      return data;
    }
  }
  
  // @}

  // Used by template code above
  // These are already in C++17, but currently only using C++11...
    template<class...> struct disjunction : std::false_type { };
    template<class B1> struct disjunction<B1> : B1 { };
    template<class B1, class... Bn>
    struct disjunction<B1, Bn...> 
    : std::conditional<bool(B1::value), B1, disjunction<Bn...>>::type { };

    template<class...> struct conjunction : std::true_type { };
    template<class B1> struct conjunction<B1> : B1 { };
    template<class B1, class... Bn>
    struct conjunction<B1, Bn...> 
    : std::conditional<bool(B1::value), conjunction<Bn...>, B1>::type {};

  T* _data;
  T* _end;
  bool _isShared;
};

} }

#endif
