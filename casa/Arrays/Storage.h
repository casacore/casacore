#ifndef CASACORE_STORAGE_2_H
#define CASACORE_STORAGE_2_H

#include <cstring>
#include <memory>
  
namespace casacore {

// This class is like a std::vector with a static size. It is used in the
// Array class, and is necessary because std::vector specializes for bool.
// It holds the same functionality as a normal array, but enables allocation
// through different allocators.
template<typename T, typename Alloc>
class Storage : public Alloc
{
public:
  Storage() :
    Alloc(),
    _data(nullptr),
    _end(nullptr)
  { }
  
  Storage(std::size_t n) :
    Alloc(),
    _data(construct(n)),
    _end(_data + n)
  { }
  
  Storage(std::size_t n, const Alloc& allocator) :
    Alloc(allocator),
    _data(construct(n)),
    _end(_data + n)
  { }
  
  Storage(std::size_t n, const T& val) :
    Alloc(),
    _data(construct(n, val)),
    _end(_data + n)
  { }
  
  Storage(std::size_t n, const T& val, const Alloc& allocator) :
    Alloc(allocator),
    _data(construct(n, val)),
    _end(_data + n)
  { }
  
  template<typename InputIterator>
  Storage(InputIterator startIter, InputIterator endIter, const Alloc& allocator=Alloc()) :
    Alloc(allocator),
    _data(construct(startIter, endIter)),
    _end(_data + std::distance(startIter, endIter))
  { }
  
  Storage(const Storage&) = delete;
  Storage(Storage&&) = delete;
  
  static std::unique_ptr<Storage> MakeFromMove(T* startIter, T* endIter, const Alloc& allocator)
  {
    return std::unique_ptr<Storage>(new Storage(startIter, endIter, allocator, std::true_type()));
  }
  
  ~Storage() noexcept
  {
    if(size())
    {
      for(size_t i=0; i!=size(); ++i)
        _data[size()-i-1].~T();
      Alloc::deallocate(_data, size());
    }
  }
  
  Storage& operator=(const Storage&) = delete;
  Storage& operator=(Storage&&) = delete;
  
  T* data() { return _data; }
  const T* data() const { return _data; }
  
  size_t size() const { return _end - _data; }
  
private:
  Storage(T* startIter, T* endIter, const Alloc& allocator, std::true_type /*move*/) :
    Alloc(allocator),
    _data(construct_move(startIter, endIter)),
    _end(_data + (endIter-startIter))
  { }
  
  T* construct(size_t n)
  {
    if(n == 0)
      return nullptr;
    else {
      T* data = Alloc::allocate(n);
      try {
        for(size_t i=0; i!=n; ++i)
          new (&data[i]) T();
      } catch(...) {
        // TODO all already constructed objects should be destructed in reverse
        // C++17 has uninitialized_default_construct_n to fix this
        Alloc::deallocate(data, n);
      }
      return data;
    }
  }
  
  T* construct(size_t n, const T& val)
  {
    if(n == 0)
      return nullptr;
    else {
      T* data = Alloc::allocate(n);
      try {
        for(size_t i=0; i!=n; ++i)
          new (&data[i]) T(val);
      } catch(...) {
        // TODO all already constructed objects should be destructed in reverse
        // C++17 has uninitialized_default_construct_n to fix this
        Alloc::deallocate(data, n);
      }
      return data;
    }
  }
  
  template<typename InputIterator>
  T* construct(InputIterator startIter, InputIterator endIter)
  {
    if(startIter == endIter)
      return nullptr;
    else {
      size_t n = std::distance(startIter, endIter);
      T* data = Alloc::allocate(n);
      try {
        for(size_t i=0; i!=n; ++i)
        {
          new (&data[i]) T(*startIter);
          ++startIter;
        }
      } catch(...) {
        // TODO all already constructed objects should be destructed in reverse
        // C++17 has uninitialized_default_construct_n to fix this
        Alloc::deallocate(data, n);
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
      T* data = Alloc::allocate(n);
      try {
        for(size_t i=0; i!=n; ++i)
          new (&data[i]) T(std::move(startIter[i]));
      } catch(...) {
        // TODO all already constructed objects should be destructed in reverse
        // C++17 has uninitialized_default_construct_n to fix this
        Alloc::deallocate(data, n);
      }
      return data;
    }
  }
  
  T* _data;
  T* _end;
};

}

#endif
