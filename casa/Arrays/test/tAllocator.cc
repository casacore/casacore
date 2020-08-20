#include "../IPosition.h"
#include "../Array.h"

#include <boost/test/unit_test.hpp>

using namespace casacore;

class Pool {
public:
  Pool(size_t poolSize=1024) :
    _size(poolSize), _position(0),
    _nAlloc(0), _nDealloc(0)
  { 
    _pool = new char[poolSize];
  }
  
  Pool(const Pool& source) = delete;
  
  ~Pool() noexcept(false) {
    if(_nAlloc != _nDealloc)
      throw std::runtime_error("Memory leak: nalloc=" + std::to_string(_nAlloc) + ", ndealloc=" + std::to_string(_nDealloc));
    delete[] _pool;
  }
  
  Pool& operator=(const Pool& source) = delete;
  
  char* allocate(size_t n)
  {
    char* data = &_pool[_position];
    size_t allocsize = n==0 ? 1 : n; // We much return a unique ptr to be able to distinguish it
    _position += allocsize;
    if(_position > _size)
      throw std::runtime_error("Pool is full");
    ++_nAlloc;
    _chunks.emplace(reinterpret_cast<char*>(data), std::make_pair(n, true));
    return data;
  }
  
  void deallocate(char* data, size_t n)
  {
    // Generally, deallocate(nullptr) is not allowed for all allocators, so this function throws when doing so
    auto iter = _chunks.find(reinterpret_cast<char*>(data));
    if(iter == _chunks.end())
      throw std::runtime_error("Can't deallocate chunk at " + std::to_string((size_t)data) + ": not an allocated chunk");
    if(!iter->second.second)
      throw std::runtime_error("Can't deallocate chunk at " + std::to_string((size_t)data) + ": was already deallocated");
    if(iter->second.first != n)
      throw std::runtime_error("Can't deallocate chunk at " + std::to_string((size_t)data) + ": invalid size specified, expected " + std::to_string(iter->second.first) + ", was " + std::to_string(n));
    iter->second.second = false;
    ++_nDealloc;
  }
  
private:
  char* _pool;
  size_t _size, _position;
  size_t _nAlloc, _nDealloc;
  std::map<char*, std::pair<size_t, bool>> _chunks;
};

// Type that satisfies the minimum requirements of an Allocator
template<typename T>
class PoolAllocator {
public:
  // No default constructor is required.
  PoolAllocator(size_t mem) : _pool(new Pool(mem)) { }
  
  // Copy and move constructors are required: default ones are okay.
  PoolAllocator(const PoolAllocator<T>& source) = default;
  PoolAllocator<T>& operator=(const PoolAllocator<T>& source) = default;
  
  // Rebind constructor
  template<typename U>
  PoolAllocator(const PoolAllocator<U>& source) :
    _pool(source._pool)
  { }
  
  // Rebind assignment
  template<typename U>
  PoolAllocator<T>& operator=(const PoolAllocator<U>& source)
  { 
    _pool = source._pool;
    return *this;
  }
  
  T* allocate(size_t n) { return reinterpret_cast<T*>(_pool->allocate(n*sizeof(T))); }
  void deallocate(T* data, size_t n) { _pool->deallocate(reinterpret_cast<char*>(data), n*sizeof(T)); }
private:
  std::shared_ptr<Pool> _pool;
};

BOOST_AUTO_TEST_SUITE(allocator)

BOOST_AUTO_TEST_CASE(poolallocator)
{
  // Do some basic tests to check if the custom allocator works properly
  PoolAllocator<int> alloc(1024);
  int* a = alloc.allocate(1);
  int* b = alloc.allocate(5);
  int* c = alloc.allocate(1);
  int* n = alloc.allocate(0);
  BOOST_CHECK_NE(a, nullptr);
  BOOST_CHECK_NE(b, nullptr);
  BOOST_CHECK_NE(c, nullptr);
  BOOST_CHECK_NE(a, b);
  BOOST_CHECK_NE(b, c);
  BOOST_CHECK_NE(a, c);
  a[0] = 1982;
  for(size_t i=0; i!=5; ++i)
    b[i] = 2020+i;
  c[0] = 37;
  BOOST_CHECK_EQUAL(a[0], 1982);
  for(size_t i=0; i!=5; ++i)
    BOOST_CHECK_EQUAL(b[i], 2020+i);
  BOOST_CHECK_EQUAL(c[0], 37);
  
  alloc.deallocate(b, 5);
  int* d = alloc.allocate(3);
  BOOST_CHECK_NE(d, nullptr);
  BOOST_CHECK_NE(a, d);
  BOOST_CHECK_NE(c, d);
  d[0] = 8; d[1] = 9; d[2] = 10;
  BOOST_CHECK_EQUAL(a[0], 1982);
  BOOST_CHECK_EQUAL(c[0], 37);
  for(size_t i=0; i!=3; ++i)
  BOOST_CHECK_EQUAL(d[i], 8+i);
  
  alloc.deallocate(n, 0);
  alloc.deallocate(d, 3);
  alloc.deallocate(a, 1);
  alloc.deallocate(c, 1);
}

BOOST_AUTO_TEST_CASE(custom_allocator_array)
{
  PoolAllocator<int> allocA(1024), allocB(1024);
  Array<int, PoolAllocator<int>>
    a(allocA),
    b(IPosition(2, 3), 1982, allocA);
  a = std::move(b);
  Array<int, PoolAllocator<int>> c(IPosition(4, 1), allocB);
  c.assign( a );
}

BOOST_AUTO_TEST_CASE(array_shared_storage)
{
  int storage = 42;
  
  Array<int> a(IPosition{1}, &storage, StorageInitPolicy::SHARE);
  BOOST_CHECK_EQUAL(storage, 42);
  std::vector<int> ref={42};
  BOOST_CHECK_EQUAL_COLLECTIONS(a.begin(), a.end(), ref.begin(), ref.end());
  
  a.assign_conforming(Array<int>(IPosition{1}, 37));
  BOOST_CHECK_EQUAL(storage, 37);
  ref[0] = 37;
  BOOST_CHECK_EQUAL_COLLECTIONS(a.begin(), a.end(), ref.begin(), ref.end());
}

BOOST_AUTO_TEST_CASE(array_take_shared_storage)
{
  IPosition shape{3, 4, 2, 3};
  std::vector<int> v(shape.product(), 1982);
  
  Array<int> a;
  a.takeStorage(shape, v.data(), StorageInitPolicy::SHARE);
  BOOST_CHECK_EQUAL(v.data()[0], 1982);
  std::vector<int> ref1(shape.product(), 1982);
  BOOST_CHECK_EQUAL_COLLECTIONS(a.begin(), a.end(), ref1.begin(), ref1.end());
  
  a.assign_conforming(Array<int>(shape, 1988));
  std::vector<int> ref2(shape.product(), 1988);
  BOOST_CHECK_EQUAL_COLLECTIONS(v.begin(), v.end(), ref2.begin(), ref2.end());
  BOOST_CHECK_EQUAL_COLLECTIONS(a.begin(), a.end(), ref2.begin(), ref2.end());
}

BOOST_AUTO_TEST_CASE(array_uninitialized)
{
  IPosition shape{1, 2, 3, 4};
  Array<int> a(shape, Array<int>::uninitialized);
  BOOST_CHECK_EQUAL(a.shape(), shape);
  a = 3;
  BOOST_CHECK(allEQ(a, 3));
  Array<int> b(shape, Array<int>::uninitialized);
  b.assign_conforming(a);
  BOOST_CHECK(allEQ(b, 3));
}

BOOST_AUTO_TEST_SUITE_END()
