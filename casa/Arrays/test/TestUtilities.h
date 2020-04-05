#ifndef TEST_UTILITIES_H
#define TEST_UTILITIES_H

#include "../Array.h"
#include "../IPosition.h"

#include <vector>

#include <boost/test/unit_test.hpp>

inline void check(const casacore::Array<bool>& v, std::initializer_list<int> list)
{
  std::vector<int> a;
  a.reserve(v.nelements());
  for(bool b : v)
    a.push_back(b ? 1 : 0);
  BOOST_CHECK_EQUAL_COLLECTIONS(a.begin(), a.end(), list.begin(), list.end());
}

template<typename T>
inline void check(const casacore::Array<T>& v, std::initializer_list<T> list)
{
  BOOST_CHECK_EQUAL_COLLECTIONS(v.begin(), v.end(), list.begin(), list.end());
}

template<typename T1, typename T2>
inline void check(const casacore::MaskedArray<T1>& v, std::initializer_list<T1> arr, std::initializer_list<T2> mask)
{
  check(v.getArray(), arr);
  check(v.getMask(), mask);
}

inline void check(const casacore::IPosition& ip, std::initializer_list<int> reference)
{
  BOOST_CHECK_EQUAL_COLLECTIONS(ip.begin(), ip.end(), reference.begin(), reference.end());
}

#endif
