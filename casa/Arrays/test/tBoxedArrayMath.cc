//# tBoxedArrayMath.cc: Test program for function boxedArrayMath
//# Copyright (C) 2008
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#include "../MaskArrMath.h"
#include "../ArrayLogical.h"
#include "../ArrayPartMath.h"
//#include "../ArrayIO.h"
#include "../MaskArrIO.h"

#include <iostream>
#include <initializer_list>

#include <boost/test/unit_test.hpp>

using namespace casacore;
using namespace std;

BOOST_AUTO_TEST_SUITE(boxed_array_math)

template<typename T>
void check(const Array<T>& a, const IPosition& expectedShape, std::initializer_list<T> expectedValues)
{
  BOOST_CHECK_EQUAL(a.shape(), expectedShape);
  if(a.shape() == expectedShape)
  {
    std::vector<T> v = a.tovector();
    auto iterE = expectedValues.begin();
    auto iterV = v.begin();
    while(iterE != expectedValues.end())
    {
      BOOST_CHECK_CLOSE_FRACTION(*iterE, *iterV, 1e-6);
      ++iterV; ++iterE;
    }
  }
}

void check(const Array<bool>& a, const IPosition& expectedShape, std::initializer_list<bool> expectedValues)
{
  BOOST_CHECK_EQUAL(a.shape(), expectedShape);
  if(a.shape() == expectedShape)
  {
    std::vector<bool> v = a.tovector();
    auto iterE = expectedValues.begin();
    auto iterV = v.begin();
    while(iterE != expectedValues.end())
    {
      BOOST_CHECK_EQUAL(*iterE, *iterV);
      ++iterV; ++iterE;
    }
  }
}

template<typename T>
void check(const MaskedArray<T>& a, const IPosition& expectedShape, std::initializer_list<T> expectedValues, std::initializer_list<bool> expectedMask)
{
  check(a.getArray(), expectedShape, expectedValues);
  check(a.getMask(), expectedShape, expectedMask);
}

BOOST_AUTO_TEST_CASE( sumfunc1 )
{
  IPosition shape(2,5,5);
  Array<float> arr(shape);
  indgen (arr);
  check(
    boxedArrayMath(arr, IPosition(2,2), SumFunc<float>()),
    IPosition{3,3},
    {12.f, 20.f, 13.f, 52.f, 60.f, 33.f, 41.f, 45.f, 24.f}
  );
  check(
    boxedArrayMath(arr, IPosition(2,1), SumFunc<float>()),
    IPosition{5, 5},
      {0.f, 1.f, 2.f, 3.f, 4.f,
      5.f, 6.f, 7.f, 8.f, 9.f,
      10.f, 11.f, 12.f, 13.f, 14.f,
      15.f, 16.f, 17.f, 18.f, 19.f,
      20.f, 21.f, 22.f, 23.f, 24.f}
  );
  check(
    boxedArrayMath(arr, IPosition(1,0), SumFunc<float>()),
    IPosition{1, 5},
    { 10.f, 35.f, 60.f, 85.f, 110.f }
  );
}

BOOST_AUTO_TEST_CASE( sumfunc2 )
{
  IPosition shape(2,5,5);
  Array<float> arr(shape);
  indgen (arr);
  check(
    boxedArrayMath(arr, IPosition(4,2), SumFunc<float>()),
    IPosition{3, 3},
    { 12.f, 20.f, 13.f, 52.f, 60.f, 33.f, 41.f, 45.f, 24.f }
  );
  check(
    boxedArrayMath(arr, IPosition(3,1), SumFunc<float>()),
    IPosition{5, 5},
      {0.f, 1.f, 2.f, 3.f, 4.f,
      5.f, 6.f, 7.f, 8.f, 9.f,
      10.f, 11.f, 12.f, 13.f, 14.f,
      15.f, 16.f, 17.f, 18.f, 19.f,
      20.f, 21.f, 22.f, 23.f, 24.f}
  );
  check(
    boxedArrayMath(arr, IPosition(),    SumFunc<float>()),
    IPosition{5, 5},
      {0.f, 1.f, 2.f, 3.f, 4.f,
      5.f, 6.f, 7.f, 8.f, 9.f,
      10.f, 11.f, 12.f, 13.f, 14.f,
      15.f, 16.f, 17.f, 18.f, 19.f,
      20.f, 21.f, 22.f, 23.f, 24.f}
  );
}

BOOST_AUTO_TEST_CASE( medianfunc )
{
  IPosition shape(2,5,5);
  Array<float> arr(shape);
  indgen (arr);
  check(
    boxedArrayMath(arr, IPosition(2,2), MedianFunc<float>()),
    IPosition{3, 3},
    { 3.f, 5.f, 6.5f, 13.f, 15.f, 16.5f, 20.5f, 22.5f, 24.f }
  );
  check(
    boxedArrayMath(arr, IPosition(2,1), MedianFunc<float>()),
    IPosition{5, 5},
      {0.f, 1.f, 2.f, 3.f, 4.f,
      5.f, 6.f, 7.f, 8.f, 9.f,
      10.f, 11.f, 12.f, 13.f, 14.f,
      15.f, 16.f, 17.f, 18.f, 19.f,
      20.f, 21.f, 22.f, 23.f, 24.f}
  );
  check(
    boxedArrayMath(arr, IPosition(2,0), MedianFunc<float>()),
    IPosition{1, 1},
      {12.f}
  );
}

BOOST_AUTO_TEST_CASE( masked_init )
{
  Array<float> darr(IPosition{5, 5});
  indgen(darr);
  MaskedArray<float> arr = darr(darr<=float(10) || darr>float(14));
  check(arr.getMask(), IPosition{5, 5},
    { true, true, true, true, true,
      true, true, true, true, true,
      true, false, false, false, false,
      true, true, true, true, true,
      true, true, true, true, true });
  
  BOOST_CHECK_CLOSE_FRACTION(sum(arr), 250, 1e-6);
}

BOOST_AUTO_TEST_CASE( masked_sum1 )
{
  Array<float> darr(IPosition{5, 5});
  indgen(darr);
  MaskedArray<float> arr = darr(darr<=float(10) || darr>float(14));
  check(
    boxedArrayMath(arr, IPosition(2,2), MaskedSumFunc<float>()),
    IPosition{3, 3},
    { 12.f, 20.f, 13.f, 41.f, 35.f, 19.f, 41.f, 45.f, 24.f },
    { true, true, true, true, true, true, true, true, true }
  );
  check(
    boxedArrayMath(arr, IPosition(2,1), MaskedSumFunc<float>()),
    IPosition{5, 5},
    {0.f, 1.f, 2.f, 3.f, 4.f,
    5.f, 6.f, 7.f, 8.f, 9.f,
    10.f, 0.f, 0.f, 0.f, 0.f,
    15.f, 16.f, 17.f, 18.f, 19.f,
    20.f, 21.f, 22.f, 23.f, 24.f},
    { true, true, true, true, true,
    true, true, true, true, true,
    true, false, false, false, false,
    true, true, true, true, true,
    true, true, true, true, true });
}

BOOST_AUTO_TEST_CASE( masked_sum2 )
{
  Array<float> darr(IPosition{5, 5});
  indgen(darr);
  MaskedArray<float> arr = darr(darr<=float(10) || darr>float(14));
  check(
    boxedArrayMath(arr, IPosition(4,2), MaskedSumFunc<float>()),
    IPosition{3, 3},
    { 12.f, 20.f, 13.f, 41.f, 35.f, 19.f, 41.f, 45.f, 24.f },
    { true, true, true, true, true, true, true, true, true }
  );
  check(
    boxedArrayMath(arr, IPosition(3,1), MaskedSumFunc<float>()),
    IPosition{5, 5},
    {0.f, 1.f, 2.f, 3.f, 4.f,
    5.f, 6.f, 7.f, 8.f, 9.f,
    10.f, 0.f, 0.f, 0.f, 0.f,
    15.f, 16.f, 17.f, 18.f, 19.f,
    20.f, 21.f, 22.f, 23.f, 24.f},
    { true, true, true, true, true,
    true, true, true, true, true,
    true, false, false, false, false,
    true, true, true, true, true,
    true, true, true, true, true });
}

BOOST_AUTO_TEST_CASE( masked_median )
{
  Array<float> darr(IPosition{5, 5});
  indgen(darr);
  MaskedArray<float> arr = darr(darr<=float(10) || darr>float(14));
  check(
    boxedArrayMath(arr, IPosition(2,2), MaskedMedianFunc<float>()),
    IPosition{3, 3},
    {3.f, 5.f, 6.5f, 15.f, 17.5f, 19.f, 20.5f, 22.5f, 24.f},
    { true, true, true, true, true, true, true, true, true }
  );
  check(
    boxedArrayMath(arr, IPosition(2,1), MaskedMedianFunc<float>()),
    IPosition{5, 5},
    {0.f, 1.f, 2.f, 3.f, 4.f,
    5.f, 6.f, 7.f, 8.f, 9.f,
    10.f, 0.f, 0.f, 0.f, 0.f,
    15.f, 16.f, 17.f, 18.f, 19.f,
    20.f, 21.f, 22.f, 23.f, 24.f},
    { true, true, true, true, true,
    true, true, true, true, true,
    true, false, false, false, false,
    true, true, true, true, true,
    true, true, true, true, true });
}

BOOST_AUTO_TEST_SUITE_END()
