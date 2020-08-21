//# tSlidingArrayMath.cc: Test program for function slidingArrayMath
//# Copyright (C) 2006
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

#include "../MaskArrMath.h"
#include "../ArrayLogical.h"
#include "../ArrayPartMath.h"
//#include "../ArrayIO.h"

#include <algorithm>

#include <boost/test/unit_test.hpp>

using namespace casacore;

BOOST_AUTO_TEST_SUITE(sliding_array_math)

template<typename InitListT>
void check(const Array<float>& arr, std::initializer_list<InitListT> ref)
{
  BOOST_CHECK_EQUAL(arr.nelements(), ref.size());
  Array<float>::const_iterator arrIter = arr.begin();
  typename std::initializer_list<InitListT>::const_iterator refIter = ref.begin();
  while(arrIter!=arr.end() && refIter!=ref.end())
  {
    BOOST_CHECK_CLOSE_FRACTION(*arrIter, *refIter, 1e-6);
    ++arrIter; ++refIter;
  }
}

float smartMedian (const Array<float>& arr)
{
  std::vector<float> bbuf(arr.size());
  float* buf = bbuf.data();
  int nl = 0;
  int nr = arr.size();
  float pivot = arr(arr.shape()/2);
  Array<float>::const_iterator iterEnd=arr.end();
  for (Array<float>::const_iterator iter=arr.begin();
       iter!=iterEnd;
       ++iter) {
    float val = *iter;
    if (val <= pivot) {
      buf[nl++] = val;
    } else {
      buf[--nr] = val;
    }
  }
  if (nl >= nr/2) {
    std::nth_element(buf, buf+nr/2, buf+nl);
    return buf[nr/2];
  } else {
    std::nth_element(buf+nl, buf+nr/2-nl, buf+nr-nl);
    return buf[nr/2-nl];
  }
}

BOOST_AUTO_TEST_CASE( standard )
{
  IPosition shape(2,5,5);
  Array<float> arr(shape);
  indgen (arr);
  check(slidingArrayMath(arr, IPosition(2,2), SumFunc<float>(), false),
        {300});
  check(slidingArrayMath(arr, IPosition(2,1), SumFunc<float>(), false),
        {54, 63, 72, 99, 108, 117, 144, 153, 162});
  check(slidingArrayMath(arr, IPosition(1,0), SumFunc<float>(), false),
        {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
          13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24});
  check(slidingArrayMath(arr, IPosition(4,2), SumFunc<float>(), true),
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          300,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0});
  check(slidingArrayMath(arr, IPosition(3,1), SumFunc<float>(), true),
        { 0, 0, 0, 0, 0,
          0, 54, 63, 72, 0,
          0, 99, 108, 117, 0,
          0, 144, 153, 162, 0,
          0, 0, 0, 0, 0 });
  check(slidingArrayMath(arr, IPosition(),    SumFunc<float>(), true),
        {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
          13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24});
  check(slidingArrayMath(arr, IPosition(2,2), MedianFunc<float>(), false),
        { 12 });
  check(slidingArrayMath(arr, IPosition(2,1), MedianFunc<float>(), false),
        { 6, 7, 8, 11, 12, 13, 16, 17, 18 });
  check(slidingArrayMath(arr, IPosition(2,0), MedianFunc<float>(), false),
        {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
          13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24});
}

BOOST_AUTO_TEST_CASE( masked )
{
  IPosition shape(2,5,5);
  Array<float> darr(shape);
  indgen(darr);
  MaskedArray<float> arr = darr(darr<=float(10) || darr>float(14));
  BOOST_CHECK_CLOSE_FRACTION( sum(arr), 250, 1e-6);
  
  check(slidingArrayMath(arr, IPosition(2,2), MaskedSumFunc<float>(), false),
        { 250 });
  check(slidingArrayMath(arr, IPosition(2,1), MaskedSumFunc<float>(), false),
        {31, 27, 33, 76, 72, 78, 121, 117, 123 });
  check(slidingArrayMath(arr, IPosition(4,2), MaskedSumFunc<float>(), true),
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          250,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0});
  check(slidingArrayMath(arr, IPosition(3,1), MaskedSumFunc<float>(), true),
        { 0, 0, 0, 0, 0,
          0, 31, 27, 33, 0,
          0, 76, 72, 78, 0,
          0, 121, 117, 123, 0,
          0, 0, 0, 0, 0 });
  check(slidingArrayMath(arr, IPosition(2,2), MaskedMedianFunc<float>(), false),
        { 10 });
  check(slidingArrayMath(arr, IPosition(2,1), MaskedMedianFunc<float>(), false),
        {5.0, 4.5, 5.5, 10.0, 12.0, 13.0, 17.0, 19.5, 20.5 });
}

BOOST_AUTO_TEST_SUITE_END()
