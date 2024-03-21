//# tArrayIteratorSTL.cc: Test program for the Array Iterator member class
//# Copyright (C) 2002,2003
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#include "../Array.h"
#include "../ArrayMath.h"

#include <vector>
#include <iterator>

#include <boost/test/unit_test.hpp>

using namespace casacore;

BOOST_AUTO_TEST_SUITE(array_iterator_stl)

void testSub (Array<int>& arr1, const IPosition& blc,
	      const IPosition& trc, const IPosition& inc)
{
  Array<int> arr = arr1(blc,trc,inc);
  Array<int> arrs;
  arrs.assign_conforming(arr);
  std::vector<int> vec(arr.begin(), arr.end());
  Array<int>::const_iterator iters = arrs.begin();
  size_t i=0;
  Array<int>::const_iterator enditer=arr.end();
  for (Array<int>::const_iterator iter=arr.begin(); iter!=enditer; ++iter) {
    BOOST_CHECK_EQUAL(*iter, *iters);
    BOOST_CHECK_EQUAL(vec[i], *iters);
    BOOST_CHECK_EQUAL(arrs.data()[i], *iters);
    iters++;
    i++;
  }
  BOOST_CHECK (i == arr.size());
  BOOST_CHECK (std::distance(arr.begin(), arr.end()) == int(arr.size()));
}

BOOST_AUTO_TEST_CASE( iterator )
{
  Array<int> arr(IPosition(3,4,5,6));
  indgen(arr);
  {
    int i=0;
    Array<int>::const_iterator enditer=arr.end();
    for (Array<int>::const_iterator iter=arr.begin(); iter!=enditer; ++iter) {
      BOOST_CHECK_EQUAL (*iter, i);
      ++i;
    }
    BOOST_CHECK_EQUAL (size_t(i), arr.nelements());
  }
  testSub (arr, IPosition(3,0,0,0), IPosition(3,3,4,5), IPosition(3,1,1,1));
  testSub (arr, IPosition(3,0,0,0), IPosition(3,3,4,5), IPosition(3,2,1,1));
  testSub (arr, IPosition(3,0,0,0), IPosition(3,3,4,5), IPosition(3,1,1,2));
  testSub (arr, IPosition(3,3,0,0), IPosition(3,3,4,5), IPosition(3,1,1,1));
  testSub (arr, IPosition(3,3,4,1), IPosition(3,3,4,4), IPosition(3,1,1,1));
  testSub (arr, IPosition(3,1,2,1), IPosition(3,3,3,4), IPosition(3,2,1,1));
}

void checkEmpty(Array<int> earr)
{
  for (const int& i : earr) {
    (void) i; // prevent warning for unused i
    BOOST_CHECK(false); // should be empty
  }
  for (Array<int>::iterator itera1=earr.begin();
  itera1!=earr.end(); itera1++) {
    BOOST_CHECK(false); // should be empty
  }
  for (Array<int>::iterator itera2=earr.begin();
  itera2!=earr.end(); itera2++) {
    BOOST_CHECK(false); // should be empty
  }
  for (Array<int>::contiter itera3=earr.cbegin(); 
  itera3!=earr.cend(); itera3++) {
    BOOST_CHECK(false); // should be empty
  }
  for (Array<int>::contiter itera4=earr.cbegin();
  itera4!=earr.cend(); itera4++) {
    BOOST_CHECK(false); // should be empty
  }
  BOOST_CHECK(true);
}

BOOST_AUTO_TEST_CASE( uninitialized_array )
{
  Array<int> uninitialized;
  checkEmpty(uninitialized);
}

BOOST_AUTO_TEST_CASE( empty_array )
{
  IPosition eshp;
  Array<int> empty(eshp);
  checkEmpty(empty);
}

BOOST_AUTO_TEST_SUITE_END()
