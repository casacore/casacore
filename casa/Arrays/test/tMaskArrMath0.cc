//# tMaskArrMath0.cc: Test program for MaskedArrays mathematical operations.
//# Copyright (C) 1993,1994,1995,1996,1999,2000,2001
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#include "../IPosition.h"
#include "../Array.h"
#include "../ArrayMath.h"
#include "../ArrayLogical.h"
//#include "../ArrayIO.h"
#include "../Vector.h"
#include "../Matrix.h"
#include "../Cube.h"
#include "../ArrayError.h"
#include "../MaskedArray.h"
#include "../MaskArrMath.h"

#include "TestUtilities.h"

#include <boost/test/unit_test.hpp>

#include <initializer_list>

using namespace casacore;

BOOST_AUTO_TEST_SUITE(masked_array_math0)

struct Fixture
{
  Vector<int> f, g, h;
  Vector<bool> b;
  
  Fixture() : f(10), g(10), h(10), b(10)
  {
    indgen (f);
  }
};

BOOST_FIXTURE_TEST_CASE(less_than, Fixture)
{
  b = (f<3);
  check(b, {1, 1, 1, 0, 0, 0, 0, 0, 0, 0});
  
  MaskedArray<int> m(h,b);
  h = 0;
  indgen (m);
  check(h, {0, 1, 2, 0, 0, 0, 0, 0, 0, 0});
}

BOOST_FIXTURE_TEST_CASE(larger_and_less, Fixture)
{
  check(((f>3) && (f<7)), {0, 0, 0, 0, 1, 1, 1, 0, 0, 0});
  
  MaskedArray<int> m( h( ((f>3) && (f<7)) ) );
  h = 0;
  indgen (m, 10);
  check(h, {0, 0, 0, 0, 10, 11, 12, 0, 0, 0});
}

BOOST_FIXTURE_TEST_CASE(sum_assign1, Fixture)
{
  b = (f>2);
  check(b, {0, 0, 0, 1, 1, 1, 1, 1, 1, 1});
  
  MaskedArray<int> m(h,b);
  
  h = 1;
  m += 5;
  check(h, {1, 1, 1, 6, 6, 6, 6, 6, 6, 6});
}

BOOST_FIXTURE_TEST_CASE(sum_assign2, Fixture)
{
  
  b = (f>2);
  check(b, {0, 0, 0, 1, 1, 1, 1, 1, 1, 1});
  
  h = 1;
  h(b) += 5;
  check(h, {1, 1, 1, 6, 6, 6, 6, 6, 6, 6});
}

BOOST_FIXTURE_TEST_CASE(sum_assign3, Fixture)
{
  
  b = (f>2);
  check(b, {0, 0, 0, 1, 1, 1, 1, 1, 1, 1});
  
  MaskedArray<int> m(h,b);
  
  h = -1;
  m += f;
  check(h, {-1, -1, -1, 2, 3, 4, 5, 6, 7, 8});
}

BOOST_FIXTURE_TEST_CASE(sum_assign4, Fixture)
{
  b = (f>2);
  check(b, {0, 0, 0, 1, 1, 1, 1, 1, 1, 1});
  
  h = -1;
  h(b) += f;
  check(h, {-1, -1, -1, 2, 3, 4, 5, 6, 7, 8});
}

BOOST_FIXTURE_TEST_CASE(sum_assign5, Fixture)
{
  b = (f>4);
  check(b, {0, 0, 0, 0, 0, 1, 1, 1, 1, 1});
  
  MaskedArray<int> m(f,b);
  
  indgen (h);
  check(h, {0, 1, 2, 3, 4, 5, 6, 7, 8, 9});
  
  h += m;
  check(h, {0, 1, 2, 3, 4, 10, 12, 14, 16, 18});
}

BOOST_FIXTURE_TEST_CASE(sum_assign6, Fixture)
{
  b = (f>3);
  check(b, {0, 0, 0, 0, 1, 1, 1, 1, 1, 1});
  
  Vector<bool> c(10);
  c = (f<8);
  check(c, {1, 1, 1, 1, 1, 1, 1, 1, 0, 0});
  
  MaskedArray<int> m(h,b), n(f,c);
  h = -1;
  m += n;
  check(h, {-1, -1, -1, -1, 3, 4, 5, 6, -1, -1});
}

BOOST_FIXTURE_TEST_CASE(sum_assign7, Fixture)
{
  b = (f>3);
  check(b, {0, 0, 0, 0, 1, 1, 1, 1, 1, 1});
  
  Vector<bool> c(10);
  c = (f<8);
  check(c, {1, 1, 1, 1, 1, 1, 1, 1, 0, 0});
  
  MaskedArray<int> n(f,c);
  
  h = -1;
  h(b) += n;
  check(h, {-1, -1, -1, -1, 3, 4, 5, 6, -1, -1});
}

BOOST_AUTO_TEST_SUITE_END()
