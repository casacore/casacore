//# tMaskArrLogi.cc: Test program for MaskedArray logical operations
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
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

#include "../IPosition.h"
#include "../Array.h"
#include "../ArrayError.h"
//#include "../ArrayIO.h"
#include "../ArrayLogical.h"
#include "../ArrayMath.h"
#include "../Vector.h"
#include "../Matrix.h"
#include "../Cube.h"
#include "../LogiVector.h"
#include "../MaskedArray.h"
#include "../MaskArrIO.h"
#include "../MaskArrLogi.h"
#include "../MaskArrMath.h"

#include "TestUtilities.h"

#include <boost/test/unit_test.hpp>

#include <initializer_list>

using namespace casacore;

struct Fixture
{
  std::vector<int> ref;
  Vector<int> u, v, w, x, y, z;
  LogicalArray b;
  
  Fixture() :
    u(10), v(10), w(10), x(10), y(10), z(10),
    b(IPosition(1,10))
  {
    indgen (u, -2);
    u(0) = 8;
    u(1) = 9;
    v=-1;
    w=11;
    x=1;
    indgen (y);
    z=5;
    b = ((y > 3) && (y < 8));
  }
};

BOOST_AUTO_TEST_SUITE(mask_array_logical_operators)

BOOST_FIXTURE_TEST_CASE(prologue, Fixture)
{
  check(u, {8, 9, 0, 1, 2, 3, 4, 5, 6, 7});
  check(v, {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1});
  check(w, {11, 11, 11, 11, 11, 11, 11, 11, 11, 11});
  check(x, {1, 1, 1, 1, 1, 1, 1, 1, 1, 1});
  check(y, {0, 1, 2, 3, 4, 5, 6, 7, 8, 9});
  check(z, {5, 5, 5, 5, 5, 5, 5, 5, 5, 5});
  check(b, {false, false, false, false, true, true, true, true, false, false});
}

BOOST_FIXTURE_TEST_CASE(constructor, Fixture)
{
  Vector<int> a(x.copy());
  MaskedArray<int> ma (a, b);
  MaskedArray<int> mma (ma, y>5);
  
  mma = 75;
  check(a, {1, 1, 1, 1, 1, 1, 75, 75, 1, 1});
}

BOOST_FIXTURE_TEST_CASE(brackets_operator, Fixture)
{
  Vector<int> a(x.copy());
  MaskedArray<int> ma (a, b);
  
  ma (y>5) = 75;
  check(a, {1, 1, 1, 1, 1, 1, 75, 75, 1, 1});
}

BOOST_FIXTURE_TEST_CASE(le_operator1, Fixture)
{
  check((y(b) <= z).getArray(), {0, 0, 0, 0, 1, 1, 0, 0, 0, 0});
}

BOOST_FIXTURE_TEST_CASE(le_operator2, Fixture)
{
  check((y <= z(b)).getArray(), {0, 0, 0, 0, 1, 1, 0, 0, 0, 0});
}

BOOST_FIXTURE_TEST_CASE(le_operator3, Fixture)
{
  check((y(b) <= z(y>4)).getArray(), {0, 0, 0, 0, 0, 1, 0, 0, 0, 0});
}

BOOST_FIXTURE_TEST_CASE(le_operator4, Fixture)
{
  check((y(b) <= 5).getArray(), {0, 0, 0, 0, 1, 1, 0, 0, 0, 0});
}

BOOST_FIXTURE_TEST_CASE(le_operator5, Fixture)
{
  check((5 > y(b)).getArray(), {0, 0, 0, 0, 1, 0, 0, 0, 0, 0});
}

BOOST_FIXTURE_TEST_CASE(logical_and1, Fixture)
{
  check(((5 > y(b)) && true).getArray(), {0, 0, 0, 0, 1, 0, 0, 0, 0, 0});
}

BOOST_FIXTURE_TEST_CASE(logical_and2, Fixture)
{
  check(((5 > y(b)) && false).getArray(), {0, 0, 0, 0, 0, 0, 0, 0, 0, 0});
}

BOOST_FIXTURE_TEST_CASE(logical_or1, Fixture)
{
  check(((5 > y(b)) || true).getArray(), {0, 0, 0, 0, 1, 1, 1, 1, 0, 0});
}

BOOST_FIXTURE_TEST_CASE(logical_or2, Fixture)
{
  check(((5 > y(b)) || false).getArray(), {0, 0, 0, 0, 1, 0, 0, 0, 0, 0});
}

BOOST_FIXTURE_TEST_CASE(logical_and3, Fixture)
{
  check((true && (5 > y(b))).getArray(), {0, 0, 0, 0, 1, 0, 0, 0, 0, 0});
}

BOOST_FIXTURE_TEST_CASE(logical_and4, Fixture)
{
  check((false && (5 > y(b))).getArray(), {0, 0, 0, 0, 0, 0, 0, 0, 0, 0});
}

BOOST_FIXTURE_TEST_CASE(logical_or3, Fixture)
{
  check((true || (5 > y(b))).getArray(), {0, 0, 0, 0, 1, 1, 1, 1, 0, 0});
}

BOOST_FIXTURE_TEST_CASE(logical_or4, Fixture)
{
  check( (false || (5 > y(b))).getArray(), {0, 0, 0, 0, 1, 0, 0, 0, 0, 0});
}

BOOST_FIXTURE_TEST_CASE(negate, Fixture)
{
  Vector<int> a_log (10);
  indgen (a_log);
  
  LogicalVector b_log ((a_log > 1) && (a_log < 6));
  
  check(a_log, {0, 1, 2, 3, 4, 5, 6, 7, 8, 9});
  check(b_log, {0, 0, 1, 1, 1, 1, 0, 0, 0, 0});
  
  MaskedLogicalArray mla_log ((a_log >= 3), b_log);
  check(mla_log.getArray(), {0, 0, 0, 1, 1, 1, 1, 1, 1, 1});
  check(mla_log.getMask(), {0, 0, 1, 1, 1, 1, 0, 0, 0, 0});
  
  check((!mla_log).getArray(), {0, 0, 1, 0, 0, 0, 1, 1, 1, 1});
  check((!mla_log).getMask(), {0, 0, 1, 1, 1, 1, 0, 0, 0, 0});
  
  check(((a_log < 3)(b_log)).getArray(), {1, 1, 1, 0, 0, 0, 0, 0, 0, 0});
  check(((a_log < 3)(b_log)).getMask(), {0, 0, 1, 1, 1, 1, 0, 0, 0, 0});
  
  BOOST_CHECK_EQUAL(allEQ (!mla_log, (a_log < 3)(b_log)), true);
}

BOOST_FIXTURE_TEST_CASE(all_le, Fixture)
{
  BOOST_CHECK(!allLE (y(b), z));
  BOOST_CHECK(allLE (y(b), w));
}

BOOST_FIXTURE_TEST_CASE(all_gt, Fixture)
{
  BOOST_CHECK(!allGT (z, y(b)));
  BOOST_CHECK(allGT (w, y(b)));
}

BOOST_FIXTURE_TEST_CASE(all_lt1, Fixture)
{
  BOOST_CHECK(!allLT (u(u<9), y(y<7)));
  BOOST_CHECK(allLT (u(u<8), y(y<7)));
}

BOOST_FIXTURE_TEST_CASE(any_le, Fixture)
{
  BOOST_CHECK(anyLE (y(b), z));
  BOOST_CHECK(!anyLE (y(b), v));
}

BOOST_FIXTURE_TEST_CASE(any_gt, Fixture)
{
  BOOST_CHECK(anyGT (z, y(b)));
  BOOST_CHECK(!anyGT (v, y(b)));
}

BOOST_FIXTURE_TEST_CASE(any_lt1, Fixture)
{
  BOOST_CHECK(anyLT (u(u<9), y(y<7)));
  BOOST_CHECK(!anyLT (u(u>8), y(y<7)));
}

BOOST_FIXTURE_TEST_CASE(any_and1, Fixture)
{
  BOOST_CHECK(anyAND (b(y>5), y>4));
  BOOST_CHECK(!anyAND (b(y==4), y>4));
}

BOOST_FIXTURE_TEST_CASE(any_and2, Fixture)
{
  BOOST_CHECK(anyAND (y>4, b(y>5)));
  BOOST_CHECK(!anyAND (y>4, b(y==4)));
}

BOOST_FIXTURE_TEST_CASE(any_and3, Fixture)
{
  BOOST_CHECK(anyAND (b(y>4), b(y>5)));
  BOOST_CHECK(!anyAND (b(y<3), b(y<=3)));
}

BOOST_FIXTURE_TEST_CASE(any_or1, Fixture)
{
  BOOST_CHECK(anyOR (b(y>5), y>4));
  BOOST_CHECK(!anyOR (b(y==3), y>4));
}

BOOST_FIXTURE_TEST_CASE(any_or2, Fixture)
{
  BOOST_CHECK(anyOR (y>4, b(y>5)));
  BOOST_CHECK(!anyOR (y>4, b(y==3)));
}

BOOST_FIXTURE_TEST_CASE(any_or3, Fixture)
{
  BOOST_CHECK(anyOR (b(y>4), b(y>5)));
  BOOST_CHECK(!anyOR (b(y<3), b(y<=3)));
}

BOOST_FIXTURE_TEST_CASE(all_lt2, Fixture)
{
  BOOST_CHECK(!allLT (y(y>5), 7));
  BOOST_CHECK(allLT (y(y>5), 11));
}

BOOST_FIXTURE_TEST_CASE(all_ge, Fixture)
{
  BOOST_CHECK(!allGE (7, y(y>5)));
  BOOST_CHECK(allGE (11, y(y>5)));
}

BOOST_FIXTURE_TEST_CASE(any_lt2, Fixture)
{
  BOOST_CHECK(anyLT (y(y>5), 7));
  BOOST_CHECK(!anyLT (y(y>5), 5));
}

BOOST_FIXTURE_TEST_CASE(any_ge, Fixture)
{
  BOOST_CHECK(anyGE (7, y(y>5)));
  BOOST_CHECK(!anyGE (5, y(y>5)));
}

BOOST_FIXTURE_TEST_CASE(all_and1, Fixture)
{
  BOOST_CHECK(!allAND (b(y>5), true));
  BOOST_CHECK(allAND (b(b), true));
  BOOST_CHECK(!allAND (b(b), false));
}

BOOST_FIXTURE_TEST_CASE(all_and2, Fixture)
{
  BOOST_CHECK(!allAND (true, b(y>5)));
  BOOST_CHECK(allAND (true, b(b)));
  BOOST_CHECK(!allAND (false, b(b)));
}

BOOST_FIXTURE_TEST_CASE(all_or1, Fixture)
{
  BOOST_CHECK(!allOR (b(y>5), false));
  BOOST_CHECK(allOR (b(b), false));
  BOOST_CHECK(allOR (b(y>5), true));
}

BOOST_FIXTURE_TEST_CASE(all_or2, Fixture)
{
  BOOST_CHECK(!allOR (false, b(y>5)));
  BOOST_CHECK(allOR (false, b(b)));
  BOOST_CHECK(allOR (true, b(y>5)));
}

BOOST_FIXTURE_TEST_CASE(any_and4, Fixture)
{
  BOOST_CHECK(anyAND (b(y>5), true));
  BOOST_CHECK(!anyAND (b(y<3), true));
  BOOST_CHECK(!anyAND (b(y>5), false));
}

BOOST_FIXTURE_TEST_CASE(any_and5, Fixture)
{
  BOOST_CHECK(anyAND (true, b(y>5)));
  BOOST_CHECK(!anyAND (true, b(y<3)));
  BOOST_CHECK(!anyAND (false, b(y>5)));
}

BOOST_FIXTURE_TEST_CASE(any_or4, Fixture)
{
  BOOST_CHECK(anyOR (b(y>5), false));
  BOOST_CHECK(!anyOR (b(y<3), false));
  BOOST_CHECK(anyOR (b(y<3), true));
}

BOOST_FIXTURE_TEST_CASE(any_or5, Fixture)
{
  BOOST_CHECK(anyOR (false, b(y>5)));
  BOOST_CHECK(!anyOR (false, b(y<3)));
  BOOST_CHECK(anyOR (true, b(y<3)));
}

BOOST_AUTO_TEST_SUITE_END()
