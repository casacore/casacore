//# tArrayLogical.cc: Test program for Array logical operators
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

#include "../IPosition.h"
#include "../Array.h"
#include "../ArrayMath.h"
#include "../ArrayLogical.h"
//#include "../ArrayIO.h"
#include "../Vector.h"
#include "../Matrix.h"
#include "../Cube.h"
#include "../ArrayError.h"
#include "../LogiVector.h"

#include <boost/test/unit_test.hpp>

using namespace casacore;

BOOST_AUTO_TEST_SUITE(array_logical)

template<typename T>
void Compare(const T& l, const std::string& str)
{
  BOOST_CHECK_EQUAL(to_string(l), str);
}

BOOST_AUTO_TEST_CASE( array_position_iterator )
{
  Vector<int> x(5, 1), y(5);
  LogicalVector b(5), c(5);
  indgen (y);

  BOOST_CHECK (allSame(x));
  BOOST_CHECK (!allSame(y));

  b.assign_conforming (x <= y);
  Compare(b, "[0, 1, 1, 1, 1]");

  b.assign_conforming (x < y);
  Compare(b, "[0, 0, 1, 1, 1]");

  b.assign_conforming (x >= y);
  Compare(b, "[1, 1, 0, 0, 0]");

  b.assign_conforming (x > y);
  Compare(b, "[1, 0, 0, 0, 0]");

  b.assign_conforming (x == y);
  Compare(b, "[0, 1, 0, 0, 0]");

  b.assign_conforming (x != y);
  Compare(b, "[1, 0, 1, 1, 1]");

  b.assign_conforming ((const Array<int> &)y <= 1);
  Compare(b, "[1, 1, 0, 0, 0]");

  b.assign_conforming ((const Array<int> &)y < 1);
  Compare(b, "[1, 0, 0, 0, 0]");

  b.assign_conforming ((const Array<int> &)y >= 1);
  Compare(b, "[0, 1, 1, 1, 1]");

  b.assign_conforming ((const Array<int> &)y > 1);
  Compare(b, "[0, 0, 1, 1, 1]");

  b.assign_conforming ((const Array<int> &)y == 1);
  Compare(b, "[0, 1, 0, 0, 0]");

  b.assign_conforming ((const Array<int> &)y != 1);
  Compare(b, "[1, 0, 1, 1, 1]");

  b.assign_conforming (1 <= (const Array<int> &)y);
  Compare(b, "[0, 1, 1, 1, 1]");

  b.assign_conforming (1 < (const Array<int> &)y);
  Compare(b, "[0, 0, 1, 1, 1]");

  b.assign_conforming (1 >= (const Array<int> &)y);
  Compare(b, "[1, 1, 0, 0, 0]");

  b.assign_conforming (1 > (const Array<int> &)y);
  Compare(b, "[1, 0, 0, 0, 0]");

  b.assign_conforming (1 == (const Array<int> &)y);
  Compare(b, "[0, 1, 0, 0, 0]");

  b.assign_conforming (1 != (const Array<int> &)y);
  Compare(b, "[1, 0, 1, 1, 1]");

  b.assign_conforming (! ((const Array<int> &)y >= 3));
  c.assign_conforming   ((const Array<int> &)y <  3);
  Compare(b, "[1, 1, 1, 0, 0]");
  Compare(c, "[1, 1, 1, 0, 0]");

  Vector<double> x1(2, 10000), x2(2);
  x2(0) = 10001; x2(1) = 10002;
  Compare(near(x1, x2, 0.99e-4), "[0, 0]");
  Compare(near(x1, x2, 1.01e-4), "[1, 0]");
  Compare(near(x1, x2, 2.01e-4), "[1, 1]");
  Compare(nearAbs(x1, x2, 0.99), "[0, 0]");
  Compare(nearAbs(x1, x2, 1.01), "[1, 0]");
  Compare(nearAbs(x1, x2, 2.01), "[1, 1]");
}

BOOST_AUTO_TEST_SUITE_END()
