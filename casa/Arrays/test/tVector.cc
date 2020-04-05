//# tVector.cc: Test program for the Vector class
//# Copyright (C) 2015
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
//# $Id: tArray.cc 21521 2014-12-10 08:06:42Z gervandiepen $

//#include "../ArrayIO.h"
#include "../Vector.h"

#include <boost/test/unit_test.hpp>

#include <sstream>

using namespace std;
using namespace casacore;

BOOST_AUTO_TEST_SUITE(vector)

BOOST_AUTO_TEST_CASE( vector_int_io )
{
  Vector<int> vvec1{1,2,4,-2};
  std::ostringstream str;
  str << vvec1;
  BOOST_CHECK_EQUAL(str.str(), "[1, 2, 4, -2]");
}

BOOST_AUTO_TEST_CASE( vector_double_io )
{
  Vector<double> vvec2(std::vector<int>{1,2,4,-2});
  std::ostringstream str;
  str << vvec2;
  BOOST_CHECK_EQUAL(str.str(), "[1, 2, 4, -2]");
}

BOOST_AUTO_TEST_SUITE_END()
