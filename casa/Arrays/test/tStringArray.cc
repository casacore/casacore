//# tStringArray.cc: This program tests Array<std::string> and related classes
//# Copyright (C) 1993,1994,1995,2001
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

//# Includes


#include "../Array.h"
#include "../Vector.h"
#include "../Matrix.h"
#include "../Cube.h"
#include "../ArrayError.h"

#include <string>

#include <boost/test/unit_test.hpp>

using namespace casacore;

BOOST_AUTO_TEST_SUITE(string_array)

BOOST_AUTO_TEST_CASE( vector_string )
{
  Vector<std::string> vs(5);
  vs(0) = "Every";vs(1) = "Good";vs(2) = "Boy";vs(3) = "Deserves";
  vs(4) = "Fudge";
  BOOST_CHECK_EQUAL(vs(0), "Every");
  BOOST_CHECK_EQUAL(vs(1), "Good");
  BOOST_CHECK_EQUAL(vs(2), "Boy");
  BOOST_CHECK_EQUAL(vs(3), "Deserves");
  BOOST_CHECK_EQUAL(vs(4), "Fudge");
}

BOOST_AUTO_TEST_SUITE_END()
