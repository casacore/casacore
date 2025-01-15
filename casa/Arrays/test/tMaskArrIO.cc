//# tMaskArrIO.cc: Test program for MaskedArray IO
//# Copyright (C) 1994,1995,1996,1998,1999,2000,2001
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

#include "../MaskedArray.h"
#include "../MaskArrMath.h"
#include "../MaskArrIO.h"
#include "../Array.h"
#include "../ArrayLogical.h"
#include "../ArrayMath.h"
//#include "../ArrayIO.h"
#include "../Vector.h"
#include "../Matrix.h"
#include "../Cube.h"
#include "../ArrayError.h"

#include <boost/test/unit_test.hpp>

using namespace casacore;

BOOST_AUTO_TEST_SUITE(mask_array_io)

BOOST_AUTO_TEST_CASE(masked_vector)
{
  const char* reference[5] = {
    "Array: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]\n"
    "Mask:  [1, 1, 1, 1, 1, 1, 1, 0, 0, 0]",
    "Array: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]\n"
    "Mask:  [1, 1, 1, 1, 1, 1, 0, 0, 0, 0]",
    "Array: [2, 3, 4, 5, 6, 7, 8, 9, 10, 11]\n"
    "Mask:  [1, 1, 1, 1, 1, 0, 0, 0, 0, 0]",
    "Array: [3, 4, 5, 6, 7, 8, 9, 10, 11, 12]\n"
    "Mask:  [1, 1, 1, 1, 0, 0, 0, 0, 0, 0]",
    "Array: [4, 5, 6, 7, 8, 9, 10, 11, 12, 13]\n"
    "Mask:  [1, 1, 1, 0, 0, 0, 0, 0, 0, 0]"
  };
  
  Vector<int> a(10);
  for (int orig0=0; orig0 < 5; orig0++) {
    indgen (a, orig0);
    std::ostringstream str;
    str << a(a<7);
    BOOST_CHECK_EQUAL(str.str(), reference[orig0]);
  }
}


BOOST_AUTO_TEST_CASE(masked_matrix)
{
  Matrix<int> a(10u,3u);
  indgen (a, 0);
  std::ostringstream str;
  str << a(a<7);
  BOOST_CHECK_EQUAL(str.str(),
    "Array: Axis Lengths: [10, 3]  (NB: Matrix in Row/Column order)\n"
    "[0, 10, 20\n"
    " 1, 11, 21\n"
    " 2, 12, 22\n"
    " 3, 13, 23\n"
    " 4, 14, 24\n"
    " 5, 15, 25\n"
    " 6, 16, 26\n"
    " 7, 17, 27\n"
    " 8, 18, 28\n"
    " 9, 19, 29]\n"
    "\n"
    "Mask:  Axis Lengths: [10, 3]  (NB: Matrix in Row/Column order)\n"
    "[1, 0, 0\n"
    " 1, 0, 0\n"
    " 1, 0, 0\n"
    " 1, 0, 0\n"
    " 1, 0, 0\n"
    " 1, 0, 0\n"
    " 1, 0, 0\n"
    " 0, 0, 0\n"
    " 0, 0, 0\n"
    " 0, 0, 0]\n");
}

BOOST_AUTO_TEST_CASE(masked_cube)
{
  Cube<int> a(10,3,4);
  indgen (a, 0);
  std::ostringstream str;
  str << a(a<7);
  BOOST_CHECK_EQUAL(str.str(),
    "Array: Ndim=3 Axis Lengths: [10, 3, 4] \n"
    "[0, 0, 0][0, 1, 2, 3, 4, 5, 6, 7, 8, 9]\n"
    "[0, 1, 0][10, 11, 12, 13, 14, 15, 16, 17, 18, 19]\n"
    "[0, 2, 0][20, 21, 22, 23, 24, 25, 26, 27, 28, 29]\n"
    "[0, 0, 1][30, 31, 32, 33, 34, 35, 36, 37, 38, 39]\n"
    "[0, 1, 1][40, 41, 42, 43, 44, 45, 46, 47, 48, 49]\n"
    "[0, 2, 1][50, 51, 52, 53, 54, 55, 56, 57, 58, 59]\n"
    "[0, 0, 2][60, 61, 62, 63, 64, 65, 66, 67, 68, 69]\n"
    "[0, 1, 2][70, 71, 72, 73, 74, 75, 76, 77, 78, 79]\n"
    "[0, 2, 2][80, 81, 82, 83, 84, 85, 86, 87, 88, 89]\n"
    "[0, 0, 3][90, 91, 92, 93, 94, 95, 96, 97, 98, 99]\n"
    "[0, 1, 3][100, 101, 102, 103, 104, 105, 106, 107, 108, 109]\n"
    "[0, 2, 3][110, 111, 112, 113, 114, 115, 116, 117, 118, 119]\n"
    "\n"
    "Mask:  Ndim=3 Axis Lengths: [10, 3, 4] \n"
    "[0, 0, 0][1, 1, 1, 1, 1, 1, 1, 0, 0, 0]\n"
    "[0, 1, 0][0, 0, 0, 0, 0, 0, 0, 0, 0, 0]\n"
    "[0, 2, 0][0, 0, 0, 0, 0, 0, 0, 0, 0, 0]\n"
    "[0, 0, 1][0, 0, 0, 0, 0, 0, 0, 0, 0, 0]\n"
    "[0, 1, 1][0, 0, 0, 0, 0, 0, 0, 0, 0, 0]\n"
    "[0, 2, 1][0, 0, 0, 0, 0, 0, 0, 0, 0, 0]\n"
    "[0, 0, 2][0, 0, 0, 0, 0, 0, 0, 0, 0, 0]\n"
    "[0, 1, 2][0, 0, 0, 0, 0, 0, 0, 0, 0, 0]\n"
    "[0, 2, 2][0, 0, 0, 0, 0, 0, 0, 0, 0, 0]\n"
    "[0, 0, 3][0, 0, 0, 0, 0, 0, 0, 0, 0, 0]\n"
    "[0, 1, 3][0, 0, 0, 0, 0, 0, 0, 0, 0, 0]\n"
    "[0, 2, 3][0, 0, 0, 0, 0, 0, 0, 0, 0, 0]\n"
  );
}

BOOST_AUTO_TEST_SUITE_END()
