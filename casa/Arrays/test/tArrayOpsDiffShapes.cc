//# tArrayOpsDiffShapes.cc: This program tests the ArrayMath class
//# Copyright (C) 2009
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
#include "../ArrayOpsDiffShapes.h"
#include "../ArrayLogical.h"
#include "../IPosition.h"

#include <boost/test/unit_test.hpp>

using namespace casacore;

BOOST_AUTO_TEST_SUITE(array_ops_diff_shapes)

void test_reformedMask(bool truthval, size_t ni = 3,
		       size_t nj = 4, size_t nk = 5, size_t nl = 6)
{
  Array<bool> data(IPosition(4, ni, nj, nk, nl));
  const IPosition expectedShape(2, ni, nj);
  Array<bool> expected(expectedShape);

  for(size_t i = 0; i < ni; ++i){
    for(size_t j = 0; j < nj; ++j){
      bool expected_val = (i + j) % 2 ? true : false;

      expected(IPosition(2, i, j)) = expected_val;
      for(size_t k = 0; k < nk; ++k){
        for(size_t l = 0; l < nl; ++l){
          if(expected_val)	// Make sure [0, 0] gets truthval in case nk ==
            // nl == 1.
            data(IPosition(4, i, j, k, l)) = (k + l) % 2 ? !truthval : truthval;
          else
            data(IPosition(4, i, j, k, l)) = !truthval;
        }
      }
    }
  }

  bool allequal = allEQ(reformedMask(data, truthval, expectedShape), expected);
  
  BOOST_CHECK (allequal);
}

void test_binOpExpanders(size_t ni = 2, size_t nj = 3, size_t nk = 4)
{
  const IPosition leftShape(3, ni, nj, nk);
  Array<double> left(leftShape);
  Array<float> right(IPosition(2, ni, nj));
  Array<double> expected(leftShape);
  
  for(size_t i = 0; i < ni; ++i){
    for(size_t j = 0; j < nj; ++j){
      double rightval = 2.0 + 10.0 * (i + 10.0 * j);	// [  2 102 202 ]
                                                        // [ 12 112 212 ]
      right(IPosition(2, i, j)) = rightval;             // [ 22 122 222 ]
      for(size_t k = 0; k < nk; ++k){
	const IPosition ijk(3, i, j, k);
	double leftval = 1.0 + 0.1 * (i + 0.1 * (j + 0.1 * k));

	left(ijk)     = leftval;
	expected(ijk) = leftval + rightval;
      }
    }
  }
  //const Array<double> constleft(left);
  //BOOST_CHECK(allEQ(binOpExpandR(left, right, std::plus<double>()),
  //			 expected));
  binOpExpandInPlace(left, right, std::plus<double>());

  bool allequal = allEQ(left, expected);
  
  BOOST_CHECK (allequal);
}

BOOST_AUTO_TEST_CASE(test)
{
  for(size_t i = 0; i <= 1; ++i){
    for(size_t j = 0; j <= 1; ++j){
      test_reformedMask(true, 3, 4, 1 + 3 * i, 1 + 4 * i);
      test_reformedMask(false, 1 + 4 * j, 1 + 2 * j, 1 + 4 * i, 1 + 3 * i);
      test_binOpExpanders(1 + i, 2 + j, 4);
    }
  }
}

BOOST_AUTO_TEST_SUITE_END()
