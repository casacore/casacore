//# tArrayMath.cc: This program tests the itrators in the ArrayMath class
//# Copyright (C) 2008
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

//# Includes
#include "../Array.h"
#include "../ArrayMath.h"
#include "../ArrayLogical.h"

#include <cstdlib>

#include <boost/test/unit_test.hpp>

using namespace casacore;

BOOST_AUTO_TEST_SUITE(array_math_transform)

BOOST_AUTO_TEST_CASE(test)
{
  IPosition shape(3,10,11,12);
  Array<int> arr1(shape);
  Array<int> arr2(shape);
  Array<int> exp1(shape);
  Array<int> exp2(shape);
  Array<int> expa(shape);
  Array<int> res(shape);
  indgen (arr1, -100);
  indgen (arr2);
  for (size_t i=0; i<arr1.nelements(); ++i) {
    exp1.data()[i] = arr1.data()[i] + arr2.data()[i];
    exp2.data()[i] = arr1.data()[i] + 20;
    expa.data()[i] = std::abs(arr1.data()[i]);
  }

  // First test transform of contiguous arrays.
  arrayContTransform (arr1, arr2, res, std::plus<int>());
  BOOST_CHECK (allEQ (res, exp1));
  arrayContTransform (arr1, 20, res, std::plus<int>());
  BOOST_CHECK (allEQ (res, exp2));
  arrayContTransform (20, arr1, res, std::plus<int>());
  BOOST_CHECK (allEQ (res, exp2));
  arrayContTransform (arr1, res, [](int a){return std::abs(a);});
  BOOST_CHECK (allEQ (res, expa));

  arrayTransform (arr1, arr2, res, std::plus<int>());
  BOOST_CHECK (allEQ (res, exp1));
  arrayTransform (arr1, 20, res, std::plus<int>());
  BOOST_CHECK (allEQ (res, exp2));
  arrayTransform (20, arr1, res, std::plus<int>());
  BOOST_CHECK (allEQ (res, exp2));
  arrayTransform (arr1, res, [](int a){return std::abs(a);});
  BOOST_CHECK (allEQ (res, expa));

  BOOST_CHECK (allEQ (exp1, arrayTransformResult (arr1, arr2,
                                                       std::plus<int>())));
  BOOST_CHECK (allEQ (exp2, arrayTransformResult (arr1, 20,
                                                       std::plus<int>())));
  BOOST_CHECK (allEQ (exp2, arrayTransformResult (20, arr1,
                                                       std::plus<int>())));
  BOOST_CHECK (allEQ (expa, arrayTransformResult (arr1,
    [](int a){return std::abs(a);})));

  res.assign_conforming(arr1);
  arrayTransformInPlace (res, arr2, std::plus<int>());
  BOOST_CHECK (allEQ (res, exp1));
  res.assign_conforming(arr1);
  arrayTransformInPlace (res, 20, std::plus<int>());
  BOOST_CHECK (allEQ (res, exp2));
  res.assign_conforming(arr1);
  arrayTransformInPlace (res, [](int a){return std::abs(a);});
  BOOST_CHECK (allEQ (res, expa));

  // Now test non-contiguous arrays.
  Slicer slicer(IPosition(3,1,2,3), IPosition(3,7,8,9), IPosition(3,2),
                Slicer::endIsLast);
  Array<int> arr1sl (arr1(slicer));
  Array<int> arr2sl (arr2(slicer));
  Array<int> ressl  (res(slicer));
  Array<int> exp1sl (exp1(slicer));
  Array<int> exp2sl (exp2(slicer));
  Array<int> expasl (expa(slicer));

  res.assign_conforming(exp1);
  arrayTransform (arr1sl, arr2sl, ressl, std::plus<int>());
  BOOST_CHECK (allEQ (res, exp1));
  res.assign_conforming(exp2);
  arrayTransform (arr1sl, 20, ressl, std::plus<int>());
  BOOST_CHECK (allEQ (res, exp2));
  arrayTransform (20, arr1sl, ressl, std::plus<int>());
  BOOST_CHECK (allEQ (res, exp2));
  res.assign_conforming(expa);
  arrayTransform (arr1sl, ressl, [](int a){return std::abs(a);});
  BOOST_CHECK (allEQ (res, expa));

  BOOST_CHECK (allEQ (exp1sl, arrayTransformResult (arr1sl, arr2sl,
                                                         std::plus<int>())));
  BOOST_CHECK (allEQ (exp2sl, arrayTransformResult (arr1sl, 20,
                                                         std::plus<int>())));
  BOOST_CHECK (allEQ (exp2sl, arrayTransformResult (20, arr1sl,
                                                         std::plus<int>())));
  BOOST_CHECK (allEQ (expasl, arrayTransformResult (arr1sl,
    [](int a){return std::abs(a);})));

  res.assign_conforming(exp1);
  ressl.assign_conforming(arr1sl);
  arrayTransformInPlace (ressl, arr2sl, std::plus<int>());
  BOOST_CHECK (allEQ (res, exp1));
  res.assign_conforming(exp2);
  ressl.assign_conforming(arr1sl);
  arrayTransformInPlace (ressl, 20, std::plus<int>());
  BOOST_CHECK (allEQ (res, exp2));
  res.assign_conforming(expa);
  ressl.assign_conforming(arr1sl);
  arrayTransformInPlace (res, [](int a){return std::abs(a);});
  BOOST_CHECK (allEQ (res, expa));
}

BOOST_AUTO_TEST_SUITE_END()
