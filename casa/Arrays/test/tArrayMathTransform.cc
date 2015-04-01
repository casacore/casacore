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
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/iostream.h>

using namespace casacore;

void doIt()
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
  for (uInt i=0; i<arr1.nelements(); ++i) {
    exp1.data()[i] = arr1.data()[i] + arr2.data()[i];
    exp2.data()[i] = arr1.data()[i] + 20;
    expa.data()[i] = std::abs(arr1.data()[i]);
  }

  // First test transform of contiguous arrays.
  arrayContTransform (arr1, arr2, res, std::plus<int>());
  AlwaysAssertExit (allEQ (res, exp1));
  arrayContTransform (arr1, 20, res, std::plus<int>());
  AlwaysAssertExit (allEQ (res, exp2));
  arrayContTransform (20, arr1, res, std::plus<int>());
  AlwaysAssertExit (allEQ (res, exp2));
  arrayContTransform (arr1, res, Abs<int>());
  AlwaysAssertExit (allEQ (res, expa));

  arrayTransform (arr1, arr2, res, std::plus<int>());
  AlwaysAssertExit (allEQ (res, exp1));
  arrayTransform (arr1, 20, res, std::plus<int>());
  AlwaysAssertExit (allEQ (res, exp2));
  arrayTransform (20, arr1, res, std::plus<int>());
  AlwaysAssertExit (allEQ (res, exp2));
  arrayTransform (arr1, res, Abs<int>());
  AlwaysAssertExit (allEQ (res, expa));

  AlwaysAssertExit (allEQ (exp1, arrayTransformResult (arr1, arr2,
                                                       std::plus<int>())));
  AlwaysAssertExit (allEQ (exp2, arrayTransformResult (arr1, 20,
                                                       std::plus<int>())));
  AlwaysAssertExit (allEQ (exp2, arrayTransformResult (20, arr1,
                                                       std::plus<int>())));
  AlwaysAssertExit (allEQ (expa, arrayTransformResult (arr1,
                                                       Abs<int>())));

  res = arr1;
  arrayTransformInPlace (res, arr2, std::plus<int>());
  AlwaysAssertExit (allEQ (res, exp1));
  res = arr1;
  arrayTransformInPlace (res, 20, std::plus<int>());
  AlwaysAssertExit (allEQ (res, exp2));
  res = arr1;
  arrayTransformInPlace (res, Abs<int>());
  AlwaysAssertExit (allEQ (res, expa));

  // Now test non-contiguous arrays.
  Slicer slicer(IPosition(3,1,2,3), IPosition(3,7,8,9), IPosition(3,2),
                Slicer::endIsLast);
  Array<int> arr1sl (arr1(slicer));
  Array<int> arr2sl (arr2(slicer));
  Array<int> ressl  (res(slicer));
  Array<int> exp1sl (exp1(slicer));
  Array<int> exp2sl (exp2(slicer));
  Array<int> expasl (expa(slicer));

  res = exp1;
  arrayTransform (arr1sl, arr2sl, ressl, std::plus<int>());
  AlwaysAssertExit (allEQ (res, exp1));
  res = exp2;
  arrayTransform (arr1sl, 20, ressl, std::plus<int>());
  AlwaysAssertExit (allEQ (res, exp2));
  arrayTransform (20, arr1sl, ressl, std::plus<int>());
  AlwaysAssertExit (allEQ (res, exp2));
  res = expa;
  arrayTransform (arr1sl, ressl, Abs<int>());
  AlwaysAssertExit (allEQ (res, expa));

  AlwaysAssertExit (allEQ (exp1sl, arrayTransformResult (arr1sl, arr2sl,
                                                         std::plus<int>())));
  AlwaysAssertExit (allEQ (exp2sl, arrayTransformResult (arr1sl, 20,
                                                         std::plus<int>())));
  AlwaysAssertExit (allEQ (exp2sl, arrayTransformResult (20, arr1sl,
                                                         std::plus<int>())));
  AlwaysAssertExit (allEQ (expasl, arrayTransformResult (arr1sl,
                                                         Abs<int>())));

  res   = exp1;
  ressl = arr1sl;
  arrayTransformInPlace (ressl, arr2sl, std::plus<int>());
  AlwaysAssertExit (allEQ (res, exp1));
  res   = exp2;
  ressl = arr1sl;
  arrayTransformInPlace (ressl, 20, std::plus<int>());
  AlwaysAssertExit (allEQ (res, exp2));
  res   = expa;
  ressl = arr1sl;
  arrayTransformInPlace (res, Abs<int>());
  AlwaysAssertExit (allEQ (res, expa));
}

int main()
{
  try {
    doIt();
  } catch (AipsError x) {
    cout << "Unexpected exception: " << x.getMesg() << endl;
    return 1;
  }
  cout << "OK" << endl;
  return 0;
}
