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
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

//# Includes
#include <casa/Arrays/Array.h>
#include <casa/Arrays/ArrayOpsDiffShapes.h>
#include <casa/Arrays/ArrayLogical.h>
#include <casa/Arrays/IPosition.h>
#include <casa/Utilities/Assert.h>
#include <casa/iostream.h>

#include <casa/namespace.h>

void test_reformedMask(Bool truthval, uInt ni = 3,
		       uInt nj = 4, uInt nk = 5, uInt nl = 6)
{
  cout << "test_reformedMask(" << truthval << ", "
       << ni << ", " << nj << ", " << nk << ", " << nl << "):" << endl;
  
  Array<Bool> data(IPosition(4, ni, nj, nk, nl));
  const IPosition expectedShape(2, ni, nj);
  Array<Bool> expected(expectedShape);

  for(uInt i = 0; i < ni; ++i){
    for(uInt j = 0; j < nj; ++j){
      Bool expected_val = (i + j) % 2 ? true : false;

      expected(IPosition(2, i, j)) = expected_val;
      for(uInt k = 0; k < nk; ++k){
	for(uInt l = 0; l < nl; ++l){
	  if(expected_val)	// Make sure [0, 0] gets truthval in case nk ==
				// nl == 1.
	    data(IPosition(4, i, j, k, l)) = (k + l) % 2 ? !truthval : truthval;
	  else
	    data(IPosition(4, i, j, k, l)) = !truthval;
	}
      }
    }
  }

  Bool allequal = allEQ(reformedMask(data, truthval, expectedShape), expected);
  
  cout << "\t" << (allequal ? "OK" : "FAILURE")
       << ":\tallEQ(reformedMask(data, " << truthval << ", " << expectedShape
       << "), expected) = " << allequal << endl;
  
  AlwaysAssertExit (allequal);
}

void test_binOpExpanders(uInt ni = 2, uInt nj = 3, uInt nk = 4)
{
  cout << "test_binOpExpanders(" << ni << ", " << nj << ", "
       << nk << "):" << endl;

  const IPosition leftShape(3, ni, nj, nk);
  Array<Double> left(leftShape);
  Array<Float> right(IPosition(2, ni, nj));
  Array<Double> expected(leftShape);
  
  for(uInt i = 0; i < ni; ++i){
    for(uInt j = 0; j < nj; ++j){
      Double rightval = 2.0 + 10.0 * (i + 10.0 * j);	// [  2 102 202 ]
                                                        // [ 12 112 212 ]
      right(IPosition(2, i, j)) = rightval;             // [ 22 122 222 ]
      for(uInt k = 0; k < nk; ++k){
	const IPosition ijk(3, i, j, k);
	Double leftval = 1.0 + 0.1 * (i + 0.1 * (j + 0.1 * k));

	left(ijk)     = leftval;
	expected(ijk) = leftval + rightval;

	Double leftcheck = left(ijk);
	Double rightcheck = expected(ijk);
	Double diff = rightcheck - leftcheck - rightval;
	    
	if(diff > 1e-6 || diff < -1e-6){
	  cout << "(" << i << ", " << j << ", " << k << "):\n"
	       << "\tlv, lc = " << leftval << ", " << leftcheck << endl;
	  cout << "\trv, rc = " << rightval << ", " << rightcheck << endl;
	}
      }
    }
  }
  //const Array<Double> constleft(left);
  //AlwaysAssertExit(allEQ(binOpExpandR(left, right, std::plus<Double>()),
  //			 expected));
  binOpExpandInPlace(left, right, std::plus<Double>());

  Bool allequal = allEQ(left, expected);
  
  cout << "\t" << (allequal ? "OK" : "FAILURE")
       << ":\tallEQ(left, expected) = " << allequal << endl;

  if(!allequal){
    for(uInt i = 0; i < ni; ++i){
      cout << i << endl;
      for(uInt j = 0; j < nj; ++j){
	cout << "\t" << j << endl;
	for(uInt k = 0; k < nk; ++k){
	  const IPosition ijk(3, i, j, k);

	  cout << "\t\t" << k << "\t" << left(ijk) << "\t" << expected(ijk) << endl;
	}
      }
    }
  }  
  
  AlwaysAssertExit (allequal);
}

int main()
{
  try {
    for(uInt i = 0; i <= 1; ++i){
      for(uInt j = 0; j <= 1; ++j){
	test_reformedMask(true, 3, 4, 1 + 3 * i, 1 + 4 * i);
	test_reformedMask(false, 1 + 4 * j, 1 + 2 * j, 1 + 4 * i, 1 + 3 * i);
	test_binOpExpanders(1 + i, 2 + j, 4);
      }
    }
  } catch (AipsError x) {
    cout << "Error: " << x.getMesg() << endl;
    return 1;
  } catch (...) {
    cout << "Unknown exception caught." << endl;
    return 2;
  }
  cout << "All tests passed!" << endl;
  return 0;
}
