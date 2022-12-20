//# tDiagonal.cc: Test program for the Array diagonals function
//# Copyright (C) 2012
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
//#include "../ArrayIO.h"
#include "../MatrixIter.h"

#include <boost/test/unit_test.hpp>

#include <cstdint>
#include <limits>

using namespace casacore;

BOOST_AUTO_TEST_SUITE(diagonal)

// Get the diagonals directly from the 4-dim array.
// The order of the values is the same as when using Array::diagonals.
Vector<int> getDiag (const Array<int>& arr, int axis, int off)
{
  const IPosition& shp = arr.shape();
  int dlen = shp[axis] - abs(off);
  Vector<int> vec(arr.size() / shp[axis] / shp[axis] * dlen);
  int* vecp = vec.data();
  int i[4];
  for (i[3]=0; i[3]<shp[3]; ++i[3]) {
    for (i[2]=0; i[2]<shp[2]; ++i[2]) {
      for (i[1]=0; i[1]<shp[1]; ++i[1]) {
        for (i[0]=0; i[0]<shp[0]; ++i[0]) {
          if (i[axis] == i[axis+1]-off) {
            *vecp++ = arr(IPosition(4,i[0],i[1],i[2],i[3]));
          }
        }
      }
    }
  }
  BOOST_CHECK (vecp == vec.data() + vec.size());
  return vec;
}

// Check if all diagonals using a given axis are correct.
void checkDiagonals (const Array<int>& arr, size_t axis)
{
  const IPosition& shp = arr.shape();
  // Determine the size of the axes before the first diagonals axis.
  // This is the step size to be used in the MatrixIterator loop.
  int step = 1;
  for (size_t i=0; i<axis; ++i) {
    step *= shp[i];
  }
  int len = shp[axis];
  // Get the diagonals for all possible offsets below and above main diagonal.
  for (int off=-len+1; off<len; ++off) {
    /// for (int off=0; off<1; ++off) {
    int diagLen = len - abs(off);
    Array<int> darr = arr.diagonals(axis, off);
    // Copy them to a consecutive array (to see if copy works fine).
    Array<int> carr (darr.copy());
    // Also get them directly for comparison.
    Vector<int> diags = getDiag(arr, axis, off);
    ///cout <<"diag steps="<< darr.steps()<<endl;
    ///cout << darr;
    int nr = 0;
    int st = 0;
    // Now iterate as matrix through the array and get diagonal of each of them.
    // Check if they are the same as result of getDiag.
    // Also check if the same as result of diagonals() by iterating as vector.
    ReadOnlyVectorIterator<int> diter(darr, axis);
    ReadOnlyVectorIterator<int> citer(carr, axis);
    ReadOnlyMatrixIterator<int> miter(arr, axis, axis+1);
    for (; !miter.pastEnd(); miter.next(), diter.next(), citer.next()) {
      Matrix<int> marr(miter.matrix());
      ///cout << marr;
      Vector<int> dmarr(marr.diagonal(off));
      ///cout << dmarr << endl;
      ///cout << "slice="<<st<<' '<<diagLen<<' '<<step<<' '<<axis<<endl;
      BOOST_CHECK (allEQ(dmarr, diter.vector()));
      BOOST_CHECK (allEQ(dmarr, citer.vector()));
      ///cout << dmarr << diags(Slice(st, diagLen, step)) << endl;
      BOOST_CHECK (allEQ(dmarr, diags(Slice(st, diagLen, step))));
      ++st;
      // If all first axes before diagonal axis are processed, go to the
      // next axis after the diagonal axis.
      if (++nr == step) {
        nr = 0;
        st += (diagLen-1) * step;
      }
    }
    BOOST_CHECK (diter.pastEnd());
    BOOST_CHECK (citer.pastEnd());
  } 
}

void fullCompare (const Array<int>& darr, const Array<int>& arr, const std::vector<int>& values)
{
  // Compare the diagonals of axis 1.
  // Both directly and using Array::iterator to see if that works fine.
  std::vector<int>::const_iterator valIter = values.begin();
  for (auto darrVal : darr) {
    BOOST_CHECK_EQUAL( darrVal , *valIter);
    ++valIter;
  }
  // Also print the diagonals using getDiag and directly.
  // Both for main diagonal and for other diagonals.
  Vector<int> getDiagRes = getDiag(arr, 1, 0);
  BOOST_CHECK_EQUAL_COLLECTIONS(getDiagRes.begin(), getDiagRes.end(), darr.begin(), darr.end());
  
  std::vector<int> arrDiagValues{
    arr(IPosition(4,0,0,1,0)),
    arr(IPosition(4,1,0,1,0)),
    arr(IPosition(4,0,1,2,0)),
    arr(IPosition(4,1,1,2,0)),
    arr(IPosition(4,0,0,1,1)),
    arr(IPosition(4,1,0,1,1)),
    arr(IPosition(4,0,1,2,1)),
    arr(IPosition(4,1,1,2,1))
  };
  getDiagRes.assign( getDiag(arr, 1, 1) );
  BOOST_CHECK_EQUAL_COLLECTIONS(getDiagRes.begin(), getDiagRes.end(), arrDiagValues.begin(), arrDiagValues.end());
  BOOST_CHECK_EQUAL_COLLECTIONS(getDiagRes.begin(), getDiagRes.end(), valIter, valIter+8);
  valIter += 8;
  
  arrDiagValues = std::vector<int>{
    arr(IPosition(4,0,1,0,0)),
    arr(IPosition(4,1,1,0,0)),
    arr(IPosition(4,0,2,1,0)),
    arr(IPosition(4,1,2,1,0)),
    arr(IPosition(4,0,1,0,1)),
    arr(IPosition(4,1,1,0,1)),
    arr(IPosition(4,0,2,1,1)),
    arr(IPosition(4,1,2,1,1))
  };
  getDiagRes.assign( getDiag(arr, 1, -1) );
  BOOST_CHECK_EQUAL_COLLECTIONS(getDiagRes.begin(), getDiagRes.end(), arrDiagValues.begin(), arrDiagValues.end());
  BOOST_CHECK_EQUAL_COLLECTIONS(getDiagRes.begin(), getDiagRes.end(), valIter, valIter+8);
  valIter += 8;
  
  arrDiagValues = std::vector<int>{
    arr(IPosition(4,0,0,2,0)),
    arr(IPosition(4,1,0,2,0)),
    arr(IPosition(4,0,0,2,1)),
    arr(IPosition(4,1,0,2,1))
  };
  getDiagRes.assign( getDiag(arr, 1, 2) );
  BOOST_CHECK_EQUAL_COLLECTIONS(getDiagRes.begin(), getDiagRes.end(), arrDiagValues.begin(), arrDiagValues.end());
  BOOST_CHECK_EQUAL_COLLECTIONS(getDiagRes.begin(), getDiagRes.end(), valIter, valIter+4);
  valIter += 4;
  
  arrDiagValues = std::vector<int>{
    arr(IPosition(4,0,2,0,0)),
    arr(IPosition(4,1,2,0,0)),
    arr(IPosition(4,0,2,0,1)),
    arr(IPosition(4,1,2,0,1))
  };
  getDiagRes.assign( getDiag(arr, 1, -2) );
  BOOST_CHECK_EQUAL_COLLECTIONS(getDiagRes.begin(), getDiagRes.end(), arrDiagValues.begin(), arrDiagValues.end());
  BOOST_CHECK_EQUAL_COLLECTIONS(getDiagRes.begin(), getDiagRes.end(), valIter, valIter+4);
  valIter += 4;
  BOOST_CHECK(valIter == values.end());
}

BOOST_AUTO_TEST_CASE( full )
{
  Array<int> arr(IPosition(4,2,3,3,2));
  BOOST_CHECK_EQUAL (arr.steps(), IPosition(4,1,2,6,18));
  indgen (arr);
  // Get the diagonals and check if shape and steps are correct.
  Array<int> darr(arr.diagonals(1));
  BOOST_CHECK_EQUAL (darr.shape(), IPosition(3,2,3,2));
  BOOST_CHECK_EQUAL (darr.steps(), IPosition(3,1,8,18));
  fullCompare (darr, arr, std::vector<int>{
    0, 1, 8, 9, 16, 17, 18, 19, 26, 27, 34, 35,
    6, 7, 14, 15, 24, 25, 32, 33,
    2, 3, 10, 11, 20, 21, 28, 29,
    12, 13, 30, 31,
    4, 5, 22, 23
  });
  checkDiagonals (arr, 1);
  // Now test if for an array where all axes have the same length, so
  // we can get the diagonals for all axes.
  Array<int> arr2(IPosition(4,5,5,5,5));
  indgen (arr2);
  for (int ax=0; ax<3; ++ax) {
    checkDiagonals (arr2, ax);
  }
}

BOOST_AUTO_TEST_CASE( subset )
{
  // Do the same for a subset (with same shape as testFull) of a larger array.
  Array<int> farr(IPosition(4,6,6,12,8));
  indgen (farr);
  Array<int> arr (farr(IPosition(4,1,1,1,3), IPosition(4,4,3,7,5),
                       IPosition(4,2,1,3,2)));
  BOOST_CHECK (arr.steps() == IPosition(4,2,6,36*3,36*12*2));
  Array<int> darr(arr.diagonals(1));
  BOOST_CHECK (darr.shape() == IPosition(3,2,3,2));
  ///  BOOST_CHECK (darr.steps() == IPosition(3,1,8,18));
  fullCompare (darr, arr, std::vector<int>{ 
    1339, 1341, 1453, 1455, 1567, 1569, 2203, 2205, 2317, 2319, 2431, 2433,
    1447, 1449, 1561, 1563, 2311, 2313, 2425, 2427,
    1345, 1347, 1459, 1461, 2209, 2211, 2323, 2325,
    1555, 1557, 2419, 2421,
    1351, 1353, 2215, 2217
  });
  checkDiagonals (arr, 1);
  // Now test if for a subset where all axes have the same length, so
  // we can get the diagonals for all axes.
  Array<int> arr2 (farr(IPosition(4,1,0,2,3), IPosition(4,5,4,10,7),
                        IPosition(4,1,1,2,1)));
  indgen (arr2);
  for (int ax=0; ax<3; ++ax) {
    checkDiagonals (arr2, ax);
  }
}

BOOST_AUTO_TEST_CASE( diag_subset )
{
  // Do the same for the diagonals of a subset (with same shape as testFull)
  // of a larger array.
  Array<int> farr(IPosition(5,12,12,12,12,12));
  indgen (farr);
  Array<int> sarr (farr(IPosition(5,1,2,3,4,5), IPosition(5,7,8,9,10,11),
                        IPosition(5,2,2,2,2,2)));
  Array<int> arr (sarr.diagonals(2));
  Array<int> darr(arr.diagonals(1));
  BOOST_CHECK (darr.shape() == IPosition(3,4,4,4));
  // Also get main diagonal in another way.
  BOOST_CHECK (allEQ(darr, getDiag(arr, 1, 0).reform(darr.shape())));
  checkDiagonals (arr, 1);
  // Do the same to check if a contiguous copy gives the same result.
  Array<int> arr2(arr.copy());
  Array<int> darr2(arr2.diagonals(1));
  BOOST_CHECK (allEQ(darr2, darr));
  BOOST_CHECK (allEQ(darr2, getDiag(arr2, 1, 0).reform(darr2.shape())));
  checkDiagonals (arr2, 1);
  // Test if all give the same result.
  for (int ax1=0; ax1<4; ++ax1) {
    Array<int> arr3 (sarr.diagonals(ax1));
    Array<int> arr4 (arr3(IPosition(4,1,1,1,1), IPosition(4,3,3,3,3),
                          IPosition(4,2,2,2,2)));
    for (int ax=0; ax<3; ++ax) {
      checkDiagonals (arr3, ax);
      // Also for a subset of the diagonals.
      checkDiagonals (arr4, ax);
    }
  }
}

BOOST_AUTO_TEST_SUITE_END()
