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
//#
//# $Id$

//# If AIPS_DEBUG is not set, the Assert's won't be called.
#if !defined(AIPS_DEBUG)
#define AIPS_DEBUG
#endif

//# For extra debugging
#if !defined(AIPS_ARRAY_INDEX_CHECK)
#define AIPS_ARRAY_INDEX_CHECK
#endif

#include <casa/iostream.h>

#include <casa/aips.h>
#include <casa/BasicSL/String.h>
#include <casa/BasicSL/Complex.h>
#include <casa/BasicMath/Math.h>
#include <casa/Utilities/Assert.h>

#include <casa/Arrays/IPosition.h>
#include <casa/Arrays/Array.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/ArrayIO.h>
#include <casa/Arrays/MatrixIter.h>

using namespace casa;
using namespace std;

// Get the diagonals directly from the 4-dim array.
// The order of the values is the same as when using Array::diagonals.
Vector<Int> getDiag (const Array<Int>& arr, Int axis, Int off)
{
  const IPosition& shp = arr.shape();
  Int dlen = shp[axis] - abs(off);
  Vector<Int> vec(arr.size() / shp[axis] / shp[axis] * dlen);
  Int* vecp = vec.data();
  Int i[4];
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
  AlwaysAssertExit (vecp == vec.data() + vec.size());
  return vec;
}

// Check if all diagonals using a given axis are correct.
void checkDiagonals (const Array<Int>& arr, uInt axis)
{
  ///cout << "checkdiagonals"<<endl;
  const IPosition& shp = arr.shape();
  // Determine the size of the axes before the first diagonals axis.
  // This is the step size to be used in the MatrixIterator loop.
  Int step = 1;
  for (uInt i=0; i<axis; ++i) {
    step *= shp[i];
  }
  Int len = shp[axis];
  // Get the diagonals for all possible offsets below and above main diagonal.
  for (Int off=-len+1; off<len; ++off) {
    /// for (Int off=0; off<1; ++off) {
    Int diagLen = len - abs(off);
    Array<Int> darr = arr.diagonals(axis, off);
    // Copy them to a consecutive array (to see if copy works fine).
    Array<Int> carr (darr.copy());
    // Also get them directly for comparison.
    Vector<Int> diags = getDiag(arr, axis, off);
    ///cout <<"diag steps="<< darr.steps()<<endl;
    ///cout << darr;
    Int nr = 0;
    Int st = 0;
    // Now iterate as matrix through the array and get diagonal of each of them.
    // Check if they are the same as result of getDiag.
    // Also check if the same as result of diagonals() by iterating as vector.
    ReadOnlyVectorIterator<Int> diter(darr, axis);
    ReadOnlyVectorIterator<Int> citer(carr, axis);
    ReadOnlyMatrixIterator<Int> miter(arr, axis, axis+1);
    for (; !miter.pastEnd(); miter.next(), diter.next(), citer.next()) {
      Matrix<Int> marr(miter.matrix());
      ///cout << marr;
      Vector<Int> dmarr(marr.diagonal(off));
      ///cout << dmarr << endl;
      ///cout << "slice="<<st<<' '<<diagLen<<' '<<step<<' '<<axis<<endl;
      AlwaysAssertExit (allEQ(dmarr, diter.vector()));
      AlwaysAssertExit (allEQ(dmarr, citer.vector()));
      ///cout << dmarr << diags(Slice(st, diagLen, step)) << endl;
      AlwaysAssertExit (allEQ(dmarr, diags(Slice(st, diagLen, step))));
      ++st;
      // If all first axes before diagonal axis are processed, go to the
      // next axis after the diagonal axis.
      if (++nr == step) {
        nr = 0;
        st += (diagLen-1) * step;
      }
    }
    AlwaysAssertExit (diter.pastEnd());
    AlwaysAssertExit (citer.pastEnd());
  } 
}

void printDiag (const Array<Int>& darr, const Array<Int>& arr)
{
  // Print the diagonals of axis 1.
  // Both directly and using Array::iterator to see if that works fine.
  cout<<darr<<endl;
  cout << "diagonals  ";
  for (Array<Int>::const_iterator iter=darr.begin();
       iter!=darr.end(); ++iter) {
    cout << *iter << "  ";
  }
  cout << endl;
  // Also print the diagonals using getDiag and directly.
  // Both for main diagonal and for other diagonals.
  cout << "getDiag-0 " << getDiag(arr, 1, 0) << endl;
  cout << "direct+1   "
       << arr(IPosition(4,0,0,1,0)) << "  "
       << arr(IPosition(4,1,0,1,0)) << "  "
       << arr(IPosition(4,0,1,2,0)) << "  "
       << arr(IPosition(4,1,1,2,0)) << "  "
       << arr(IPosition(4,0,0,1,1)) << "  "
       << arr(IPosition(4,1,0,1,1)) << "  "
       << arr(IPosition(4,0,1,2,1)) << "  "
       << arr(IPosition(4,1,1,2,1)) << endl;
  cout << "getDiag+1 "  << getDiag(arr, 1, 1) << endl;
  cout << "direct-1   "
       << arr(IPosition(4,0,1,0,0)) << "  "
       << arr(IPosition(4,1,1,0,0)) << "  "
       << arr(IPosition(4,0,2,1,0)) << "  "
       << arr(IPosition(4,1,2,1,0)) << "  "
       << arr(IPosition(4,0,1,0,1)) << "  "
       << arr(IPosition(4,1,1,0,1)) << "  "
       << arr(IPosition(4,0,2,1,1)) << "  "
       << arr(IPosition(4,1,2,1,1)) << endl;
  cout << "getDiag-1 " << getDiag(arr, 1, -1) << endl;
  cout << "direct+2   "
       << arr(IPosition(4,0,0,2,0)) << "  "
       << arr(IPosition(4,1,0,2,0)) << "  "
       << arr(IPosition(4,0,0,2,1)) << "  "
       << arr(IPosition(4,1,0,2,1)) << endl;
  cout << "getDiag+2 " << getDiag(arr, 1, 2) << endl;
  cout << "direct-2   "
       << arr(IPosition(4,0,2,0,0)) << "  "
       << arr(IPosition(4,1,2,0,0)) << "  "
       << arr(IPosition(4,0,2,0,1)) << "  "
       << arr(IPosition(4,1,2,0,1)) << endl;
  cout << "getDiag-2 " << getDiag(arr, 1, -2) << endl;
}

void testFull()
{
  cout << endl << "Test full ..." << endl;
  Array<Int> arr(IPosition(4,2,3,3,2));
  AlwaysAssertExit (arr.steps() == IPosition(4,1,2,6,18));
  indgen (arr);
  // Get the diagonals and check if shape and steps are correct.
  Array<Int> darr(arr.diagonals(1));
  AlwaysAssertExit (darr.shape() == IPosition(3,2,3,2));
  AlwaysAssertExit (darr.steps() == IPosition(3,1,8,18));
  printDiag (darr, arr);
  checkDiagonals (arr, 1);
  // Now test if for an array where all axes have the same length, so
  // we can get the diagonals for all axes.
  Array<Int> arr2(IPosition(4,5,5,5,5));
  indgen (arr2);
  for (Int ax=0; ax<3; ++ax) {
    checkDiagonals (arr2, ax);
  }
}

void testSubset()
{
  // Do the same for a subset (with same shape as testFull) of a larger array.
  cout << endl << "Test subset ..." << endl;
  Array<Int> farr(IPosition(4,6,6,12,8));
  indgen (farr);
  Array<Int> arr (farr(IPosition(4,1,1,1,3), IPosition(4,4,3,7,5),
                       IPosition(4,2,1,3,2)));
  AlwaysAssertExit (arr.steps() == IPosition(4,2,6,36*3,36*12*2));
  Array<Int> darr(arr.diagonals(1));
  AlwaysAssertExit (darr.shape() == IPosition(3,2,3,2));
  ///  AlwaysAssertExit (darr.steps() == IPosition(3,1,8,18));
  cout<<darr<<endl;
  printDiag (darr, arr);
  checkDiagonals (arr, 1);
  // Now test if for a subset where all axes have the same length, so
  // we can get the diagonals for all axes.
  Array<Int> arr2 (farr(IPosition(4,1,0,2,3), IPosition(4,5,4,10,7),
                        IPosition(4,1,1,2,1)));
  indgen (arr2);
  for (Int ax=0; ax<3; ++ax) {
    checkDiagonals (arr2, ax);
  }
}

void testDiag()
{
  // Do the same for the diagonals of a subset (with same shape as testFull)
  // of a larger array.
  cout << endl << "Test diag subset ..." << endl;
  Array<Int> farr(IPosition(5,12,12,12,12,12));
  indgen (farr);
  Array<Int> sarr (farr(IPosition(5,1,2,3,4,5), IPosition(5,7,8,9,10,11),
                        IPosition(5,2,2,2,2,2)));
  Array<Int> arr (sarr.diagonals(2));
  Array<Int> darr(arr.diagonals(1));
  AlwaysAssertExit (darr.shape() == IPosition(3,4,4,4));
  // Also get main diagonal in another way.
  AlwaysAssertExit (allEQ(darr, getDiag(arr, 1, 0).reform(darr.shape())));
  checkDiagonals (arr, 1);
  // Do the same to check if a contiguous copy gives the same result.
  Array<Int> arr2(arr.copy());
  Array<Int> darr2(arr2.diagonals(1));
  AlwaysAssertExit (allEQ(darr2, darr));
  AlwaysAssertExit (allEQ(darr2, getDiag(arr2, 1, 0).reform(darr2.shape())));
  checkDiagonals (arr2, 1);
  // Test if all give the same result.
  for (Int ax1=0; ax1<4; ++ax1) {
    Array<Int> arr3 (sarr.diagonals(ax1));
    Array<Int> arr4 (arr3(IPosition(4,1,1,1,1), IPosition(4,3,3,3,3),
                          IPosition(4,2,2,2,2)));
    for (Int ax=0; ax<3; ++ax) {
      checkDiagonals (arr3, ax);
      // Also for a subset of the diagonals.
      checkDiagonals (arr4, ax);
    }
  }
}

int main()
{
  try {
    // Test if taking diagonals of a contiguous array works fine.
    testFull();
    // Test if taking diagonals of a subset of an array works fine.
    testSubset();
    // Test if taking diagonals of diagonals works fine.
    testDiag();
  } catch (std::exception& x) {
    cerr << "Unexpected exception: " << x.what() << endl;
    return 1;
  }
  return 0;
}
