//# tMArrayUtil.cc: test program for MArrayUtil
//# Copyright (C) 2012
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This library is free software; you can redistribute it and/or modify it
//# under the terms of the GNU Library General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or (at your
//# option) any later version.
//#
//# This library is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
//# License for more details.
//#
//# You should have received a copy of the GNU Library General Public License
//# along with this library; if not, write to the Free Software Foundation,
//# Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#include <casacore/tables/TaQL/MArray.h>
#include <casacore/tables/TaQL/MArrayUtil.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Utilities/Assert.h>
#include <iostream>

using namespace casacore;
using namespace std;

void testReorder()
{
  // Create and fill an array and mask.
  Array<int32_t> arr(IPosition(4,2,3,4,5));
  indgen(arr);
  Array<bool> mask(arr.shape());
  mask = true;
  mask(arr%7==0) = false;
  // Create an unmasked array from it, reorder it, and check the result.
  MArray<int32_t> marr1(arr);
  MArray<int32_t> res1 = reorderArray (marr1, IPosition(2,1,3));
  AlwaysAssertExit (! res1.hasMask());
  for (int i4=0; i4<5; ++i4) {
    for (int i3=0; i3<4; ++i3) {
      for (int i2=0; i2<3; ++i2) {
        for (int i1=0; i1<2; ++i1) {
          int32_t v = res1.array()(IPosition(4,i2,i4,i1,i3));
          AlwaysAssertExit (v == arr(IPosition(4,i1,i2,i3,i4)));
        }
      }
    }
  }
  // Create a masked array from it, reorder it, and check the result.
  MArray<int32_t> marr2(arr, mask);
  MArray<int32_t> res2 = reorderArray (marr2, IPosition(2,3,1));
  AlwaysAssertExit (res2.hasMask());
  for (int i4=0; i4<5; ++i4) {
    for (int i3=0; i3<4; ++i3) {
      for (int i2=0; i2<3; ++i2) {
        for (int i1=0; i1<2; ++i1) {
          int32_t v = res2.array()(IPosition(4,i4,i2,i1,i3));
          AlwaysAssertExit (v == arr(IPosition(4,i1,i2,i3,i4)));
          bool m = res2.mask()(IPosition(4,i4,i2,i1,i3));
          AlwaysAssertExit (m == mask(IPosition(4,i1,i2,i3,i4)));
        }
      }
    }
  }
  // Now reorder without actually changing the order.
  // Check that no copy is made if told so.
  MArray<int32_t> res3 = reorderArray (marr2, IPosition(4,0,1,2,3), false);
  AlwaysAssertExit (res3.hasMask());
  AlwaysAssertExit (arr.data() == res3.array().data());
  AlwaysAssertExit (mask.data() == res3.mask().data());
  // The same but, now with a copy.
  MArray<int32_t> res4 = reorderArray (marr2, IPosition(4,0,1,2,3), true);
  AlwaysAssertExit (res4.hasMask());
  AlwaysAssertExit (arr.data() != res4.array().data());
  AlwaysAssertExit (mask.data() != res4.mask().data());
  AlwaysAssertExit (allEQ(res4.array(), arr));
  AlwaysAssertExit (allEQ(res4.mask(), mask));

  // Check if a reordered null array is null.
  AlwaysAssertExit (reorderArray (MArray<int32_t>(), IPosition()).isNull());
}

void testReverse()
{
  // Create and fill an array and mask.
  Array<int32_t> arr(IPosition(4,2,3,4,5));
  indgen(arr);
  Array<bool> mask(arr.shape());
  mask = true;
  mask(arr%7==0) = false;
  // Create an unmasked array from it, reverse it, and check the result.
  MArray<int32_t> marr1(arr);
  MArray<int32_t> res1 = reverseArray (marr1, IPosition(2,1,3));
  AlwaysAssertExit (! res1.hasMask());
  for (int i4=0; i4<5; ++i4) {
    for (int i3=0; i3<4; ++i3) {
      for (int i2=0; i2<3; ++i2) {
        for (int i1=0; i1<2; ++i1) {
          int32_t v = res1.array()(IPosition(4,i1,2-i2,i3,4-i4));
          AlwaysAssertExit (v == arr(IPosition(4,i1,i2,i3,i4)));
        }
      }
    }
  }
  // Create a masked array from it, reverse it, and check the result.
  MArray<int32_t> marr2(arr, mask);
  MArray<int32_t> res2 = reverseArray (marr2, IPosition(2,3,1));
  AlwaysAssertExit (res2.hasMask());
  for (int i4=0; i4<5; ++i4) {
    for (int i3=0; i3<4; ++i3) {
      for (int i2=0; i2<3; ++i2) {
        for (int i1=0; i1<2; ++i1) {
          int32_t v = res2.array()(IPosition(4,i1,2-i2,i3,4-i4));
          AlwaysAssertExit (v == arr(IPosition(4,i1,i2,i3,i4)));
          bool m = res2.mask()(IPosition(4,i1,2-i2,i3,4-i4));
          AlwaysAssertExit (m == mask(IPosition(4,i1,i2,i3,i4)));
        }
      }
    }
  }
  // Now reverse without actually changing the order.
  // Check that no copy is made if told so.
  MArray<int32_t> res3 = reverseArray (marr2, IPosition(), false);
  AlwaysAssertExit (res3.hasMask());
  AlwaysAssertExit (arr.data() == res3.array().data());
  AlwaysAssertExit (mask.data() == res3.mask().data());
  // The same but, now with a copy.
  MArray<int32_t> res4 = reverseArray (marr2, IPosition(), true);
  AlwaysAssertExit (res4.hasMask());
  AlwaysAssertExit (arr.data() != res4.array().data());
  AlwaysAssertExit (mask.data() != res4.mask().data());
  AlwaysAssertExit (allEQ(res4.array(), arr));
  AlwaysAssertExit (allEQ(res4.mask(), mask));

  // Check if a reverseed null array is null.
  AlwaysAssertExit (reverseArray (MArray<int32_t>(), IPosition()).isNull());
}

int main()
{
  testReorder();
  testReverse();
  return 0;
}
