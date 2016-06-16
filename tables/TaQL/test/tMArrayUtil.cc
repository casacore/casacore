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
//#
//# $Id: tMArrayUtil.cc 21273 2012-10-31 15:25:10Z gervandiepen $

#include <casacore/tables/TaQL/MArray.h>
#include <casacore/tables/TaQL/MArrayUtil.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <iostream>

using namespace casacore;
using namespace std;

int main()
{
  // Create and fill an array and mask.
  Array<Int> arr(IPosition(4,2,3,4,5));
  indgen(arr);
  Array<Bool> mask(arr.shape());
  mask = True;
  mask(arr%7==0) = False;
  // Create an unmasked array from it, reorder it, and check the result.
  MArray<Int> marr1(arr);
  MArray<Int> res1 = reorderArray (marr1, IPosition(2,1,3));
  AlwaysAssertExit (! res1.hasMask());
  for (int i4=0; i4<5; ++i4) {
    for (int i3=0; i3<4; ++i3) {
      for (int i2=0; i2<3; ++i2) {
        for (int i1=0; i1<2; ++i1) {
          Int v = res1.array()(IPosition(4,i2,i4,i1,i3));
          AlwaysAssertExit (v == arr(IPosition(4,i1,i2,i3,i4)));
        }
      }
    }
  }
  // Create a masked array from it, reorder it, and check the result.
  MArray<Int> marr2(arr, mask);
  MArray<Int> res2 = reorderArray (marr2, IPosition(2,3,1));
  AlwaysAssertExit (res2.hasMask());
  for (int i4=0; i4<5; ++i4) {
    for (int i3=0; i3<4; ++i3) {
      for (int i2=0; i2<3; ++i2) {
        for (int i1=0; i1<2; ++i1) {
          Int v = res2.array()(IPosition(4,i4,i2,i1,i3));
          AlwaysAssertExit (v == arr(IPosition(4,i1,i2,i3,i4)));
          Bool m = res2.mask()(IPosition(4,i4,i2,i1,i3));
          AlwaysAssertExit (m == mask(IPosition(4,i1,i2,i3,i4)));
        }
      }
    }
  }
  // Now reorder without actually changing the order.
  // Check that no copy is made if told so.
  MArray<Int> res3 = reorderArray (marr2, IPosition(4,0,1,2,3), False);
  AlwaysAssertExit (res3.hasMask());
  AlwaysAssertExit (arr.data() == res3.array().data());
  AlwaysAssertExit (mask.data() == res3.mask().data());
  // The same but, now with a copy.
  MArray<Int> res4 = reorderArray (marr2, IPosition(4,0,1,2,3), True);
  AlwaysAssertExit (res4.hasMask());
  AlwaysAssertExit (arr.data() != res4.array().data());
  AlwaysAssertExit (mask.data() != res4.mask().data());

  // Check if a reordered null array is null.
  AlwaysAssertExit (reorderArray (MArray<Int>(), IPosition()).isNull());

  return 0;
}
