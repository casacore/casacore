//# tMArray.cc: test program for MArray
//# Copyright (C) 2015
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
//# $Id: tMArrayMath.cc 21262 2012-09-07 12:38:36Z gervandiepen $

#include <casacore/tables/TaQL/MArray.h>
#include <casacore/tables/TaQL/MArrayMath.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Utilities/Assert.h>
#include <iostream>

using namespace casacore;
using namespace std;

void testExcp()
{
  cout << "Testing MArray exceptions ..." << endl;
  Array<Int> arr(Array<Int>(IPosition(1,1)));
  Bool err = False;
  try {
    MArray<Int> a1(arr, Array<Bool>(IPosition(1,2)));
  } catch (const AipsError& x) {
    err = True;
    cout << x.what() << endl;
  }
  AlwaysAssertExit (err);
  err = False;
  try {
    MArray<Int> a1(arr, Array<Bool>(IPosition(1,1)), True);
  } catch (const AipsError& x) {
    err = True;
    cout << x.what() << endl;
  }
  AlwaysAssertExit (err);
  err = False;
  try {
    MArray<Int> a1(arr);
    a1.setMask (Array<Bool>(IPosition(1,2)));
  } catch (const AipsError& x) {
    err = True;
    cout << x.what() << endl;
  }
}

void testNull()
{
  cout << "Testing null MArray ..." << endl;
  // Create null array.
  MArray<Int> a1;
  AlwaysAssertExit (a1.isNull());
  AlwaysAssertExit (a1.empty());
  AlwaysAssertExit (! a1.hasMask());
  AlwaysAssertExit (a1.shape().empty());
  AlwaysAssertExit (a1.ndim() == 0);
  AlwaysAssertExit (a1.size() == 0);
  // Resize it.
  a1.resize (IPosition(2,3,4), False);
  AlwaysAssertExit (! a1.isNull());
  AlwaysAssertExit (! a1.hasMask());
  AlwaysAssertExit (! a1.shape().empty());
  AlwaysAssertExit (a1.ndim() == 2);
  AlwaysAssertExit (a1.size() == 12);
  AlwaysAssertExit (a1.nelements() == 12);
  // Copy constructor.
  MArray<Int> a2(a1);
  AlwaysAssertExit (! a2.isNull());
  AlwaysAssertExit (! a2.hasMask());
  AlwaysAssertExit (! a2.shape().empty());
  AlwaysAssertExit (a2.ndim() == 2);
  AlwaysAssertExit (a2.size() == 12);
  AlwaysAssertExit (a2.array().data() == a1.array().data());
  // Assignment.
  MArray<Int> a3(Array<Int>(IPosition(2,3,4), 1),
                 Array<Bool>(IPosition(2,3,4), True));
  a1 = a3;
  AlwaysAssertExit (! a1.isNull());
  AlwaysAssertExit (a1.hasMask());
  AlwaysAssertExit (a1.ndim() == 2);
  AlwaysAssertExit (a1.size() == 12);
  AlwaysAssertExit (allEQ (a1.array(), 1));
  AlwaysAssertExit (allTrue (a1.mask()));
  AlwaysAssertExit (a2.array().data() == a1.array().data());
  AlwaysAssertExit (allEQ (a2.array(), 1));
  AlwaysAssertExit (! a2.hasMask());
  // Reference.
  a1.reference (MArray<Int>());
  AlwaysAssertExit (a1.isNull());
  AlwaysAssertExit (! a1.hasMask());
  AlwaysAssertExit (a1.shape().empty());
  AlwaysAssertExit (a1.ndim() == 0);
  AlwaysAssertExit (a1.size() == 0);
  AlwaysAssertExit (allEQ (a2.array(), 1));
  // Empty array.
  MArray<Int> a4(Array<Int>(), Array<Bool>(), False);
  AlwaysAssertExit (! a4.isNull());
  AlwaysAssertExit (a4.empty());
  AlwaysAssertExit (! a4.hasMask());
  AlwaysAssertExit (a4.shape().empty());
  AlwaysAssertExit (a4.ndim() == 0);
  AlwaysAssertExit (a4.size() == 0);
}

void testMask()
{
  cout << "Testing masked MArray ..." << endl;
  IPosition shp(3,5,4,3);
  Array<Int> arr(shp);
  indgen(arr);
  Array<Bool> maskArr(arr%3 == 0);
  Array<Bool> maskArr2(arr%5 == 0);
  // Create with a mask.
  MArray<Int> a1(arr, maskArr);
  AlwaysAssertExit (! a1.isNull());
  AlwaysAssertExit (a1.hasMask());
  AlwaysAssertExit (a1.shape() == shp);
  AlwaysAssertExit (a1.ndim() == 3);
  AlwaysAssertExit (a1.size() == 60);
  AlwaysAssertExit (a1.nvalid() == 40);
  AlwaysAssertExit (a1.array().data() == arr.data());
  AlwaysAssertExit (a1.mask().data() == maskArr.data());
  // Create another array.
  MArray<Int> a2(arr.copy(), maskArr2.copy());
  indgen (a2.array(), 100);
  AlwaysAssertExit (a2.array().data() != arr.data());
  AlwaysAssertExit (a2.mask().data() != maskArr2.data());
  // Test copy constructor.
  MArray<Int> a3(a2);
  AlwaysAssertExit (! a3.isNull());
  AlwaysAssertExit (a3.hasMask());
  AlwaysAssertExit (a3.shape() == shp);
  AlwaysAssertExit (a3.ndim() == 3);
  AlwaysAssertExit (a3.size() == 60);
  AlwaysAssertExit (a3.nvalid() == 48);
  AlwaysAssertExit (a3.array().data() == a2.array().data());
  AlwaysAssertExit (a3.mask().data() == a2.mask().data());
  // Test assignment to empty array.
  MArray<Int> a4;
  AlwaysAssertExit (a4.isNull());
  AlwaysAssertExit (! a4.hasMask());
  a4 = a1;
  AlwaysAssertExit (! a4.isNull());
  AlwaysAssertExit (a4.hasMask());
  AlwaysAssertExit (a4.shape() == shp);
  AlwaysAssertExit (a4.ndim() == 3);
  AlwaysAssertExit (a4.size() == 60);
  AlwaysAssertExit (a4.nvalid() == 40);
  AlwaysAssertExit (allEQ (a4.array(), a1.array()));
  AlwaysAssertExit (allEQ (a4.mask(), a1.mask()));
  // Test assignment to non-empty array.
  a4 = a3;
  AlwaysAssertExit (allEQ (a4.array(), a2.array()));
  AlwaysAssertExit (allEQ (a4.mask(), a2.mask()));
  AlwaysAssertExit (a4.array().data() != a2.array().data());
  AlwaysAssertExit (a4.mask().data() != a2.mask().data());
  // Test reference.
  a4.reference (a3);
  AlwaysAssertExit (! a4.isNull());
  AlwaysAssertExit (a4.hasMask());
  AlwaysAssertExit (a4.shape() == shp);
  AlwaysAssertExit (a4.ndim() == 3);
  AlwaysAssertExit (a4.size() == 60);
  AlwaysAssertExit (a4.nvalid() == 48);
  AlwaysAssertExit (a4.array().data() == a2.array().data());
  AlwaysAssertExit (a4.mask().data() == a2.mask().data());
  a4.reference (MArray<Int>());
  AlwaysAssertExit (a4.isNull());
  AlwaysAssertExit (! a4.hasMask());
  AlwaysAssertExit (a4.shape() == IPosition());
  AlwaysAssertExit (a4.ndim() == 0);
  AlwaysAssertExit (a4.size() == 0);
  AlwaysAssertExit (a4.nvalid() == 0);
  // Test flatten.
  Vector<Int> flat = a1.flatten();
  AlwaysAssertExit (flat.size() == 40);
  AlwaysAssertExit (sum(flat) == (1+58+2+59)*20/2);
  // Test combineMask
  Array<Bool> cmask(a1.combineMask(a2));
  AlwaysAssertExit (allEQ(cmask, arr%3==0 || arr%5==0));
  // Test removeMask.
  a3.removeMask();
  AlwaysAssertExit (! a3.isNull());
  AlwaysAssertExit (! a3.hasMask());
  AlwaysAssertExit (a3.shape() == shp);
  AlwaysAssertExit (a3.ndim() == 3);
  AlwaysAssertExit (a3.size() == 60);
  AlwaysAssertExit (a3.nvalid() == 60);
}

void testFill()
{
  cout << "Testing MArray fill ..." << endl;
  IPosition shp1(3,5,4,3);
  Array<Int> arr1(shp1);
  indgen(arr1);
  IPosition shp2(2,6,2);
  Array<Int> arr2(shp2);
  indgen(arr2, 100);
  Array<Bool> maskArr(arr1%3 == 0);
  // Create.
  MArray<Int> a1(arr1, maskArr);
  MArray<Int> a2(arr2);
  MArray<Short> a3;
  AlwaysAssertExit (a3.isNull());
  AlwaysAssertExit (! a3.hasMask());
  // Test fill from another MArray with another type.
  a3.fill (a1);
  AlwaysAssertExit (! a3.isNull());
  AlwaysAssertExit (a3.hasMask());
  AlwaysAssertExit (a3.shape() == shp1);
  AlwaysAssertExit (a3.mask().data() == a1.mask().data());
  AlwaysAssertExit (sum(a3) == sum(a1));
  // Test fill from another MArray with the same type.
  a1.fill (a2);
  AlwaysAssertExit (! a1.isNull());
  AlwaysAssertExit (! a1.hasMask());
  AlwaysAssertExit (a1.shape() == shp2);
  AlwaysAssertExit (sum(a1) == sum(a2));
  AlwaysAssertExit (a1.array().data() != a2.array().data());
  AlwaysAssertExit (allEQ(a1.array(), a2.array()));
  AlwaysAssertExit (a1.mask().data() == a2.mask().data());
  a1.fill (Array<Short>());
  AlwaysAssertExit (! a1.isNull());
  AlwaysAssertExit (! a1.hasMask());
  AlwaysAssertExit (a1.shape().empty());
  AlwaysAssertExit (a1.empty());
}

void testSlice()
{
  cout << "Testing MArray slice ..." << endl;
  IPosition shp(3,5,4,3);
  Array<Int> arr(shp);
  indgen(arr);
  Array<Bool> maskArr(arr%3 == 0);
  MArray<Int> a1(arr, maskArr);
  // Create slice.
  IPosition st(3,1,1,1);
  IPosition end(3,4,2,1);
  IPosition incr(3,2,1,1);
  MArray<Int> a2(a1(st, end, incr));
  AlwaysAssertExit (a2.hasMask());
  AlwaysAssertExit (a2.size() == 4);
  AlwaysAssertExit (a2.nvalid() == 3);
  AlwaysAssertExit (allEQ(a2.array(), arr(st, end, incr)));
  AlwaysAssertExit (allEQ(a2.mask(), maskArr(st, end, incr)));
}

int main()
{
  try {
    testExcp();
    testNull();
    testMask();
    testFill();
    testSlice();
  } catch (AipsError& x) {
    cout << "Unexpected exception: " << x.what() << endl;
    return 1;
  }
  cout << "OK" << endl;
  return 0;
}
