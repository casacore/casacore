//# tArrayBase.cc: Test program for the Array class
//# Copyright (C) 2013
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
//# $Id: tArrayBase.cc 21335 2013-03-28 14:20:18Z gervandiepen $

//# If AIPS_DEBUG is not set, the Assert's won't be called.
#if !defined(AIPS_DEBUG)
#define AIPS_DEBUG
#endif

#include <casacore/casa/iostream.h>

#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Arrays/ArrayError.h>

#include <casacore/casa/namespace.h>

// Check that an array is empty.
void checkEmpty (const ArrayBase& arr)
{
  AlwaysAssertExit (arr.ndim() == 0);
  AlwaysAssertExit (arr.shape() == IPosition());
  AlwaysAssertExit (arr.size() == 0);
  AlwaysAssertExit (arr.nelements() == 0);
  AlwaysAssertExit (arr.empty());
  AlwaysAssertExit (arr.endPosition() == IPosition());
  AlwaysAssertExit (arr.steps() == IPosition());
  AlwaysAssertExit (arr.contiguousStorage());
}

// Check that an array contains data.
void checkFilled (const ArrayBase& arr)
{
  AlwaysAssertExit (arr.ndim() == 3);
  AlwaysAssertExit (arr.shape() == IPosition(3,5,6,10));
  AlwaysAssertExit (arr.size() == 5*6*10);
  AlwaysAssertExit (arr.nelements() == 5*6*10);
  AlwaysAssertExit (!arr.empty());
  AlwaysAssertExit (arr.endPosition() == IPosition(3,4,5,9));
  AlwaysAssertExit (arr.steps() == IPosition(3,1,5,30));
  AlwaysAssertExit (arr.contiguousStorage());
}

// Check that the virtual functions throw an exception for a base object.
void checkBaseVF (const ArrayBase& arr)
{
  Bool ok = True;
  try {
    arr.makeIterator(1);
    ok = False;
  } catch (const std::exception&) {
  }
  AlwaysAssertExit (ok);
  try {
    arr.makeArray();
    ok = False;
  } catch (const std::exception&) {
  }
  AlwaysAssertExit (ok);
  try {
    arr.getSection(Slicer());
    ok = False;
  } catch (const std::exception&) {
  }
  AlwaysAssertExit (ok);
  ArrayBase arr2(arr);
  try {
    arr2.resize(IPosition());
    ok = False;
  } catch (const std::exception&) {
  }
  AlwaysAssertExit (ok);
  try {
    arr2.assignBase(arr);
    ok = False;
  } catch (const std::exception&) {
  }
  AlwaysAssertExit (ok);
}

// Check if the values in the array are correct.
void checkValues (const ArrayBase& arr, Int st, uInt nr)
{
  const Array<Int>& arri = dynamic_cast<const Array<Int>&>(arr);
  AlwaysAssertExit (arri.size() == nr);
  for (uInt i=0; i<arri.size(); ++i) {
    AlwaysAssertExit (arri.data()[i] == Int(st+i));
  }
}


// Test if objects of the base class behave fine.
void testBase()
{
  // Check if fine for default constructor.
  ArrayBase arr1;
  checkEmpty  (arr1);
  checkBaseVF (arr1);
  // Check if fine for normal constructor.
  ArrayBase arr2 (IPosition(3,5,6,10));
  checkFilled (arr2);
  checkBaseVF (arr2);
  // Check copy constructor.
  ArrayBase arr1c (arr1);
  checkEmpty  (arr1c);
  checkBaseVF (arr1c);
  ArrayBase arr2c (arr2);
  checkFilled (arr2c);
  checkBaseVF (arr2c);
  // Check self-assignment.
  arr1c = arr1c;
  checkEmpty  (arr1c);
  checkBaseVF (arr1c);
  arr2c = arr2c;
  checkFilled (arr2c);
  checkBaseVF (arr2c);
  // Check assignment.
  arr2c = arr1;
  checkEmpty  (arr2c);
  checkBaseVF (arr2c);
  arr1c = arr2;
  checkFilled (arr1c);
  checkBaseVF (arr1c);
}

// Now test if it works fine for a real array.
void testArr()
{
  // Check the array is empty.
  CountedPtr<ArrayBase> arr0 = new Array<Int>();
  checkEmpty  (*arr0);
  // Make a similar array and resize it.
  CountedPtr<ArrayBase> arr1 = arr0->makeArray();
  arr1->resize (IPosition(3,5,6,10));
  checkFilled (*arr1);
  // Store values and check them.
  indgen (dynamic_cast<Array<Int>&>(*arr1));
  checkValues (*arr1, 0, 300);
  // Create and check another array.
  CountedPtr<ArrayBase> arr2 = new Array<Int>(IPosition(3,5,6,10));
  checkFilled (*arr2);
  indgen (dynamic_cast<Array<Int>&>(*arr2),10);
  checkValues (*arr2, 10, 300);
  checkEmpty  (*arr2->makeArray());
  // Check resize and assign.
  arr2->resize (IPosition());
  checkEmpty  (*arr2);
  arr2->assignBase (*arr1);
  checkFilled (*arr2);
  checkValues (*arr2, 0, 300);
  // Check if getting a section is fine.
  checkFilled (*arr2->getSection(Slicer(IPosition(3,0), IPosition(3,5,6,10))));
  checkValues (*arr2->getSection(Slicer(IPosition(3,0), IPosition(3,5,6,10))),
               0, 300);
  checkValues (*arr2->getSection(Slicer(IPosition(3,0,0,2), IPosition(3,5,6,4))),
               60, 120);
  // Check assign in various ways.
  arr1->assignBase (Array<Int>());
  checkEmpty (*arr1);
  arr1->assignBase (*arr2);
  checkFilled (*arr1);
  // Make sure that after the assign arr1 and arr2 reference different data.
  dynamic_cast<Array<Int>&>(*arr1) += 100;
  checkValues (*arr1, 100, 300);
  checkValues (*arr2, 0, 300);
  // Check iteration.
  CountedPtr<ArrayPositionIterator> iter = arr2->makeIterator(2);
  int nr = 0;
  while (!iter->pastEnd()) {
    checkValues (iter->getArray(), nr*30, 30);
    iter->next();
    nr++;
  }
  AlwaysAssertExit (nr==10);
  // Check that assign of a different array type fails.
  Bool ok = True;
  try {
    arr2->assignBase (Array<uInt>());
    ok = False;
  } catch (const std::exception&) {
  }
  AlwaysAssertExit (ok);
}

int main()
{
  try {
    testBase();
    testArr();
  } catch (const AipsError& x) {
    cout << "\nCaught an unexpected exception: " << x.getMesg() << endl;
    return 1;
  } 
  cout << "OK" << endl;
  return 0;
}
