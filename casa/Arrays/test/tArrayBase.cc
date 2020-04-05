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

#define BOOST_TEST_MODULE array2
#define BOOST_TEST_DYN_LINK

#include <boost/test/unit_test.hpp>

#include "../Array.h"
#include "../ArrayBase.h"
#include "../ArrayMath.h"
#include "../ArrayPosIter.h"
#include "../Slicer.h"

using namespace casacore;

BOOST_AUTO_TEST_SUITE(array_base)

// Check that an array is empty.
void checkEmpty (const ArrayBase& arr)
{
  BOOST_CHECK_EQUAL (arr.ndim(), 0);
  BOOST_CHECK_EQUAL (arr.shape(), IPosition());
  BOOST_CHECK_EQUAL (arr.size(), 0);
  BOOST_CHECK_EQUAL (arr.nelements(), 0);
  BOOST_CHECK (arr.empty());
  BOOST_CHECK_EQUAL (arr.endPosition(), IPosition());
  BOOST_CHECK_EQUAL (arr.steps(), IPosition());
  BOOST_CHECK (arr.contiguousStorage());
}

// Check that an array contains data.
void checkFilled (const ArrayBase& arr)
{
  BOOST_CHECK_EQUAL (arr.ndim(), 3);
  BOOST_CHECK_EQUAL (arr.shape(), IPosition(3,5,6,10));
  BOOST_CHECK_EQUAL (arr.size(), 5*6*10);
  BOOST_CHECK_EQUAL (arr.nelements(), 5*6*10);
  BOOST_CHECK (!arr.empty());
  BOOST_CHECK_EQUAL (arr.endPosition(), IPosition(3,4,5,9));
  BOOST_CHECK_EQUAL (arr.steps(), IPosition(3,1,5,30));
  BOOST_CHECK (arr.contiguousStorage());
}

// Check that the virtual functions throw an exception for a base object.
void checkBaseVF (const ArrayBase& arr)
{
  bool ok = true;
  /*try {
    arr.makeIterator(1);
    ok = false;
  } catch (const std::exception&) {
  }*/
  BOOST_CHECK (ok);
  BOOST_CHECK_THROW(arr.makeArray(), std::exception);
  BOOST_CHECK (ok);
  /*try {
    arr.getSection(Slicer());
    ok = false;
  } catch (const std::exception&) {
  }*/
  BOOST_CHECK (ok);
  ArrayBase arr2(arr);
  BOOST_CHECK_THROW(arr2.resize(IPosition()), std::exception);
  BOOST_CHECK (ok);
  BOOST_CHECK_THROW(arr2.assignBase(arr), std::exception);
  BOOST_CHECK (ok);
}

// Check if the values in the array are correct.
void checkValues (const ArrayBase& arr, int st, size_t nr)
{
  const Array<int>& arri = dynamic_cast<const Array<int>&>(arr);
  BOOST_CHECK_EQUAL (arri.size(), nr);
  for (size_t i=0; i<arri.size(); ++i) {
    BOOST_CHECK_EQUAL (arri.data()[i], int(st+i));
    if(arri.data()[i] != int(st+i))
      break; // stop to prevent dozens of warnings
  }
}

// Test if objects of the base class behave fine.
BOOST_AUTO_TEST_CASE( default_constructor )
{
  // Check if fine for default constructor.
  ArrayBase arr1;
  checkEmpty  (arr1);
  checkBaseVF (arr1);
}

BOOST_AUTO_TEST_CASE( normal_constructor )
{
  // Check if fine for normal constructor.
  ArrayBase arr2 (IPosition(3,5,6,10));
  checkFilled (arr2);
  checkBaseVF (arr2);
}

BOOST_AUTO_TEST_CASE( copy_constructor_empty )
{
  // Check copy constructor.
  ArrayBase arr1;
  ArrayBase arr1c (arr1);
  checkEmpty  (arr1c);
  checkBaseVF (arr1c);
}

BOOST_AUTO_TEST_CASE( copy_constructor_filled )
{
  ArrayBase arr2 (IPosition(3,5,6,10));
  ArrayBase arr2c (arr2);
  checkFilled (arr2c);
  checkBaseVF (arr2c);
}

BOOST_AUTO_TEST_CASE( self_assignment_empty )
{
  // Check self-assignment.
  ArrayBase arr;
  arr.assign(arr);
  checkEmpty  (arr);
  checkBaseVF (arr);
}

BOOST_AUTO_TEST_CASE( self_assignment_filled )
{
  ArrayBase arr2 (IPosition(3,5,6,10));
  ArrayBase arr2c (arr2);
  arr2c.assign(arr2c);
  checkFilled (arr2c);
  checkBaseVF (arr2c);
}

BOOST_AUTO_TEST_CASE( assignment_empty )
{
  ArrayBase arr1;
  ArrayBase arr2 (IPosition(3,5,6,10));
  arr2.assign( arr1 );
  checkEmpty  (arr2);
  checkBaseVF (arr2);
}

BOOST_AUTO_TEST_CASE( assignment_filled )
{
  ArrayBase arr1;
  ArrayBase arr2 (IPosition(3,5,6,10));
  arr1.assign( arr2 );
  checkFilled (arr1);
  checkBaseVF (arr1);
}

// Now test if it works fine for a real array.
BOOST_AUTO_TEST_CASE( array_empty )
{
  std::unique_ptr<ArrayBase> arr0(new Array<int>());
  checkEmpty  (*arr0);
}

BOOST_AUTO_TEST_CASE( array_resize_empty )
{
  // Make a similar array and resize it.
  std::unique_ptr<ArrayBase> arr0(new Array<int>());
  std::unique_ptr<ArrayBase> arr1 = arr0->makeArray();
  arr1->resize (IPosition(3,5,6,10));
  checkFilled (*arr1);
}

BOOST_AUTO_TEST_CASE( array_values1 )
{
  // Store values and check them.
  std::unique_ptr<ArrayBase> arr0(new Array<int>());
  std::unique_ptr<ArrayBase> arr1 = arr0->makeArray();
  arr1->resize (IPosition(3,5,6,10));
  indgen (dynamic_cast<Array<int>&>(*arr1));
  checkValues (*arr1, 0, 300);
}

BOOST_AUTO_TEST_CASE( array_values2 )
{
  // Create and check another array.
  std::unique_ptr<ArrayBase> arr2( new Array<int>(IPosition(3,5,6,10)) );
  indgen (dynamic_cast<Array<int>&>(*arr2),10);
  checkValues (*arr2, 10, 300);
}

BOOST_AUTO_TEST_CASE( make_array )
{
  std::unique_ptr<ArrayBase> arr2( new Array<int>(IPosition(3,5,6,10)) );
  indgen (dynamic_cast<Array<int>&>(*arr2),10);
  checkEmpty  (*arr2->makeArray());
}

BOOST_AUTO_TEST_CASE( array_resize_filled )
{
  std::unique_ptr<ArrayBase> arr2( new Array<int>(IPosition(3,5,6,10)) );
  indgen (dynamic_cast<Array<int>&>(*arr2),10);
  arr2->resize (IPosition());
  checkEmpty  (*arr2);
}

BOOST_AUTO_TEST_CASE( array_resize_assign_base )
{
  std::unique_ptr<ArrayBase> arr0(new Array<int>());
  std::unique_ptr<ArrayBase> arr2( new Array<int>(IPosition(3,5,6,10)) );
  std::unique_ptr<ArrayBase> arr1 = arr0->makeArray();
  arr1->resize (IPosition(3,5,6,10));
  indgen (dynamic_cast<Array<int>&>(*arr1));
  indgen (dynamic_cast<Array<int>&>(*arr2),10);
  arr2->resize (IPosition());
  arr2->assignBase (*arr1);
  checkFilled (*arr2);
  checkValues (*arr2, 0, 300);
}

BOOST_AUTO_TEST_CASE( array_section )
{
  std::unique_ptr<ArrayBase> arr2( new Array<int>(IPosition(3,5,6,10)) );
  indgen (dynamic_cast<Array<int>&>(*arr2));
  // Check if getting a section is fine.
  checkFilled (*arr2->getSection(Slicer(IPosition(3,0), IPosition(3,5,6,10))));
  checkValues (*arr2->getSection(Slicer(IPosition(3,0), IPosition(3,5,6,10))),
               0, 300);
  checkValues (*arr2->getSection(Slicer(IPosition(3,0,0,2), IPosition(3,5,6,4))),
               60, 120);
}

BOOST_AUTO_TEST_CASE( array_assign_base )
{
  std::unique_ptr<ArrayBase> arr0( new Array<int>(IPosition(3,5,6,10)) );
  std::unique_ptr<ArrayBase> arr1 = arr0->makeArray();
  arr1->resize (IPosition(3,5,6,10));
  indgen (dynamic_cast<Array<int>&>(*arr1));
  // Check assign in various ways.
  arr1->assignBase (Array<int>());
  checkEmpty (*arr1);
}

BOOST_AUTO_TEST_CASE( array_assign_reference )
{
  std::unique_ptr<ArrayBase> arr1( new Array<int>(IPosition(3,5,6,10)) );
  arr1->assignBase (Array<int>());
  std::unique_ptr<ArrayBase> arr2( new Array<int>(IPosition(3,5,6,10)) );
  indgen (dynamic_cast<Array<int>&>(*arr2));
  arr1->assignBase (*arr2);
  checkFilled (*arr1);
  // Make sure that after the assign arr1 and arr2 reference different data.
  // TODO this I want to change!
  dynamic_cast<Array<int>&>(*arr1) += 100;
  checkValues (*arr1, 100, 300);
  //checkValues (*arr2, 0, 300);
}

BOOST_AUTO_TEST_CASE( array_iteration )
{
  std::unique_ptr<ArrayBase> arr2( new Array<int>(IPosition(3,5,6,10)) );
  indgen (dynamic_cast<Array<int>&>(*arr2));
  std::unique_ptr<ArrayPositionIterator> iter = arr2->makeIterator(2);
  int nr = 0;
  while (!iter->pastEnd()) {
    checkValues (iter->getArray(), nr*30, 30);
    iter->next();
    nr++;
  }
  BOOST_CHECK_EQUAL (nr, 10);
}

BOOST_AUTO_TEST_CASE( array_assign_wrong_type )
{
  std::unique_ptr<ArrayBase> arr2( new Array<int>(IPosition(3,5,6,10)) );
  indgen (dynamic_cast<Array<int>&>(*arr2));
  // Check that assign of a different array type fails.
  try {
    arr2->assignBase (Array<size_t>());
    BOOST_CHECK(false);
  } catch (const std::exception&) {
    BOOST_CHECK(true);
  }
}

// TODO move construct & move assign

BOOST_AUTO_TEST_SUITE_END()

