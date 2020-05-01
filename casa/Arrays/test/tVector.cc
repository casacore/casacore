//# tVector.cc: Test program for the Vector class
//# Copyright (C) 2015
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
//# $Id: tArray.cc 21521 2014-12-10 08:06:42Z gervandiepen $

//#include "../ArrayIO.h"
#include "../Vector.h"

#include <boost/test/unit_test.hpp>

#include <sstream>

using namespace std;
using namespace casacore;

BOOST_AUTO_TEST_SUITE(vector_class)

BOOST_AUTO_TEST_CASE( int_io )
{
  Vector<int> vvec1{1,2,4,-2};
  std::ostringstream str;
  str << vvec1;
  BOOST_CHECK_EQUAL(str.str(), "[1, 2, 4, -2]");
}

BOOST_AUTO_TEST_CASE( double_io )
{
  Vector<double> vvec2(std::vector<int>{1,2,4,-2});
  std::ostringstream str;
  str << vvec2;
  BOOST_CHECK_EQUAL(str.str(), "[1, 2, 4, -2]");
}

BOOST_AUTO_TEST_CASE( vector_init )
{
  Vector<int> x(10);
  x = 5;     
  for (int i=0; i < 10; i++)
    BOOST_CHECK_EQUAL(x(i), 5);
}

BOOST_AUTO_TEST_CASE( copy_constructor )
{
  Vector<int> x(10);
  x = 5;     
	Vector<int> y(x);
	BOOST_CHECK(x.nrefs() == y.nrefs() && x.nrefs() > 1);
	for (int i=0; i < 10; i++)
    BOOST_CHECK_EQUAL(y(i), 5);
}

BOOST_AUTO_TEST_CASE( copy )
{
  Vector<int> x(10);
  x = 5;     
  Vector<int> z(x.copy());
  z = 11;
  for (int i=0; i < 10; i++)
  {
    BOOST_CHECK_EQUAL(z(i), 11);
    BOOST_CHECK_EQUAL(x(i), 5);
  }
}

BOOST_AUTO_TEST_CASE( slice1 )
{
  Vector<int> x(10);
  x = 5;     
  Vector<int> z(x.copy());
  z = 11;
  // The semantics of this changed; before, this could be done with the assignment operator
	x(Slice(0,5,2)).assign_conforming(z(Slice(0,5,2)));
	for(int i=0; i < 10; i += 2)
  {
    BOOST_CHECK_EQUAL(x(i), 11);
    BOOST_CHECK_EQUAL(x(i+1), 5);
  }
}

BOOST_AUTO_TEST_CASE( slice2 )
{
  Vector<float> x1(5);
  indgen(x1);
  BOOST_CHECK(allEQ(x1(Slice(0,2,2)), Vector<float>{0, 2}));
}

BOOST_AUTO_TEST_CASE( default_constructor )
{
	Vector<int> zz;
	BOOST_CHECK_EQUAL(zz.nelements(),0);
	BOOST_CHECK_EQUAL(zz.size(), 0);
	BOOST_CHECK(zz.empty());
}

BOOST_AUTO_TEST_CASE( init_constructor )
{
	Vector<int> y1(5, 4);
	BOOST_CHECK (allEQ(y1, 4));
}

BOOST_AUTO_TEST_CASE( slice_constructor )
{
  Vector<int> x(10, 5);
  Vector<int> z(10, 11);
  // The semantics of this changed; before, this could be done with the assignment operator
	x(Slice(0,5,2)).assign_conforming(z(Slice(0,5,2)));
	Vector<int> zzz(x(Slice(0,5,2)));
	zzz.unique();
	BOOST_CHECK_EQUAL(zzz.nrefs(), 1);
  BOOST_CHECK(allEQ(zzz, 11));
  BOOST_CHECK_EQUAL(zzz.nelements(), 5);
	BOOST_CHECK_EQUAL(zzz.size(), 5);
	BOOST_CHECK(!zzz.empty());
}

BOOST_AUTO_TEST_CASE( string )
{
	Vector<std::string> vs(5);
	vs(0) = "Every";vs(1) = "Good";vs(2) = "Boy";vs(3) = "Deserves";
	vs(4) = "Fudge";
	BOOST_CHECK_EQUAL(vs(0), "Every");
	BOOST_CHECK_EQUAL(vs(1), "Good");
	BOOST_CHECK_EQUAL(vs(2), "Boy");
	BOOST_CHECK_EQUAL(vs(3), "Deserves");
	BOOST_CHECK_EQUAL(vs(4), "Fudge");
}

BOOST_AUTO_TEST_CASE( resize )
{
  Vector<int> x(10, 5);
  Vector<int> z(10, 11);
  // The semantics of this changed; before, this could be done with the assignment operator
	x(Slice(0,5,2)).assign_conforming(z(Slice(0,5,2)));
	Vector<int> zzz(x(Slice(0,5,2)));
	zzz.unique();
	zzz.resize(10);
	zzz = 13;
	BOOST_CHECK_EQUAL(zzz.nelements(), 10);
}

BOOST_AUTO_TEST_CASE( assign_conforming )
{
	Vector<int> yyy(10), zzz(10, 13);
	yyy = -13;
	zzz.assign_conforming( yyy );
	BOOST_CHECK(allEQ(zzz, yyy));
  BOOST_CHECK(allEQ(zzz, -13));
}

BOOST_AUTO_TEST_CASE( tostdvector )
{
	Vector<int> zzz(10, -13);
	std::vector<int> stdvec = zzz.tovector();
	BOOST_CHECK_EQUAL(stdvec.size(), 10);
  for(size_t i=0; i!=10; ++i)
    BOOST_CHECK_EQUAL(stdvec[i], -13);
}

BOOST_AUTO_TEST_CASE( from_stdvector )
{
	Vector<int> aaa(std::vector<int>(10, -13));
	BOOST_CHECK(allEQ (aaa, Vector<int>(10, -13)));
	BOOST_CHECK(allEQ (aaa, -13));
}

// Various vector tests

BOOST_AUTO_TEST_CASE( take_part )
{
  IPosition shape(4,20,21,22,23);
  Array<int> arr(shape);
  indgen(arr);
  Array<int> arr2;
  
  // Take a part of the original array.
  arr2.reference(arr(IPosition(4,9,0,9,9), IPosition(4,9,shape(1)-1,9,9)));
  Vector<int> vec(arr2);
  BOOST_CHECK (vec.ndim()==1  &&  vec.nelements()==size_t(shape(1)));
  BOOST_CHECK (vec.shape() == IPosition(1,shape(1)));
  BOOST_CHECK (!vec.contiguousStorage());
  for (size_t i=0; i<vec.size(); ++i) {
    BOOST_CHECK (vec(i) == arr(IPosition(4,9,i,9,9)));
    BOOST_CHECK (vec(i) == arr2(IPosition(4,0,i,0,0)));
  }
}
  
BOOST_AUTO_TEST_CASE( vector_various )
{
  IPosition shape(4,20,21,22,23);
  Array<int> arr(shape);
  indgen(arr);
  Array<int> arr2;
  
  Array<int> arr3(arr(IPosition(4,1,2,3,4),
    IPosition(4,19,18,20,22),
    IPosition(4,2)));
  IPosition shp3 = arr3.shape();
  arr2.reference(arr3(IPosition(4,1,3,1,2),
    IPosition(4,1,3,shp3(2)-1,2),
    IPosition(4,1,1,2,1)));
  // Note that elements 1..shp3(2)-1 with step 2 gives shp3(2)/2 elements.
  Vector<int> vec(arr2);
  BOOST_CHECK (vec.ndim()==1  &&  vec.size()==size_t(shp3(2))/2);
  BOOST_CHECK (vec.shape() == IPosition(1,shp3(2)/2));
  BOOST_CHECK (!vec.contiguousStorage());
  for (size_t i=0; i<vec.size(); ++i) {
    BOOST_CHECK (vec(i) == arr3(IPosition(4,1,3,1+2*i,2)));
    BOOST_CHECK (vec(i) == arr2(IPosition(4,0,0,i,0)));
    BOOST_CHECK (&(vec(i)) == &(arr2(IPosition(4,0,0,i,0))));
    BOOST_CHECK (&(vec(i)) == &(arr(IPosition(4,3,8,5+4*i,8))));
  }
}

BOOST_AUTO_TEST_CASE( init_from_data )
{
  IPosition shape(1, 2);
  std::unique_ptr<std::vector<int>> values(new std::vector<int>(shape.product()));
  Vector<int> c(shape, values->data(), COPY);
  values.reset();
  c.resize(IPosition(1, 3), false);
  c.resize(4, false);
  BOOST_CHECK(true);
}

BOOST_AUTO_TEST_CASE( copy_resize )
{
  // Test the copying Vector::resize functions
  Vector<int> vi(10);
  indgen(vi);
  vi.resize(20, true);
  BOOST_CHECK(vi(0) == 0 && vi(9) == 9);
  vi.resize(IPosition(1,5), true);
  BOOST_CHECK(vi(0) == 0 && vi(4) == 4);
  vi.resize(IPosition(1,10)); // All bets are off, nothing to test
}

BOOST_AUTO_TEST_CASE( tovector )
{
  // tovector tests
  Vector<int> x(3);
  x[0] = 20;
  x[1] = 33;
  x[2] = -20;
  std::vector<int> tx;
  x.tovector(tx);
  Vector<int> xx = x.tovector();
  BOOST_CHECK(tx.size() == x.size());
  BOOST_CHECK(tx.size() == xx.size());

  for (size_t i=0; i<x.size(); i++) {
    BOOST_CHECK(x[i] == tx[i]);
    BOOST_CHECK(x[i] == xx[i]);
  }
}

BOOST_AUTO_TEST_CASE( stdvector_constructor )
{
  // Make sure compiler does not find ambiguous constructor.
  Vector<size_t> vs1(3, 2);
  BOOST_CHECK (allEQ(vs1, size_t(2)));
  Vector<size_t> vs2(3, 2);
  BOOST_CHECK (allEQ(vs2, size_t(2)));
  // Construct from iterator.
  std::vector<size_t> v(5);
  v[0] = 2;
  v[1] = 3;
  v[2] = 4;
  v[3] = 5;
  v[4] = 6;
  Vector<size_t> myvec(v.begin(), v.size(), 0);
  BOOST_CHECK(v.size() == myvec.size());
  for (size_t i=0; i<5; i++) {
    BOOST_CHECK(v[i] == myvec[i]);
  }
  // Construct from std::vector.
  std::vector<int> v2(2);
  v2[0] = 5;
  v2[1] = -2;
  Vector<int> myvec2(v2);
  BOOST_CHECK(v2.size() == myvec2.size());
  for (size_t i=0; i<2; i++) {
    BOOST_CHECK(v2[i] == myvec2[i]);
  }
  // Construct and convert type.
  Vector<double> myvec3(v2.begin(), v2.size(), 0);
  BOOST_CHECK(v2.size() == myvec3.size());
  for (size_t i=0; i<2; i++) {
    BOOST_CHECK(v2[i] == myvec3[i]);
  }
}

BOOST_AUTO_TEST_CASE( multi_dimensional )
{
  IPosition shape(4,20,21,22,23);
  Array<int> arr(shape);
  indgen(arr);
  Array<int> arr2;
  
  // Test use of a Vector from an Array with > 1 dimensions.
  arr2.reference(arr(IPosition(4,0), IPosition(4,shape(0)-1,0,0,0)));
  Vector<int> vec(arr2);
  BOOST_CHECK (vec.ndim()==1  &&  vec.nelements()==size_t(shape(0)));
  BOOST_CHECK (vec.shape() == IPosition(1,shape(0)));
  BOOST_CHECK (vec.contiguousStorage());
  for (size_t i=0; i<vec.size(); ++i) {
    BOOST_CHECK (vec(i) == arr(IPosition(4,i,0,0,0)));
    BOOST_CHECK (vec(i) == arr2(IPosition(4,i,0,0,0)));
  }
}

BOOST_AUTO_TEST_CASE( multi_dimensional_slice )
{
  IPosition shape(4,20,21,22,23);
  Array<int> arr(shape);
  indgen(arr);
  Array<int> arr2;
  
  // Test it for a slice.
  arr2.reference(arr(IPosition(4,0,9,9,9), IPosition(4,shape(0)-1,9,9,9)));
  Vector<int> vec(arr2);
  BOOST_CHECK (vec.ndim()==1  &&  vec.nelements()==size_t(shape(0)));
  BOOST_CHECK (vec.shape() == IPosition(1,shape(0)));
  BOOST_CHECK (vec.contiguousStorage());
  for (size_t i=0; i<vec.size(); ++i) {
    BOOST_CHECK (vec(i) == arr(IPosition(4,i,9,9,9)));
    BOOST_CHECK (vec(i) == arr2(IPosition(4,i,0,0,0)));
  }
}

BOOST_AUTO_TEST_CASE( slice_single_element )
{
  IPosition shape(4,20,21,22,23);
  Array<int> arr(shape);
  indgen(arr);
  Array<int> arr2;
  
  arr2.reference(arr(IPosition(4,9,9,9,9), IPosition(4,9,9,9,9)));
  Vector<int> vec(arr2);
  BOOST_CHECK (vec.ndim()==1  &&  vec.nelements()==1);
  BOOST_CHECK (vec.shape() == IPosition(1,1));
  BOOST_CHECK (vec.contiguousStorage());
  BOOST_CHECK (vec(0) == arr(IPosition(4,9,9,9,9)));
  BOOST_CHECK (vec(0) == arr2(IPosition(4,0,0,0,0)));
}

BOOST_AUTO_TEST_CASE( multi_dimensional_copy )
{
  // Test the Vector copy ctor for arrays with !1 dimension.
  Array<int> arr;
  Vector<int> vec(arr);
  BOOST_CHECK (vec.ndim()==1  &&  vec.nelements()==0);
  BOOST_CHECK (vec.shape() == IPosition(1,0));
}

BOOST_AUTO_TEST_SUITE_END()
