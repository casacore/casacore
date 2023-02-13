//# tArray.cc: Test program for the Array class
//# Copyright (C) 1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2015
//# Associated Universities, Inc. Washington DC, USA.
//# National Astronomical Observatory of Japan
//# 2-21-1, Osawa, Mitaka, Tokyo, 181-8588, Japan.
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
//#        internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#include "../IPosition.h"
#include "../Array.h"
#include "../Vector.h"
#include "../Slice.h"
#include "../Slicer.h"
#include "../ArrayError.h"

#include <boost/test/unit_test.hpp>

#include <cstdint>
#include <limits>

using namespace casacore;

BOOST_AUTO_TEST_SUITE(array)

static int zero(int)
{
  return 0;
}

static int minusone(const int &)
{
  return -1;
}

BOOST_AUTO_TEST_CASE( multidim_construct )
{
  IPosition shape(2, 5, 5);
  Array<int> x(shape);
  BOOST_CHECK_EQUAL(x.ndim(), 2);
  BOOST_CHECK_EQUAL(x.shape(), shape);
}

BOOST_AUTO_TEST_CASE( string_construct )
{
  class CasaStringLike : public std::string {
  public:
    CasaStringLike() : std::string() { }
    CasaStringLike(char c) : std::string(1, c) { }
    CasaStringLike(const char* c) : std::string(c) { }
  };
  IPosition shape(2, 2, 2);
  Array<CasaStringLike> x(shape, CasaStringLike("some_string"));
  //Array<CasaStringLike> x(shape, "some_string"); // <-- This should not compile
  BOOST_CHECK_EQUAL(x.ndim(), 2);
  BOOST_CHECK_EQUAL(x.shape(), shape);
  std::vector<CasaStringLike> ref(2*2, CasaStringLike("some_string"));
  BOOST_CHECK_EQUAL_COLLECTIONS(ref.begin(), ref.end(), x.begin(), x.end());
}

BOOST_AUTO_TEST_CASE( index )
{
  Array<int> x(IPosition(2, 5, 5));
  IPosition index(2);
  index = 2;
  x(index) = 6;
  BOOST_CHECK_EQUAL(x(index), 6);
}

BOOST_AUTO_TEST_CASE( multidim_set )
{
  Array<int> x(IPosition(2, 5, 5));
  x.set(-1);
  for(size_t i=0; i!=5; ++i) {
    for(size_t j=0; j!=5; ++j) {
      BOOST_CHECK_EQUAL(x(IPosition(2,i,j)), -1);
    }
  }
}

BOOST_AUTO_TEST_CASE( multidim_shape1 )
{
  IPosition shape(2, 5, 5);
  Array<int> x(shape), y(shape+1);
  y.resize(x.shape());
  BOOST_CHECK_EQUAL(y.shape(), x.shape()); 
}

BOOST_AUTO_TEST_CASE( multidim_shape2 )
{
  IPosition shape(2, 5, 5);
  Array<int> x(shape), y(shape+1);
  y.resize(x.shape());
  y.assign_conforming(x);
  BOOST_CHECK_EQUAL(y.shape(), x.shape()); 
}

BOOST_AUTO_TEST_CASE( multidim_shape3 )
{
  IPosition shape(2, 5, 5);
  Array<int> y(shape);
  y.resize(shape + 3);
  BOOST_CHECK_EQUAL( y.shape(), shape + 3); 
}

BOOST_AUTO_TEST_CASE( multidim_initialize )
{
  Array<int> y1(IPosition(2, 5, 5), 4);
  BOOST_CHECK (allEQ(y1, 4));
}

BOOST_AUTO_TEST_CASE( slices1 )
{
	IPosition i1(3), i2(3);
	i1 = 0; i2 = 3;
	Array<int> a1(i2);
	a1 = 0;
	i2 = 1;
	a1(i1, i2) = 1;
	BOOST_CHECK(allEQ (a1(i1, i2), 1));
	BOOST_CHECK_EQUAL(a1(i1), 1);
	BOOST_CHECK_EQUAL(a1(i2), 1);
}

BOOST_AUTO_TEST_CASE( slices2 )
{
	IPosition i1(3), i2(3);
	i1 = 0; i2 = 3;
	Array<int> a1(i2);
	a1 = 0;
	i2 = 1;
	a1(i1, i2) = 1;
	i2 = 2;
	BOOST_CHECK_EQUAL(a1(i2), 0);
}

BOOST_AUTO_TEST_CASE( apply1 )
{
	Array<int> a1(IPosition(3, 3, 3, 3));
  a1.apply(zero);
  BOOST_CHECK(allEQ (a1, 0));
}

BOOST_AUTO_TEST_CASE( apply2 )
{
  Array<int> a1(IPosition(3, 3, 3, 3));
  a1.apply(zero);
  a1.apply(minusone);
  BOOST_CHECK(allEQ (a1, -1));
}

BOOST_AUTO_TEST_CASE( apply_function )
{
  Vector<float> vi(10);
  indgen(vi);
  vi.apply([](float x)->float { return x * x; });
  for (int i=0; i < 10; i++)
  {
    BOOST_CHECK_CLOSE_FRACTION(vi(i), i*i, 1e-5);
  }
}

BOOST_AUTO_TEST_CASE( copy )
{
  Array<int> a1(IPosition(3, 3, 3, 3));
  a1.apply(minusone);
  BOOST_CHECK(allEQ (a1, a1.copy()));
}

BOOST_AUTO_TEST_CASE( assign )
{
  Array<int> a1(IPosition(3, 3, 3, 3));
  a1.apply(minusone);
  Array<int> a2(a1);
  a1.assign_conforming(a1);
  BOOST_CHECK(allEQ (a1, a2));
}

BOOST_AUTO_TEST_CASE( resize1 )
{
	IPosition i2(3);
  i2 = 0;
  Array<int> a1(IPosition(3, 3, 3, 3));
  Array<int> a2(a1);
  a1.resize(i2);
  a1.reference(a2);
  BOOST_CHECK(allEQ (a1, a2));
}

BOOST_AUTO_TEST_CASE( resize2 )
{
  IPosition i2(3);
  i2.resize(1);
  i2 = 10;
  Array<int> a2(IPosition(3, 3, 3, 3)), a1(a2);
  a2.resize(i2);
  a2 = 10;
  i2 = 0;
  a1.resize(i2);
  a1.assign_conforming(a2);
  BOOST_CHECK(allEQ (a1, 10));
}

BOOST_AUTO_TEST_CASE( unique )
{
  IPosition i2(1, 100);
  Array<float> *a3 = new Array<float>(i2);
  *a3 = 11.0;
  BOOST_CHECK_EQUAL(11.0, (*a3)(IPosition(3, 0, 0, 0)));
  Array<float> a4(a3->operator()(IPosition(a3->ndim(), 0), 
    a3->shape()-1, IPosition(1,2)));
  BOOST_CHECK(allEQ (a4, 11.0F));
  delete a3;
  a4.unique();
  BOOST_CHECK(allEQ (a4, 11.0F));
}

BOOST_AUTO_TEST_CASE( reform )
{
  Array<float> ab1(IPosition(4,5,6,7,8));
  indgen(ab1);
  Array<float> ab2 (ab1(IPosition(4,1,2,1,3), IPosition(4,2,2,5,7),
    IPosition(4,1,1,2,3)).reform (IPosition(3,2,3,2)));
  for (size_t i=0; i<2; i++)
  {
    for (size_t j=0; j<3; j++)
    {
      for (size_t k=0; k<2; k++)
      {
        BOOST_CHECK_EQUAL (&(ab2(IPosition(3,i,j,k))),
          &(ab1(IPosition(4,1+i,2,1+j*2,3+k*3))));
      }
    }
  }
}

BOOST_AUTO_TEST_CASE( slicer1 )
{
  Array<float> ab1(IPosition(4,5,6,7,8));
  indgen(ab1);
  Slicer sl(IPosition(4,1,2,1,3), IPosition(4,2,2,5,5),
    IPosition(4,1,1,2,3), Slicer::endIsLast);
  Array<float> absl = ab1(sl);
  BOOST_CHECK (absl.shape() == IPosition(4,2,1,3,1));
  for (size_t i=0; i<2; i++) {
    for (size_t j=0; j<3; j++) {
      for (size_t k=0; k<1; k++) {
        BOOST_CHECK_EQUAL (&(absl(IPosition(4,i,0,j,k))),
          &(ab1(IPosition(4,1+i,2,1+j*2,3+k*3))));
      }
    }
  }
}

BOOST_AUTO_TEST_CASE( slicer2 )
{
  Array<float> ab1(IPosition(4,5,6,7,8));
  indgen(ab1);
  Slicer sl(IPosition(4,1,2,1,3),
      IPosition(4,2,2,Slicer::MimicSource,7),
      IPosition(4,1,1,2,3), Slicer::endIsLast);
  Array<float> absl = ab1(sl);
  BOOST_CHECK (absl.shape() == IPosition(4,2,1,3,2));
  for (size_t i=0; i<2; i++) {
    for (size_t j=0; j<3; j++) {
      for (size_t k=0; k<2; k++) {
        BOOST_CHECK_EQUAL (&(absl(IPosition(4,i,0,j,k))),
          &(ab1(IPosition(4,1+i,2,1+j*2,3+k*3))));
      }
    }
  }
}

BOOST_AUTO_TEST_CASE( empty_slice )
{
  Array<int> a1(IPosition(3,2,3,4));
  Array<int> a2 = a1(IPosition(3,0,0,2), IPosition(3,0,0,1));
  BOOST_CHECK_EQUAL (a2.shape(), IPosition(3,1,1,0));
  BOOST_CHECK_EQUAL (a2.size(), 0);
}

// reformOrResize, adjustLastAxis

static Array<int> reformArray()
{
  IPosition shape (2, 3, 4);
  Array<int> a0 (shape);

  for (int r = 0; r < 3; r++)
    for (int c = 0; c < 4; c++)
      a0 (IPosition (2, r, c)) = r * 10 + c;
  return a0;
}

BOOST_AUTO_TEST_CASE( array_reform_or_resize )
{
  Array<int> a1 = reformArray();
  // Test a no-op for adjustLastAxis.
  a1.reformOrResize (IPosition (2, 3, 4));
  BOOST_CHECK_EQUAL (a1.shape(), reformArray().shape());
}

BOOST_AUTO_TEST_CASE( array_reform )
{
  Array<int> a1 = reformArray();
  // Simple reform which shouldn't involve resizing.
  IPosition newShape (2, 4, 3);
  bool resized = a1.reformOrResize (newShape);

  BOOST_CHECK_EQUAL (a1.shape(), newShape);
  BOOST_CHECK_EQUAL ((size_t) a1.capacity(), newShape.product());
  BOOST_CHECK (!resized);
}

BOOST_AUTO_TEST_CASE( array_reform_with_resize )
{
  Array<int> a1 = reformArray();
  // Simple reform which should involve resizing.
  IPosition newShape (2, 3, 10);
  bool resized = a1.reformOrResize (newShape);

  BOOST_CHECK_EQUAL (a1.shape(), newShape);
  BOOST_CHECK_EQUAL ((size_t) a1.capacity(), newShape.product());
  BOOST_CHECK (resized);
}

BOOST_AUTO_TEST_CASE( array_reform_smaller )
{
  // Simple reform to make it smaller
  Array<int> a1 = reformArray();
	IPosition newShape (2, 3, 3);
	bool resized = a1.reformOrResize (newShape);

  BOOST_CHECK_EQUAL (a1.shape(), newShape);
	BOOST_CHECK_EQUAL (a1.capacity(), reformArray().capacity()); // same allocation size
	BOOST_CHECK (!resized);
}

BOOST_AUTO_TEST_CASE( array_reform_exception )
{
  // See that when resizing is required but forbidden that exception thrown.
  Array<int> a1 = reformArray();
  try {
    a1.reformOrResize (IPosition (2, 3, 10), 0, false); // forbid resize
    BOOST_CHECK (false); // shouldn't get here
  } catch (ArrayConformanceError & e) {
    // Everything's fine if we get here.
    BOOST_CHECK (true);
  }
}

BOOST_AUTO_TEST_CASE( array_dimensionality_exception )
{
	// Attempt to change dimensionality should throw exception.
  Array<int> a1 = reformArray();
	try {
    a1.reformOrResize (IPosition (1, 12)); // attempt to change dimensionality
    BOOST_CHECK (false); // shouldn't get here
	} catch (ArrayConformanceError & e){
    // Everything's fine if we get here.
    BOOST_CHECK (true);
  }
}

BOOST_AUTO_TEST_CASE( array_shared_exception )
{
	// Arrays that share storage must cause exception if method is called.
	Array<int> a1 = reformArray();
	Array<int> a2(a1); // copy construction --> sharing
	try {
    a1.reformOrResize (IPosition (2, 3, 3)); // would work except for sharing
    BOOST_CHECK (false); // shouldn't get here
	} catch (ArrayConformanceError & e) {
    // Everything's fine if we get here.
    BOOST_CHECK (true);
  }
}

BOOST_AUTO_TEST_CASE( array_padding )
{
	Array<int> a1 = reformArray();
  // See if padding functionality works.  Capacity ought to be 50% larger
  // than actually used.
  IPosition newShape (IPosition (2, 3, 6));
  bool resized = a1.reformOrResize (newShape, 50);
  
  BOOST_CHECK_EQUAL (a1.shape(), newShape);
  BOOST_CHECK (resized);
  BOOST_CHECK_EQUAL ((size_t) a1.capacity(), newShape.product() * 3 / 2);
}

BOOST_AUTO_TEST_CASE( array_adjustlastaxis1 )
{
	Array<int> a0 = reformArray(), a1 = reformArray();
	// AdjustLastAxis the last dimension by minus one and check that the data is preserved.

	IPosition newShape (IPosition (2, 3, 3));
	bool resized = a1.adjustLastAxis (newShape);

	BOOST_CHECK_EQUAL (a1.shape(), newShape);
	BOOST_CHECK (! resized); // should just reform

	for (int i = 0; i < 3; i++) {
    for (int j = 0; j < 3; j++) {
      BOOST_CHECK_EQUAL (a1 (IPosition (2, i, j)), a0 (IPosition (2, i, j)));
    }
	}
}

BOOST_AUTO_TEST_CASE( array_adjustlastaxis2 )
{
	Array<int> a0 = reformArray(), a1 = reformArray();
	// AdjustLastAxis the last dimension by one and check that the data is preserved.
	IPosition newShape (IPosition (2, 3, 5));
	bool resized = a1.adjustLastAxis (newShape);

	BOOST_CHECK_EQUAL (a1.shape(), newShape);
	BOOST_CHECK (resized); // should have been resized

	for (int i = 0; i < 3; i++) {
    for (int j = 0; j < 4; j++) {
      BOOST_CHECK_EQUAL (a1 (IPosition (2, i, j)), a0 (IPosition (2, i, j)));
    }
  }
}

BOOST_AUTO_TEST_CASE( array_resize_exception )
{
	Array<int> a1 = reformArray();

	// See that when resizing is required but forbidden that exception thrown.
	try {
	    a1.adjustLastAxis (IPosition (2, 3, 10), 0, false); // forbid resize
	    BOOST_CHECK (false); // shouldn't get here
	} catch (ArrayConformanceError & e){
	    // Everything's fine if we get here.
	    BOOST_CHECK (true); // shouldn't get here
	}
}

BOOST_AUTO_TEST_CASE( multi_dimensional_copy )
{
  Array<int> arr;
  Array<int> arr2(arr);
  BOOST_CHECK (arr2.ndim()==0  &&  arr2.nelements()==0);
  BOOST_CHECK (arr2.shape() == IPosition());
}

BOOST_AUTO_TEST_CASE( resize_copy )
{
  Array<int> arr1(IPosition(3,4,5,6));
  indgen (arr1);
  Array<int> arr2;
  arr2.assign_conforming( arr1 );
  arr1.resize (IPosition(3,4,5,8), true);
  BOOST_CHECK (allEQ (arr2, arr1(IPosition(3,0), IPosition(3,3,4,5))));
  arr1.resize (IPosition(3,6,4,2), true);
  BOOST_CHECK (allEQ (arr2(IPosition(3,0), IPosition(3,3,3,1)),
			   arr1(IPosition(3,0), IPosition(3,3,3,1))));
  arr1.resize();
  arr1.assign_conforming( arr2 );
  arr1.resize (IPosition(2,6,4), true);
  Array<int> arr1ca = arr1.reform(IPosition(3,6,4,1));
  BOOST_CHECK (allEQ (arr2(IPosition(3,0), IPosition(3,3,3,0)),
			   arr1ca(IPosition(3,0), IPosition(3,3,3,0))));
  arr1.resize (IPosition(4,8,3,2,4), true);
  Array<int> arr1cb = arr1.reform(IPosition(3,8,3,8));
  BOOST_CHECK (allEQ (arr2(IPosition(3,0), IPosition(3,3,2,0)),
			   arr1cb(IPosition(3,0), IPosition(3,3,2,0))));
}

BOOST_AUTO_TEST_CASE( new_interface1 )
{
  IPosition const shape(2, 2, 3);
  size_t const nelems = shape.product();
  Array<int> ai(shape, int(0));
  bool deleteIt;
  
  int const *ptr = ai.getStorage(deleteIt);
  for (size_t i = 0; i < nelems; ++i) {
      BOOST_CHECK(ptr[i] == 0);
  }
  ai.freeStorage(ptr, deleteIt);
  BOOST_CHECK(ptr == nullptr);
}

BOOST_AUTO_TEST_CASE( new_interface2 )
{
  IPosition const shape(2, 2, 3);
  size_t const nelems = shape.product();
  Array<int> ai(shape, int(0));
  bool deleteIt;
  
  int *ptr = ai.getStorage(deleteIt);
  for (size_t i = 0; i < nelems; ++i) {
      BOOST_CHECK(ptr[i] == 0);
      ptr[i] = int(i);
  }
  ai.putStorage(ptr, deleteIt);
  BOOST_CHECK(ptr == 0);
  for (size_t i = 0; i < nelems; ++i) {
      BOOST_CHECK(ai.data()[i] == int(i));
  }
}

BOOST_AUTO_TEST_CASE( new_interface3 )
{
  IPosition const shape(2, 2, 3);
  Array<int> ai(shape, int(0));
  bool deleteIt;
  
  void const *ptr = ai.getVStorage(deleteIt);
  ai.freeVStorage(ptr, deleteIt);
  BOOST_CHECK(ptr == nullptr);
}

BOOST_AUTO_TEST_CASE( new_interface4 )
{
  IPosition const shape(2, 2, 3);
  Array<int> ai(shape, int(0));
  bool deleteIt;
  
  void *ptr = ai.getStorage(deleteIt);
  ai.putVStorage(ptr, deleteIt);
  BOOST_CHECK(ptr == nullptr);
}

namespace {

struct LifecycleChecker {
  LifecycleChecker() {
    if (ctor_count >= ctor_error_trigger) {
      throw 0;
    }
    ++ctor_count;
  }
  LifecycleChecker(LifecycleChecker const &) {
    if (ctor_count >= ctor_error_trigger) {
      throw 0;
    }
    ++ctor_count;
  }
  LifecycleChecker(LifecycleChecker&& src) :
    LifecycleChecker(src)
  { }
  ~LifecycleChecker() {
    ++dtor_count;
  }
  LifecycleChecker & operator =(LifecycleChecker const&) {
    if (assign_count >= assign_error_trigger) {
      throw 0;
    }
    ++assign_count;
    return *this;
  }
  LifecycleChecker & operator =(LifecycleChecker&& rhs) {
    return operator=(rhs);
  }
  static void clear() {
    assign_count = ctor_count = dtor_count = 0;
    assign_error_trigger = ctor_error_trigger = std::numeric_limits<size_t>::max();
  }
  static size_t assign_count;
  static size_t ctor_count;
  static size_t dtor_count;
  static size_t ctor_error_trigger;
  static size_t assign_error_trigger;
};

size_t LifecycleChecker::assign_count = 0;
size_t LifecycleChecker::ctor_count = 0;
size_t LifecycleChecker::dtor_count = 0;
size_t LifecycleChecker::ctor_error_trigger = std::numeric_limits<size_t>::max();
size_t LifecycleChecker::assign_error_trigger = std::numeric_limits<size_t>::max();

BOOST_AUTO_TEST_CASE( life_cycle_1 )
{
  IPosition const shape(2, 2, 3);
  LifecycleChecker::clear();
  LifecycleChecker *ptr = new LifecycleChecker[shape.product()];
  {
    Array<LifecycleChecker> a;
    a.takeStorage(shape, ptr, SHARE);
    a.resize(IPosition(2, 3, 3), false);
  }
  delete[] ptr;
  BOOST_CHECK_EQUAL(LifecycleChecker::ctor_count, LifecycleChecker::dtor_count);
}

// This test-case is no longer valid: deallocation of an array that's
// allocated with new[] is not supported in Array2.
/*
BOOST_AUTO_TEST_CASE( life_cycle_2 )
{
  IPosition const shape(2, 2, 3);
  LifecycleChecker::clear();
  LifecycleChecker *ptr = new LifecycleChecker[shape.product()];
  {
    Array<LifecycleChecker> a;
    a.takeStorage(shape, ptr, TAKE_OVER);
    a.resize(IPosition(2, 3, 3), true);
  }
  BOOST_CHECK_EQUAL(LifecycleChecker::ctor_count, LifecycleChecker::dtor_count);
}
*/

BOOST_AUTO_TEST_CASE( life_cycle_3 )
{
  IPosition const shape(2, 2, 3);
  LifecycleChecker::clear();
  LifecycleChecker *ptr = new LifecycleChecker[shape.product()];
  {
      Array<LifecycleChecker> a;
      a.takeStorage(shape, ptr, COPY);
  }
  delete[] ptr;
  BOOST_CHECK_EQUAL(LifecycleChecker::ctor_count, LifecycleChecker::dtor_count);
}

BOOST_AUTO_TEST_CASE( life_cycle_4 )
{
  IPosition const shape(2, 2, 3);
  LifecycleChecker::clear();
  LifecycleChecker *ptr = new LifecycleChecker[shape.product()];
  {
    Array<LifecycleChecker> a(shape, ptr, SHARE);
  }
  delete[] ptr;
  BOOST_CHECK_EQUAL(LifecycleChecker::ctor_count, LifecycleChecker::dtor_count);
}

/*
BOOST_AUTO_TEST_CASE( life_cycle_5 )
{
  IPosition const shape(2, 2, 3);
  LifecycleChecker::clear();
  LifecycleChecker *ptr = new LifecycleChecker[shape.product()];
  {
      Array<LifecycleChecker> a(shape, ptr, TAKE_OVER);
  }
  BOOST_CHECK_EQUAL(LifecycleChecker::ctor_count, LifecycleChecker::dtor_count);
}
*/

BOOST_AUTO_TEST_CASE( life_cycle_6 )
{
  IPosition const shape(2, 2, 3);
  LifecycleChecker::clear();
  LifecycleChecker *ptr = new LifecycleChecker[shape.product()];
  {
      Array<LifecycleChecker> a(shape, ptr, COPY);
  }
  delete[] ptr;
  BOOST_CHECK_EQUAL(LifecycleChecker::ctor_count, LifecycleChecker::dtor_count);
}

BOOST_AUTO_TEST_CASE( life_cycle_7 )
{
  IPosition const shape(2, 2, 3);
  size_t const nelems = shape.product();
  LifecycleChecker::clear();
  LifecycleChecker *ptr = std::allocator<LifecycleChecker>().allocate(shape.product());
  for (size_t i = 0; i < nelems; ++i) {
      std::allocator<LifecycleChecker>().construct(&ptr[i]);
  }
  {
    Array<LifecycleChecker> a;
    a.takeStorage(shape, ptr, SHARE);
    a.resize(IPosition(2, 3, 3), false);
  }
  for (size_t i = 0; i < nelems; ++i) {
    std::allocator<LifecycleChecker>().destroy(&ptr[i]);
  }
  std::allocator<LifecycleChecker>().deallocate(ptr, nelems);
  BOOST_CHECK_EQUAL(LifecycleChecker::ctor_count, LifecycleChecker::dtor_count);
}

BOOST_AUTO_TEST_CASE( life_cycle_8 )
{
  IPosition const shape(2, 2, 3);
  size_t const nelems = shape.product();
  LifecycleChecker::clear();
  std::allocator<LifecycleChecker> allocator;
  LifecycleChecker *ptr = allocator.allocate(shape.product());
  for (size_t i = 0; i < nelems; ++i) {
    allocator.construct(&ptr[i]);
  }
  {
    Array<LifecycleChecker> a;
    a.takeStorage(shape, ptr, TAKE_OVER);
    a.resize(IPosition(2, 3, 3), true);
  }
  BOOST_CHECK_EQUAL(LifecycleChecker::ctor_count, LifecycleChecker::dtor_count);
}

BOOST_AUTO_TEST_CASE( life_cycle_9 )
{
  IPosition const shape(2, 2, 3);
  size_t const nelems = shape.product();
  LifecycleChecker::clear();
  std::allocator<LifecycleChecker> allocator;
  LifecycleChecker *ptr = allocator.allocate(shape.product());
  {
      Array<LifecycleChecker> a;
      a.takeStorage(shape, ptr, COPY);
  }
  allocator.deallocate(ptr, nelems);
  BOOST_CHECK_EQUAL(LifecycleChecker::ctor_count, LifecycleChecker::dtor_count);
}

BOOST_AUTO_TEST_CASE( life_cycle_10 )
{
  IPosition const shape(2, 2, 3);
  size_t const nelems = shape.product();
  LifecycleChecker::clear();
  std::allocator<LifecycleChecker> allocator;
  LifecycleChecker *ptr = allocator.allocate(shape.product());
  for (size_t i = 0; i < nelems; ++i) {
    allocator.construct(&ptr[i]);
  }
  {
    Array<LifecycleChecker> a(shape, ptr, SHARE);
  }
  for (size_t i = 0; i < nelems; ++i) {
    allocator.destroy(&ptr[i]);
  }
  allocator.deallocate(ptr, nelems);
  BOOST_CHECK_EQUAL(LifecycleChecker::ctor_count, LifecycleChecker::dtor_count);
}

BOOST_AUTO_TEST_CASE( life_cycle_11 )
{
  IPosition const shape(2, 2, 3);
  size_t const nelems = shape.product();
  LifecycleChecker::clear();
  std::allocator<LifecycleChecker> allocator;
  LifecycleChecker *ptr = allocator.allocate(shape.product());
  for (size_t i = 0; i < nelems; ++i) {
    allocator.construct(&ptr[i]);
  }
  {
    Array<LifecycleChecker> a(shape, ptr, TAKE_OVER);
  }
  BOOST_CHECK_EQUAL(LifecycleChecker::ctor_count, LifecycleChecker::dtor_count);
}

BOOST_AUTO_TEST_CASE( life_cycle_12 )
{
  IPosition const shape(2, 2, 3);
  size_t const nelems = shape.product();
  LifecycleChecker::clear();
  std::allocator<LifecycleChecker> allocator;
  LifecycleChecker *ptr = allocator.allocate(shape.product());
  {
      Array<LifecycleChecker> a(shape, ptr, COPY);
  }
  allocator.deallocate(ptr, nelems);
  BOOST_CHECK_EQUAL(LifecycleChecker::ctor_count, LifecycleChecker::dtor_count);
}

/*
BOOST_AUTO_TEST_CASE( life_cycle_13 )
{
  IPosition const shape(2, 2, 3);
  LifecycleChecker::clear();
  {
    // no longer supported in Array2
    // Array<LifecycleChecker> a(shape, ArrayInitPolicies::NO_INIT);
  }
  BOOST_CHECK(LifecycleChecker::ctor_count == 0);
  BOOST_CHECK(LifecycleChecker::dtor_count == (size_t)shape.product());
}*/

BOOST_AUTO_TEST_CASE( life_cycle_14 )
{
  IPosition const shape(2, 2, 3);
  LifecycleChecker::clear();
  {
    Array<LifecycleChecker> a(shape);
  }
  BOOST_CHECK_EQUAL(LifecycleChecker::ctor_count, (size_t) shape.product());
  BOOST_CHECK_EQUAL(LifecycleChecker::dtor_count, (size_t) shape.product());
}

/*
BOOST_AUTO_TEST_CASE( life_cycle_15 )
{
  IPosition const shape(2, 2, 3);
  {
    Array<LifecycleChecker> a;
    LifecycleChecker::clear();
    // no longer supported in Array2
    // a.resize(shape, false, ArrayInitPolicies::NO_INIT);
  }
  BOOST_CHECK(LifecycleChecker::ctor_count == 0);
  BOOST_CHECK(LifecycleChecker::dtor_count == (size_t )shape.product());
}*/

BOOST_AUTO_TEST_CASE( life_cycle_16 )
{
  IPosition const shape(2, 2, 3);
  {
    Array<LifecycleChecker> a;
    LifecycleChecker::clear();
    a.resize(shape, false);
  }
  BOOST_CHECK_EQUAL(LifecycleChecker::ctor_count, (size_t )shape.product());
  BOOST_CHECK_EQUAL(LifecycleChecker::dtor_count, (size_t )shape.product());
}

BOOST_AUTO_TEST_CASE( array_pos_iterator )
{
  IPosition const shape(2, 2, 3);
  Array<int> ai(shape);
  BOOST_CHECK_EQUAL(ai.capacity(), 2 * 3);
  for (ssize_t c = 0; c < 3; ++c) {
      for (ssize_t r = 0; r < 2; ++r) {
          IPosition pos(2, r, c);
          ai(pos) = r*100 + c;
      }
  }
  int *p = ai.data();
  int order[] = {0, 100, 1, 101, 2, 102};
  for (ssize_t i = 0; i < (int)ai.capacity(); ++i) {
      BOOST_CHECK_EQUAL(p[i], order[i]);
  }
  size_t count1 = 0;
  for (ArrayPositionIterator iter1(shape, 1); !iter1.pastEnd(); iter1.next())
    ++count1;
  BOOST_CHECK_EQUAL(count1, 3);
  size_t count2 = 0;
  for (ArrayPositionIterator iter2(shape, 0); !iter2.pastEnd(); iter2.next())
    ++count2;
  BOOST_CHECK_EQUAL(count2, 6);
}

} // anonymous namespace

// Various further tests (coming from 'main()')

BOOST_AUTO_TEST_CASE( array_empty )
{
  Array<int> ai1;
  BOOST_CHECK_EQUAL(ai1.ndim(), 0);
  BOOST_CHECK_EQUAL(ai1.nelements(), 0); 
}

BOOST_AUTO_TEST_CASE( array_init1 )
{
  IPosition ip1(5,1,2,3,4,5);
  Array<int> ai2(ip1);
  BOOST_CHECK_EQUAL(ai2.ndim(), 5);
  BOOST_CHECK_EQUAL(ai2.nelements(), 120);
  BOOST_CHECK_EQUAL(ai2.shape(), ip1);
}

BOOST_AUTO_TEST_CASE( array_init2 )
{
  IPosition ip3(1,11);
  Array<int> ai4(ip3);
  ai4.set(10);
  BOOST_CHECK(allEQ(ai4, 10));
}

BOOST_AUTO_TEST_CASE( array_iposition_index )
{
  IPosition ip3(1,11);
  Array<int> ai4(ip3);
  ai4.set(10);
  IPosition ip5(1);
  for(int i=0; i <11; i++) {
    ip5(0) = i;
    BOOST_CHECK_EQUAL(ai4(ip5), 10);          // T operator()(IPosition)
  }
}

BOOST_AUTO_TEST_CASE( array_reference )
{
  IPosition ip1(5,1,2,3,4,5);
  Array<int> ai3(ip1);
  IPosition ip3(1,11);
  Array<int> ai4(ip3);
  ai4.set(10);
  
  ai3.reference(ai4);
  BOOST_CHECK(ai4.nrefs() == 2 && ai3.nrefs() == 2);
  BOOST_CHECK(ai3.ndim() == 1 && ai3.shape() == 11);
  BOOST_CHECK(&ai3(IPosition{0}) == &ai4(IPosition{0}));
  // Eventually should carry on with all member functions. Still,
  // The test coverage isn't terrible.
}

/*
BOOST_AUTO_TEST_CASE( pointer_array )
{
  // Tests of the pointer->Array functions
  std::vector<int> ip(100);
  IPosition shape(2, 5, 20);
  Array<int> ai(shape, ip.data(), SHARE); // sharing no longer possible in Array2
  indgen(ai);
  for (int i=0; i < 100; i++) {
    BOOST_CHECK(ip[i] == i);
  }
  Array<int> ai2(shape, ip.data(), COPY);
  BOOST_CHECK(allEQ(ai2, ai));
  ai2 = 11;
  BOOST_CHECK(ip[0] == 0 && ip[99] == 99 && 
        ai(IPosition(2,4,19)) == 99 && allEQ(ai2, 11));
  Vector<int> vi(IPosition(1, 100), ip.data(), SHARE);
  Matrix<int> mi(IPosition(2, 10, 10), ip.data(), SHARE);
  Cube<int> ci(IPosition(3, 4, 5, 5), ip.data(), SHARE);
  vi(99) = 66;
  BOOST_CHECK(vi(99) == 66 && mi(9,9) == 66 && ci(3,4,4) == 66 &&
        ai(IPosition(2,4,19)) == 66);
}*/

BOOST_AUTO_TEST_CASE( non_degenerate1 )
{
  // Test the nonDegenerate() function
  Array<int> a1(IPosition(5,1,2,1,3,1));
  indgen(a1);
  BOOST_CHECK(a1.nonDegenerate().shape() == IPosition(2,2,3));
  BOOST_CHECK(a1.nonDegenerate(1).shape() == IPosition(3,1,2,3));
  Array<int> c = a1.nonDegenerate(1);
  BOOST_CHECK(c(IPosition(3,0,1,2)) == 5);
  c(IPosition{0,1,2}) = 99;
  BOOST_CHECK(a1(IPosition(5, 0, 1, 0, 2, 0)) == 99);
  BOOST_CHECK(a1.nonDegenerate(4).shape() == IPosition(4,1,2,1,3));
  Array<int> a2(IPosition(3,1,1,1));
  BOOST_CHECK(a2.nonDegenerate().shape() == IPosition(1,1));
}

BOOST_AUTO_TEST_CASE( non_degenerate2 )
{
  Array<int> a1(IPosition(5,1,2,1,3,1));
  indgen(a1);
  const Array<int> a3(a1);
  Array<int> c = a1.nonDegenerate(1);
  c(IPosition(3, 0,1,2)) = 99;
  BOOST_CHECK(a3.nonDegenerate().shape() == IPosition(2,2,3));
  BOOST_CHECK(a3.nonDegenerate(1).shape() == IPosition(3,1,2,3));
  BOOST_CHECK(a3.nonDegenerate()(IPosition(2,0,2)) == 4);
  BOOST_CHECK(a3.nonDegenerate()(IPosition(2,1,2)) == 99);
}

BOOST_AUTO_TEST_CASE( non_degenerate3 )
{
  Array<int> a1(IPosition(5,1,2,1,3,1));
  indgen(a1);
  const Array<int> a3(a1);
  Array<int> c = a1.nonDegenerate(1);
  c(IPosition(3, 0,1,2)) = 99;
  
  Array<int> a4;
  a4.nonDegenerate(a1);
  BOOST_CHECK(a4.shape() == IPosition(2,2,3));
  BOOST_CHECK(a4(IPosition(2,0,2)) == 4);
  BOOST_CHECK(a4(IPosition(2,1,2)) == 99);
  a4.nonDegenerate(a1, 1);
  BOOST_CHECK(a4.shape() == IPosition(3,1,2,3));
  BOOST_CHECK(a4(IPosition(3,0,0,0)) == 0);
  BOOST_CHECK(a4(IPosition(3,0,1,2)) == 99);
}

BOOST_AUTO_TEST_CASE( add_generate )
{
  Array<int> a1(IPosition(2,10,10));
  indgen(a1);
  BOOST_CHECK(a1.addDegenerate(1u).shape()==IPosition(3,10,10,1));

  Array<int> m = a1(IPosition(2,1),IPosition(2,3),IPosition(2,2));
  BOOST_CHECK(m(IPosition(2, 0,0)) == 11);
  BOOST_CHECK(m(IPosition(2, 1,1)) == 33);
  Array<int> md(m.addDegenerate(2u));
  BOOST_CHECK(md.shape() == IPosition(4,2,2,1,1));
  BOOST_CHECK(md(IPosition(4,0)) == 11);
  BOOST_CHECK(md(IPosition(4,1,1,0,0)) == 33);
  md(IPosition(4,0)) = 100;
  BOOST_CHECK(m(IPosition(2, 0,0)) == 100);

  const Array<int> a2(m);
  BOOST_CHECK(a2.addDegenerate(1u).shape() == IPosition(3,2,2,1));
}

BOOST_AUTO_TEST_CASE( zero_dimensional_arrays )
{
  // Test 0-dimensioned (not sized) arrays
  IPosition shape(0);
  Array<int> ai(shape);
  Array<int> ai2(ai);
  ai2.assign_conforming( ai );
  ai = 999;
  BOOST_CHECK(ai.ndim() == 0 && ai2.ndim() == 0 && 
        ai.nelements() == 0);
}

BOOST_AUTO_TEST_CASE( nondegenerate_on_subsection )
{
  // Test nonDegenerate on an Array subsection.
  IPosition shape0(5,2,3,4,5,6);
  Array<float> data(shape0);
  indgen(data, float(0.0));
  IPosition blc(5, 0);
  IPosition trc = shape0 - 1;
  for (int i=0; i<shape0(0); i++) {
    blc(0) = i;
    trc(0) = i;
    for (int j=0; j<shape0(3); j++) {
      blc(3) = j;
      trc(3) = j;
      Array<float> data2 = data(blc, trc);
      IPosition shape1(3, shape0(1), shape0(2), shape0(4));
      Array<float> data3 = data2.nonDegenerate();
      Array<float> data4 = data2.reform(shape1);
      BOOST_CHECK (allEQ(data3, data4));
      bool deleteIt;
      const float* dataPtr = data2.getStorage (deleteIt);
      Array<float> data5 (shape1, dataPtr);
      BOOST_CHECK (allEQ(data3, data5));
      data2.freeStorage (dataPtr, deleteIt);
    }
  }
}

BOOST_AUTO_TEST_CASE( assign_empty )
{
  Array<int> zeroArr;
  zeroArr = Array<int>();
  BOOST_CHECK_EQUAL( zeroArr.shape().size(), 0);
  BOOST_CHECK( zeroArr.shape() == IPosition() );
}

BOOST_AUTO_TEST_SUITE_END()
