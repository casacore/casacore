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
//#
//# $Id: tArray.cc 21521 2014-12-10 08:06:42Z gervandiepen $

#include "../IPosition.h"
#include "../Array.h"
#include "../ArrayMath.h"
#include "../ArrayLogical.h"
#include "../Vector.h"
#include "../Matrix.h"
#include "../Cube.h"
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

BOOST_AUTO_TEST_CASE( vector )
{
  Vector<int> x(10);
  x = 5;     
  for (int i=0; i < 10; i++)
    BOOST_CHECK_EQUAL(x(i), 5);
}

BOOST_AUTO_TEST_CASE( vector_copy_constructor )
{
  Vector<int> x(10);
  x = 5;     
	Vector<int> y(x);
	BOOST_CHECK(x.nrefs() == y.nrefs() && x.nrefs() > 1);
	for (int i=0; i < 10; i++)
    BOOST_CHECK_EQUAL(y(i), 5);
}

BOOST_AUTO_TEST_CASE( vector_copy )
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

BOOST_AUTO_TEST_CASE( vector_slice1 )
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

BOOST_AUTO_TEST_CASE( vector_slice2 )
{
  Vector<float> x1(5);
  indgen(x1);
  BOOST_CHECK(allEQ(x1(Slice(0,2,2)), Vector<float>{0, 2}));
}

BOOST_AUTO_TEST_CASE( vector_default_constructor )
{
	Vector<int> zz;
	BOOST_CHECK_EQUAL(zz.nelements(),0);
	BOOST_CHECK_EQUAL(zz.size(), 0);
	BOOST_CHECK(zz.empty());
}

BOOST_AUTO_TEST_CASE( vector_init_constructor )
{
	Vector<int> y1(5, 4);
	BOOST_CHECK (allEQ(y1, 4));
}

BOOST_AUTO_TEST_CASE( vector_slice_constructor )
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

BOOST_AUTO_TEST_CASE( vector_string )
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

BOOST_AUTO_TEST_CASE( vector_resize )
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

BOOST_AUTO_TEST_CASE( vector_assign_conforming )
{
	Vector<int> yyy(10), zzz(10, 13);
	yyy = -13;
	zzz.assign_conforming( yyy );
	BOOST_CHECK(allEQ(zzz, yyy));
  BOOST_CHECK(allEQ(zzz, -13));
}

BOOST_AUTO_TEST_CASE( vector_tostdvector )
{
	Vector<int> zzz(10, -13);
	std::vector<int> stdvec = zzz.tovector();
	BOOST_CHECK_EQUAL(stdvec.size(), 10);
  for(size_t i=0; i!=10; ++i)
    BOOST_CHECK_EQUAL(stdvec[i], -13);
}

BOOST_AUTO_TEST_CASE( vector_from_stdvector )
{
	Vector<int> aaa(std::vector<int>(10, -13));
	BOOST_CHECK(allEQ (aaa, Vector<int>(10, -13)));
	BOOST_CHECK(allEQ (aaa, -13));
}

// Math functions
BOOST_AUTO_TEST_CASE( math_floor )
{
  Vector<double> x({0.1, 0.2, 0.3, 0.4, 0.5});
  Vector<double> y = floor(x);
	for (int i = 0; i < 5; i++)
    BOOST_CHECK_EQUAL(y(i), 0.0);
}

BOOST_AUTO_TEST_CASE( math_pow )
{
  Vector<double> x({0.1, 0.2, 0.3, 0.4, 0.5});
  Vector<double> z = pow(x, 1.0);
  BOOST_CHECK(allNear (z, x, 1.0e-10));
  z.assign_conforming(pow(x, 2.0));
  BOOST_CHECK(allNear (z, x*x, 1.0e-10));
  Vector<double> y(5, 0.0);
  z.assign_conforming(pow(x, y));
  for (int i = 0; i < 5; i++)
    BOOST_CHECK_CLOSE_FRACTION(z(i), 1.0, 1e-6);
}

BOOST_AUTO_TEST_CASE( math_minmax1 )
{
  Vector<double> z(5, 1.0);
	BOOST_CHECK_EQUAL(min(z), 1.0);
  BOOST_CHECK_EQUAL(max(z), 1.0);
	z(4) = -1.0; z(3) = 22.0;
	BOOST_CHECK_EQUAL(min(z), -1.0);
  BOOST_CHECK_EQUAL(max(z), 22.0);
  
}

BOOST_AUTO_TEST_CASE( math_minmax2 )
{
	Vector<int> vi1(5), vi2(5), vi3;
	indgen(vi1);
	vi2 = 0;
	vi2(3) = -3;
	vi3.assign_conforming( casacore::min(vi1, vi2) );
	BOOST_CHECK_EQUAL(vi3(0), 0);
	BOOST_CHECK_EQUAL(vi3(1), 0);
	BOOST_CHECK_EQUAL(vi3(2), 0);
	BOOST_CHECK_EQUAL(vi3(3), -3);
	BOOST_CHECK_EQUAL(vi3(4), 0);
	vi2(3) = 9;
	vi3.assign_conforming( casacore::max(-vi1, vi2) );
	BOOST_CHECK_EQUAL(vi3(0), 0);
	BOOST_CHECK_EQUAL(vi3(1), 0);
	BOOST_CHECK_EQUAL(vi3(2), 0);
	BOOST_CHECK_EQUAL(vi3(3), 9);
	BOOST_CHECK_EQUAL(vi3(4), 0);
}

BOOST_AUTO_TEST_CASE( math_masked_minmax )
{
  Matrix<float> a(10u, 3u);
  Matrix<bool> mask(10u, 3u);

  float val;
  for (int j=0; j<3; j++) {
      for (int i=0; i<10; i++) {
          a(i, j) = sin( (10*j + i) * 0.6);
          val = a(i, j);
          mask(i, j) = (val > 0) && (val < 0.5);
      }
  }
  for (int i=0; i<10; i++) {
      mask(i, 0)= false;
  }

  float min,max;
  IPosition minPos(2),maxPos(2);

  minMax (min, max, minPos, maxPos, a);
  BOOST_CHECK_EQUAL(minPos, IPosition(2, 8, 0));
  BOOST_CHECK_EQUAL(maxPos, IPosition(2, 3, 1));
  BOOST_CHECK_EQUAL(min, a(minPos));
  BOOST_CHECK_EQUAL(max, a(maxPos));

  minMax(min, max,a);
  BOOST_CHECK_EQUAL(min, a(minPos));
  BOOST_CHECK_EQUAL(max, a(maxPos));

  minMax (min, max, minPos, maxPos, a, mask);
  BOOST_CHECK_EQUAL(minPos, IPosition(2, 1, 2));
  BOOST_CHECK_EQUAL(maxPos, IPosition(2, 5, 1));
  BOOST_CHECK_EQUAL(min, a(minPos));
  BOOST_CHECK_EQUAL(max, a(maxPos));
}

// Testing Vector math and logicals
BOOST_AUTO_TEST_CASE( logical )
{
  Vector<int> x(5, 1), y(5, 2);
  BOOST_CHECK(allEQ (x, 1));
  BOOST_CHECK(allEQ (y, 2));
  BOOST_CHECK(allLE (x, y));
  BOOST_CHECK(allGE (y, x));
  BOOST_CHECK(allNE (x, 2));
  BOOST_CHECK(allNE (x, y));
  BOOST_CHECK(allEQ (x, x));
  BOOST_CHECK(allLT (x, y));
  BOOST_CHECK(allGT (y, x));
}

BOOST_AUTO_TEST_CASE( add_scalar )
{
  Vector<int> x(5, 1), y(5, 2);
	x += 1;
	BOOST_CHECK(allEQ (x, y));
}

BOOST_AUTO_TEST_CASE( add_vector )
{
  Vector<int> x(5, 2), y(5, 2);
	x += y;
	BOOST_CHECK(allEQ (x, 2*y));
}

BOOST_AUTO_TEST_CASE( mul_scalar )
{
  Vector<int> x(5, 4);
	x.assign_conforming( -1 * x );
	BOOST_CHECK(allEQ (x, -4));
}

BOOST_AUTO_TEST_CASE( vector_indgen )
{
  Vector<int> x(5), y(5);
  indgen(x); indgen(y, 1);
  BOOST_CHECK(allEQ (x - y, -1));
  for (int i = 0; i < 5; i++)
    BOOST_CHECK_EQUAL(x(i), i);
}

BOOST_AUTO_TEST_CASE( vector_operations )
{
  Vector<int> x(5), y(5);
  indgen(x); indgen(y, 1);
  BOOST_CHECK(sum(x) == 10);
  BOOST_CHECK(product(y) == 120);
  BOOST_CHECK(mean(y) == median(y) && mean(y) == 3);
}

BOOST_AUTO_TEST_CASE( vector_comparison )
{
  Vector<int> x(5), y(5);
  indgen(x); indgen(y, 1);
  BOOST_CHECK(anyLE(x, y) && anyLE(x, y-1) &&
  !anyLE(x, y-2));
  BOOST_CHECK(anyLT(x, y) && !anyLT(x, y-1) &&
  !anyLT (x, y-2));
  BOOST_CHECK(!anyGE(x, y) && anyGE (x, y-1) &&
  anyGE (x, y-2));
  BOOST_CHECK(!anyGT(x, y) && !anyGT (x, y-1) &&
  anyGT (x, y-2));
  BOOST_CHECK(!anyEQ(x, y) && anyEQ(x, y-1) &&
  !anyEQ(x, y-2));
  BOOST_CHECK(anyNE(x, y) && !anyNE (x, y-1) &&
  anyNE(x, y-2));

  BOOST_CHECK(anyLE(x,1) && anyLE(x,0)&& !anyLE(x,-1));
  BOOST_CHECK(anyLT(x,1) && !anyLT(x,0)&&!anyLT(x,-1));
  BOOST_CHECK(anyGE(x,3)&&anyGE(x,4) && !anyGE(x, 5));
  BOOST_CHECK(anyGT(x,3) && !anyGT(x,4)&&!anyGT(x, 5));
  BOOST_CHECK(anyEQ(x,3) && anyEQ(x,4) && !anyEQ(x,5));
  BOOST_CHECK(anyNE(x,3) && anyNE(x,4) && anyNE(x, 5));

  BOOST_CHECK(anyLE(3,x) && anyLE(4,x) && !anyLE(5,x));
  BOOST_CHECK(anyLT(3,x) && !anyLT(4,x) &&!anyLT(5,x));
  BOOST_CHECK(!anyGE(-1,x) && anyGE(0,x) &&anyGE(1,x));
  BOOST_CHECK(!anyGT(-1,x) && !anyGT(0,x)&&anyGT(1,x));
  BOOST_CHECK(!anyEQ(-1,x)&& anyEQ(0,x) && anyEQ(1,x));
  BOOST_CHECK(anyNE(-1,x) && anyNE(0,x) &&anyNE (1,x));
}

BOOST_AUTO_TEST_CASE( vector_statistics )
{
	Vector<double> vd(5);
	indgen(vd,1.0);
	BOOST_CHECK(std::fabs(variance(vd) - 2.5) < 0.0001);
	BOOST_CHECK(std::fabs(stddev(vd) - std::sqrt(2.5)) < 0.0001);
	BOOST_CHECK(std::fabs(avdev(vd) - 1.2) < 0.0001);
}

BOOST_AUTO_TEST_CASE( vector_complex )
{
  Vector<std::complex<float>> vc(2);
  vc(0) = std::complex<float>(1.0, 2.0);
  vc(1) = std::complex<float>(0.0, 1.0);
  Vector<float> vr = real(vc);
  BOOST_CHECK_EQUAL(vr(1), 0.0f);
  vr.assign_conforming(imag(vc));
  BOOST_CHECK_EQUAL(vr(0), 2.0f);
  float pi2 = 3.1415927/2.0;
  vr.assign_conforming( phase(vc));
  float pi2out = vr(1);
  BOOST_CHECK_LT(fabs(pi2 - pi2out), 0.00001);
  vr.assign_conforming( amplitude(vc) );
  BOOST_CHECK_EQUAL(vr(1), 1.0f);
}

BOOST_AUTO_TEST_CASE( square_cube )
{
  Vector<int> vf(3);
  vf = -3;
  BOOST_CHECK(allEQ (square(vf), 9));
  BOOST_CHECK(allEQ (cube(vf), -27));
}

BOOST_AUTO_TEST_CASE( near_tests )
{
  Vector<float> x1(2), x2(2);
  x1 = 10000;
  x2(0) = 10001; x2(1) = 10002;
  // Array,Array
  BOOST_CHECK(allNear(x1, x2, 2.01e-4));
  BOOST_CHECK(!allNear(x1, x2, 1.01e-4));
  BOOST_CHECK(anyNear(x1, x2, 1.01e-4));
  BOOST_CHECK(!anyNear(x1, x2, 0.99e-4));
  BOOST_CHECK(allNearAbs(x1, x2, 2.01));
  BOOST_CHECK(!allNearAbs(x1, x2, 1.01));
  BOOST_CHECK(anyNearAbs(x1, x2, 1.01));
  BOOST_CHECK(!anyNearAbs(x1, x2, 0.99));

  // Constant,Array
  BOOST_CHECK(allNear(10000.0f, x2, 2.01e-4));
  BOOST_CHECK(!allNear(10000.0f, x2, 1.01e-4));
  BOOST_CHECK(anyNear(10000.0f, x2, 1.01e-4));
  BOOST_CHECK(!anyNear(10000.0f, x2, 0.99e-4));
  BOOST_CHECK(allNearAbs(10000.0f, x2, 2.01));
  BOOST_CHECK(!allNearAbs(10000.0f, x2, 1.01));
  BOOST_CHECK(anyNearAbs(10000.0f, x2, 1.01));
  BOOST_CHECK(!anyNearAbs(10000.0f, x2, 0.99));

  // Array,Constant
  BOOST_CHECK(allNear(x1, 10002.0f, 2.01e-4));
  BOOST_CHECK(!allNear(x1, 10002.0f, 1.01e-4));
  BOOST_CHECK(!anyNear(x1, 10002.0f, 1.01e-4));
  BOOST_CHECK(!anyNear(x1, 10002.0f, 0.99e-4));
  BOOST_CHECK(allNearAbs(x1, 10002.0f, 2.01));
  BOOST_CHECK(!allNearAbs(x1, 10002.0f, 1.01));
  BOOST_CHECK(anyNearAbs(x1, 10001.0f, 1.01));
  BOOST_CHECK(!anyNearAbs(x1, 10002.0f, 0.99));
}

BOOST_AUTO_TEST_CASE( median_and_fractile )
{
  Vector<float> x1(5), x2(10), x3(100), x4(101), x5(101);
  indgen(x1);
  indgen(x2);
  indgen(x3);
  indgen(x4);
  indgen(x5);
  BOOST_CHECK_EQUAL (median(x1) , 2.);
  BOOST_CHECK_EQUAL (median(x1, true) , 2.);
  BOOST_CHECK_EQUAL (median(x1, true, false, false) , 2.);
  BOOST_CHECK_EQUAL (median(Vector<float>{0., 2.}) , 1.);
  BOOST_CHECK_EQUAL (median(x1(Slice(0,2,2))) , 1.);
  BOOST_CHECK_EQUAL (median(x2) , 4.5);
  BOOST_CHECK_EQUAL (median(x2, true) , 4.5);
  BOOST_CHECK_EQUAL (median(x2, true, false, false) , 4.);
  BOOST_CHECK_EQUAL (median(x3) , 49.5);
  BOOST_CHECK_EQUAL (median(x3, true) , 49.5);
  BOOST_CHECK_EQUAL (medianInPlace(x3) , 49.5);
  BOOST_CHECK_EQUAL (median(x3, true, false, true) , 49.);
  BOOST_CHECK_EQUAL (median(x4) , 50.);
  BOOST_CHECK_EQUAL (median(x4, true) , 50.);
  BOOST_CHECK_EQUAL (median(x4, true, false, false) , 50.);

  BOOST_CHECK_EQUAL (madfm(x1) , 1.);
  BOOST_CHECK_EQUAL (madfm(x1, true) , 1.);
  BOOST_CHECK_EQUAL (madfm(x1, true, false, false) , 1.);
  BOOST_CHECK_EQUAL (madfm(x1(Slice(0,2,2))) , 1.);
  BOOST_CHECK_EQUAL (madfm(x4) , 25.);
  // Make sure x4 is not changed.
  BOOST_CHECK (allEQ (x4, x5));

  BOOST_CHECK_EQUAL (fractile(x1, 0.0) , 0.);
  BOOST_CHECK_EQUAL (fractile(x1, 0.25) , 1.);
  BOOST_CHECK_EQUAL (fractile(x1, 0.5) , 2.);
  BOOST_CHECK_EQUAL (fractile(x1, 0.75) , 3.);
  BOOST_CHECK_EQUAL (fractile(x1, 1.0) , 4.);
  BOOST_CHECK_EQUAL (fractile(x1, 0.75, true) , 3.);
  BOOST_CHECK_EQUAL (fractile(x2, 0.5) , 4.);
  BOOST_CHECK_EQUAL (fractile(x3, 0.5, false, true) , 49.);
  BOOST_CHECK_EQUAL (fractile(x4, 0.0) , 0.);
  BOOST_CHECK_EQUAL (fractile(x4, 0.5) , 50.);
  BOOST_CHECK_EQUAL (fractile(x4, 0.05) , 5.);
  BOOST_CHECK_EQUAL (fractile(x4, 0.951) , 95.);
  // Make sure x4 is not changed.
  BOOST_CHECK (allEQ (x4, x5));

  BOOST_CHECK_EQUAL (interQuartileRange(x1) , 2.);
  BOOST_CHECK_EQUAL (interFractileRange(x1, 0.25) , 2.);
  BOOST_CHECK_EQUAL (interHexileRange(x4),
    fractile(x4, 5./6.) - fractile(x4, 1./6.));
  // Make sure x4 is not changed.
  BOOST_CHECK (allEQ (x4, x5));
}

// Simple matrix tests

BOOST_AUTO_TEST_CASE( matrix_initialize )
{
  Matrix<int> a(5u, 5u);
  a = 3;
  BOOST_CHECK(a.nrow() == a.ncolumn() && a.nrow() == 5);
  BOOST_CHECK(allEQ(a, 3));
  BOOST_CHECK(allLE(a, 3)); 
  BOOST_CHECK(allEQ(a, a)); 
  BOOST_CHECK(allNE(a, 1));
  
  BOOST_CHECK (allEQ(Matrix<int>(5u,6u, 4u), 4));
}

BOOST_AUTO_TEST_CASE( matrix_assign )
{
  Matrix<int> a(5u, 5u, 3), b;
  b.assign_conforming( 2*a );
  BOOST_CHECK(allEQ (b, 6));
  a.row(3) = 6;
  BOOST_CHECK(allEQ (a.row(3), 6));
  a.column(3) = 1;
  BOOST_CHECK(allEQ (a.column(3), 1));
  a.diagonal(-1) = 7;
  BOOST_CHECK(allEQ (a.diagonal(-1), 7));
}

BOOST_AUTO_TEST_CASE( matrix_slice )
{
	Matrix<int> a(5u, 5u, 3);
	Matrix<int> c = a(Slice(2,1), Slice(3,1));
	BOOST_CHECK_EQUAL(c.size(), 1);
	BOOST_CHECK_EQUAL(c(0, 0), 3);
}

BOOST_AUTO_TEST_CASE( matrix_reform )
{
  Matrix<int> a(5u, 5u, 3);
  IPosition l(1);
  l(0) = a.nelements();
  Vector<int> d(a.reform(l));
  for (int i = 0; i < 5; i++)
    for (int j = 0; j < 5; j++)
      BOOST_CHECK(a(i,j) == d(i + j*5));
}

BOOST_AUTO_TEST_CASE( matrix_from_vector )
{
  Vector<int> v(10);
  indgen(v);
  Matrix<int> vm(v);
  BOOST_CHECK_EQUAL(vm.ndim(), 2);
  BOOST_CHECK_EQUAL(vm.nelements(), v.nelements());
  for (int i = 0; i < int(v.nelements()); i++)
  {
    BOOST_CHECK_EQUAL(vm(i,0), v(i));
    BOOST_CHECK_EQUAL(v(i), i);
  }
}

// Cube tests

BOOST_AUTO_TEST_CASE( cube_initialize )
{
  Cube<int> c(3,3,3);
  c = 3;
  for (int k=0; k <= 2; k++)
    for (int j=0; j <= 2; j++)
      for (int i=0; i <= 2; i++)
        BOOST_CHECK(c(i,j,k) == 3);

  for (int k=0; k <= 2; k++)
    BOOST_CHECK(allEQ (c.xyPlane(k), 3));
}

BOOST_AUTO_TEST_CASE( cube_copy_constructor )
{
  Cube<int> c(3,3,3);
  // Check copy ctor
  Cube<int> c2(c);
  c(1,1,1) = -3;
  BOOST_CHECK_EQUAL(c2(1,1,1), -3);
}

BOOST_AUTO_TEST_CASE( cube_assignment )
{
  Cube<int> c(3,3,3), c3;
  c3.assign_conforming( c );
  BOOST_CHECK(allEQ (c3, c));
  Cube<int> y1(5, 6, 7, 4);
  BOOST_CHECK (allEQ(y1, 4));
}

BOOST_AUTO_TEST_CASE( cube_slice1 )
{
  Cube<int> c(3,3,3), c3 = c;
  // slice
  BOOST_CHECK(allEQ (c3 (Slice(0,2), Slice(1,2), 1),
    c (Slice(0,2), Slice(1,2), 1)));
}

BOOST_AUTO_TEST_CASE( cube_slice2 )
{
  Cube<int> c(3,3,3), c3 = c;
	Cube<int> c4(c3(Slice(0,2), Slice(1,2), 1));
	IPosition c4shape(c4.Array<int>::shape());
	BOOST_CHECK_EQUAL(c4.nelements(), 4);
  BOOST_CHECK_EQUAL(c4shape(2), 1);
}

BOOST_AUTO_TEST_CASE( cube_plane )
{
  Cube<int> c(3,3,3,3);
  // middle plane
  IPosition blc({0, 0, 1}), trc({2, 2, 1});
  c(blc, trc) = 11;
  BOOST_CHECK(allEQ (c.xyPlane(1), 11));
  BOOST_CHECK(allEQ (c.xyPlane(0), 3));
  BOOST_CHECK(allEQ (c.xyPlane(2), 3));
}

BOOST_AUTO_TEST_CASE( cube_index )
{
  Cube<int> c(3,3,3,3);
  IPosition blc({0, 0, 1}), trc({2, 2, 1});
  c(blc, trc) = 11;
  Array<int> cinx (c[1]);
  BOOST_CHECK (allEQ (cinx, c.xyPlane(1)));
  cinx.reference (cinx[0]);
  BOOST_CHECK_EQUAL (cinx.shape(), c.shape().getFirst(1));
  cinx.reference (cinx[0]);
  BOOST_CHECK_EQUAL (cinx.shape(), IPosition(1,1));
  cinx.reference (cinx[0]);
  BOOST_CHECK_EQUAL (cinx.shape(), IPosition(1,1));
  BOOST_CHECK (allEQ (cinx, 11));
}

BOOST_AUTO_TEST_CASE( matrix_storage1 )
{
  Matrix<int> m(8u, 8u, -1);
  m = -1;
  bool deleteIt;
  int *storage = m.getStorage(deleteIt);
  BOOST_CHECK_EQUAL(deleteIt, false);
  BOOST_CHECK_EQUAL(storage[0], -1);
  BOOST_CHECK_EQUAL(storage[8*8-1], -1);
  
  for (size_t i = 0; i < m.nelements(); i++)
    storage[i] = 1;
  BOOST_CHECK(allEQ (m, 1));
	m.putStorage(storage, deleteIt);
}

BOOST_AUTO_TEST_CASE( matrix_storage2 )
{
  Matrix<int> m(8u, 8u, 1);
  bool deleteIt;
	int *storage = m(Slice(0,2,3), Slice(2,2,4)).getStorage(deleteIt);
	BOOST_CHECK_EQUAL(deleteIt, true);
	for (int i=0; i < 4; i++)
    storage[i] = 0;
	BOOST_CHECK_EQUAL(m(0,2), 1);
  BOOST_CHECK_EQUAL(m(0,6), 1);
  BOOST_CHECK_EQUAL(m(3,2), 1);
  BOOST_CHECK_EQUAL(m(3,6), 1);
	m(Slice(0,2,3), Slice(2,2,4)).putStorage(storage, deleteIt);
	BOOST_CHECK_EQUAL(m(0,2), 0);
  BOOST_CHECK_EQUAL(m(0,6), 0);
  BOOST_CHECK_EQUAL(m(3,2), 0);
  BOOST_CHECK_EQUAL(m(3,6), 0);
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

// Various vector tests

BOOST_AUTO_TEST_CASE( vector_multi_dimensional_copy )
{
  // Test the Vector copy ctor for arrays with !1 dimension.
  Array<int> arr;
  Array<int> arr2(arr);
  BOOST_CHECK (arr2.ndim()==0  &&  arr2.nelements()==0);
  BOOST_CHECK (arr2.shape() == IPosition());
  Vector<int> vec(arr);
  BOOST_CHECK (vec.ndim()==1  &&  vec.nelements()==0);
  BOOST_CHECK (vec.shape() == IPosition(1,0));
  Matrix<int> mat(arr);
  BOOST_CHECK (mat.ndim()==2  &&  mat.nelements()==0);
  BOOST_CHECK (mat.shape() == IPosition(2,0));
  Cube<int> cub(arr);
  BOOST_CHECK (cub.ndim()==3  &&  cub.nelements()==0);
  BOOST_CHECK (cub.shape() == IPosition(3,0));
}

BOOST_AUTO_TEST_CASE( vector_multi_dimensional )
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

BOOST_AUTO_TEST_CASE( vector_multi_dimensional_slice )
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

BOOST_AUTO_TEST_CASE( vector_slice_single_element )
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

BOOST_AUTO_TEST_CASE( vector_take_part )
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

void checkRCDVec (const Vector<int>& v1, const Vector<int>& v2)
{
  BOOST_CHECK (allEQ(v1,v2));
  Array<int>::const_iterator iter1 = v1.begin();
  Array<int>::const_iterator iter2 = v2.begin();
  for (size_t i=0; i<v1.size(); ++i, ++iter1, ++iter2) {
    BOOST_CHECK (v1[i] == v2[i]);
    BOOST_CHECK (iter1 != v1.end());
    BOOST_CHECK (iter2 != v2.end());
    BOOST_CHECK (v1[i] == *iter1);
    BOOST_CHECK (v2[i] == *iter2);
  }
  BOOST_CHECK (iter1 == v1.end());
  BOOST_CHECK (iter2 == v2.end());
}

void checkRCD (const Vector<int>& vn, const Vector<int>& vc)
{
  Slice sl(1,3,2);  // start=1,n=3,inc=2
  checkRCDVec (vn, vc);
  checkRCDVec (vn(sl), vc(sl));
  checkRCDVec (vn(IPosition(1,1), IPosition(1,5), IPosition(1,2)), vn(sl));
  checkRCDVec (vc(IPosition(1,1), IPosition(1,5), IPosition(1,2)), vc(sl));
}

void doRowColDiag (const Matrix<int>& m)
{
  // Make contiguous copy of matrix.
  Matrix<int> cm(m.copy());
  BOOST_CHECK (cm.contiguousStorage());
  // Check row selection and subsetting.
  Vector<int> r0(m.row(1));
  Vector<int> cr0(cm.row(1));
  BOOST_CHECK (!r0.contiguousStorage() && !cr0.contiguousStorage());
  checkRCD (r0, cr0);
  // Check column selection and subsetting.
  Vector<int> c0(m.column(1));
  Vector<int> cc0(cm.column(1));
  BOOST_CHECK (cc0.contiguousStorage());
  checkRCD (c0, cc0);
  // Check diagonal selection and subsetting.
  Vector<int> d0(m.diagonal());
  Vector<int> cd0(cm.diagonal());
  BOOST_CHECK (!d0.contiguousStorage() && !cd0.contiguousStorage());
  checkRCD (d0, cd0);
}

BOOST_AUTO_TEST_CASE( row_col_diag )
{
  Matrix<int> m(18,18);
  indgen (m);
  doRowColDiag (m);
  doRowColDiag (m(IPosition(2,1,1), IPosition(2,12,12), IPosition(2,1,1)));
  doRowColDiag (m(IPosition(2,1,1), IPosition(2,12,12), IPosition(2,2,2)));
  doRowColDiag (m(IPosition(2,1,2), IPosition(2,17,12), IPosition(2,3,2)));
}

BOOST_AUTO_TEST_CASE( cube_init_from_data )
{
  IPosition shape(3, 2, 2, 2);
  std::unique_ptr<std::vector<int>> values(new std::vector<int>(shape.product()));
  Cube<int> c(shape, values->data(), COPY);
  values.reset();
  c.resize(IPosition(3, 2, 3, 2), false);
  c.resize(2, 4, 4, false);
  BOOST_CHECK(true);
}

BOOST_AUTO_TEST_CASE( matrix_init_from_data )
{
  IPosition shape(2, 2, 2);
  std::unique_ptr<std::vector<int>> values(new std::vector<int>(shape.product()));
  Matrix<int> c(shape, values->data(), COPY);
  values.reset();
  c.resize(IPosition(2, 2, 3), false);
  c.resize(4, 4, false);
  BOOST_CHECK(true);
}

BOOST_AUTO_TEST_CASE( vector_init_from_data )
{
  IPosition shape(1, 2);
  std::unique_ptr<std::vector<int>> values(new std::vector<int>(shape.product()));
  Vector<int> c(shape, values->data(), COPY);
  values.reset();
  c.resize(IPosition(1, 3), false);
  c.resize(4, false);
  BOOST_CHECK(true);
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
  std::allocator<LifecycleChecker> allocator;
  LifecycleChecker *ptr = allocator.allocate(shape.product());
  for (size_t i = 0; i < nelems; ++i) {
      allocator.construct(&ptr[i]);
  }
  {
    Array<LifecycleChecker> a;
    a.takeStorage(shape, ptr, SHARE, allocator);
    a.resize(IPosition(2, 3, 3), false);
  }
  for (size_t i = 0; i < nelems; ++i) {
    allocator.destroy(&ptr[i]);
  }
  allocator.deallocate(ptr, nelems);
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
    a.takeStorage(shape, ptr, TAKE_OVER, allocator);
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
      a.takeStorage(shape, ptr, COPY, allocator);
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
    Array<LifecycleChecker> a(shape, ptr, SHARE, allocator);
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
    Array<LifecycleChecker> a(shape, ptr, TAKE_OVER, allocator);
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
      Array<LifecycleChecker> a(shape, ptr, COPY, allocator);
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
  Cube<int> c = a1.nonDegenerate(1);
  BOOST_CHECK(c(0,1,2) == 5);
  c(0,1,2) = 99;
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
  Cube<int> c = a1.nonDegenerate(1);
  c(0,1,2) = 99;
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
  Cube<int> c = a1.nonDegenerate(1);
  c(0,1,2) = 99;
  
   Array<int> a4;
  a4.nonDegenerate(a1);
  BOOST_CHECK(a4.shape() == IPosition(2,2,3));
  BOOST_CHECK(a4(IPosition(2,0,2)) == 4);
  BOOST_CHECK(a4(IPosition(2,1,2)) == 99);
  a4.nonDegenerate(a1, 1);
  BOOST_CHECK(a4.shape() == IPosition(3,1,2,3));
  BOOST_CHECK(a4(IPosition(3,0,0,0)) == 0);
  BOOST_CHECK(a4(IPosition(3,0,1,2)) == 99);
  
  // Test if a non-degerate Cube throws an exception.
  bool caught = false;
  Cube<int> c1(IPosition(3,1,2,3));
  Cube<int> cr;
  try {
    cr.nonDegenerate(c1);
  } catch (const std::exception&) {
    caught = true;
  }
  BOOST_CHECK (caught);
  cr.nonDegenerate(c1, 1);
  BOOST_CHECK (cr.shape() == IPosition(3,1,2,3));
  
  // Test if a non-degerate Matrix throws an exception.
  Matrix<int> m1(IPosition(2,1,2));
  Matrix<int> mr;
  try {
    mr.nonDegenerate(m1);
  } catch (const std::exception&) {
    caught = true;
  }
  BOOST_CHECK (caught);
  mr.nonDegenerate(m1, 1);
  BOOST_CHECK (mr.shape() == IPosition(2,1,2));
}

BOOST_AUTO_TEST_CASE( add_generate )
{
  Array<int> a1(IPosition(2,10,10));
  indgen(a1);
  BOOST_CHECK(a1.addDegenerate(1u).shape()==IPosition(3,10,10,1));

  Matrix<int> m = a1(IPosition(2,1),IPosition(2,3),IPosition(2,2));
  BOOST_CHECK(m(0,0) == 11);
  BOOST_CHECK(m(1,1) == 33);
  Array<int> md(m.addDegenerate(2u));
  BOOST_CHECK(md.shape() == IPosition(4,2,2,1,1));
  BOOST_CHECK(md(IPosition(4,0)) == 11);
  BOOST_CHECK(md(IPosition(4,1,1,0,0)) == 33);
  md(IPosition(4,0)) = 100;
  BOOST_CHECK(m(0,0) == 100);

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

BOOST_AUTO_TEST_CASE( copying_vector_resize )
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

BOOST_AUTO_TEST_CASE( matrix_reference_1d_array )
{
  // Matrix.reference(1-d array)
  Array<int> ai(IPosition(1,10));
  Matrix<int> mi;
  mi.reference(ai);
  BOOST_CHECK(mi.shape() == IPosition(2,10,1));
  ai = 11;
  BOOST_CHECK(allEQ(mi, 11));
}

BOOST_AUTO_TEST_CASE( array_assign )
{
  // Array assign
  Array<int> ai(IPosition(1,10));
  ai = 1;
  Matrix<int> mi(5,3);
  mi = 2;
  bool exc = false;
  try {
    mi.assign (ai);
  } catch (std::exception&) {
    exc = true;
  }
  BOOST_CHECK (exc);
  BOOST_CHECK(mi.shape() == IPosition(2,5,3));
  BOOST_CHECK(allEQ(mi, 2));
  ai.assign (mi);
  BOOST_CHECK(ai.shape() == IPosition(2,5,3));
  BOOST_CHECK(allEQ(ai, 2));
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

BOOST_AUTO_TEST_CASE( matrix_identity )
{
  for (size_t i=0; i<20; i++) {
    Matrix<double> x = Matrix<double>::identity(i);
    BOOST_CHECK(x.ncolumn() == i);
    BOOST_CHECK(x.nrow() == i);
    for (size_t j=0; j<i; j++) {
      for (size_t k=0; k<i; k++) {
        if (j == k) {
          BOOST_CHECK(x(j, k) == 1);
        } else {
          BOOST_CHECK(x(j, k) == 0);
        }
      }
    }
  }
}

BOOST_AUTO_TEST_SUITE_END()
