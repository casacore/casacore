#include <boost/test/unit_test.hpp>

#include <cstdint>
#include <limits>

#include "../Array.h"
#include "../ArrayMath.h"
#include "../ArrayLogical.h"
#include "../Matrix.h"

using namespace casacore;

BOOST_AUTO_TEST_SUITE(array_operations)

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

BOOST_AUTO_TEST_SUITE_END()
