#include <boost/test/unit_test.hpp>

#include <cstdint>
#include <limits>

#include "../Cube.h"

using namespace casacore;

BOOST_AUTO_TEST_SUITE(cube_class)

// Cube tests

BOOST_AUTO_TEST_CASE( initialize )
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

BOOST_AUTO_TEST_CASE( copy_constructor )
{
  Cube<int> c(3,3,3);
  // Check copy ctor
  Cube<int> c2(c);
  c(1,1,1) = -3;
  BOOST_CHECK_EQUAL(c2(1,1,1), -3);
}

BOOST_AUTO_TEST_CASE( assignment )
{
  Cube<int> c(3,3,3), c3;
  c3.assign_conforming( c );
  BOOST_CHECK(allEQ (c3, c));
  Cube<int> y1(5, 6, 7, 4);
  BOOST_CHECK (allEQ(y1, 4));
}

BOOST_AUTO_TEST_CASE( slice1 )
{
  Cube<int> c(3,3,3), c3 = c;
  // slice
  BOOST_CHECK(allEQ (c3 (Slice(0,2), Slice(1,2), 1),
    c (Slice(0,2), Slice(1,2), 1)));
}

BOOST_AUTO_TEST_CASE( slice2 )
{
  Cube<int> c(3,3,3), c3 = c;
	Cube<int> c4(c3(Slice(0,2), Slice(1,2), 1));
	IPosition c4shape(c4.Array<int>::shape());
	BOOST_CHECK_EQUAL(c4.nelements(), 4);
  BOOST_CHECK_EQUAL(c4shape(2), 1);
}

BOOST_AUTO_TEST_CASE( plane )
{
  Cube<int> c(3,3,3,3);
  // middle plane
  IPosition blc({0, 0, 1}), trc({2, 2, 1});
  c(blc, trc) = 11;
  BOOST_CHECK(allEQ (c.xyPlane(1), 11));
  BOOST_CHECK(allEQ (c.xyPlane(0), 3));
  BOOST_CHECK(allEQ (c.xyPlane(2), 3));
}

BOOST_AUTO_TEST_CASE( index )
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

BOOST_AUTO_TEST_CASE( init_from_data )
{
  IPosition shape(3, 2, 2, 2);
  std::unique_ptr<std::vector<int>> values(new std::vector<int>(shape.product()));
  Cube<int> c(shape, values->data(), COPY);
  values.reset();
  c.resize(IPosition(3, 2, 3, 2), false);
  c.resize(2, 4, 4, false);
  BOOST_CHECK(true);
}

BOOST_AUTO_TEST_CASE( multi_dimensional_copy )
{
  // Test the copy ctor for arrays with !1 dimension.
  Array<int> arr;
  Cube<int> cub(arr);
  BOOST_CHECK (cub.ndim()==3  &&  cub.nelements()==0);
  BOOST_CHECK (cub.shape() == IPosition(3,0));
}

BOOST_AUTO_TEST_CASE( non_degenerate )
{
  // Test if a non-degerate Cube throws an exception.
  Cube<int> c1(IPosition(3,1,2,3));
  Cube<int> cr;
  BOOST_CHECK_THROW(cr.nonDegenerate(c1), std::exception);
  cr.nonDegenerate(c1, 1);
  BOOST_CHECK (cr.shape() == IPosition(3,1,2,3));
}

BOOST_AUTO_TEST_SUITE_END()
