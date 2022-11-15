//# tMaskedArray.cc: Test program for MaskedArrays
//# Copyright (C) 1993,1994,1995,1996,1998,1999,2000,2001
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

#include "../IPosition.h"
#include "../Array.h"
#include "../ArrayMath.h"
#include "../ArrayLogical.h"
//#include "../ArrayIO.h"
#include "../Vector.h"
#include "../Matrix.h"
#include "../Cube.h"
#include "../ArrayError.h"
#include "../MaskedArray.h"
#include "../MaskArrIO.h"
#include "../LogiCube.h"
#include "../LogiVector.h"

#include "TestUtilities.h"

#include <boost/test/unit_test.hpp>

#include <initializer_list>

using namespace casacore;

BOOST_AUTO_TEST_SUITE(masked_array)

BOOST_AUTO_TEST_CASE(assignment)
{
  const IPosition ip(1,5);
  Vector<int> x(ip), y(ip);
  LogicalArray b(ip);
  Array<int> z(ip);
  
  bool xOK = x.ok();
  BOOST_CHECK(xOK);
  
  size_t xNdim = x.ndim();
  BOOST_CHECK_EQUAL(xNdim, 1);
  
  size_t xNelements = x.nelements();
  BOOST_CHECK_EQUAL(xNelements, 5);
  
  IPosition xShape = x.shape();
  BOOST_CHECK_EQUAL(xShape, IPosition(1,5));
  
  x=1;
  indgen(y);
  b = (x <= y);
  
  check(x, {1, 1, 1, 1, 1});
  check(y, {0, 1, 2, 3, 4});
  check(b, {0, 1, 1, 1, 1});
  
  MaskedArray<int> m(z, b);
  z=0;
  check(z, {0, 0, 0, 0, 0});
  m = 5;
  check(z, {0, 5, 5, 5, 5});
  
  MaskedArray<int> n(m);
  n = 6;
  check(z, {0, 6, 6, 6, 6});
  z(b) = 7;
  check(z, {0, 7, 7, 7, 7});
  z (x <= y) = 8;
  check(z, {0, 8, 8, 8, 8});
}

BOOST_AUTO_TEST_CASE(assignment_to_vector)
{
  Vector<float> vec1{1, 2, 3, 4, 5};
  Vector<float> vec2{6, 7, 8, 9, 10};
  LogicalVector lvec{true, true, false, true, true};
  vec1 = vec2(lvec);
  Vector<float> ref{6, 7, 3, 9, 10};
  BOOST_CHECK_EQUAL_COLLECTIONS(vec1.begin(), vec1.end(), ref.begin(), ref.end());
}

BOOST_AUTO_TEST_CASE(array_operations1)
{
  Array<int> yc (IPosition(1,24));
  indgen (yc);
  const Array<int> ycc (yc);
  
  check(ycc(ycc<18).getArray(), {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23}); 
  check(ycc(ycc<18).getMask(), {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0}); 
  
  check(yc((yc<18))(yc>3),
    {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23},
    {0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0});

  (yc(yc<18))(yc>3) = 8;
  check(yc, {0, 1, 2, 3, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 18, 19, 20, 21, 22, 23});
}

BOOST_AUTO_TEST_CASE(array_operations2)
{
  Array<int> yc (IPosition(1,24));
  indgen (yc);
  
  check(yc((yc<18)(yc>3)),
    {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23},
    {0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0});
  
  yc((yc<18)(yc>3)) = 8;
  check(yc, {0, 1, 2, 3, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 18, 19, 20, 21, 22, 23});
}

BOOST_AUTO_TEST_CASE(array_operations3)
{
  Array<int> yc (IPosition(1,24));
  indgen (yc);
  
  check((yc(yc>7)) ((yc<18)(yc>3)),
        {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23},
        {0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0});
  
  (yc(yc>7)) ((yc<18)(yc>3)) = 8;
  check(yc, {0, 1, 2, 3, 4, 5, 6, 7, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 18, 19, 20, 21, 22, 23});
}

BOOST_AUTO_TEST_CASE(array_operations4)
{
  Array<int> yc (IPosition(1,24));
  indgen (yc);
  const Array<int> ycc  (yc);
  const MaskedArray<int> ycc1 (yc, yc<18);
  MaskedArray<int> ycc2 (yc, yc<18);
  const MaskedArray<int> ycc3 (yc, yc<18);
  MaskedArray<int> ycc4 (yc, yc<18);
  
  check(ycc1(ycc>7),
        {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23},
        {0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0});
  
  check(ycc2(ycc>7),
        {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23},
        {0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0});
  
  check(ycc3(ycc>7),
        {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23},
        {0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0});
  
  check(ycc4(ycc>7),
        {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23},
        {0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0});
  
  check(yc(yc>7),
        {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23},
        {0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1});
  
  check(ycc(ycc>7),
        {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23},
        {0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1});
}

BOOST_AUTO_TEST_CASE(array_operations5)
{
  Array<int> yc (IPosition(2,20,30));
  indgen (yc);
  Array<int> ycc  (yc);
  MaskedArray<int> ycc1 (yc, yc<318||yc/2*2==yc);
  MaskedArray<int> ycc2 (ycc1(IPosition(2,15,14),
                              IPosition(2,18,19)));
  
  check(ycc2,
  { 295, 296, 297, 298,
    315, 316, 317, 318,
    335, 336, 337, 338,
    355, 356, 357, 358,
    375, 376, 377, 378,
    395, 396, 397, 398 },
  { 1, 1, 1, 1,
    1, 1, 1, 1,
    0, 1, 0, 1,
    0, 1, 0, 1,
    0, 1, 0, 1,
    0, 1, 0, 1 });
  
  check(ycc2(IPosition(2,1,0), IPosition(2,3,4), IPosition(2,1,2)),
  { 296, 297, 298,
    336, 337, 338,
    376, 377, 378 },
  { 1, 1, 1,
    1, 0, 1,
    1, 0, 1 });
}

BOOST_AUTO_TEST_CASE(vector_operations1)
{
  Vector<int> w(5), z({0, 8, 8, 8, 8});
  indgen (w, 6);

  w.assign_conforming( z(w<z) );
  check(w, {6, 8, 8, 9, 10});
}

BOOST_AUTO_TEST_CASE(vector_operations2)
{
  Vector<int> yc (24);
  indgen (yc);
  
  const Vector<int> ycc (yc);
  
  check(ycc(ycc<18),
        {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23},
        {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0});
  
  check((yc(yc<18))(yc>3),
        {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23},
        {0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0});
  
  (yc(yc<18))(yc>3) = 8;
  check(yc, {0, 1, 2, 3, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 18, 19, 20, 21, 22, 23});
}

BOOST_AUTO_TEST_CASE(vector_operations3)
{
  Vector<int> yc (24);
  indgen (yc);
  
  check(yc((yc<18)(yc>3)),
        {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23},
        {0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0});
  
  yc((yc<18)(yc>3)) = 8;
  check(yc, {0, 1, 2, 3, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 18, 19, 20, 21, 22, 23});
}

BOOST_AUTO_TEST_CASE(vector_operations4)
{
  Vector<int> yc (24);
  indgen (yc);
  
  check( (yc(yc>7)) ((yc<18)(yc>3)),
         {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23},
         {0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0});
  
  (yc(yc>7)) ((yc<18)(yc>3)) = 8;
  
  check(yc, {0, 1, 2, 3, 4, 5, 6, 7, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 18, 19, 20, 21, 22, 23});
}

BOOST_AUTO_TEST_CASE(matrix_operations1)
{
  Matrix<int> v8(5u,3u);
  Matrix<int> v(5u,3u);
  v8 = 8;
  indgen (v);

  check(v8,
  { 8, 8, 8, 8, 8,
    8, 8, 8, 8, 8,
    8, 8, 8, 8, 8 });
  check(v,
  { 0, 1, 2, 3, 4,
    5, 6, 7, 8, 9,
    10, 11, 12, 13, 14 } );

  v.assign_conforming( v8(v<v8) );
  check(v, 
  { 8, 8, 8, 8, 8,
    8, 8, 8, 8, 9,
    10, 11, 12, 13, 14 });

  v(v<9) = 9;
  check(v, 
  { 9, 9, 9, 9, 9,
    9, 9, 9, 9, 9,
    10, 11, 12, 13, 14 });
}

BOOST_AUTO_TEST_CASE(matrix_operations2)
{
  Matrix<int> yc (4u,6u);
  indgen (yc);
  const Matrix<int> ycc (yc);
  
  check(ycc(ycc<18), 
        {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23},
        {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0});
  
  check((yc(yc<18))(yc>3),
        {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23},
        {0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0});
  
  (yc(yc<18))(yc>3) = 8;
  check(yc, {0, 1, 2, 3, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 18, 19, 20, 21, 22, 23});
}

BOOST_AUTO_TEST_CASE(matrix_operations3)
{
  Matrix<int> yc (4u,6u);
  indgen (yc);
  
  check( yc((yc<18)(yc>3)),
        {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23},
        {0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0});
  
  yc((yc<18)(yc>3)) = 8;
  check(yc, {0, 1, 2, 3, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 18, 19, 20, 21, 22, 23});
}

BOOST_AUTO_TEST_CASE(matrix_operations4)
{
  Matrix<int> yc (4u,6u);
  indgen (yc);
  
  check( (yc(yc>7)) ((yc<18)(yc>3)),
         {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23},
         {0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0});
  
  (yc(yc>7)) ((yc<18)(yc>3)) = 8;
  check(yc, {0, 1, 2, 3, 4, 5, 6, 7, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 18, 19, 20, 21, 22, 23});
}

BOOST_AUTO_TEST_CASE(cube_operations1)
{
  Cube<int> u15(5,3,2);
  Cube<int> u(5,3,2);
  u15 = 15;
  indgen (u);

  BOOST_CHECK_EQUAL(to_string(u15),
    "Ndim=3 Axis Lengths: [5, 3, 2] \n"
    "[0, 0, 0][15, 15, 15, 15, 15]\n"
    "[0, 1, 0][15, 15, 15, 15, 15]\n"
    "[0, 2, 0][15, 15, 15, 15, 15]\n"
    "[0, 0, 1][15, 15, 15, 15, 15]\n"
    "[0, 1, 1][15, 15, 15, 15, 15]\n"
    "[0, 2, 1][15, 15, 15, 15, 15]\n");

  BOOST_CHECK_EQUAL(to_string(u),
    "Ndim=3 Axis Lengths: [5, 3, 2] \n"
    "[0, 0, 0][0, 1, 2, 3, 4]\n"
    "[0, 1, 0][5, 6, 7, 8, 9]\n"
    "[0, 2, 0][10, 11, 12, 13, 14]\n"
    "[0, 0, 1][15, 16, 17, 18, 19]\n"
    "[0, 1, 1][20, 21, 22, 23, 24]\n"
    "[0, 2, 1][25, 26, 27, 28, 29]\n");

  u.assign_conforming( u15(u<u15) );
  BOOST_CHECK_EQUAL(to_string(u),
    "Ndim=3 Axis Lengths: [5, 3, 2] \n"
    "[0, 0, 0][15, 15, 15, 15, 15]\n"
    "[0, 1, 0][15, 15, 15, 15, 15]\n"
    "[0, 2, 0][15, 15, 15, 15, 15]\n"
    "[0, 0, 1][15, 16, 17, 18, 19]\n"
    "[0, 1, 1][20, 21, 22, 23, 24]\n"
    "[0, 2, 1][25, 26, 27, 28, 29]\n");
  u(u<16) = 16;
  BOOST_CHECK_EQUAL(to_string(u),
    "Ndim=3 Axis Lengths: [5, 3, 2] \n"
    "[0, 0, 0][16, 16, 16, 16, 16]\n"
    "[0, 1, 0][16, 16, 16, 16, 16]\n"
    "[0, 2, 0][16, 16, 16, 16, 16]\n"
    "[0, 0, 1][16, 16, 17, 18, 19]\n"
    "[0, 1, 1][20, 21, 22, 23, 24]\n"
    "[0, 2, 1][25, 26, 27, 28, 29]\n");  
}

BOOST_AUTO_TEST_CASE(cube_operations2)
{
  Cube<int> yc (4,3,2);
  indgen (yc);
  
  const Cube<int> ycc (yc);
  check(ycc(ycc<18),
        {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23},
        {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0});
  
  check((yc(yc<18))(yc>3),
        {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23},
        {0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0});
  
  (yc(yc<18))(yc>3) = 8;
  check(yc, {0, 1, 2, 3, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 18, 19, 20, 21, 22, 23});
}

BOOST_AUTO_TEST_CASE(cube_operations3)
{
  Cube<int> yc (4,3,2);
  indgen (yc);
  
  check((yc(yc<18))(yc>3),
        {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23},
        {0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0});
  
  yc((yc<18)(yc>3)) = 8;
  check(yc, {0, 1, 2, 3, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 18, 19, 20, 21, 22, 23});
}

BOOST_AUTO_TEST_CASE(cube_operations4)
{
  Cube<int> yc (4,3,2);
  indgen (yc);
  
  check((yc(yc>7)) ((yc<18)(yc>3)),
         {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23},
         {0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0});
  
  (yc(yc>7)) ((yc<18)(yc>3)) = 8;
  check(yc, {0, 1, 2, 3, 4, 5, 6, 7, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 18, 19, 20, 21, 22, 23});
}

struct MCUFixture
{
  Cube<int> cu;
  LogicalCube lcu;
  MCUFixture() :
    cu (5, 3, 2),
    lcu (5, 3, 2)
  {
    indgen (cu);
    lcu.assign_conforming( (cu > 10) && (cu <= 20) );
  }
};

BOOST_FIXTURE_TEST_CASE(compressed_array1, MCUFixture)
{
  MaskedArray<int> mcu (cu, lcu);
  BOOST_CHECK_EQUAL(to_string(mcu),
    "Array: Ndim=3 Axis Lengths: [5, 3, 2] \n"
    "[0, 0, 0][0, 1, 2, 3, 4]\n"
    "[0, 1, 0][5, 6, 7, 8, 9]\n"
    "[0, 2, 0][10, 11, 12, 13, 14]\n"
    "[0, 0, 1][15, 16, 17, 18, 19]\n"
    "[0, 1, 1][20, 21, 22, 23, 24]\n"
    "[0, 2, 1][25, 26, 27, 28, 29]\n"
    "\n"
    "Mask:  Ndim=3 Axis Lengths: [5, 3, 2] \n"
    "[0, 0, 0][0, 0, 0, 0, 0]\n"
    "[0, 1, 0][0, 0, 0, 0, 0]\n"
    "[0, 2, 0][0, 1, 1, 1, 1]\n"
    "[0, 0, 1][1, 1, 1, 1, 1]\n"
    "[0, 1, 1][1, 0, 0, 0, 0]\n"
    "[0, 2, 1][0, 0, 0, 0, 0]\n");
}

BOOST_FIXTURE_TEST_CASE(compressed_array2, MCUFixture)
{
  MaskedArray<int> mcu (cu, lcu);
  Vector<int> vec (mcu.getCompressedArray());
  check(vec, {11, 12, 13, 14, 15, 16, 17, 18, 19, 20});
}

BOOST_FIXTURE_TEST_CASE(compressed_array3, MCUFixture)
{
  MaskedArray<int> mcu (cu, lcu);
  Matrix<int> mat (mcu.getCompressedArray (IPosition (2,5,2)));
  check(mat, {11, 12, 13, 14, 15, 16, 17, 18, 19, 20});
}

BOOST_FIXTURE_TEST_CASE(compressed_array4, MCUFixture)
{
  MaskedArray<int> mcu (cu, lcu);
  Matrix<int> mat (5u,2u);
  mcu.getCompressedArray (mat);
  check(mat, {11, 12, 13, 14, 15, 16, 17, 18, 19, 20});
  
  mcu.setCompressedArray (-mat);
  
  BOOST_CHECK_EQUAL(to_string(mcu),
    "Array: Ndim=3 Axis Lengths: [5, 3, 2] \n"
    "[0, 0, 0][0, 1, 2, 3, 4]\n"
    "[0, 1, 0][5, 6, 7, 8, 9]\n"
    "[0, 2, 0][10, -11, -12, -13, -14]\n"
    "[0, 0, 1][-15, -16, -17, -18, -19]\n"
    "[0, 1, 1][-20, 21, 22, 23, 24]\n"
    "[0, 2, 1][25, 26, 27, 28, 29]\n"
    "\n"
    "Mask:  Ndim=3 Axis Lengths: [5, 3, 2] \n"
    "[0, 0, 0][0, 0, 0, 0, 0]\n"
    "[0, 1, 0][0, 0, 0, 0, 0]\n"
    "[0, 2, 0][0, 1, 1, 1, 1]\n"
    "[0, 0, 1][1, 1, 1, 1, 1]\n"
    "[0, 1, 1][1, 0, 0, 0, 0]\n"
    "[0, 2, 1][0, 0, 0, 0, 0]\n");
}

struct MCU2
{
  Cube<int> cu;
  LogicalCube lcu;
  MCU2() :
  cu (5u, 3u, 2u),
  lcu(5u, 3u, 2u)
  {
    indgen (cu);
    lcu.assign_conforming( (cu > 10) && (cu <= 20) );
  }
};

BOOST_FIXTURE_TEST_CASE(read_only1, MCU2)
{
  MaskedArray<int> mcu (cu, lcu);
  BOOST_CHECK(!mcu.isReadOnly());
}

BOOST_FIXTURE_TEST_CASE(read_only2, MCU2)
{
  MaskedArray<int> mcu (cu, lcu, false);
  BOOST_CHECK(!mcu.isReadOnly());
}

BOOST_FIXTURE_TEST_CASE(read_only3, MCU2)
{
  MaskedArray<int> mcu (cu, lcu, true);
  BOOST_CHECK(mcu.isReadOnly());
}

BOOST_FIXTURE_TEST_CASE(read_only4, MCU2)
{
  MaskedArray<int> mcu (cu, lcu);
  mcu.setReadOnly();
  BOOST_CHECK(mcu.isReadOnly());
}

BOOST_FIXTURE_TEST_CASE(read_only5, MCU2)
{
  MaskedLogicalArray mlcu (lcu, lcu);
  MaskedArray<int> mcu (cu, mlcu, true);
  BOOST_CHECK(mcu.isReadOnly());
}

BOOST_FIXTURE_TEST_CASE(read_only6, MCU2)
{
  MaskedArray<int> mmcu (cu, lcu);
  MaskedArray<int> mcu (mmcu, lcu, true);
  BOOST_CHECK(mcu.isReadOnly());
}

BOOST_FIXTURE_TEST_CASE(read_only7, MCU2)
{
  MaskedArray<int> mmcu (cu, lcu);
  MaskedLogicalArray mlcu (lcu, lcu);
  MaskedArray<int> mcu (mmcu, mlcu, true);
  BOOST_CHECK(mcu.isReadOnly());
}

BOOST_FIXTURE_TEST_CASE(read_only8, MCU2)
{
  MaskedArray<int> mmcu (cu, lcu, true);
  MaskedArray<int> mcu (mmcu, lcu, false);
  BOOST_CHECK(mcu.isReadOnly());
}

BOOST_FIXTURE_TEST_CASE(read_only9, MCU2)
{
  MaskedArray<int> mmcu (cu, lcu, true);
  MaskedLogicalArray mlcu (lcu, lcu);
  MaskedArray<int> mcu (mmcu, mlcu, false);
  BOOST_CHECK(mcu.isReadOnly());
}

BOOST_FIXTURE_TEST_CASE(read_only10, MCU2)
{
  const Array<int> ccu (cu);
  BOOST_CHECK(ccu(lcu).isReadOnly());
}  

BOOST_FIXTURE_TEST_CASE(read_only11, MCU2)
{
  MaskedArray<int> mcu (cu, lcu, true);
  BOOST_CHECK(mcu(lcu).isReadOnly());
}

BOOST_FIXTURE_TEST_CASE(read_only12, MCU2)
{
  MaskedArray<int> mmcu (cu, lcu, true);
  MaskedArray<int> mcu (mmcu);
  BOOST_CHECK(mcu.isReadOnly());
}

BOOST_FIXTURE_TEST_CASE(read_only13, MCU2)
{
  MaskedArray<int> mmcu (cu, lcu, true);
  MaskedArray<int> mcu (mmcu, false);
  BOOST_CHECK(mcu.isReadOnly());
}

BOOST_FIXTURE_TEST_CASE(read_only14, MCU2)
{
  MaskedArray<int> mmcu (cu, lcu, true);
  MaskedArray<int> mcu (mmcu, true);
  BOOST_CHECK(mcu.isReadOnly());
}

BOOST_FIXTURE_TEST_CASE(read_only15, MCU2)
{
  MaskedArray<int> mcu (cu, lcu, true);
  BOOST_CHECK(!mcu.copy().isReadOnly()); 
}

BOOST_FIXTURE_TEST_CASE(read_only16, MCU2)
{
  MaskedArray<int> mcu (cu, lcu, true);
  BOOST_CHECK(!mcu.copy(false).isReadOnly()); 
}

BOOST_FIXTURE_TEST_CASE(read_only17, MCU2)
{
  MaskedArray<int> mcu (cu, lcu);
  BOOST_CHECK(mcu.copy(true).isReadOnly());
}

BOOST_FIXTURE_TEST_CASE(read_only18, MCU2)
{
  MaskedArray<int> mcu (cu, lcu);
  BOOST_CHECK(!mcu.copy(false).isReadOnly());
}

BOOST_AUTO_TEST_SUITE_END()
