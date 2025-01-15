//# tMatrixMath.cc: Test functions in MatrixMath.h
//# Copyright (C) 1995,1996,1999,2001, 2004
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

//# Includes

#include "../MatrixMath.h"
#include "../ArrayMath.h"
#include "../ArrayLogical.h"

#include <boost/test/unit_test.hpp>

using namespace casacore;

BOOST_AUTO_TEST_SUITE(matrix_math)

BOOST_AUTO_TEST_CASE( all )
{
  Matrix<double> ind(3,3);
  ind(0,0) = 2; ind(0,1) = 8; ind(0,2) = 6;
  ind(1,0) = 4; ind(1,1) = 2; ind(1,2) = -2;
  ind(2,0) = 3; ind(2,1) = -1; ind(2,2) = 1;

  Matrix<double> outd(3,3);
  outd(0,0) = 2; outd(0,1) = 4; outd(0,2) = 3;
  outd(1,0) = 8; outd(1,1) = 2; outd(1,2) = -1;
  outd(2,0) = 6; outd(2,1) = -2; outd(2,2) = 1;

  BOOST_CHECK(allNearAbs(transpose(ind), outd, 0.00001));

  // Now test the other types - float/std::complex<float>/std::complex<double>

  Matrix<float> inf(3,3), outf(3,3);
  convertArray(inf, ind); convertArray(outf, outd);
  BOOST_CHECK(allNearAbs(transpose(inf), outf, 0.00001));

  Matrix<std::complex<float>> inc(3,3), outc(3,3);
  convertArray(inc, ind); convertArray(outc, outd);
  BOOST_CHECK(allNearAbs(transpose(inc), outc, 0.00001));

  Matrix<std::complex<double>> indc(3,3), outdc(3,3);
  convertArray(indc, ind); convertArray(outdc, outd);
  BOOST_CHECK(allNearAbs(transpose(indc), outdc, 0.00001));

  Vector<double> a(2, 2);
  Vector<double> b(2, 3);
  BOOST_CHECK_EQUAL(crossProduct2D(a, b), 0);
  a[0] = -2;
  BOOST_CHECK_EQUAL(crossProduct2D(a, b),-12);
  b[1] = -5;
  BOOST_CHECK_EQUAL(crossProduct2D(a, b), 4);
}

BOOST_AUTO_TEST_SUITE_END()
