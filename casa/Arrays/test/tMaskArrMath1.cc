//# tMaskArrMath1.cc: Test program for MaskedArrays mathematical operations.
//# Copyright (C) 1993,1994,1995,1996,1999,2000,2001
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
//# $Id$

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
#include "../MaskArrMath.h"

#include <complex>

#include <boost/test/unit_test.hpp>

using namespace casacore;

BOOST_AUTO_TEST_SUITE(masked_array_math1)

struct Fixture {
  Vector<double> df, dg, dh;
  Vector<bool> b;
  Fixture() :
    df(10), dg(10), dh(10),
    b(10)
  {
    indgen (df);
    indgen (dg, 2.0);
    dh = 2.0;
  }
};
  

BOOST_FIXTURE_TEST_CASE(cos_func, Fixture)
{
// Math
  dh.assign_conforming( cos (df ((df > 2.5) && (df < 6.5))) );
  Vector<double> ref{2, 2, 2, -0.989992, -0.653644, 0.283662, 0.96017, 2, 2, 2};
  BOOST_CHECK(allNear(dh, ref, 1e-4));
}

BOOST_FIXTURE_TEST_CASE(atan2_func1, Fixture)
{
  dh.assign_conforming(atan2 (df ((df > 2.5) && (df < 6.5)), dg) );
  Vector<double> ref{2, 2, 2, 0.54042, 0.588003, 0.620249, 0.643501, 2, 2, 2};
  BOOST_CHECK(allNear(dh, ref, 1e-4));
}

BOOST_FIXTURE_TEST_CASE(atan2_func2, Fixture)
{
  dh.assign_conforming(atan2 (dg, df ((df > 2.5) && (df < 6.5))) );
  Vector<double> ref{2, 2, 2, 1.03038, 0.982794, 0.950547, 0.927295, 2, 2, 2};
  BOOST_CHECK(allNear(dh, ref, 1e-4));
}

BOOST_FIXTURE_TEST_CASE(atan2_func3, Fixture)
{
  dh.assign_conforming(atan2 (dg ((df > 3.5) && (df < 7.5)),
                              df ((df > 2.5) && (df < 6.5))) );
  Vector<double> ref{2, 2, 2, 2, 0.982794, 0.950547, 0.927295, 2, 2, 2};
  BOOST_CHECK(allNear(dh, ref, 1e-4));
}

BOOST_FIXTURE_TEST_CASE(atan2_func4, Fixture)
{
  dh.assign_conforming(atan2 (df ((df > 2.5) && (df < 6.5)), 2.0) );
  Vector<double> ref{2, 2, 2, 0.982794, 1.10715, 1.19029, 1.24905, 2, 2, 2};
  BOOST_CHECK(allNear(dh, ref, 1e-4));
}

BOOST_FIXTURE_TEST_CASE(atan2_func5, Fixture)
{
  dh.assign_conforming(atan2 (2.0, df ((df > 2.5) && (df < 6.5))) );
  Vector<double> ref{2, 2, 2, 0.588003, 0.463648, 0.380506, 0.321751, 2, 2, 2};
  BOOST_CHECK(allNear(dh, ref, 1e-4));
}

BOOST_FIXTURE_TEST_CASE(pow_func1, Fixture)
{
  dh.assign_conforming(pow (df ((df > 2.5) && (df < 6.5)), dg) );
  Vector<double> ref{2, 2, 2, 243, 4096, 78125, 1.67962e+06, 2, 2, 2};
  BOOST_CHECK(allNear(dh, ref, 1e-4));
}

BOOST_FIXTURE_TEST_CASE(pow_func2, Fixture)
{
  dh.assign_conforming(pow (dg, df ((df > 2.5) && (df < 6.5))) );
  Vector<double> ref{2, 2, 2, 125, 1296, 16807, 262144, 2, 2, 2};
  BOOST_CHECK(allNear(dh, ref, 1e-4));
}

BOOST_FIXTURE_TEST_CASE(pow_func3, Fixture)
{
  dh.assign_conforming(pow (dg ((df > 3.5) && (df < 7.5)),
                            df ((df > 2.5) && (df < 6.5))) );
  Vector<double> ref{2, 2, 2, 2, 1296, 16807, 262144, 2, 2, 2};
  BOOST_CHECK(allNear(dh, ref, 1e-4));
}

BOOST_FIXTURE_TEST_CASE(pow_func4, Fixture)
{
  dh.assign_conforming(pow (df ((df > 2.5) && (df < 6.5)), 2.0) );
  Vector<double> ref{2, 2, 2, 9, 16, 25, 36, 2, 2, 2};
  BOOST_CHECK(allNear(dh, ref, 1e-4));
}

BOOST_FIXTURE_TEST_CASE(sum_func, Fixture)
{
  BOOST_CHECK_CLOSE_FRACTION( sum (df ((df > 2.5) && (df < 6.5))), 18, 1e-4);
}

BOOST_FIXTURE_TEST_CASE(sumsquares_func, Fixture)
{
  BOOST_CHECK_CLOSE_FRACTION( sumsquares (df ((df > 2.5) && (df < 6.5))), 86, 1e-4);
}

BOOST_FIXTURE_TEST_CASE(product_func, Fixture)
{
  BOOST_CHECK_CLOSE_FRACTION( product (df ((df > 2.5) && (df < 6.5))), 360, 1e-4);
}

BOOST_FIXTURE_TEST_CASE(mean_func, Fixture)
{
  BOOST_CHECK_CLOSE_FRACTION( mean (df ((df > 2.5) && (df < 6.5))), 4.5, 1e-4);
}

BOOST_FIXTURE_TEST_CASE(variance_func, Fixture)
{
  BOOST_CHECK_CLOSE_FRACTION( variance (df ((df > 2.5) && (df < 6.5))), 1.66667, 1e-4);
}

BOOST_FIXTURE_TEST_CASE(pvariance_func, Fixture)
{
  BOOST_CHECK_CLOSE_FRACTION( pvariance (df ((df > 2.5) && (df < 6.5)), 1),1.66667, 1e-4);
}

BOOST_FIXTURE_TEST_CASE(stddev_func, Fixture)
{
  BOOST_CHECK_CLOSE_FRACTION( stddev (df ((df > 2.5) && (df < 6.5))), 1.29099, 1e-4);
}

BOOST_FIXTURE_TEST_CASE(pstddev_func, Fixture)
{
  BOOST_CHECK_CLOSE_FRACTION( pstddev (df ((df > 2.5) && (df < 6.5)), 1), 1.29099, 1e-4);
}

BOOST_FIXTURE_TEST_CASE(avdev_func, Fixture)
{
  BOOST_CHECK_CLOSE_FRACTION( avdev (df ((df > 2.5) && (df < 6.5))), 1, 1e-4);
}

BOOST_FIXTURE_TEST_CASE(rms_func, Fixture)
{
  BOOST_CHECK_CLOSE_FRACTION( rms (df ((df > 2.5) && (df < 6.5))), 4.63681, 1e-4);
}

BOOST_FIXTURE_TEST_CASE(median_even1_func, Fixture)
{
  BOOST_CHECK_CLOSE_FRACTION( median (df ((df > 2.5) && (df < 6.5)), true), 4.5, 1e-4);
}

BOOST_FIXTURE_TEST_CASE(median_even2_func, Fixture)
{
  Vector<double> dfunsort(10);
  dfunsort.assign_conforming( df );
  double tmp;
  tmp = dfunsort (9);
  dfunsort (9) = dfunsort (5);
  dfunsort (5) = tmp;
  double result (-1.0);
  result = median (dfunsort ((dfunsort > 2.5) &&
  (dfunsort < 6.5)));
  BOOST_CHECK_CLOSE_FRACTION( result, 4.5, 1e-4);
  Vector<double> ref{0, 1, 2, 3, 4, 9, 6, 7, 8, 5};
  BOOST_CHECK(allNear(dfunsort, ref, 1e-4));
}

BOOST_FIXTURE_TEST_CASE(median_odd1_func, Fixture)
{
  BOOST_CHECK_CLOSE_FRACTION( median (df ((df > 2.5) && (df < 7.5)), true), 5, 1e-4);
}

BOOST_FIXTURE_TEST_CASE(median_odd2_func, Fixture)
{
  Vector<double> dfunsort(10);
  dfunsort.assign_conforming( df );
  double tmp;
  tmp = dfunsort (9);
  dfunsort (9) = dfunsort (5);
  dfunsort (5) = tmp;
  double result (-1.0);
  result = median (dfunsort ((dfunsort > 2.5) &&
  (dfunsort < 7.5)));
  BOOST_CHECK_CLOSE_FRACTION( result, 5, 1e-4);
  Vector<double> ref{0, 1, 2, 3, 4, 9, 6, 7, 8, 5};
  BOOST_CHECK(allNear(dfunsort, ref, 1e-4));
}

BOOST_FIXTURE_TEST_CASE(square_func, Fixture)
{
  dh.assign_conforming(square (df ((df > 2.5) && (df < 6.5))) );
  Vector<double> ref{2, 2, 2, 9, 16, 25, 36, 2, 2, 2};
  BOOST_CHECK(allNear(dh, ref, 1e-4));
}

BOOST_FIXTURE_TEST_CASE(cube_func, Fixture)
{
  dh.assign_conforming(cube (df ((df > 2.5) && (df < 6.5))) );
  Vector<double> ref{2, 2, 2, 27, 64, 125, 216, 2, 2, 2};
  BOOST_CHECK(allNear(dh, ref, 1e-4));
}

BOOST_AUTO_TEST_CASE(complex_variance_func)
{
  // Test Complex variance.
  Vector<std::complex<float>> vecc(10);
  indgen (vecc, std::complex<float>(1.5,-3.3));
  Vector<bool> vecb(10, false);
  vecb[2] = vecb[5] = true;
  BOOST_CHECK (arrays_internal::near (pvariance(vecc(vecb)), (pvariance(real(vecc(vecb))) +
  pvariance(imag(vecc(vecb))))));
}

BOOST_AUTO_TEST_SUITE_END()
