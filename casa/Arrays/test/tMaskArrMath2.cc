//# tMaskArrMath2.cc: Test program for MaskedArrays mathematical operations.
//# Copyright (C) 1993,1994,1995,1996,1997,1999,2000,2001
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
#include "../MaskArrMath.h"

#include <boost/test/unit_test.hpp>

#include "TestUtilities.h"

using namespace casacore;

BOOST_AUTO_TEST_SUITE(masked_array_math2)

struct Fixture {
  Vector<double> df, dh;
  Vector<bool> b;
  Vector<double> dk;
  
  Fixture() :
    df(10), dh(10),
    b(10),
    dk({0., 1., 5., 3., 9., 2., 7., 7., 4., 3.})
  { 
    indgen (df);
  }
};

BOOST_FIXTURE_TEST_CASE(fixture, Fixture)
{
  check(dk, {0., 1., 5., 3., 9., 2., 7., 7., 4., 3.});
}

BOOST_FIXTURE_TEST_CASE(min_ma, Fixture)
{
  BOOST_CHECK_CLOSE_FRACTION( min (dk ((dk > 2.5) && (dk < 7.5))),
                              3, 1e-6);
}

BOOST_FIXTURE_TEST_CASE(max_ma, Fixture)
{
  BOOST_CHECK_CLOSE_FRACTION( max (dk ((dk > 2.5) && (dk < 7.5))),
                              7, 1e-6);
}

BOOST_FIXTURE_TEST_CASE(minmax_ma1, Fixture)
{
  double minVal;
  double maxVal;
  
  IPosition minPos (1);
  IPosition maxPos (1);
  
  minMax (minVal, maxVal, minPos, maxPos,
          dk((dk > 2.5) && (dk < 7.5)));
  BOOST_CHECK_CLOSE_FRACTION(minVal, 3, 1e-6);
  BOOST_CHECK_CLOSE_FRACTION(maxVal, 7, 1e-6);
  BOOST_CHECK_EQUAL(minPos, IPosition(1, 3));
  BOOST_CHECK_EQUAL(maxPos, IPosition(1, 6));
}

BOOST_FIXTURE_TEST_CASE(minmax_ma2, Fixture)
{
  double minVal;
  double maxVal;
  
  minMax (minVal, maxVal,
          dk((dk > 2.5) && (dk < 7.5)));
  
  BOOST_CHECK_CLOSE_FRACTION(minVal, 3, 1e-6);
  BOOST_CHECK_CLOSE_FRACTION(maxVal, 7, 1e-6);
}

BOOST_FIXTURE_TEST_CASE(min_ma_3pars, Fixture)
{
  // min (MaskedArray, Array, Array)
  dh = 2.0;
  min (dh((dk > 2.5) && (dk < 7.5)), dk, df);

  // Input:   0., 1., 5., 3., 9., 2., 7., 7., 4., 3.
  check( dh, {2., 2., 2., 3., 2., 2., 6., 7., 4., 3.});
  // Output:  2., 2., 0., 0., 2., 2., 0., 0., 0., 0.
}

BOOST_FIXTURE_TEST_CASE(max_ma_3pars, Fixture)
{
  dh = 2.0;
  max (dh((dk > 2.5) && (dk < 7.5)), dk, df);
  
  check( dh, {2., 2., 5., 3., 2., 2., 7., 7., 8., 9.});
}

BOOST_FIXTURE_TEST_CASE(min_ma_2pars_a, Fixture)
{
  dh = 2.0;
  dh.assign_conforming( min (dk((dk > 2.5) && (dk < 7.5)), df) );
  check( dh, {2., 2., 2., 3., 2., 2., 6., 7., 4., 3.});
}

BOOST_FIXTURE_TEST_CASE(max_ma_2pars, Fixture)
{
  dh = 2.0;
  dh.assign_conforming( max (dk((dk > 2.5) && (dk < 7.5)), df) );
  check( dh, {2., 2., 5., 3., 2., 2., 7., 7., 8., 9.});
}

BOOST_FIXTURE_TEST_CASE(min_ma_2pars_b, Fixture)
{
  dh = 2.0;
  // min(Array<double>, MaskedArray<double>)
  dh.assign_conforming( min (dk((dk > 2.5) && (dk < 7.5)), df) );
  check( dh, {2., 2., 2., 3., 2., 2., 6., 7., 4., 3.});
}

BOOST_FIXTURE_TEST_CASE(max_ma_2pars_b, Fixture)
{
  dh = 2.0;
  dh.assign_conforming( max (df, dk((dk > 2.5) && (dk < 7.5))) );
  check( dh, {2., 2., 5., 3., 2., 2., 7., 7., 8., 9.});
}

BOOST_FIXTURE_TEST_CASE(min_ma_2pars_c, Fixture)
{
  // min(MaskedArray<double>, MaskedArray<double>)
  dh = 2.0;
  dh.assign_conforming(min (dk((dk > 2.5) && (dk < 7.5)),
            df((df > 2.5) && (df < 7.5))) );
  check( dh, {2., 2., 2., 3., 2., 2., 6., 7., 2., 2.});
}

BOOST_FIXTURE_TEST_CASE(max_ma_2pars_c, Fixture)
{
  dh = 2.0;
  dh.assign_conforming(max (dk((dk > 2.5) && (dk < 7.5)),
            df((df > 2.5) && (df < 7.5))) );
  check( dh, {2., 2., 2., 3., 2., 2., 7., 7., 2., 2.});
}

BOOST_FIXTURE_TEST_CASE(min_ma_2pars_d, Fixture)
{
  // min (MaskedArray<double>, double)
  dh = 2.0;
  dh.assign_conforming( min (dk((dk > 2.5) && (dk < 7.5)), 5.0) );
  check( dh, {2., 2., 5., 3., 2., 2., 5., 5., 4., 3.});
}

BOOST_FIXTURE_TEST_CASE(max_ma_2pars_d, Fixture)
{
  dh = 2.0;
  dh.assign_conforming( max (dk((dk > 2.5) && (dk < 7.5)), 5.0) );
  check( dh, {2., 2., 5., 5., 2., 2., 7., 7., 5., 5.});
}

BOOST_FIXTURE_TEST_CASE(min_ma_2pars_e, Fixture)
{
  dh = 2.0;
  dh.assign_conforming( min (5.0, dk((dk > 2.5) && (dk < 7.5))) );
  check( dh, {2., 2., 5., 3., 2., 2., 5., 5., 4., 3.});
}

BOOST_FIXTURE_TEST_CASE(max_ma_2pars_e, Fixture)
{
  dh = 2.0;
  dh.assign_conforming( max (5.0, dk((dk > 2.5) && (dk < 7.5))) );
  check( dh, {2., 2., 5., 5., 2., 2., 7., 7., 5., 5.});
}

BOOST_AUTO_TEST_SUITE_END()
