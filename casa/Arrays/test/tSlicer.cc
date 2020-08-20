//# tSlicer.cc: This program tests the Slicer class
//# Copyright (C) 1994,1995,1996,1999,2000,2001
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

//# Includes

#include "../Slicer.h"
#include "../Slice.h"
#include "../IPosition.h"
#include "../ArrayError.h"

#include <boost/test/unit_test.hpp>

#include "TestUtilities.h"

// <summary> Test the Slicer class </summary>

using namespace casacore;

BOOST_AUTO_TEST_SUITE(slicer)

struct Fixture
{
  // Define the shape of an array.
  // Also define an origin.
  IPosition shape;
  IPosition blc,trc,inc;
  Fixture() :
    shape (2,20,30)
  { }
};

BOOST_FIXTURE_TEST_CASE( define, Fixture )
{
  // The following (outcommented) constructor results in a compile
  // error, because this private constructor prevents an automatic
  // conversion of int to IPosition.
  //# Slicer xxx(0);

  // Now define some Slicer's and apply the shape.
  Slicer ns0 (IPosition(2,0,24));
  check(ns0.inferShapeFromSource (shape, blc, trc, inc), {1, 1});
  check(blc, {0, 24});
  check(trc, {0, 24});
  check(inc, {1, 1});
  
  Slicer ns1 (IPosition(2,3,5), IPosition(2,13,21),
  Slicer::endIsLast);
  check(ns1.inferShapeFromSource (shape,blc,trc,inc), {11, 17});
  check(blc, {3, 5});
  check(trc, {13, 21});
  check(inc, {1, 1});
  
  Slicer ns2 (IPosition(2,3,5), IPosition(2,13,21), IPosition(2,3,2),
  Slicer::endIsLast);
  check(ns2.inferShapeFromSource (shape, blc,trc,inc), {4, 9});
  check(blc, {3, 5});
  check(trc, {12, 21});
  check(inc, {3, 2});
  
  BOOST_CHECK_EQUAL(ns2.ndim(), 2);
  check(ns2.start(), {3, 5});
  check(ns2.end(), {13, 21});
  check(ns2.stride(), {3, 2});
  check(ns2.length(), {4, 9});
  BOOST_CHECK_EQUAL(to_string(ns2), "[3, 5] to [13, 21] with stride [3, 2], length [4, 9]");
}

BOOST_FIXTURE_TEST_CASE( from_output_length, Fixture )
{
  IPosition blc,trc,inc;
  // Define some Slicer's via an output length.
  Slicer ns10 (IPosition(2,0,24));
  check(ns10.inferShapeFromSource (shape, blc, trc, inc), {1, 1});
  check(blc, {0, 24});
  check(trc, {0, 24});
  check(inc, {1, 1});
  
  Slicer ns11 (IPosition(2,3,5), IPosition(2,13,21));
  check(ns11.inferShapeFromSource (shape, blc,trc,inc), {13, 21});
  check(blc, {3, 5});
  check(trc, {15, 25});
  check(inc, {1, 1});
  
  Slicer ns12 (IPosition(2,3,5), IPosition(2,4,11), IPosition(2,3,2));
  check(ns12.inferShapeFromSource (shape, blc,trc,inc), {4, 11});
  check(blc, {3, 5});
  check(trc, {12, 25});
  check(inc, {3, 2});
  
  BOOST_CHECK_EQUAL(ns12.ndim(), 2);
  check(ns12.start(), {3, 5});
  check(ns12.end(), {12, 25});
  check(ns12.stride(), {3, 2});
  check(ns12.length(), {4, 11});
}

BOOST_FIXTURE_TEST_CASE( undetermined_blc_trc, Fixture )
{
  // Define some Slicer's with an undetermined blc and/or trc.
  Slicer ns20 (IPosition(2,Slicer::MimicSource,24));
  check(ns20.inferShapeFromSource (shape, blc,trc,inc), {1, 1});
  check(blc, {0, 24});
  check(trc, {0, 24});
  check(inc, {1, 1});
  
  Slicer ns21 (IPosition(2,3,5), IPosition(2,13,Slicer::MimicSource),
    Slicer::endIsLast);
  check(ns21.inferShapeFromSource (shape, blc,trc,inc), {11, 25});
  check(blc, {3, 5});
  check(trc, {13, 29});
  check(inc, {1, 1});
  
  Slicer ns22 (IPosition(2,Slicer::MimicSource,5), IPosition(2,13,21),
    IPosition(2,3,2), Slicer::endIsLast);
  check(ns22.inferShapeFromSource (shape, blc,trc,inc), {5, 9});
  check(blc, {0, 5});
  check(trc, {12, 21});
  check(inc, {3, 2});
  
  BOOST_CHECK_EQUAL(ns22.ndim(), 2);
  check(ns22.start(), {-2147483646, 5});
  check(ns22.end(), {13, 21});
  check(ns22.stride(), {3, 2});
  check(ns22.length(), {-2147483646, 9});
}

BOOST_FIXTURE_TEST_CASE( undetermined_blc_length, Fixture )
{
  // Define some Slicer's with an undetermined blc and/or length.
  Slicer ns30 (IPosition(2,Slicer::MimicSource,24));
  check(ns30.inferShapeFromSource (shape, blc,trc,inc), {1, 1});
  check(blc, {0, 24});
  check(trc, {0, 24});
  check(inc, {1, 1});
  
  Slicer ns31 (IPosition(2,3,5), IPosition(2,13,Slicer::MimicSource));
  check(ns31.inferShapeFromSource (shape, blc,trc,inc), {13, 25});
  check(blc, {3, 5});
  check(trc, {15, 29});
  check(inc, {1, 1});
  
  Slicer ns32 (IPosition(2,Slicer::MimicSource,5), IPosition(2,5,11),
    IPosition(2,3,2));
  check(ns32.inferShapeFromSource (shape, blc,trc,inc), {5, 11});
  check(blc, {0, 5});
  check(trc, {12, 25});
  check(inc, {3, 2});
  
  BOOST_CHECK_EQUAL(ns32.ndim(), 2);
  check(ns32.start(), {-2147483646, 5});
  check(ns32.end(), {-2147483634, 25});
  check(ns32.stride(), {3, 2});
  check(ns32.length(), {5, 11});

  Slicer ns40 (Slice(0), Slice(24));
  check(ns40.inferShapeFromSource (shape, blc,trc,inc), {1, 1});
  check(blc, {0, 24});
  check(trc, {0, 24});
  check(inc, {1, 1});
  
  Slicer ns41 (Slice(3,13), Slice(5,21));
  check(ns41.inferShapeFromSource (shape, blc,trc,inc), {13, 21});
  check(blc, {3, 5});
  check(trc, {15, 25});
  check(inc, {1, 1});
  
  Slicer ns42 (Slice(3,5,3), Slice(5,11,2));
  check(ns42.inferShapeFromSource (shape, blc,trc,inc), {5, 11});
  check(blc, {3, 5});
  check(trc, {15, 25});
  check(inc, {3, 2});
  
  BOOST_CHECK_EQUAL(ns42.ndim(), 2);
  check(ns42.start(), {3, 5});
  check(ns42.end(), {15, 25});
  check(ns42.stride(), {3, 2});
  check(ns42.length(), {5, 11});
  
  Slice tmp1, tmp2(5,11,2);
  Slicer ns43(tmp1, tmp2);
  check(ns43.inferShapeFromSource (shape, blc,trc,inc), {20, 11});
  check(blc, {0, 5});
  check(trc, {19, 25});
  check(inc, {1, 2});
  
  BOOST_CHECK_EQUAL(ns43.ndim(), 2);
  check(ns43.start(), {-2147483646, 5});
  check(ns43.end(), {-2147483646, 25});
  check(ns43.stride(), {1, 2});
  check(ns43.length(), {-2147483646, 11});
}

BOOST_FIXTURE_TEST_CASE( zero_length, Fixture )
{
  // Try length 0.
  Slicer ns50 (Slice(0,5,2), Slice(24,0,3));
  check(ns50.inferShapeFromSource (shape, blc,trc,inc), {5, 0});
  check(blc, {0, 24});
  check(trc, {8, 23});
  check(inc, {2, 3});
  
  BOOST_CHECK_EQUAL(ns50.ndim(), 2);
  check(ns50.start(), {0, 24});
  check(ns50.end(), {8, 21});
  check(ns50.stride(), {2, 3});
  check(ns50.length(), {5, 0});
}

BOOST_FIXTURE_TEST_CASE( negative_start_or_end, Fixture )
{
  // Define some Slicers with a negative start or end.
  Slicer ns60 (IPosition(2,-13,10), IPosition(2,15,-3), Slicer::endIsLast);
  check(ns60.inferShapeFromSource (shape, blc,trc,inc), {9, 18});
  check(blc, {7, 10});
  check(trc, {15, 27});
  check(inc, {1, 1});
}

BOOST_FIXTURE_TEST_CASE( copy_constructor, Fixture )
{
  // Try copy constructor.
  Slicer ns2 (IPosition(2,3,5), IPosition(2,13,21), IPosition(2,3,2),
  Slicer::endIsLast);
  ns2.inferShapeFromSource (shape, blc,trc,inc);
  Slicer ns3(ns2);
  BOOST_CHECK_EQUAL(ns3.ndim(), 2);
  check(ns3.start(), {3, 5});
  check(ns3.end(), {13, 21});
  check(ns3.stride(), {3, 2});
  check(ns3.length(), {4, 9});
  
  // Try assignment (the difference in ndim should work fine).
  Slicer ns4(IPosition(1,0));
  BOOST_CHECK_EQUAL(ns4.ndim(), 1);
  ns4 = ns3;
  BOOST_CHECK_EQUAL(ns4.ndim(), 2);
  check(ns4.start(), {3, 5});
  check(ns4.end(), {13, 21});
  check(ns4.stride(), {3, 2});
  check(ns4.length(), {4, 9});
}

BOOST_FIXTURE_TEST_CASE( equality, Fixture )
{
  Slicer ns2 (IPosition(2,3,5), IPosition(2,13,21), IPosition(2,3,2),
  Slicer::endIsLast);
  ns2.inferShapeFromSource (shape, blc,trc,inc);
  Slicer ns3(ns2);
  Slicer ns50 (Slice(0,5,2), Slice(24,0,3));
  check(ns50.inferShapeFromSource (shape, blc,trc,inc), {5, 0});
  // Try equality
  BOOST_CHECK(ns2==ns3);
  BOOST_CHECK(!(ns2==ns50));
}

BOOST_AUTO_TEST_CASE( errors )
{
  // Do some erroneous constructions.
   // different lengths
  BOOST_CHECK_THROW(Slicer(IPosition(2,0,0), IPosition(3,0,0,0)), ArrayError);
  BOOST_CHECK_THROW(Slicer(IPosition(2,0,0), IPosition(3,0,0,0), Slicer::endIsLast), ArrayError);
  // trc < blc
  BOOST_CHECK_THROW(Slicer(IPosition(2,0,1), IPosition(2,0,0), Slicer::endIsLast), ArrayError);
  // length < 0
  BOOST_CHECK_THROW(Slicer(IPosition(2,0,1), IPosition(2,0,-1), Slicer::endIsLength), ArrayError);
  // inc <= 0
  BOOST_CHECK_THROW(Slicer(IPosition(2,0,1), IPosition(2,1,1), IPosition(2,-1,0)), ArrayError);
  BOOST_CHECK_THROW(Slicer(IPosition(2,0,1), IPosition(2,1,1), IPosition(2,0,0)), ArrayError);
}

BOOST_FIXTURE_TEST_CASE( change_length, Fixture )
{
  // Check if changing length of trc,blc,inc works fine.
  Slicer ns90;
  check(ns90.inferShapeFromSource (IPosition(1,10), blc, trc, inc), {10});
  check(blc, {0});
  check(trc, {9});
  check(inc, {1});
  BOOST_CHECK_EQUAL(ns90.ndim(), 1);
  check(ns90.start(), {-2147483646});
  check(ns90.end(), {-2147483646});
  check(ns90.stride(), {1});
  check(ns90.length(), {-2147483646});
  
  // shape length invalid
  BOOST_CHECK_THROW(ns90.inferShapeFromSource (shape, blc, trc, inc), ArrayError);
}

BOOST_AUTO_TEST_SUITE_END()
