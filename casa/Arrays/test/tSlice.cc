//# tSlice.cc: Test program for class Slice.
//# Copyright (C) 2008
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This library is free software; you can redistribute it and/or modify it
//# under the terms of the GNU Library General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or (at your
//# option) any later version.
//#
//# This library is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
//# License for more details.
//#
//# You should have received a copy of the GNU Library General Public License
//# along with this library; if not, write to the Free Software Foundation,
//# Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#include "../Slice.h"
#include "../Slicer.h"
#include "../Vector.h"

#include <boost/test/unit_test.hpp>

using namespace casacore;

BOOST_AUTO_TEST_SUITE(slice)

BOOST_AUTO_TEST_CASE( check_slices_uninitialized )
{
  Slicer first;
  IPosition shape(3,100,110,120);
  Vector<Vector<Slice> > slices;
  IPosition shp = Slice::checkSlices (slices, first, shape);
  BOOST_CHECK_EQUAL (slices.size(), 3);
  BOOST_CHECK_EQUAL (shp, shape);
  BOOST_CHECK_EQUAL (first.start(), IPosition(3,0));
  BOOST_CHECK_EQUAL (first.length(), shape);
  BOOST_CHECK_EQUAL (first.stride(), IPosition(3,1));
}

BOOST_AUTO_TEST_CASE( check_slices_sized )
{
  Slicer first;
  IPosition shape(3,100,110,120);
  Vector<Vector<Slice> > slices(3);
  IPosition shp = Slice::checkSlices (slices, first, shape);
  BOOST_CHECK_EQUAL (slices.size(), 3);
  BOOST_CHECK_EQUAL (shp, shape);
  BOOST_CHECK_EQUAL (first.start(), IPosition(3,0));
  BOOST_CHECK_EQUAL (first.length(), shape);
  BOOST_CHECK_EQUAL (first.stride(), IPosition(3,1));
}

BOOST_AUTO_TEST_CASE( check_slices_initialized )
{
  Slicer first;
  IPosition shape(3,100,110,120);
  Vector<Vector<Slice> > slices(2);
  slices[0].resize(2);
  slices[0][0] = Slice(20,10);
  slices[0][1] = Slice(25,20);
  slices[1].resize(3);
  slices[1][0] = Slice(22,12,2);
  slices[1][1] = Slice(20,10);
  slices[1][2] = Slice(34,10,2);
  IPosition shp = Slice::checkSlices (slices, first, shape);
  BOOST_CHECK_EQUAL (slices.size(), 3);
  BOOST_CHECK_EQUAL (shp, IPosition(3,30,32,shape[2]));
  BOOST_CHECK_EQUAL (first.start(), IPosition(3,20,22,0));
  BOOST_CHECK_EQUAL (first.length(), IPosition(3,10,12,shape[2]));
  BOOST_CHECK_EQUAL (first.stride(), IPosition(3,1,2,1));
}

BOOST_AUTO_TEST_CASE( constructor )
{
  Slice slice(3,10,5);
  BOOST_CHECK_EQUAL (slice.start(), 3);
  BOOST_CHECK_EQUAL (slice.length(), 10);
  BOOST_CHECK_EQUAL (slice.end(), 48);
  BOOST_CHECK_EQUAL (slice.inc(), 5);
}

BOOST_AUTO_TEST_CASE( constructor_endislength1 )
{
  Slice slice(3,48,5, false);
  BOOST_CHECK_EQUAL (slice.start(), 3);
  BOOST_CHECK_EQUAL (slice.length(), 10);
  BOOST_CHECK_EQUAL (slice.end(), 48);
  BOOST_CHECK_EQUAL (slice.inc(), 5);
}

BOOST_AUTO_TEST_CASE( constructor_endislength2 )
{
  Slice slice(2,10,5, false);
  BOOST_CHECK_EQUAL (slice.start(), 2);
  BOOST_CHECK_EQUAL (slice.length(), 2);
  BOOST_CHECK_EQUAL (slice.end(), 7);
  BOOST_CHECK_EQUAL (slice.inc(), 5);
}

BOOST_AUTO_TEST_SUITE_END()
