//# tAxesSpecifier.cc: Test program for class AxesSpecifier
//# Copyright (C) 2000,2001
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#include "../AxesSpecifier.h"
#include "../Slicer.h"

#include <boost/test/unit_test.hpp>

using namespace casacore;

BOOST_AUTO_TEST_SUITE(axis_specifier)

BOOST_AUTO_TEST_CASE( tests )
{
  AxesMapping map;
  {
    AxesSpecifier as;
    map = as.apply (IPosition(3,1,10,1));
    BOOST_CHECK (map.getToNew() == IPosition(3,0,1,2));
    BOOST_CHECK (map.getToOld() == IPosition(3,0,1,2));
    BOOST_CHECK (!map.isRemoved());
    BOOST_CHECK (!map.isReordered());
  }
  {
    AxesSpecifier as(true);
    map = as.apply (IPosition(3,1,10,1));
    BOOST_CHECK (map.getToNew() == IPosition(3,0,1,2));
    BOOST_CHECK (map.getToOld() == IPosition(3,0,1,2));
    BOOST_CHECK (!map.isRemoved());
    BOOST_CHECK (!map.isReordered());
  }
  {
    AxesSpecifier as(false);
    map = as.apply (IPosition(3,1,10,1));
    BOOST_CHECK (map.getToNew() == IPosition(3,-1,0,-1));
    BOOST_CHECK (map.getToOld() == IPosition(1,1));
    BOOST_CHECK (map.isRemoved());
    BOOST_CHECK (!map.isReordered());
  }
  {
    AxesSpecifier as(IPosition(1,0));
    map = as.apply (IPosition(3,1,10,1));
    BOOST_CHECK (map.getToNew() == IPosition(3,0,1,-1));
    BOOST_CHECK (map.getToOld() == IPosition(2,0,1));
    BOOST_CHECK (map.isRemoved());
    BOOST_CHECK (!map.isReordered());
  }
  {
    AxesSpecifier as(IPosition(4,1,2,1,2));
    map = as.apply (IPosition(3,1,10,1));
    BOOST_CHECK (map.getToNew() == IPosition(3,-1,0,1));
    BOOST_CHECK (map.getToOld() == IPosition(2,1,2));
    BOOST_CHECK (map.isRemoved());
    BOOST_CHECK (!map.isReordered());
  }
  {
    AxesSpecifier as(IPosition(4,1,0,1,2), IPosition(2,2,0));
    AxesSpecifier as1(IPosition(2,0,2));
    as = as1;
    map = as.apply (IPosition(3,1,10,1));
    BOOST_CHECK (map.getToNew() == IPosition(3,0,1,2));
    BOOST_CHECK (map.getToOld() == IPosition(3,0,1,2));
    BOOST_CHECK (!map.isRemoved());
    BOOST_CHECK (!map.isReordered());
  }
  {
    AxesSpecifier as1(IPosition(4,1,0,1,2), IPosition(2,2,0));
    AxesSpecifier as(as1);
    map = as.apply (IPosition(3,1,10,1));
    BOOST_CHECK (map.getToNew() == IPosition(3,1,2,0));
    BOOST_CHECK (map.getToOld() == IPosition(3,2,0,1));
    BOOST_CHECK (!map.isRemoved());
    BOOST_CHECK (map.isReordered());
  }
  {
    AxesSpecifier as(IPosition(4,1,2,1,2), IPosition(2,1,0));
    map = as.apply (IPosition(3,1,10,1));
    AxesMapping map1(map);
    BOOST_CHECK (map1.getToNew() == IPosition(3,-1,1,0));
    BOOST_CHECK (map1.getToOld() == IPosition(2,2,1));
    BOOST_CHECK (map1.isRemoved());
    BOOST_CHECK (map1.isReordered());
    BOOST_CHECK (map1.shapeToNew(IPosition(3,1,3,4)) == IPosition(2,4,3));
    BOOST_CHECK (map1.posToNew(IPosition(3,0,3,4)) == IPosition(2,4,3));
    BOOST_CHECK (map1.shapeToOld(IPosition(2,4,3)) == IPosition(3,1,3,4));
    BOOST_CHECK (map1.posToOld(IPosition(2,4,3)) == IPosition(3,0,3,4));
    Slicer slin(IPosition(3,0,1,2), IPosition(3,1,3,2));
    Slicer slout = map1.slicerToNew (slin);
    BOOST_CHECK (slout.start() == IPosition(2,2,1));
    BOOST_CHECK (slout.length() == IPosition(2,2,3));
    BOOST_CHECK (slout.stride() == IPosition(2,1,1));
    Slicer slout2 = map1.slicerToOld (slout);
    BOOST_CHECK (slout2.start() == slin.start());
    BOOST_CHECK (slout2.length() == slin.length());
    BOOST_CHECK (slout2.stride() == slin.stride());
  }
}

BOOST_AUTO_TEST_SUITE_END()
