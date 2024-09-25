//# tExtendSpecifier.cc: Test program for class ExtendSpecifier
//# Copyright (C) 2001
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

#include "../ExtendSpecifier.h"
#include "../Slicer.h"

#include <boost/test/unit_test.hpp>

#include <stdexcept>

using namespace casacore;

BOOST_AUTO_TEST_SUITE(extend_specifier)

BOOST_AUTO_TEST_CASE( construct1 )
{
  IPosition oldShape(4,10,1,3,1);
  IPosition newShape(5,10,1,5,3,8);
  ExtendSpecifier spec (oldShape, newShape, IPosition(1,2), IPosition(1,4));

  BOOST_CHECK_EQUAL( spec.oldShape(), IPosition(4, 10, 1, 3, 1) );
  BOOST_CHECK_EQUAL( spec.newShape(), IPosition(5, 10, 1, 5, 3, 8) );
  BOOST_CHECK_EQUAL( spec.newAxes(), IPosition(1, 2) );
  BOOST_CHECK_EQUAL( spec.stretchAxes(), IPosition(1, 4) );
  BOOST_CHECK_EQUAL( spec.extendAxes(), IPosition(2, 2, 4) );
  BOOST_CHECK_EQUAL( spec.oldOldAxes(), IPosition(3, 0, 1, 2) );
  BOOST_CHECK_EQUAL( spec.oldNewAxes(), IPosition(3, 0, 1, 3) );

  IPosition sh(4,3,4,5,6);
  IPosition newsh = spec.convertNew(sh);
  BOOST_CHECK_EQUAL( newsh, IPosition(5, 3, 4, 1, 5, 1) );
  
  Slicer sl(IPosition(5,0,1,2,3,4), IPosition(5,1,2,3,4,5) );
  BOOST_CHECK_EQUAL( sl.start(), IPosition(5, 0, 1, 2, 3, 4) );
  BOOST_CHECK_EQUAL( sl.length(), IPosition(5, 1, 2, 3, 4, 5) );
  BOOST_CHECK_EQUAL( sl.stride(), IPosition(5, 1, 1, 1, 1, 1) );

  IPosition shp;
  Slicer sln = spec.convert (shp, sl);
  BOOST_CHECK_EQUAL( sln.start(), IPosition(4, 0, 1, 3, 0));
  BOOST_CHECK_EQUAL( sln.length(), IPosition(4, 1, 2, 4, 1));
  BOOST_CHECK_EQUAL( sln.stride(), IPosition(4, 1, 1, 1, 1));
  BOOST_CHECK_EQUAL( shp, IPosition(5, 1, 2, 1, 4, 1));
}

BOOST_AUTO_TEST_CASE( construct2 )
{
  IPosition oldShape(4,10,1,1,3);
  IPosition newShape(5,10,1,5,3,8);
  ExtendSpecifier spec (oldShape, newShape, IPosition(1,4), IPosition(1,2));
  
  BOOST_CHECK_EQUAL( spec.oldShape(), IPosition(4, 10, 1, 1, 3) );
  BOOST_CHECK_EQUAL( spec.newShape(), IPosition(5, 10, 1, 5, 3, 8) );
  BOOST_CHECK_EQUAL( spec.newAxes(), IPosition(1, 4) );
  BOOST_CHECK_EQUAL( spec.stretchAxes(), IPosition(1, 2) );
  BOOST_CHECK_EQUAL( spec.extendAxes(), IPosition(2, 2, 4) );
  BOOST_CHECK_EQUAL( spec.oldOldAxes(), IPosition(3, 0, 1, 3) );
  BOOST_CHECK_EQUAL( spec.oldNewAxes(), IPosition(3, 0, 1, 3) );

  IPosition sh(4,3,4,5,6);
  IPosition newsh = spec.convertNew(sh);
  BOOST_CHECK_EQUAL( newsh, IPosition(5, 3, 4, 1, 6, 1) );

  Slicer sl(IPosition(5,0,1,2,3,4), IPosition(5,1,2,3,4,5));
  BOOST_CHECK_EQUAL( sl.start(), IPosition(5, 0, 1, 2, 3, 4) );
  BOOST_CHECK_EQUAL( sl.length(), IPosition(5, 1, 2, 3, 4, 5) );
  BOOST_CHECK_EQUAL( sl.stride(), IPosition(5, 1, 1, 1, 1, 1) );
  
  IPosition shp;
  Slicer sln = spec.convert (shp, sl);
  BOOST_CHECK_EQUAL( sln.start(), IPosition(4, 0, 1, 0, 3));
  BOOST_CHECK_EQUAL( sln.length(), IPosition(4, 1, 2, 1, 4));
  BOOST_CHECK_EQUAL( sln.stride(), IPosition(4, 1, 1, 1, 1));
  BOOST_CHECK_EQUAL( shp, IPosition(5, 1, 2, 1, 4, 1));
}

BOOST_AUTO_TEST_CASE(erroneous_constructions)
{
  BOOST_CHECK_THROW(
    ExtendSpecifier(IPosition(1,1), IPosition(1,1), IPosition(), IPosition()),
    std::runtime_error
  );
  BOOST_CHECK_THROW(
    ExtendSpecifier(IPosition(1,1), IPosition(1,1),
			  IPosition(), IPosition(1,0)),
    std::runtime_error
  );
  BOOST_CHECK_THROW(
    ExtendSpecifier(IPosition(2,2,1), IPosition(2,1,1),
			  IPosition(), IPosition(1,0)),
    std::runtime_error
  );
  BOOST_CHECK_THROW(
    ExtendSpecifier(IPosition(2,1,2), IPosition(2,2,3),
			  IPosition(), IPosition(1,0)),
    std::runtime_error
  );
  BOOST_CHECK_THROW(
    ExtendSpecifier(IPosition(2,1,2), IPosition(2,2,2),
			  IPosition(), IPosition(2,0,0)),
    std::runtime_error
  );
  BOOST_CHECK_THROW(
    ExtendSpecifier spec (IPosition(2,1,2), IPosition(2,2,2),
			  IPosition(1,0), IPosition(1,0)),
    std::runtime_error
  );
  BOOST_CHECK_THROW(
    ExtendSpecifier spec (IPosition(2,1,2), IPosition(2,2,2),
			  IPosition(1,0), IPosition()),
    std::runtime_error
  );
  BOOST_CHECK_THROW(
    ExtendSpecifier spec (IPosition(2,1,2), IPosition(2,2,2),
			  IPosition(), IPosition(1,-1)),
    std::runtime_error
  );
  BOOST_CHECK_THROW(
    ExtendSpecifier spec (IPosition(2,1,2), IPosition(2,2,2),
			  IPosition(), IPosition(1,2)),
    std::runtime_error
  );
}

BOOST_AUTO_TEST_SUITE_END()
