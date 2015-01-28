//# tLELAttribute.cc: Test program for class LELAttribute
//# Copyright (C) 2003
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
//#
//# $Id$

#include <casacore/lattices/LEL/LELAttribute.h>
#include <casacore/casa/Utilities/Assert.h>

#include <casacore/casa/namespace.h>
void doIt()
{
  {
    // Test scalar
    LELAttribute attr1;
    AlwaysAssertExit (!attr1.isMasked());
    AlwaysAssertExit (attr1.isScalar());
    AlwaysAssertExit (attr1.isReduced());
    AlwaysAssertExit (!attr1.isRegion());
    AlwaysAssertExit (attr1.shape() == IPosition());
    AlwaysAssertExit (attr1.tileShape() == IPosition());
  }
  {
    // Test array
    LELAttribute attr1(True, IPosition(1,3), IPosition(1,4), LELCoordinates());
    AlwaysAssertExit (attr1.isMasked());
    AlwaysAssertExit (!attr1.isScalar());
    AlwaysAssertExit (!attr1.isReduced());
    AlwaysAssertExit (!attr1.isRegion());
    AlwaysAssertExit (attr1.shape() == IPosition(1,3));
    AlwaysAssertExit (attr1.tileShape() == IPosition(1,4));
  }
  {
    // Test region
    LELAttribute attr1(3);
    AlwaysAssertExit (!attr1.isMasked());
    AlwaysAssertExit (!attr1.isScalar());
    AlwaysAssertExit (!attr1.isReduced());
    AlwaysAssertExit (attr1.isRegion());
    AlwaysAssertExit (attr1.shape() == IPosition(3,0));
    AlwaysAssertExit (attr1.tileShape() == IPosition());
  }
  {
    // Combine scalar and array
    LELAttribute attr1;
    LELAttribute attr2(True, IPosition(1,3), IPosition(1,4), LELCoordinates());
    {
      LELAttribute attr3 (attr1, attr2);
      AlwaysAssertExit (attr3.isMasked());
      AlwaysAssertExit (!attr3.isScalar());
      AlwaysAssertExit (!attr3.isReduced());
      AlwaysAssertExit (!attr3.isRegion());
      AlwaysAssertExit (attr3.shape() == IPosition(1,3));
      AlwaysAssertExit (attr3.tileShape() == IPosition(1,4));
    }
    {
      LELAttribute attr3 (attr2, attr1);
      AlwaysAssertExit (attr3.isMasked());
      AlwaysAssertExit (!attr3.isScalar());
      AlwaysAssertExit (!attr3.isReduced());
      AlwaysAssertExit (!attr3.isRegion());
      AlwaysAssertExit (attr3.shape() == IPosition(1,3));
      AlwaysAssertExit (attr3.tileShape() == IPosition(1,4));
    }
  }
  {
    // Combine scalar and scalar
    LELAttribute attr1;
    LELAttribute attr2;
    {
      LELAttribute attr3 (attr1, attr2);
      AlwaysAssertExit (!attr3.isMasked());
      AlwaysAssertExit (attr3.isScalar());
      AlwaysAssertExit (attr3.isReduced());
      AlwaysAssertExit (!attr3.isRegion());
      AlwaysAssertExit (attr3.shape() == IPosition());
      AlwaysAssertExit (attr3.tileShape() == IPosition());
    }
    {
      LELAttribute attr3 (attr2, attr1);
      AlwaysAssertExit (!attr3.isMasked());
      AlwaysAssertExit (attr3.isScalar());
      AlwaysAssertExit (attr3.isReduced());
      AlwaysAssertExit (!attr3.isRegion());
      AlwaysAssertExit (attr3.shape() == IPosition());
      AlwaysAssertExit (attr3.tileShape() == IPosition());
    }
  }
  {
    // Combine arrays with different shapes
    LELAttribute attr1(False, IPosition(2,3,5), IPosition(2,4,6),
		       LELCoordinates(), True);
    LELAttribute attr2(True, IPosition(1,3), IPosition(1,4), LELCoordinates());
    {
      LELAttribute attr3 (attr1, attr2, False);
      AlwaysAssertExit (attr3.isMasked());
      AlwaysAssertExit (!attr3.isScalar());
      AlwaysAssertExit (attr3.isReduced());
      AlwaysAssertExit (!attr3.isRegion());
      AlwaysAssertExit (attr3.shape() == IPosition(2,3,5));
      AlwaysAssertExit (attr3.tileShape() == IPosition(2,4,6));
    }
    {
      LELAttribute attr3 (attr2, attr1, False);
      AlwaysAssertExit (attr3.isMasked());
      AlwaysAssertExit (!attr3.isScalar());
      AlwaysAssertExit (attr3.isReduced());
      AlwaysAssertExit (!attr3.isRegion());
      AlwaysAssertExit (attr3.shape() == IPosition(2,3,5));
      AlwaysAssertExit (attr3.tileShape() == IPosition(2,4,6));
    }
    Bool caught = False;
    try {
      LELAttribute attr3 (attr2, attr1);
    } catch (AipsError& x) {
      caught = True;              // mismatching axes
    }
    AlwaysAssertExit (caught);
  }
  {
    // Combine array with an array with unknown shape.
    LELAttribute attr1(False, IPosition(2,3,5), IPosition(2,4,6),
		       LELCoordinates(), True);
    LELAttribute attr2(True, IPosition(), IPosition(), LELCoordinates());
    {
      LELAttribute attr3 (attr1, attr2, True);
      AlwaysAssertExit (attr3.isMasked());
      AlwaysAssertExit (!attr3.isScalar());
      AlwaysAssertExit (attr3.isReduced());
      AlwaysAssertExit (!attr3.isRegion());
      AlwaysAssertExit (attr3.shape() == IPosition(2,3,5));
      AlwaysAssertExit (attr3.tileShape() == IPosition(2,4,6));
    }
    {
      LELAttribute attr3 (attr2, attr1, False);
      AlwaysAssertExit (attr3.isMasked());
      AlwaysAssertExit (!attr3.isScalar());
      AlwaysAssertExit (attr3.isReduced());
      AlwaysAssertExit (!attr3.isRegion());
      AlwaysAssertExit (attr3.shape() == IPosition(2,3,5));
      AlwaysAssertExit (attr3.tileShape() == IPosition(2,4,6));
    }
  }
}

int main()
{
  try {
    doIt();
  } catch (std::exception& x) {
    cout << "Caught exception: " << x.what() << endl;
    return 1;
  }
  cout << "OK" << endl;
  return 0;
}
