//# tMath.cc:
//# Copyright (C) 1999,2000,2001
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

#include <casacore/casa/aips.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/BasicMath/Math.h>

#include <casacore/casa/namespace.h>
int main() {
  try {
    {
      Float x;
      setNaN(x);
      AlwaysAssert(isNaN(x), AipsError);
      AlwaysAssert(!isFinite(x), AipsError);
    }
    {
      Double x = floatNaN();
      AlwaysAssert(isNaN(x), AipsError);
      AlwaysAssert(!isFinite(x), AipsError);
    }
    {
      Float x = doubleNaN();
      AlwaysAssert(isNaN(x), AipsError);
      AlwaysAssert(!isFinite(x), AipsError);
    }
    {
      Double x;
      setNaN(x);
      AlwaysAssert(isNaN(x), AipsError);
      AlwaysAssert(!isFinite(x), AipsError);
    }
    {
      Float x;
      setInf(x);
      AlwaysAssert(isInf(x), AipsError);
      AlwaysAssert(!isFinite(x), AipsError);
    }
    {
      Double x = floatInf();
      AlwaysAssert(isInf(x), AipsError);
      AlwaysAssert(!isFinite(x), AipsError);
    }
    {
      Float x = doubleInf();
      AlwaysAssert(isInf(x), AipsError);
      AlwaysAssert(!isFinite(x), AipsError);
    }
    {
      Double x;
      setInf(x);
      AlwaysAssert(isInf(x), AipsError);
      AlwaysAssert(!isFinite(x), AipsError);
    }
    {
      Double x = 321.544;
      AlwaysAssert(roundDouble(x) == 320, AipsError);
      AlwaysAssert(roundDouble(x,3) == 322, AipsError);
      x = 21.45554;
      AlwaysAssert(roundDouble(x,2) == 21, AipsError);
      AlwaysAssert(roundDouble(x) == 21.5, AipsError);
      x = -11.324;
      AlwaysAssert(roundDouble(x,2) == -11, AipsError);
      x = -4502034;
      AlwaysAssert(roundDouble(x) == -4500000, AipsError);
      x = -0.012345;
      AlwaysAssert(nearAbs(roundDouble(x,4), -0.01235, 1e-8), AipsError);
      x = 0;
      AlwaysAssert(roundDouble(x) == 0, AipsError);
    }
  }
  catch (AipsError x) {
    cerr << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  } 
  cout << "OK" << endl;
  return 0;
}
// Local Variables: 
// compile-command: "gmake OPTLIB=1 tMath"
// End: 
