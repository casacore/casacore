//# tMath.cc:
//# Copyright (C) 1999,2000
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

#include <aips/aips.h>
#include <aips/Exceptions/Error.h>
#include <aips/Exceptions/Excp.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/String.h>
#include <iostream.h>

#include <aips/Mathematics/Math.h>

int main() {
  try {
    {
      Float x;
      setNaN(x);
      AlwaysAssert(isNaN(x), AipsError);
    }
    {
      Double x = floatNaN();
      AlwaysAssert(isNaN(x), AipsError);
    }
    {
      Float x = doubleNaN();
      AlwaysAssert(isNaN(x), AipsError);
    }
    {
      Double x;
      setNaN(x);
      AlwaysAssert(isNaN(x), AipsError);
    }
    {
      Float x;
      setInf(x);
      AlwaysAssert(isInf(x), AipsError);
    }
    {
      Double x = floatInf();
      AlwaysAssert(isInf(x), AipsError);
    }
    {
      Float x = doubleInf();
      AlwaysAssert(isInf(x), AipsError);
    }
    {
      Double x;
      setInf(x);
      AlwaysAssert(isInf(x), AipsError);
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
