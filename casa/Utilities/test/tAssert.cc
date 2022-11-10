//# tAssert.cc: This program tests the ASSERT macros
//# Copyright (C) 2022
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

//# Includes

#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/namespace.h>

int main()
{
#ifdef AIPS_DEBUG
  cout << "AIPS_DEBUG is set" << endl;
  DebugAssert (1==1, AipsError);
  DebugAssertExit (1==1);
  try {
    DebugAssert (1==0, AipsError);
    AlwaysAssertExit (1==0);
  } catch (const AipsError& x) {
    cout << "  " << x.what() << endl;
  }
#else  
  cout << "AIPS_DEBUG is not set" << endl;
  DebugAssert (1==0, AipsError);   // should be a no-op
#endif

  AlwaysAssert (1==1, AipsError);
  AlwaysAssertExit (1==1);
  try {
    AlwaysAssert (1==2, AipsError);
    return 1;
  } catch (const AipsError& x) {
    cout << "  " << x.what() << endl;
  }

  cout << "tAssert ended OK" << endl;
  return 0;                                 // exit with success status
}
