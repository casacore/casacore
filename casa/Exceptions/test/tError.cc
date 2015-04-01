//# tError.cc: Test program for the Error class
//# Copyright (C) 2014
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
//# $Id: tArray.cc 21335 2013-03-28 14:20:18Z gervandiepen $

#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Utilities/Assert.h>

using namespace casacore;

int main()
{
  try {
    CASATHROW (AipsError, "msg " << 31);
  } catch (const AipsError& x) {
    cout << x.what() << endl;
    AlwaysAssertExit (x.getMesg() == "msg 31");
  }

  ThrowIf (false, "msg1");

  bool caught = false;
  try {
    ThrowIf (true, "msg2");
  } catch (const AipsError& x) {
    cout << x.what() << endl;
    caught = true;
  }
  AlwaysAssertExit (caught);

  ThrowIfError (0, "msg1");

  caught = false;
  try {
    ThrowIfError (10, "msg2");
  } catch (const AipsError& x) {
    cout << x.what() << endl;
    caught = true;
  }
  AlwaysAssertExit (caught);

  return 0;
}
