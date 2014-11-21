//# tEnvVar.cc: This program tests the EnvironmentVariable class
//# Copyright (C) 1994,1995,1999,2000,2001,2002
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

#include <casacore/casa/OS/EnvVar.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
int main ()
{
  try {
    AlwaysAssertExit (EnvironmentVariable::isDefined ("HOME"));
    AlwaysAssertExit (! EnvironmentVariable::isDefined ("crazyHOMExyz"));
    AlwaysAssertExit (EnvironmentVariable::get("HOME").length() > 0);
    AlwaysAssertExit (EnvironmentVariable::get("crazyHOMExyz").length() == 0);
    EnvironmentVariable::set("crazyHOMExyz", "abc");
    AlwaysAssertExit (EnvironmentVariable::isDefined ("crazyHOMExyz"));
    AlwaysAssertExit (EnvironmentVariable::get("crazyHOMExyz").length() != 0);
    AlwaysAssertExit (EnvironmentVariable::get("crazyHOMExyz") == "abc");
  } catch (AipsError x) {
    cerr << "Unexpected exception: " << x.getMesg() << endl;
    return 1;
  } 
  cout << "OK" << endl;
  return 0;

}
