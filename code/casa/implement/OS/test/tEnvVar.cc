//# tEnvVar.cc: This program tests the EnvironmentVariables class
//# Copyright (C) 1994,1995,1999,2000
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
//#--------------------------------------------------------------------------
#include <aips/OS/EnvVar.h>
#include <aips/Exceptions/Error.h>
//#--------------------------------------------------------------------------
int main ()
{

  try {
    EnvironmentVariables ev0;
    uInt ev0Count = ev0.number ();
    if (ev0.number () < 5)
      throw (AipsError ("tEnvVar needs at least5 variables defined "
                        "in the environment of the parent shell"));
    EnvironmentVariables ev1 ("abracadabra=kalamazoo");
    if (ev1.number () != ev0Count + 1)
      throw (AipsError ("failed to add new pair with ev1 ctor"));
    // case is significant, so this adds a new pair to the environment
    EnvironmentVariables ev2 ("ABRACADABRA","KALAMAZOO");  
    if (ev2.number () != ev0Count + 2)
      throw (AipsError ("failed to add new pair with ev2 ctor"));
    if (ev0.name (1) != ev1.name (1))
      throw (AipsError ("ev0 & ev1 name #1 do not agree"));
    if (ev0.value (2) != ev1.value (2))
      throw (AipsError ("ev0 & ev1 value #2 do not agree"));
    String name4 = ev1.name (4);
    if (ev0.value (name4) != ev2.value (name4))
      throw (AipsError ("value extraction by name failed"));
    if (!ev0.isSet (name4))
      throw (AipsError ("isSet by name failed"));
    ev0.unSet (name4);
    if (ev0.isSet (name4))
      throw (AipsError ("unSet by name failed"));
    uInt count = ev0.number ();
    ev0.set ("grass", "hopper");
    if (ev0.value ("grass") != "hopper")
      throw (AipsError ("set & value (grass) retrieval failed"));
    ev0.set ("sweet=potato");
    if (ev0.value ("sweet") != "potato")
      throw (AipsError ("set & value (sweet) retrieval failed"));
    if (ev0.number () != ev0Count + 3)
       throw (AipsError ("sets, unsets, and ctor-implicit-sets do not tally"));
  } catch   (AipsError x) {
    cerr << "----------------------- exception! -------------------" << endl;
    cerr << x.getMesg () << endl;
    cerr << "at line " << x.thrownLine () << " in " << x.thrownFile () << endl;
  } 
  
  cout << "OK" << endl;
  return 0;

}
//#--------------------------------------------------------------------------
