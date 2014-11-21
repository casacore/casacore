//# tAipsrc.cc: This program tests the Aipsrc interface
//# Copyright (C) 1996,1997,2002
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

#include <casacore/casa/aips.h>
#include <casacore/casa/System/Aipsrc.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
int main(){
  String aipsrcKeyword("printer.ps1.paper");
  String aipsrcValue;

  Aipsrc::find(aipsrcValue, aipsrcKeyword);
  cout << aipsrcKeyword << " " << aipsrcValue << endl;

  Aipsrc::reRead();
  Aipsrc::find(aipsrcValue, aipsrcKeyword);
  cout << aipsrcKeyword << " " << aipsrcValue << endl;

  aipsrcKeyword = String("bogus.wes.key");
  Aipsrc::find(aipsrcValue, aipsrcKeyword);
  cout << aipsrcKeyword << " " << aipsrcValue << endl;

  aipsrcKeyword = String("printer.ps.paper");
  Aipsrc::find(aipsrcValue, aipsrcKeyword);
  cout << aipsrcKeyword << " " << aipsrcValue << endl;

  aipsrcKeyword = String("dummy.abc");
  Aipsrc::find(aipsrcValue, aipsrcKeyword);
  cout << "*" << aipsrcKeyword << "*" << aipsrcValue << "*" << endl;

  cout << "AIPSROOT: " << Aipsrc::aipsRoot() << endl;
  cout << "AIPSARCH: " << Aipsrc::aipsArch() << endl;
  cout << "AIPSSITE: " << Aipsrc::aipsSite() << endl;
  cout << "AIPSHOST: " << Aipsrc::aipsHost() << endl;
  cout << "AIPSHOME: " << Aipsrc::aipsHome() << endl;

  Aipsrc::show(cout);

  // test the "find with default"
  {
    String result;
    AlwaysAssertExit(Aipsrc::find(result, "foobar", "default") == False &&
		 result == "default");
    AlwaysAssertExit(Aipsrc::findNoHome(result, "foobar", "default") == False &&
		 result == "default");
    
  }

  // test the register
  {
    uInt n = Aipsrc::registerRC("foobar", "invalid");
    uInt n1= Aipsrc::registerRC("printer.ps1.paper", "B4");
    cout << "Registrations: " << n << ", " << n1 << endl;
    cout << "Values: " << Aipsrc::get(n) << ", " << Aipsrc::get(n1) << endl;
    n = Aipsrc::registerRC("foobar", "invalid");
    Aipsrc::set(n1, "C4");
    cout << "Registrations: " << n << ", " << n1 << endl;
    cout << "Values: " << Aipsrc::get(n) << ", " << Aipsrc::get(n1) << endl;

    Aipsrc::save(n1);
  }


  return 0; 
}


