//# Program.cc: This program ...
//# Copyright (C) 1996,1997
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

#include <aips/aips.h>
#include <aips/Tasking/Aipsrc.h>
#include <aips/Utilities/Assert.h>

int main(){
  String aipsrcKeyword("printer.ps1.paper");
  String aipsrcValue, aipsrcFile, aipsrcLine;

  Aipsrc::find(aipsrcValue, aipsrcFile, aipsrcLine, aipsrcKeyword);
  cerr << aipsrcKeyword << " " << aipsrcValue << endl
       << aipsrcFile << endl << aipsrcLine << endl << endl;

  Aipsrc::reRead();
  Aipsrc::find(aipsrcValue, aipsrcKeyword);
  cerr << aipsrcKeyword << " " << aipsrcValue << endl;
  Aipsrc::find(aipsrcValue, aipsrcFile, aipsrcLine, aipsrcKeyword);
  cerr << aipsrcKeyword << " " << aipsrcValue << endl
       << aipsrcFile << endl << aipsrcLine << endl << endl;

  aipsrcKeyword = String("bogus.wes.key");
  Aipsrc::find(aipsrcValue, aipsrcKeyword);
  cerr << aipsrcKeyword << " " << aipsrcValue << endl;
  Aipsrc::find(aipsrcValue, aipsrcFile, aipsrcLine, aipsrcKeyword);
  cerr << aipsrcKeyword << " " << aipsrcValue << endl
       << aipsrcFile << endl << aipsrcLine << endl << endl;

  aipsrcKeyword = String("printer.ps.paper");
  Aipsrc::find(aipsrcValue, aipsrcFile, aipsrcLine, aipsrcKeyword);
  cerr << aipsrcKeyword << " " << aipsrcValue << endl
       << aipsrcFile << endl << aipsrcLine << endl << endl;

  Aipsrc::find(aipsrcValue, aipsrcFile, aipsrcLine, aipsrcKeyword);
  cerr << aipsrcKeyword << " " << aipsrcValue << endl
       << aipsrcFile << endl << aipsrcLine << endl << endl;

  aipsrcKeyword = String("printer.psnet.paper");

  aipsrcKeyword = String("dummy.abc");
  Aipsrc::find(aipsrcValue, aipsrcFile, aipsrcLine, aipsrcKeyword);
  cerr << "*" << aipsrcKeyword << "*" << aipsrcValue << "*" << endl
       << "*" << aipsrcFile << "*" << endl 
       << "*" << aipsrcLine << "*" << endl;

  Aipsrc::show(cerr);

  // test the "find with default"
  {
    String result;
    AlwaysAssertExit(Aipsrc::find(result, "foobar", "default") == False &&
		 result == "default");
    AlwaysAssertExit(Aipsrc::findNoHome(result, "foobar", "default") == False &&
		 result == "default");
    
  }

  return 0; 
}


