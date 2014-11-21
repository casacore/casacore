//# tParam.cc:  the test code for the Param class.
//# Copyright (C) 1994,1995,1998,1999,2000,2001
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
#include <casacore/casa/Inputs/Param.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
int main()
{
  try {

    // default constructor
    Param deflt;
    
    // normal constructor with optional value and help strings
    Param normal("key", "value", "help", "type", "range", "unit");
    
    // copy constructor
    Param copyofnormal(normal);
    
    // destructor
    //~Param();
    
    // assignment operator
    deflt = normal;
    
    // Equality comparitor - this test is meaningless
    AlwaysAssertExit(!(deflt == normal));
    
    // I/O operators
    //<group>
//friend ostream & operator<<(ostream &, const Param &p);
//friend istream & operator>>(istream &, Param &p);
    
//friend AipsIO & operator<<(AipsIO &, const Param &p);
//friend AipsIO & operator>>(AipsIO &, Param &p);
    //</group>
      
    // get a String parameter value; prompt if switch is TRUE
    AlwaysAssertExit(String("value")==deflt.getString());
    
   // set new parameter value; return FALSE if invalid value
    AlwaysAssertExit(deflt.put("5.0"));
    
    // get a double parameter value; prompt if switch is TRUE
    AlwaysAssertExit(5.0 == deflt.getDouble());
    
    // get an int parameter value; prompt if switch is TRUE
    AlwaysAssertExit(5 == deflt.getInt());
    
    // get a Block<double> parameter value; prompt if switch is TRUE
    AlwaysAssertExit(deflt.put("5.0, 5.0, 5.0"));
    int i;
    for (i=0;i<3;i++) AlwaysAssertExit(5.0 == deflt.getDoubleArray()[i]);
    
    // get an Block<Int> parameter value; prompt if switch is TRUE
    for (i=0;i<3;i++) AlwaysAssertExit(5 == deflt.getIntArray()[i]);
    
// this function doesn't work
    // get a Block<String> parameter value; prompt if switch is TRUE
//    for (i=0;i<3;i++) cout << deflt.getStringArray()[i] << endl;
//    for (i=0;i<3;i++) AlwaysAssertExit(String("5") == deflt.getStringArray()[i]);

    // get a Boolean parameter value; prompt if switch is TRUE
    deflt.put("True");
    AlwaysAssertExit(deflt.getBool());
  
    // get parameter value as a string
    AlwaysAssertExit(String("True")==deflt.get());
    
    // get parameter help string
    AlwaysAssertExit(String("help")==deflt.getHelp());
    
    // get parameter name 
    AlwaysAssertExit(String("key")==deflt.getKey());
    
    // get the string `key = value' for the parameter
    AlwaysAssertExit(String("key=True")==deflt.keyVal());
    
    // get the type of a parameter
    AlwaysAssertExit(String("type")==deflt.getType());
    
    // get the valid range of a parameter
    AlwaysAssertExit(String("range")==deflt.getRange());
    
    // get the units of a parameter
    AlwaysAssertExit(String("unit")==deflt.getUnit());
    
    // set a parameter as a system parameter
    deflt.setSystem(True);
    
    // check if a parameter is a system parameter
    AlwaysAssertExit(deflt.isSystem());
    
    // set an index for a program parameter
    deflt.setIndex(2);
    
    // get the index of a parameter
    AlwaysAssertExit(2==deflt.getIndex());

    cout << "OK" << endl;

  } catch(AipsError x) {
    cout << "Caught exception" << endl;
    cout << "Message is: " << x.getMesg() << endl;
  }    
}


