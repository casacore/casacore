//# tInput.cc: this tests the simple argc, argv command line user interface.
//# Copyright (C) 1994,1995,1999,2000,2002
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
#include <casacore/casa/Inputs/Input.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
int main(int argc, const char* argv[])
{
  try {
    // The default constructor enables the creation of parameters. 
    // If the optional Bool argument is True, the parameters "help" and "debug" 
    // are created from their shell environment values.
    // It puts the program in no-prompt mode unless environment variable HELP 
    // is defined with value "prompt". The output debug level is set according 
    // to the value of the environment variable DEBUG.
    Input sysInput(1);
    Input prgInput;
    
    // Destructor.
    //~Input();
    
    // Create a new parameter, either from scratch or looking it
    // up from an internal list of templates.
    // The function also checks whether parameters can still be created,
    // and whether key is unique for the program.
    // The value, help and remaining arguments are all optional.
    // <group>
    prgInput.create("zero"); 
    prgInput.create("one", "1"); 
    prgInput.create("two", "2.00", "the key named two"); 
    prgInput.create("three", "three", "the key named three", "String"); 
    prgInput.create("four", "True", "the key named four", "Bool", "N/A");
    prgInput.create("five", "5,5,5,5,5", "a Block of fives", "Block<Int>", 
		    "3-5", "meters/second");
    // <group>
    
    // Set a new value for an existing named parameter
    // Returns FALSE if key is an unknown parameter name.
    // <group>
    AlwaysAssertExit(prgInput.put("zero", "False"));
    // The single argument is of the
    // form `key=value', where key is a valid parameter name.
    AlwaysAssertExit(prgInput.put("zero=666.012"));
    // </group>
    
    // fill the parameter list from argc, argv command line args
    prgInput.readArguments(argc, argv);
    
    // Disable the creation of parameters. Highly recommended, but
    // not required. ReadArguments calls it when filling the values from argv[]
    // prgInput.Close();
    
    // Get the double value of the parameter (or 0.0 if unknown key).
    // If the program is in prompt mode, ask the user for the value.
    AlwaysAssertExit(666.012 == prgInput.getDouble("zero"));
    
    // Get the Block<double> value of the parameter (or default Block if 
    // unknown key).
    // If the program is in prompt mode, ask the user for the value.
    AlwaysAssertExit(1.0==prgInput.getDoubleArray("one")[0]);

    // Get the int value of the parameter (or 0 if unknown key).
    // If the program is in prompt mode, ask the user for the value.
    AlwaysAssertExit(2 == prgInput.getInt("two"));

    // Get the Block<int> value of the parameter (or default Block if 
    // unknown key)
    // If the program is in prompt mode, ask the user for the value.
    for (int i=0;i<5;i++) AlwaysAssertExit(5==prgInput.getIntArray("five")[i]);
    
    // Get the String value of the parameter (or "" if unknown key).
    // If the program is in prompt mode, ask the user for the value.
    AlwaysAssertExit(String("three")== prgInput.getString("three"));
    
    // Get the boolean value of the parameter (or FALSE if unknown key).
    // If the program is in prompt mode, ask the user for the value.
    AlwaysAssertExit(prgInput.getBool("four"));
    
    // Get the total number of parameters of this program
    AlwaysAssertExit (prgInput.count() == 9);
    
    // See if the current debug level is thresholded
    AlwaysAssertExit (!prgInput.debug(2))
    
    // Set version string for announcements
    prgInput.version("OK");
    
    // Announce program and version.
    prgInput.announce();
    
  } catch(AipsError x) {
    cout << "Caught exception" << endl;
    cout << "Message is: " << x.getMesg() << endl;
  } 
}

  
