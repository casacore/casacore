//# Param: A simple keyword/value pair with internal help Strings.
//# Copyright (C) 1993,1994,1995,1999,2000,2001
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

#ifndef CASA_PARAM_H
#define CASA_PARAM_H


#include <casacore/casa/aips.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/stdlib.h>
#include <casacore/casa/string.h>             // need things like strlen() and such

//# Forward declarations
#include <casacore/casa/iosfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary> 
// A simple keyword/value pair with internal help Strings.
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tParam.cc" demos="">
//</reviewed>

// <prerequisite>
//   <li> none noted
// </prerequisite>
//
// <etymology>
// The Param class name is a shortening of "parameter" and is indicative of 
// the class being designed as a keyword/value pair relating to command line
// arguments.  The existing Keyword class does a much better job for most
// other purposes.
// </etymology>
//
// <synopsis>
// The Param is constructed with all arguments being Strings.  This is a 
// reflection of the C-type command line argument method of passing 
// an integer (argc or argument count) and an array of pointers to characters
// (argv or argument vector.)  If "char* argv[]" is broken into its individual
// arguments they may be used to fill a Param.  The constructor pairs up a 
// "key" to a value.  A help String argument is provided to assist in prompted
// filling of Param values.  The expected return type may be entered as well 
// as a range of potential values.  Finally, the units of the value are also 
// specified.  The intent is to provide a well documented value and a "key"
// by which to "call" it.
//
// The "getWhatever" member functions of Param convert the internal Strings 
// into the desired output data type.  The Strings themselves may also be 
// returned.
// </synopsis> 
//
// <example>
// <srcblock>
// // we will create a Param which contains the boundary for an iteration loop.
// String key("IterBound");
// // give "IterBound" a default value
// String value("200");
// // a help String for prompting
// String help("The Boundary value for the chutzpah iterator.");
// // The expected return type is an integer
// String type("Int");
// // The range of "legal" values 
// String range("10-10000");
// // the units of the value
// String unit("unitless"):
// // Now we may build our Param
// Param PleaseDontTouchMeThere(key, value, help, type, range, unit);
// // to retrieve the value we use the GetInt function
// for (Int i=0, i<PleaseDontTouchMeThere.getInt(); i++, chutzpah++);
// </srcblock></example>
//
// <motivation>
// The Param class was an early attempt at keywords within Casacore. They have 
// become obsolete but hang on due to their relationship with the Input class.
// </motivation>
//
// <todo asof="Thu 1995/04/06 21:26:43 GMT">
//   <li> fix the GetStringArray() function
//   <li> convert from Block<T> to Array<T> as return values.
//   <li> replace entirely with Casacore Keywords?
// </todo>


class Param
{
public:
				// constructors and destructor
  // default constructor
  Param();

  // normal constructor with optional value and help strings
  Param (const String& key, const String& value, const String& help,
	 const String& type, const String& range, const String& unit);
  
  // copy constructor
  Param (const Param&);
  
  // destructor
  ~Param();
  
  // assignment operator
  Param& operator= (const Param&);
  
  // Equality comparitor.
  // <note role=warning> This function ALWAYS returns 
  // false.  I have no idea why it was designed to do this. </note>
  Bool operator== (const Param&) const;
  
  // I/O operators
  //<group>
  friend ostream& operator<< (ostream&, const Param& p);
  friend istream& operator>> (istream&, Param& p);
  friend AipsIO& operator<< (AipsIO&, const Param& p);
  friend AipsIO& operator>> (AipsIO&, Param& p);
  //</group>
  
  // get a double parameter value; prompt if switch is TRUE
  Double getDouble (Bool do_prompt=False) const;

  // get a Block<double> parameter value; prompt if switch is TRUE
  Block<Double> getDoubleArray (Bool do_prompt=False) const;
  
  // get an Int parameter value; prompt if switch is TRUE
  Int getInt (Bool do_prompt=False) const;

  // get an Block<Int> parameter value; prompt if switch is TRUE
  Block<Int> getIntArray (Bool do_prompt=False) const;
  
  // get a String parameter value; prompt if switch is TRUE
  const String& getString (Bool do_prompt=False) const;

  // get a Block<String> parameter value; prompt if switch is TRUE
  Block<String> getStringArray (Bool do_prompt=False) const;
  
  // get a Boolean parameter value; prompt if switch is TRUE
  Bool getBool (Bool do_prompt=False) const;
  
  // get parameter value as a string
  const String& get() const
    { return value; }
  
  // get parameter help string
  const String& getHelp() const
    { return help; }
  
  // get parameter name 
  const String& getKey() const
    { return key; }
  
  // get the string `key = value' for the parameter
  String keyVal() const
    { return key + "=" + value; }
  
  // get the type of a parameter
  const String& getType() const
    { return type; }
  
  // get the valid range of a parameter
  const String& getRange() const
    { return range; }
  
  // get the units of a parameter
  const String& getUnit() const
    { return unit; }
  
  // set new parameter value; return FALSE if invalid value
  Bool put (const String& a_value);
  
  // set a parameter as a system parameter
  void setSystem (Bool val)
    { system = val; }
  
  // check if a parameter is a system parameter
  Bool isSystem() const
    { return system; }
  
  // set an index for a program parameter
  void setIndex (Int inx)
    { index = inx; }

  // get the index of a parameter
  Int getIndex() const
    { return index; }


private:
  // parameter name
  String key;

  // parameter value
  String value;

  // help string
  String help;

  // type of parameter 		
  String type;

  // range/validity/pre-check
  String range;  

  // optional unit associated with value
  String unit;
  
  // boolean data member which indicates the Param's key has a value.
  Bool hasvalue;     

  // boolean data member which indicates the Param is system wide.
  Bool system;

  // index for program keywords (>=1)
  Int index;
};



} //# NAMESPACE CASACORE - END

#endif



