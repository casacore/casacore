//# EnvVar.h: Environment variables class
//# Copyright (C) 1993,1994,1995,1999
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

#if !defined(AIPS_ENVIRONMENTVARIABLES_H)
#define AIPS_ENVIRONMENTVARIABLES_H

#include <aips/aips.h>
#include <aips/Utilities/String.h>

// <summary> This class allows for setting and getting enviroment variables
// </summary>
// <use visibility=export>
// <reviewed reviewer="Paul Shannon, pshannon@nrao.edu" date="1995/02/08" tests="tEnvVar" demos="">
// <synopsis> 
// Environment variables are familiar to every Unix, MSDOS and VMS
// computer user.  This class makes it convenient to set, get, and
// enquire about environment variables from within a C++ program.
// </synopsis> 
// <example>
// Display all of the environment variables, add one, make sure the
// count has changed, then remove it.
// <srcblock>
//  EnvironmentVariables ev;
//  cout << "currently " << ev.number() << " variables set" << endl;
//
//  for (uInt i=0; i < ev.number (); i++)
//     cout << ev.name (i) << ": " << ev.value (i) << endl;
//
//  ev.set ("newVar", "newValue");
//  cout << "now there are " << ev.number  () << " variables" << endl;
//  cout << "variable newVar has value " << ev.value ("newVar") << endl;
//
//  if ev.isSet ("newVar")
//     ev.unSet ("newVar");
// </srcblock>
// </example>



// <todo asof=1994/02/08>
//   <li> class is poorly designed: with no data members, hard-coded maximum
//        number of environment variables
//   <li> redesign should include destructor 
//   <li> static member functions are a curious choice, usually used
//        to share data among class instances...
//   <li> name, value ought to throw an exception when number is out
//        of range, rather than return an empty string.
// </todo>

class EnvironmentVariables {

 public:
   enum {maxenviron = 1000}; // Maximun environment size

   // Construct a new instance, which is filled with the current environment.
   EnvironmentVariables ();

   // Construct a new instance, which is filled with the current environment,
   // to which is added the name=value pair <nameAndValuePair>.
   EnvironmentVariables (const String &nameAndValuePair);

   // Construct a new instance, which is filled with the current environment
   // to which is added the new name/value pair described by the two
   // arguments.
   EnvironmentVariables (const String &name, const String &value);

   // The number of environment variables.
   static uInt number ();

   // The name of the nth environment variable; returns an empty String
   // if number is out of range.
   static String name (uInt number);

   // The value of the nth environment variable; an empty string if
   // number is out of range.
   static String value (uInt number);

   // Get the value corresponding to name.
   // otherwise, returns a empty String
   static String value (const String &name);

   // Is name set? 
   static Bool isSet (const String &name);

   // Deletes the name/value pair.
   static void unSet (const String &name);

   // Add name/value pair to the environment.  
   // If the name already exists, its old definition is first removed.
   static Bool set (const String &name, const String &value);

   // Add name/value pair (encoded in a single String: "name=value")
   // to the environment.  
   // If the name already exists, its old definition is first removed.
   static Bool set (const String &nameAndValuePair);

 private:
   char *myenviron [maxenviron+1];
   static Bool setenv (const String &name, const String &value);
};

#endif

