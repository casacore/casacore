//# EnvVar.h: Environment variables class
//# Copyright (C) 1993,1994,1995,1999,2002
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

#ifndef CASA_ENVVAR_H
#define CASA_ENVVAR_H

#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// This class allows for getting enviroment variables
// </summary>

// <use visibility=export>
// <reviewed reviewer="Paul Shannon" date="1995/02/08" tests="tEnvVar" demos="">

// <synopsis> 
// Environment variables are familiar to every Unix, MSDOS and VMS
// computer user.  This class makes it convenient to get and
// enquire about environment variables from within a C++ program.
// </synopsis> 

// <example>
// Check if an environment variable is defined.
// If so, get its value.
// <srcblock>
//  if (EnvironmentVariable::isDefined ("PATH")) {
//    cout << EnvironmentVariable::get ("PATH") << endl;
//  }
// </srcblock>
// </example>

// <todo asof=1994/02/08>
// </todo>

class EnvironmentVariable
{
public:
  // Is environment variable with given name defined? 
  static Bool isDefined (const String& name);

  // Get the value of environment variable with given name.
  // If not defined, return an empty String.
  static String get (const String& name);

  // Define environment variable.
  // If it already exists, its value will be overwritten.
  static void set (const String& name, const String& value);

private:
  // This class is not meant to be constructed.
  EnvironmentVariable();
};



} //# NAMESPACE CASACORE - END

#endif

