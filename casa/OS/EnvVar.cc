//# EnvVar.cc: Environment variables class
//# Copyright (C) 1993,1994,1995,1998,2001,2002
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

#include <casacore/casa/OS/EnvVar.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/stdlib.h>
#include <cstring>                  //# for strcpy with gcc-4.3


namespace casacore { //# NAMESPACE CASACORE - BEGIN

Bool EnvironmentVariable::isDefined (const String& name)
{
  return getenv (name.chars());
}

String EnvironmentVariable::get (const String& name)
{
  Char* env = getenv (name.chars());
  if (env) return String(env);
  return String();
}

void EnvironmentVariable::set (const String& name, const String& value)
{
  uInt nl = name.length();
  uInt vl = value.length();
  Char* str = new Char [nl + vl + 2];
  strcpy (str, name.chars());
  str[nl] = '=';
  strcpy (str+nl+1, value.chars());
  // Note that putenv takes over the pointer, so we should not delete str.
  AlwaysAssert (putenv(str) == 0, AipsError);
}

} //# NAMESPACE CASACORE - END

