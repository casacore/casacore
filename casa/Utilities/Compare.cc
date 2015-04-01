//# Compare.cc: Non-templated code to compare two objects
//# Copyright (C) 2013
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
//# $Id: Compare.tcc 20997 2010-11-17 07:05:29Z gervandiepen $

#include <casacore/casa/Utilities/Compare.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN


CompareNoCase::~CompareNoCase()
{}

int CompareNoCase::comp(const void * obj1, const void * obj2) const
{
  const String& v1 = *static_cast<const String*>(obj1);
  const String& v2 = *static_cast<const String*>(obj2);
  return fcompare (v1, v2);
}


} //# NAMESPACE CASACORE - END

