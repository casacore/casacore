//# STLMath.cc: Math operations on STL-like containers
//# Copyright (C) 2014
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
//# $Id: STLMath.h 21331 2013-03-26 15:08:06Z gervandiepen $

//# Includes
#include <casacore/casa/BasicSL/STLMath.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  void throwContainerSizes (const char* name, size_t l1, size_t l2)
  {
    throw AipsError ("STLMath function " + String(name) +
                     ": container sizes differ ( " +
                     String::toString(l1) + " and " +
                     String::toString(l2) + ')');
  }

} //# NAMESPACE CASACORE - END
