//# Map.cc: Associative array classes (non-templated)
//# Copyright (C) 1994,1995,1998
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

#include <casacore/casa/Containers/Map.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

void throw_mapiter_init_error(){
  throw(AipsError("Map Initialization Error"));
}

void throw_map_init_error(){
  throw(AipsError("Map Initialization Error"));
}

void throw_invalid_mapiter_error(){
  throw(AipsError("Use of invalid iterator."));
}

void throw_map_constop_error(){
  throw(AipsError("Invalid Operation on a Const Map"));
}

} //# NAMESPACE CASACORE - END

