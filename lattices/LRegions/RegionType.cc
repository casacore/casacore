//# RegionType.h: Define the various region types in an enum.
//# Copyright (C) 1998,2002
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

#include <casacore/lattices/LRegions/RegionType.h>

#include <casacore/casa/Exceptions/Error.h>

namespace casacore {

RegionType::AbsRelType RegionType::absRelTypeFromString(const String& absrel) {

    String str(absrel);
    str.upcase();
    if(str.contains("ABS"))
      return RegionType::Abs;
    else if(str.contains("RELREF"))
      return RegionType::RelRef;
    else if(str.contains("RELCEN"))
      return RegionType::RelCen;
    else
      throw(AipsError(String("Undefined region type")+absrel));
  }

}

