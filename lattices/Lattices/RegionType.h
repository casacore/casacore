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

#ifndef LATTICES_REGIONTYPE_H
#define LATTICES_REGIONTYPE_H

#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Define the various region types.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <synopsis>
// This class defines 2 enum's used by the region classes in module
// Lattices and Images.
// </synopsis>


class RegionType
{
public:
    // Define the type of region.
    // The values are used in regionmanager(gui).g, so they should
    // not be changed.
    enum Type {
        // Not used yet.
	Invalid = -1,
        // Other type is not used yet.
	Other = 0,
        // lattice region (pixel coordinates)
	LC = 1,
        // image region (world coordinates)
	WC = 2,
        // array slicer (pixel based with optional stride)
	ArrSlicer = 3,
        // Number of recognized types only
        nRegionTypes
    };

    // Define if a region is absolute or relative.
    // The values are used in regionmanager(gui).g, so they should
    // not be changed.
    enum AbsRelType {
        // absolute
	Abs    = 1,
        // relative to reference pixel
	RelRef = 2,
        // relative to center
	RelCen = 3,
        //# relative to a direction
//#	RelDir = 4,
        // Number of recognized types only
	nAbsRelTypes
    };

    static AbsRelType absRelTypeFromString(const String& absreltype);


};



} //# NAMESPACE CASACORE - END

#endif
