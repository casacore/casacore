//# OrdMapIO.h: AipsIO for templated class OrderedMap
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

#ifndef CASA_ORDMAPIO_H
#define CASA_ORDMAPIO_H

// <summary>
// Input/output operators for OrderedMaps.
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=OrderedMap>OrderedMap</linkto>
//   <li> ostream
// </prerequisite>

// <synopsis> 
// The global functions in MapIO.h provide easy input and output of
// OrderedMap objects.
// </synopsis>
//
// <group name="OrderedMap IO">
// </group>

#include <casacore/casa/aips.h>
#include <casacore/casa/Containers/MapIO.h>
#include <casacore/casa/Containers/OrderedMap.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN
} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/casa/Containers/OrdMapIO.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif


