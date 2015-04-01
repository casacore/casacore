//# MapIO.h: Classes to perform IO for Map classas
//# Copyright (C) 1993,1994,1995,1999,2000
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

#ifndef CASA_MAPIO_H
#define CASA_MAPIO_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Containers/Map.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class AipsIO;

// <summary>
// Input/output operators for Maps.
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Map>Map</linkto>
//   <li> ostream
// </prerequisite>

// <synopsis> 
// These global functions provide easy input and output of Map objects.
// </synopsis>
//
// <group name="Map IO">

// Input/output
template<class key, class value> ostream& operator<<(ostream&, const Map<key,value>&);
template<class key, class value> ostream &operator<<(ostream &, const ConstMapIter<key,value> &);
// </group>


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/casa/Containers/MapIO.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
