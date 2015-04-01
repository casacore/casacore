//# SimOrdMapIO.h: SimOrdMap IO operations
//# Copyright (C) 1993,1994,1995,1999,2000,2001
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

#ifndef CASA_SIMORDMAPIO_H
#define CASA_SIMORDMAPIO_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Containers/SimOrdMap.h>
#include <casacore/casa/iosfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// Class declarations.
class AipsIO;

// <summary> Global IO functions </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
// This header file defines the AipsIO functions for template SimpleOrderedMap.
// If these functions were defined in the SimpleOrderedMap class itself,
// it would require that AipsIO was defined for the template arguments.
// This would prevent using SimpleOrderedMap with imported classes.
// <group name=inoutput>

template<class K, class V>
AipsIO& operator<<(AipsIO&, const SimpleOrderedMap<K,V>&);

template<class K, class V>
AipsIO& operator>>(AipsIO&, SimpleOrderedMap<K,V>&);

template<class K, class V>
ostream& operator<<(ostream&, const SimpleOrderedMap<K,V>&);
// </group>


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/casa/Containers/SimOrdMapIO.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
