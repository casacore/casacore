//# <HashMap.h>: this defines HashMap, which is a hashed associative array
//# Copyright (C) 1995,1996,1999,2000
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

#ifndef CASA_HASHMAPIO_H
#define CASA_HASHMAPIO_H


#include <casacore/casa/aips.h>
#include <casacore/casa/Containers/HashMapIter.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
//     HashMap/HashMapIter IO functions
// </summary>
//
// <synopsis>
// These are the declarations for the standard
// <linkto class=HashMap>HashMap</linkto>/
// <linkto class=HashMapIter>HashMapIter</linkto> IO functions.
// They allow these classes to be written out to an <em>iostream</em>
// or to <linkto class=AipsIO>AipsIO</linkto>.
// </synopsis>
//
// <linkfrom anchor=hashmapio classes="HashMap HashMapIter ConstHashMapIter">
//  Related <here>IO functions</here>
// </linkfrom>
//
// <group name=hashmapio>
template<class key, class val> ostream &operator<<(ostream &, const ConstHashMapIter<key,val> &);
template<class key, class val> ostream& operator<<(ostream&, const HashMap<key,val>&);
// </group>


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/casa/Containers/HashMapIO.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
