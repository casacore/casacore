//# SimOrdMapIO.cc: SimOrdMap IO operations
//# Copyright (C) 1993,1994,1995,2001
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

#ifndef CASA_SIMORDMAPIO_TCC
#define CASA_SIMORDMAPIO_TCC

#include <casacore/casa/Containers/SimOrdMapIO.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/iostream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// Write an OrderedMap into an AipsIO styream.
template<class K, class V>
AipsIO& operator<< (AipsIO& ios, const SimpleOrderedMap<K,V>& map)
{
    // Start writing the object (VERSION 1).
    ios.putstart ("SimpleOrderedMap", SimpleOrderedMap<K,V>::Version());
    ios << map.defaultVal();
    ios << map.ndefined();
    ios << map.incr();
    for (uInt i=0; i<map.ndefined(); i++) {
	ios << map.getKey(i);
	ios << map.getVal(i);
    }
    ios.putend ();
    return ios;
}

template<class K, class V>
AipsIO& operator>> (AipsIO& ios, SimpleOrderedMap<K,V>& map)
{
    K key;
    V val;
    // Start reading the object.
    // Delete the current keys and values.
    ios.getstart ("SimpleOrderedMap");
    map.clear();
    // Now read in the values and store them into the map.
    ios >> map.defaultVal();
    uInt nr,ni;
    ios >> nr;
    ios >> ni;
    map.incr(ni);
    for (uInt i=0; i<nr; i++) {
	ios >> key;
	ios >> val;
	map.define(key,val);
    }
    ios.getend ();
    return ios;
}

  
template <class K, class V>
ostream& operator<< (ostream& ios, const SimpleOrderedMap<K,V>& map)
{
    ios << "nrtot=";
    ios << map.ntot();
    ios << " nrelem=";
    ios << map.ndefined();
    ios << " nrincr=";
    ios << map.incr();
    ios << endl;
    for (uInt i=0; i<map.ndefined(); i++) {
	ios << map.getKey(i);
	ios << " ";
	ios << map.getVal(i);
	ios << endl;
    }
    return ios;
}

} //# NAMESPACE CASACORE - END


#endif
