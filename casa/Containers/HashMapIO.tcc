//# HashMapIO.cc: Classes to perform IO for HashMap classas
//# Copyright (C) 1993,1994,1995,2000
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

#ifndef CASA_HASHMAPIO_TCC
#define CASA_HASHMAPIO_TCC


#include <casacore/casa/Containers/HashMapIO.h>

#if defined(AIPS_STUPID_SUN)
#undef AIPS_STUPID_SUN
#endif

namespace casacore { //# NAMESPACE CASACORE - BEGIN


template<class key, class val> ostream &operator<<(ostream &ios, const HashMap<key,val> &map) {
#if !defined(AIPS_STUPID_SUN)
  ConstHashMapIter<key,val> mapP(map);
#else
  ConstHashMapIter<key,val> *mapPP = map.getIter();
  ConstHashMapIter<key,val> &mapP = *mapPP;
#endif
  mapP.toStart();
//ios << map.ndefined();
//ios << map.defaultVal();
  while (mapP.atEnd() != True) {
    ios << "(";
    ios << mapP.getKey();
    ios << ",";
    ios << mapP.getVal();
    ios << ")";
    mapP++;
  }
#if defined(AIPS_STUPID_SUN)
  delete mapPP;
#endif
  return ios;
}

template<class key, class val> ostream &operator<<(ostream &ios, const ConstHashMapIter<key,val> &map) {

  return operator<<(ios,map.container());

}

} //# NAMESPACE CASACORE - END


#endif
