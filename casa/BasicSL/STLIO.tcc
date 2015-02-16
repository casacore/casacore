//# STLIO.tcc: text output IO for any STL-like container
//# Copyright (C) 2011
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
//# $Id: STLIO.tcc 21315 2013-02-13 12:24:02Z gervandiepen $

#ifndef CASA_STLIO_TCC
#define CASA_STLIO_TCC

//# Includes
#include <casacore/casa/BasicSL/STLIO.h>
#include <casacore/casa/IO/AipsIOCarray.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  template<class ITER>
  void showDataIter (ostream& os, ITER begin, const ITER& end, const char* sep,
                     const char* prefix, const char* postfix)
  {
    // Note that the begin iterator is passed by value, so it can be used
    // directly.
    os << prefix;
    if (begin != end) {
      os << *begin;
      ++begin;
    }
    for (; begin!=end; ++begin) {
      os << sep << *begin;
    }
    os << postfix;
  }

  template<typename T>
  AipsIO& operator>> (AipsIO& ios, std::vector<T>& v)
  {
    ios.getstart ("Block");
    uInt nr;
    ios >> nr;
    v.resize(nr);
    getAipsIO(ios, nr, &(v[0]));
    ios.getend();
    return ios;
  }
  template<typename T>
  AipsIO& operator<< (AipsIO& ios, const std::vector<T>& v)
  {
    ios.putstart ("Block", 1);
    putAipsIO (ios, (uInt)v.size(), &(v[0]));
    ios.putend();
    return ios;
  }

} //# NAMESPACE CASACORE - END

#endif
