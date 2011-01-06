//# ContainerIO.tcc: text output IO for any container
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
//# $Id$

//# Includes
#include <casa/Containers/ContainerIO.h>

namespace casa { //# NAMESPACE CASA - BEGIN

  template<class ITER>
  void showDataIter (ostream& os, ITER begin, const ITER& end, const char* sep,
                     const char* prefix, const char* postfix)
  {
    // Note that the begin iterator is passed by value, so it can be used
    // directly.
    os << prefix;
    Bool first = True;
    for (; begin!=end; ++begin) {
      if (first) {
        first = False;
      } else {
        os << sep;
      }
      os << *begin;
    }
    os << postfix;
  }

} //# NAMESPACE CASA - END

