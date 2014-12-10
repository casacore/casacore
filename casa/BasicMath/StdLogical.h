//# StdLogical.h: Logical Operations on STL-style containers
//# Copyright (C) 2012
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


#ifndef CASA_STDLOGICAL_H
#define CASA_STDLOGICAL_H

#include <casacore/casa/aips.h>
#include <casacore/casa/BasicMath/Functors.h>

namespace casacore {

  // Arbitrary compare operation on two STL-style containers.
  // It returns True if containers have equal size and
  // all elements compare True.
  template<typename C1, typename C2, typename CompareOperator>
  bool compareAll (const C1& l, const C2& r, CompareOperator op)
  {
    if (l.size() != r.size()) return false;
    return compareAll (l.begin(), l.end(), r.begin(), op);
  }

  // Test if all elements of the containers are relatively near each other.
  template<typename C1, typename C2, typename U>
  bool allNear (const C1& l, const C2& r, U tolerance)
    { return compareAll (l, r, Near<U>(tolerance)); }

  // Test if all elements of the containers are absolutely near each other.
  template<typename C1, typename C2, typename U>
  bool allNearAbs (const C1& l, const C2& r, U tolerance)
    { return compareAll (l, r, NearAbs<U>(tolerance)); }


} //# end namespace casacore


//# #ifndef CASACORE_NO_AUTO_TEMPLATES
//# #include <casacore/casa/BasicMath/StdLogical.tcc>
//# #endif //# CASACORE_NO_AUTO_TEMPLATES

#endif
