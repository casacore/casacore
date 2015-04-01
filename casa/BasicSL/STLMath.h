//# STLMath.h: Math operations on STL-like containers
//# Copyright (C) 2014
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
//# $Id: STLMath.h 21331 2013-03-26 15:08:06Z gervandiepen $

#ifndef CASA_STLMATH_H
#define CASA_STLMATH_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/BasicMath/Functors.h>
#include <vector>
#include <algorithm>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  // <summary>
  //    Math operations on STL-like containers
  // </summary>

  // <use visibility=export>

  // <reviewed reviewer="Paul Shannon" date="1995/02/21" tests="" demos="">
  // </reviewed>

  // <prerequisite>
  //   <li> STL container concept
  // </prerequisite>

  // <synopsis> 
  // This file defines a few functions with a math operation on a vector.
  // Others can be added later.
  // </synopsis>

  // <group name="Container Math">

  // Throw an exception that two container sizes mismatch.
  void throwContainerSizes (const char* name, size_t l1, size_t l2);

  // Check if the sizes of both containers are the same.
  template<typename CONTAINER>
  inline void checkContainerSizes (const CONTAINER& left, const CONTAINER& right,
                                   const char* name)
  {
    if (left.size() != right.size()) {
      throwContainerSizes (name, left.size(), right.size());
    }
  }

  // Add two std::vector objects.
  template<class T>
  std::vector<T> operator+ (const std::vector<T> &left,
                            const std::vector<T> &right)
  {
    checkContainerSizes(left, right, "+");
    std::vector<T> result(left.size());
    std::transform (left.begin(), left.end(), right.begin(),
                    result.begin(), std::plus<T>());
    return result;
  }

  // Divide a vector by a scalar.
  template<class T>
  std::vector<T> operator/ (const std::vector<T> &left, const T &right)
  {
    std::vector<T> result(left.size());
    std::transform (left.begin(), left.end(), result.begin(),
                    std::bind2nd(std::divides<T>(), right));
    return result;
  }

  // </group>

} //# NAMESPACE CASACORE - END

#endif
