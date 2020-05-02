//# ArrayMathBase.h: Basic functions and classes for math on Array objects
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
//# $Id: ArrayMathBase.h 21262 2012-09-07 12:38:36Z gervandiepen $

#ifndef CASA_ARRAYMATHBASE_2_H
#define CASA_ARRAYMATHBASE_2_H

#include "ArrayFwd.h"

namespace casacore {

  // <summary>
  // Basic class for math on Array objects
  // </summary>
  //
  // <reviewed reviewer="UNKNOWN" date="" tests="tArrayMath">
  //
  // <prerequisite>
  //   <li> <linkto class=Array>Array</linkto>
  // </prerequisite>
  //
  // <synopsis>
  // The abstract base class ArrayFunctorBase is defined for functors to
  // be used in functions like slidingXXX.
  // Virtual functions instead of templated functions are used to avoid
  // code bloat when used in functions like partialArrayMath. Because a
  // reduction operation usually takes much more time than the call, using
  // virtual functions hardly imposes a performance penalty.
  // </synopsis>
  template<typename T, typename RES=T> class ArrayFunctorBase {
  public:
    virtual ~ArrayFunctorBase() {}
    virtual RES operator() (const Array<T>&) const = 0;
  };

} //# end namespace

#endif
