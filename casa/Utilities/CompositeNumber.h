//# CompositeNumber.h: generate a composite number
//# Copyright (C) 2000
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

#ifndef CASA_COMPOSITENUMBER_H
#define CASA_COMPOSITENUMBER_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Containers/Block.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary> This class generates composite numbers </summary>
// <use visibility=export>

// <synopsis>
// This class generates a list of composite numbers made up
// of powers of 2, 3, and 5, which are less than
// some max value and returns the smallest composite number greater
// than some number given.
// </synopsis>

// <example>
// <srcblock>
// CompositeNumber cn(1000);
// Int n = cn.nextLarger(319);
// Int m = cn.nextSmaller(462);
// Int l = cn.nearest(462);
// </srcblock>
// </example>


class CompositeNumber
{
public:
  // constructor:
  // Note: if you later make a call with value > maxval, we
  // will recalculate the list of composite numbers
  CompositeNumber (const uInt maxval = 8192);

  // destructor
  ~CompositeNumber();

  // return the next larger composite number
  uInt nextLarger(const uInt value);

  // return the next smaller composite number
  uInt nextSmaller(const uInt value);

  // return the nearest composite number
  uInt nearest(const uInt value);

  // return the next larger even composite number
  uInt nextLargerEven(const uInt value);

  // return the next smaller even composite number
  uInt nextSmallerEven(const uInt value);

  // return the closest even composite number
  uInt nearestEven(const uInt value);

  // returns True is value is composite
  Bool isComposite(const uInt value);

 private:

  Block<uInt> itsNumbers;
  uInt         itsMaxComplete;

  void generate(const uInt maxval);

};


} //# NAMESPACE CASACORE - END

#endif
