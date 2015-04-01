//# NNGridder.cc: Nearest Neighbour Gridder
//# Copyright (C) 1996,1997,1999
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
//#
//# $Id$

#ifndef SCIMATH_NNGRIDDER_TCC
#define SCIMATH_NNGRIDDER_TCC

#include <casacore/scimath/Mathematics/NNGridder.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/Arrays/ArrayMath.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// Constructor
template <class Domain, class Range>
NNGridder<Domain, Range>::NNGridder(const IPosition& shape,
				    const Vector<Domain>& scale,
				    const Vector<Domain>& offset) 
: Gridder<Domain, Range>(shape, scale, offset)
{
  fillCorrectionVectors();
  loc.resize(ndim);
  loc=0;
}

// Grid a value by moving to nearest neighbour
template <class Domain, class Range>
Bool NNGridder<Domain, Range>::grid(Array<Range> &gridded,
				    const Vector<Domain>& position,
				    const Range& value)
{
  loc=location(loc, position);
  loc-=offsetVec;
  if(onGrid(loc)) {
    gridded(loc)+=value;
    return True;
  }
  else {
    return False;
  }
}

// Degrid a value by taking value of nearest neighbour
template <class Domain, class Range>
Bool NNGridder<Domain, Range>::degrid(const Array<Range>& gridded,
				      const Vector<Domain>& position,
				      Range& value)
{
  loc=location(loc, position);
  loc-=offsetVec;
  if(onGrid(loc)) {
    value=gridded(loc);
    return True;
  }
  else {
    return False;
  }
}

// Correction factor for 1 dimension. This is the value that
// must be divided to get a correct flux.
template <class Domain, class Range>
Range NNGridder<Domain, Range>::correctionFactor1D(Int loc, Int len) 
{
  Int offset=loc-len/2;
  if(offset!=0) {
    Double arg=C::pi*Double(offset)/Double(len);
    return sin(arg)/arg;
  }
  else {
    return 1.0;
  }
}


} //# NAMESPACE CASACORE - END


#endif
