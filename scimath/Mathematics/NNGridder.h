//# NNGridder.h: Definition for Nearest Neighbour Gridder
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

#ifndef SCIMATH_NNGRIDDER_H
#define SCIMATH_NNGRIDDER_H

#include <casacore/casa/aips.h>
#include <casacore/scimath/Mathematics/Gridder.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// A non-negative gridding class
// </summary>

template <class Domain, class Range>
class NNGridder : public Gridder<Domain, Range>
{
public:
  NNGridder(const IPosition& shape, const Vector<Domain>& scale,
	    const Vector<Domain>& offset);

  virtual ~NNGridder() {}

  virtual Bool grid(Array<Range>& gridded,
		    const Vector<Domain>& position,
		    const Range& value);

  virtual Bool degrid(const Array<Range>& gridded,
		      const Vector<Domain>& position,
		      Range& value);

protected:
  virtual Range correctionFactor1D(Int loc, Int len);

  Vector<Int> loc;

protected:
  //# Make members of parent classes known.
  using Gridder<Domain,Range>::ndim;
  using Gridder<Domain,Range>::offsetVec;
  using Gridder<Domain,Range>::fillCorrectionVectors;
  using Gridder<Domain,Range>::onGrid;
};

} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Mathematics/NNGridder.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
