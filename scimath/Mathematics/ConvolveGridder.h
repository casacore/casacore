//# ConvolveGridder.h: Definition for Convolutional Gridder
//# Copyright (C) 1996,1997,1999,2003
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

#ifndef SCIMATH_CONVOLVEGRIDDER_H
#define SCIMATH_CONVOLVEGRIDDER_H

#include <casacore/casa/aips.h>
#include <casacore/scimath/Mathematics/Gridder.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Does convolutional gridding
// </summary>

template <class Domain, class Range>
class ConvolveGridder : public Gridder<Domain, Range>
{
public:

  ConvolveGridder(const IPosition& shape, const Vector<Domain>& scale,
		  const Vector<Domain>& offset, const String& convType="SF");

  virtual void setConvolutionFunction(const String& type);

  virtual ~ConvolveGridder() {}

  virtual Bool grid(Array<Range>& gridded,
		    const Vector<Domain>& position,
		    const Range& value);

  virtual Bool degrid(const Array<Range>& gridded,
		      const Vector<Domain>& position,
		      Range& value);

  Vector<Double>& cFunction();

  Vector<Int>& cSupport();

  Int& cSampling();

protected:
  virtual Range correctionFactor1D(Int loc, Int len);

private:
  Vector<Double> convFunc;
  Vector<Int> supportVec;
  Vector<Int> loc;
  Int sampling;
  Int support;
  String cType;

public:
  using Gridder<Domain,Range>::onGrid;
protected:
  //# Make members of parent classes known.
  using Gridder<Domain,Range>::ndim;
  using Gridder<Domain,Range>::shape;
  using Gridder<Domain,Range>::scale;
  using Gridder<Domain,Range>::offset;
  using Gridder<Domain,Range>::posVec;
  using Gridder<Domain,Range>::locVec;
  using Gridder<Domain,Range>::shapeVec;
  using Gridder<Domain,Range>::zeroShapeVec;
  using Gridder<Domain,Range>::offsetVec;
  using Gridder<Domain,Range>::centerVec;
  using Gridder<Domain,Range>::fillCorrectionVectors;
};

} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Mathematics/ConvolveGridder.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
