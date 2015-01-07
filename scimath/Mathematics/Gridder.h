//# Gridder.h: Definition for Gridder
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

#ifndef SCIMATH_GRIDDER_H
#define SCIMATH_GRIDDER_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class IPosition;

// <summary>
// A base class for gridding
// </summary>

template <class Domain, class Range>
class Gridder {
public:

  Gridder();

  Gridder(const IPosition& shape, const Vector<Domain>& scale,
	  const Vector<Domain>& offset);

  virtual ~Gridder();

  virtual Bool grid(Array<Range>&, const Vector<Domain>& position,
		    const Range& value) = 0;

  virtual Bool degrid(const Array<Range>&, const Vector<Domain>& position,
		      Range& value) = 0;

  virtual Range correct(const IPosition& loc);

  // Return a correction vector in x for loc y
  virtual void correctX1D(Vector<Range>& factor, const Int locy);

  Vector<Int>& location(Vector<Int>& loc, const Vector<Domain>& pos);

  Vector<Domain>& position(Vector<Domain>& gpos, const Vector<Domain>& pos);

  virtual Bool onGrid(const Vector<Int>& loc);

  virtual Bool onGrid(const Vector<Int>& loc, const Vector<Int>& delta);

  virtual Bool onGrid(const Vector<Domain>& pos);

  void setOffset(const Vector<Int>& off);

  void setOffset(const IPosition& off);

protected:

  Int nint(Double val) {return Int(std::floor(val+0.5));}

  virtual void fillCorrectionVectors();

  // Correction factor for 1 dimension. This is virtual and
  // must be assigned appropriately for derived classes
  virtual Range correctionFactor1D(Int loc, Int len) = 0;

  Int ndim;
  IPosition shape;		// Shape of array

  Vector<Domain> scale; 	// Scaling from world to pixel
  Vector<Domain> offset;	// Scaling from world to pixel

  Vector<Domain> posVec;	// Scaled location

  Vector<Int> locVec;   	// Vector for location type quantities
  Vector<Int> shapeVec;		// Vector for shape
  Vector<Int> zeroShapeVec;	// Vector for zero shape
  Vector<Int> offsetVec;	// Offset to be added to coordinates
  Vector<Int> centerVec;        // IPosition for center

  Vector <Vector<Range> > correctionVectors;

};

} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Mathematics/Gridder.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
