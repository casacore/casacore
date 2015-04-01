//# ExtendSpecifier.h: Specification of new and stretched lattice axes
//# Copyright (C) 2001
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

#ifndef CASA_EXTENDSPECIFIER_H
#define CASA_EXTENDSPECIFIER_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/IPosition.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class Slicer;
template<class T> class Block;

// <summary>
// Specification of new and stretched lattice axes
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tExtendSpecifier.cc" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="IPosition">IPosition</linkto>
// </prerequisite>

// <synopsis>
// ExtendSpecifier is a class internally used by class
// <linkto class=ExtendLattice>ExtendLattice</linkto>.
// It holds the information which axes are stretched and which axes
// are new. Note that a stretched axis has to have length 1 in the
// original shape.
// <p>
// The class only contains the functionality needed by ExtendLattice.
// which are (mainly) 2 conversion functions. One function converts
// a slicer from the extended lattice to the original lattice, so
// ExtendLattice can read the correct data.
// The other function converts a shape in the original lattice to the
// shape in the extended lattice.
// <br>Some data is precalculated for more efficient processing
// of the conversion of slicers and shapes.
// </synopsis>

// <example>
// <srcblock>
// IPosition oldShape(4,10,1,3,1);
// IPosition newShape(5,10,1,5,3,8);
// ExtendSpecifier spec (oldShape, newShape, IPosition(1,2), IPosition(1,4));
// </srcblock>
// This example extends the old shape to the new shape.
// <br>The 3rd argument tells that axes 2 is new. The newShape tells that
// its length will be 5. Note that adding this axis means that axes 2
// in the old shape will get axes 3 in the new shape.
// <br>The 4th argument tells that axes 4 (in the new shape!!) is stretched
// (to 8 according to newShape).
// </example>

//# <todo asof="yyyy/mm/dd">
//# </todo>

class ExtendSpecifier
{
public:
  // Default constructor generates empty IPositions.
  ExtendSpecifier();

  // Tell if no or all degenerate axes have to be removed.
  ExtendSpecifier (const IPosition& oldShape,
		   const IPosition& newShape,
		   const IPosition& newAxes,
		   const IPosition& stretchAxes);

  // Copy constructor (copy semantics).
  ExtendSpecifier(const ExtendSpecifier& other);
  
  ~ExtendSpecifier();

  // Assignment (copy semantics).
  // This and that do not have to have the same length.
  ExtendSpecifier& operator= (const ExtendSpecifier& other);

  // Return the new shape.
  const IPosition& newShape() const
    { return itsNewShape; }

  // Return the new axes.
  const IPosition& newAxes() const
    { return itsNewAxes; }

  // Return the axes to be stretched.
  const IPosition& stretchAxes() const
    { return itsStretchAxes; }

  // Return the old shape.
  const IPosition& oldShape() const
    { return itsOldShape; }

  // Return the axes to be extended (i.e. new and stretch axes).
  const IPosition& extendAxes() const
    { return itsExtendAxes; }

  // Return the old axes (i.e. axes new nor stretched) as in old shape.
  const IPosition& oldOldAxes() const
    { return itsOldOldAxes; }

  // Return the old axes as in new shape.
  const IPosition& oldNewAxes() const
    { return itsOldNewAxes; }

  // Convert the slicer to the specification for the old shape.
  // It fills <src>shape</src> with the shape to reform the section
  // length such that it contains the new axes.
  Slicer convert (IPosition& shape, const Slicer& section) const;

  // Convert a shape to the specification for the new shape.
  IPosition convertNew (const IPosition& oldShape) const;

private:
  // Fill the flags for the given axes.
  // It throws an exception if the axis is invalid or multiply given.
  void fill (Block<Bool>& flags, const IPosition& axes) const;


  IPosition itsOldShape;
  IPosition itsNewShape;
  IPosition itsNewAxes;
  IPosition itsStretchAxes;
  IPosition itsExtendAxes;
  IPosition itsOldOldAxes;
  IPosition itsOldNewAxes;
};



} //# NAMESPACE CASACORE - END

#endif
