//# AxesSpecifier.h: Specification of axes to keep or remove
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
//#
//# $Id$

#ifndef CASA_AXESSPECIFIER_H
#define CASA_AXESSPECIFIER_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/AxesMapping.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations


// <summary>
// Specification of axes to keep or remove
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tAxesSpecifier.cc" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="IPosition">IPosition</linkto>
// </prerequisite>

// <synopsis>
// AxesSpecifier makes it possible to specify which axes should
// be used in a shape. Degenerate axes (i.e. axes with length 0)
// can be thrown away which makes it possible to reduce the
// dimensionality of an array. All degenerate axes can be thrown
// away, but one can also specify which ones should be kept.
// <p>
// Another option of this class is to reorder the axes, thus to
// make the axes of a lattice appear in a different order.
// This can be useful when two images with diferent axes orders
// have to be combined.
// <p>
// When an AxesSpecifier has to be used for a lattice, the lattice's
// shape has to be applied to the AxesSpecifier. The result is
// a <linkto class=AxesMapping>AxesMapping</linkto> object.
// This object is (for example) used internally in the
// <linkto class=SubLattice>SubLattice</linkto> class to know how
// to map the axes form the original lattice to the sublattice.
// <note role=caution>
// Reordering axes is not supported (yet) by the other Casacore classes
// like Lattices and Images.
// </note>
// </synopsis>

// <example>
// This example tells that all degenerate axes have to be kept.
// The axes are reordered to 1,0,2. Thus the first and second axes are
// swapped.
// <srcblock>
// AxesSpecifier spec(True, IPosition(3,1,0,2));
// AxesMapping map = spec.apply (IPosition(3,4,1,5));
// AlwaysAssertExit (map.posToNew (IPosition(3,2,0,3)) == IPosition(3,0,2,3));
// AlwaysAssertExit (map.posToOld (IPosition(3,0,2,3)) == IPosition(3,2,0,3));
//
// The following specification would have the same effect, because the
// unspecified axes are kept in their natural order.
// AxesSpecifier spec(True, IPosition(1,1));
// </srcblock>
//
// The same example as above, but now degenerated axes are removed.
// Note that because the second axis is removed, the third axis now
// get the second axis, thus gets swapped with the first axis.
// <br>Also note the difference between the functions <src>posToOld</src>
// and <src>shapeToOld</src>.
// <srcblock>
// AxesSpecifier spec(False, IPosition(1,1));
// AxesMapping map = spec.apply (IPosition(3,4,1,5));
// AlwaysAssertExit (map.posToNew (IPosition(3,2,0,3)) == IPosition(2,3,2));
// AlwaysAssertExit (map.posToOld (IPosition(3,3,2)) == IPosition(3,2,0,3);
// AlwaysAssertExit (map.shapeToOld (IPosition(3,3,2)) == IPosition(3,2,1,3);
// </srcblock>
// </example>

//# <todo asof="yyyy/mm/dd">
//# </todo>

class AxesSpecifier
{
public:
  // The default constructor keeps all axes.
  AxesSpecifier();

  // Tell if no or all degenerate axes have to be removed.
  explicit AxesSpecifier (Bool keepDegenerate);

  // Tell if no or all degenerate axes have to be removed.
  // <br>The argument <src>axisPath</src> makes it possible to specify in
  // which order the KEPT axes have to be used. Unspecified axes are
  // appended to the end. It gives a means to reorder the axes of a lattice.
  // <br>E.g. for a 4-dim lattice axisPath [2,0] means axis order [2,0,1,3].
  explicit AxesSpecifier (Bool keepDegenerate, const IPosition& axisPath);

  // Tell which (degenerate) axes have to be kept.
  // Non-degenerate axes will always be kept.
  explicit AxesSpecifier (const IPosition& keepAxes);

  // The argument <src>keepAxes</src> tells which degenerate axes have
  // to be kept. Non-degenerate axes will always be kept.
  // <br>The argument <src>axisPath</src> makes it possible to specify in
  // which order the KEPT axes have to be used. Unspecified axes are
  // appended to the end. It gives a means to reorder the axes of a lattice.
  // <br>E.g. for a 4-dim lattice axisPath [2,0] means axis order [2,0,1,3].
  AxesSpecifier (const IPosition& keepAxes, const IPosition& axisPath);

  // Copy constructor (copy semantics).
  AxesSpecifier(const AxesSpecifier& other);
  
  ~AxesSpecifier();

  // Assignment (copy semantics).
  // This and that do not have to have the same length.
  AxesSpecifier& operator= (const AxesSpecifier& other);

  // Apply the specification to a shape.
  // It returns an <linkto class=AxesMapping>AxesMapping</linkto>
  // object which takes care of mapping old to new axes order.
  AxesMapping apply (const IPosition& shape) const;

  // Are we keeping all degenerate axes ?
  Bool keep() const {return itsKeep;};

private:
  IPosition itsAxes;
  IPosition itsPath;
  Bool      itsKeep;
};



} //# NAMESPACE CASACORE - END

#endif
