//# LELLattCoord.h: The base letter class for lattice coordinates in LEL
//# Copyright (C) 1998,1999,2000,2001
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

#ifndef LATTICES_LELLATTCOORD_H
#define LATTICES_LELLATTCOORD_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/lattices/LEL/LELLattCoordBase.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class LatticeExprNode;
class LattRegionHolder;
class IPosition;


// <summary>
// The base letter class for lattice coordinates in LEL.
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="Lattice"> Lattice</linkto>
//   <li> <linkto class="LELLattCoordBase"> LELLattCoordBase</linkto>
// </prerequisite>

// <synopsis>
// This class is a letter class for the envelope class
// <linkto class=LELCoordinates>LELCoordinates</linkto>.
// It acts as the coordinates class for Lattice objects without
// coordinates (like PagedArray).
//
// It does not do anything, but makes it possible that other classes
// (like <linkto class=LELImageCoord>LELImageCoord</linkto>)
// implement their own behaviour.
// </synopsis> 

// <motivation>
// It must be possible to handle image coordinates in a lattice
// expression.   
// </motivation>

//# <todo asof="1998/01/31">
//#  <li>
//# </todo>


class LELLattCoord : public LELLattCoordBase
{
public:
  LELLattCoord();

  // A virtual destructor is needed so that it will use the actual
  // destructor in the derived class.
  virtual ~LELLattCoord();

  // The class does not have true coordinates.
  virtual Bool hasCoordinates() const;

  // Create a SubLattice for an expression node.
  virtual LatticeExprNode makeSubLattice
                                    (const LatticeExprNode& expr,
				     const LattRegionHolder& region) const;

  // Create an extension for an expression node.
  virtual LatticeExprNode makeExtendLattice
                                    (const LatticeExprNode& expr,
				     const IPosition& newShape,
				     const LELLattCoordBase& newCoord) const;

  // Create a rebinning for an expression node.
  virtual LatticeExprNode makeRebinLattice
                                    (const LatticeExprNode& expr,
				     const IPosition& binning) const;

  // Get the coordinates of the spectral axis for the given shape.
  // This function throws an exception as a Lattice has no coordinates.
  virtual uInt getSpectralInfo (Vector<Double>& worldCoordinates,
				const IPosition& shape) const;

  // The name of the class.
  virtual String classname() const;

  // Check how the coordinates of this and that compare.
  virtual Int compare (const LELLattCoordBase& other) const;

  // Check how the coordinates of this and that image compare.
  // This function is used by <src>conform</src> to make a
  // double virtual dispatch possible.
  virtual Int doCompare (const LELImageCoord& other) const;
};



} //# NAMESPACE CASACORE - END

#endif

