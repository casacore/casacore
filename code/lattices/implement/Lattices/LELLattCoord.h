//# LELLattCoord.h: The base letter class for lattice coordinates in LEL
//# Copyright (C) 1998,1999,2000
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

#if !defined(AIPS_LELLATTCOORD_H)
#define AIPS_LELLATTCOORD_H


//# Includes
#include <aips/Lattices/LELLattCoordBase.h>
#include <aips/Utilities/String.h>

//# Forward Declarations
class LatticeExprNode;
class LattRegionHolder;


// <summary>
// The base letter class for lattice coordinates in LEL.
// </summary>
//
// <use visibility=export>
//
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class="Lattice"> Lattice</linkto>
//   <li> <linkto class="LELCoordinates"> LELCoordinates</linkto>
// </prerequisite>
//
// <synopsis>
// This base class is the basic letter for the envelope class
// <linkto class=LELCoordinates>LELCoordinates</linkto>.
// It does not do anything, but makes it possible that derived classes
// (like LELImageCoord) implement their own behaviour.  LELLattCoord is
// not an abstract base class.  Thus it can be constructed and
// used to return null LELCoordinate objects for Lattices that 
// don't have Coordinates (e.g. ArrayLattice, PagedArray)
// </synopsis> 
//
// <example>
// <srcblock>
// </srcblock>
// </example>
//
// <motivation>
// It must be possible to handle image coordinates in a lattice
// expression.   
// </motivation>
//
// <todo asof="1998/01/31">
// </todo>


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

    // The name of the class.
    virtual String classname() const;

    // The coordinates of this and that conform.
    virtual Bool conform (const LELLattCoordBase& other) const;

    // The coordinates of this and that image conform.
    virtual Bool doConform (const LELImageCoord& other) const;
};


#endif

