//# LELImageCoord.h: The letter class for image coordinates
//# Copyright (C) 1998,1999
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

#if !defined(AIPS_LELIMAGECOORD_H)
#define AIPS_LELIMAGECOORD_H

//# Includes
#include <trial/Lattices/LELLattCoord.h>
#include <trial/Coordinates/CoordinateSystem.h>
#include <aips/Utilities/CountedPtr.h>


// <summary>
// The letter class for image coordinates.
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=LELLattCoord>LELLattCoord</linkto>
// </prerequisite>

// <synopsis>
// This base class is the basic letter for the envelope class
// <linkto class=LELCoordinates>LELCoordinates</linkto>.
// It does not do anything, but makes it possible that derived classes
// (like LELImageCoord) implement their own behaviour.
// </synopsis> 

// <example>
// <srcblock>
// </srcblock>
// </example>

// <motivation>
// It must be possible to handle image coordinates in a lattice.
// expression.
// </motivation>

//# <todo asof="1996/07/01">
//#   <li>
//# </todo>


class LELImageCoord : public LELLattCoord
{
public:
    LELImageCoord();

    LELImageCoord (const CoordinateSystem& coords);

    virtual ~LELImageCoord();

    // Get the coordinates.
    const CoordinateSystem& coordinates() const;

    // Create a SubLattice for an expression node.
    virtual LatticeExprNode makeSubLattice
                                    (const LatticeExprNode& expr,
				     const LattRegionHolder& region) const;

    // The class has true coordinates (thus returns True).
    virtual Bool hasCoordinates() const;

    // The name of the class.
    virtual String classname() const;

    // Check if the coordinates of this and that conform.
    // It calls doConform on the that object.
    virtual Bool conform (const LELLattCoord& other) const;

    // Check if the coordinates of this and that conform.
    virtual Bool doConform (const LELImageCoord& other) const;

private:
    CountedPtr<CoordinateSystem> coords_p;
};


#endif

