//# LatticeCoordinates.h: Envelope class for Lattice coordinates
//# Copyright (C) 1998
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

#if !defined(AIPS_LATTICECOORDINATES_H)
#define AIPS_LATTICECOORDINATES_H


//# Includes
#include <aips/aips.h>
#include <trial/Lattices/LattCoord.h>
#include <aips/Utilities/CountedPtr.h>


// <summary>
// Envelope class for Lattice coordinates.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
// </prerequisite>

// <synopsis> 
// </synopsis>

// <example>
// <srcblock>
// </srcblock>
// </example>

// <motivation>
// </motivation>

// <todo asof="1995/09/12">
//  <li>
// </todo>


class LatticeCoordinates
{
public:
    // The default constructor creates a LattCoord object.
    LatticeCoordinates();

    // Construct the object from the given letter class.
    // It takes over the pointer and takes care of destructing
    // the LattCoord object.
    LatticeCoordinates (LattCoord* coordinates);

    // Copy constructor (reference semantics).
    LatticeCoordinates (const LatticeCoordinates& that);

    ~LatticeCoordinates();

    // Assignment (reference semantics).
    LatticeCoordinates& operator= (const LatticeCoordinates& that);

    // Does the class have true coordinates?
    Bool hasCoordinates() const;

    // Check if the coordinates of this and that conform.
    // <br>The default implementation returns False.
    Bool conform (const LatticeCoordinates& that) const;

    // Return the underlying letter object.
    // This should in general not be used, but for specific (Image) cases
    // it might be needed.
    const LattCoord& coordinates() const;

private:
    // The pointer to the underlying object.
    CountedPtr<LattCoord> coords_p;
};


#endif
