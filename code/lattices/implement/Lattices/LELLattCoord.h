//# LattCoord.h: The base letter class for lattice coordinates
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

#if !defined(AIPS_LATTCOORD_H)
#define AIPS_LATTCOORD_H

//# Includes
#include <aips/aips.h>
#include <aips/Utilities/String.h>

//# Forward Declarations
class ImageCoord;


// <summary>
// The base letter class for lattice coordinates.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> Abstract Base class Inheritance - try "Advanced C++" by James
//        O. Coplien, Ch. 5.
// </prerequisite>

// <synopsis>
// This base class is the basic letter for the envelope class
// <linkto class=LatticeCoordinates>LatticeCoordinates</linkto>.
// It does not do anything, but makes it possible that derived classes
// (like ImageCoord) implement their own behaviour.
// </synopsis> 

// <example>
// <srcblock>
// </srcblock>
// </example>

// <motivation>
// It must be possible to handle image coordinates in a lattice
// expression.
// </motivation>

//# <todo asof="1996/07/01">
//#   <li>
//# </todo>


class LattCoord
{
public:
    LattCoord();

    // A virtual destructor is needed so that it will use the actual destructor
    // in the derived class.
    virtual ~LattCoord();

    // Does the class have true coordinates?
    // <br>The default implementation returns False.
    virtual Bool hasCoordinates() const;

    // The name of the class.
    virtual String classname() const;

    // Check if the coordinates of this and that conform.
    // <br>The default implementation returns False.
    virtual Bool conform (const LattCoord& other) const;

    // Check if the coordinates of this and that image conform.
    // <br>The default implementation returns False.
    virtual Bool doConform (const ImageCoord& other) const;
};


#endif

