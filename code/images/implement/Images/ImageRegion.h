//# ImageRegion.h: Class to hold a region of interest in an image
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

#if !defined(AIPS_IMAGEREGION_H)
#define AIPS_IMAGEREGION_H

//# Includes
#include <aips/aips.h>

//# Forward Declarations
class CoordinateSystem;
class IPosition;
class LCRegion;
class String;
class TableRecord;
class WCRegion;


// <summary>
// Class to hold a region of interest in an image.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Slicer>Slicer</linkto>
// </prerequisite>

// <synopsis> 
// The only purpose of ImageRegion is to have a single object for
// the various kinds of regions (e.g. LCRegion and WCRegion).
// So far, it is only used to hold an LCRegion or WCRegion object.
// In the future it can also be useful to hold other types of regions.
// </synopsis> 

// <example>
// <srcblock>
// </srcblock>
// </example>

// <motivation>
// It was felt that making an abstract base class LatticeRegion for
// LCRegion and WCRegion would create undesirable dependencies of
// module Lattices on module Coordinates. E.g. it would be impossible
// to have a function toWCRegion.
// Therefore the container class ImageRegion is chosen.
// </motivation>

//# <todo asof="1997/11/11">
//# <li>
//# </todo>

class ImageRegion
{
public:
    // Construct from a region based on lattice coordinates.
    ImageRegion (const LCRegion&);

    // Construct from a region based on world coordinates.
    ImageRegion (const WCRegion&);

    // Copy constructor (copy semantics).
    ImageRegion (const ImageRegion& other);

    ~ImageRegion();

    // Assignment (copy semantics).
    ImageRegion& operator= (const ImageRegion& other);

    // Comparison
    // <group>
    Bool operator==(const ImageRegion& other) const;
    Bool operator!=(const ImageRegion& other) const;
    // </group>

    // Test if the underlying region is a WCRegion.
    Bool isWCRegion() const;

    // Get the region as an LCRegion or WCRegion.
    // An exception is thrown if the region is not the correct type.
    // Function <src>isWCRegion()</src> can be used to test the type.
    // <group>
    const LCRegion& asLCRegion() const;
    const WCRegion& asWCRegion() const;
    // </group>

    // Convert to an LCRegion using the given coordinate system.
    const LCRegion& toLCRegion (const CoordinateSystem& cSys,
                                const IPosition& shape) const;

    // Convert the (derived) object to a record.
    // The record can be used to make the object persistent.
    TableRecord toRecord (const String& tableName) const;

    // Convert correct object from a record.
    static ImageRegion fromRecord (const TableRecord&,
				   const String& tableName);

private:
    LCRegion* itsLC;
    WCRegion* itsWC;
};


inline Bool ImageRegion::isWCRegion() const
{
    return ToBool (itsWC != 0);
}


#endif
