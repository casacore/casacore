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
#include <trial/Lattices/LatticeRegion.h>

//# Forward Declarations
class CoordinateSystem;
class IPosition;
class LCRegion;
class LCSlicer;
class WCRegion;
class String;
class TableRecord;


// <summary>
// Class to hold a region of interest in an image.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=LCSlicer>LCSlicer</linkto>
//   <li> <linkto class=WCRegion>WCRegion</linkto>
// </prerequisite>

// <synopsis> 
// The only purpose of ImageRegion is to have a single object for
// the various kinds of regions
// <linkto class=LCSlicer>LCSlicer</linkto> and
// <linkto class=WCRegion>WCRegion</linkto>.
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
    // Construct from a slicer based on lattice coordinates.
    ImageRegion (const LCSlicer&);

    // Construct from a region based on world coordinates.
    ImageRegion (const WCRegion&);

    // Similar constructors as above, but using a pointer.
    // It takes over the pointer, so the user should not delete the
    // object. It is deleted by the ImageRegion destructor.
    // <group>
    ImageRegion (LCSlicer*);
    ImageRegion (WCRegion*);
    // </group>

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

    // Test if the underlying region is an WCRegion, etc..
    // <group>
    Bool isLCSlicer() const;
    Bool isWCRegion() const;
    // </group>

    // Get the region as an LCSlicer or WCRegion.
    // An exception is thrown if the region is not the correct type.
    // Functions <src>isWCRegion()</src>, etc. can be used to test the type.
    // <group>
    const LCSlicer& asLCSlicer() const;
    const WCRegion& asWCRegion() const;
    // </group>

    // Get the dimensionality.
    uInt ndim() const;

    // Convert to a LatticeRegion using the given coordinate system
    // (with reference pixel) and shape.
    // It will also make the region complete (absolute and non-fractional).
    LatticeRegion toLatticeRegion (const CoordinateSystem& cSys,
				   const IPosition& shape) const;

    // Convert to an LCRegion using the given coordinate system
    // (with reference pixel) and shape.
    // It will also make the region complete (absolute and non-fractional).
    // An exception is thrown if the region type is a LCSlicer.
    // The axes argument tells which axes to use from the coordinate
    // system and shape.
    LCRegion* toLCRegion (const CoordinateSystem& cSys,
			  const IPosition& shape) const;

    // Convert the (derived) object to a record.
    // The record can be used to make the object persistent.
    TableRecord toRecord (const String& tableName) const;

    // Convert correct object from a record.
    static ImageRegion* fromRecord (const TableRecord&,
				    const String& tableName);

private:
    LCSlicer*   itsSlicer;
    WCRegion*   itsWC;
    uInt        itsNdim;
};


inline Bool ImageRegion::isLCSlicer() const
{
    return ToBool (itsSlicer != 0);
}
inline Bool ImageRegion::isWCRegion() const
{
    return ToBool (itsWC != 0);
}
inline Bool ImageRegion::operator!= (const ImageRegion& other) const
{
    return ToBool (! ImageRegion::operator== (other));
}
inline uInt ImageRegion::ndim() const
{
    return itsNdim;
}


#endif
