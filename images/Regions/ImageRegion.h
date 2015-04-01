//# ImageRegion.h: Class to hold a region of interest in an image
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

#ifndef IMAGES_IMAGEREGION_H
#define IMAGES_IMAGEREGION_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/lattices/LRegions/LattRegionHolder.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

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
// the various kinds of regions. It can hold a
// <linkto class=LCRegion>LCRegion</linkto>,
// <linkto class=LCSlicer>LCSlicer</linkto>, and
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

class ImageRegion : public LattRegionHolder
{
public:
    // Default constructor (has no region at all).
    ImageRegion();

    // Construct from a region based on lattice coordinates.
    ImageRegion (const LCRegion&);

    // Construct from a slicer based on lattice coordinates.
    ImageRegion (const LCSlicer&);

    // Construct from a region based on world coordinates.
    ImageRegion (const WCRegion&);

    // Similar constructors as above, but using a pointer.
    // It takes over the pointer, so the user should not delete the
    // object. It is deleted by the ImageRegion destructor.
    // <group>
    explicit ImageRegion (LCRegion*);
    explicit ImageRegion (LCSlicer*);
    explicit ImageRegion (WCRegion*);
    // </group>

    // Copy constructor (copy semantics).
    ImageRegion (const ImageRegion& other);

    virtual ~ImageRegion();

    // Assignment (copy semantics).
    ImageRegion& operator= (const ImageRegion& other);

    // Clone the object.
    virtual ImageRegion* clone() const;

    // Comparison
    virtual Bool operator==(const LattRegionHolder& other) const;

    // Create an ImageRegion from a lattice expression. Returned pointer
    // is created via new(); it is the caller's responsibility to delete it.
    static ImageRegion* fromLatticeExpression(const String& latticeExpression);

    // Create an ImageRegion from a record. The returned pointer is created via
    // new(). It's the callers responsibility to delete it.
    // If a null pointer is passed in for <src>logger</src> no logging is done,
    // otherwise informational messages regarding bounding boxes are emitted
    // to the <src>logger</src> object.
    static ImageRegion* fromRecord (LogIO *logger,
                                    const CoordinateSystem& coords,
                                    const IPosition& imShape,
                                    const Record& regionRecord);

    // Test if the underlying region is an WCRegion.
    virtual Bool isWCRegion() const;

    // Get the region as a pointer to WCRegion.
    // An exception is thrown if the region is not the correct type.
    // Functions <src>isWCRegion()</src> can be used to test the type.
    virtual const WCRegion* asWCRegionPtr() const;

    // Get the region as an LCSlicer or WCRegion.
    // An exception is thrown if the region is not the correct type.
    // Functions <src>isWCRegion()</src>, etc. can be used to test the type.
    // <group>
    const LCRegion& asLCRegion() const;
    const LCSlicer& asLCSlicer() const;
    const WCRegion& asWCRegion() const;
    // </group>

    // Get the region as a writable mask.
    // It throws an exception if the region is not an LCRegion or if
    // its mask is not writable.
    LCRegion& asMask();

    // Convert to a LatticeRegion using the given coordinate system
    // (with reference pixel) and shape.
    // It will also make the region complete (absolute and non-fractional).
    virtual LatticeRegion toLatticeRegion (const CoordinateSystem& cSys,
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

    // Form a compound from this and the other region.
    // <group>
    virtual LattRegionHolder* makeUnion (const LattRegionHolder& other) const;
    virtual LattRegionHolder* makeIntersection
                                        (const LattRegionHolder& other) const;
    virtual LattRegionHolder* makeDifference
                                        (const LattRegionHolder& other) const;
    virtual LattRegionHolder* makeComplement() const;
    // </group>

private:
    WCRegion*   itsWC;
};


inline const LCRegion& ImageRegion::asLCRegion() const
{
    return *asLCRegionPtr();
}

inline const LCSlicer& ImageRegion::asLCSlicer() const
{
    return *asLCSlicerPtr();
}

inline const WCRegion& ImageRegion::asWCRegion() const
{
    return *asWCRegionPtr();
}



} //# NAMESPACE CASACORE - END

#endif
