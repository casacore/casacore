//# WCRegion.h: Class to define a region of interest in an image
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

#if !defined(AIPS_WCREGION_H)
#define AIPS_WCREGION_H

//# Includes
#include <aips/aips.h>

//# Forward Declarations
class LCRegion;
class CoordinateSystem;
class TableRecord;
class IPosition;


// <summary>
// Base class to define world coordinate regions of interest in an image.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class=Slicer>Slicer</linkto>
// </prerequisite>
//
// <synopsis> 
// WCRegion is the base class for world coordinate regions.
// It defines the functionality simply as conversion to an LCRegion.
// This is because you need an LCRegion to be able to access the
// pixels in a Lattice.
//
// The conversion functions should be flexible in that the
// supplied CoordinateSystem does not have to be the same
// as that with which the derived class was constructed. 
// This means that you can apply a WCRegion from one image
// to another, provided that operation has some meaning.
// </synopsis> 
//
// <example>
// <srcblock>
// </srcblock>
// </example>
//
// <motivation>
// User should be able to specify their regions in world coordinates
// as well as lattice coordinates.
// </motivation>
//
//# <todo asof="1997/11/11">
//# <li>
//# </todo>

class WCRegion
{
public:
    WCRegion();

    // Copy constructor (copy semantics).
    WCRegion (const WCRegion& other);

    virtual ~WCRegion();

    // Clone a WCRegion object.
    virtual WCRegion* cloneRegion() const = 0;

    // Convert to an LCRegion using the given coordinate system and shape
    virtual LCRegion* toLCRegion (const CoordinateSystem& cSys,
                                  const IPosition& shape) const = 0;

    // Convert the (derived) object to a record.
    // The record can be used to make the object persistent.
    virtual TableRecord toRecord() const = 0;

    // Convert correct object from a record.
    static WCRegion* fromRecord (const TableRecord&);

protected:
    // Assignment (copy semantics) makes only sense for a derived class.
    WCRegion& operator= (const WCRegion& other);
};


inline WCRegion::WCRegion()
{}

inline WCRegion::WCRegion (const WCRegion&)
{}

inline WCRegion& WCRegion::operator= (const WCRegion&)
{
    return *this;
}


#endif
