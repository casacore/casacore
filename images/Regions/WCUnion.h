//# WCUnion.h: Make the union of 2 or more image regions
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

#ifndef IMAGES_WCUNION_H
#define IMAGES_WCUNION_H

//# Includes
#include <images/Regions/WCCompound.h>


namespace casa { //# NAMESPACE CASA - BEGIN

// <summary>
// Make the union of 2 or more image regions.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=WCCompound>WCCompound</linkto>
// </prerequisite>

// <synopsis> 
// The WCUnion class is a specialization of class
// <linkto class=WCCompound>WCCompound</linkto>.
// It makes it possible to take the union of 2 or more image regions.
// Note that only world coordinate regions can be used in a compound,
// thus an LCSlicer object is not allowed in a union.
// <p>
// The union of regions is the collection of the pixels masked-on
// in any region.
// Note that the bounding box of a union is determined by the outermost
// pixels. It means that if regions are very far apart, the bounding
// box of the union is very large and therefore inefficient.
// <p>
// The regions in a union can have different axes and dimensionalities.
// The axes and dimensionality of a union are determined by the
// collection of all different axes in its regions. Each individual region
// will be auto-extended along the axes not being part of the region.
// E.g. one can define a WCBox with axis RA and another WCBox with
// axis DEC. The union will be 2-dim with axes RA and DEC. The first
// box will be auto-extended to cover the DEC axis, which results
// in a 2-dim box with its DEC axis the length of the image's DEC axis.
// Similarly the second box will be auto-extended to cover the RA axis.
// </synopsis> 

// <example>
// <srcblock>
// </srcblock>
// </example>

//# <todo asof="1997/11/11">
//# <li>
//# </todo>


class WCUnion: public WCCompound
{
public:
    // Construct the union of one or more image regions.
    // The image regions have to contain WCRegion objects, otherwise an
    // exception is thrown.
    // <group>
    WCUnion (const ImageRegion& region1, const ImageRegion& region2);
    WCUnion (const ImageRegion* region1,
	     const ImageRegion* region2 = 0,
	     const ImageRegion* region3 = 0,
	     const ImageRegion* region4 = 0,
	     const ImageRegion* region5 = 0,
	     const ImageRegion* region6 = 0,
	     const ImageRegion* region7 = 0,
	     const ImageRegion* region8 = 0,
	     const ImageRegion* region9 = 0,
	     const ImageRegion* region10 = 0);
    WCUnion (const PtrBlock<const ImageRegion*>& regions);
    // </group>

    // Construct from multiple regions given as a Block.
    // When <src>takeOver</src> is True, the destructor will delete the
    // given regions. Otherwise a copy of the regions is made.
    WCUnion (Bool takeOver, const PtrBlock<const WCRegion*>& regions);

    // Copy constructor (copy semantics).
    WCUnion (const WCUnion& other);

    virtual ~WCUnion();

    // Assignment (copy semantics).
    WCUnion& operator= (const WCUnion& other);

    // Comparison
    virtual Bool operator== (const WCRegion& other) const;

    // Make a copy of the derived object.
    virtual WCRegion* cloneRegion() const;

    // Get the class name (to store in the record).
    static String className();

    // Get the region type.  Returns className()
    virtual String type() const;

    // Convert the (derived) object to a record.
    virtual TableRecord toRecord (const String& tableName) const;

    // Convert correct object from a record.
    static WCUnion* fromRecord (const TableRecord&,
				const String& tableName);

protected:
    // Convert to an LCRegion using the given coordinate system and shape.
    // pixelAxesMap(i) gives the pixel axis in cSys of axes <src>i</src>
    // in the axesDesc.
    virtual LCRegion* doToLCRegion (const CoordinateSystem& cSys,
				    const IPosition& shape,
				    const IPosition& pixelAxesMap,
				    const IPosition& outOrder) const;
};



} //# NAMESPACE CASA - END

#endif
