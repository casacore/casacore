//# WCComplement.h: Make the complement of an image region
//# Copyright (C) 1998,2004
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

#ifndef IMAGES_WCCOMPLEMENT_H
#define IMAGES_WCCOMPLEMENT_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/images/Regions/WCCompound.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Make the complement of an image region.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=WCCompound>WCCompound</linkto>
// </prerequisite>

// <synopsis> 
// The WCComplement class is a specialization of class
// <linkto class=WCCompound>WCCompound</linkto>.
// It makes it possible to take the complement of the given region
// (which can be a simple WCBox, but also a complex compound region).
// Note that only world coordinate regions can be used in a compound,
// thus an LCSlicer object is not allowed in an intersection.
// <p>
// Note that a region consists of all its masked-on pixels inside the
// bounding box of the region. Thus the complement consists of all
// pixels outside the bounding box and all masked-off pixels inside
// the bounding box. So the complement of the complement of a region
// is the region itself.
// </synopsis> 

// <example>
// <srcblock>
// </srcblock>
// </example>

//# <todo asof="1997/11/11">
//# <li>
//# </todo>


class WCComplement: public WCCompound
{
public:
    WCComplement();

    // Construct the complement of the given region.
    WCComplement (const ImageRegion& region1);

    // Copy constructor (copy semantics).
    WCComplement (const WCComplement& other);

    virtual ~WCComplement();

    // Assignment (copy semantics).
    WCComplement& operator= (const WCComplement& other);

    // Comparison
    virtual Bool operator== (const WCRegion& other) const;

    // Make a copy of the derived object.
    // cloneRegion needs to return a WCRegion * because the
    // SGI compiler is smart enough to do the right thing.
    virtual WCRegion* cloneRegion() const;

    // Get the class name (to store in the record).
    static String className();

    // Get the region type.  Returns className()
    virtual String type() const;

    // Convert the (derived) object to a record.
    virtual TableRecord toRecord (const String& tableName) const;

    // Convert correct object from a record.
    static WCComplement* fromRecord (const TableRecord&,
				     const String& tableName);

    // Construct from multiple regions.
    // When <src>takeOver</src> is True, the destructor will delete the
    // given regions. Otherwise a copy of the regions is made.
    WCComplement (Bool takeOver,
		  const PtrBlock<const WCRegion*>& regions);

protected:
    // Convert to an LCRegion using the given coordinate system and shape.
    // pixelAxesMap(i) gives the pixel axis in cSys of axes <src>i</src>
    // in the axesDesc.
    virtual LCRegion* doToLCRegion (const CoordinateSystem& cSys,
				    const IPosition& shape,
				    const IPosition& pixelAxesMap,
				    const IPosition& outOrder) const;
};



} //# NAMESPACE CASACORE - END

#endif
