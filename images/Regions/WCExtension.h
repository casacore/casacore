//# WCExtension.h: Make the extension an image region
//# Copyright (C) 1998,2001
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

#ifndef IMAGES_WCEXTENSION_H
#define IMAGES_WCEXTENSION_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/images/Regions/WCCompound.h>
#include <casacore/casa/Arrays/IPosition.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations
class WCBox;


// <summary>
// Make the extension of an image region.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=WCCompound>WCCompound</linkto>
// </prerequisite>

// <synopsis> 
// The WCExtension class is a specialization of class
// <linkto class=WCCompound>WCCompound</linkto>.
// It makes it possible to extend a region along straight lines to
// other dimensions. It is also possible to extend existing axes with
// length 1, i.e. to stretch such axes.
// E.g. a circle in the RA,DEC plane can be extended to
// a cylinder in a RA,DEC,FREQ cube. It is possible to extend over
// more than one dimension. One can also limit the extension range
// E.g. in the forementioned example the circle can be extended
// for a given range of frequencies only.
// <br>The extension axes and ranges have to be given as a
// <linkto class=WCBox>WCBox</linkto> object. The axes which are part
// of the box and the region are the axes to be stretched. Box axes
// which are not part of the region are the extension axes.
// <p>
// Note that regions get automatically extended when a region is used
// for a higher dimensioned image. The extension is done for all
// unknown axes (for their entire length).
// </synopsis> 

// <example>
// <srcblock>
// </srcblock>
// </example>

//# <todo asof="1997/11/11">
//# <li>
//# </todo>


class WCExtension: public WCCompound
{
public:
    // Construct the extension of an image region using the axes
    // and blc,trc given in the extendBox.
    // The axes in region and box have to be disjoint.
    WCExtension (const ImageRegion& region, const WCBox& extendBox);

    // Copy constructor (copy semantics).
    WCExtension (const WCExtension& other);

    virtual ~WCExtension();

    // Assignment (copy semantics).
    WCExtension& operator= (const WCExtension& other);

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
    static WCExtension* fromRecord (const TableRecord&,
				    const String& tableName);

protected:
    // WCExtension can extend a region if WCBox can do so.
    virtual Bool canExtend() const;

    // Convert to an LCRegion using the given coordinate system and shape.
    // pixelAxesMap(i) gives the pixel axis in cSys of axes <src>i</src>
    // in the axesDesc.
    virtual LCRegion* doToLCRegion (const CoordinateSystem& cSys,
				    const IPosition& shape,
				    const IPosition& pixelAxesMap,
				    const IPosition& outOrder) const;

private:
    // Construct from multiple regions given as a Block.
    // When <src>takeOver</src> is True, the destructor will delete the
    // given regions. Otherwise a copy of the regions is made.
    WCExtension (Bool takeOver, const PtrBlock<const WCRegion*>& regions);

    // Find the axes to be extended and stretched.
    // The extend axes are the axis numbers in the box.
    // For the stretch axes both box and region axes are returned.
    void findAxes (IPosition& extendBoxAxes, IPosition& stretchBoxAxes,
		   IPosition& stretchRegionAxes) const;
};



} //# NAMESPACE CASACORE - END

#endif
