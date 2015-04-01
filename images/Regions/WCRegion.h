//# WCRegion.h: Class to define a region of interest in an image
//# Copyright (C) 1998,2000,2001
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

#ifndef IMAGES_WCREGION_H
#define IMAGES_WCREGION_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/coordinates/Coordinates/CoordinateSystem.h>
#include <casacore/tables/Tables/TableRecord.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class LCRegion;
class RecordInterface;
class IPosition;
class String;


// <summary>
// Base class to define world coordinate regions of interest in an image.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class=LCRegion>LCRegion</linkto>
// </prerequisite>
//
// <synopsis> 
// WCRegion is the base class for world coordinate regions.
// The axes in a WCRegion have names (e.g. RA, DEC, FREQ) and
// carry sometimes an associated reference frame with it.
// An WCRegion object is converted to the appropriate
// <linkto class=LCRegion>LCRegion</linkto> object when they
// are used to take a subset from an image.
// LCRegion's are pixel based and are
// used to access the correct pixels in the image.
// The conversion has the following rules:
// <ol>
//  <li> All axes of the region must be axes in the image.
//  <li> An image axis does not have to be an axis in the region.
//   Thus the image can have a higher dimensionality than the region.
//   If that is the case, the region is auto-extended to the image's
//   dimensionality by using the full range for those axes.
//  <li> The order of the axes in region and image do not have to
//   be the same. They get reordered as needed.
// </ol>
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

    // Destructor
    virtual ~WCRegion();

    // Comparison
    // <group>
    virtual Bool operator==(const WCRegion& other) const;
    Bool operator!=(const WCRegion& other) const;
    // </group>

    // Clone a WCRegion object.
    virtual WCRegion* cloneRegion() const = 0;

    // Return region type.
    // Just returns the class name of the derived class.
    virtual String type() const = 0;

    // Get the dimensionality (i.e. the number of axes).
    // Note that usually all axes have a description, but in some cases
    // (e.g. WCLELMask) that may not be the case.
    // The default implementation returns the number of axes in the
    // axes description.
    virtual uInt ndim() const;

    // Get the description of all axes.
    const Record& getAxesDesc() const;

    // Get the description of the given axis.
    // It is a record containing some fields describing the axis.
    const Record& getAxisDesc (uInt axis) const;

    // Return the axis number of the description of an axis in the full
    // axes description.
    // -1 is returned if not found.
    Int axisNr (const Record& desc, const Record& axesDesc) const;

    // Are both axis descriptions equal?
    Bool isAxisDescEqual (const Record& desc1, const Record& desc2) const;

    // Can the region extend itself?
    // By default it cannot.
    virtual Bool canExtend() const;

    // Get or set the comment.
    // <group>
    const String& comment() const;
    void setComment (const String& comment);
    // </group>

    // Convert to an LCRegion using the given new coordinate system and shape.
    // An exception is thrown if the region's dimensionality is more
    // than the length of the shape vector or if an axis in the region
    // is unknown in the new coordinate system..
    // When less, the default implementation extends the region over the
    // remaining axes.
    // <br>If the region does not need to have coordinates (like WCLELMask)
    // the function has to be overridden.
    virtual LCRegion* toLCRegion (const CoordinateSystem& cSys,
				  const IPosition& shape) const;

    // Convert to an LCRegion using the given coordinate system and shape.
    // This function is meant for internal use by WCCompound objects.
    // <br>pixelAxesMap(i) is the axis in cSys and shape for region axis i.
    // <br>outOrder(i) is the axis in the output LCRegion for region axis i.
    // <br>The length of pixelAxesMap and outOrder is the dimensionality of
    // the output LCRegion. It can be more than the dimensionality of this
    // WCRegion object. In that case the region gets extended along the
    // latter axes. If the region cannot extend itself, this function
    // will create an LCExtension object to extend the region.
    // <br>Note that initially pixelAxisMap and outOrder are the same,
    // but when called for regions in compound regions they may start
    // to differ. 
    LCRegion* toLCRegionAxes (const CoordinateSystem& cSys,
			      const IPosition& shape,
			      const IPosition& pixelAxesMap,
			      const IPosition& outOrder) const;

    // Convert the (derived) object to a record.
    // The record can be used to make the object persistent.
    // The <src>tableName</src> argument can be used by derived
    // classes (e.g. LCPagedMask) to put very large objects.
    virtual TableRecord toRecord(const String& tableName) const = 0;

    // Convert correct object from a record.
    static WCRegion* fromRecord (const TableRecord& rec,
                                 const String& tableName);

    // Define the type and class name in the record.
    void defineRecordFields (RecordInterface& record,
                             const String& className) const;

protected:
    // Assignment (copy semantics) makes only sense for a derived class.
    WCRegion& operator= (const WCRegion& other);

    // Add an axis with its description.
    // An exception is thrown if the axis already exists in this region.
    void addAxisDesc (const Record& axisDesc);

    // Make a description of a pixel axis in the coordinate system.
    Record makeAxisDesc (const CoordinateSystem& cSys, uInt pixelAxis) const;

    // Make a description of all pixel axes in the coordinate system
    // (in pixel axes order).
    Record makeAxesDesc (const CoordinateSystem& cSys) const;

    // Convert to an LCRegion using the given coordinate system and shape.
    // <br>pixelAxesMap(i) is the axis in cSys and shape for region axis i.
    // <br>outOrder(i) is the axis in the output LCRegion for region axis i.
    // <br>They always have the same length.
    // If the region can extend itself, the length of pixelAxesMap and
    // outOrder can be more than the dimensionality of the region.
    // The latter axes in them are the extension axes.
    virtual LCRegion* doToLCRegion (const CoordinateSystem& cSys,
				    const IPosition& shape,
				    const IPosition& pixelAxesMap,
				    const IPosition& extendAxes) const = 0;

// Convert relative to absolute world as needed
    void makeWorldAbsolute (Vector<Double>& world,
                            const Vector<Int>& absRel,
                            const CoordinateSystem& cSys,
                            const IPosition& shape) const;

    static void unitInit();

    void checkAxes (
    	const IPosition& pixelAxes,
        const CoordinateSystem& cSys,
        const Vector<String>& quantityUnits
    ) const;

	static void convertPixel(
		Double& pixel,
	    const Double& value,
	    const String& unit,
	    const Int absRel,
	    const Double refPix,
	    const Int shape
	);
private:
    String itsComment;
    Record itsAxesDesc;
};


inline Bool WCRegion::operator!= (const WCRegion& other) const
{
   return (!operator==(other));
}
inline const String& WCRegion::comment() const
{
    return itsComment;
}
inline void WCRegion::setComment (const String& comment)
{
    itsComment = comment;
}
inline const Record& WCRegion::getAxesDesc() const
{
    return itsAxesDesc;
}



} //# NAMESPACE CASACORE - END

#endif
