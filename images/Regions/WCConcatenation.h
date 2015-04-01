//# WCConcatenation.h: Combine multiple ImageRegion's into a new dimension
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

#ifndef IMAGES_WCCONCATENATION_H
#define IMAGES_WCCONCATENATION_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/images/Regions/WCCompound.h>
#include <casacore/images/Regions/WCBox.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Combine multiple ImageRegion's into a new dimension.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=WCCompound>WCCompound</linkto>
// </prerequisite>

// <synopsis> 
// The WCConcatenation class is a specialization of class
// <linkto class=WCCompound>WCCompound</linkto>.
// It makes it possible to combine multiple regions and to add a
// dimension on them. The axis and the range (beginning and end)
// of that new dimension have to be specified using an
// <linkto class=WCBox>WCBox</linkto> object.
// That axes should not be an axis in the given regions.
// <p>
// WCConcatenation can be seen as a mixture of the classes
// <linkto class=WCUnion>WCUnion</linkto> and
// <linkto class=WCExtension>WCExtension</linkto>. Like WCUnion it
// combines regions and like WCExtension it increases the dimensionality
// for the new region (be it with only 1).
// <br>
// Unlike WCUnion the axes have to be the same in all regions,
// because creating a WCConcatenation means combining similar regions.
// <p>
// E.g. One can define a different polygon in the RA-DEC plane of each
// channel. WCConcatenation makes it possible to combine the polygons
// to one 3D region in the RA-DEC-Freq cube.
// </synopsis> 

// <example>
// This example combines <src>n</src> (relative) circles 
// given in the RA,DEC plane along the FREQ-axis.
// In this example the regions used are circles with the same centers,
// but it is also  possible to combine differently shaped regions.
// Note that WCConcatenation takes over the pointers to the individual regions,
// so they do not need to be deleted (the WCConcatenation destructor does it).
// <srcblock>
// IPosition center (2,10,20);
// PtrBlock<ImageRegion*> cirPtr(n);
// for (i=0; i<n; i++) {
//   // Each circle has a different radius.
//   cirPtr(i) = new WCEllipsoid cir1 (center, 1 + i%(n/2));
// }
// // Construct the concatenation for a range (given as a box in fractions).
// // Extend along the FREQ-axis (the 2nd axis in the given cSys)..
// // Take over the region pointers.
// Vector<Quantity> blc(1);
// Vector<Quantity> trc(1);
// blc(0) = Quantity (0.25, "frac");
// trc(0) = Quantity (0.75, "frac");
// WCConcatenation region (True, cirPtr, WCBox(blc, trc, cSys, IPosition(1,2));
// </srcblock>
// This example is artificial in the sense that WCEllipsoid does not
// exist yet and the WCBox constructor looks a bit different.
// One should probably also do a bit more trouble to find out if FREQ
// is indeed the 2nd axis in the coordinate system.
// </example>

//# <todo asof="1997/11/11">
//# <li> 
//# </todo>


class WCConcatenation: public WCCompound
{
public:
    // Combine the given regions.
    // When <src>takeOver</src> is True, the destructor will delete the
    // given regions. Otherwise a copy of the regions is made.
    // The extend range has to be given as a 1-dimensional box.
    // <group>
    WCConcatenation (const PtrBlock<const ImageRegion*>& regions,
		     const WCBox& extendRange);
    WCConcatenation (Bool takeOver, const PtrBlock<const WCRegion*>& regions,
		     const WCBox& extendRange);
    // </group>

    // Copy constructor (copy semantics).
    WCConcatenation (const WCConcatenation& other);

    virtual ~WCConcatenation();

    // Assignment (copy semantics).
    WCConcatenation& operator= (const WCConcatenation& other);

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
    static WCConcatenation* fromRecord (const TableRecord&,
					const String& tableName);

protected:
    // Convert to an LCRegion using the given coordinate system and shape.
    // pixelAxesMap(i) gives the pixel axis in cSys of axes <src>i</src>
    // in the axesDesc.
    virtual LCRegion* doToLCRegion (const CoordinateSystem& cSys,
				    const IPosition& shape,
				    const IPosition& pixelAxesMap,
				    const IPosition& outOrder) const;

private:
    // Do a check and fill the remainder of the object.
    void fill();

    //# Variables
    WCBox itsExtendBox;
};



} //# NAMESPACE CASACORE - END

#endif
