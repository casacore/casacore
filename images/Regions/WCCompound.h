//# WCCompound.h: Base class for compound WCRegion objects
//# Copyright (C) 1998,1999,2003
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

#ifndef IMAGES_WCCOMPOUND_H
#define IMAGES_WCCOMPOUND_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/images/Regions/WCRegion.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Arrays/IPosition.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class ImageRegion;
class LCRegion;
class CoordinateSystem;
class RecordInterface;
class TableRecord;
class String;


// <summary>
// Base class for compound WCRegion objects.
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=WCRegion>WCRegion</linkto>
// </prerequisite>

// <synopsis> 
// WCCompound is the base class for world coordinate regions.
// It defines the functionality simply as conversion to an LCRegion.
// This is because you need an LCRegion to be able to access the
// pixels in a Lattice.

// The conversion functions should be flexible in that the
// supplied CoordinateSystem does not have to be the same
// as that with which the derived class was constructed. 
// This means that you can apply a WCCompound from one image
// to another, provided that operation has some meaning.
// </synopsis> 

// <example>
// <srcblock>
// </srcblock>
// </example>

//# <todo asof="1997/11/11">
//# <li>
//# </todo>


class WCCompound : public WCRegion
{
public:
    // Construct from one or more image regions.
    // The image regions have to contain WCRegion objects, otherwise an
    // exception is thrown.
    // <group>
    WCCompound (const ImageRegion& region1, const ImageRegion& region2);
    WCCompound (const ImageRegion* region1,
		const ImageRegion* region2 = 0,
		const ImageRegion* region3 = 0,
		const ImageRegion* region4 = 0,
		const ImageRegion* region5 = 0,
		const ImageRegion* region6 = 0,
		const ImageRegion* region7 = 0,
		const ImageRegion* region8 = 0,
		const ImageRegion* region9 = 0,
		const ImageRegion* region10 = 0);
    WCCompound (const PtrBlock<const ImageRegion*>& regions);
    // </group>

    // Construct from multiple regions given as a Block.
    // When <src>takeOver</src> is True, the destructor will delete the
    // given regions. Otherwise a copy of the regions is made.
    WCCompound (Bool takeOver, const PtrBlock<const WCRegion*>& regions);

    // Copy constructor (copy semantics).
    WCCompound (const WCCompound& other);

    virtual ~WCCompound();

    // Comparison
    virtual Bool operator==(const WCRegion& other) const;

    // Get the contributing regions.
    const PtrBlock<const WCRegion*>& regions() const;
    
protected:
    // Assignment (copy semantics) makes only sense for a derived class.
    WCCompound& operator= (const WCCompound& other);

    // Convert each WCRegion to an LCRegion.
    // The axes argument tells which axes to use from the coordinate
    // system and shape.
    void multiToLCRegion (PtrBlock<const LCRegion*>& regions,
			  const CoordinateSystem& cSys,
			  const IPosition& shape,
			  const IPosition& pixelAxesMap,
			  const IPosition& extendAxes) const;

    // Store the contributing regions in a record.
    TableRecord makeRecord (const String& tableName) const;

    // Retrieve the contributing objects from the record.
    static void unmakeRecord (PtrBlock<const WCRegion*>&,
			      const TableRecord&,
			      const String& tableName);

private:
    // Check if the ImageRegion's contain WCRegion's and extract them.
    void makeWCRegion (const PtrBlock<const ImageRegion*>&);

    // Check if the regions are correct.
    // If needed, make a copy of the region objects.
    void init (Bool takeOver);

    //# Member variables.
    PtrBlock<const WCRegion*> itsRegions;
    Block<IPosition>          itsAxesUsed;
};


inline const PtrBlock<const WCRegion*>& WCCompound::regions() const
{
    return itsRegions;
}



} //# NAMESPACE CASACORE - END

#endif
