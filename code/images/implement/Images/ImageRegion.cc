//# ImageRegion.cc: Class to hold a region of interest in an image
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

#include <trial/Images/ImageRegion.h>
#include <trial/Images/WCRegion.h>
#include <trial/Lattices/LCSlicer.h>
#include <trial/Lattices/LCExtension.h>
#include <trial/Lattices/RegionType.h>
#include <trial/Coordinates/CoordinateSystem.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Arrays/Vector.h>
#include <aips/Utilities/String.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>

typedef Vector<Int> imageregion_gppbug1;
typedef Vector<Double> imageregion_gppbug2;


ImageRegion::ImageRegion (const LCSlicer& slicer)
: itsSlicer (new LCSlicer(slicer)),
  itsWC     (0),
  itsNdim   (slicer.ndim())
{}

ImageRegion::ImageRegion (const WCRegion& region)
: itsSlicer (0),
  itsWC     (region.cloneRegion()),
  itsNdim   (region.ndim())
{}

ImageRegion::ImageRegion (LCSlicer* slicer)
: itsSlicer (slicer),
  itsWC     (0),
  itsNdim   (slicer->ndim())
{}

ImageRegion::ImageRegion (WCRegion* region)
: itsSlicer (0),
  itsWC     (region),
  itsNdim   (region->ndim())
{}

ImageRegion::ImageRegion (const ImageRegion& other)
: itsSlicer (0),
  itsWC     (0)
{
    operator= (other);
}

ImageRegion::~ImageRegion()
{
    delete itsSlicer;
    delete itsWC;
}

ImageRegion& ImageRegion::operator= (const ImageRegion& other)
{
    if (this != &other) {
	delete itsSlicer;
        delete itsWC;
	itsSlicer = other.itsSlicer;
	itsWC = other.itsWC;
	itsNdim = other.itsNdim;
	if (itsSlicer != 0) {
	    itsSlicer = new LCSlicer(*itsSlicer);
	}
	if (itsWC != 0) {
	    itsWC = itsWC->cloneRegion();
	}
    }
    return *this;
}

Bool ImageRegion::operator== (const ImageRegion& other) const
{
    if (isWCRegion()   != other.isWCRegion()
    ||  isLCSlicer()   != other.isLCSlicer()) {
	return False;
    }
    Bool match;
    if (isLCSlicer()) {
	match = (*itsSlicer == other.asLCSlicer());
    } else {
	match = (*itsWC == other.asWCRegion());
    }
    return match;
}

const LCSlicer& ImageRegion::asLCSlicer() const
{
    AlwaysAssert (isLCSlicer(), AipsError);
    return *itsSlicer;
}

const WCRegion& ImageRegion::asWCRegion() const
{
    AlwaysAssert (isWCRegion(), AipsError);
    return *itsWC;
}

LatticeRegion ImageRegion::toLatticeRegion (const CoordinateSystem& cSys,
					    const IPosition& shape) const
{
    if (isLCSlicer()) {
	return LatticeRegion (itsSlicer->toSlicer (cSys.referencePixel(),
						   shape),
			      shape);
    }
    // LatticeRegion takes over the LCRegion pointer,
    // so it does not need to be deleted.
    // This is the top conversion, so use all axes.
    return LatticeRegion (toLCRegion (cSys, shape));
}

LCRegion* ImageRegion::toLCRegion (const CoordinateSystem& cSys,
				   const IPosition& shape) const
{
    // Convert the region to an LCRegion.
    LCRegion* region = 0;
    if (isWCRegion()) {
        region = itsWC->toLCRegion (cSys, shape);
    } else {
	throw (AipsError ("ImageRegion::toLCRegion - "
			  " cannot convert its LCSlicer object to LCRegion"));
    }
    return region;
}

TableRecord ImageRegion::toRecord (const String& tableName) const
{
    TableRecord record;
    if (isWCRegion()) {
        return itsWC->toRecord (tableName);
    }
    return itsSlicer->toRecord (tableName);
}

ImageRegion* ImageRegion::fromRecord (const TableRecord& record,
				      const String& tableName)
{
    // See if this is a region record.
    if (! record.isDefined ("isRegion")) {
	throw (AipsError ("ImageRegion::fromRecord - "
			  "record does not define a region"));
    }
    // Convert to correct region object.
    // Note that in the following the ImageRegion constructors take
    // over the pointer returned by fromRecord.
    Int regionType = record.asInt ("isRegion");       
    if (regionType == RegionType::WC) {
	return new ImageRegion (WCRegion::fromRecord (record, tableName));
    } else if (regionType != RegionType::Slicer) {
	throw (AipsError ("ImageRegion::fromRecord - "
			  "record has an unknown region type"));
    }
    return new ImageRegion (LCSlicer::fromRecord (record, tableName));
}
