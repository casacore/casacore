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
#include <trial/Lattices/LCRegion.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>


ImageRegion::ImageRegion (const LCRegion& region)
: itsLC (region.cloneRegion()),
  itsWC (0)
{}

ImageRegion::ImageRegion (const WCRegion& region)
: itsLC (0),
  itsWC (region.cloneRegion())
{}

ImageRegion::ImageRegion (const ImageRegion& other)
: itsLC (0),
  itsWC (0)
{
    operator= (other);
}

ImageRegion::~ImageRegion()
{
    delete itsLC;
    delete itsWC;
}

ImageRegion& ImageRegion::operator= (const ImageRegion& other)
{
    if (this != &other) {
        delete itsLC;
        delete itsWC;
	itsLC = 0;
	itsWC = other.itsWC;
	if (isWCRegion()) {
	    itsWC = itsWC->cloneRegion();
	} else {
	    itsLC = other.itsLC->cloneRegion();
	}
    }
    return *this;
}

const LCRegion& ImageRegion::asLCRegion() const
{
    AlwaysAssert (! isWCRegion(), AipsError);
    return *itsLC;
}
const WCRegion& ImageRegion::asWCRegion() const
{
    AlwaysAssert (isWCRegion(), AipsError);
    return *itsWC;
}

const LCRegion& ImageRegion::toLCRegion (const CoordinateSystem& cSys,
                                         const IPosition& shape) const
{
    if (isWCRegion()) {
        ImageRegion* This = (ImageRegion*)this;
        This->itsLC = itsWC->toLCRegion (cSys, shape);
    }
    return *itsLC;
}

TableRecord ImageRegion::toRecord (const String& tableName) const
{
    TableRecord record;
    if (isWCRegion()) {
        record.defineRecord ("WC", itsWC->toRecord(tableName));
    } else {
        record.defineRecord ("LC", itsLC->toRecord(tableName));
    }
    return record;
}

ImageRegion ImageRegion::fromRecord (const TableRecord& record,
				     const String& tableName)
{
    ImageRegion* region;
    if (record.isDefined ("WC")) {
        WCRegion* ptr = WCRegion::fromRecord (record.asRecord("WC"),
                                              tableName);
	region = new ImageRegion(*ptr);
	delete ptr;
    } else {
        LCRegion* ptr = LCRegion::fromRecord (record.asRecord("LC"),
					      tableName);
	region = new ImageRegion(*ptr);
	delete ptr;
    }
    ImageRegion tmp(*region);
    delete region;
    return tmp;
}
