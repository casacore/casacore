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
#include <trial/Lattices/RegionType.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>
#include <aips/Utilities/String.h>


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
        return itsWC->toRecord(tableName);
    } else {
        return itsLC->toRecord(tableName);
    }
}

ImageRegion ImageRegion::fromRecord (const TableRecord& record,
				     const String& tableName)
{

// See if this is a valid LC or WC region record


   if (!record.isDefined("isRegion")) {
      throw (AipsError ("ImageRegion::fromRecord - record does not define a region"));
   }
  
// Convert to correct region type

   Int regionType = record.asInt("isRegion");       
   ImageRegion* region = 0;
   if (regionType == Int(RegionType::WC)) {
      WCRegion* ptr = WCRegion::fromRecord (record, tableName);
      region = new ImageRegion(*ptr);
      delete ptr;
   } else if (regionType == Int(RegionType::LC)) {
       LCRegion* ptr = LCRegion::fromRecord (record, tableName);
       region = new ImageRegion(*ptr);
       delete ptr;
   } else {
      throw (AipsError ("ImageRegion::fromRecord - record is neither an LC nor WC region"));
   }

   ImageRegion tmp(*region);
   delete region;
   return tmp;
}
