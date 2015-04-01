//# WCIntersection.cc: Make the intersection of 2 or more image regions
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


#include <casacore/images/Regions/WCIntersection.h>
#include <casacore/images/Regions/ImageRegion.h>
#include <casacore/lattices/LRegions/LCIntersection.h>
#include <casacore/tables/Tables/TableRecord.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

WCIntersection::WCIntersection (const ImageRegion& region1,
				const ImageRegion& region2)
: WCCompound (region1, region2)
{}

WCIntersection::WCIntersection (const ImageRegion* region1,
				const ImageRegion* region2,
				const ImageRegion* region3,
				const ImageRegion* region4,
				const ImageRegion* region5,
				const ImageRegion* region6,
				const ImageRegion* region7,
				const ImageRegion* region8,
				const ImageRegion* region9,
				const ImageRegion* region10)
: WCCompound (region1, region2, region3, region4, region5,
	      region6, region7, region8, region9, region10)
{}

WCIntersection::WCIntersection (const PtrBlock<const ImageRegion*>& regions)
: WCCompound (regions)
{}

WCIntersection::WCIntersection (Bool takeOver,
				const PtrBlock<const WCRegion*>& regions)
: WCCompound (takeOver, regions)
{}

WCIntersection::WCIntersection (const WCIntersection& other)
: WCCompound (other)
{}

WCIntersection::~WCIntersection()
{}

WCIntersection& WCIntersection::operator= (const WCIntersection& other)
{
    if (this != &other) {
	WCCompound::operator= (other);
    }
    return *this;
}

Bool WCIntersection::operator== (const WCRegion& other) const
{
   return WCCompound::operator== (other);
}

WCRegion* WCIntersection::cloneRegion() const
{
    return new WCIntersection (*this);
}

LCRegion* WCIntersection::doToLCRegion (const CoordinateSystem& cSys,
					const IPosition& shape,
					const IPosition& pixelAxesMap,
					const IPosition& outOrder) const
{
    PtrBlock<const LCRegion*> regions;
    multiToLCRegion (regions, cSys, shape, pixelAxesMap, outOrder);
    return new LCIntersection (True, regions);
}

String WCIntersection::className()
{
    return "WCIntersection";
}

String WCIntersection::type() const
{
   return className();
}

TableRecord WCIntersection::toRecord (const String& tableName) const
{
    TableRecord rec;
    defineRecordFields (rec, className());
    rec.defineRecord ("regions", makeRecord(tableName));
    return rec;
}

WCIntersection* WCIntersection::fromRecord (const TableRecord& rec,
					    const String& tableName)
{
    PtrBlock<const WCRegion*> regions;
    unmakeRecord (regions, rec.asRecord("regions"), tableName);
    return new WCIntersection (True, regions);
}

} //# NAMESPACE CASACORE - END

