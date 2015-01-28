//# WCDifference.cc: Make the difference of 2 image regions
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


#include <casacore/images/Regions/WCDifference.h>
#include <casacore/images/Regions/ImageRegion.h>
#include <casacore/lattices/LRegions/LCDifference.h>
#include <casacore/tables/Tables/TableRecord.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

WCDifference::WCDifference (const ImageRegion& region1,
			    const ImageRegion& region2)
: WCCompound (region1, region2)
{}

WCDifference::WCDifference (const PtrBlock<const ImageRegion*>& regions)
: WCCompound (regions)
{}

WCDifference::WCDifference (Bool takeOver, const PtrBlock<const WCRegion*>& regions)
: WCCompound (takeOver, regions)
{}

WCDifference::WCDifference (const WCDifference& other)
: WCCompound (other)
{}

WCDifference::~WCDifference()
{}

WCDifference& WCDifference::operator= (const WCDifference& other)
{
    if (this != &other) {
	WCCompound::operator= (other);
    }
    return *this;
}

Bool WCDifference::operator== (const WCRegion& other) const
{
   return WCCompound::operator== (other);
}

WCRegion* WCDifference::cloneRegion() const
{
    return new WCDifference (*this);
}

LCRegion* WCDifference::doToLCRegion (const CoordinateSystem& cSys,
				      const IPosition& shape,
				      const IPosition& pixelAxesMap,
				      const IPosition& outOrder) const
{
    PtrBlock<const LCRegion*> regions;
    multiToLCRegion (regions, cSys, shape, pixelAxesMap, outOrder);
    return new LCDifference (True, regions);
}

String WCDifference::className()
{
    return "WCDifference";
}

String WCDifference::type() const
{
   return className();
}

TableRecord WCDifference::toRecord (const String& tableName) const
{
    TableRecord rec;
    defineRecordFields (rec, className());
    rec.defineRecord ("regions", makeRecord(tableName));
    return rec;
}

WCDifference* WCDifference::fromRecord (const TableRecord& rec,
					const String& tableName)
{
    PtrBlock<const WCRegion*> regions;
    unmakeRecord (regions, rec.asRecord("regions"), tableName);
    return new WCDifference (True, regions);
}

} //# NAMESPACE CASACORE - END

