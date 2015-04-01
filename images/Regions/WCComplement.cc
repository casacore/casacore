//# WCComplement.cc: Make the complement of an image region
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


#include <casacore/images/Regions/WCComplement.h>
#include <casacore/images/Regions/ImageRegion.h>
#include <casacore/lattices/LRegions/LCComplement.h>
#include <casacore/tables/Tables/TableRecord.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

WCComplement::WCComplement (const ImageRegion& region)
: WCCompound (&region)
{}

WCComplement::WCComplement (Bool takeOver,
			    const PtrBlock<const WCRegion*>& regions)
: WCCompound (takeOver, regions)
{}

WCComplement::WCComplement (const WCComplement& other)
: WCCompound (other)
{}

WCComplement::~WCComplement()
{}

WCComplement& WCComplement::operator= (const WCComplement& other)
{
    if (this != &other) {
	WCCompound::operator= (other);
    }
    return *this;
}

Bool WCComplement::operator== (const WCRegion& other) const
{
   return WCCompound::operator== (other);
}

  //Clone needs to be a WCRegion cause the SGI compiler is 
  //not smart enough to do the right thing.
WCRegion* WCComplement::cloneRegion() const
{
    return new WCComplement (*this);
}

LCRegion* WCComplement::doToLCRegion (const CoordinateSystem& cSys,
				      const IPosition& shape,
				      const IPosition& pixelAxesMap,
				      const IPosition& outOrder) const
{
    PtrBlock<const LCRegion*> regions;
    multiToLCRegion (regions, cSys, shape, pixelAxesMap, outOrder);
    return new LCComplement (True, regions);
}

String WCComplement::className()
{
    return "WCComplement";
}

String WCComplement::type() const
{
   return className();
}

TableRecord WCComplement::toRecord (const String& tableName) const
{
    TableRecord rec;
    defineRecordFields (rec, className());
    rec.defineRecord ("regions", makeRecord(tableName));
    return rec;
}

WCComplement* WCComplement::fromRecord (const TableRecord& rec,
					const String& tableName)
{
    PtrBlock<const WCRegion*> regions;
    unmakeRecord (regions, rec.asRecord("regions"), tableName);
    return new WCComplement (True, regions);
}

} //# NAMESPACE CASACORE - END

