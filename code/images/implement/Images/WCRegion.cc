//# WCRegion.cc: Class to define a region of interest in an image
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

#include <trial/Images/WCRegion.h>

#include <aips/Exceptions/Error.h>
#include <aips/Tables/TableRecord.h>
#include <trial/Images/WCBox.h>
#include <trial/Images/WCPolygon.h>
#include <trial/Lattices/RegionType.h>

WCRegion::WCRegion()
{}

WCRegion::WCRegion (const WCRegion&)
// 
// There is no private data in WCRegion so
// nothing to do
//
{}
 
WCRegion& WCRegion::operator= (const WCRegion&)
//
// There is no private data in WCRegion so
// there is nothing to assign
//
{
    return *this;
}

WCRegion::~WCRegion()
{}


WCRegion* WCRegion::fromRecord (const TableRecord& rec,
                                const String& tableName)
{
    const String& name = rec.asString ("name");
    if (name == WCBox::className()) {
        return WCBox::fromRecord (rec, tableName);
    } else if (name == WCPolygon::className()) {
        return WCPolygon::fromRecord (rec, tableName);
    } else {
        throw (AipsError ("WCRegion::fromRecord - " + name +
                          " is unknown derived WCRegion class"));
    }
    return 0;
}


void WCRegion::defineRecordFields (RecordInterface& record,
                                   const String& className) const
{
    record.define ("isRegion", Int(RegionType::WC));
    record.define ("name", className);
}

