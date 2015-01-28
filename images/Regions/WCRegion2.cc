//# WCRegion.cc: Implementation of WCRegion::fromRecord
//# Copyright (C) 1998,2000
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

#include <casacore/images/Regions/WCRegion.h>
#include <casacore/images/Regions/WCBox.h>
#include <casacore/images/Regions/WCEllipsoid.h>
#include <casacore/images/Regions/WCPolygon.h>
#include <casacore/images/Regions/WCLELMask.h>
#include <casacore/images/Regions/WCUnion.h>
#include <casacore/images/Regions/WCIntersection.h>
#include <casacore/images/Regions/WCDifference.h>
#include <casacore/images/Regions/WCComplement.h>
#include <casacore/images/Regions/WCExtension.h>
#include <casacore/images/Regions/WCConcatenation.h>
#include <casacore/lattices/LRegions/RegionType.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

WCRegion* WCRegion::fromRecord (const TableRecord& rec,
                                const String& tableName)
{
    if (!rec.isDefined("isRegion")
    ||  rec.asInt("isRegion") != RegionType::WC) {
	throw (AipsError ("WCRegion::fromRecord - "
			  "record does not contain an WC region"));
    }
    const String& name = rec.asString ("name");
    WCRegion* regPtr = 0;
    if (name == WCBox::className()) {
        regPtr = WCBox::fromRecord (rec, tableName);
///    } else if (name == WCEllipsoid::className()) {
///        regPtr = WCEllipsoid::fromRecord (rec, tableName);
    } else if (name == WCPolygon::className()) {
        regPtr = WCPolygon::fromRecord (rec, tableName);
    }
    else if (name == WCEllipsoid::className()) {
        regPtr = WCEllipsoid::fromRecord (rec, tableName);
    } else if (name == WCLELMask::className()) {
        regPtr = WCLELMask::fromRecord (rec, tableName);
    } else if (name == WCUnion::className()) {
        regPtr = WCUnion::fromRecord (rec, tableName);
    } else if (name == WCIntersection::className()) {
        regPtr = WCIntersection::fromRecord (rec, tableName);
    } else if (name == WCDifference::className()) {
        regPtr = WCDifference::fromRecord (rec, tableName);
    } else if (name == WCComplement::className()) {
        regPtr = WCComplement::fromRecord (rec, tableName);
    } else if (name == WCExtension::className()) {
        regPtr = WCExtension::fromRecord (rec, tableName);
    } else if (name == WCConcatenation::className()) {
        regPtr = WCConcatenation::fromRecord (rec, tableName);
    }
    else {
        throw (AipsError ("WCRegion::fromRecord - " + name +
                          " is unknown derived WCRegion class"));
    }
    if (rec.isDefined ("comment")) {
	regPtr->setComment (rec.asString ("comment"));
    }
    return regPtr;
}

} //# NAMESPACE CASACORE - END

