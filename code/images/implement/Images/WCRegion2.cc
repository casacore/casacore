//# WCRegion.cc: Implementation of WCRegion::fromRecord
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
#include <trial/Images/WCBox.h>
///#include <trial/Images/WCEllipsoid.h>
#include <trial/Images/WCPolygon.h>
#include <trial/Images/WCUnion.h>
#include <trial/Images/WCIntersection.h>
#include <trial/Images/WCDifference.h>
#include <trial/Images/WCComplement.h>
#include <trial/Images/WCExtension.h>
#include <trial/Images/WCConcatenation.h>
#include <trial/Lattices/RegionType.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Exceptions/Error.h>


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
    } else {
        throw (AipsError ("WCRegion::fromRecord - " + name +
                          " is unknown derived WCRegion class"));
    }
    if (rec.isDefined ("comment")) {
	regPtr->setComment (rec.asString ("comment"));
    }
    return regPtr;
}
