//# LCRegion2.cc: Implementation of LCRegion::fromRecord
//# Copyright (C) 1997,1998
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

#include <trial/Lattices/LCRegion.h>
#include <trial/Lattices/RegionType.h>
#include <trial/Lattices/LCBox.h>
#include <trial/Lattices/LCEllipsoid.h>
#include <trial/Lattices/LCPolygon.h>
#include <trial/Lattices/LCMask.h>
#include <trial/Lattices/LCPagedMask.h>
#include <trial/Lattices/LCIntersection.h>
#include <trial/Lattices/LCUnion.h>
#include <trial/Lattices/LCConcatenation.h>
#include <trial/Lattices/LCComplement.h>
#include <trial/Lattices/LCDifference.h>
#include <trial/Lattices/LCExtension.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Exceptions/Error.h>


LCRegion* LCRegion::fromRecord (const TableRecord& rec,
				const String& tableName)
{
    if (!rec.isDefined("isRegion")
    ||  rec.asInt("isRegion") != RegionType::LC) {
	throw (AipsError ("LCRegion::fromRecord - "
			  "record does not contain an LC region"));
    }
    const String& name = rec.asString ("name");
    LCRegion* regPtr = 0;
    if (name == LCBox::className()) {
	regPtr = LCBox::fromRecord (rec, tableName);
    } else if (name == LCEllipsoid::className()) {
	regPtr = LCEllipsoid::fromRecord (rec, tableName);
    } else if (name == LCPolygon::className()) {
      	regPtr = LCPolygon::fromRecord (rec, tableName);
    } else if (name == LCMask::className()) {
      	regPtr = LCMask::fromRecord (rec, tableName);
    } else if (name == LCPagedMask::className()) {
      	regPtr = LCPagedMask::fromRecord (rec, tableName);
    } else if (name == LCIntersection::className()) {
      	regPtr = LCIntersection::fromRecord (rec, tableName);
    } else if (name == LCUnion::className()) {
      	regPtr = LCUnion::fromRecord (rec, tableName);
    } else if (name == LCConcatenation::className()) {
      	regPtr = LCConcatenation::fromRecord (rec, tableName);
    } else if (name == LCComplement::className()) {
      	regPtr = LCComplement::fromRecord (rec, tableName);
    } else if (name == LCDifference::className()) {
      	regPtr = LCDifference::fromRecord (rec, tableName);
    } else if (name == LCExtension::className()) {
        regPtr = LCExtension::fromRecord (rec, tableName);
    } else {
	throw (AipsError ("LCRegion::fromRecord - " + name +
			  " is unknown derived LCRegion class"));
    }
    if (rec.isDefined ("comment")) {
	regPtr->setComment (rec.asString ("comment"));
    }
    return regPtr;
}
