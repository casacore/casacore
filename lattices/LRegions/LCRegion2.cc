//# LCRegion2.cc: Implementation of LCRegion::fromRecord
//# Copyright (C) 1997,1998,1999,2001
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

#include <casacore/lattices/LRegions/LCRegion.h>
#include <casacore/lattices/LRegions/RegionType.h>
#include <casacore/lattices/LRegions/LCBox.h>
#include <casacore/lattices/LRegions/LCEllipsoid.h>
#include <casacore/lattices/LRegions/LCPolygon.h>
#include <casacore/lattices/LRegions/LCPixelSet.h>
#include <casacore/lattices/LRegions/LCPagedMask.h>
#include <casacore/lattices/LRegions/LCIntersection.h>
#include <casacore/lattices/LRegions/LCUnion.h>
#include <casacore/lattices/LRegions/LCConcatenation.h>
#include <casacore/lattices/LRegions/LCComplement.h>
#include <casacore/lattices/LRegions/LCDifference.h>
#include <casacore/lattices/LRegions/LCExtension.h>
#include <casacore/lattices/LRegions/LCStretch.h>
#include <casacore/lattices/LRegions/LCHDF5Mask.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

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
    } else if (name == LCPixelSet::className()) {
      	regPtr = LCPixelSet::fromRecord (rec, tableName);
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
    } else if (name == LCStretch::className()) {
        regPtr = LCStretch::fromRecord (rec, tableName);
    } else if (name == LCHDF5Mask::className()) {
      	regPtr = LCHDF5Mask::fromRecord (rec, tableName);
    } else {
	throw (AipsError ("LCRegion::fromRecord - " + name +
			  " is unknown derived LCRegion class"));
    }
    if (rec.isDefined ("comment")) {
	regPtr->setComment (rec.asString ("comment"));
    }
    return regPtr;
}

} //# NAMESPACE CASACORE - END

