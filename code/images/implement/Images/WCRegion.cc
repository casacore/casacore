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
#include <trial/Lattices/RegionType.h>
#include <trial/Lattices/LCExtension.h>
#include <trial/Lattices/LCBox.h>
#include <trial/Coordinates/CoordinateSystem.h>
#include <trial/Coordinates/Coordinate.h>
#include <trial/Coordinates/DirectionCoordinate.h>
#include <trial/Coordinates/SpectralCoordinate.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Utilities/GenSort.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>


WCRegion::WCRegion()
{}

WCRegion::WCRegion (const WCRegion& other)
: itsComment  (other.itsComment),
  itsAxesDesc (other.itsAxesDesc)
{}
 
WCRegion& WCRegion::operator= (const WCRegion& other)
{
    if (this != &other) {
	itsComment  = other.itsComment;
	itsAxesDesc = other.itsAxesDesc;
    }
    return *this;
}

WCRegion::~WCRegion()
{}


Bool WCRegion::operator== (const WCRegion& other) const
{
    // Type check.
    return ToBool (type() == other.type());
}

void WCRegion::defineRecordFields (RecordInterface& record,
                                   const String& className) const
{
    record.define ("isRegion", Int(RegionType::WC));
    record.define ("name", className);
    record.define ("comment", itsComment);
}


const Record& WCRegion::getAxisDesc (uInt axis) const
{
    AlwaysAssert (axis < itsAxesDesc.nfields(), AipsError);
    return itsAxesDesc.subRecord (axis);
}

Int WCRegion::axisNr (const Record& desc, const Record& axesDesc) const
{
    uInt nf = axesDesc.nfields();
    for (uInt i=0; i<nf; i++) {
	if (isAxisDescEqual (desc, axesDesc.subRecord(i))) {
	    return i;
	}
    }
    return -1;
}

Bool WCRegion::isAxisDescEqual (const Record& desc1, const Record& desc2) const
{
    uInt nf = desc1.nfields();
    if (desc2.nfields() != nf) {
	return False;
    }
    for (uInt j=0; j<nf; j++) {
	Int fld = desc1.fieldNumber (desc2.name(j));
	if (fld < 0) {
	    return False;                // field does not exist
	}
	if (desc2.dataType(j) != desc1.dataType(fld)) {
	    return False;
	}
	switch (desc2.dataType(j)) {
	case TpInt:
	    if (desc2.asInt(j) != desc1.asInt(fld)) {
		return False;
	    }
	    break;
	case TpString:
	    if (desc2.asString(j) != desc1.asString(fld)) {
		return False;
	    }
	    break;
	default:
	    throw (AipsError ("WCRegion::isAxisEqual - "
			      "cannot handle data type"));
	}
    }
    return True;
}

void WCRegion::addAxisDesc (const Record& desc)
{
    itsAxesDesc.defineRecord (itsAxesDesc.nfields(), desc);
}

Record WCRegion::makeAxisDesc (const CoordinateSystem& cSys, uInt axis) const
{
    Int coord, axisInCoord;
    Record axisrec;
    AlwaysAssert (axis <cSys.nPixelAxes(), AipsError);
    cSys.findPixelAxis (coord, axisInCoord, axis);
    Int type = cSys.type (coord);
    axisrec.define ("type", type);
    axisrec.define ("axis", axisInCoord);
    switch (type) {
    case Coordinate::DIRECTION:
    {
	Int type = cSys.directionCoordinate(coord).directionType();
	axisrec.define ("dirtype", type);
	break;
    }
    case Coordinate::SPECTRAL:
    {
	Int type = cSys.spectralCoordinate(coord).frequencySystem();
	axisrec.define ("freqtype", type);
	break;
    }
    default:
	Vector<String> names = cSys.coordinate(coord).worldAxisNames();
	axisrec.define ("name", names(axisInCoord));
    }
    return axisrec;
}

Record WCRegion::makeAxesDesc (const CoordinateSystem& cSys) const
{
    Record desc;
    for (uInt i=0; i<cSys.nPixelAxes(); i++) {
	desc.defineRecord (i, makeAxisDesc (cSys, i));
    }
    return desc;
}

Bool WCRegion::canExtend() const
{
    return False;
}

LCRegion* WCRegion::toLCRegion (const CoordinateSystem& cSys,
				const IPosition& shape) const
{
    uInt i,n;
    // Make sure shape length matches number of pixel axes.
    AlwaysAssert (shape.nelements() == cSys.nPixelAxes(), AipsError);
    // Make pixel axes description of coordinate system.
    Record desc = makeAxesDesc (cSys);
    // Find the mapping of the axesDesc of the region to the axesDesc
    // of the new coordinate system.
    // An exception is thrown if a region axis is not used in the
    // coordinate system.
    uInt ndout = shape.nelements();
    uInt ndreg = ndim();
    IPosition pixelAxesMap(ndout);
    IPosition axisUsed(ndout, 0);
    n = 0;
    for (i=0; i<ndreg; i++) {
        Int axis = axisNr (getAxisDesc(i), desc);
	if (axis < 0) {
	    throw (AipsError ("WCRegion::toLCRegion - "
			      "a region axis is unknown in target "
			      "coordinate system"));
	}
	pixelAxesMap(n++) = axis;
	axisUsed(axis) = True;
    }
    for (i=0; i<ndout; i++) {
        if (axisUsed(i) == 0) {
	    pixelAxesMap(n++) = i;
	}
    }
    return toLCRegion (cSys, shape, pixelAxesMap, pixelAxesMap);
}

LCRegion* WCRegion::toLCRegion (const CoordinateSystem& cSys,
				const IPosition& shape,
				const IPosition& pixelAxesMap,
				const IPosition& outOrder) const
{
    uInt i;
    // We have an nD region which is used for an mD image (m>=n).
    // outOrder(i) gives output axis of axis i.
    // pixelAxesMap(i) gives cSys/shape axis of axis i.
    // First determine along which axes the region has to be extended.
    uInt ndreg = ndim();
    uInt ndout = pixelAxesMap.nelements();
    DebugAssert (ndout>=ndreg, AipsError);
    // If no extension is needed or if the region can extend itself,
    // life is simple.
    if (ndout == ndreg  ||  canExtend()) {
        return doToLCRegion (cSys, shape, pixelAxesMap, outOrder);
    }
    // We have to make the extension here.
    // So split the IPositions into the region part and the extension part.
    IPosition pixAxesMap(ndreg);
    IPosition outOrd(ndreg);
    IPosition extendAxes(ndout-ndreg);
    IPosition extendShape(ndout-ndreg);
    Vector<uInt> inx(ndreg);
    GenSortIndirect<Int>::sort (inx, outOrder.storage(), ndreg);
    for (i=0; i<ndreg; i++) {
        pixAxesMap(i) = pixelAxesMap(i);
	outOrd(inx(i)) = i;
    }
    for (i=ndreg; i<ndout; i++) {
        extendAxes(i-ndreg) = outOrder(i);
	extendShape(i-ndreg) = shape(pixelAxesMap(i));
    }
    LCRegion* regPtr = doToLCRegion (cSys, shape, pixAxesMap, outOrd);
    LCRegion* extPtr = new LCExtension (*regPtr, extendAxes,
					LCBox(extendShape));
    delete regPtr;
    return extPtr;
}
