//# WCRegion.cc: Class to define a region of interest in an image
//# Copyright (C) 1998,2000,2001,2002,2003
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
#include <casacore/lattices/LRegions/RegionType.h>
#include <casacore/lattices/LRegions/LCExtension.h>
#include <casacore/lattices/LRegions/LCBox.h>
#include <casacore/coordinates/Coordinates/CoordinateSystem.h>
#include <casacore/coordinates/Coordinates/Coordinate.h>
#include <casacore/coordinates/Coordinates/DirectionCoordinate.h>
#include <casacore/coordinates/Coordinates/SpectralCoordinate.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Utilities/GenSort.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

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
    return  (type() == other.type());
}

uInt WCRegion::ndim() const
{
    return itsAxesDesc.nfields();
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
	Int type = cSys.directionCoordinate(coord).directionType(True);
	axisrec.define ("dirtype", type);
	break;
    }
    case Coordinate::SPECTRAL:
    {
	Int type = cSys.spectralCoordinate(coord).frequencySystem(True);
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

    if (shape.nelements() != cSys.nPixelAxes()) {
    	ostringstream os;
    	os << "WCRegion::" << __FUNCTION__ << ": shape has "
			<< shape.nelements() << " elements, the coordinate system has "
			<< cSys.nPixelAxes() << " axes. The actual shape is "
			<< shape;
    	throw AipsError(os.str());
    }
    // Make pixel axes description of coordinate system.
    Record desc = makeAxesDesc (cSys);
    // Find the mapping of the axesDesc of the region to the axesDesc
    // of the new coordinate system.
    // An exception is thrown if a region axis is not used in the
    // coordinate system.
    uInt ndout = shape.nelements();
    uInt ndreg = itsAxesDesc.nfields();
    IPosition pixelAxesMap(ndout);
    IPosition axisUsed(ndout, 0);
    n = 0;
    for (i=0; i<ndreg; i++) {
        Int axis = axisNr (getAxisDesc(i), desc);
	if (axis < 0) {
	    throw (AipsError ("WCRegion::toLCRegion - "
			      "a region axis is unknown or inconsistent in target "
			      "coordinate system"));
	}
	pixelAxesMap(n++) = axis;
	axisUsed(axis) = 1;
    }
    for (i=0; i<ndout; i++) {
        if (axisUsed(i) == 0) {
	    pixelAxesMap(n++) = i;
	}
    }
    return toLCRegionAxes (cSys, shape, pixelAxesMap, pixelAxesMap);
}

LCRegion* WCRegion::toLCRegionAxes (const CoordinateSystem& cSys,
				    const IPosition& shape,
				    const IPosition& pixelAxesMap,
				    const IPosition& outOrder) const
{
    uInt i;
    // We have an nD region which is used for an mD image (m>=n).
    // outOrder(i) gives output axis of axis i.
    // pixelAxesMap(i) gives cSys/shape axis of axis i.
    // First determine along which axes the region has to be extended.
    uInt ndreg = itsAxesDesc.nfields();
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
    std::vector<Int> tmp(outOrder.begin(), outOrder.end());
    GenSortIndirect<Int>::sort (inx, &(tmp[0]), ndreg);
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



void WCRegion::makeWorldAbsolute (Vector<Double>& world, 
                                  const Vector<Int>& absRel, 
                                  const CoordinateSystem& cSys,
                                  const IPosition& shape) const
{
// For values that are already absolute, temporarily use rel = 0
// The absrel vector may have any length from 0 to nWorld
// Any relative values may be relative to ref val or image centre
// First deal with relative to reference value

   Vector<Int> ar(world.nelements());   
   const uInt nAR = absRel.nelements();
   Vector<Double> t(world.copy());
//
   for (uInt i=0; i<world.nelements(); i++) {
      if (i < nAR) {
         ar(i) = absRel(i);
      } else {
         ar(i) = RegionType::Abs;
      }
      if (ar(i) == RegionType::Abs) t(i) = 0.0;
   }
   Vector<Double> t2(t.copy());

// Convert to absolute at reference pixel

   cSys.makeWorldAbsolute(t);

// Now deal with relative to the image centre

   Vector<Double> p(shape.nelements());
   for (uInt i=0; i<shape.nelements(); i++) {  
      if (shape(i)==1) {
         p(i) = 0.0;
      } else {
         p(i) = (Double(shape(i))/2.0) - 0.5;
      }
   }
   Vector<Double> w;
   if (!cSys.toWorld(w,p)) {
      throw (AipsError (cSys.errorMessage()));
   }

// Make absolute w.r.t. the reference location in 'w'
   cSys.makeWorldAbsoluteRef (t2, w);

// Overwrite result for relative values.

   for (uInt i=0; i<world.nelements(); i++) {
      if (ar(i) == RegionType::RelRef) {
         world(i) = t(i);
      } else if (ar(i) == RegionType::RelCen) {
         world(i) = t2(i);
      }  
   }
}

void WCRegion::unitInit() {
   static Bool doneUnitInit = False;
   if (!doneUnitInit) {
      UnitMap::putUser("pix",UnitVal(1.0), "pixel units");
      UnitMap::putUser("frac",UnitVal(1.0), "fractional units");
      UnitMap::putUser("def",UnitVal(1.0), "default value");
      UnitMap::putUser("default",UnitVal(1.0), "default value");
      doneUnitInit = True;
   }
}

void WCRegion::checkAxes (
	const IPosition& pixelAxes,
    const CoordinateSystem& cSys,
    const Vector<String>& quantityUnits
) const {

	// Make sure we have world axes for these pixel axes

	Vector<Int> worldAxes(pixelAxes.size());
	Vector<String> units = cSys.worldAxisUnits();

	for (uInt i=0; i<pixelAxes.size(); i++) {
		worldAxes[i] = cSys.pixelAxisToWorldAxis(pixelAxes[i]);
		if (worldAxes[i] == -1) {
			throw (
				AipsError(
					"WCRegion::" + String(__FUNCTION__)
					+ "from " + type() + ": pixelAxes["
					+ String::toString(i)
					+ "]=" + String::toString(pixelAxes[i])
					+ " has no corresponding world axis"
				)
			);
		}
		String unit = quantityUnits[i];
		if (unit == "default") {
			throw (
				AipsError(
					"WCRegion::" + String(__FUNCTION__)
					+ "from " + type() + ": default units are not allowed"
				)
			);
		}
		if (unit != "pix" && unit != "frac") {
			if (Unit(unit) != Unit(units(worldAxes[i]))) {
				throw (
					AipsError(
						"WCRegion::" + String(__FUNCTION__)
						+ "from " + type()
						+ ": units of quantity[" + String::toString(i)
						+ "]=" + unit
						+ " are inconsistent with units of coordinate system"
						+ "units (" + units(worldAxes[i]) + ")"
					)
				);
			}
		}
	}
}

void WCRegion::convertPixel(
	Double& pixel,
    const Double& value,
    const String& unit,
    const Int absRel,
    const Double refPix,
    const Int shape
) {
   Bool isWorld = True;
   if (unit == "pix") {
      pixel = value;
      isWorld = False;
   } else if (unit == "frac") {
      pixel = value * shape;
      isWorld = False;
   }
//
   if (isWorld) return;
//
   if (absRel == RegionType::RelRef) {
      pixel += refPix;
   } else if (absRel == RegionType::RelCen) {
      pixel += Double(shape)/2;
   }
}

} //# NAMESPACE CASACORE - END

