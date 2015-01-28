//# WCPolygon.cc: Class to define a 2D polygonal world coordinate region of interest 
//# Copyright (C) 1998,1999,2000,2001
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

#include <casacore/images/Regions/WCEllipsoid.h>

#include <casacore/casa/Quanta/QuantumHolder.h>
#include <casacore/lattices/LRegions/LCEllipsoid.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN


WCEllipsoid::WCEllipsoid() {}

WCEllipsoid::WCEllipsoid(
	const Vector<Quantity>& center,
	const Vector<Quantity>& radii,
    const IPosition& pixelAxes,
    const CoordinateSystem& csys,
    const RegionType::AbsRelType absRel
)
: _center(center), _radii(radii), _pixelAxes(pixelAxes),
  _csys(csys), _absRel(absRel), _theta(Quantity(0, "rad")),
  _specType(NOT_SPECIAL)
{
	_init();
}

WCEllipsoid::WCEllipsoid(
	const Vector<Quantity>& center,
	const Quantity& radius,
    const IPosition& pixelAxes,
    const CoordinateSystem& csys,
    const RegionType::AbsRelType absRel
)
: _center(center), _radii(_center.size(), radius),
  _pixelAxes(pixelAxes), _csys(csys), _absRel(absRel),
  _theta(Quantity(0, "rad")), _specType(SPHERE) {
	_init();
}

WCEllipsoid::WCEllipsoid(
	const Quantity& xcenter, const Quantity& ycenter,
	const Quantity& majorAxis, const Quantity& minorAxis,
	const Quantity& theta,
	const uInt pixelAxis0, const uInt pixelAxis1,
    const CoordinateSystem& csys,
    const RegionType::AbsRelType absRel
) : _csys(csys), _absRel(absRel), _specType(ELLIPSE_2D) {
	AlwaysAssert (csys.nPixelAxes() >= 2, AipsError);
	AlwaysAssert (csys.nWorldAxes() >= 2, AipsError);
	String msg;
	if (! theta.isConform("rad")) {
		throw AipsError(
			String(__FUNCTION__)
			+ ": theta is not an angular quantity"
		);
	}
	if (! xcenter.isConform(ycenter)) {
		throw AipsError(
			String(__FUNCTION__)
			+ ": xcenter and ycenter do not have the same base unit"
		);
	}
	if (! majorAxis.isConform(minorAxis)) {
		throw AipsError(
			String(__FUNCTION__)
			+ ": major and and minor axes do not have the same base unit"
		);
	}
	if (majorAxis.getValue() < minorAxis.getValue(majorAxis.getUnit())) {
		throw AipsError(
			String(__FUNCTION__)
			+ ": major axis is smaller than minor axis."
		);
	}
	_theta.setValue(fmod(theta.getValue("rad"), C::pi));
	_theta.setUnit("rad");
	if (_theta.getValue() < 0) {
		_theta + Quantity(C::pi, "rad");
	}
	_center.resize(2);
	_center[0] = xcenter;
	_center[1] = ycenter;
	_radii.resize(2);
	_radii[0] = majorAxis;
	_radii[1] = minorAxis;
	_pixelAxes.resize(2);
	_pixelAxes[0] = pixelAxis0;
	_pixelAxes[1] = pixelAxis1;

	_init();
}

WCEllipsoid::WCEllipsoid(const WCEllipsoid& that)
: WCRegion(that),
  _center(that._center),
  _radii(that._radii),
  _pixelAxes(that._pixelAxes),
  _csys(that._csys),             // This one makes a copy
  _absRel(that._absRel),
  _theta(that._theta),
  _specType(that._specType)
{}


WCEllipsoid& WCEllipsoid::operator= (const WCEllipsoid& that)
//
// Assignment (copy semantics)
//
{
   if (this != &that) {
      WCRegion::operator= (that);
      _center = that._center;
      _radii = that._radii;
      _pixelAxes = that._pixelAxes;
      _csys = that._csys;           // This one makes a copy
      _absRel = that._absRel;
      _theta = that._theta;
      _specType = that._specType;

    }
    return *this;
}

Bool WCEllipsoid::operator== (const WCRegion& other) const {
	if (type() != other.type()) {
		return False;
	}

	const WCEllipsoid& that = (const WCEllipsoid&)other;

	if (_absRel != that._absRel) {
		return False;
	}
	if (! near(_theta.getValue(), that._theta.getValue())) {
		return False;
	}
	if (_theta.getUnit() != that._theta.getUnit()) {
		return False;
	}

	if (_pixelAxes.size() != that._pixelAxes.size()) {
		return False;
	}

	for (uInt i=0; i<_pixelAxes.size(); i++) {
		if (
				! near(_center[i].getValue(), that._center[i].getValue())
				|| _center[i].getUnit() != that._center[i].getUnit()
				|| ! near(_radii[i].getValue(), that._radii[i].getValue())
				|| _radii[i].getUnit() != that._radii[i].getUnit()
				|| _pixelAxes[i] != that._pixelAxes[i]
		) {
			return False;
		}
	}
	if (! _csys.near(that._csys)) {
		return False;
	}
	return True;
}


WCRegion* WCEllipsoid::cloneRegion() const
{
   return new WCEllipsoid(*this);
}

Bool WCEllipsoid::canExtend() const {
    return False;
}

String WCEllipsoid::type() const {
	return className();
}

String WCEllipsoid::className() {
	return "WCEllipsoid";
}

TableRecord WCEllipsoid::toRecord(const String&) const {
	unitInit();
	TableRecord rec;
	defineRecordFields(rec, className());

	rec.define("oneRel", True);
	rec.define("type", Int(_specType));
	rec.define("absrel", Int(_absRel));

	const uInt nAxes = _pixelAxes.nelements();
	Vector<Int> pixelAxes(nAxes);
	pixelAxes = (_pixelAxes+1).asVector();
	rec.define ("pixelAxes", pixelAxes);

	// Save ellipsoid. Convert abspix to one rel
	String error;

	{
		// center
		TableRecord rec2, rec3;
		for (uInt i=0; i<_center.size(); i++) {
			Double tmp = _center[i].getValue();
			String units = _center[i].getUnit();
			if (units == "pix" && _absRel == RegionType::Abs) {
				tmp += 1.0;
			}
			QuantumHolder qh(Quantity(tmp, units));
			if (! qh.toRecord(error, rec2)) {
				throw (
					AipsError (
						"WCEllipsoid::" + String(__FUNCTION__)
						+ ": could not save center because " + error
					)
				);
			}
			rec3.defineRecord(i, rec2);
		}
		rec.defineRecord("center", rec3);
	}
	{
		// radii
		TableRecord rec2, rec3;
		QuantumHolder qh;

		switch (_specType) {
		case SPHERE:
			qh =QuantumHolder(_radii[0]);
			if (! qh.toRecord(error, rec2)) {
				throw (
					AipsError (
						"WCEllipsoid::" + String(__FUNCTION__)
						+ ": could not save sphere radius because " + error
					)
				);
			}
			rec.defineRecord("radius", rec2);
			break;
		case ELLIPSE_2D:
		case NOT_SPECIAL:
		default:
			// both general and 2-d ellipse go here
			String error;
			for (uInt i=0; i<_radii.size(); i++) {
				qh = QuantumHolder(_radii[i]);
				if (! qh.toRecord(error, rec2)) {
					throw (
						AipsError (
							"WCEllipsoid::" + String(__FUNCTION__)
							+ ": could not save radii because " + error
					)
				);
				}
				rec3.defineRecord(i, rec2);
			}
			rec.defineRecord("radii", rec3);
		}
	}
	{
		// theta
		TableRecord rec2;
		QuantumHolder qh(_theta);
		if (! qh.toRecord(error, rec2)) {
			throw (
				AipsError (
					"WCEllipsoid::" + String(__FUNCTION__)
					+ ": could not save theta because " + error
				)
			);
		}
		rec.defineRecord("theta", rec2);
	}
	if (! _csys.save(rec, "coordinates")) {
		throw (AipsError ("WCEllipsoid::toRecord: could not save Coordinate System"));
	}
	return rec;
}


WCEllipsoid* WCEllipsoid::fromRecord (
	const TableRecord& rec,
    const String&
) {
	// Get CoordinateSystem

	unitInit();
	CoordinateSystem* csys =  CoordinateSystem::restore(rec,"coordinates");
	Bool oneRel = rec.asBool("oneRel");
	RegionType::AbsRelType absRel = RegionType::AbsRelType(rec.asInt("absrel"));
	SpecialType specType = SpecialType(rec.asInt("type"));

	// Get pixel axes and convert to zero rel.

	Vector<Int> tmp = Vector<Int>(rec.toArrayInt ("pixelAxes"));
	IPosition pixelAxes(tmp);
	if (oneRel) {
		pixelAxes -= 1;
	}

	// Get the ellipsoid

	Vector<Quantity> center(pixelAxes.size());
	String error, units;
	WCEllipsoid *ellipsoid = 0;
	Vector<Quantity> radii(pixelAxes.size());
	Quantity radius, theta;
	{
		// center
		QuantumHolder qh;
		const RecordInterface& subRecord = rec.asRecord("center");
		for (uInt i=0; i<pixelAxes.size(); i++) {
			const RecordInterface& centerRecord = subRecord.asRecord(i);
			if (!qh.fromRecord(error, centerRecord)) {
				throw (
					AipsError(
						"WCEllipsoid::fromRecord - could not recover center because of " + error
					)
				);
			}
			center[i] = qh.asQuantity();
			// Convert from 1-rel to 0-rel for absolute pixel units
			if (units=="pix" && absRel==RegionType::Abs && oneRel) {
				center[i] -= 1.0;
			}
		}
	}
	{
		// radii and theta if necessary
		QuantumHolder qh;
		switch (specType) {
		case SPHERE:
			{
				const RecordInterface& radiusRecord = rec.asRecord("radius");
				if (!qh.fromRecord(error, radiusRecord)) {
					throw (
						AipsError(
							"WCEllipsoid::fromRecord - could not recover sphere radius because of " + error
						)
					);
				}
			}
			radius = qh.asQuantity();
			break;
		case ELLIPSE_2D:
			AlwaysAssert(pixelAxes.size() == 2, AipsError);
			{
				const RecordInterface& thetaRecord = rec.asRecord("theta");
				if (!qh.fromRecord(error, thetaRecord)) {
					throw (
						AipsError(
							"WCEllipsoid::fromRecord - could not recover 2-d ellipse theta because of " + error
						)
					);
				}
			}
			theta = qh.asQuantity();
			// do not break, allow fall thru to default to get radii too.
		case NOT_SPECIAL:
		default:
			{
				const RecordInterface& radiusRecord = rec.asRecord("radii");
				for (uInt i=0; i<pixelAxes.size(); i++) {
					const RecordInterface& rec2 = radiusRecord.asRecord(i);
					if (!qh.fromRecord(error, rec2)) {
					throw (
						AipsError(
							"WCEllipsoid::fromRecord - could not recover ellipse radii because of " + error
						)
					);
					}
					radii[i] = qh.asQuantity();
				}
			}
		}
	}
	// construct and return
	switch (specType) {
	case SPHERE:
		ellipsoid = new WCEllipsoid(
			center, radius, pixelAxes, *csys, absRel
		);
		break;
	case ELLIPSE_2D:
		ellipsoid = new WCEllipsoid(
			center[0], center[1], radii[0], radii[1], theta,
			pixelAxes[0], pixelAxes[1], *csys, absRel
		);
		break;
	default:
		ellipsoid = new WCEllipsoid(
			center, radii, pixelAxes, *csys, absRel
		);
	}
	delete csys;
	return ellipsoid;
}


LCRegion* WCEllipsoid::doToLCRegion(
	const CoordinateSystem& csys,
    const IPosition& latticeShape,
    const IPosition& pixelAxesMap,
    const IPosition& outOrder
) const {
	Vector<Double> wCenter(csys.referenceValue().copy());
	Vector<String> centerUnits(csys.worldAxisUnits().copy());

	Vector<Double> wRadius(csys.nWorldAxes(), 0);
	Vector<String> radiusUnits(csys.worldAxisUnits().copy());

	// Reorder world coordinates for output CS and set units.
	// "funny" values and units (pix, frac) are handled later and are
	// ignored at this stage
	for (uInt i=0; i<_pixelAxes.nelements(); i++) {
		Int latticePixelAxis = pixelAxesMap[i];
        Int worldAxis = csys.pixelAxisToWorldAxis(latticePixelAxis);
		Quantity value = _center[latticePixelAxis];
		if (
			value.getUnit() != "pix"
			&& value.getUnit() != "frac"
			&& value.getUnit() != "default"
		) {
			// other WC classes seem to use the reference pixel value and
			// ignore the actual pixel values if pix or frac which I don't
			// understand, but for now I'm using that algorithm
	        wCenter[worldAxis] = value.getValue();
	        centerUnits[worldAxis] = value.getUnit();
		}
	}
	// Convert to pixels for all pixel axes of csys for center

	CoordinateSystem mycsys = csys;
	Vector<Int> absRel(wCenter.size(), _absRel);
	if (! mycsys.setWorldAxisUnits(centerUnits)) {
		throw (AipsError ("WCEllipsoid::doToLCregion - center units are inconsistent with coordinate system"));
	}
	makeWorldAbsolute (wCenter, absRel, mycsys, latticeShape);
	Vector<Double> pCenter;

	if (! mycsys.toPixel(pCenter, wCenter)) {
		throw (
			AipsError(
				"WCEllipsoid::doToLCregion - conversion of center to pixel coordinates failed"
			)
		);
	}
	for (uInt i=0; i<_pixelAxes.nelements(); i++) {
		Int latticePixelAxis = pixelAxesMap[i];
        Int worldAxis = csys.pixelAxisToWorldAxis(latticePixelAxis);
		Quantity value = _radii[latticePixelAxis];
		if (
			value.getUnit() != "pix"
		) {
			// other WC classes seem to use the reference pixel value and
			// ignore the actual pixel values if pix or frac which I don't
			// understand, but for now I'm using that algorithm
	        wRadius[worldAxis] = value.getValue();
	        radiusUnits[worldAxis] = value.getUnit();
		}
	}
	if (! mycsys.setWorldAxisUnits(radiusUnits)) {
		throw (AipsError ("WCEllipsoid::doToLCregion - center units are inconsistent with coordinate system"));
	}
	Vector<Double> pIncrement = mycsys.increment();
	Vector<Double> pRadius(_radii.size());
	for (uInt i=0; i<pRadius.size(); i++) {
		if (_radii[i].getUnit() == "pix") {
			pRadius[i] = _radii[i].getValue();
		}
		else {
			Quantity inc(fabs(pIncrement[i]), mycsys.worldAxisUnits()[i]);
			pRadius[i] = (_radii[i]/inc).getValue();
		}
	}
	// Now recover only those values from pCenter that we actually
	// want.  Here we handle frac/pixel/default units as well.

	Vector<Double> refPix = mycsys.referencePixel();
	const uInt nAxes = outOrder.nelements();
	Vector<Double> outCenter(nAxes);
	Vector<Double> outRadius(nAxes);

	IPosition outShape(nAxes);
	for (uInt i=0; i<_pixelAxes.nelements(); i++) {
		Int latticePixelAxis = pixelAxesMap[i];
		Double pixel = pCenter(latticePixelAxis);
		convertPixel(
			pixel, _center[i].getValue(), _center[i].getUnit(),
			_absRel, refPix[i], latticeShape[latticePixelAxis]
		);
	    outCenter[outOrder[i]] = pixel;
	    outRadius[outOrder[i]] = pRadius(latticePixelAxis);
	    outShape[outOrder[i]] = latticeShape(latticePixelAxis);
	}

	// Create the LCEllipsoid.

	switch(_specType) {
	case SPHERE:
		return new LCEllipsoid(outCenter, outRadius[0], outShape);
	case ELLIPSE_2D:
		// I'm pretty sure theta does not need to be mucked with
		// if the order of the axes changes.
		return new LCEllipsoid(
			outCenter[0], outCenter[1], outRadius[0], outRadius[1],
			_theta.getValue("rad"), outShape
		);
	default:
                break;
	}
        return new LCEllipsoid(outCenter, outRadius, outShape);
}


void WCEllipsoid::_init() {
	if (_pixelAxes.size() != _center.size()) {
		throw AipsError(
			"LCEllipsoid::" + String(__FUNCTION__)
			+ ": Different sizes for pixel axes and center vectors"
		);
	}
	if (_pixelAxes.size() != _radii.size()) {
		throw AipsError(
				"LCEllipsoid::" + String(__FUNCTION__)
			+ ": Different sizes for pixel axes and radii vectors"
		);
	}
	_checkPixelAxes();
	unitInit();
	_checkUnits();

	for (uInt i=0; i<_pixelAxes.nelements(); i++) {
		addAxisDesc (makeAxisDesc (_csys, _pixelAxes(i)));
	}
}

void WCEllipsoid::_checkPixelAxes() const {
	ostringstream oss;
	oss << _pixelAxes;
	String paAsString = oss.str();
	for (uInt i=0; i<_pixelAxes.size(); i++) {
		if (
			_pixelAxes[i] > Int(_csys.nPixelAxes()-1)
		) {
			throw (
				AipsError(
					"WCEllipsoid::" + String(__FUNCTION__)
					+ ": the specified pixel axes are greater than"
					+ "the number of pixel axes in the coordinate system"
				)
			);
		}
		if (paAsString.freq(String::toString(_pixelAxes[i])) > 1) {
			throw (
				AipsError(
					"WCEllipsoid::" + String(__FUNCTION__)
					+ ": You have specified the same pixel axis more than once"
				)
			);
		}
	}
}

void WCEllipsoid::_checkUnits() const {
	Vector<String> units(_radii.size());
	for (uInt i=0; i<units.size(); i++) {
		units[i] = _radii[i].getUnit();
	}
	try {
		checkAxes(_pixelAxes, _csys, units);
	}
	catch (AipsError x) {
		throw AipsError(
			x.getMesg() + " Checking radii units"
		);
	}
	for (uInt i=0; i<units.size(); i++) {
		units[i] = _center[i].getUnit();
	}
	try {
		checkAxes(_pixelAxes, _csys, units);
	}
	catch (AipsError x) {
		throw AipsError(
			x.getMesg() + " Checking center units"
		);
	}
}



} //# NAMESPACE CASACORE - END

