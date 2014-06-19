//# Copyright (C) 1995,1997,1998,1999,2000,2001,2002,2003
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

#include <components/ComponentModels/GaussianBeam.h>

#include <casa/Containers/Record.h>
#include <casa/Quanta/QC.h>
#include <casa/Quanta/QuantumHolder.h>
#include <casa/Quanta/QLogical.h>

namespace casa {

const GaussianBeam GaussianBeam::NULL_BEAM = GaussianBeam();


GaussianBeam::GaussianBeam() : _major(Quantity(0, "arcsec")),
	_minor(Quantity(0, "arcsec")), _pa(Quantity(0, "deg")) {
}

GaussianBeam::GaussianBeam(
	const Quantity& major, const Quantity& minor,
	const Quantity& pa
) {
	setMajorMinor(major, minor);
	setPA(pa);
}

GaussianBeam::GaussianBeam(
	const Vector<Quantity>& parms
) {
	if (parms.size() != 3) {
		throw AipsError(
			"GaussianBeam(const Vector<Quantity>& parms): parms must have exactly three elements"
		);
	}
	setMajorMinor(parms[0], parms[1]);
	setPA(parms[2]);
}

GaussianBeam::GaussianBeam(const GaussianBeam& other) :
	_major(other._major), _minor(other._minor),
	_pa(other._pa) {}

GaussianBeam::~GaussianBeam() {}

GaussianBeam& GaussianBeam::operator=(const GaussianBeam& other) {
	if (this != &other) {
		_major = other._major;
		_minor = other._minor;
		_pa = other._pa;
	}
	return *this;
}

Bool GaussianBeam::operator==(const GaussianBeam& other) const {
	return _major == other._major && _minor == other._minor
		&& _pa == other._pa;
	/*
	return _major.getValue("rad") == other._major.getValue("rad")
		&& _minor.getValue("rad") == other._minor.getValue("rad")
		&& getPA(True).getValue("rad") == other.getPA(True).getValue("rad");
		*/
}

Bool GaussianBeam::operator!=(const GaussianBeam& other) const {
	return ! operator==(other);
}

const Quantity& GaussianBeam::getMajor() const {
	return _major;
}

Double GaussianBeam::getMajor(const Unit& u) const {
	return _major.getValue(u);
}

const Quantity& GaussianBeam::getMinor() const {
	return _minor;
}

Double GaussianBeam::getMinor(const Unit& u) const {
	return _minor.getValue(u);
}

Quantity GaussianBeam::getPA(const Bool unwrap) const {
	if (
		unwrap
		&& (
			_pa > QC::qTurn || _pa <= -QC::qTurn
		)
	) {
		Quantity pa((fmod(_pa.getValue("deg"), 180)), "deg");
		if (pa > QC::qTurn) {
			pa -= QC::hTurn;
			pa.convert(_pa.getUnit());
			return pa;
		}
	}
	return _pa;
}

Double GaussianBeam::getPA(const Unit& u, const Bool unwrap) const {
	return getPA(unwrap).getValue(u);
}

void GaussianBeam::setMajorMinor(
	const Quantity& majAx, const Quantity& minAx
) {
	static ostringstream oss;
	ThrowIf(
		majAx.getValue() < 0,
		"Major axis cannot be less than zero."
	);
	ThrowIf(
		minAx.getValue() < 0,
		"Minor axis cannot be less than zero."
	);
	ThrowIf (
		! majAx.isConform("rad"),
		"Major axis must have angular units ("
		+ majAx.getUnit() + " is not)."
	);
	ThrowIf (
		! minAx.isConform("rad"),
		"Major axis must have angular units ("
		+ minAx.getUnit() + " is not)."
	);
	ThrowIf(
		majAx < minAx,
		"Major axis must be greater or equal to minor axis"
	);
	_major = majAx;
	_minor = minAx;
}

void GaussianBeam::setPA(const Quantity& pa) {
	if (! pa.isConform("rad")) {
		ostringstream oss;
		oss << className() << "::" << __FUNCTION__;
		oss << ": Position angle must have angular units ("
			<< pa.getUnit() << " is not).";
		throw AipsError(oss.str());
	}
	_pa = pa;
}

Bool GaussianBeam::isNull() const {
	return _major.getValue() == 0 || _minor.getValue() == 0;
}

Double GaussianBeam::getArea(const Unit& unit) const {
    // NOTE we never want to return a Qauntity because of the
    // nonstandard handling of solid angle units in CASA
    Quantity qunit(1, unit);
	if (qunit.isConform("sr") || qunit.isConform("rad2")) {
		static const Double coeff = C::pi/(4*C::ln2);
        return coeff * (_major * _minor).getValue(unit);
	}
	else {
		ostringstream oss;
		oss << className() << "::" << __FUNCTION__
			<< ": Unit " << unit.getName() << " is not a solid angle.";
		throw AipsError(oss.str());
	}
}

const String& GaussianBeam::className() {
	static const String c = "GaussianBeam";
	return c;
}

Record GaussianBeam::toRecord() const {
	Record outRec;
	QuantumHolder qh(_major);
	Record tmp;
	String error;
	// there's no need for error checking, this object holds bona-fide quantities.
	qh.toRecord(error, tmp);
	outRec.defineRecord("major", tmp);
	qh = QuantumHolder(_minor);
	qh.toRecord(error, tmp);
	outRec.defineRecord("minor", tmp);
	qh = QuantumHolder(_pa);
	qh.toRecord(error, tmp);
	outRec.defineRecord("positionangle", tmp);
	return outRec;
}

GaussianBeam GaussianBeam::fromRecord(
	const Record& rec
) {
	if (rec.nfields() != 3) {
		throw AipsError("Beam record does not contain 3 fields");
	}
	QuantumHolder qh;
	if (! rec.isDefined("major")) {
		throw AipsError("Field major missing from restoring beam record");
	}
	const RecordInterface& subRec0 = rec.asRecord("major");
	String error;
	if (! qh.fromRecord(error, subRec0)) {
		throw AipsError(error);
	}
	Quantity major = qh.asQuantumDouble();


	if (! rec.isDefined("minor")) {
		throw AipsError("Field minor missing from restoring beam record");
	}
	const RecordInterface& subRec1 = rec.asRecord("minor");
	if (! qh.fromRecord(error, subRec1)) {
		throw AipsError(error);
	}
	Quantity minor = qh.asQuantumDouble();

	if (! rec.isDefined("positionangle")) {
		throw AipsError("Field positionangle missing from restoring beam record");
	}
	const RecordInterface& subRec2 = rec.asRecord("positionangle");
	if (! qh.fromRecord(error, subRec2)) {
		throw AipsError(error);
	}
	Quantity pa = qh.asQuantumDouble();
	return GaussianBeam(major, minor, pa);
}

ostream &operator<<(ostream &os, const GaussianBeam& beam) {
	os << "major: " << beam.getMajor() << ", minor: " << beam.getMinor()
		<< ", pa: " << beam.getPA(True);
	return os;
}

LogIO &operator<<(LogIO &os, const GaussianBeam& beam) {
	ostringstream oss;
	oss << beam;
	os << oss.str();
	return os;
}

Bool GaussianBeam::deconvolve(
	Angular2DGaussian& deconvolvedSize,
	const Angular2DGaussian& convolvedSize
) const {
	Unit radians(String("rad"));
	Unit positionAngleModelUnit = deconvolvedSize._pa.getFullUnit();
	Unit majorAxisModelUnit = deconvolvedSize._major.getFullUnit();
	Unit minorAxisModelUnit = deconvolvedSize._minor.getFullUnit();

	// Get values in radians
	Double majorSource = convolvedSize._major.getValue(radians);
	Double minorSource = convolvedSize._minor.getValue(radians);
	Double thetaSource = convolvedSize._pa.getValue(radians);
	Double majorBeam = _major.getValue(radians);
	Double minorBeam = _minor.getValue(radians);
	Double thetaBeam = _pa.getValue(radians);
	// Do the sums

	Double alpha  = square(majorSource*cos(thetaSource)) +
		square(minorSource*sin(thetaSource)) -
		square(majorBeam*cos(thetaBeam)) -
		square(minorBeam*sin(thetaBeam));
	Double beta   = square(majorSource*sin(thetaSource)) +
		square(minorSource*cos(thetaSource)) -
		square(majorBeam*sin(thetaBeam)) -
		square(minorBeam*cos(thetaBeam));
	Double gamma  = 2 * ( (square(minorSource)-square(majorSource))*sin(thetaSource)*cos(thetaSource) -
		(square(minorBeam)-square(majorBeam))*sin(thetaBeam)*cos(thetaBeam) );
	// Set result in radians

	Double s = alpha + beta;
	Double t = sqrt(square(alpha-beta) + square(gamma));
	Double limit = min(majorSource,minorSource);
	limit = min(limit,majorBeam);
	limit = min(limit,minorBeam);
	limit = 0.1*limit*limit;

	if(alpha<0.0 || beta<0.0 || s<t) {
		if(0.5*(s-t)<limit && alpha>-limit && beta>-limit) {
			// Point source. Fill in values of beam
			deconvolvedSize = GaussianBeam(
				Quantity(_major.get(majorAxisModelUnit)),
				Quantity(_minor.get(minorAxisModelUnit)),
				Quantity(_pa.get(positionAngleModelUnit))
			);
			// unwrap
			deconvolvedSize.setPA(deconvolvedSize.getPA(True));
			return True;
		}
		else {
			throw AipsError("Source may be only (slightly) resolved in one direction");
		}
	}
	Quantity majax(sqrt(0.5*(s+t)), radians);
	majax.convert(majorAxisModelUnit);
	Quantity minax(sqrt(0.5*(s-t)), radians);
	minax.convert(minorAxisModelUnit);
	Quantity pa(
		abs(gamma)+abs(alpha-beta) == 0.0
			? 0.0
			: 0.5*atan2(-gamma,alpha-beta),
		radians);
	pa.convert(positionAngleModelUnit);
	deconvolvedSize = GaussianBeam(majax, minax, pa);
	// unwrap
	deconvolvedSize.setPA(deconvolvedSize.getPA(True));
	return False;
}

Vector<Quantity> GaussianBeam::toVector(const Bool unwrap) const {
	Vector<Quantity> beam(3);
	beam[0] = _major;
	beam[1] = _minor;
	beam[2] = unwrap ? getPA(True) : _pa;
	return beam;
}


void GaussianBeam::convert(
	const String& majUnit, const String& minUnit, const String& paUnit
) {
	_major.convert(majUnit);
	_minor.convert(minUnit);
	_pa.convert(paUnit);
}

Bool near(
	const GaussianBeam& left, const GaussianBeam& other,
	const Double relWidthTol, const Quantity& absPATol
) {
	if (! absPATol.isConform("rad")) {
		throw AipsError(
			"GaussianBeam::near(): absPATol does not have angular units"
		);
	}
	return casa::near(left.getMajor(), other.getMajor(), relWidthTol)
		&& casa::near(left.getMinor(), other.getMinor(), relWidthTol)
		&& casa::nearAbs(left.getPA(True), other.getPA(True), absPATol);
}

}
