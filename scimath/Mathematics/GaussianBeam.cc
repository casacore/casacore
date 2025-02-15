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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#include <casacore/scimath/Mathematics/GaussianBeam.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Quanta/QC.h>
#include <casacore/casa/Quanta/QuantumHolder.h>
#include <casacore/casa/Quanta/QLogical.h>

namespace casacore {

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
    if (unwrap) {
        return _unwrap(_pa);
    }
    return _pa;
}

Double GaussianBeam::getPA(const Unit& u, const Bool unwrap) const {
    return getPA(unwrap).getValue(u);
}

Quantity GaussianBeam::_unwrap(const Quantity& pa) {
    if (pa > QC::qTurn( ) || pa <= -QC::qTurn( )) {
        Quantity upa((fmod(pa.getValue("deg"), 180)), "deg");
        if (upa > QC::qTurn( )) {
            upa -= QC::hTurn( );
        }
        else if (upa <= -QC::qTurn( )) {
            upa += QC::hTurn( );
        }
        upa.convert(pa.getUnit());
        return upa;
    }
    return pa;
}

void GaussianBeam::setMajorMinor(
    const Quantity& majAx, const Quantity& minAx
) {
    static ostringstream oss;
    auto majVal = majAx.getValue();
    auto minVal = minAx.getValue();
    ThrowIf(
        isInf(majVal) || isInf(minVal) || isNaN(majVal) || isNaN(minVal),
        "Neither the major nor the minor axis value is permitted "
        "to be infinity or NaN"
    );
    ThrowIf(
        majVal < 0, "Major axis cannot be less than zero."
    );
    ThrowIf(
        minVal < 0, "Minor axis cannot be less than zero."
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

void GaussianBeam::setPA(const Quantity& pa, Bool unwrap) {
    auto paVal = pa.getValue();
    ThrowIf(
        isInf(paVal) || isNaN(paVal),
        "The position angle value is not permitted to be infinity or NaN"
    );
    ThrowIf(
        ! pa.isConform("rad"),
        "Position angle must have angular units ("
        +  pa.getUnit() + " is not)."
    );
    _pa = unwrap ? _unwrap(pa) : pa;
}

Bool GaussianBeam::isNull() const {
    return _major.getValue() == 0 || _minor.getValue() == 0;
}

Double GaussianBeam::getArea(const Unit& unit) const {
    // NOTE we never want to return a Qauntity because of the
    // nonstandard handling of solid angle units in CASA
    Quantity qunit(1, unit);
    if (qunit.isConform("sr") || qunit.isConform("rad2")) {
        static const Double coeff = M_PI/(4.0*M_LN2);
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
    return casacore::near(left.getMajor(), other.getMajor(), relWidthTol)
        && casacore::near(left.getMinor(), other.getMinor(), relWidthTol)
        && casacore::nearAbs(left.getPA(True), other.getPA(True), absPATol);
}

}
