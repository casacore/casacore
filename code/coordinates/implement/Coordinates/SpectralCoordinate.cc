//# <ClassFileName.h>: this defines <ClassName>, which ...
//# Copyright (C) 1997
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
//#
//# $Id$

#include <trial/Coordinates/SpectralCoordinate.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Utilities/Assert.h>
#include <aips/Containers/Record.h>

SpectralCoordinate::SpectralCoordinate(MFrequency::Types type,
				       Double f0, Double inc, Double refChan,
				       Double restFrequency)
 : type_p(type), crval_p(f0), cdelt_p(inc), restfreq_p(restFrequency),
   crpix_p(refChan), unit_p("Hz"), name_p("Frequency"), matrix_p(1.0)
{
    // Nothing
}

SpectralCoordinate::SpectralCoordinate(const SpectralCoordinate &other)
{
    type_p = other.type_p;
    crval_p = other.crval_p;
    cdelt_p = other.cdelt_p;
    crpix_p = other.crpix_p;
    unit_p = other.unit_p;
    name_p = other.name_p;
    matrix_p = other.matrix_p;
    restfreq_p = other.restfreq_p;
}

SpectralCoordinate &SpectralCoordinate::operator=(
					  const SpectralCoordinate &other)
{
    if (this != &other) {
	type_p = other.type_p;
	crval_p = other.crval_p;
	cdelt_p = other.cdelt_p;
	crpix_p = other.crpix_p;
	unit_p = other.unit_p;
	name_p = other.name_p;
	matrix_p = other.matrix_p;
	restfreq_p = other.restfreq_p;
    }
    return *this;
}

SpectralCoordinate::~SpectralCoordinate()
{
    // Nothing
}

Coordinate::Type SpectralCoordinate::type() const
{
    return Coordinate::SPECTRAL;
}

uInt SpectralCoordinate::nPixelAxes() const
{
    return 1;
}

uInt SpectralCoordinate::nWorldAxes() const
{
    return 1;
}

Bool SpectralCoordinate::toWorld(Vector<Double> &world, 
				 const Vector<Double> &pixel) const
{
    AlwaysAssert(world.nelements() == 1 && pixel.nelements() == 1, AipsError);
    world(0) = crval_p + cdelt_p * matrix_p * (pixel(0) - crpix_p);
    return True;
}

Bool SpectralCoordinate::toPixel(Vector<Double> &pixel, 
				 const Vector<Double> &world) const
{
    AlwaysAssert(world.nelements() == 1 && pixel.nelements() == 1, AipsError);
    pixel(0) = (world(0) - crval_p) / (cdelt_p * matrix_p) + crpix_p;
    return True;
}


Vector<String> SpectralCoordinate::worldAxisNames() const
{
    Vector<String> names(1);
    names = name_p;
    return names;
}

Vector<String> SpectralCoordinate::worldAxisUnits() const
{
    Vector<String> units(1);
    units = unit_p;
    return units;
}

Vector<Double> SpectralCoordinate::referencePixel() const
{
    Vector<Double> crpix(1);
    crpix = crpix_p;
    return crpix;
}

Matrix<Double> SpectralCoordinate::linearTransform() const
{
    Matrix<Double> matrix(1,1);
    matrix(0,0) = matrix_p;
    return matrix;
}

Vector<Double> SpectralCoordinate::increment() const
{
    Vector<Double> cdelt(1);
    cdelt = cdelt_p;
    return cdelt;
}

Vector<Double> SpectralCoordinate::referenceValue() const
{
    Vector<Double> crval(1);
    crval = crval_p;
    return crval;
}

Bool SpectralCoordinate::setWorldAxisNames(const Vector<String> &names)
{
    AlwaysAssert(names.nelements() == 1, AipsError);
    name_p = names(0);
    return True;
}

Bool SpectralCoordinate::setWorldAxisUnits(const Vector<String> &units,
					   Bool adjust)
{
    Double before = cdelt_p;
    AlwaysAssert(units.nelements() == 1, AipsError);
    Bool ok = Coordinate::setWorldAxisUnits(units, adjust);
    if (ok) {
	unit_p = units(0);
	Double after = cdelt_p;
	restfreq_p *= after / before;
    }
    return ok;
}


Bool SpectralCoordinate::setReferencePixel(const Vector<Double> &refPix)
{
    AlwaysAssert(refPix.nelements() == 1, AipsError);
    crpix_p = refPix(0);
    return True;
}

Bool SpectralCoordinate::setLinearTransform(const Matrix<Double> &xform)
{
    AlwaysAssert(xform.nelements() == 1, AipsError);
    matrix_p = xform(0,0);
    return True;
}

Bool SpectralCoordinate::setIncrement(const Vector<Double> &inc) 
{
    AlwaysAssert(inc.nelements() == 1, AipsError);
    cdelt_p = inc(0);
    return True;
}

Bool SpectralCoordinate::setReferenceValue(const Vector<Double> &refval)
{
    AlwaysAssert(refval.nelements() == 1, AipsError);
    crval_p = refval(0);
    return True;
}

Double SpectralCoordinate::restFrequency() const
{
    return restfreq_p;
}

Bool SpectralCoordinate::setRestFrequency(Double newFrequency)
{
    restfreq_p = newFrequency;
    return True;
}

Bool SpectralCoordinate::save(RecordInterface &container,
			    const String &fieldName) const
{
    Bool ok = ToBool(!container.isDefined(fieldName));
    if (ok) {
	Record subrec;
	String system = "unknown";
	switch (type_p) {
	case MFrequency::REST: system = "REST"; break;
	case MFrequency::LSR: system = "LSR"; break;
	case MFrequency::LSRK: system = "LSRK"; break;
	case MFrequency::BARY: system = "BARY"; break;
	case MFrequency::GEO: system = "GEO"; break;
	case MFrequency::TOPO: system = "TOPO"; break;
	case MFrequency::GALACTO: system = "GALACTO"; break;
	}
	subrec.define("system", system);
	subrec.define("crval", referenceValue());
	subrec.define("crpix", referencePixel());
	subrec.define("cdelt", increment());
	subrec.define("pc", linearTransform());
	subrec.define("axes", worldAxisNames());
	subrec.define("units", worldAxisUnits());
	subrec.define("restfreq", restFrequency());

	container.defineRecord(fieldName, subrec);
    }
    return ok;
}

SpectralCoordinate *SpectralCoordinate::restore(const RecordInterface &container,
					   const String &fieldName)
{
    if (! container.isDefined(fieldName)) {
	return 0;
    }

    Record subrec(container.asRecord(fieldName));
    
    // We should probably do more type-checking as well as checking
    // for existence of the fields.
    if (! subrec.isDefined("system")) {
	return 0;
    }
    String system;
    subrec.get("system", system);
    MFrequency::Types sys;
    if (system == "REST") {
	sys = MFrequency::REST;
    } else if (system == "LSR") {
	sys = MFrequency::LSR;
    } else if (system == "LSRK") {
	sys = MFrequency::LSRK;
    } else if (system == "BARY") {
	sys = MFrequency::BARY;
    } else if (system == "GEO") {
	sys = MFrequency::GEO;
    } else if (system == "TOPO") {
	sys = MFrequency::TOPO;
    } else if (system == "GALACTO") {
	sys = MFrequency::GALACTO;
    } else {
	return 0;
    }
    
    if (!subrec.isDefined("crval")) {
	return 0;
    }
    Vector<Double> crval;
    subrec.get("crval", crval);

    if (!subrec.isDefined("crpix")) {
	return 0;
    }
    Vector<Double> crpix;
    subrec.get("crpix", crpix);

    if (!subrec.isDefined("cdelt")) {
	return 0;
    }
    Vector<Double> cdelt;
    subrec.get("cdelt", cdelt);

    if (!subrec.isDefined("pc")) {
	return 0;
    }
    Matrix<Double> pc;
    subrec.get("pc", pc);

    
    if (!subrec.isDefined("axes")) {
	return 0;
    }
    Vector<String> axes;
    subrec.get("axes", axes);
    
    if (!subrec.isDefined("units")) {
	return 0;
    }
    Vector<String> units;
    subrec.get("units", units);

    if (!subrec.isDefined("restfreq")) {
	return 0;
    }
    Double restfreq;
    subrec.get("restfreq", restfreq);

    SpectralCoordinate *retval = 
	new SpectralCoordinate(sys, 0, 1, 0, 0);
    AlwaysAssert(retval, AipsError);

    // We have to do the units first since they will change the
    // reference value and increment if we do them too late.
    retval->setWorldAxisUnits(units);
    retval->setWorldAxisNames(axes);
    retval-> setIncrement(cdelt);
    retval->setReferenceValue(crval);
    retval->setReferencePixel(crpix);
    retval->setRestFrequency(restfreq);
							  
    return retval;
}

Coordinate *SpectralCoordinate::clone() const
{
    return new SpectralCoordinate(*this);
}
