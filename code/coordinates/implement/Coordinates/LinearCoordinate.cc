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

#include <trial/Coordinates/LinearCoordinate.h>
#include <aips/Utilities/Assert.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Containers/Record.h>

LinearCoordinate::LinearCoordinate(uInt naxis)
  : transform_p(naxis), crval_p(naxis), names_p(naxis), units_p(naxis)
{
    crval_p.set(0);
}

LinearCoordinate::LinearCoordinate(const Vector<String> &names,
				   const Vector<String> &units,
				   const Vector<Double> &refVal,
				   const Vector<Double> &inc,
				   const Matrix<Double> &xform,
				   const Vector<Double> &refPix)
  : transform_p(refPix, inc, xform), 
    names_p(names.nelements()),
    units_p(names.nelements()), crval_p(names.nelements())
{
    uInt naxis = names.nelements();
    names_p = names;
    units_p = units;
    AlwaysAssert(units.nelements() == naxis &&
		 refVal.nelements() == naxis &&
		 inc.nelements() == naxis &&
		 xform.nrow() == naxis &&
		 xform.ncolumn() == naxis &&
		 refPix.nelements() == naxis, AipsError);
    for (uInt i=0; i<naxis; i++) {
	crval_p[i] = refVal(i);
    }
}

LinearCoordinate::LinearCoordinate(const LinearCoordinate &other)
    : transform_p(other.transform_p), names_p(other.names_p.copy()),
      units_p(other.units_p.copy()), crval_p(other.crval_p)
{
    // Nothing
}

LinearCoordinate &LinearCoordinate::operator=(const LinearCoordinate &other)
{
    if (this != &other) {
	uInt naxis = other.nWorldAxes();
	names_p.resize(naxis); names_p = other.names_p;
	units_p.resize(naxis); units_p = other.units_p;
	crval_p = other.crval_p;
	transform_p = other.transform_p;
    }
    return *this;
}

LinearCoordinate::~LinearCoordinate()
{
    // Nothing
}

Coordinate::Type LinearCoordinate::type() const
{
    return Coordinate::LINEAR;
}

uInt LinearCoordinate::nPixelAxes() const
{
    return names_p.nelements();
}

uInt LinearCoordinate::nWorldAxes() const
{
    return nPixelAxes();
}

    static String errmsg;
Bool LinearCoordinate::toWorld(Vector<Double> &world, 
			       const Vector<Double> &pixel) const
{
    Bool ok = transform_p.reverse(world, pixel, errmsg);
    if (!ok) {
	set_error(errmsg);
    } else {
	uInt n = pixel.nelements();
	for (uInt i=0; i<n; i++) {
	    world(i) += crval_p[i];
	}
    }
    return ok;
}

Bool LinearCoordinate::toPixel(Vector<Double> &pixel, 
			       const Vector<Double> &world) const
{

    uInt n = world.nelements();
    Bool ok = ToBool(n == names_p.nelements());

    if (! ok) {
	set_error("world position is the wrong size");
    } else {
	for (uInt i=0; i<n; i++) {
	    pixel(i) = world(i) - crval_p[i];
	}
	ok = transform_p.forward(pixel, pixel, errmsg);
	if (!ok) {
	    set_error(errmsg);
	}
    }
    return ok;
}

Vector<String> LinearCoordinate::worldAxisNames() const
{
    return names_p.copy();
}

Vector<String> LinearCoordinate::worldAxisUnits() const
{
    return units_p.copy();
}

Vector<Double> LinearCoordinate::referenceValue() const
{
    return Vector<Double>(crval_p);
}

Vector<Double> LinearCoordinate::increment() const
{
    return transform_p.cdelt();
}

Matrix<Double> LinearCoordinate::linearTransform() const
{
    return transform_p.pc();
}

Vector<Double> LinearCoordinate::referencePixel() const
{
    return transform_p.crpix();
}

Bool LinearCoordinate::setWorldAxisNames(const Vector<String> &names)
{
    Bool ok = ToBool(names.nelements() == nWorldAxes());
    if (! ok) {
	set_error("Vector has the wrong size");
    } else {
	names_p = names;
    }

    return ok;
}

Bool LinearCoordinate::setWorldAxisUnits(const Vector<String> &units, 
					  Bool adjust)
{
    Bool ok = Coordinate::setWorldAxisUnits(units, adjust);
    if (ok) {
	ok = ToBool(units.nelements() == nWorldAxes());
	if (ok) {
	    units_p = units;
	}
    }
    return ok;
}


Bool LinearCoordinate::setReferencePixel(const Vector<Double> &refPix)
{
    Bool ok = ToBool(refPix.nelements() == nWorldAxes());
    if (! ok) {
	set_error("Vector has the wrong size");
    } else {
	transform_p.crpix(refPix);
    }

    return ok;
}

Bool LinearCoordinate::setLinearTransform(const Matrix<Double> &xform)
{
    Bool ok = ToBool(xform.nrow() == nWorldAxes() && 
		     xform.ncolumn() == nWorldAxes() );
    if (! ok) {
	set_error("Vector has the wrong size");
    } else {
	transform_p.pc(xform);
    }

    return ok;
}

Bool LinearCoordinate::setIncrement(const Vector<Double> &inc)
{
    Bool ok = ToBool(inc.nelements() == nWorldAxes());
    if (! ok) {
	set_error("Vector has the wrong size");
    } else {
	transform_p.cdelt(inc);
    }

    return ok;
}

Bool LinearCoordinate::setReferenceValue(const Vector<Double> &refval)
{
    Bool ok = ToBool(refval.nelements() == nWorldAxes());
    if (! ok) {
	set_error("Vector has the wrong size");
    } else {
	refval.toBlock(crval_p);
    }

    return ok;
}

Bool LinearCoordinate::save(RecordInterface &container,
			    const String &fieldName) const
{
    Bool ok = ToBool(!container.isDefined(fieldName));
    if (ok) {
	Record subrec;
	subrec.define("crval", referenceValue());
	subrec.define("crpix", referencePixel());
	subrec.define("cdelt", increment());
	subrec.define("pc", linearTransform());
	subrec.define("axes", worldAxisNames());
	subrec.define("units", worldAxisUnits());

	container.defineRecord(fieldName, subrec);
    }
    return ok;
}

LinearCoordinate *LinearCoordinate::restore(const RecordInterface &container,
					   const String &fieldName)
{
    if (! container.isDefined(fieldName)) {
	return 0;
    }

    Record subrec(container.asRecord(fieldName));
    
    // We should probably do more type-checking as well as checking
    // for existence of the fields.
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

    LinearCoordinate *retval = 
	new LinearCoordinate(axes, units, crval, cdelt, pc, crpix);
    AlwaysAssert(retval, AipsError);
							  
    return retval;
}



Coordinate *LinearCoordinate::clone() const
{
    return new LinearCoordinate(*this);
}
