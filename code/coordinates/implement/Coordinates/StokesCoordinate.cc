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

#include <trial/Coordinates/StokesCoordinate.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Matrix.h>

#include <aips/Containers/Record.h>
#include <aips/Utilities/Assert.h>

StokesCoordinate::StokesCoordinate(const Vector<Int> &whichStokes)
    : values_p(whichStokes.nelements()), crval_p(0), crpix_p(0), matrix_p(1),
      cdelt_p(1), name_p("Stokes"), unit_p("")
{
    whichStokes.toBlock(values_p);
    // Make sure the stokes occur at most once
    Block<Bool> alreadyUsed(Stokes::NumberOfTypes);
    alreadyUsed = False;
    for (uInt i=0; i<values_p.nelements(); i++) {
	AlwaysAssert(Stokes::type(values_p[i]) != Stokes::Undefined, AipsError);
	AlwaysAssert(!alreadyUsed[whichStokes(i)], AipsError);
	alreadyUsed[whichStokes(i)] = True;
    }
}

StokesCoordinate::StokesCoordinate(const StokesCoordinate &other)
{
    values_p = other.values_p;
    crval_p = other.crval_p;
    crpix_p = other.crpix_p;
    matrix_p = other.matrix_p;
    cdelt_p = other.cdelt_p;
    name_p = other.name_p;
    unit_p = other.unit_p;
}

StokesCoordinate &StokesCoordinate::operator=(const StokesCoordinate &other)
{
    if (this != &other) {
	values_p = other.values_p;
	crval_p = other.crval_p;
	crpix_p = other.crpix_p;
	matrix_p = other.matrix_p;
	cdelt_p = other.cdelt_p;
	name_p = other.name_p;
	unit_p = other.unit_p;
    }

    return *this;
}

StokesCoordinate::~StokesCoordinate()
{
    // Nothing
}

Coordinate::Type StokesCoordinate::type() const
{
    return Coordinate::STOKES;
}

uInt StokesCoordinate::nPixelAxes() const
{
    return 1;
}

uInt StokesCoordinate::nWorldAxes() const
{
    return 1;
}

Bool StokesCoordinate::toWorld(Stokes::StokesTypes &stokes, Int pixel) const
{
    Bool ok = False;
    if (pixel >=0 && pixel<values_p.nelements()) {
	ok = True;
	stokes = Stokes::type(values_p[pixel]);
    } else {
	ok = False;
	set_error("Stokes has not been defined for this pixel");
    }
    return ok;
}

Bool StokesCoordinate::toPixel(Int &pixel, Stokes::StokesTypes stokes) const
{
    Bool found = False;
    uInt n = values_p.nelements();
    for (uInt i=0; i<n; i++) {
	if (stokes == values_p[i]) {
	    found = True;
	    break;
	}
    }

    if (found) {
	pixel = i;
    } else {
	set_error("Stokes type does not occur on axis");
	pixel = -99999;
    }

    return found;
}

Bool StokesCoordinate::toWorld(Vector<Double> &world, 
			       const Vector<Double> &pixel) const
{
    AlwaysAssert(pixel.nelements()==1 && world.nelements()==1, AipsError);
    Stokes::StokesTypes result;
    Bool ok = toWorld(result, Int(pixel(0)));
    world(0) = Double(result);
    return ok;
}

Bool StokesCoordinate::toPixel(Vector<Double> &pixel, 
		     const Vector<Double> &world) const
{
    AlwaysAssert(pixel.nelements()==1 && world.nelements()==1, AipsError);
    Int result;
    Stokes::StokesTypes stokes = Stokes::type(Int(world(0)));
    Bool ok = toPixel(result, stokes);
    pixel(0) = Double(result);
    return ok;
}

Vector<Int> StokesCoordinate::stokes() const
{
    return Vector<Int>(values_p);
}


Vector<String> StokesCoordinate::worldAxisNames() const
{
    Vector<String> names(1);
    names = name_p;
    return names;
}

Vector<String> StokesCoordinate::worldAxisUnits() const
{
    Vector<String> units(1);
    units = unit_p;
    return units;
}

Vector<Double> StokesCoordinate::referencePixel() const
{
    Vector<Double> crpix(1);
    crpix = crpix_p;
    return crpix;
}

Matrix<Double> StokesCoordinate::linearTransform() const
{
    Matrix<Double> matrix(1,1);
    matrix(0,0) = matrix_p;
    return matrix;
}

Vector<Double> StokesCoordinate::increment() const
{
    Vector<Double> cdelt(1);
    cdelt = cdelt_p;
    return cdelt;
}

Vector<Double> StokesCoordinate::referenceValue() const
{
    Vector<Double> crval(1);
    crval = crval_p;
    return crval;
}

Bool StokesCoordinate::setWorldAxisNames(const Vector<String> &names)
{
    AlwaysAssert(names.nelements() == 1, AipsError);
    name_p = names(0);
    return True;
}

Bool StokesCoordinate::setWorldAxisUnits(const Vector<String> &units,
					   Bool adjust)
{
    AlwaysAssert(units.nelements() == 1, AipsError);
    if (units(0) == "") {
        return True;
    }
    Bool ok = Coordinate::setWorldAxisUnits(units, adjust);
    if (ok) {
	unit_p = units(0);
    }
    return ok;
}


Bool StokesCoordinate::setReferencePixel(const Vector<Double> &refPix)
{
    AlwaysAssert(refPix.nelements() == 1, AipsError);
    crpix_p = refPix(0);
    return True;
}

Bool StokesCoordinate::setLinearTransform(const Matrix<Double> &xform)
{
    AlwaysAssert(xform.nelements() == 1, AipsError);
    matrix_p = xform(0,0);
    return True;
}

Bool StokesCoordinate::setIncrement(const Vector<Double> &inc) 
{
    AlwaysAssert(inc.nelements() == 1, AipsError);
    cdelt_p = inc(0);
    return True;
}

Bool StokesCoordinate::setReferenceValue(const Vector<Double> &refval)
{
    AlwaysAssert(refval.nelements() == 1, AipsError);
    crval_p = refval(0);
    return True;
}

Bool StokesCoordinate::save(RecordInterface &container,
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
	// subrec.define("units", worldAxisUnits()); // it never makes sense to set the units

	Vector<String> stokes(values_p.nelements());
	for (uInt i=0; i<stokes.nelements(); i++) {
	    Int dummy = values_p[i];
	    stokes(i) = Stokes::name((Stokes::StokesTypes)dummy);
	}
	subrec.define("stokes", stokes);

	container.defineRecord(fieldName, subrec);
    }
    return ok;
}

StokesCoordinate *StokesCoordinate::restore(const RecordInterface &container,
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

    
    // ignore units for stokes axes - doesn't seem to ever make sense
//     if (!subrec.isDefined("units")) {
// 	return 0;
//     }
//     Vector<String> units;
//     subrec.get("units", units);

    if (!subrec.isDefined("stokes")) {
	return 0;
    }
    Vector<String> stokes;
    subrec.get("stokes", stokes);
    Vector<Int> istokes(stokes.nelements());
    for (uInt i=0; i<istokes.nelements(); i++) {
	istokes(i) = Stokes::type(stokes(i));
    }
    

    StokesCoordinate *retval = new StokesCoordinate(istokes);
    AlwaysAssert(retval, AipsError);

    // retval->setWorldAxisUnits(units); // It never makes sense to set the units
    retval->setWorldAxisNames(axes);
    retval-> setIncrement(cdelt);
    retval->setReferenceValue(crval);
    retval->setReferencePixel(crpix);

    AlwaysAssert(retval, AipsError);
							  
    return retval;
}

Coordinate *StokesCoordinate::clone() const
{
    return new StokesCoordinate(*this);
}

String StokesCoordinate::format(String& units,
                                const Coordinate::formatType format,
                                const Double worldValue,
                                const uInt worldAxis,
                                const Bool absolute,
                                const Int precision) const
{
   units = worldAxisUnits()(worldAxis);
   const Stokes::StokesTypes sType = Stokes::type(Int(worldValue+0.5));
   return Stokes::name(sType);
}
