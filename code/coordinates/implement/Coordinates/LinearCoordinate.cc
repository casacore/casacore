//# <LinearCoordinate.h>: this defines LinearCoordinate
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
//#
//# $Id$

#include <trial/Coordinates/LinearCoordinate.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/LinearSearch.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Containers/Record.h>
#include <aips/Mathematics/Math.h>

#include <strstream.h>

LinearCoordinate::LinearCoordinate(uInt naxis)
  : transform_p(naxis), names_p(naxis), units_p(naxis), crval_p(naxis)
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

String LinearCoordinate::showType() const
{
    return String("Linear");
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
   uInt n = nPixelAxes();             // nWorldAxes == nPixelAxes 
   world.resize(n);
   AlwaysAssert(pixel.nelements()==n, AipsError);
//
   Bool ok = transform_p.reverse(world, pixel, errmsg);
   if (!ok) {
      set_error(errmsg);
   } else {
      for (uInt i=0; i<n; i++) {
         world(i) += crval_p[i];
      }
   }
   return ok;
}

Bool LinearCoordinate::toPixel(Vector<Double> &pixel, 
			       const Vector<Double> &world) const
{
   uInt n = nPixelAxes();             // nWorldAxes == nPixelAxes 
   pixel.resize(n);
   AlwaysAssert(world.nelements()==n, AipsError);
   for (uInt i=0; i<n; i++) {
      pixel(i) = world(i) - crval_p[i];
   }
   Bool ok = transform_p.forward(pixel, pixel, errmsg);
   if (!ok) {
      set_error(errmsg);
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

void LinearCoordinate::checkFormat(Coordinate::formatType& format,
                                   const Bool ) const
//
//
{
// Scientific or fixed formats only are allowed.
// Absolute or offset is irrelevant
 
   if (format == Coordinate::DEFAULT) {
      format = Coordinate::SCIENTIFIC;
   } else {
      if (format != Coordinate::SCIENTIFIC &&
          format != Coordinate::FIXED) format = Coordinate::SCIENTIFIC;
   }
}

void LinearCoordinate::getPrecision(Int& precision,
                                    Coordinate::formatType& format,
                                    const Bool absolute,
                                    const Int defPrecScientific,
                                    const Int defPrecFixed,
                                    const Int ) const
{
// Scientific or fixed formats only are allowed.
// Absolute or offset is irrelevant
 
   checkFormat (format, absolute);
   
   if (format == Coordinate::SCIENTIFIC) {
      if (defPrecScientific >= 0) {
         precision = defPrecScientific;
      } else {
         precision = 6;              
      }
   } else if (format == Coordinate::FIXED) {
      if (defPrecFixed >= 0) {
         precision = defPrecFixed;
      } else {
         precision = 6;
      }
   }
}

String LinearCoordinate::format(String& units,
                                const Coordinate::formatType format,
                                const Double worldValue,
                                const uInt worldAxis,
                                const Bool absolute,
                                const Int precision) const
{
   AlwaysAssert(worldAxis < nWorldAxes(), AipsError);
 
// Check format
 
   Coordinate::formatType form = format;
   checkFormat (form, absolute);
   
   
// Set default precision
 
   Int prec = precision;
   if (prec < 0) getPrecision(prec, form, absolute, -1, -1, -1);
       
   
// Format and get units
         
   ostrstream oss;
   if (form == Coordinate::SCIENTIFIC) {
      oss.setf(ios::scientific, ios::floatfield);
      oss.precision(prec);
      oss << worldValue;
   } else if (form == Coordinate::FIXED) {
      oss.setf(ios::fixed, ios::floatfield);
      oss.precision(prec);
      oss << worldValue;        
   }                            
   units = worldAxisUnits()(worldAxis);
 
   return String(oss);
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


Bool LinearCoordinate::near(const Coordinate* pOther,
                            Double tol) const
{
   Vector<Int> excludeAxes;
   return near(pOther, excludeAxes, tol);
}


Bool LinearCoordinate::near(const Coordinate* pOther,
                            const Vector<Int>& excludeAxes,
                            Double tol) const
{
   if (pOther->type() != this->type()) {
      set_error("Comparison is not with another LinearCoordinate");
      return False;
   }

   LinearCoordinate* lCoord = (LinearCoordinate*)pOther;


// Check descriptor vector lengths

   if (names_p.nelements() != lCoord->names_p.nelements()) {
      set_error("The LinearCoordinates have differing numbers of world axis names");
      return False;
   }
   if (units_p.nelements() != lCoord->units_p.nelements()) {
      set_error("The LinearCoordinates have differing numbers of axis units");
      return False;
   }
   if (crval_p.nelements() != lCoord->crval_p.nelements()) {
      set_error("The LinearCoordinates have differing numbers of reference values");
      return False;
   }


// Number of pixel and world axes is the same for a LinearCoordinate
// Add an assertion check should this change.  Other code in LC has 
// checked that all the vectors we are comparing have the same 
// length as nPixelAxes()

   AlwaysAssert(nPixelAxes()==nWorldAxes(), AipsError);
   Vector<Bool> exclude(nPixelAxes());
   exclude = False;
   Bool found;
   uInt j = 0;
   uInt i;
   for (i=0; i<nPixelAxes(); i++) {
      if (linearSearch(found, excludeAxes, Int(i), excludeAxes.nelements()) >= 0)
        exclude(j++) = True;
    }

// Check the descriptors

   ostrstream oss;
   for (i=0; i<names_p.nelements(); i++) {
      if (!exclude(i)) {
         if (names_p(i) != lCoord->names_p(i)) {
            oss << "The LinearCoordinates have differing axis names for axis "
                << i << ends;
            set_error(String(oss));
            return False;
         }
      }
   }
   for (i=0; i<units_p.nelements(); i++) {
      if (!exclude(i)) {
         if (units_p(i) != lCoord->units_p(i)) {
            oss << "The LinearCoordinates have differing axis units for axis "
                << i << ends;
            set_error(String(oss));
            return False;
         }
      }
   }
   for (i=0; i<crval_p.nelements(); i++) {
      if (!exclude(i)) {
         if (!::near(crval_p[i],lCoord->crval_p[i],tol)) {
            oss << "The LinearCoordinates have differing reference values for axis "
                << i << ends;
            set_error(String(oss));
            return False;
         }
      }
   }


// Check the linear transform

   if (!transform_p.near(lCoord->transform_p,excludeAxes,tol)) {
      set_error("The LinearCoordinates have differing linear transformation matrices");
      return False;
   }

   return True;
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
