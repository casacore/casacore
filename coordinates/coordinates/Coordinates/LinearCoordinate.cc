//# LinearCoordinate.cc: this defines LinearCoordinate
//# Copyright (C) 1997,1998,1999,2000,2001,2003,2004
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

#include <coordinates/Coordinates/LinearCoordinate.h>
#include <coordinates/Coordinates/LinearXform.h>

#include <casa/Exceptions/Error.h>
#include <casa/Utilities/Assert.h>
#include <casa/Utilities/LinearSearch.h>
#include <casa/Utilities/Regex.h>
#include <casa/Arrays/Matrix.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/Vector.h>
#include <casa/BasicSL/String.h>
#include <casa/Containers/Record.h>
#include <casa/BasicMath/Math.h>
#include <casa/Quanta/Quantum.h>
#include <casa/Quanta/UnitMap.h>
#include <casa/Quanta/Unit.h>

#include <casa/sstream.h>

namespace casa { //# NAMESPACE CASA - BEGIN


LinearCoordinate::LinearCoordinate(uInt naxis)
: Coordinate()
{ 
    Vector<Double> refVal(naxis), refPix(naxis), incr(naxis);
    Matrix<Double> pc(naxis,naxis);
    Vector<String> names(naxis), units(naxis);
//
    pc = 0.0;
    for (uInt i=0; i<naxis; i++) {
       refVal[i] = 0.0;
       refPix[i] = 0.0;
       incr[i] = 1.0;
       pc(i,i) = 1.0;
       units[i] = String("");
       names[i] = String("");
    }
//
    makeWCS (wcs_p, naxis, refPix, refVal, incr, pc, units, names);
//
    setDefaultWorldMixRanges();
}

LinearCoordinate::LinearCoordinate(const Vector<String>& names,
				   const Vector<String>& units,
				   const Vector<Double>& refVal,
				   const Vector<Double>& inc,
				   const Matrix<Double>& pc,
				   const Vector<Double>& refPix)
: Coordinate()
{
    uInt naxis = names.nelements();
    makeWCS (wcs_p, naxis, refPix, refVal, inc, pc, units, names);
//
    setDefaultWorldMixRanges();
}


LinearCoordinate::LinearCoordinate(const Vector<String>& names,
                                   const Vector<Quantum<Double> >& refVal,
                                   const Vector<Quantum<Double> >& inc,   
                                   const Matrix<Double>& pc,
                                   const Vector<Double>& refPix)
: Coordinate()
{

// Check dimensions

    const uInt n = names.nelements();
    AlwaysAssert(refVal.nelements() == n &&
		 inc.nelements() == n &&
		 pc.nrow() == n &&
		 pc.ncolumn() == n &&
		 refPix.nelements() == n, AipsError);
//
    Vector<Double> cdelt(n), crval(n);
    Vector<String> units(n);
    for (uInt i=0; i<n; i++) {
       if (refVal[i].isConform(inc[i])) {

// Assign 

          units[i] = refVal[i].getUnit();
  	  crval[i] = refVal[i].getValue();

// Convert inc to units of refVal

          cdelt[i] = inc[i].getValue(Unit(units[i]));
       } else {
          throw (AipsError("Units of reference value and increment inconsistent"));
       }
    }
//
    makeWCS (wcs_p, n, refPix, crval, cdelt, pc, units, names);
//
    setDefaultWorldMixRanges();
}


LinearCoordinate::LinearCoordinate(const ::wcsprm& wcs, Bool oneRel)
: Coordinate()
{

// Test only holds linear wcs structure

// Copy WCS structure.  Flag 1 means allocate memory 

   wcs_p.flag = -1;
   int err = wcscopy (1, &(wcs), &wcs_p);
   if (err != 0) {
      String errmsg = "wcs wcscopy_error: ";
      errmsg += wcscopy_errmsg[err];
      throw(AipsError(errmsg));
   }
   set_wcs(wcs_p);
//
   for (Int i=0; i<wcs_p.naxis; i++) {

// Make 0-rel

      if (oneRel) wcs_p.crpix[i] -= 1.0;

// Fix up FITS units case

      String name(wcs.cunit[i]);
      Unit u(name);
      Unit u2 = UnitMap::fromFITS(u);
      strncpy (wcs_p.cunit[i], u2.getName().chars(), 9);
   }
//
    setDefaultWorldMixRanges();
}


LinearCoordinate::LinearCoordinate(const LinearCoordinate &other)
: Coordinate(other)
{
   wcs_p.flag = -1;    // Indicates not initialized
   copy(other);
}

LinearCoordinate &LinearCoordinate::operator=(const LinearCoordinate &other)
{
    if (this != &other) {
        Coordinate::operator=(other);
        copy(other);
    }
    return *this;
}

LinearCoordinate::~LinearCoordinate()
{
   if (wcs_p.flag != -1) {
      wcsfree(&wcs_p);
   }
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
    return wcs_p.naxis;
}

uInt LinearCoordinate::nWorldAxes() const
{
    return nPixelAxes();
}

Bool LinearCoordinate::toWorld(Vector<Double> &world, 
			       const Vector<Double> &pixel) const
{
   return toWorldWCS (world, pixel, wcs_p);
}

Bool LinearCoordinate::toPixel(Vector<Double> &pixel, 
			       const Vector<Double> &world) const
{
   return toPixelWCS (pixel, world, wcs_p);
}


Vector<String> LinearCoordinate::worldAxisNames() const
{
    const uInt n = nPixelAxes();
    Vector<String> tmp(n);
    for (uInt i=0; i<n; i++) {
       tmp[i] = String(wcs_p.ctype[i]);
    }
    return tmp;
}

Vector<String> LinearCoordinate::worldAxisUnits() const
{
    const uInt n = nWorldAxes();
    Vector<String> tmp(n);
    for (uInt i=0; i<n; i++) {
       tmp[i] = String(wcs_p.cunit[i]);
    }
    return tmp;
}

Vector<Double> LinearCoordinate::referenceValue() const
{
    const uInt n = nWorldAxes();
    Vector<Double> tmp(n);
    for (uInt i=0; i<n; i++) {
       tmp[i] = wcs_p.crval[i];
    }
    return tmp;
}

Vector<Double> LinearCoordinate::increment() const
{
    const uInt n = nWorldAxes();
    Vector<Double> tmp(n);
    for (uInt i=0; i<n; i++) {
       tmp[i] = wcs_p.cdelt[i];
    }
    return tmp;
}

Matrix<Double> LinearCoordinate::linearTransform() const
{
   Matrix<Double> tmp;
   pcToXform (tmp, wcs_p);
   return tmp;
}

Vector<Double> LinearCoordinate::referencePixel() const
{
    const uInt n = nPixelAxes();
    Vector<Double> tmp(n);
    for (uInt i=0; i<n; i++) {
       tmp[i] = wcs_p.crpix[i];
    }
    return tmp;
}

Bool LinearCoordinate::setWorldAxisNames(const Vector<String> &names)
{
    Bool ok = (names.nelements() == nWorldAxes());
    if (!ok) {
       set_error("names vector has the wrong size");
    } else {
       for (uInt i=0; i<nWorldAxes(); i++) {
          strcpy (wcs_p.ctype[i], names[i].chars());
       }
    }
//
    return ok;
}

Bool LinearCoordinate::setWorldAxisUnits(const Vector<String> &units)
{
    Vector<Double> d1 = increment();
    Bool ok = Coordinate::setWorldAxisUnits(units);
    if (ok) {
       for (uInt i=0; i<nWorldAxes(); i++) {
          strcpy (wcs_p.cunit[i], units[i].chars());
       }

// Presently wcs does not actually do anything with units

       set_wcs(wcs_p);
    }
    return ok;
}


Bool LinearCoordinate::overwriteWorldAxisUnits(const Vector<String> &units)
{
   Bool ok = units.nelements() == nWorldAxes();
   if (ok) {
      for (uInt i=0; i<nWorldAxes(); i++) {
         strcpy (wcs_p.cunit[i], units[i].chars());
      }
   } else {
      set_error ("units vector has the wrong size");
   }
   return ok;
}

Bool LinearCoordinate::setReferencePixel(const Vector<Double> &refPix)
{
    Bool ok = (refPix.nelements() == nWorldAxes());
    if (! ok) {
	set_error("reference pixel vector has the wrong size");
    } else {
      for (uInt i=0; i<nWorldAxes(); i++) {
         wcs_p.crpix[i] = refPix[i];
      }
      set_wcs(wcs_p);
    }
    return ok;
}

Bool LinearCoordinate::setLinearTransform(const Matrix<Double> &pc)
{
    Bool ok = (pc.nrow() == nWorldAxes() && 
		     pc.ncolumn() == nWorldAxes() );
    if (!ok) {
       set_error("Transform matrix has the wrong size");
    } else {
       xFormToPC(wcs_p, pc);
       set_wcs(wcs_p);
    }
//
    return ok;
}

Bool LinearCoordinate::setIncrement(const Vector<Double> &inc)
{
    Bool ok = (inc.nelements() == nWorldAxes());
    if (! ok) {
	set_error("increment vector has the wrong size");
    } else {
       for (uInt i=0; i<nWorldAxes(); i++) {
          wcs_p.cdelt[i] = inc[i];
       }
       set_wcs(wcs_p);
    }

    return ok;
}

Bool LinearCoordinate::setReferenceValue(const Vector<Double> &refval)
{
    Bool ok = (refval.nelements() == nWorldAxes());
    if (! ok) {
	set_error("reference value vector has the wrong size");
    } else {
       for (uInt i=0; i<nWorldAxes(); i++) {
          wcs_p.crval[i] = refval[i];
       }
       set_wcs(wcs_p);
    }
//
    return ok;
}


Bool LinearCoordinate::near(const Coordinate& other,
                            Double tol) const
{
   Vector<Int> excludeAxes;
   return near(other, excludeAxes, tol);
}


Bool LinearCoordinate::near(const Coordinate& other,
                            const Vector<Int>& excludeAxes,
                            Double tol) const
{
   if (other.type() != this->type()) {
      set_error("Comparison is not with another LinearCoordinate");
      return False;
   }

   const LinearCoordinate& lCoord = dynamic_cast<const LinearCoordinate&>(other);

// Check descriptor vector lengths

   Vector<String> names1(worldAxisNames());
   Vector<String> names2(lCoord.worldAxisNames());
   Vector<String> units1(worldAxisUnits());
   Vector<String> units2(lCoord.worldAxisUnits());
   Vector<Double> crval1(referenceValue());
   Vector<Double> crval2(lCoord.referenceValue());
   Vector<Double> cdelt1(increment());
   Vector<Double> cdelt2(lCoord.increment());
   Vector<Double> crpix1(referencePixel());
   Vector<Double> crpix2(lCoord.referencePixel());
//
   if (names1.nelements() != names2.nelements()) {
      set_error("The LinearCoordinates have differing numbers of world axis names");
      return False;
   }
   if (units1.nelements() != units2.nelements()) {
      set_error("The LinearCoordinates have differing numbers of axis units");
      return False;
   }
   if (crval1.nelements() != crval2.nelements()) {
      set_error("The LinearCoordinates have differing numbers of reference values");
      return False;
   }
   if (cdelt1.nelements() != cdelt2.nelements()) {
      set_error("The LinearCoordinates have differing numbers of increments");
      return False;
   }
   if (crpix1.nelements() != crpix2.nelements()) {
      set_error("The LinearCoordinates have differing numbers of reference pixels");
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

   ostringstream oss;
   for (i=0; i<names1.nelements(); i++) {
      if (!exclude(i)) {
//
// the to/from FITS header conversion will convert linear axis
// names to upper case.  So to prevent that reflection failing,
// test on upper case only.  Also we need to strip off
// trailing white space (FITS length will be 8 chars)
//
         {
           String x1 = names1(i);
           x1.upcase();
           String x2 = names2(i);
           x2.upcase();
//
           Int i1 = x1.index(RXwhite,0);
           if (i1==-1) i1 = x1.length();
           Int i2 = x2.index(RXwhite,0);
           if (i2==-1) i2 = x2.length();
//
           String y1 = String(x1.before(i1));
           String y2 = String(x2.before(i2));
//
           if (y1 != y2) {
              oss << "The LinearCoordinates have differing axis names for axis "
                  << i;
              set_error(String(oss));
              return False;
           }
        }
      }
   }
   for (i=0; i<units1.nelements(); i++) {
      if (!exclude(i)) {

// This is bad.  After reading from FITS, the units are upper case.  
// So we cannot distinguish between a unit which is the same letter
// but case different if its been throgh FITS...

         {
           String x1 = units1(i);
           x1.upcase();
           String x2 = units2(i);
           x2.upcase();
//
           Int i1 = x1.index(RXwhite,0);
           if (i1==-1) i1 = x1.length();
           Int i2 = x2.index(RXwhite,0);
           if (i2==-1) i2 = x2.length();
//
           String y1 = String(x1.before(i1));
           String y2 = String(x2.before(i2));
           if (y1 != y2) {
             oss << "The LinearCoordinates have differing axis units for axis "
                 << i;
             set_error(String(oss));
             return False;
           }
        }
      }
   }
   for (i=0; i<crval1.nelements(); i++) {
      if (!exclude(i)) {
         if (!casa::near(crval1[i],crval2[i],tol)) {
            oss << "The LinearCoordinates have differing reference values for axis "
                << i;
            set_error(String(oss));
            return False;
         }
         if (!casa::near(cdelt1[i],cdelt2[i],tol)) {
            oss << "The LinearCoordinates have differing increments for axis "
                << i;
            set_error(String(oss));
            return False;
         }
         if (!casa::near(crpix1[i],crpix2[i],tol)) {
            oss << "The LinearCoordinates have differing reference values for axis "
                << i;
            set_error(String(oss));
            return False;
         }
      }
   }
//
// Check the matrix.
     
    Matrix<Double> pc1 = linearTransform();
    Matrix<Double> pc2 = lCoord.linearTransform();
    if (pc1.nrow()    != pc2.nrow()) {
       set_error(String("The LinearCoordinates have different PC matrix shapes"));
       return False;
    }
    if (pc1.ncolumn() != pc2.ncolumn()) {
       set_error(String("The LinearCoordinates have different PC matrix shapes"));
       return False;
    }
     
// Compare row by row.  An axis will turn up in the PC matrix in any row
// or column with that number.  E.g., values pertaining to axis "i" will
// be found in all entries of row "i" and all entries of column "i".
        
    for (uInt j=0; j<pc1.nrow(); j++) {
        Vector<Double> row1 = pc1.row(j);
        Vector<Double> row2 = pc2.row(j);
        if (!exclude(j)) {
            for (uInt i=0; i<row1.nelements(); i++) {
                if (!exclude(i)) {
                    if (!casa::near(row1(i),row2(i),tol)) {
                       set_error(String("The LinearCoordinates have different PC matrices"));
                       return False;
                    }
                }
            }
        }
    }
//
   return True;
}


Bool LinearCoordinate::save(RecordInterface &container,
			    const String &fieldName) const
{
    Bool ok = (!container.isDefined(fieldName));
    if (ok) {
	Record subrec;
	subrec.define("crval", referenceValue());
	subrec.define("crpix", referencePixel());
	subrec.define("cdelt", increment());
	subrec.define("pc", linearTransform());
	subrec.define("axes", worldAxisNames());
	subrec.define("units", worldAxisUnits());
//
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
//
    Record subrec(container.asRecord(fieldName));
    
// We should probably do more type-checking as well as checking
// for existence of the fields.

    Vector<Double> crval;
    subrec.get("crval", crval);
//
    if (!subrec.isDefined("crpix")) {
	return 0;
    }
    Vector<Double> crpix;
    subrec.get("crpix", crpix);
//
    if (!subrec.isDefined("cdelt")) {
	return 0;
    }
    Vector<Double> cdelt;
    subrec.get("cdelt", cdelt);
//
    if (!subrec.isDefined("pc")) {
	return 0;
    }
    Matrix<Double> pc;
    subrec.get("pc", pc);
//
    if (!subrec.isDefined("axes")) {
	return 0;
    }
    Vector<String> axes;
    subrec.get("axes", axes);
//
    if (!subrec.isDefined("units")) {
	return 0;
    }
    Vector<String> units;
    subrec.get("units", units);
//
    LinearCoordinate* pLinear = new LinearCoordinate(axes, units, crval, 
                                                     cdelt, pc, crpix);
    AlwaysAssert(pLinear, AipsError);
    return pLinear;
}



Coordinate *LinearCoordinate::clone() const
{
    return new LinearCoordinate(*this);
}




Coordinate* LinearCoordinate::makeFourierCoordinate (const Vector<Bool>& axes,
                                                     const Vector<Int>& shape) const
//        
// axes says which axes in the coordinate are to be transformed
// shape is the shape of the image for all axes in this coordinate 
//
{
   if (axes.nelements() != nPixelAxes()) {
      set_error ("Invalid number of specified axes");
      return 0;
   }
   uInt nT = 0;
   for (uInt i=0; i<nPixelAxes(); i++) if (axes[i]) nT++;
   if (nT==0) {
      set_error("You have not specified any axes to transform");
      return 0;
   }
//
   if (shape.nelements() != nPixelAxes()) {
      set_error("Invalid number of elements in shape");
      return 0;
   }

// Find the canonical input units that we should convert to.
// Find the Fourier coordinate names and units

   const Vector<String>& units(worldAxisUnits());
   const Vector<String>& names(worldAxisNames());
//
   Vector<String> unitsCanon(worldAxisUnits().copy());
   Vector<String> unitsOut = worldAxisUnits().copy();
   Vector<String> namesOut(worldAxisNames().copy());
//
   Vector<Double> crval2(referenceValue().copy());
   Vector<Double> crpix(referencePixel().copy());
   Vector<Double> scale(nPixelAxes(), 1.0);
//
   for (uInt i=0; i<nPixelAxes(); i++) {
      if (axes[i]) {
         crval2[i] = 0.0;
         Coordinate::fourierUnits(namesOut[i], unitsOut[i], unitsCanon[i], 
                                  Coordinate::LINEAR, i, units[i], names[i]);
         scale[i] = 1.0 / Double(shape[i]);
         crpix[i] = Int(shape[i] / 2);
      }
   }

// Make a copy of ourselves so we can change the units.  Otherwise we
// could make this function non-const and then put it back

   LinearCoordinate lc = *this;
   if (!lc.setWorldAxisUnits(unitsCanon)) {
      set_error ("Could not set world axis units");
      return 0;
   }

// Now create the new LinearCoordinate, using the LinearXform class
// to invert the coordinate matrices

   LinearXform linear(referencePixel(), increment(), linearTransform());
//
   String errMsg;
   LinearXform* pLinearF = linear.fourierInvert(errMsg, axes, crpix, scale);
   if (pLinearF==0) {
      set_error(errMsg);
      return 0;
   }
//
   LinearCoordinate* pLinear = new LinearCoordinate(namesOut, unitsOut, crval2, 
                                                    pLinearF->cdelt(),
                                                    pLinearF->pc(),
                                                    pLinearF->crpix());
   delete pLinearF;
   return pLinear;
}


void LinearCoordinate::makeWCS (::wcsprm& wcs, 
                                uInt naxis, const Vector<Double>& refPix,
                                const Vector<Double>& refVal, 
                                const Vector<Double>& incr, 
                                const Matrix<Double>& pc,
                                const Vector<String>& units,
                                const Vector<String>& names)
{
   AlwaysAssert(refPix.nelements() == naxis && refVal.nelements() == naxis &&
                incr.nelements() == naxis && pc.nrow() == naxis &&
                pc.ncolumn() == naxis && units.nelements()==naxis &&
                names.nelements()==naxis, AipsError);

// Set up wcs structure internals

    wcs.flag = -1;
    int iret = wcsini(1, naxis, &wcs);
    if (iret != 0) {
        String errmsg = "wcs wcsini_error: ";
        errmsg += wcsini_errmsg[iret];
        throw(AipsError(errmsg));
    }

// Assign values

    for (uInt i=0; i<naxis; i++) {
       wcs.crpix[i] = refPix[i];
       wcs.crval[i] = refVal[i];
       wcs.cdelt[i] = incr[i];
       strcpy (wcs.ctype[i], names[i].chars());
       strcpy (wcs.cunit[i], units[i].chars());
    }
    xFormToPC (wcs, pc);

// Set the rest of the wcs structure
       
    set_wcs (wcs);
}

void LinearCoordinate::copy(const LinearCoordinate &other)
{

// Copy WCS structure.  Flag 1 means allocate memory 

   if (wcs_p.flag != -1) {
      wcsfree(&wcs_p);        
   }
   int err = wcscopy (1, &(other.wcs_p), &wcs_p);
   if (err != 0) {
      String errmsg = "wcs wcscopy_error: ";
      errmsg += wcscopy_errmsg[err];
      throw(AipsError(errmsg));
   }
   set_wcs(wcs_p);
}

} //# NAMESPACE CASA - END

