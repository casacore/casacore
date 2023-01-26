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

#include <casacore/coordinates/Coordinates/LinearCoordinate.h>
#include <casacore/coordinates/Coordinates/LinearXform.h>

#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Utilities/LinearSearch.h>
#include <casacore/casa/Utilities/Regex.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/casa/Quanta/UnitMap.h>
#include <casacore/casa/Quanta/Unit.h>

#include <casacore/casa/sstream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN


LinearCoordinate::LinearCoordinate(uint32_t naxis)
: Coordinate()
{ 
    Vector<double> refVal(naxis), refPix(naxis), incr(naxis);
    Matrix<double> pc(naxis,naxis);
    Vector<String> names(naxis), units(naxis);
//
    pc = 0.0;
    for (uint32_t i=0; i<naxis; i++) {
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
				   const Vector<double>& refVal,
				   const Vector<double>& inc,
				   const Matrix<double>& pc,
				   const Vector<double>& refPix)
: Coordinate()
{
    uint32_t naxis = names.nelements();
    makeWCS (wcs_p, naxis, refPix, refVal, inc, pc, units, names);
//
    setDefaultWorldMixRanges();
}


LinearCoordinate::LinearCoordinate(const Vector<String>& names,
                                   const Vector<Quantum<double> >& refVal,
                                   const Vector<Quantum<double> >& inc,   
                                   const Matrix<double>& pc,
                                   const Vector<double>& refPix)
: Coordinate()
{

// Check dimensions

    const uint32_t n = names.nelements();
    AlwaysAssert(refVal.nelements() == n &&
		 inc.nelements() == n &&
		 pc.nrow() == n &&
		 pc.ncolumn() == n &&
		 refPix.nelements() == n, AipsError);
//
    Vector<double> cdelt(n), crval(n);
    Vector<String> units(n);
    for (uint32_t i=0; i<n; i++) {
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


LinearCoordinate::LinearCoordinate(const ::wcsprm& wcs, bool oneRel)
: Coordinate()
{

// Test only holds linear wcs structure

// Copy WCS structure.  Flag 1 means allocate memory 

   wcs_p.flag = -1;
   copy_wcs(wcs, wcs_p);
   set_wcs(wcs_p);
//
   for (int32_t i=0; i<wcs_p.naxis; i++) {

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

uint32_t LinearCoordinate::nPixelAxes() const
{
    return wcs_p.naxis;
}

uint32_t LinearCoordinate::nWorldAxes() const
{
    return nPixelAxes();
}

bool LinearCoordinate::toWorld(Vector<double> &world, 
			       const Vector<double> &pixel, bool) const
{
   return toWorldWCS (world, pixel, wcs_p);
}

bool LinearCoordinate::toPixel(Vector<double> &pixel, 
			       const Vector<double> &world) const
{
   return toPixelWCS (pixel, world, wcs_p);
}


Vector<String> LinearCoordinate::worldAxisNames() const
{
    const uint32_t n = nPixelAxes();
    Vector<String> tmp(n);
    for (uint32_t i=0; i<n; i++) {
       tmp[i] = String(wcs_p.ctype[i]);
    }
    return tmp;
}

Vector<String> LinearCoordinate::worldAxisUnits() const
{
    const uint32_t n = nWorldAxes();
    Vector<String> tmp(n);
    for (uint32_t i=0; i<n; i++) {
       tmp[i] = String(wcs_p.cunit[i]);
    }
    return tmp;
}

Vector<double> LinearCoordinate::referenceValue() const
{
    const uint32_t n = nWorldAxes();
    Vector<double> tmp(n);
    for (uint32_t i=0; i<n; i++) {
       tmp[i] = wcs_p.crval[i];
    }
    return tmp;
}

Vector<double> LinearCoordinate::increment() const
{
    const uint32_t n = nWorldAxes();
    Vector<double> tmp(n);
    for (uint32_t i=0; i<n; i++) {
       tmp[i] = wcs_p.cdelt[i];
    }
    return tmp;
}

Matrix<double> LinearCoordinate::linearTransform() const
{
   Matrix<double> tmp;
   pcToXform (tmp, wcs_p);
   return tmp;
}

Vector<double> LinearCoordinate::referencePixel() const
{
    const uint32_t n = nPixelAxes();
    Vector<double> tmp(n);
    for (uint32_t i=0; i<n; i++) {
       tmp[i] = wcs_p.crpix[i];
    }
    return tmp;
}

bool LinearCoordinate::setWorldAxisNames(const Vector<String> &names)
{
    bool ok = (names.nelements() == nWorldAxes());
    if (!ok) {
       set_error("names vector has the wrong size");
    } else {
       for (uint32_t i=0; i<nWorldAxes(); i++) {
          strcpy (wcs_p.ctype[i], names[i].chars());
       }
    }
//
    return ok;
}

bool LinearCoordinate::setWorldAxisUnits(const Vector<String> &units)
{
    Vector<double> d1 = increment();
    bool ok = Coordinate::setWorldAxisUnits(units);
    if (ok) {
       for (uint32_t i=0; i<nWorldAxes(); i++) {
          strcpy (wcs_p.cunit[i], units[i].chars());
       }

// Presently wcs does not actually do anything with units

       set_wcs(wcs_p);
    }
    return ok;
}


bool LinearCoordinate::overwriteWorldAxisUnits(const Vector<String> &units)
{
   bool ok = units.nelements() == nWorldAxes();
   if (ok) {
      for (uint32_t i=0; i<nWorldAxes(); i++) {
         strcpy (wcs_p.cunit[i], units[i].chars());
      }
   } else {
      set_error ("units vector has the wrong size");
   }
   return ok;
}

bool LinearCoordinate::setReferencePixel(const Vector<double> &refPix)
{
    bool ok = (refPix.nelements() == nWorldAxes());
    if (! ok) {
	set_error("reference pixel vector has the wrong size");
    } else {
      for (uint32_t i=0; i<nWorldAxes(); i++) {
         wcs_p.crpix[i] = refPix[i];
      }
      set_wcs(wcs_p);
    }
    return ok;
}

bool LinearCoordinate::setLinearTransform(const Matrix<double> &pc)
{
    bool ok = (pc.nrow() == nWorldAxes() && 
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

bool LinearCoordinate::setIncrement(const Vector<double> &inc)
{
    bool ok = (inc.nelements() == nWorldAxes());
    if (! ok) {
	set_error("increment vector has the wrong size");
    } else {
       for (uint32_t i=0; i<nWorldAxes(); i++) {
          wcs_p.cdelt[i] = inc[i];
       }
       set_wcs(wcs_p);
    }

    return ok;
}

bool LinearCoordinate::setReferenceValue(const Vector<double> &refval)
{
    bool ok = (refval.nelements() == nWorldAxes());
    if (! ok) {
	set_error("reference value vector has the wrong size");
    } else {
       for (uint32_t i=0; i<nWorldAxes(); i++) {
          wcs_p.crval[i] = refval[i];
       }
       set_wcs(wcs_p);
    }
//
    return ok;
}


bool LinearCoordinate::near(const Coordinate& other,
                            double tol) const
{
   Vector<int32_t> excludeAxes;
   return near(other, excludeAxes, tol);
}


bool LinearCoordinate::near(const Coordinate& other,
                            const Vector<int32_t>& excludeAxes,
                            double tol) const
{
   if (other.type() != this->type()) {
      set_error("Comparison is not with another LinearCoordinate");
      return false;
   }

   const LinearCoordinate& lCoord = dynamic_cast<const LinearCoordinate&>(other);

// Check descriptor vector lengths

   Vector<String> names1(worldAxisNames());
   Vector<String> names2(lCoord.worldAxisNames());
   Vector<String> units1(worldAxisUnits());
   Vector<String> units2(lCoord.worldAxisUnits());
   Vector<double> crval1(referenceValue());
   Vector<double> crval2(lCoord.referenceValue());
   Vector<double> cdelt1(increment());
   Vector<double> cdelt2(lCoord.increment());
   Vector<double> crpix1(referencePixel());
   Vector<double> crpix2(lCoord.referencePixel());
//
   if (names1.nelements() != names2.nelements()) {
      set_error("The LinearCoordinates have differing numbers of world axis names");
      return false;
   }
   if (units1.nelements() != units2.nelements()) {
      set_error("The LinearCoordinates have differing numbers of axis units");
      return false;
   }
   if (crval1.nelements() != crval2.nelements()) {
      set_error("The LinearCoordinates have differing numbers of reference values");
      return false;
   }
   if (cdelt1.nelements() != cdelt2.nelements()) {
      set_error("The LinearCoordinates have differing numbers of increments");
      return false;
   }
   if (crpix1.nelements() != crpix2.nelements()) {
      set_error("The LinearCoordinates have differing numbers of reference pixels");
      return false;
   }

// Number of pixel and world axes is the same for a LinearCoordinate
// Add an assertion check should this change.  Other code in LC has 
// checked that all the vectors we are comparing have the same 
// length as nPixelAxes()

   AlwaysAssert(nPixelAxes()==nWorldAxes(), AipsError);
   Vector<bool> exclude(nPixelAxes());
   exclude = false;
   bool found;
   uint32_t j = 0;
   uint32_t i;
   for (i=0; i<nPixelAxes(); i++) {
      if (linearSearch(found, excludeAxes, int32_t(i), excludeAxes.nelements()) >= 0)
        exclude(j++) = true;
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
           int32_t i1 = x1.index(RXwhite,0);
           if (i1==-1) i1 = x1.length();
           int32_t i2 = x2.index(RXwhite,0);
           if (i2==-1) i2 = x2.length();
//
           String y1 = String(x1.before(i1));
           String y2 = String(x2.before(i2));
//
           if (y1 != y2) {
              oss << "The LinearCoordinates have differing axis names for axis "
                  << i;
              set_error(String(oss));
              return false;
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
           int32_t i1 = x1.index(RXwhite,0);
           if (i1==-1) i1 = x1.length();
           int32_t i2 = x2.index(RXwhite,0);
           if (i2==-1) i2 = x2.length();
//
           String y1 = String(x1.before(i1));
           String y2 = String(x2.before(i2));
           if (y1 != y2) {
             oss << "The LinearCoordinates have differing axis units for axis "
                 << i;
             set_error(String(oss));
             return false;
           }
        }
      }
   }
   for (i=0; i<crval1.nelements(); i++) {
      if (!exclude(i)) {
         if (!casacore::near(crval1[i],crval2[i],tol)) {
            oss << "The LinearCoordinates have differing reference values for axis "
                << i << ", " << crval1[i] << " vs. " << crval2[i];
            set_error(String(oss));
            return false;
         }
         if (!casacore::near(cdelt1[i],cdelt2[i],tol)) {
            oss << "The LinearCoordinates have differing increments for axis "
                << i << ", " << cdelt1[i] << " vs. " << cdelt2[i];
            set_error(String(oss));
            return false;
         }
         if (!casacore::near(crpix1[i],crpix2[i],tol)) {
            oss << "The LinearCoordinates have differing reference values for axis "
                << i << ", " << crpix1[i] << " vs. " << crpix2[i];
            set_error(String(oss));
            return false;
         }
      }
   }
//
// Check the matrix.
     
    Matrix<double> pc1 = linearTransform();
    Matrix<double> pc2 = lCoord.linearTransform();
    if (pc1.nrow()    != pc2.nrow()) {
       set_error(String("The LinearCoordinates have different PC matrix shapes"));
       return false;
    }
    if (pc1.ncolumn() != pc2.ncolumn()) {
       set_error(String("The LinearCoordinates have different PC matrix shapes"));
       return false;
    }
     
// Compare row by row.  An axis will turn up in the PC matrix in any row
// or column with that number.  E.g., values pertaining to axis "i" will
// be found in all entries of row "i" and all entries of column "i".
        
    for (uint32_t j=0; j<pc1.nrow(); j++) {
        Vector<double> row1 = pc1.row(j);
        Vector<double> row2 = pc2.row(j);
        if (!exclude(j)) {
            for (uint32_t i=0; i<row1.nelements(); i++) {
                if (!exclude(i)) {
                    if (!casacore::near(row1(i),row2(i),tol)) {
                       set_error(String("The LinearCoordinates have different PC matrices"));
                       return false;
                    }
                }
            }
        }
    }
//
   return true;
}


bool LinearCoordinate::save(RecordInterface &container,
			    const String &fieldName) const
{
    bool ok = (!container.isDefined(fieldName));
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

    Vector<double> crval(subrec.toArrayDouble("crval"));
//
    if (!subrec.isDefined("crpix")) {
	return 0;
    }
    Vector<double> crpix(subrec.toArrayDouble("crpix"));
//
    if (!subrec.isDefined("cdelt")) {
	return 0;
    }
    Vector<double> cdelt(subrec.toArrayDouble("cdelt"));
//
    if (!subrec.isDefined("pc")) {
	return 0;
    }
    Matrix<double> pc(subrec.toArrayDouble("pc"));
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




Coordinate* LinearCoordinate::makeFourierCoordinate (const Vector<bool>& axes,
                                                     const Vector<int32_t>& shape) const
//        
// axes says which axes in the coordinate are to be transformed
// shape is the shape of the image for all axes in this coordinate 
//
{
   if (axes.nelements() != nPixelAxes()) {
      set_error ("Invalid number of specified axes");
      return 0;
   }
   uint32_t nT = 0;
   for (uint32_t i=0; i<nPixelAxes(); i++) if (axes[i]) nT++;
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
   Vector<double> crval2(referenceValue().copy());
   Vector<double> crpix(referencePixel().copy());
   Vector<double> scale(nPixelAxes(), 1.0);
//
   for (uint32_t i=0; i<nPixelAxes(); i++) {
      if (axes[i]) {
         crval2[i] = 0.0;
         Coordinate::fourierUnits(namesOut[i], unitsOut[i], unitsCanon[i], 
                                  Coordinate::LINEAR, i, units[i], names[i]);
         scale[i] = 1.0 / double(shape[i]);
         crpix[i] = int32_t(shape[i] / 2);
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
                                uint32_t naxis, const Vector<double>& refPix,
                                const Vector<double>& refVal, 
                                const Vector<double>& incr, 
                                const Matrix<double>& pc,
                                const Vector<String>& units,
                                const Vector<String>& names)
{
   AlwaysAssert(refPix.nelements() == naxis && refVal.nelements() == naxis &&
                incr.nelements() == naxis && pc.nrow() == naxis &&
                pc.ncolumn() == naxis && units.nelements()==naxis &&
                names.nelements()==naxis, AipsError);

// Set up wcs structure internals

    wcs.flag = -1;
    init_wcs(wcs, naxis);

// Assign values

    for (uint32_t i=0; i<naxis; i++) {
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
   copy_wcs(other.wcs_p, wcs_p);
   set_wcs(wcs_p);
}

} //# NAMESPACE CASACORE - END

