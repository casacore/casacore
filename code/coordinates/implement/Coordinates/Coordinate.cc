//# Coordinate.cc: this defines the Coordinate class
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

#include <trial/Coordinates/Coordinate.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/Vector.h>
#include <aips/Utilities/Assert.h>
#include <aips/Quanta/Unit.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayLogical.h>

#include <iomanip.h>  
#include <strstream.h>


Coordinate::~Coordinate()
{
    // Nothing
}

uInt Coordinate::toWorldMany(Matrix<Double> &world, 
			     const Matrix<Double> &pixel, 
			     Vector<Int> &failures) const
{
    uInt nworld = nWorldAxes();
    uInt npixel = nPixelAxes();

    AlwaysAssert(nworld == world.nrow() && npixel == pixel.nrow(), AipsError);

    uInt ntransforms = world.ncolumn();
    AlwaysAssert(ntransforms == pixel.ncolumn(), AipsError);

    Bool deleteWorld;
    Double *worldptr = world.getStorage(deleteWorld);
    Double *wp = worldptr;
    Bool deletePixel;
    const Double *pixelptr = pixel.getStorage(deletePixel);
    const Double *pp = pixelptr;
    
    Bool deleteWorldTmp;
    Bool deletePixelTmp;
    Vector<Double> worldtmp(nworld);
    Double *worldtmpptr = worldtmp.getStorage(deleteWorldTmp);
    Vector<Double> pixeltmp(npixel);
    Double *pixeltmpptr = pixeltmp.getStorage(deletePixelTmp);

    String errormsg;
    uInt nerror = 0;
    for (uInt i=0; i<ntransforms; i++) {
	for (uInt j=0; j<npixel; j++) {
	    pixeltmpptr[j] = *pp++;
	}
	Bool ok = toWorld(worldtmp, pixeltmp);
	if (!ok) {
	    nerror++;
	    if (nerror > failures.nelements()) {
		failures.resize(2*nerror, True);
	    }
	    failures(nerror-1) = i;
	    if (nerror == 1) {
		errormsg = errorMessage(); // Save the first error message
	    }
	} else {
	    for (uInt j=0; j<nworld; j++) {
		*wp++ = worldtmpptr[j];
	    }
	}
    }
    if (nerror != 0) {
	set_error(errormsg); // put back the first error
    }

    world.putStorage(worldptr, deleteWorld);
    pixel.freeStorage(pixelptr, deletePixel);
    worldtmp.putStorage(worldtmpptr, deleteWorldTmp);
    pixeltmp.putStorage(pixeltmpptr, deletePixelTmp);
    
    return nerror;
}

uInt Coordinate::toPixelMany(Matrix<Double> &pixel, 
			     const Matrix<Double> &world, 
			     Vector<Int> &failures) const
{
    uInt nworld = nWorldAxes();
    uInt npixel = nPixelAxes();

    AlwaysAssert(nworld == world.nrow() && npixel == pixel.nrow(), AipsError);

    uInt ntransforms = world.ncolumn();
    AlwaysAssert(ntransforms == pixel.ncolumn(), AipsError);

    Bool deleteWorld;
    const Double *worldptr = world.getStorage(deleteWorld);
    const Double *wp = worldptr;
    Bool deletePixel;
    Double *pixelptr = pixel.getStorage(deletePixel);
    Double *pp = pixelptr;
    
    Bool deleteWorldTmp;
    Bool deletePixelTmp;
    Vector<Double> worldtmp(nworld);
    Double *worldtmpptr = worldtmp.getStorage(deleteWorldTmp);
    Vector<Double> pixeltmp(npixel);
    Double *pixeltmpptr = pixeltmp.getStorage(deletePixelTmp);

    String errormsg;
    uInt nerror = 0;
    for (uInt i=0; i<ntransforms; i++) {
	for (uInt j=0; j<npixel; j++) {
	    worldtmpptr[j] = *wp++;
	}
	Bool ok = toPixel(pixeltmp, worldtmp);
	if (!ok) {
	    nerror++;
	    if (nerror > failures.nelements()) {
		failures.resize(2*nerror, True);
	    }
	    failures(nerror-1) = i;
	    if (nerror == 1) {
		errormsg = errorMessage(); // Save the first error message
	    }
	} else {
	    for (uInt j=0; j<nworld; j++) {
		*pp++ = pixeltmpptr[j];
	    }
	}
    }
    if (nerror != 0) {
	set_error(errormsg); // put back the first error
    }

    world.freeStorage(worldptr, deleteWorld);
    pixel.putStorage(pixelptr, deletePixel);
    worldtmp.putStorage(worldtmpptr, deleteWorldTmp);
    pixeltmp.putStorage(pixeltmpptr, deletePixelTmp);
    
    return nerror;
}

// Does everything except set the units vector, which must be done in the derived class.
Bool Coordinate::setWorldAxisUnits(const Vector<String> &units, 
					  Bool adjust)
{
    if (units.nelements() != nWorldAxes()) {
	set_error("Wrong number of elements in units vector");
	return False;
    } else {
	// If the units are unchanged just return True.
	Vector<String> old = worldAxisUnits();
	if (allEQ(old.ac(), units.ac())) {
	    return True;
	}
    }

    Bool ok = True;

    if (adjust) {
	String error;
	Vector<Double> factor;
	ok = find_scale_factor(error, factor, units, worldAxisUnits());
	if (ok) {
	    ok = setIncrement(increment().ac() * factor.ac());
	    if (ok) {
		ok = setReferenceValue(referenceValue().ac() * factor.ac());
	    }
	} else {
	    set_error(error);
	}
    }

    return ok;
}

void Coordinate::checkFormat(Coordinate::formatType& format, 
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


void Coordinate::getPrecision(Int &precision,
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

String Coordinate::format(String& units,
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


// after = factor * before
Bool Coordinate::find_scale_factor(String &error, Vector<Double> &factor, 
				   const Vector<String> &units, 
				   const Vector<String> &oldUnits)
{
    factor.resize(units.nelements());
    Bool ok = ToBool(units.nelements() == oldUnits.nelements());
    if (! ok) {
	error = "units and oldUnits are different sizes!";
    } else {
	// Try to find the scaling factors between the old and new units
	uInt n = units.nelements();
	for (uInt i=0; i<n && ok; i++) {
	    if (UnitVal::check(oldUnits(i)) && UnitVal::check(units(i))) {
		Unit before = oldUnits(i);
		Unit after = units(i);
		ok = ToBool(before.getValue() == after.getValue());
		if (!ok) {
		    error = "Units are not compatible dimensionally";
		} else {
		    factor(i) = before.getValue().getFac() / 
			after.getValue().getFac();
		}
	    } else {
		ok = False;
		error = "Unknown unit - cannot calculate scaling";
	    }
	}
    }
    return ok;
}



void Coordinate::set_error(const String &errorMsg) const
{
    // Cast away const until we have mutable
    Coordinate *This = (Coordinate *)this;
    This->error_p = errorMsg;
}
