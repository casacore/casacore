//# Coordinate.cc: this defines the Coordinate class
//# Copyright (C) 1997,1998,1999,2000
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
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayLogical.h>
#include <trial/Coordinates/Projection.h>
#include <aips/Exceptions/Error.h>
#include <aips/Logging/LogIO.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Measures/MDirection.h>
#include <aips/Quanta/Unit.h>
#include <aips/Utilities/Assert.h>

#include <iomanip.h>  
#include <strstream.h>


Coordinate::Coordinate()
{
}


Coordinate::Coordinate(const Coordinate& other)
: error_p(other.error_p)
{
}

Coordinate& Coordinate::operator=(const Coordinate& other)
{
   if (this != &other) {
      error_p = other.error_p;
   }
   return *this;
}
 

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


Bool Coordinate::toMix(Vector<Double>& worldOut,
                       Vector<Double>& pixelOut,
                       const Vector<Double>& worldIn,
                       const Vector<Double>& pixelIn,
                       const Vector<Bool>& worldAxes,   
                       const Vector<Bool>& pixelAxes,
                       const Vector<Double>&,
                       const Vector<Double>&) const
//
// Default implementation ok for non-coupled coordinated like
// Linear.  Coupled coordinates like DirectionCoordinate
// need their own implementation
//
{
   const uInt nWorld = worldAxes.nelements();
   const uInt nPixel = pixelAxes.nelements();
//
   AlwaysAssert(nWorld == nWorldAxes(), AipsError);
   AlwaysAssert(worldIn.nelements()==nWorld, AipsError);
   AlwaysAssert(nPixel == nPixelAxes(), AipsError);   
   AlwaysAssert(pixelIn.nelements()==nPixel, AipsError);   
//
   for (uInt i=0; i<nPixel; i++) {
      if (pixelAxes(i) && worldAxes(i)) {
         set_error("Coordinate::toMix - duplicate pixel/world axes");
         return False;
      }
      if (!pixelAxes(i) && !worldAxes(i)) {
         set_error("Coordinate::toMix - each axis must be either pixel or world");
         return False;
      }
   }
//
// Resize happens first time or maybe after an assignment
//
   if (world_tmp_p.nelements()!=nWorld) world_tmp_p.resize(nWorld);
   if (pixel_tmp_p.nelements()!=nPixel) pixel_tmp_p.resize(nPixel);
//
// Convert world to pixel.  Use  reference value unless
// world value given. Copy output pixels to output vector 
// and overwrite with any input pixel values that were given
//
   world_tmp_p = referenceValue().copy();
   for (uInt i=0; i<nWorld; i++) {
      if (worldAxes(i)) world_tmp_p(i) = worldIn(i);
   }
   if (!toPixel(pixel_tmp_p,world_tmp_p)) return False;
//
   if (pixelOut.nelements()!=nPixel) pixelOut.resize(nPixel);
   pixelOut = pixel_tmp_p.copy();
   for (uInt i=0; i<nPixel; i++) {
      if (pixelAxes(i)) pixelOut(i) = pixelIn(i);
   }
//
// Convert pixel to world.  Use reference pixel unless
// pixel value given. Copy output worlds to output vector 
// and overwrite with any input world values that were given
//
   pixel_tmp_p = referencePixel().copy();
   for (uInt i=0; i<nPixel; i++) {
      if (pixelAxes(i)) pixel_tmp_p(i) = pixelIn(i);
   }
   if (!toWorld(world_tmp_p,pixel_tmp_p)) return False;
   if (worldOut.nelements()!=nWorld) worldOut.resize(nWorld);
   worldOut = world_tmp_p.copy();
   for (uInt i=0; i<nWorld; i++) {
      if (worldAxes(i)) worldOut(i) = worldIn(i);
   }
//
   return True;
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
	if (allEQ(old, units)) {
	    return True;
	}
    }

    Bool ok = True;

    if (adjust) {
	String error;
	Vector<Double> factor;
	ok = find_scale_factor(error, factor, units, worldAxisUnits());
	if (ok) {
	    ok = setIncrement(increment() * factor);
	    if (ok) {
		ok = setReferenceValue(referenceValue() * factor);
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
                              Bool absolute,
                              Int defPrecScientific,
                              Int defPrecFixed,
                              Int ) const
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
                          Coordinate::formatType format, 
                          Double worldValue, 
                          uInt worldAxis, 
                          Bool absolute, 
                          Int precision,
                          Bool native) const
{
   AlwaysAssert(worldAxis < nWorldAxes(), AipsError);
 
// Check format
 
   Coordinate::formatType form = format;
   checkFormat (form, absolute);
   
   
// Set default precision
 
   Int prec = precision;
   if (prec < 0) getPrecision(prec, form, absolute, -1, -1, -1);
       
   
// Format and get units.  Always format in native coordinates
// here.  native is only active in derived classes
         
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


String Coordinate::formatQuantity (String& units,
                                   Coordinate::formatType format2, 
                                   const Quantum<Double>& worldValue, 
                                   uInt worldAxis, 
                                   Bool absolute, 
                                   Int precision,
                                   Bool native) const
{
   AlwaysAssert(worldAxis < nWorldAxes(), AipsError);

// Use derived class formatter

   String unit = worldAxisUnits()(worldAxis);
   return format(units, format2, worldValue.getValue(Unit(unit)),
                 worldAxis, absolute, precision, native);
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
    error_p = errorMsg;
}





Vector<String> Coordinate::make_Direction_FITS_ctype (const Projection& proj,
                                                      const Vector<String>& axisNames,
                                                      Double refLat, Bool printError) const
//
// Reflat in radians
//
{
    LogIO os(LogOrigin("Coordinate", "make_Direction_FITS_ctype", WHERE));
    Vector<String> ctype(2);
    Vector<Double> projParameters = proj.parameters();

//
    Bool isNCP = False;
    for (uInt i=0; i<2; i++) {
       String name = axisNames(i);
       while (name.length() < 4) {
           name += "-";
       }
       switch(proj.type()) {
          case Projection::TAN:  // Fallthrough
          case Projection::ARC:
              name = name + "-" + proj.name();
              break;
          case Projection::SIN:

// This is either "real" SIN or NCP

              AlwaysAssert(projParameters.nelements() == 2, AipsError);
              if (::near(projParameters(0), 0.0) && ::near(projParameters(1), 0.0)) {
                  // True SIN
                  name = name + "-" + proj.name();
              } else {
                  // NCP?
                  // From Greisen and Calabretta
                  // The potential divide by zero should never occur in
                  // a real DirectionCoordinate as you better not have observed at
                  // lat=0 with an EW array
                  if (::near(projParameters(0), 0.0) &&
                      ::near(projParameters(1), 1.0/tan(refLat))) {
                      // Is NCP
                      isNCP = True;
                      name = name + "-NCP";
                  } else {
                      // Doesn't appear to be NCP
                      // Only print this once
                      if (!isNCP) {
                          os << LogIO::WARN << "SIN projection with non-zero"
                              " projp does not appear to be NCP." << endl <<
                              "However, assuming NCP anyway." << LogIO::POST;

                      }
                      name = name + "-NCP";
                      isNCP = True;
                  }
              }
              break;
          default:
             if (i == 0) {

// Only print the message once for long/lat
                if (printError) {
                   os << LogIO::WARN << proj.name()
                      << " is not known to standard FITS (it is known to WCS)."
                      << LogIO::POST;
                }
             }
             name = name + "-" + proj.name();
             break;
       }
       ctype(i) = name;
    }
    return ctype;
}


Coordinate* Coordinate::makeFourierCoordinate (const Vector<Bool>& axes,
                                               const Vector<Int>& shape)  const
{
   String tmp = String("Coordinates of type ") + showType() + String(" cannot be Fourier Transformed");
   throw(AipsError(tmp));
}


void Coordinate::fourierUnits (String& nameOut, String& unitOut, String& unitInCanon,
                               Coordinate::Type type, Int axis,   
                               const String& unitIn,
                               const String& nameIn) const

//
// A disgusting fudgy routine to work out some nice names and units
// Fourier coordinates.  Rather limited in its knowledge currently.
//
{
   Unit time("s");
   Unit freq("Hz");
   Unit rad("rad");
   Unit unitIn2(unitIn);
//
   if (type==Coordinate::DIRECTION) {
      if (unitIn2==rad) {
         unitInCanon = String("rad");
         if (axis==0) {
            nameOut = String("UU");
         } else if (axis==1) {
            nameOut = String("VV");
         } else {
            throw(AipsError("Illegal DirectionCoordinate axis"));
         }
         unitOut = String("lambda");
      } else {
         nameOut = String("Inverse(") + nameIn + String(")");
         unitOut = String("1/") + unitIn;
         unitInCanon = unitIn;
      }
   } else if (type==Coordinate::LINEAR ||
              type==Coordinate::SPECTRAL ||
              type==Coordinate::TABULAR) {
      if (unitIn2==freq) {
         nameOut = String("Time");
         unitOut = String("s");
         unitInCanon = "Hz";
      } else if (unitIn2==time) {
         nameOut = String("Frequency");
         unitOut = String("Hz");
         unitInCanon = "s";
      } else {
         nameOut = String("Inverse(") + nameIn + String(")");
         unitOut = String("1/") + unitIn;
         unitInCanon = unitIn;
      }
   } else if (type==Coordinate::STOKES) {
      throw (AipsError("Cannot provide Fourier coordinate name for Stokes coordinate"));
   } else if (type==Coordinate::COORDSYS) {
      throw (AipsError("Cannot provide Fourier coordinate name for CoordinateSystem coordinate"));
   } else {
      nameOut = String("Inverse(") + nameIn + String(")");
      unitOut = String("1/") + unitIn;
      unitInCanon = unitIn;
   }
}

