//# DirectionCoordinate.cc: this defines the DirectionCoordinate class
//# Copyright (C) 1997,1998,1999,2000,2001,2002
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

#include <wcslib/wcs.h>
#include <wcslib/cel.h>
#include <wcslib/proj.h>
#include <wcslib/cylfix.h>

#include <trial/Coordinates/DirectionCoordinate.h>
#include <trial/Coordinates/LinearCoordinate.h>
#include <trial/Coordinates/Coordinate.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Containers/Record.h>
#include <aips/Logging/LogIO.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Measures/MeasConvert.h>
#include <aips/Quanta/MVAngle.h>
#include <aips/Quanta/Quantum.h>
#include <aips/Quanta/Unit.h>
#include <aips/Quanta/RotMatrix.h>
#include <aips/Quanta/Euler.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/LinearSearch.h>
#include <aips/Utilities/String.h>

#include <aips/iomanip.h>  
#include <aips/strstream.h>


DirectionCoordinate::DirectionCoordinate()
: Coordinate(),
  type_p(MDirection::J2000), 
  conversionType_p(type_p), 
  projection_p(Projection(Projection::CAR)),
  celprm_p(0), 
  prjprm_p(0), 
  wcs_p(0), 
  linear_p(1),
  names_p(2), 
  units_p(2), 
  prefUnits_p(2), 
  worldMin_p(0),
  worldMax_p(0),
  pConversionMachineTo_p(0),
  pConversionMachineFrom_p(0),
  canDoToMix_p(True),
  canDoToMixErrorMsg_p("")
{
    Matrix<Double> xform(2,2); 
    xform = 0.0;
    xform.diagonal() = 1.0;
    makeDirectionCoordinate (0.0, 0.0, 1.0, 1.0,
                             xform, 0.0, 0.0, 999.0, 999.0);
    setDefaultWorldMixRanges();
    setRotationMatrix();
}

DirectionCoordinate::DirectionCoordinate(MDirection::Types directionType,
                                         const Projection &projection,
                                         Double refLong, Double refLat,
                                         Double incLong, Double incLat,
                                         const Matrix<Double> &xform,
                                         Double refX, Double refY,
                                         Double longPole, Double latPole)
: Coordinate(),
  type_p(directionType), 
  conversionType_p(type_p), 
  projection_p(projection),
  celprm_p(0), 
  prjprm_p(0), 
  wcs_p(0), 
  linear_p(1),
  names_p(axisNames(directionType).copy()),
  units_p(2),
  prefUnits_p(2), 
  worldMin_p(0),
  worldMax_p(0),
  pConversionMachineTo_p(0),
  pConversionMachineFrom_p(0),
  canDoToMix_p(True),
  canDoToMixErrorMsg_p("")
{
    makeDirectionCoordinate (refLong, refLat, incLong, incLat,
                             xform, refX, refY, longPole, latPole);
    setDefaultWorldMixRanges();
    setRotationMatrix();
}

DirectionCoordinate::DirectionCoordinate(MDirection::Types directionType,
                                         const Projection &projection,  
                                         const Quantum<Double>& refLong,
                                         const Quantum<Double>& refLat,  
                                         const Quantum<Double>& incLong,
                                         const Quantum<Double>& incLat,
                                         const Matrix<Double> &xform,
                                         Double refX, Double refY,
                                         const Quantum<Double>& longPole,
                                         const Quantum<Double>& latPole)
: Coordinate(),
  type_p(directionType), 
  conversionType_p(type_p), 
  projection_p(projection),
  celprm_p(0), 
  prjprm_p(0), 
  wcs_p(0), 
  linear_p(1),
  names_p(axisNames(directionType).copy()),
  units_p(2),
  prefUnits_p(2), 
  worldMin_p(0),
  worldMax_p(0),
  pConversionMachineTo_p(0),
  pConversionMachineFrom_p(0),
  canDoToMix_p(True),
  canDoToMixErrorMsg_p("")
{
   Unit rad("rad");
//
   if (!refLong.isConform(rad)) {
      throw(AipsError("Specified longitude is not angular"));
   }
   if (!refLat.isConform(rad)) {
      throw(AipsError("Specified latitude is not angular"));
   }
   if (!incLong.isConform(rad)) {
      throw(AipsError("Specified longitude increment is not angular"));
   }
   if (!incLat.isConform(rad)) {
      throw(AipsError("Specified latitude increment is not angular"));
   }
//
   Double lon = refLong.getValue(rad);
   Double lat = refLat.getValue(rad);
   Double dlon = incLong.getValue(rad);
   Double dlat = incLat.getValue(rad);
//
   Double dLongPole = longPole.getValue();
   Double dLatPole = latPole.getValue();
//
// Any value >=999 is taken as the default, regardless of units
//
   if (dLongPole < 999.0) {                 
      dLongPole = longPole.getValue(rad);
   } else {
      dLongPole = 999.0;
   }
   if (dLatPole < 999.0) {
      dLatPole = latPole.getValue(rad);
   } else {
      dLatPole = 999.0;
   }
//
   makeDirectionCoordinate (lon, lat, dlon, dlat, xform, refX, refY,
                            dLongPole, dLatPole);
   setDefaultWorldMixRanges();
   setRotationMatrix();
}


DirectionCoordinate::DirectionCoordinate(const DirectionCoordinate &other)
: Coordinate(other),
  type_p(other.type_p), 
  conversionType_p(other.conversionType_p), 
  projection_p(other.projection_p),
  celprm_p(0), 
  prjprm_p(0), 
  wcs_p(0), 
  linear_p(other.linear_p),
  to_degrees_p(other.to_degrees_p.copy()),
  to_radians_p(other.to_radians_p.copy()),
  names_p(other.names_p.copy()), 
  units_p(other.units_p.copy()),
  prefUnits_p(other.prefUnits_p.copy()), 
  worldMin_p(other.worldMin_p.copy()),
  worldMax_p(other.worldMax_p.copy()),
  rot_p(other.rot_p),
  pConversionMachineTo_p(0),
  pConversionMachineFrom_p(0),
  canDoToMix_p(other.canDoToMix_p),
  canDoToMixErrorMsg_p(other.canDoToMixErrorMsg_p)
{
    copy_celprm_and_prjprm(celprm_p, prjprm_p, wcs_p, c_ctype_p, 
                           c_crval_p, other.celprm_p, other.prjprm_p,
                           other.wcs_p, other.c_ctype_p, other.c_crval_p);
    makeConversionMachines();
}


DirectionCoordinate &DirectionCoordinate::operator=(const DirectionCoordinate &other)
{
    if (this != &other) {
        Coordinate::operator=(other);
//
	type_p = other.type_p;
	conversionType_p = other.conversionType_p;
	projection_p = other.projection_p;
//
	DirectionCoordinate::copy_celprm_and_prjprm(celprm_p, prjprm_p, wcs_p, 
                               c_ctype_p, c_crval_p,
			       other.celprm_p, other.prjprm_p,
                               other.wcs_p, 
			       other.c_ctype_p, other.c_crval_p);
	linear_p = other.linear_p;
	names_p = other.names_p;
	units_p = other.units_p;
	prefUnits_p = other.prefUnits_p;
        worldMin_p = other.worldMin_p;
        worldMax_p = other.worldMax_p;
	to_degrees_p = other.to_degrees_p.copy();
	to_radians_p = other.to_radians_p.copy();
        rot_p = other.rot_p;
//
        if (pConversionMachineTo_p) {
           delete pConversionMachineTo_p;
           pConversionMachineTo_p = 0;
        }
        if (pConversionMachineFrom_p) {
           delete pConversionMachineFrom_p;
           pConversionMachineFrom_p = 0;
        }
        makeConversionMachines();
//
        canDoToMix_p = other.canDoToMix_p;
        canDoToMixErrorMsg_p = other.canDoToMixErrorMsg_p;
    }
    return *this;
}

DirectionCoordinate::~DirectionCoordinate()
{
    delete celprm_p; 
    celprm_p = 0;
//
    delete prjprm_p; 
    prjprm_p = 0;
//
    delete wcs_p;
    wcs_p = 0;
//
    if (pConversionMachineTo_p) {
       delete pConversionMachineTo_p;
       pConversionMachineTo_p = 0;
    }
    if (pConversionMachineFrom_p) {
       delete pConversionMachineFrom_p;
       pConversionMachineFrom_p = 0;
    }
}

Coordinate::Type DirectionCoordinate::type() const
{
    return Coordinate::DIRECTION;
}

String DirectionCoordinate::showType() const
{
    return String("Direction");
}


uInt DirectionCoordinate::nPixelAxes() const
{
    return 2;
}

uInt DirectionCoordinate::nWorldAxes() const
{
    return 2;
}

void DirectionCoordinate::setReferenceConversion (MDirection::Types type)
{
// See if something to do

   if (conversionType_p==type) return;

// If conversion type the same as the native type, just 
// remove the machines

   conversionType_p = type;
   if (pConversionMachineTo_p) {
      delete pConversionMachineTo_p;      
      pConversionMachineTo_p = 0;
   }
   if (pConversionMachineFrom_p) {
      delete pConversionMachineFrom_p;
      pConversionMachineFrom_p = 0;
   }
   if (conversionType_p == type_p) return;

// Now make new machines as needed

   makeConversionMachines();
}

Bool DirectionCoordinate::toWorld(Vector<Double> &world,
 				  const Vector<Double> &pixel) const
{
 // Temporaries

    double d_phi, d_theta, d_x, d_y, d_lng, d_lat;
//
    world.resize(2);
    AlwaysAssert(pixel.nelements() == 2, AipsError);

// world contains linear xformed numbers

    String errorMsg;
    Bool ok = linear_p.reverse(world, pixel, errorMsg);
    if (ok) {
	d_x = world(0);
        d_y = world(1);
//
	int errnum = celrev(pcodes[projection_p.type()],
			    d_x, d_y, prjprm_p, &d_phi, &d_theta,
			    celprm_p, &d_lng, &d_lat);
	if (errnum==0) {
	    world(0) = d_lng;
	    world(1) = d_lat;

// Convert to appropriate units from degrees.  phi and theta
// may be returned in a different interface somewhen.

//          phi = d_phi / to_degrees_p[0];
//          theta = d_theta / to_degrees_p[1];
           toOther(world);
	} else {
           errorMsg = String("wcslib celrev error: ") + celrev_errmsg[errnum];
           set_error(errorMsg);
           return False;
	}
    } 

// Convert to specified conversion reference type

    convertTo(world);
//
    return True;
}


Bool DirectionCoordinate::toPixel(Vector<Double> &pixel,
				  const Vector<Double> &world) const
{
// Temporaries 

    static Vector<Double> world_tmp;
    double d_theta, d_phi, d_lng, d_lat, d_x, d_y;
    String errorMsg;
//
    AlwaysAssert(world.nelements() == nWorldAxes(), AipsError);
    pixel.resize(2);
    world_tmp.resize(nWorldAxes());
//
    world_tmp(0) = world(0); 
    world_tmp(1) = world(1);

// Convert from specified conversion reference type

    convertFrom(world_tmp);
//
    toDegrees(world_tmp);
    d_lng = world_tmp(0);
    d_lat = world_tmp(1);
    int errnum = celfwd(pcodes[projection_p.type()], d_lng, d_lat,
			celprm_p, &d_phi, &d_theta, prjprm_p, 
                        &d_x, &d_y);
    if (errnum!=0) {
       errorMsg = String("wcslib celfwd error: ") + celfwd_errmsg[errnum];
       set_error(errorMsg);
       return False;
    } else {
        world_tmp(0) = d_x; 
        world_tmp(1) = d_y;
//
	if (!linear_p.forward(pixel, world_tmp, errorMsg)) {
           set_error(errorMsg);
           return False;
        }

// phi and theta may be returned in a different interface somewhen

//        phi = d_phi;
//        theta = d_theta;
    }
//
    return True;
}

Bool DirectionCoordinate::toMix(Vector<Double>& worldOut,
                                Vector<Double>& pixelOut,
                                const Vector<Double>& worldIn,
                                const Vector<Double>& pixelIn,
                                const Vector<Bool>& worldAxes,
                                const Vector<Bool>& pixelAxes,
                                const Vector<Double>& worldMin,
                                const Vector<Double>& worldMax) const
{
   Bool useConversionType = False;

// Temporaries

   static Vector<Double> in_tmp;
   static Vector<Double> out_tmp;
//
   const uInt nWorld = worldAxes.nelements();
   const uInt nPixel = pixelAxes.nelements();
   AlwaysAssert(nWorld==nWorldAxes(), AipsError);
   AlwaysAssert(nPixel==nPixelAxes(), AipsError);
   AlwaysAssert(worldIn.nelements()==nWorld, AipsError);
   AlwaysAssert(pixelIn.nelements()==nPixel, AipsError);
   AlwaysAssert(worldMin.nelements()==nWorld, AipsError);
   AlwaysAssert(worldMax.nelements()==nWorld, AipsError);
//
   for (uInt i=0; i<nPixel; i++) {   
      if (pixelAxes(i) && worldAxes(i)) {
         set_error("DirectionCoordinate::toMix - duplicate pixel/world axes");
         return False; 
      }
      if (!pixelAxes(i) && !worldAxes(i)) {
         set_error("DirectionCoordinate::toMix - each coordinate must be either pixel or world");
         return False;
      }
   }
//
   worldOut.resize(nWorldAxes());
   pixelOut.resize(nPixelAxes());
//
   if (pixelAxes(0) && pixelAxes(1)) {
//
// pixel->world
//
      if (!toWorld(worldOut, pixelIn)) return False;
      pixelOut(0) = pixelIn(0);
      pixelOut(1) = pixelIn(1);
   } else if (worldAxes(0) && worldAxes(1)) {
//
// world->pixel
//
      if (!toPixel(pixelOut, worldIn)) return False;
      worldOut(0) = worldIn(0);
      worldOut(1) = worldIn(1);
   } else if (pixelAxes(0) && worldAxes(1)) {
//
// pixel,world->world,pixel
//
      if (!canDoToMix_p) {
         set_error("DirectionCoordinate::toMix - " + 
                   canDoToMixErrorMsg_p);
          return False;
      }
//
      in_tmp.resize(2);
      out_tmp.resize(2);
//
      in_tmp(0) = pixelIn(0);
      in_tmp(1) = worldIn(1);
      if (!toMix2(out_tmp, in_tmp, worldMin, 
                  worldMax, False)) {
         return False;
      }
//
      pixelOut(0) = in_tmp(0);
      pixelOut(1) = out_tmp(1);
      worldOut(0) = out_tmp(0);
      worldOut(1) = in_tmp(1);  
//
      if (useConversionType) {
         convertTo(worldOut);
      }
   } else if (worldAxes(0) && pixelAxes(1)) {
//
// world,pixel->pixel,world
//
      if (!canDoToMix_p) {
         set_error("DirectionCoordinate::toMix - " + 
                   canDoToMixErrorMsg_p);
          return False;
      }
//
      in_tmp.resize(2);
      out_tmp.resize(2);
//
      in_tmp(0) = worldIn(0);
      in_tmp(1) = pixelIn(1);
      if (!toMix2(out_tmp, in_tmp, worldMin, 
                  worldMax, True)) {
         return False;
      }
//
      pixelOut(0) = out_tmp(0);
      pixelOut(1) = in_tmp(1);
      worldOut(0) = in_tmp(0);
      worldOut(1) = out_tmp(1);
//
      if (useConversionType) {
         convertTo(worldOut);
      }

   }
   return True;   
}
 

MDirection::Types DirectionCoordinate::directionType() const
{
    return type_p;
}

Projection DirectionCoordinate::projection() const
{
    return projection_p;
}

Vector<String> DirectionCoordinate::worldAxisNames() const
{
    return names_p;
}

Vector<String> DirectionCoordinate::worldAxisUnits() const
{
    return units_p;
}

Bool DirectionCoordinate::setPreferredWorldAxisUnits (const Vector<String>& units)
{
    if (!Coordinate::setPreferredWorldAxisUnits(units)) return False;
//
    prefUnits_p = units;
    return True;
}

Vector<String> DirectionCoordinate::preferredWorldAxisUnits() const
{
   return prefUnits_p;
}


Vector<Double> DirectionCoordinate::referenceValue() const
{
    Vector<Double> crval(2);
    crval(0) = celprm_p->ref[0];
    crval(1) = celprm_p->ref[1];
    toOther(crval);
    return crval;
}

Vector<Double> DirectionCoordinate::increment() const
{
    Vector<Double> cdelt(linear_p.cdelt().copy());
    toOther(cdelt);
    return cdelt;
}

Matrix<Double> DirectionCoordinate::linearTransform() const
{
    return linear_p.pc();
}

Vector<Double> DirectionCoordinate::referencePixel() const
{
    return linear_p.crpix();
}

Bool DirectionCoordinate::setWorldAxisNames(const Vector<String> &names)
{
    Bool ok = (names.nelements()==nWorldAxes());
    if (!ok) {
       set_error("names vector must be of length 2");
       return False;
    }
//
    names_p = names;
    return True;
}

Bool DirectionCoordinate::setWorldAxisUnits(const Vector<String> &units)
{
    Bool ok = (units.nelements()==nWorldAxes());
    if (!ok) {
       set_error("units vector must be of length 2");
       return False;
    }
//
    String error;
    Vector<Double> factor;
    ok = find_scale_factor(error, factor, units, worldAxisUnits());
    if (ok) {
      to_degrees_p[0] /= factor(0);
      to_degrees_p[1] /= factor(1);
//
      to_radians_p[0] = to_degrees_p[0] * C::degree;
      to_radians_p[1] = to_degrees_p[1] * C::degree;
    } else {
      set_error(error);
    }
    if (ok) {
       units_p = units;
       worldMin_p *= factor;
       worldMax_p *= factor;
    }
    return ok;
}


Bool DirectionCoordinate::setReferencePixel(const Vector<Double> &refPix)
{
    Bool ok = (refPix.nelements()==nPixelAxes());
    if (!ok) {
       set_error("reference pixels vector must be of length 2");
       return False;
    }
//
    linear_p.crpix(refPix);
    return True;
}

Bool DirectionCoordinate::setLinearTransform(const Matrix<Double> &xform)
{
    Bool ok = (xform.nrow() == nWorldAxes() && 
                     xform.ncolumn() == nWorldAxes());
    if (!ok) {
       set_error("linear transform matrix has wrong shape");
       return False;
    }
//
    linear_p.pc(xform);
    return True;
}

Bool DirectionCoordinate::setIncrement(const Vector<Double> &inc)
{
    Bool ok = (inc.nelements()==nWorldAxes());
    if (!ok) {
       set_error("Two increments must be provided!");
       return False;
    }
//
    Vector<Double> tmp(inc.copy());
    toDegrees(tmp);
    linear_p.cdelt(tmp);

    return ok;
}

Bool DirectionCoordinate::setReferenceValue(const Vector<Double> &refval)
{
    Bool ok = (refval.nelements()==nWorldAxes());
    if (!ok) {
       set_error("Two ref. values must be provided!");
       return False;
    }
//
    Vector<Double> tmp(refval.copy());
    toDegrees(tmp);
    celprm_p->flag  = 0;
    celprm_p->ref[0] = tmp(0);
    celprm_p->ref[1] = tmp(1);
    String name = Projection::name(projection().type());
    const char *nameptr = name.chars();
    int errnum = celset(nameptr, celprm_p, prjprm_p);
    ok = (errnum==0);
    if (!ok) {
       String errmsg = "wcs celset_error: ";
       errmsg += celset_errmsg[errnum];
       set_error(errmsg);
    }

// Update offset coordinate rotation matrix

    setRotationMatrix();

// Fill in the silly wcs private members I have to cart about
// for toMix caching

    c_crval_p[0] = tmp(0);
    c_crval_p[1] = tmp(1);
//
    return ok;
}

Vector<String> DirectionCoordinate::axisNames(MDirection::Types type, 
					      Bool FITSName)
{
    Vector<String> names(2);
    if (FITSName) {
	switch(type) {
	case MDirection::J2000:
	case MDirection::JMEAN:
	case MDirection::APP:
	case MDirection::B1950:
	case MDirection::BMEAN:
	case MDirection::BTRUE:
	    names(0) = "RA";
	    names(1) = "DEC";
	    break;
	case MDirection::GALACTIC:
	    names(0) = "GLON";
	    names(1) = "GLAT";
	    break;
	case MDirection::HADEC:
	    names(0) = "HA";
	    names(1) = "DEC";
	    break;
	case MDirection::AZEL:
	    names(0) = "AZ";
	    names(1) = "EL";
	    break;
	default:
	    names = "????";
	}
    } else {
	switch(type) {
	case MDirection::J2000: // These are all RA/DEC systems
	case MDirection::JMEAN:
	case MDirection::APP:
	case MDirection::B1950:
	case MDirection::BMEAN:
	case MDirection::BTRUE:
	    names(0) = "Right Ascension";
	    names(1) = "Declination";
	    break;
	case MDirection::GALACTIC:
	    names(0) = "Longitude";
	    names(1) = "Latitude";
	    break;
	case MDirection::HADEC:
	    names(0) = "Hour Angle";
	    names(1) = "Declination";
	    break;
	case MDirection::AZEL:
	    names(0) = "Azimuth";
	    names(1) = "Elevation";
	    break;
	default:
	    names = "Unknown";
	}
    }
    return names;
}


void DirectionCoordinate::checkFormat(Coordinate::formatType& format,
                                      Bool absolute) const
{   
   MDirection::GlobalTypes gtype = MDirection::globalType(type_p);

   if (format == Coordinate::DEFAULT) {
      if (gtype == MDirection::GRADEC || gtype == MDirection::GHADEC) {
         if (absolute) {
            format = Coordinate::TIME;
         } else {
            format = Coordinate::SCIENTIFIC;
         }
      } else if (gtype == MDirection::GLONGLAT) {
         format = Coordinate::FIXED;
      } else if (gtype == MDirection::GAZEL) {
         format = Coordinate::FIXED;
      } else {
         format = Coordinate::SCIENTIFIC;
      }
   }
}



void DirectionCoordinate::getPrecision (Int& precision,
                                        Coordinate::formatType& format,
                                        Bool absolute,
                                        Int defPrecScientific,
                                        Int defPrecFixed,
                                        Int defPrecTime) const
{
// Fill in DEFAULT

   checkFormat(format, absolute);

// Set precisions depending upon requested format type and
// desire to see absolute or offset coordinate

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
   } else if (format == Coordinate::TIME) {
      if (defPrecTime >= 0) {
         precision = defPrecTime;
      } else {
         precision = 3;
      }
   }
}


String DirectionCoordinate::format(String& units,
                                   Coordinate::formatType format,
                                   Double worldValue,
                                   uInt worldAxis,
                                   Bool isAbsolute,
                                   Bool showAsAbsolute,
                                   Int precision)
{
   AlwaysAssert(worldAxis< nWorldAxes(), AipsError);
   AlwaysAssert(nWorldAxes()==2, AipsError);

// Convert given world value to absolute or relative as needed
   
   static Vector<Double> world;
   world.resize(nWorldAxes());
//
   if (showAsAbsolute) {
      if (!isAbsolute) {
         world = 0.0;
         world(worldAxis) = worldValue;
         makeWorldAbsolute(world);
         worldValue = world(worldAxis);
      }
   } else {
      if (isAbsolute) {
         world = referenceValue();
         world(worldAxis) = worldValue;
         makeWorldRelative(world);
         worldValue = world(worldAxis);
      }
   } 
     
// Fill in DEFAULT format

   Coordinate::formatType form = format;

// Check format.  If showAsAbsolute==F, TIME formatting is switched off

   checkFormat(form, showAsAbsolute);

// Set default precision if needed

   Int prec = precision;
   if (prec < 0) getPrecision(prec, form, showAsAbsolute, -1, -1, -1);

// Set global DirectionCoordinate type

   MDirection::GlobalTypes gtype = MDirection::globalType(type_p);

// Check units

   static const Unit unitRAD("rad");
   String nativeUnit = worldAxisUnits()(worldAxis);
   String prefUnit;
   if (units.empty()) {
      prefUnit = preferredWorldAxisUnits()(worldAxis);
      if (prefUnit.empty()) {
//            leave empty telling formatLong/Lat to make up something nice
      } else {
         units = prefUnit;       // prefUnit always consistent
      }
   } else {
      Unit unitCurrent(units);
      if (unitCurrent != unitRAD) {
         throw (AipsError("Specified unit is invalid"));
      }
   }
   
// Convert value to radians first so we can make an MVAngle from it

   Double worldValue2;
   worldValue2 = C::degree * worldValue * to_degrees_p[worldAxis];
   MVAngle mVA(worldValue2);

// We need to put the longitudes into the correct convention
// All latitudes better be -90 -> 90 and we don't need to fiddle them
// If the units are empty, the formatters will make something up

   String string;
   if (worldAxis==0) {
      string = formatLongitude (units, mVA, gtype, showAsAbsolute, form, prec);
   } else {
      string = formatLatitude (units, mVA, showAsAbsolute, form, prec);
   }

// Only return actual units if not TIME formatting

   if (form==Coordinate::TIME) units = String("");
//
   return string;
}


String DirectionCoordinate::formatLongitude (String& units, MVAngle& mVA,
                                             MDirection::GlobalTypes gtype,
                                             Bool absolute, 
                                             Coordinate::formatType form,
                                             Int prec) const
{
   ostrstream oss;         
   MVAngle mVA2(mVA);

// Time formatting we can do straight away

   if (form==Coordinate::TIME) {

// Format with mVA2. TIME formatting really only makes
// sense for RA and HA, but we do something with the others
// TIME formatting is only possible for absolute=T

      prec += 6;
      if (gtype == MDirection::GRADEC) {
         oss << mVA2.string(MVAngle::TIME,prec);
      } else if (gtype==MDirection::GHADEC) {
         oss << mVA2.string(MVAngle::TIME+MVAngle::DIG2,prec);
      } else if (gtype==MDirection::GAZEL) {
         oss << mVA2.string(MVAngle::ANGLE,prec);
      } else if (gtype==MDirection::GLONGLAT) {
         oss << mVA2.string(MVAngle::ANGLE,prec);
      }
//
      oss << ends;
      return String(oss);
   }

// Continue on with other format types

   Double value = mVA2.get().getValue();     // Radians
   Bool emptyUnits = units.empty();
//
   if (gtype==MDirection::GRADEC){ 
      if (absolute) mVA2 = mVA(0.0);                   // 0->2pi (0->360)
      if (emptyUnits) {
         value = mVA2.get(Unit("deg")).getValue();
         units = "deg";
      } else {
         value = mVA2.get(Unit(units)).getValue();
      }
   } else if (gtype==MDirection::GHADEC) {
      if (absolute) mVA2 = mVA();                      // -pi->pi (-180->180)
      if (emptyUnits) {
         value = mVA2.get().getValue() * 24.0 / C::_2pi;
         units = "h";
      } else {
         value = mVA2.get(Unit(units)).getValue();
      }
   } else if (gtype==MDirection::GAZEL) {
      if (absolute) mVA2 = mVA(0.0);                   // 0->2pi (0->360)
      if (emptyUnits) {
         value = mVA2.get(Unit("deg")).getValue();
         units = "deg";
      } else {
         value = mVA2.get(Unit(units)).getValue();
      }
   } else if (gtype==MDirection::GLONGLAT) {
      if (type_p==MDirection::ECLIPTIC ||
          type_p==MDirection::MECLIPTIC ||
          type_p==MDirection::TECLIPTIC) {
         if (absolute) mVA2 = mVA();                  // -pi->pi (-180->180)
      } else {
         if (absolute) mVA2 = mVA(0.0);               // 0->2pi (0->360)
      }
      if (emptyUnits) {
         value = mVA2.get(Unit("deg")).getValue();
         units = "deg";
      } else {
         value = mVA2.get(Unit(units)).getValue();
      }
   } else {
      if (absolute) mVA2 = mVA(0.0);                  // 0->2pi (0->360)
      value = mVA2.get(Unit(units)).getValue();
   }

// For relative coordinates, use arcsec if user didnt specify

   if (!absolute) {
      if (emptyUnits&& units=="deg") {
         value *= 3600.0;
         units = "arcsec";
      }
   }
//
   if (form==Coordinate::SCIENTIFIC) {
      oss.setf(ios::scientific, ios::floatfield);
      oss.precision(prec);
      oss << value;
   } else if (form==Coordinate::FIXED) {
      oss.setf(ios::fixed, ios::floatfield);
      oss.precision(prec);
      oss << value;
   }
//
   oss << ends;
   return String(oss);
}

String DirectionCoordinate::formatLatitude (String& units, MVAngle& mVA,
                                            Bool absolute, 
                                            Coordinate::formatType form,
                                            Int prec) const
{
   ostrstream oss;         
   MVAngle mVA2(mVA);

// TIME formatting we can do straight away

   if (form==Coordinate::TIME) {
      prec += 6;
      oss << mVA2.string(MVAngle::ANGLE+MVAngle::DIG2,prec);
   }

// Continue on with other format types

   Double value = mVA2.get().getValue();     // Radians
   Bool emptyUnits = units.empty();
//
   if (!emptyUnits) {
      value = mVA2.get(Unit(units)).getValue();
   } else {
      value = mVA2.get(Unit("deg")).getValue();
      units = "deg";
   }
//
   if (!absolute) {
      if (emptyUnits && units=="deg") {
         value *= 3600.0;
         units = "arcsec";
      }
   }
//
   if (form==Coordinate::SCIENTIFIC) {
      oss.setf(ios::scientific, ios::floatfield);
      oss.precision(prec);
      oss << value;
   } else if (form==Coordinate::FIXED) {
      oss.setf(ios::fixed, ios::floatfield);
      oss.precision(prec);
      oss << value;
   }
   oss << ends;
   return String(oss);
}




Coordinate *DirectionCoordinate::clone() const
{
    return new DirectionCoordinate(*this);
}

void DirectionCoordinate::toDegrees(Vector<Double> &other) const
{
    other(0) *= to_degrees_p[0];
    other(1) *= to_degrees_p[1];
}


Bool DirectionCoordinate::near(const Coordinate& other, 
                               Double tol) const
{
   Vector<Int> excludeAxes;
   return near(other, excludeAxes, tol);
}



Bool DirectionCoordinate::near(const Coordinate& other, 
                               const Vector<Int>& excludeAxes,
                               Double tol) const

{
   if (this->type() != other.type()) {
      set_error("Comparison is not with another DirectionCoordinate");
      return False;
   }
     
   const DirectionCoordinate& dCoord = dynamic_cast<const DirectionCoordinate&>(other);
   
   if (!projection_p.near(dCoord.projection_p, tol)) {
      set_error("The DirectionCoordinates have differing projections");
      return False;
   }
   if (type_p != dCoord.type_p) {
      set_error("The DirectionCoordinates have differing types");
      return False;
   }


// Number of pixel and world axes is the same for a DirectionCoordinate
// Add an assertion check should this change 
 
   AlwaysAssert(nPixelAxes()==nWorldAxes(), AipsError);
   Vector<Bool> exclude(nPixelAxes());   
   exclude = False;
   Bool found;
   const uInt nExcl = excludeAxes.nelements();
   if (nExcl > 0) {
      for (uInt i=0; i<nPixelAxes(); i++) {
         if (linearSearch(found, excludeAxes, Int(i), nExcl) >= 0) exclude(i) = True;
      }
   }

// Check linear transformation

   if (!linear_p.near(dCoord.linear_p, excludeAxes, tol)) {
      set_error("The DirectionCoordinates have differing linear transformation matrices");
      return False;
   }


// Check names and units

   ostrstream oss;
   if (names_p.nelements() != dCoord.names_p.nelements()) {
      set_error("The DirectionCoordinates have differing numbers of world axis names");
      return False;
   }
   if (units_p.nelements() != dCoord.units_p.nelements()) {
      set_error("The DirectionCoordinates have differing numbers of axis units");
      return False;
   }
   for (uInt i=0; i<names_p.nelements(); i++) {
      if (!exclude(i)) {
         if (names_p(i) != dCoord.names_p(i)) {
            oss << "The DirectionCoordinates have differing axis names for axis "
                << i << ends;
            set_error(String(oss));
            return False;      
         }
      }
   }
   for (uInt i=0; i<units_p.nelements(); i++) {
      if (!exclude(i)) {
         if (units_p(i) != dCoord.units_p(i)) {
            oss << "The DirectionCoordinates have differing axis units for axis "
                << i << ends;
            set_error(String(oss));
            return False;      
         }
      }
   }

     
// WCS structures.  

// The flag does not really indicate permanent state
//
//   if (celprm_p->flag != dCoord.celprm_p->flag) {
//      set_error("The DirectionCoordinates have differing WCS spherical coordinate structures");
//      return False;
//   }

// Items 0 and 2 are longitudes, items 1 and 3 are latitiudes
// So treat them as axes 0 and 1 ???

   for (uInt i=0; i<2; i++) {

     if (!exclude(i)) {
        if (!::near(celprm_p->ref[i],dCoord.celprm_p->ref[i],tol)) {
           set_error("The DirectionCoordinates have differing WCS spherical coordinate longitudes");
           return False;
        }
        if (!::near(celprm_p->ref[i+2],dCoord.celprm_p->ref[i+2],tol)) {
           set_error("The DirectionCoordinates have differing WCS spherical coordinate latitudes");
           return False;
        }
     }
   }

// No idea what to do here for exclusion axes !
// Don't test prjprm->flag as it is a transient flag 
// whose value may change depending on what computations you
// do when
// 

   if (prjprm_p->r0!=0.0 && dCoord.prjprm_p->r0!=0.0) {

// 0 is the start value, and the WCS routines will change
// it, so if either value is 0, it's ok

      if (prjprm_p->r0 != dCoord.prjprm_p->r0) {
         set_error("The DirectionCoordinates have differing WCS projection parameters");
         return False;
       }
   }
   for (uInt i=0; i<10; i++) {
      if (!::near(prjprm_p->p[i],dCoord.prjprm_p->p[i],tol)) {
         set_error("The DirectionCoordinates have differing WCS projection structures");
         return False;
      }
   }
   
                          
// Probably these arrays are implicitly the same if the units are the same
// but might as well make sure

   if (!exclude(0)) {
      if (!::near(to_degrees_p[0],dCoord.to_degrees_p[0],tol)) {
         set_error("The DirectionCoordinates have differing unit conversion vectors");
         return False;
      }
   }
   if (!exclude(1)) {
      if (!::near(to_degrees_p[1],dCoord.to_degrees_p[1],tol)) {
         set_error("The DirectionCoordinates have differing unit conversion vectors");
         return False;
      }
   }

// These things are needed only for toMix caching speedup

   for (uInt i=0; i<2; i++) {
      if (strcmp(c_ctype_p[i],dCoord.c_ctype_p[i])!=0) {
         set_error("The DirectionCoordinates have differing FITS ctypes");
         return False;
      }
      if (!::near(c_crval_p[i],dCoord.c_crval_p[i],tol)) {
         set_error("The DirectionCoordinates have differing FITS crvals");
         return False;
      }
   }
// 
// The flag variable may change with use, so don't test it.
//   Bool ok = wcs_p->flag==dCoord.wcs_p->flag &&
//
   Bool ok = strcmp(wcs_p->pcode,dCoord.wcs_p->pcode)==0 &&
        strcmp(wcs_p->lngtyp,dCoord.wcs_p->lngtyp)==0 &&
        strcmp(wcs_p->lattyp,dCoord.wcs_p->lattyp)==0 &&
        wcs_p->lng==dCoord.wcs_p->lng &&
        wcs_p->lat==dCoord.wcs_p->lat &&        
        wcs_p->cubeface==dCoord.wcs_p->cubeface;
   if (!ok) {
      set_error("The DirectionCoordinates have differing wcs structures");
      return False;
   }
   return True;
}




Bool DirectionCoordinate::save(RecordInterface &container,
                               const String &fieldName) const
{
    Bool ok = (!container.isDefined(fieldName));
    if (ok) {
	Record subrec;
	Projection proj = projection();
	String system = MDirection::showType(type_p);
//
	subrec.define("system", system);
	subrec.define("projection", proj.name());
	subrec.define("projection_parameters", proj.parameters());
	subrec.define("crval", referenceValue());
	subrec.define("crpix", referencePixel());
	subrec.define("cdelt", increment());
	subrec.define("pc", linearTransform());
	subrec.define("axes", worldAxisNames());
	subrec.define("units", worldAxisUnits());
//
	String convSystem = MDirection::showType(conversionType_p);
	subrec.define("conversionSystem", convSystem); 
//
        subrec.define("longpole", celprm_p->ref[2]);    // Always degrees
        subrec.define("latpole", celprm_p->ref[3]);     // Always degrees
//
	container.defineRecord(fieldName, subrec);
    }
    return ok;
}

DirectionCoordinate* DirectionCoordinate::restore(const RecordInterface &container,
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
    MDirection::Types sys;
    Bool ok = MDirection::getType(sys, system);
    if (!ok) {
	return 0;
    }
    
    if (!subrec.isDefined("projection") || 
	!subrec.isDefined("projection_parameters")) {
	return 0;
    }
    String projname;
    subrec.get("projection", projname);
    Vector<Double> projparms;
    subrec.get("projection_parameters", projparms);
    Projection proj(Projection::type(projname), projparms);
//
    if (!subrec.isDefined("crval")) {
	return 0;
    }
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
    Double longPole, latPole;
    longPole = latPole = 999.0;            // Optional
    if (subrec.isDefined("longpole")) {    
       subrec.get("longpole", longPole);   // Always degrees
       longPole *= C::degree;
    }
    if (subrec.isDefined("latpole")) {
       subrec.get("latpole", latPole);     // Always degrees
       latPole *= C::degree;
    }
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

// Dummy DC which we overwrite.  Use refVal [0.0, 0.5] rather than [0.0, 0.0]
// to avoid divide by zero in Coordinate::make_Direction_FITS_ctype() function.
// See that function for further comments
// 

    Quantum<Double> refLong(crval(0), units(0));
    Quantum<Double> refLat(crval(1), units(1));
    Quantum<Double> incLong(cdelt(0), units(0));
    Quantum<Double> incLat(cdelt(1), units(1));
    DirectionCoordinate *retval = 
	new DirectionCoordinate(sys, proj, 
                                refLong.getValue(Unit("rad")), 
                                refLat.getValue(Unit("rad")), 
                                incLong.getValue(Unit("rad")), 
                                incLat.getValue(Unit("rad")), 
                                pc, crpix(0), crpix(1), 
                                longPole, latPole);
    AlwaysAssert(retval, AipsError);

    // Set the actual units and names
    retval->setWorldAxisUnits(units);
    retval->setWorldAxisNames(axes);

    // Set the conversion type if it exists
    if (subrec.isDefined("conversionSystem")) {
       String conversionSystem;
       subrec.get("conversionSystem", conversionSystem);
       MDirection::Types cSystem;
       Bool ok = MDirection::getType(cSystem, conversionSystem);
       if (ok) {
          retval->setReferenceConversion(cSystem);
       }
    }
//
    return retval;
}


void DirectionCoordinate::toOther(Vector<Double> &degrees) const
{
    degrees(0) /= to_degrees_p[0];
    degrees(1) /= to_degrees_p[1];
}

Bool DirectionCoordinate::toMix2(Vector<Double>& out,
                                 const Vector<Double>& in,
                                 const Vector<Double>& minWorld,
                                 const Vector<Double>& maxWorld,
                                 Bool longIsWorld) const
//
// This function is implemented at the wcs interface level, whereas
// the toWorld and toPixel functions are implemented at the cel
// (lower) level.  This is why we have to make some extra things
// for this function (wcs_p, c_ctype_p, c_crval_p) which aren't used
// elsewhere.
//
// vectors must be of length 2. no checking
//
{
//
// Temporaries
//
    int mixpix;
    int mixcel;
    double mix_vspan[2];
    double mix_world[2];
    double mix_pixcrd[2];
    double mix_imgcrd[2];
    double mix_vstep;
    int mix_viter;
    double mix_phi, mix_theta;
    String errorMsg;
//
// Set input world/pixel arrays
//
    if (longIsWorld) {
       mixcel = 1;          // 1 or 2 (a code, not an index)
       mixpix = 1;          // index into pixcrd array
//
       mix_world[wcs_p->lng] = in(0) * to_degrees_p[0];
       mix_pixcrd[mixpix] = in(1);
//
// Latitude span
//
       mix_vspan[0] = minWorld(1) * to_degrees_p[1];
       mix_vspan[1] = maxWorld(1) * to_degrees_p[1];
    } else {
       mixcel = 2;          // 1 or 2 (a code, not an index)
       mixpix = 0;          // index into pixcrd array
//
       mix_world[wcs_p->lat] = in(1) * to_degrees_p[1];
       mix_pixcrd[mixpix] = in(0);
//
// Longitude span
//
       mix_vspan[0] = minWorld(0) * to_degrees_p[0];
       mix_vspan[1] = maxWorld(0) * to_degrees_p[0];
    }
//
// Do it
//
//    mix_vstep = 1.0;
    mix_vstep = 0.0;
//
    mix_viter = 5;
    prjprm_p->flag = -1;                    // Return a solution, even if ambiguous

/*
cerr << "Enter DC::toMix2" << endl;
cerr << "ctype = " << c_ctype_p[0] << " " << c_ctype_p[1];
cerr << "   crval = " << c_crval_p[0] << " " << c_crval_p[1] << endl;
cerr << "mixpix, mixcel = " << mixpix << mixcel << endl;
cerr << "vspan = " << mix_vspan[0] << ", " << mix_vspan[1];
cerr << "  vstep, viter = " << mix_vstep << ", " << mix_viter << endl;
cerr << "input world = " << mix_world[0] << ", " << mix_world[1] << endl;
cerr << "input pixel = " << mix_pixcrd[0] << ", " << mix_pixcrd[1] << endl;
*/
    int iret = wcsmix(c_ctype_p, wcs_p, mixpix, mixcel, mix_vspan, 
                      mix_vstep, mix_viter, 
                      mix_world, c_crval_p, celprm_p, &mix_phi, &mix_theta, 
                      prjprm_p, mix_imgcrd, linear_p.linprmWCS(), mix_pixcrd);

/*
cerr << "iret = " << iret << endl;
cerr << "output world = " << mix_world[0] << ", " << mix_world[1] << endl;
cerr << "output pixel = " << mix_pixcrd[0] << ", " << mix_pixcrd[1] << endl;
cerr << "phi, theta=" << mix_phi << ", " << mix_theta << endl;
cerr << "Exit DC::toMix2" << endl;
*/

    if (iret!=0) {
        errorMsg= "wcs wcsmix_error: ";
        errorMsg += wcsmix_errmsg[iret];
        set_error(errorMsg);
        return False;
    }
//
// Fish out the results
//
    if (longIsWorld) {
       out(0) = mix_pixcrd[0];
       out(1) = mix_world[wcs_p->lat] / to_degrees_p[1];
    } else {
       out(0) = mix_world[wcs_p->lng] / to_degrees_p[0];
       out(1) = mix_pixcrd[1];
    } 

// Phi and theta may be returned in a different interface somewhen
// Could assign these in toMix rather than pass them out
// but not very clear

//    phi = mix_phi / to_degrees_p[0];
//    theta = mix_theta / to_degrees_p[1];
//
    return True;
}


// Helper functions to help us interface to WCS

void DirectionCoordinate::make_celprm_and_prjprm(Bool& canDoToMix, String& canDoToMixErrorMsg,
                                                 celprm*& pCelPrm, prjprm*& pPrjPrm, wcsprm*& pWcs,  
                                                 char c_ctype[2][9], double c_crval[2],
                                                 const Projection& proj, MDirection::Types directionType,
                                                 Double refLong, Double refLat,  
                                                 Double longPole, Double latPole) const
{
    pCelPrm = new celprm;
    if (! pCelPrm) {
        throw(AllocError("DirectionCoordinate::make_celprm_and_prjprm()", sizeof(celprm)));
    }
    pCelPrm->flag = 0;
    pCelPrm->ref[0] = refLong;
    pCelPrm->ref[1] = refLat;
    pCelPrm->ref[2] = longPole;    // 999.0 means work these out internally
    pCelPrm->ref[3] = latPole;

    pPrjPrm = new prjprm;
    if (! pPrjPrm) {
        delete pCelPrm;
        pCelPrm = 0;
        throw(AllocError("static ::make_celprm_and_prjprm()", sizeof(prjprm)));
    }
    pPrjPrm->flag = 0;
    pPrjPrm->r0 = 0.0;
    objset(pPrjPrm->p, double(0.0), uInt(10));
    uInt nRequiredParameters = Projection::nParameters(proj.type());
    Vector<Double> projParameters = proj.parameters();
    AlwaysAssert(projParameters.nelements() >= nRequiredParameters, AipsError);
    int startAt = ((proj.type() == Projection::ZPN) ? 0 : 1);
    for (uInt i=0; i < nRequiredParameters; i++) {
        pPrjPrm->p[i + startAt] = projParameters(i);
    }
    String name = Projection::name(proj.type());
    const char *nameptr = name.chars();
    int errnum = celset(nameptr, pCelPrm, pPrjPrm);
    if (errnum != 0) {
        String errmsg = "wcs celset_error: ";
        errmsg += celset_errmsg[errnum];
        throw(AipsError(errmsg));
    }
// 
// All the things we make next are needed by the toMix2 function
// We don't want to make them every time this is called
//
    pWcs = new wcsprm;
    if (!pWcs) {
        throw(AllocError("static ::make_celprm_and_prjprm()", sizeof(wcsprm)));
    }
//
// Construct FITS ctype vector
//
    Bool isNCP = False;
    Vector<String> axisNames = DirectionCoordinate::axisNames(directionType, True);
    Vector<String> ctype = make_Direction_FITS_ctype(isNCP, proj, axisNames, 
                                                     refLat*C::degree, False);
    strncpy (c_ctype[0], ctype(0).chars(), 9);
    strncpy (c_ctype[1], ctype(1).chars(), 9);
//
    c_crval[0] = refLong;
    c_crval[1] = refLat;
//
// Fill in the wcs structure
//
    int iret = wcsset(2, c_ctype, pWcs);
    if (iret!=0) {
        canDoToMix = False;
        canDoToMixErrorMsg = "wcs error : ";
        canDoToMixErrorMsg += wcsset_errmsg[iret];
    }
}



void DirectionCoordinate::copy_celprm_and_prjprm(celprm* &pToCel, prjprm* &pToPrj,
                                                 wcsprm* &pToWcs, 
                                                 char toctype[2][9], double tocrval[2],
                                                 const celprm *pFromCel, const prjprm *pFromPrj,
                                                 const wcsprm *pFromWcs, 
                                                 const char fromctype[2][9], const double fromcrval[2]) const
//
// This function is used by the copy constructor (for which the
// output pointers will be 0) and assignment, for which the
// output pointers will be non-zero
//
// Note that none of these structures have pointers in them. Therefore
// we can copy them easily.  SOme of the wcs strcutures have pointers
// in them and deep copies must be made.
//
{
    AlwaysAssert(pFromCel!=0 && pFromPrj!=0 && pFromWcs!=0, AipsError);
//
    if (pToCel==0) pToCel = new celprm;
    if (!pToCel) {
        throw(AllocError("static ::copy_celprm_and_prjprm()", sizeof(celprm)));
    }
    *pToCel = *pFromCel;
//
    if (pToPrj==0) pToPrj = new prjprm;
    if (!pToPrj) {
        delete pToCel;
        pToCel = 0;
        throw(AllocError("static ::copy_celprm_and_prjprm()", sizeof(prjprm)));
    }
    *pToPrj = *pFromPrj;
//
    if (pToWcs==0) pToWcs = new wcsprm;
    if (!pToWcs) {
        delete pToCel;
        pToCel = 0;
        delete pToPrj;
        pToPrj = 0;
        throw(AllocError("static ::copy_celprm_and_prjprm()", sizeof(wcsprm)));
    }
    *pToWcs = *pFromWcs;
//
    strncpy (toctype[0], fromctype[0], 9);
    strncpy (toctype[1], fromctype[1], 9);
    tocrval[0] = fromcrval[0];
    tocrval[1] = fromcrval[1];
}


Coordinate* DirectionCoordinate::makeFourierCoordinate (const Vector<Bool>& axes,
                                                        const Vector<Int>& shape) const
//
// axes says which axes in the coordinate are to be transformed
// shape is the shape of the image for all axes in this coordinate
//
{
   AlwaysAssert(nPixelAxes()==2, AipsError);
   AlwaysAssert(nWorldAxes()==2, AipsError);
//
   if (axes.nelements() != 2) {
      throw (AipsError("Invalid number of specified axes"));
   }
   if (!axes(0) || !axes(1)) {
      throw (AipsError("You must specify both axes of the DirectionCoordinate to transform"));
   }
//
   if (shape.nelements() != 2) {
      throw (AipsError("Invalid number of elements in shape"));
   }

// Find names and units for Fourier coordinate and units to set 
// for this DirectionCoordinate 

   Vector<String> names(worldAxisNames());
   Vector<String> units(worldAxisUnits());
   Vector<String> unitsCanon(worldAxisUnits());
//
   Vector<String> namesOut(worldAxisNames().copy());
   Vector<String> unitsOut(worldAxisUnits().copy());
   fourierUnits (namesOut(0), unitsOut(0), unitsCanon(0), Coordinate::DIRECTION, 0,
                 units(0), names(0));
   fourierUnits (namesOut(1), unitsOut(1), unitsCanon(1), Coordinate::DIRECTION, 1,
                 units(1), names(1));

// Make a copy of ourselves and set the new units

   DirectionCoordinate dc = *this;
   if (!dc.setWorldAxisUnits(unitsCanon)) {
      throw(AipsError("Could not set world axis units"));
   }

// The LinearXform is always in degrees. So we need to 
// account for the appropriate units in the scale factor

   const LinearXform& linear = dc.linear_p;

// Now we must make a LinearCoordinate.  We use the LinearXform class
// to do the inversion of the conversion matrices for us.

   Vector<Double> crpix(2), scale(2), crval(2,0.0);
   crpix(0) = Int(shape(0) / 2);
   crpix(1) = Int(shape(1) / 2);
//
   scale(0) = dc.to_degrees_p[0] / Double(shape(0));
   scale(1) = dc.to_degrees_p[1] / Double(shape(1));
//
   const LinearXform linear2 = linear.fourierInvert(axes, crpix, scale);
//
   return new LinearCoordinate(namesOut, unitsOut, crval, linear2.cdelt(),
                               linear2.pc(), linear2.crpix());
}



void DirectionCoordinate::makeDirectionCoordinate(Double refLong, Double refLat,
                                                  Double incLong, Double incLat,
                                                  const Matrix<Double> &xform,
                                                  Double refX, Double refY,
                                                  Double longPole, Double latPole)
{
// Initially we are in radians

    to_degrees_p.resize(2);
    to_radians_p.resize(2);
//
    to_degrees_p[0] = 1.0 / C::degree;
    to_degrees_p[1] = to_degrees_p[0];
    to_radians_p[0] = 1.0;
    to_radians_p[1] = 1.0; 
    units_p = "rad";
//
    Vector<Double> crval(2), cdelt(2), crpix(2);
    crval(0) = refLong; 
    crval(1) = refLat;
    cdelt(0) = incLong; 
    cdelt(1) = incLat;
    crpix(0) = refX;    
    crpix(1) = refY;
//
    toDegrees(crval);
    toDegrees(cdelt);
    linear_p = LinearXform(crpix, cdelt, xform);
//
    Double longPole2 = longPole;
    Double latPole2 = latPole;
    if (longPole < 999.0) {
       longPole2 = longPole * to_degrees_p[0];
    }
    if (latPole < 999.0) {
       latPole2 = latPole * to_degrees_p[1];
    }
//
    make_celprm_and_prjprm(canDoToMix_p, canDoToMixErrorMsg_p, celprm_p, 
                           prjprm_p, wcs_p, c_ctype_p, c_crval_p, 
                           projection_p, type_p, crval(0), crval(1),  
                           longPole2, latPole2);
}




Bool DirectionCoordinate::cylindricalFix (Int shapeLong, Int shapeLat)

{
//
// Fix up Cylindrical parameters for when longitude outside of [-180,180] range
// This has to be in its own function because the image shape intrudes.


// Fish out the bits and pieces we need to poke into the WCS routine

   Vector<Double> refPix = referencePixel().copy();
   double c_crpix[2];
   c_crpix[0] = refPix[0];
   c_crpix[1] = refPix[1];
//
   Vector<Double> refVal = referenceValue().copy();
   toDegrees(refVal);
   double c_crval[2];
   c_crval[0] = refVal[0];  
   c_crval[1] = refVal[1];
//
   Vector<Double> incr = increment().copy();
   toDegrees(incr);
   double c_cdelt[2];
   c_cdelt[0] = incr[0];
   c_cdelt[1] = incr[1];
//
   linprm* pLin = linear_p.linprmWCS();
   double pc[2][2] = {{pLin->pc[0], pLin->pc[1]},
                      {pLin->pc[2], pLin->pc[3]}};
   double c_lonPole = celprm_p->ref[2];
   double c_latPole = celprm_p->ref[3];
//
   int naxis[2];
   naxis[0] = shapeLong;
   naxis[1] = shapeLat;

// Updates crval, crpix, and lonPole

/*
cerr << "Initially: " << endl;
cerr << "crpix = " << c_crpix[0] << ", " <<  c_crpix[1] << endl;
cerr << "crval = " << c_crval[0] << ", " <<  c_crval[1] << endl;
cerr << "lonpol = " << c_lonPole << endl;
*/

   int ierr = cylfix (naxis, c_crpix, pc, c_cdelt, c_ctype_p, prjprm_p->p, c_crval,
                      &c_lonPole, c_latPole);

// ierr = 0 means successful update.
// ierr = -1 means no changed needed

   if (ierr==-1) return True;
//
   if (ierr == 0) {

/*
cerr << "Cylindrical update required" << endl;
cerr << "ierr = " << ierr << endl;
cerr << "New values : " << endl;
cerr << "crpix = " << c_crpix[0] << ", " <<  c_crpix[1] << endl;
cerr << "crval = " << c_crval[0] << ", " <<  c_crval[1] << endl;
cerr << "lonpol = " << c_lonPole << endl;
*/

// Update internals

      refVal[0] = c_crval[0];
      refVal[1] = c_crval[1];
      toOther(refVal);
      setReferenceValue (refVal);

// We have to poke in the new lonPole by hand

      celprm_p->ref[2] = c_lonPole;
//
      refPix[0] = c_crpix[0];
      refPix[1] = c_crpix[1];
      setReferencePixel(refPix);
   } else {
      set_error(String("DirectionCoordinate::cylindricalFix - ") +
                String("Could not convert CYL header to [-180,180] longitude range"));
      return False;
   }
  
// 
   return True;
}




Vector<Double> DirectionCoordinate::longLatPoles () const
{
    Vector<Double> x(4);
    x(0) = celprm_p->ref[0];          // refLong;
    x(1) = celprm_p->ref[1];          // refLat;
    x(2) = celprm_p->ref[2];          // longPole
    x(3) = celprm_p->ref[3];          // latPole
    return x;
}

// These world abs/rel functions are independent of the conversion direction type.

void DirectionCoordinate::makeWorldRelative (Vector<Double>& world) const
{
    static MVDirection mv;
    AlwaysAssert(world.nelements()==2, AipsError);
//
    mv.setAngle(world[0]*to_radians_p[0], world[1]*to_radians_p[1]);
    mv *= rot_p;
//
    world[0] = cos(mv.getLat()) * mv.getLong() / to_radians_p[0];
    world[1] = mv.getLat()  / to_radians_p[1];
}

void DirectionCoordinate::makeWorldRelative (MDirection& world) const
{
    static MVDirection mv;
    mv = world.getValue() * rot_p;
    Double lon = mv.getLong();
    Double lat = mv.getLat();
    mv.setAngle(cos(lat)*lon, lat);
    world.set(mv);
}


void DirectionCoordinate::makeWorldAbsolute (Vector<Double>& world) const
{
    static MVDirection mv;
    AlwaysAssert(world.nelements()==2, AipsError);
//   
    Double lat = world[1]*to_radians_p[1];
    mv.setAngle(world[0]*to_radians_p[0]/cos(lat), lat);
    mv = rot_p * mv;
//
    world[0] = mv.getLong() / to_radians_p[0];
    world[1]= mv.getLat()  / to_radians_p[1];
}

void DirectionCoordinate::makeWorldAbsolute (Vector<Double>& world,
                                             const Vector<Double>& refVal) const
{
    static MVDirection mv;
    AlwaysAssert(world.nelements()==2, AipsError);
    AlwaysAssert(refVal.nelements()==2, AipsError);
//   
    RotMatrix rot;
    setRotationMatrix(rot, refVal(0), refVal(1));
//
    Double lat = world[1]*to_radians_p[1];
    mv.setAngle(world[0]*to_radians_p[0]/cos(lat), lat);
    mv = rot * mv;
//
    world[0] = mv.getLong() / to_radians_p[0];
    world[1]= mv.getLat()  / to_radians_p[1];
}

void DirectionCoordinate::makeWorldAbsolute (MDirection& world) const
{
    static MVDirection mv;
//
    Double lon = world.getValue().getLong();
    Double lat = world.getValue().getLat();
    mv.setAngle(lon/cos(lat), lat);
//
    world.set(rot_p * mv);
}



Bool DirectionCoordinate::setWorldMixRanges (const IPosition& shape)
{
   AlwaysAssert(nWorldAxes()==nPixelAxes(), AipsError);
   const uInt n = shape.nelements();
   if (n!=nPixelAxes()) {
      set_error("Shape must be of length nPixelAxes");
      return False;
   }
//
   worldMin_p.resize(2);
   worldMax_p.resize(2);
   const Vector<Double>& cdelt = increment();

// Find centre of image.
// If the shape is -1, it means its not known for this world
// axis (user may have removed a pixel axis in CoordinateSystem)
// Use reference pixel in this case

   Vector<Double> pixelIn(referencePixel().copy());
   Vector<Double> worldOut(nWorldAxes());
   for (uInt i=0; i<nPixelAxes(); i++) {
      if (shape(i) > 0) {
         pixelIn(i) = shape(i) / 2.0;
      }
   }
   if (!toWorld(worldOut, pixelIn)) return False;
//
   Vector<String> units = worldAxisUnits();
   Double cosdec = cos(worldOut(1) * to_radians_p[1]);
   Double fac = 1.0;

// If the shape is -1, it means its not known for this world
// axis (user may have removed a pixel axis in CoordinateSystem)
// They might have also have removed a pixel axis  but not a world axis.
// Really, in this case, the replacement world value should be used.


   Float frac = 0.5;
   Int n2 = 0;
   for (uInt i=0; i<2; i++) {
      fac = 1.0;
      if (i==0) fac = cosdec;

// Find number of pixels to offset from centre.
// Do something arbitrary if the shape is <= 1

      if (shape(i)<=1) {
         n2 = 10;
      } else  if (shape(i) > 0) { 
         n2 = (shape(i) + Int(frac*shape(i))) / 2;
      }
//
      worldMin_p(i) = worldOut(i) - abs(cdelt(i))*n2/fac;    
      worldMax_p(i) = worldOut(i) + abs(cdelt(i))*n2/fac;
//
      if (i==0) {
         worldMin_p(i) = putLongInPiRange (worldMin_p(i), units(i));
         worldMax_p(i) = putLongInPiRange (worldMax_p(i), units(i));
      }
   }
//

   return True;
}



void DirectionCoordinate::setWorldMixRanges (const Vector<Bool>& which,
                                             const Vector<Double>& world)
{
   AlwaysAssert(which.nelements()==nWorldAxes(), AipsError);
   AlwaysAssert(world.nelements()==nWorldAxes(), AipsError);
//
   worldMin_p.resize(2, True);
   worldMax_p.resize(2, True);
   const Vector<Double>& cdelt = increment();
   const Vector<String>& units = worldAxisUnits();
//
   Vector<Double> pixelIn(referencePixel().copy());
   Vector<Double> worldOut;
   toWorld(worldOut, pixelIn);
//
   Double cosdec = cos(worldOut(1) * to_radians_p[1]);
   Int n = 10;                                          // Arbitrary offset
   for (uInt i=0; i<nWorldAxes(); i++) {
      if (which(i)) {
         if (i==0) {
            worldMin_p(i) = world(i) - abs(cdelt(i))*n/cosdec;    
            worldMax_p(i) = world(i) + abs(cdelt(i))*n/cosdec;

// Put longitude  in -pi -> pi range

            worldMin_p(i) = putLongInPiRange (worldMin_p(i), units(0));
            worldMax_p(i) = putLongInPiRange (worldMax_p(i), units(0));
         } else {
            worldMin_p(i) = world(i) - abs(cdelt(i))*n;
            worldMin_p(i) = max(worldMin_p(i), -90.0/to_degrees_p[1]);
//
            worldMax_p(i) = world(i) + abs(cdelt(i))*n;
            worldMax_p(i) = min(worldMax_p(i),  90.0/to_degrees_p[1]);
         }
      }
   }
}



void DirectionCoordinate::setDefaultWorldMixRanges ()
{
   worldMin_p.resize(2);
   worldMax_p.resize(2);
   worldMin_p(0) = -180.0/to_degrees_p[0];     //long
   worldMax_p(0) =  180.0/to_degrees_p[0];
   worldMin_p(1) =  -90.0/to_degrees_p[1];     //lat
   worldMax_p(1) =   90.0/to_degrees_p[1];
}

void DirectionCoordinate::setRotationMatrix ()
{
   setRotationMatrix(rot_p, referenceValue()(0), referenceValue()(1));
}


void DirectionCoordinate::setRotationMatrix (RotMatrix& rot, 
                                             Double lon, Double lat) const
// 
// Set rotation matrix for use in handling offset coordinates
//
{
    Double refLon = lon * to_radians_p[0];
    Double refLat = lat * to_radians_p[1];
//
    MVDirection refPos(refLon, refLat);
    Euler eul(refLat, 2u, -refLon, 3);
    RotMatrix rot2(eul);
    rot2.transpose();
//
    rot = rot2;
}

void DirectionCoordinate::makeConversionMachines ()
{
   if (type_p != conversionType_p) {
      MDirection::Ref oldType(type_p);
      MDirection::Ref newType(conversionType_p);
      pConversionMachineTo_p   = new MDirection::Convert(oldType, newType);
      pConversionMachineFrom_p = new MDirection::Convert(newType, oldType);
   }
}


void DirectionCoordinate::convertTo (Vector<Double>& world) const
{
   static MVDirection inMV;

// I can't set the machine to operate in the native units because
// the user can set them differently for lon and lat

   if (pConversionMachineTo_p) {
      inMV.setAngle(world[0]*to_radians_p[0], world[1]*to_radians_p[1]);
      world  = (*pConversionMachineTo_p)(inMV).getValue().get() / to_radians_p;
   }
}

void DirectionCoordinate::convertFrom (Vector<Double>& world) const
{
   static MVDirection inMV;

// I can't set the machine to operate in the native units because
// the user can set them differently for lon and lat

   if (pConversionMachineFrom_p) {
      inMV.setAngle(world[0]*to_radians_p[0], world[1]*to_radians_p[1]);
      world = (*pConversionMachineFrom_p)(inMV).getValue().get() / to_radians_p;
   }
}

Double DirectionCoordinate::putLongInPiRange (Double lon, const String& unit) const
{  
   Unit u(unit);
   Quantum<Double> q(lon, u);
   MVAngle mva(q);
   const MVAngle& mva2 = mva();
   Double t = mva2.get(u).getValue();
//   if (t < 0) t += 360.0/to_degrees_p[0];
   return t;
}



void DirectionCoordinate::listProj ()  const
{
   cerr << endl << "Proj struct" << endl;
   cerr << "projection type = " << projection_p.type() << endl;
   cerr << " flag = " << prjprm_p->flag << endl;
   cerr << " r0   = " << prjprm_p->r0 << endl;
   cerr << " p    = ";
   for (uInt i=0; i<10; i++) {
      cerr << prjprm_p->p[i]  << ", ";
   }
   cerr << endl;
   cerr << " n    = " << prjprm_p->n << endl;
   cerr << " w    = ";
   for (uInt i=0; i<10; i++) {
      cerr << prjprm_p->w[i]  << ", ";
   }
   cerr << endl;
}

void DirectionCoordinate::listCel ()  const
{
   cerr << endl << "celprm_p struct" << endl;
   cerr << " flag = " << celprm_p->flag << endl;
   cerr << " ref  = " << celprm_p->ref[0] << ", " << celprm_p->ref[1] << ", " <<
                         celprm_p->ref[2] << ", " << celprm_p->ref[3] << endl;
   cerr << " euler= ";
   for (uInt i=0; i<5; i++) {
      cerr << celprm_p->euler[i]  << ", ";
   }
   cerr << endl;
}


void DirectionCoordinate::listWCS ()  const
{
   cerr << endl << "wcsprm struct" << endl;
   cerr << " flag = " << wcs_p->flag << endl;
   cerr << " pcode = " << wcs_p->pcode << endl;
   cerr << " lngtyp = ";
   for (uInt i=0; i<5; i++) {
      cerr << wcs_p->lngtyp[i]  << ", ";
   }
   cerr << endl;
   cerr << " lattyp = ";
   for (uInt i=0; i<5; i++) {
      cerr << wcs_p->lattyp[i]  << ", ";
   }
   cerr << endl;
   cerr << " lng, lat = " << wcs_p->lng << ", " << wcs_p->lat << endl;
   cerr << " cubeface = " << wcs_p->cubeface << endl;
   cerr << endl;
}


void DirectionCoordinate::listLin (linprm* linprm)  const
{
   cerr << endl << "linprm struct" << endl;
   cerr << " flag = " << linprm->flag << endl;
   cerr << " naxis = " << linprm->naxis << endl;
   cerr << " crpix = " << linprm->crpix[0] << ", " << linprm->crpix[1] << endl;
   cerr << " cdelt = " << linprm->cdelt[0] << ", " << linprm->cdelt[1] << endl;
   cerr << " pc = " << linprm->pc[0] << ", " << linprm->pc[1] << ", " <<
                       linprm->pc[2] << ", " << linprm->pc[3] << endl;
}
