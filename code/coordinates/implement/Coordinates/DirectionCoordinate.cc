//# DirectionCoordinate.cc: this defines the DirectionCoordinate class
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

#include <wcslib/wcs.h>
#include <wcslib/cel.h>
#include <wcslib/proj.h>

#include <trial/Coordinates/DirectionCoordinate.h>
#include <trial/Coordinates/LinearCoordinate.h>
#include <trial/Coordinates/Coordinate.h>

#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Containers/Record.h>
#include <aips/Logging/LogIO.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Quanta/MVAngle.h>
#include <aips/Quanta/Quantum.h>
#include <aips/Quanta/Unit.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/LinearSearch.h>
#include <aips/Utilities/String.h>

#include <iomanip.h>  
#include <strstream.h>


DirectionCoordinate::DirectionCoordinate()
: Coordinate(),
  type_p(MDirection::J2000), 
  projection_p(Projection(Projection::CAR)),
  celprm_p(0), 
  prjprm_p(0), 
  wcs_p(0), 
  linear_p(1),
  names_p(2), 
  units_p(2), 
  canDoToMix_p(True),
  canDoToMixErrorMsg_p("")
{
    // Initially we are in radians
    to_degrees_p[0] = 180.0 / C::pi;
    to_degrees_p[1] = to_degrees_p[0];
    to_radians_p[0] = 1.0;
    to_radians_p[1] = 1.0;
    units_p = "rad";

    Vector<Double> crval(2), cdelt(2), crpix(2);
    crval = 0.0;
    cdelt = 1.0;
    crpix = 0.0;
    Matrix<Double> xform(2,2); 
    xform = 0.0;
    xform.diagonal() = 1.0;

    toDegrees(crval);
    toDegrees(cdelt);
    linear_p = LinearXform(crpix, cdelt, xform);
    make_celprm_and_prjprm(canDoToMix_p, canDoToMixErrorMsg_p,
                           celprm_p, prjprm_p, wcs_p, 
                           c_ctype_p, c_crval_p, projection_p, type_p, 
                           crval(0), crval(1), 999.0, 999.0);

}

DirectionCoordinate::DirectionCoordinate(MDirection::Types directionType,
                                         const Projection &projection,
                                         Double refLong, Double refLat,
                                         Double incLong, Double incLat,
                                         const Matrix<Double> &xform,
                                         Double refX, Double refY)
: Coordinate(),
  type_p(directionType), 
  projection_p(projection),
  celprm_p(0), 
  prjprm_p(0), 
  wcs_p(0), 
  linear_p(1),
  names_p(axisNames(directionType).copy()),
  units_p(2),
  canDoToMix_p(True),
  canDoToMixErrorMsg_p("")
{
    // Initially we are in radians
    to_degrees_p[0] = 180.0 / C::pi;
    to_degrees_p[1] = to_degrees_p[0];
    to_radians_p[0] = 1.0;
    to_radians_p[1] = 1.0; 
    units_p = "rad";

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
    make_celprm_and_prjprm(canDoToMix_p, canDoToMixErrorMsg_p, celprm_p, 
                           prjprm_p, wcs_p, c_ctype_p, c_crval_p, 
                           projection_p, type_p, crval(0), crval(1),  
                           999.0, 999.0);
}

DirectionCoordinate::DirectionCoordinate(const DirectionCoordinate &other)
: Coordinate(other),
  type_p(other.type_p), 
  projection_p(other.projection_p),
  celprm_p(0), 
  prjprm_p(0), 
  wcs_p(0), 
  linear_p(other.linear_p),
  names_p(other.names_p.copy()), 
  units_p(other.units_p.copy()),
  canDoToMix_p(other.canDoToMix_p),
  canDoToMixErrorMsg_p(other.canDoToMixErrorMsg_p)
{
    to_degrees_p[0] = other.to_degrees_p[0];
    to_degrees_p[1] = other.to_degrees_p[1];
    to_radians_p[0] = other.to_radians_p[0];
    to_radians_p[1] = other.to_radians_p[1];
//
    copy_celprm_and_prjprm(celprm_p, prjprm_p, wcs_p, c_ctype_p, 
                           c_crval_p, other.celprm_p, other.prjprm_p,
                           other.wcs_p, other.c_ctype_p, other.c_crval_p);
}


DirectionCoordinate &DirectionCoordinate::operator=(const DirectionCoordinate &other)
{
    if (this != &other) {
        Coordinate::operator=(other);
//
	type_p = other.type_p;
	projection_p = other.projection_p;
//
	DirectionCoordinate::copy_celprm_and_prjprm(celprm_p, prjprm_p, wcs_p, 
                               c_ctype_p, c_crval_p,
			       other.celprm_p, other.prjprm_p,
                               other.wcs_p, 
			       other.c_ctype_p, other.c_crval_p);
	linear_p = other.linear_p;
	names_p = other.names_p.copy();
	units_p = other.units_p.copy();
	to_degrees_p[0] = other.to_degrees_p[0];
	to_degrees_p[1] = other.to_degrees_p[1];
	to_radians_p[0] = other.to_radians_p[0];
	to_radians_p[1] = other.to_radians_p[1];
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


Bool DirectionCoordinate::toWorld(Vector<Double> &world,
				  const Vector<Double> &pixel) const
{

// Temporaries

    static double d_phi, d_theta, d_x, d_y, d_lng, d_lat;
    static String errorMsg;
//
    if (world.nelements()!=2) world.resize(2);
    AlwaysAssert(pixel.nelements() == 2, AipsError);

// world contains linear xformed numbers

    Bool ok = linear_p.reverse(world, pixel, errorMsg);
    if (ok) {
	d_x = world(0);
        d_y = world(1);
	int errnum = celrev(pcodes[projection_p.type()],
			    d_x, d_y, prjprm_p, &d_phi, &d_theta,
			    celprm_p, &d_lng, &d_lat);
	ok = ToBool((errnum == 0));
	if (ok) {
	    world(0) = d_lng;
	    world(1) = d_lat;

// Convert to appropriate units from degrees.  phi and theta
// may be returned in a different interface somewhen.

//          phi = d_phi / to_degrees_p[0];
//          theta = d_theta / to_degrees_p[1];
           toOther(world);
	} else {
           errorMsg = "wcslib celrev error: ";
           errorMsg += celrev_errmsg[errnum];
	}
    } 
//
    if (!ok) set_error(errorMsg);

//
    return ok;
}


Bool DirectionCoordinate::toPixel(Vector<Double> &pixel,
				  const Vector<Double> &world) const
{
// Temporaries 

    static Vector<Double> world_tmp;
    static double d_theta, d_phi, d_lng, d_lat, d_x, d_y;
    static String errorMsg;
//
    if (pixel.nelements()!=2) pixel.resize(2);
    AlwaysAssert(world.nelements() == nWorldAxes(), AipsError);
    if (world_tmp.nelements()!=nWorldAxes()) world_tmp.resize(nWorldAxes());
//
    world_tmp(0) = world(0); 
    world_tmp(1) = world(1);
    toDegrees(world_tmp);

    d_lng = world_tmp(0);
    d_lat = world_tmp(1);
    int errnum = celfwd(pcodes[projection_p.type()], d_lng, d_lat,
			celprm_p, &d_phi, &d_theta, prjprm_p, 
                        &d_x, &d_y);
    Bool ok = ToBool(errnum == 0);
    if (!ok) {
       errorMsg = "wcslib celfwd error: ";
       errorMsg += celfwd_errmsg[errnum];
    } else {
        world_tmp(0) = d_x; 
        world_tmp(1) = d_y;
//
	ok = linear_p.forward(world_tmp, pixel, errorMsg);

// phi and theta may be returned in a different interface somewhen

//        phi = d_phi;
//        theta = d_theta;
    }
//
    if (!ok) set_error(errorMsg);
    return ok;
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
   if (worldOut.nelements()!=nWorldAxes()) worldOut.resize(nWorldAxes());
   if (pixelOut.nelements()!=nPixelAxes()) pixelOut.resize(nPixelAxes());
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
      if (in_tmp.nelements()!=2) in_tmp.resize(2);
      if (out_tmp.nelements()!=2) out_tmp.resize(2);
//
      in_tmp(0) = pixelIn(0);
      in_tmp(1) = worldIn(1);
      if (!toMix2(out_tmp, in_tmp, worldMin, 
                  worldMax, False)) return False;
//
      pixelOut(0) = in_tmp(0);
      pixelOut(1) = out_tmp(1);
      worldOut(0) = out_tmp(0);
      worldOut(1) = in_tmp(1);  
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
      if (in_tmp.nelements()!=2) in_tmp.resize(2);
      if (out_tmp.nelements()!=2) out_tmp.resize(2);
//
      in_tmp(0) = worldIn(0);
      in_tmp(1) = pixelIn(1);
      if (!toMix2(out_tmp, in_tmp, worldMin, 
                  worldMax, True)) return False;
//
      pixelOut(0) = out_tmp(0);
      pixelOut(1) = in_tmp(1);
      worldOut(0) = in_tmp(0);
      worldOut(1) = out_tmp(1);
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
    return names_p.copy();
}

Vector<String> DirectionCoordinate::worldAxisUnits() const
{
    return units_p.copy();
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
    Bool ok = ToBool(names.nelements()==nWorldAxes());
    if (!ok) {
       set_error("names vector must be of length 2");
       return False;
    }
//
    names_p = names;
    return True;
}

Bool DirectionCoordinate::setWorldAxisUnits(const Vector<String> &units, 
                                            Bool adjust)
{
    Bool ok = ToBool(units.nelements()==nWorldAxes());
    if (!ok) {
       set_error("units vector must be of length 2");
       return False;
    }
//
    if (adjust) {
	String error;
	Vector<Double> factor;
	ok = find_scale_factor(error, factor, units, worldAxisUnits());
	if (ok) {
	    to_degrees_p[0] /= factor(0);
	    to_degrees_p[1] /= factor(1);
//
            to_radians_p[0] = to_degrees_p[0] * C::pi / 180.0;
            to_radians_p[1] = to_degrees_p[1] * C::pi / 180.0;
	} else {
	    set_error(error);
	}
    }
    if (ok) units_p = units;
    return ok;
}


Bool DirectionCoordinate::setReferencePixel(const Vector<Double> &refPix)
{
    Bool ok = ToBool(refPix.nelements()==nPixelAxes());
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
    Bool ok = ToBool(xform.nrow() == nWorldAxes() && 
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
    Bool ok = ToBool(inc.nelements()==nWorldAxes());
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
    Bool ok = ToBool(refval.nelements()==nWorldAxes());
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
    ok = ToBool(errnum==0);
    if (!ok) {
       String errmsg = "wcs celset_error: ";
       errmsg += celset_errmsg[errnum];
    }
//
// Fill in the silly wcs private members I have to cart about
// for toMix caching
//
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
	    names(0) = "Galactic Longitude";
	    names(1) = "Galactic Latitude";
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
                                   uInt axis,
                                   Bool absolute,
                                   Int precision,
                                   Bool nativeU) const
//
// native=T means radians !
// native=F means degrees, or hours
//
{
   AlwaysAssert(axis< nWorldAxes(), AipsError);
   AlwaysAssert(nWorldAxes()==2, AipsError);

// Fill in DEFAULT format

   Coordinate::formatType form = format;

// Check format.  If absolute==F, TIME formatting is switched off

   checkFormat(form, absolute);
   Bool native = nativeU;   
   if (form==Coordinate::TIME) native = False;

// Set default precision if needed

   Int prec = precision;
   if (prec < 0) getPrecision(prec, form, absolute, -1, -1, -1);

// Set global DirectionCoordinate type

   MDirection::GlobalTypes gtype = MDirection::globalType(type_p);
   
// Convert to radians first

   Double worldValue2 = C::pi * worldValue * to_degrees_p[axis] / 180.0;
//
   ostrstream oss;         
   MVAngle mVA(worldValue2);
   MVAngle mVA2(mVA);
   units = "rad";
   Double value = mVA2.get().getValue();     // Radians

// We need to put the longitudes into the correct convention
// All latitudes better be -90 -> 90 and we don't need to fiddle them

   if (axis==0) {
      if (gtype==MDirection::GRADEC){ 
         if (absolute) mVA2 = mVA(0.0);                   // 0->2pi (0->360)
         if (!native) {
            value = mVA2.get(Unit("deg")).getValue();
            units = "deg";
         } else {
           value = mVA2.get().getValue(); 
         }
      } else if (gtype==MDirection::GHADEC) {
         if (absolute) mVA2 = mVA();                      // -pi->pi (-180->180)
         if (!native) {
            value = mVA2.get().getValue() * 24.0 / C::_2pi;
            units = "h";
         } else {
            value = mVA2.get().getValue(); 
         }
      } else if (gtype==MDirection::GAZEL) {
         if (absolute) mVA2 = mVA(0.0);                   // 0->2pi (0->360)
         if (!native) {
            value = mVA2.get(Unit("deg")).getValue();
            units = "deg";
         } else {
            value = mVA2.get().getValue();
         }
      } else if (gtype==MDirection::GLONGLAT) {
         if (type_p==MDirection::ECLIPTIC ||
             type_p==MDirection::MECLIPTIC ||
             type_p==MDirection::TECLIPTIC) {
            if (absolute) mVA2 = mVA();                  // -pi->pi (-180->180)
         } else {
            if (absolute) mVA2 = mVA(0.0);               // 0->2pi (0->360)
         }
         if (!native) {
            value = mVA2.get(Unit("deg")).getValue();
            units = "deg";
         } else {
            value = mVA2.get().getValue();
         }
      } else {
         if (absolute) mVA2 = mVA(0.0);                  // 0->2pi (0->360)
         value = mVA2.get().getValue();
      }
//
      if (!absolute) {
         if (units=="deg") value *= 3600.0;
         units = "arcsec";
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
      } else if (form==Coordinate::TIME) {

// Format with mVA2.  TIME formatting really only makes
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
      }
   } else {
      if (!native) {
         value = mVA2.get(Unit("deg")).getValue();
         units = "deg";
      }
//
      if (!absolute) {
         if (units=="deg") value *= 3600.0;
         units = "arcsec";
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
      } else if (form==Coordinate::TIME) {

// Format with mVA2.  

         prec += 6;
         oss << mVA2.string(MVAngle::ANGLE+MVAngle::DIG2,prec);
      }
   }
//
   oss << ends;
   String string(oss);

// Only return actual units if not TIME formatting

   if (form==Coordinate::TIME) units = String("");
//
   return string;
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


Bool DirectionCoordinate::near(const Coordinate* pOther, 
                               Double tol) const
{
   Vector<Int> excludeAxes;
   return near(pOther, excludeAxes, tol);
}



Bool DirectionCoordinate::near(const Coordinate* pOther, 
                               const Vector<Int>& excludeAxes,
                               Double tol) const

{
   if (this->type() != pOther->type()) {
      set_error("Comparison is not with another DirectionCoordinate");
      return False;
   }
     
   DirectionCoordinate* dCoord = (DirectionCoordinate*)pOther;
   
   if (!projection_p.near(dCoord->projection_p, tol)) {
      set_error("The DirectionCoordinates have differing projections");
      return False;
   }
   if (type_p != dCoord->type_p) {
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
      Int j = 0;
      for (uInt i=0; i<nPixelAxes(); i++) {
         if (linearSearch(found, excludeAxes, Int(i), nExcl) >= 0) 
           exclude(j++) = True;
      }
   }

// Check linear transformation

   if (!linear_p.near(dCoord->linear_p, excludeAxes, tol)) {
      set_error("The DirectionCoordinates have differing linear transformation matrices");
      return False;
   }


// Check names and units

   ostrstream oss;
   if (names_p.nelements() != dCoord->names_p.nelements()) {
      set_error("The DirectionCoordinates have differing numbers of world axis names");
      return False;
   }
   if (units_p.nelements() != dCoord->units_p.nelements()) {
      set_error("The DirectionCoordinates have differing numbers of axis units");
      return False;
   }
   for (uInt i=0; i<names_p.nelements(); i++) {
      if (!exclude(i)) {
         if (names_p(i) != dCoord->names_p(i)) {
            oss << "The DirectionCoordinates have differing axis names for axis "
                << i << ends;
            set_error(String(oss));
            return False;      
         }
      }
   }
   for (uInt i=0; i<units_p.nelements(); i++) {
      if (!exclude(i)) {
         if (units_p(i) != dCoord->units_p(i)) {
            oss << "The DirectionCoordinates have differing axis units for axis "
                << i << ends;
            set_error(String(oss));
            return False;      
         }
      }
   }

     
// WCS structures.  

   if (celprm_p->flag != dCoord->celprm_p->flag) {
      set_error("The DirectionCoordinates have differing WCS spherical coordinate structures");
      return False;
   }

// Items 0 and 2 are longitudes, items 1 and 3 are latitiudes
// So treat them as axes 0 and 1 ???

   for (uInt i=0; i<2; i++) {
     if (!exclude(i)) {
        if (!::near(celprm_p->ref[i],dCoord->celprm_p->ref[i],tol)) {
           set_error("The DirectionCoordinates have differing WCS spherical coordinate structures");
           return False;
        }
        if (!::near(celprm_p->ref[i+2],dCoord->celprm_p->ref[i+2],tol)) {
           set_error("The DirectionCoordinates have differing WCS spherical coordinate structures");
           return False;
        }
     }
   }

// No idea what to do here for exclusion axes !
// Don't test prjprm->flag as it is a transient flag 
// whose value may change depending on what computations you
// do when
// 

   if (prjprm_p->r0!=0.0 && dCoord->prjprm_p->r0!=0.0) {

// 0 is the start value, and the WCS routines will change
// it, so if either value is 0, it's ok

      if (prjprm_p->r0 != dCoord->prjprm_p->r0) {
         set_error("The DirectionCoordinates have differing WCS projection parameters");
         return False;
       }
   }
   for (uInt i=0; i<10; i++) {
      if (!::near(prjprm_p->p[i],dCoord->prjprm_p->p[i],tol)) {
         set_error("The DirectionCoordinates have differing WCS projection structures");
         return False;
      }
   }
   
                          
// Probably these arrays are implicitly the same if the units are the same
// but might as well make sure

   if (!exclude(0)) {
      if (!::near(to_degrees_p[0],dCoord->to_degrees_p[0],tol)) {
         set_error("The DirectionCoordinates have differing unit conversion vectors");
         return False;
      }
   }
   if (!exclude(1)) {
      if (!::near(to_degrees_p[1],dCoord->to_degrees_p[1],tol)) {
         set_error("The DirectionCoordinates have differing unit conversion vectors");
         return False;
      }
   }

// These things are needed only for toMix caching speedup

   for (uInt i=0; i<2; i++) {
      if (strcmp(c_ctype_p[i],dCoord->c_ctype_p[i])!=0) {
         set_error("The DirectionCoordinates have differing FITS ctypes");
         return False;
      }
      if (!::near(c_crval_p[i],dCoord->c_crval_p[i],tol)) {
         set_error("The DirectionCoordinates have differing FITS crvals");
         return False;
      }
   }
// 
// The flag variable may change with use, so don't test it.
//   Bool ok = wcs_p->flag==dCoord->wcs_p->flag &&
//
   Bool ok = strcmp(wcs_p->pcode,dCoord->wcs_p->pcode)==0 &&
        strcmp(wcs_p->lngtyp,dCoord->wcs_p->lngtyp)==0 &&
        strcmp(wcs_p->lattyp,dCoord->wcs_p->lattyp)==0 &&
        wcs_p->lng==dCoord->wcs_p->lng &&
        wcs_p->lat==dCoord->wcs_p->lat &&        
        wcs_p->cubeface==dCoord->wcs_p->cubeface;
   if (!ok) {
      set_error("The DirectionCoordinates have differing wcs structures");
      return False;
   }
   return True;
}




Bool DirectionCoordinate::save(RecordInterface &container,
			    const String &fieldName) const
{
    Bool ok = ToBool(!container.isDefined(fieldName));
    if (ok) {
	Record subrec;
	Projection proj = projection();
	String system = MDirection::showType(type_p);

	subrec.define("system", system);
	subrec.define("projection", proj.name());
	subrec.define("projection_parameters", proj.parameters());
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

DirectionCoordinate *DirectionCoordinate::restore(const RecordInterface &container,
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
    MDirection::Ref mref;
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
                                pc, crpix(0), crpix(1));
    AlwaysAssert(retval, AipsError);

    // Set the actual units and names
    retval->setWorldAxisUnits(units);
    retval->setWorldAxisNames(axes);
							  
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
    static int mixpix;
    static int mixcel;
    static double mix_vspan[2];
    static double mix_world[2];
    static double mix_pixcrd[2];
    static double mix_imgcrd[2];
    static double mix_vstep;
    static int mix_viter;
    static double mix_phi, mix_theta;
    static String errorMsg;
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
//
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
//cout << "doMix2: mixpix = " << mixpix << endl;
//cout << "doMix2: mixcel = " << mixcel << endl;
//cout << "doMix2: input world = " << mix_world[0] << ", " << mix_world[1] << endl;
//cout << "doMix2: input pixel = " << mix_pixcrd[0] << ", " << mix_pixcrd[1] << endl;
//cout << "doMix2: input vspan_p = " << mix_vspan[0] << ", " << mix_vspan[1] << endl;
//cout << "doMix2: c_crval= " << c_crval_p[0] << " " << c_crval_p[1] << endl;
//
    mix_vstep = 1.0;
    mix_viter = 2;
    int iret = wcsmix(c_ctype_p, wcs_p, mixpix, mixcel, mix_vspan, 
                      mix_vstep, mix_viter, 
                      mix_world, c_crval_p, celprm_p, &mix_phi, &mix_theta, 
                      prjprm_p, mix_imgcrd, linear_p.linprmWCS(), mix_pixcrd);
    if (iret!=0) {
        errorMsg= "wcs wcsmix_error: ";
        errorMsg += wcsmix_errmsg[iret];
        set_error(errorMsg);
        return False;
    }
//
// Fish out the results
//
//cout << "doMix2: output world = " << mix_world[0] << ", " << mix_world[1] << endl;
//cout << "doMix2: output pixel = " << mix_pixcrd[0] << ", " << mix_pixcrd[1] << endl;
//cout << "doMix2: phi, theta=" << phi << ", " << theta << endl;

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
           celprm* &pCelPrm, prjprm* &pPrjPrm, wcsprm* &pWcs,  
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
    pCelPrm->ref[2] = longPole;
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
    Vector<String> axisNames = DirectionCoordinate::axisNames(directionType, True);
    Vector<String> ctype = make_Direction_FITS_ctype(proj, axisNames, 
                                                     refLat*C::pi/180.0, False);
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

   Vector<String> names = worldAxisNames();
   Vector<String> units = worldAxisUnits();
   Vector<String> unitsCanon = worldAxisUnits();
//
   Vector<String> namesOut = worldAxisNames();
   Vector<String> unitsOut = worldAxisUnits();
   fourierUnits (namesOut(0), unitsOut(0), unitsCanon(0), Coordinate::DIRECTION, 0,
                 units(0), names(0));
   fourierUnits (namesOut(1), unitsOut(1), unitsCanon(1), Coordinate::DIRECTION, 1,
                 units(1), names(1));

// Make a copy of ourselves and set the new units

   DirectionCoordinate dc = *this;
   if (!dc.setWorldAxisUnits(unitsCanon, True)) {
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
