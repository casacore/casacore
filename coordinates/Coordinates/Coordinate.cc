//#Coordinate.cc: this defines the Coordinate class
//# Copyright (C) 1997,1998,1999,2000,2001,2002,2003,2004
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


#include <casacore/coordinates/Coordinates/Coordinate.h>

#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayAccessor.h>
#include <casacore/coordinates/Coordinates/Projection.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/measures/Measures/MDirection.h>
#include <casacore/casa/Quanta/Unit.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Utilities/Regex.h>

#include <casacore/casa/OS/Timer.h>

#include <casacore/casa/iomanip.h>
#include <casacore/casa/sstream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

Coordinate::Coordinate()
: worldMin_p(0),
  worldMax_p(0)
{}

Coordinate::Coordinate(const Coordinate& other)
: worldMin_p(0),
  worldMax_p(0),
  error_p(other.error_p)
{
   worldMin_p = other.worldMin_p;
   worldMax_p = other.worldMax_p;
}

Coordinate& Coordinate::operator=(const Coordinate& other)
{
   if (this != &other) {
      worldMin_p.resize(other.worldMin_p.nelements());
      worldMax_p.resize(other.worldMax_p.nelements());
//
      worldMin_p = other.worldMin_p;
      worldMax_p = other.worldMax_p;
      error_p = other.error_p;
   }
   return *this;
}


Coordinate::~Coordinate()
{}


bool Coordinate::toWorldMany(Matrix<double>& world,
                             const Matrix<double>& pixel,
                             Vector<bool>& failures) const
{
    AlwaysAssert(nPixelAxes()==pixel.nrow(), AipsError);
    const uint32_t nTransforms = pixel.ncolumn();
    world.resize(nWorldAxes(), nTransforms);
    failures.resize(nTransforms);
//
    Vector<double> pixTmp(nPixelAxes());
    Vector<double> worldTmp(nWorldAxes());
//
    ArrayAccessor<double, Axis<1> > jPixel(pixel);
    ArrayAccessor<double, Axis<1> > jWorld(world);
//
    String errorMsg;
    uint32_t nError = 0;
    uint32_t k,l;
    ArrayAccessor<double, Axis<0> > iPixel, iWorld;
//
    for (jPixel.reset(),jWorld.reset(),l=0; jPixel!=jPixel.end(); ++jPixel,++jWorld,l++) {
       iPixel = jPixel;            // Partial assignment
       for (iPixel.reset(),k=0; iPixel!=iPixel.end(); ++iPixel,k++) {
          pixTmp[k] = *iPixel;
       }
//
       failures[l] = !toWorld (worldTmp, pixTmp);
       if (failures[l]) {
          nError++;
          if (nError == 1) errorMsg = errorMessage();    // Save the first error message
        } else {
           iWorld = jWorld;      // Partial assigment
           for (iWorld.reset(),k=0; iWorld!=iWorld.end(); ++iWorld,k++) {
              *iWorld = worldTmp[k];
           }
        }
    }
//
    if (nError != 0) set_error(errorMsg); // put back the first error
    return (nError==0);

}


bool Coordinate::toPixelMany(Matrix<double>& pixel,
                             const Matrix<double>& world,
                             Vector<bool>& failures) const
{
    AlwaysAssert(nWorldAxes()==world.nrow(), AipsError);
    const uint32_t nTransforms = world.ncolumn();
    pixel.resize(nPixelAxes(), nTransforms);
    failures.resize(nTransforms);
//
    Vector<double> pixTmp(nPixelAxes());
    Vector<double> worldTmp(nWorldAxes());
//
    ArrayAccessor<double, Axis<1> > jPixel(pixel);
    ArrayAccessor<double, Axis<1> > jWorld(world);
//
    String errorMsg;
    uint32_t nError = 0;
    uint32_t k,l;
    ArrayAccessor<double, Axis<0> > iPixel, iWorld;
//
    for (jWorld.reset(),jPixel.reset(),l=0; jWorld!=jWorld.end(); ++jWorld,++jPixel,l++) {
       iWorld = jWorld;           // Partial assigment
       for (iWorld.reset(),k=0; iWorld!=iWorld.end(); ++iWorld,k++) {
          worldTmp[k] = *iWorld;
       }
//
       failures[l] = !toPixel(pixTmp, worldTmp);
       if (failures[l]) {
          nError++;
          if (nError == 1) errorMsg = errorMessage();    // Save the first error message
        } else {
           iPixel = jPixel;      // Partial assignment
           for (iPixel.reset(),k=0; iPixel!=iPixel.end(); ++iPixel,k++) {
              *iPixel= pixTmp[k];
           }
        }
    }
//
    if (nError != 0) set_error(errorMsg); // put back the first error
    return (nError==0);
}



bool Coordinate::toMix(Vector<double>& worldOut,
                       Vector<double>& pixelOut,
                       const Vector<double>& worldIn,
                       const Vector<double>& pixelIn,
                       const Vector<bool>& worldAxes,
                       const Vector<bool>& pixelAxes,
                       const Vector<double>&,
                       const Vector<double>&) const
//
// Default implementation ok for non-coupled coordinated like
// Linear.  Coupled coordinates like DirectionCoordinate
// need their own implementation
//
{
    static Vector<double> pixel_tmp;
    static Vector<double> world_tmp;

   const uint32_t nWorld = worldAxes.nelements();
   const uint32_t nPixel = pixelAxes.nelements();
//
   DebugAssert(nWorld == nWorldAxes(), AipsError);
   DebugAssert(worldIn.nelements()==nWorld, AipsError);
   DebugAssert(nPixel == nPixelAxes(), AipsError);
   DebugAssert(pixelIn.nelements()==nPixel, AipsError);
//
   for (uint32_t i=0; i<nPixel; i++) {
      if (pixelAxes(i) && worldAxes(i)) {
         set_error("Coordinate::toMix - duplicate pixel/world axes");
         return false;
      }
      if (!pixelAxes(i) && !worldAxes(i)) {
         set_error("Coordinate::toMix - each axis must be either pixel or world");
         return false;
      }
   }
//
// Resize happens first time or maybe after an assignment
//
   if (world_tmp.nelements()!=nWorld) world_tmp.resize(nWorld);
   if (pixel_tmp.nelements()!=nPixel) pixel_tmp.resize(nPixel);
//
// Convert world to pixel.  Use  reference value unless
// world value given. Copy output pixels to output vector
// and overwrite with any input pixel values that were given
//
   world_tmp = referenceValue();
   for (uint32_t i=0; i<nWorld; i++) {
      if (worldAxes(i)) world_tmp(i) = worldIn(i);
   }
   if (!toPixel(pixel_tmp,world_tmp)) return false;
//
   if (pixelOut.nelements()!=nPixel) pixelOut.resize(nPixel);
   pixelOut = pixel_tmp;
   for (uint32_t i=0; i<nPixel; i++) {
      if (pixelAxes(i)) pixelOut(i) = pixelIn(i);
   }
//
// Convert pixel to world.  Use reference pixel unless
// pixel value given. Copy output worlds to output vector
// and overwrite with any input world values that were given
//
   pixel_tmp = referencePixel();
   for (uint32_t i=0; i<nPixel; i++) {
      if (pixelAxes(i)) pixel_tmp(i) = pixelIn(i);
   }
   if (!toWorld(world_tmp,pixel_tmp)) return false;
   if (worldOut.nelements()!=nWorld) worldOut.resize(nWorld);
   worldOut = world_tmp;
   for (uint32_t i=0; i<nWorld; i++) {
      if (worldAxes(i)) worldOut(i) = worldIn(i);
   }
//
   return true;
}



// Does everything except set the units vector, which must be done in the derived class.
bool Coordinate::setWorldAxisUnits(const Vector<String> &units)
{
    if (units.nelements() != nWorldAxes()) {
	set_error("Wrong number of elements in units vector");
	return false;
    } else {
	// If the units are unchanged just return true.
	Vector<String> old = worldAxisUnits();
	if (allEQ(old, units)) {
	    return true;
	}
    }

    bool ok = true;

    String error;
    Vector<double> factor;
    ok = find_scale_factor(error, factor, units, worldAxisUnits());
    if (ok) {
      ok = setIncrement(increment() * factor);
      if (ok) {
         ok = setReferenceValue(referenceValue() * factor);
      }
    } else {
      set_error(error);
    }

    return ok;
}

void Coordinate::checkFormat(Coordinate::formatType& format,
                             const bool ) const
{
// Scientific or fixed formats only are allowed.
// Absolute or offset is irrelevant

   if (format != Coordinate::SCIENTIFIC &&
       format != Coordinate::FIXED) format = Coordinate::DEFAULT;
//
   if (format == Coordinate::DEFAULT)  format = Coordinate::MIXED;
}



void Coordinate::getPrecision(int32_t &precision,
                              Coordinate::formatType& format,
                              bool absolute,
                              int32_t defPrecScientific,
                              int32_t defPrecFixed,
                              int32_t ) const
{

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

      //   } else if (format == Coordinate::MIXED) {
      // Auto format by STL formatter so precision not relevant

   } else {
// RI 20091213 but we should still set it so its not later accessed uninitalized:
// (also make this branch catch Coordinate::DEFAULT
     precision = 6;
   }
}


String Coordinate::format(
	String& units, Coordinate::formatType format,
	double worldValue, uint32_t worldAxis,
	bool isAbsolute, bool showAsAbsolute,
	int32_t precision, bool usePrecForMixed
) const
//
// isAbsolute
//    T means the worldValue is given as absolute
//    F means the worldValue is given as relative
//
// showAsAbsolute
//    T means the worldValue should be formatted as absolute
//    F means the worldValue should be formatted as relative
//
{
   DebugAssert(worldAxis < nWorldAxes(), AipsError);

// Check format

   Coordinate::formatType form = format;
   checkFormat (form, showAsAbsolute);

// Set default precision

   int32_t prec = precision;
   if (prec < 0) getPrecision(prec, form, showAsAbsolute, -1, -1, -1);

// Convert given world value to absolute or relative as needed

   static Vector<double> world;
   if (world.nelements()!=nWorldAxes()) world.resize(nWorldAxes());
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

// If units are empty, use native unit.
// Convert to specified unit if possible

   String nativeUnit = worldAxisUnits()(worldAxis);
   if (units.empty()) units = nativeUnit;

// Now check validity of unit

   Unit nativeUnitU(nativeUnit);
   Unit currentUnitU(units);
//
   if (currentUnitU != nativeUnitU) {
      throw(AipsError("Requested units are invalid for this Coordinate"));
   } else {
      static Quantum<double> q;
      q.setValue(worldValue);
      q.setUnit(nativeUnitU);
      worldValue = q.getValue(currentUnitU);
   }

   ostringstream oss;
   bool precision_set = false;

// ensure that there is enough precision... may need more tweaking...
   Vector<double> inc(increment());
   if ( inc.nelements( ) > 0 && ((worldValue - trunc(worldValue)) != 0) ) {
      static Quantum<double> qdelta;
      qdelta.setValue(inc(0));
      qdelta.setUnit(nativeUnitU);
      double worldIncr = qdelta.getValue(currentUnitU);
      int needed_precision = 1;
      for ( double compare = 1.0; fabs(worldValue) > compare; compare *= 10 ) { ++needed_precision; }
      if ( fabs(worldIncr) < 1.0 )
          for ( double compare = 0.1; fabs(worldIncr) < compare; compare /= 10 ) { ++needed_precision; }
      else {
          int adjust = 1;
          for ( double compare = 1.0; fabs(worldIncr) > compare; compare *= 10 ) { ++adjust; }
          if ( adjust < needed_precision ) needed_precision -= adjust;
      }
      if ( needed_precision > 5 ) {
         oss.precision( needed_precision + 1 );
         precision_set = true;
      }
   }

// Format and get units.

   if (form == Coordinate::MIXED) {
	   if (usePrecForMixed) {
		   oss << setprecision(prec);
	   }
	   oss << worldValue;
   } else if (form == Coordinate::SCIENTIFIC) {
      oss.setf(ios::scientific, ios::floatfield);
      if ( precision_set == false ) oss.precision(prec);
      oss << worldValue;
   } else if (form == Coordinate::FIXED) {
      oss.setf(ios::fixed, ios::floatfield);
      if ( precision_set == false ) oss.precision(prec);
      oss << worldValue;
   }
//
   return String(oss);
}


String Coordinate::formatQuantity (String& units,
                                   Coordinate::formatType format2,
                                   const Quantum<double>& worldValue,
                                   uint32_t worldAxis,
                                   bool isAbsolute,
                                   bool showAsAbsolute,
                                   int32_t precision)
{
   DebugAssert(worldAxis < nWorldAxes(), AipsError);

// Use derived class formatter

   return format(units, format2,
                 worldValue.getValue(Unit(worldAxisUnits()(worldAxis))),
                 worldAxis, isAbsolute, showAsAbsolute, precision);
}


// after = factor * before
bool Coordinate::find_scale_factor(String &error, Vector<double> &factor,
				   const Vector<String> &units,
				   const Vector<String> &oldUnits)
{
    factor.resize(units.nelements());
    bool ok = (units.nelements() == oldUnits.nelements());
    if (!ok) {
	error = "units and oldUnits are different sizes!";
    } else {

// Try to find the scaling factors between the old and new units

	uint32_t n = units.nelements();
	for (uint32_t i=0; i<n && ok; i++) {
	    if (UnitVal::check(oldUnits(i)) && UnitVal::check(units(i))) {
		Unit before = oldUnits(i);
		Unit after = units(i);
		ok = (before.getValue() == after.getValue());
		if (!ok) {
		    error = "Units are not compatible dimensionally";
		} else {
		    factor(i) = before.getValue().getFac() /
			after.getValue().getFac();
		}
	    } else {
		ok = false;
		error = "Unknown unit - cannot calculate scaling";
	    }
	}
    }
    return ok;
}


String Coordinate::typeToString (Coordinate::Type type)
{
//
// I would prefer to call the virtual function
// Coordinate::showType() but then I need an object
//
   if (type==Coordinate::LINEAR) {
      return String("Linear");
   } else if (type==Coordinate::DIRECTION) {
      return String("Direction");
   } else if (type==Coordinate::SPECTRAL) {
      return String("Spectral");
   } else if (type==Coordinate::STOKES) {
      return String("Stokes");
   } else if (type==Coordinate::QUALITY) {
      return String("Quality");
   } else if (type==Coordinate::TABULAR) {
      return String("Tabular");
   } else if (type==Coordinate::COORDSYS) {
      return String("System");
   } else {
      return String("Unknown - function Coordinate::typeToString needs development");
   }
   return String("");
}

void Coordinate::set_error(const String &errorMsg) const
{
    error_p = errorMsg;
}



Coordinate* Coordinate::makeFourierCoordinate (const Vector<bool>&,
                                               const Vector<int32_t>&)  const
{
   String tmp = String("Coordinates of type ") + showType() +
                String(" cannot be Fourier Transformed");
   throw AipsError(tmp);
}


void Coordinate::fourierUnits (String& nameOut, String& unitOut, String& unitInCanon,
                               Coordinate::Type type, int32_t axis,
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
   } else if (type==Coordinate::QUALITY) {
      throw (AipsError("Cannot provide Fourier coordinate name for Quality coordinate"));
   } else if (type==Coordinate::COORDSYS) {
      throw (AipsError("Cannot provide Fourier coordinate name for CoordinateSystem coordinate"));
   } else {
      nameOut = String("Inverse(") + nameIn + String(")");
      unitOut = String("1/") + unitIn;
      unitInCanon = unitIn;
   }
}


void Coordinate::makeWorldAbsoluteMany (Matrix<double>& value) const
{
   makeWorldAbsRelMany (value, true);
}

void Coordinate::makeWorldRelativeMany (Matrix<double>& value) const
{
   makeWorldAbsRelMany (value, false);
}

void Coordinate::makePixelAbsoluteMany (Matrix<double>& value) const
{
   makePixelAbsRelMany (value, true);
}

void Coordinate::makePixelRelativeMany (Matrix<double>& value) const
{
   makePixelAbsRelMany (value, false);
}


void Coordinate::makeWorldAbsRelMany (Matrix<double>& value, bool toAbs) const
{
    Vector<double> col(nWorldAxes());
    Vector<double> lastInCol(nWorldAxes());
    Vector<double> lastOutCol(nWorldAxes());
    uint32_t k,l;
    bool same;
    ArrayAccessor<double, Axis<0> > i;
    ArrayAccessor<double, Axis<1> > j(value);
    for (j.reset(),l=0; j!=j.end(); j++,l++) {
       i = j;
       same = true;
       for (i.reset(),k=0; i!=i.end(); i++,k++) {
          col[k] = *i;
          if (l==0 || (l!=0 && !casacore::near(col[k],lastInCol[k]))) same = false;
       }
       lastInCol = col;
//
       if (same) {
          for (i.reset(),k=0; i!=i.end(); ++i,k++) {
             *i = lastOutCol[k];
          }
       } else {
          if (toAbs) {
             makeWorldAbsolute(col);
          } else {
             makeWorldRelative(col);
          }
//
          for (i.reset(),k=0; i!=i.end(); ++i,k++) {
             *i = col[k];
          }
          lastOutCol = col;
       }
    }
}



void Coordinate::makePixelAbsRelMany (Matrix<double>& value, bool abs) const
{
    Vector<double> col(nPixelAxes());
    Vector<double> lastInCol(nPixelAxes());
    Vector<double> lastOutCol(nPixelAxes());
    uint32_t k,l;
    bool same;
    ArrayAccessor<double, Axis<0> > i;
    ArrayAccessor<double, Axis<1> > j(value);
    for (j.reset(),l=0; j!=j.end(); j++,l++) {
       i = j;
       same = true;
       for (i.reset(),k=0; i!=i.end(); i++,k++) {
          col[k] = *i;
          if (l==0 || (l!=0 && !casacore::near(col[k],lastInCol[k]))) same = false;
       }
       lastInCol = col;
//
       if (same) {
          for (i.reset(),k=0; i!=i.end(); ++i,k++) {
             *i = lastOutCol[k];
          }
       } else {
          if (abs) {
             makePixelAbsolute(col);
          } else {
             makePixelRelative(col);
          }
//
          for (i.reset(),k=0; i!=i.end(); ++i,k++) {
             *i = col[k];
          }
          lastOutCol = col;
       }
    }
}




void Coordinate::makeWorldAbsolute (Vector<double>& world) const
{
   DebugAssert(world.nelements()==nWorldAxes(),AipsError);
   world += referenceValue();
}


void Coordinate::makeWorldAbsoluteRef (Vector<double>& world,
                                    const Vector<double>& refVal) const
{
   DebugAssert(world.nelements()==nWorldAxes(),AipsError);
   DebugAssert(refVal.nelements()==nWorldAxes(),AipsError);
   world += refVal;
}

void Coordinate::makeWorldRelative (Vector<double>& world) const
{
   DebugAssert(world.nelements()==nWorldAxes(),AipsError);
   world -= referenceValue();
}


void Coordinate::makePixelAbsolute (Vector<double>& pixel) const
{
   DebugAssert(pixel.nelements()==nPixelAxes(),AipsError);
   pixel += referencePixel();
}

void Coordinate::makePixelRelative (Vector<double>& pixel) const
{
   DebugAssert(pixel.nelements()==nPixelAxes(),AipsError);
   pixel -= referencePixel();
}



bool Coordinate::setWorldMixRanges (const IPosition& shape)
{
   const uint32_t n = shape.nelements();
   if (n!=nPixelAxes()) {
      set_error("Shape has must be of length nPixelAxes");
      return false;
   }
   AlwaysAssert(nPixelAxes()==nWorldAxes(), AipsError);

// Use defaults if conversion fails

   setDefaultWorldMixRanges();

// Do conversions 25% off edge of image

   Vector<double> pMin(n), pMax(n);
   Vector<double> wMin, wMax;
   for (uint32_t i=0; i<n; i++) {
      double s2 = double(shape(i)) / 2.0;
//
      if (shape(i)==0) {

// shape not known (probably pixel axis in CS removed)

         pMin(i) = referencePixel()(i) - 10.0;
         pMax(i) = referencePixel()(i) + 10.0;
      } else if (shape(i) == 1) {
         pMin(i) = 0 - 10.0;
         pMax(i) = 0 + 10.0;
      } else if (shape(i) > 0) {
         double n2 = 1.5 * s2;
         pMin(i) = s2 - n2;
         pMax(i) = s2 + n2;
      }
   }
   bool ok1 = toWorld(wMin, pMin);
   bool ok2 = toWorld(wMax, pMax);
   if (ok1 && ok2) {
      for (uint32_t i=0; i<n; i++) {
         if (shape(i) > 0) {             // If shape not known use default value
            worldMin_p(i) = wMin(i);
            worldMax_p(i) = wMax(i);
         }
      }
      return true;
   } else {
      return false;
   }
//
   return true;
}

void Coordinate::setDefaultWorldMixRanges ()
{
   const uint32_t n = nWorldAxes();
   worldMin_p.resize(n);
   worldMax_p.resize(n);
   worldMin_p = -1.0e99;
   worldMax_p =  1.0e99;
}


bool Coordinate::doNearPixel (const Coordinate& other,
                              const Vector<bool>& thisAxes,
                              const Vector<bool>& otherAxes,
                              double tol) const
{
   if (type() != other.type()) {
      set_error("Coordinate types differ");
      return false;
   }
//
   if (allEQ(thisAxes, false) && allEQ(otherAxes, false)) {
      return true;
   }
//
   if (nPixelAxes() != other.nPixelAxes()) {
      set_error("Number of pixel axes differs");
      return false;
   }
   if (nWorldAxes() != other.nWorldAxes()) {
      set_error("Number of world axes differs");
      return false;
   }
//
   const Vector<double>&  thisRefVal(referenceValue());
   const Vector<double>& otherRefVal(other.referenceValue());
   const Vector<double>&  thisInc(increment());
   const Vector<double>& otherInc(other.increment());
   const Vector<double>&  thisRefPix(referencePixel());
   const Vector<double>& otherRefPix(other.referencePixel());
/*
   const Vector<String>&  thisNames(worldAxisNames());
   const Vector<String>& otherNames(other.worldAxisNames());
*/
   const Vector<String>&  thisUnits(worldAxisUnits());
   const Vector<String>& otherUnits(other.worldAxisUnits());
//
   const Matrix<double>&  thisPC(linearTransform());
   const Matrix<double>& otherPC(other.linearTransform());
   if (thisPC.nrow() != otherPC.nrow()) {
      set_error ("PC matrices have different numbers of rows");
      return false;
   }
   if (thisPC.ncolumn() != otherPC.ncolumn()) {
      set_error ("PC matrices have different numbers of columns");
      return false;
   }
//
   for (uint32_t i=0; i<nPixelAxes(); i++) {
      if (thisAxes(i) && otherAxes(i)) {

// Units

         String x1 = thisUnits(i);
         x1.upcase();
         String x2 = otherUnits(i);
         x2.upcase();
//
         int32_t i1 = x1.index(RXwhite,0);
         if (i1==-1) i1 = x1.length();
         int32_t i2 = x2.index(RXwhite,0);
         if (i2==-1) i2 = x2.length();
//
         String y1 = String(x1.before(i1));
         String y2 = String(x2.before(i2));
         ostringstream oss;
         if (y1 != y2) {
           oss << "The Coordinates have differing axis units for axis "
               << i;
           set_error(String(oss));
           return false;
         }

// Ref val

         if (!casacore::near(thisRefVal(i), otherRefVal(i), tol)) {
            oss << "The Coordinates have differing reference values for axis "
                 << i;
            set_error(String(oss));
            return false;
         }

// Increment

         if (!casacore::near(thisInc(i), otherInc(i), tol)) {
            oss << "The Coordinates have differing increments for axis "
                 << i;
            set_error(String(oss));
            return false;
         }

// Ref pix

         if (!casacore::near(thisRefPix(i), otherRefPix(i), tol)) {
            oss << "The Coordinates have differing reference pixels for axis "
                 << i;
            set_error(String(oss));
            return false;
         }

// pc matrix. Compare row by row.  An axis will turn up in the PC
// matrix in any row or column with that number. E.g.,
// values pertaining to axis "i" will be found in all
// entries of row "i" and all entries of column "i"
// So just get the ith row and ith column and compare
// PC is always SQUARE

         AlwaysAssert(thisPC.nrow()==thisPC.ncolumn(), AipsError);
//
         Vector<double> r1 = thisPC.row(i);
         Vector<double> r2 = otherPC.row(i);
         for (uint32_t j=0; j<r1.nelements(); j++) {
            if (!casacore::near(r1(j),r2(j),tol)) return false;
         }
//
         Vector<double> c1 = thisPC.column(i);
         Vector<double> c2 = otherPC.column(i);
         for (uint32_t j=0; j<r1.nelements(); j++) {
            if (!casacore::near(c1(j),c2(j),tol)) return false;
         }
      }
   }

//
   return true;
}


Coordinate* Coordinate::rotate(const Quantity& angle) const {
	if (nPixelAxes() != 2) {
		throw AipsError(
			"Coordinate::rotate: This coordinate does not have exactly two pixel axes. Rotation is not possible."
		);
	}
	Matrix<double> xf = linearTransform();

	// Generate rotation matrix components
	double angleRad = angle.getValue(Unit("rad"));
	Matrix<double> rotm(2, 2);
	double s = sin(-angleRad);
	double c = cos(-angleRad);
	rotm(0, 0) = c;
	rotm(0, 1) = s;
	rotm(1, 0) = -s;
	rotm(1, 1) = c;

	// Create new linear transform matrix
	Matrix<double> xform(2, 2);
	xform(0, 0) = rotm(0, 0) * xf(0, 0) + rotm(0, 1) * xf(1, 0);
	xform(0, 1) = rotm(0, 0) * xf(0, 1) + rotm(0, 1) * xf(1, 1);
	xform(1, 0) = rotm(1, 0) * xf(0, 0) + rotm(1, 1) * xf(1, 0);
	xform(1, 1) = rotm(1, 0) * xf(0, 1) + rotm(1, 1) * xf(1, 1);

	// Apply new linear transform matrix
	Coordinate* result = clone();
	result->setLinearTransform(xform);
	return result;
}

bool Coordinate::toWorldWCS (Vector<double>& world, const Vector<double>& pixel,
                             ::wcsprm& wcs) const
{
    const uint32_t nAxes = nPixelAxes();
    world.resize(nAxes);
//
    DebugAssert(pixel.nelements() == nAxes, AipsError);

// Generate pointers and intermediaries for wcs

    bool delPixel, delWorld;
    const double* pixelStore = pixel.getStorage(delPixel);
    double* worldStore = world.getStorage(delWorld);
//
    double phi;
    double theta=0;   // initialize, because wcslib not always sets theta
// Convert from pixel to world with wcs units

    int stat;
    int iret;
    constexpr uint32_t NAXES_THRESHOLD = 10;
    // avoid dynamic memory allocation for modest number of coordinate axes
    // note that output stored in imgCrd is not used
    if (nAxes <= NAXES_THRESHOLD) {
      thread_local static double imgCrd[NAXES_THRESHOLD];
      iret = wcsp2s (&wcs, 1, nAxes, pixelStore, imgCrd,
		         &phi, &theta, worldStore, &stat);
    } else {
      Block<double> imgCrd(nAxes);
      iret = wcsp2s (&wcs, 1, nAxes, pixelStore, imgCrd.storage(),
		         &phi, &theta, worldStore, &stat);
    }
    pixel.freeStorage(pixelStore, delPixel);
    world.putStorage(worldStore, delWorld);
//
    if (iret!=0) {
       String errorMsg = String("wcslib wcsp2s error: ") + wcsp2s_errmsg[iret];
       set_error(errorMsg);
       return false;
    }
//
    return true;
}



bool Coordinate::toPixelWCS(Vector<double> &pixel, const Vector<double> &world,
                            ::wcsprm& wcs) const
{
   pixel.resize(world.nelements());
   const uint32_t nAxes = nWorldAxes();
   DebugAssert(world.nelements() == nAxes, AipsError);

// Generate pointers and intermediaries for wcs

   bool delPixel, delWorld;
   double* pixelStore = pixel.getStorage(delPixel);
   const double* worldStore = world.getStorage(delWorld);
//
   double phi;
   double theta;

// Convert with world with wcs units to pixel

   int stat;
   int iret;
   constexpr uint32_t NAXES_THRESHOLD = 10;
   // avoid dynamic memory allocation for modest number of coordinate axes
   // note that output stored in imgCrd is not used
   if (nAxes <= NAXES_THRESHOLD) {
     thread_local static double imgCrd[NAXES_THRESHOLD];
     iret = wcss2p (&wcs, 1, nAxes, worldStore, &phi, &theta,
		        imgCrd, pixelStore, &stat);
   } else {
     Block<double> imgCrd(nAxes);
     iret = wcss2p (&wcs, 1, nAxes, worldStore, &phi, &theta,
		        imgCrd.storage(), pixelStore, &stat);
   }
   pixel.putStorage(pixelStore, delPixel);
   world.freeStorage(worldStore, delWorld);
//
   if (iret!=0) {
      String errorMsg = String("wcslib wcss2p error: ") + wcss2p_errmsg[iret];
      set_error(errorMsg);
      return false;
   }
//
   return true;
}



bool Coordinate::toWorldManyWCS (Matrix<double>& world, const Matrix<double>& pixel,
                                 Vector<bool>& failures, ::wcsprm& wcs) const
{
    uint32_t nTransforms = pixel.ncolumn();
    uint32_t nAxes = nPixelAxes();
    AlwaysAssert(pixel.nrow()==nAxes, AipsError);
    world.resize(pixel.shape());
    failures.resize(nTransforms);

// Generate pointers and intermediaries for wcs

    bool deleteWorld, deletePixel;
    double* pWorld = world.getStorage(deleteWorld);
    const double* pPixel = pixel.getStorage(deletePixel);
//
    bool deleteImgCrd, deletePhi, deleteTheta, deleteStat;
    Matrix<double> imgCrd(nAxes,nTransforms);
    Vector<double> phi(nTransforms);
    Vector<double> theta(nTransforms);
    Vector<int32_t> stat(nTransforms);

// Convert from pixel to world with wcs units

    double* pImgCrd = imgCrd.getStorage(deleteImgCrd);
    double* pPhi = phi.getStorage(deletePhi);
    double* pTheta = theta.getStorage(deleteTheta);
    int32_t* pStat = stat.getStorage(deleteStat);
//
    int iret = wcsp2s (&wcs, nTransforms, nAxes, pPixel, pImgCrd, pPhi, pTheta, pWorld, pStat);
    for (uint32_t i=0; i<nTransforms; i++) {
       failures[i] = pStat[i]!=0;
    }
//
    pixel.freeStorage(pPixel, deletePixel);
    world.putStorage(pWorld, deleteWorld);
    imgCrd.putStorage(pImgCrd, deleteImgCrd);
    phi.putStorage(pPhi, deletePhi);
    theta.putStorage(pTheta, deleteTheta);
    stat.putStorage(pStat, deleteStat);
//
    if (iret!=0) {
        String errorMsg= "wcs wcsp2s_error: ";
        errorMsg += wcsp2s_errmsg[iret];
        set_error(errorMsg);
        return false;
    }
//
    return true;
}


bool Coordinate::toPixelManyWCS (Matrix<double>& pixel, const Matrix<double>& world,
                                 Vector<bool>& failures, ::wcsprm& wcs) const
{
    uint32_t nTransforms = world.ncolumn();
    uint32_t nAxes = nWorldAxes();
    AlwaysAssert(world.nrow()==nAxes, AipsError);
    pixel.resize(world.shape());
    failures.resize(nTransforms);

// Generate wcs pointers and intermediaries

    bool deleteWorld, deletePixel;
    double* pPixel = pixel.getStorage(deletePixel);
    const double* pWorld = world.getStorage(deleteWorld);
//
    bool deleteImgCrd, deletePhi, deleteTheta, deleteStat;
    Matrix<double> imgCrd(nAxes,nTransforms);
    Vector<double> phi(nTransforms);
    Vector<double> theta(nTransforms);
    Vector<int32_t> stat(nTransforms);
//
    double* pImgCrd = imgCrd.getStorage(deleteImgCrd);
    double* pPhi = phi.getStorage(deletePhi);
    double* pTheta = theta.getStorage(deleteTheta);
    int32_t* pStat = stat.getStorage(deleteStat);

// Convert from wcs units to pixel

    const int nC = nTransforms;
    int iret = wcss2p (&wcs, nC, nAxes, pWorld, pPhi, pTheta, pImgCrd, pPixel, pStat);
    for (uint32_t i=0; i<nTransforms; i++) {
       failures[i] = pStat[i]!=0;
    }
//
    world.freeStorage(pWorld, deleteWorld);
    pixel.putStorage(pPixel, deletePixel);
    imgCrd.putStorage(pImgCrd, deleteImgCrd);
    phi.putStorage(pPhi, deletePhi);
    theta.putStorage(pTheta, deleteTheta);
    stat.putStorage(pStat, deleteStat);
//
    if (iret!=0) {
        String errorMsg= "wcs wcss2p_error: ";
        errorMsg += wcss2p_errmsg[iret];
        set_error(errorMsg);
        return false;
    }
    return true;
}


void Coordinate::toCurrentMany(Matrix<double>& world, const Vector<double>& toCurrentFactors) const
{
    for (uint32_t i=0; i<toCurrentFactors.nelements(); i++) {
       Vector<double> row(world.row(i));                // Reference
       row *= toCurrentFactors[i];
    }
}


void Coordinate::fromCurrentMany(Matrix<double>& world, const Vector<double>& toCurrentFactors) const
{
    for (uint32_t i=0; i<toCurrentFactors.nelements(); i++) {
       Vector<double> row(world.row(i));                // Reference
       row /= toCurrentFactors[i];
    }
}


void Coordinate::convertToMany (Matrix<double>& world) const
{
    AlwaysAssert(nWorldAxes()==world.nrow(), AipsError);
    Vector<double> worldTmp(nWorldAxes());
    ArrayAccessor<double, Axis<1> > jWorld(world);
    ArrayAccessor<double, Axis<0> > iWorld;
//
    uint32_t k;
    for (jWorld.reset(); jWorld!=jWorld.end(); ++jWorld) {
       iWorld = jWorld;            // Partial assignment
       for (iWorld.reset(),k=0; iWorld!=iWorld.end(); ++iWorld,k++) {
          worldTmp[k] = *iWorld;
       }

// Convert this coordinate pair

       convertTo(worldTmp);

// Fill back into Matrix

       iWorld = jWorld;            // Partial assigment
       for (iWorld.reset(),k=0; iWorld!=iWorld.end(); ++iWorld,k++) {
           *iWorld = worldTmp[k];
       }
    }
}



void Coordinate::convertFromMany (Matrix<double>& world) const
{
    AlwaysAssert(nWorldAxes()==world.nrow(), AipsError);
    Vector<double> worldTmp(nWorldAxes());
    ArrayAccessor<double, Axis<1> > jWorld(world);
    ArrayAccessor<double, Axis<0> > iWorld;
//
    uint32_t k;
    for (jWorld.reset(); jWorld!=jWorld.end(); ++jWorld) {
       iWorld = jWorld;            // Partial assignment
       for (iWorld.reset(),k=0; iWorld!=iWorld.end(); ++iWorld,k++) {
          worldTmp[k] = *iWorld;
       }

// Convert this coordinate pair

       convertFrom(worldTmp);

// Fill back into Matrix

       iWorld = jWorld;            // Partial assigment
       for (iWorld.reset(),k=0; iWorld!=iWorld.end(); ++iWorld,k++) {
           *iWorld = worldTmp[k];
       }
    }
}

void Coordinate::pcToXform (Matrix<double>& xform, const ::wcsprm& wcs) const
{
   uint32_t n = wcs.naxis;
   xform.resize(n,n);
//
   uint32_t count = 0;
   for (uint32_t i=0; i<n; i++) {
     for (uint32_t j=0; j<n; j++) {
        xform(j,i) = wcs.pc[count];
        count++;
     }
   }
}

void Coordinate::xFormToPC (::wcsprm& wcs, const Matrix<double>& xform) const
{
   uint32_t n = wcs.naxis;
   AlwaysAssert(xform.nrow()==n && xform.ncolumn()==n, AipsError);
//
   uint32_t count = 0;
   for (uint32_t i=0; i<n; i++) {
     for (uint32_t j=0; j<n; j++) {
        wcs.pc[count] = xform(j,i);
        count++;
     }
   }

// Now, having hacked away at the PC cards, we must set a bit in
// wcs telling it that they are active.  This is because of the
// ungodly mess of allowing PC*CDELT and CD cards.  If we don't
// set this bit, wcsset (when called) will reset the wcsprm.pc cards

   wcs.altlin |= 1;
}


void Coordinate::set_wcs (::wcsprm& wcs)
{
    // wcsset calls wcsunitse, which in turn calls wcsulexe, which is thread-unsafe.
    // (in wcslib 5.17, the latest version at the time of writing, but probably
    // in all previous versions as well). Thus, this call needs to be protected
    static std::mutex wcsset_mutex;
    std::lock_guard<std::mutex> lock(wcsset_mutex);
    if (int iret = wcsset(&wcs)) {
        String errmsg = "wcs wcsset_error: ";
        errmsg += wcsset_errmsg[iret];
        throw(AipsError(errmsg));
    }
}

//#if (WCSLIB_VERSION_MAJOR == 5 && WCSLIB_VERSION_MINOR >= 7) || WCSLIB_VERSION_MAJOR > 5
// In wcslib >= 5.7 wcssub internally calls wcssnpv/wcssnps to temporarily
// set two global variables to a particular value before calling wcsini, which
// in turn reads their values; after wcsini returns wcssub sets the variables to
// their previous values. This situation creates a race condition between
// concurrent, independent calls to wcssub and wcsini. Thus, we serialize them
// with this lock
static std::mutex wcs_initsubcopy_mutex;
//#endif // WCSLIB_VERSION >= 5.7

void Coordinate::init_wcs(::wcsprm& wcs, int naxis)
{
//#if (WCSLIB_VERSION_MAJOR == 5 && WCSLIB_VERSION_MINOR >= 7) || WCSLIB_VERSION_MAJOR > 5
    std::lock_guard<std::mutex> lock(wcs_initsubcopy_mutex);
//#endif // WCSLIB_VERSION >= 5.7
    if (int iret = wcsini(1, naxis, &wcs)) {
        String errmsg = "wcs wcsini_error: ";
        errmsg += wcsini_errmsg[iret];
        throw(AipsError(errmsg));
    }
}

void Coordinate::sub_wcs(const ::wcsprm &src, int &nsub, int axes[], ::wcsprm &dst)
{
	// see init_wcs
//#if (WCSLIB_VERSION_MAJOR == 5 && WCSLIB_VERSION_MINOR >= 7) || WCSLIB_VERSION_MAJOR > 5
    std::lock_guard<std::mutex> lock(wcs_initsubcopy_mutex);
//#endif // WCSLIB_VERSION >= 5.7
	if (int iret = wcssub(1, &src, &nsub, axes, &dst)) {
		String errmsg = "wcslib wcssub error: ";
		errmsg += wcsini_errmsg[iret];
		throw(AipsError(errmsg));
	}
}

void Coordinate::copy_wcs(const ::wcsprm &src, ::wcsprm &dst)
{
	// see init_wcs
//#if (WCSLIB_VERSION_MAJOR == 5 && WCSLIB_VERSION_MINOR >= 7) || WCSLIB_VERSION_MAJOR > 5
	std::lock_guard<std::mutex> lock(wcs_initsubcopy_mutex);
//#endif // WCSLIB_VERSION >= 5.7
	if (int iret = wcssub(1, &src, 0, 0, &dst)) {
		String errmsg = "wcslib wcscopy error: ";
		errmsg += wcsini_errmsg[iret];
		throw(AipsError(errmsg));
	}
}

} //# NAMESPACE CASACORE - END

