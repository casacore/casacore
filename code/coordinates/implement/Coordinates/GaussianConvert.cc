//# GaussianConvert.cc: Class to convert between pixel and world coordinates for Gaussians
//# Copyright (C) 1997,1998,1999
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

#include <trial/Coordinates/GaussianConvert.h>
 
#include <aips/Arrays/Vector.h>
#include <trial/Coordinates/CoordinateSystem.h>
#include <aips/Exceptions/Error.h>
#include <aips/Mathematics/Math.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Utilities/Assert.h>
 
#include <iostream.h>

GaussianConvert::GaussianConvert()
: itsValid(False)
{
}


GaussianConvert::GaussianConvert(const CoordinateSystem& cSys, 
                                 const Vector<uInt>& worldAxes)
: itsCSys(cSys),
  itsWorldAxes(worldAxes.copy()),
  itsErrorMessage(""),
  itsValid(True)
{
   checkWorldAxes();
   checkCoordinateSystem();
}

GaussianConvert::~GaussianConvert()
{
}

GaussianConvert::GaussianConvert(const GaussianConvert& other)
: itsCSys(other.itsCSys), 
  itsWorldAxes(other.itsWorldAxes.copy()),
  itsErrorMessage(other.itsErrorMessage),
  itsValid(other.itsValid)
{
}

GaussianConvert& GaussianConvert::operator=(const GaussianConvert& other)
{
   if (this != &other) {
      itsCSys = other.itsCSys;
      itsWorldAxes.resize(0);
      itsWorldAxes = other.itsWorldAxes;
      itsErrorMessage = other.itsErrorMessage;
      itsValid = other.itsValid;
   }
   return *this;
}


void GaussianConvert::setCoordinateSystem (const CoordinateSystem& cSys)
{
   itsCSys = cSys;
   checkCoordinateSystem();
//
   if (itsWorldAxes.nelements()==2) itsValid = True;  
}

void GaussianConvert::setWorldAxes (const Vector<uInt>& worldAxes)
{
   itsWorldAxes.resize(0);
   itsWorldAxes = worldAxes;
   checkWorldAxes();
//
   if (itsCSys.nCoordinates()!=0) itsValid = True;  
}

Bool GaussianConvert::toWorld(Quantum<Double>& majorAxisOut, Quantum<Double>& minorAxisOut,
                              Quantum<Double>& positionAngleOut, Double majorAxisIn,
                              Double minorAxisIn, const Quantum<Double>& positionAngleIn)
{
   if (!itsValid) {
      itsErrorMessage = String("the converter state is invalid; ") +
                        String("use setCoordinateSystem and/or setWorldAxes");
      return False;
   }

// Check output quantum units. If none, use that of CoordinateSystem

   String unitMajor = majorAxisOut.getUnit();
   String unitMinor = minorAxisOut.getUnit();
//
   String unitAxes;
   if (unitMajor.length()==0 && unitMinor.length()==0) {
      unitAxes = itsCSys.worldAxisUnits()(itsWorldAxes(0));
   } else {
      if (unitMajor != unitMinor) {
         itsErrorMessage = "major and minor axes units differ";
         return False;
      }
      unitAxes = unitMajor;
   }      

// Set the units of the CoordinateSystem to be the same for the two
// axes.  We checked in the constructor that they were dimensionally equivalent, 
// so this is OK

   Vector<String> units(itsCSys.worldAxisUnits().copy());
   units(itsWorldAxes(0)) = unitAxes;
   units(itsWorldAxes(1)) = unitAxes;
   if (!itsCSys.setWorldAxisUnits(units, True)) {
      itsErrorMessage = "failed to set axis units because" + itsCSys.errorMessage();
      return False;
   }

// Convert 
   
   Double minOut, majOut;
   convertAxes (minOut, majOut, positionAngleOut, 
                minorAxisIn, majorAxisIn,  positionAngleIn, 
                itsCSys, String("toWorld"));
//
   minorAxisOut.setValue(minOut);
   minorAxisOut.setUnit(Unit(unitAxes));
   majorAxisOut.setValue(majOut);
   majorAxisOut.setUnit(Unit(unitAxes));
//
   return True;
}



Bool GaussianConvert::toPixel(Double& majorAxisOut, Double& minorAxisOut, 
                              Quantum<Double>& positionAngleOut, const Quantum<Double>& majorAxisIn,
                              const Quantum<Double>& minorAxisIn, const Quantum<Double>& positionAngleIn)
{
   if (!itsValid) {
      itsErrorMessage = "the converter state is invalid; use setCoordinateSystem and/or setWorldAxes";
      return False;
   }

// Convert axes to same unit
 
   Quantum<Double> majIn(majorAxisIn);
   Quantum<Double> minIn(minorAxisIn);
   majIn.convert(Unit(minIn.getUnit()));
   String unitAxes = majIn.getUnit();

//
// Set the units of the CoordinateSystem to be the same for the two
// axes.  We checked in the constructor that they were dimensionally equivalent, 
// so this is OK

   Vector<String> units(itsCSys.worldAxisUnits().copy());
   units(itsWorldAxes(0)) = unitAxes;
   units(itsWorldAxes(1)) = unitAxes;
   if (!itsCSys.setWorldAxisUnits(units, True)) {
      itsErrorMessage = "failed to set axis units because" + itsCSys.errorMessage();
      return False;
   }

// Convert 

   convertAxes (minorAxisOut, majorAxisOut, positionAngleOut, 
                minIn.getValue(), majIn.getValue(), positionAngleIn, 
                itsCSys, String("toPixel"));
//
   return True;
}


void GaussianConvert::convertAxes (Double& minorAxisOut, 
                                   Double& majorAxisOut,
                                   Quantum<Double>& positionAngleOut, 
                                   Double minorAxisIn, Double majorAxisIn, 
                                   const Quantum<Double>& positionAngleIn, 
                                   const CoordinateSystem& cSys, 
                                   String dir)
{
//
// The defined convention for the Gaussian2D functional, with which I should probably
// be consistent, is positive from +y to -x (ccw).   The normal astronomical convention 
// for DirectionCoordinates positive +y to +x (N through E).  This means
// I must flip the sign for DirectionCoordinates on the x axis.
//

//
// World axes already checked to exist in CS
//
   Int coordinate0, coordinate1, axisInCoordinate0, axisInCoordinate1;
   cSys.findWorldAxis(coordinate0, axisInCoordinate0, itsWorldAxes(0));
   cSys.findWorldAxis(coordinate1, axisInCoordinate1, itsWorldAxes(1));
   Bool flipX = False;
   Bool flipY = False;
   if (coordinate0==coordinate1 &&
       cSys.type(coordinate0)==Coordinate::DIRECTION) {
      if (axisInCoordinate0==0) flipX = True;     // Long is worldAxes(0)
      if (axisInCoordinate1==0) flipY = True;     // Long is worldAxes(1)
   }
//
   Double dx = cSys.increment()(itsWorldAxes(0));
   if (flipX) dx *= -1;
   Double dy  = cSys.increment()(itsWorldAxes(1));
   if (flipY) dy *= -1;
//
   Double sinpa = sin(positionAngleIn.getValue("rad"));
   Double cospa = cos(positionAngleIn.getValue("rad"));
//
   Double alpha = square(cospa/minorAxisIn) + square(sinpa/majorAxisIn);
   Double beta  = square(sinpa/minorAxisIn) + square(cospa/majorAxisIn);
   Double gamma = (2/square(minorAxisIn) - 2/square(majorAxisIn))*cospa*sinpa;
//
   if (dir=="toWorld") {
      alpha /= dx*dx;
      beta  /= dy*dy;
      gamma /= dx*dy;
   } else {
      alpha *= dx*dx;
      beta  *= dy*dy;
      gamma *= dx*dy;
   }
//
   Double s = alpha + beta;
   Double t = sqrt(square(alpha-beta)+square(gamma));
//
   minorAxisOut = sqrt(2.0/(s+t));
   majorAxisOut = sqrt(2.0/(s-t));
//
   String unitPA = positionAngleOut.getUnit();
   if (unitPA.length()==0) unitPA = positionAngleIn.getUnit();
//
// Put position angle into the range 0 -> pi (same as that
// returned by Gaussian2D functional)
//
   Double pa2;
   if (abs(gamma)+abs(alpha-beta)==0.0) {
       pa2 = 0;
   } else {
//
// -pi -> pi
//
       pa2 = 0.5*atan2(gamma,alpha-beta);
   }
   Double pa3 = GaussianConvert::positionAngleRange(pa2);
//
   positionAngleOut.setValue(pa3);
   positionAngleOut.setUnit(Unit("rad"));
   positionAngleOut.convert(Unit(unitPA));
}


Quantum<Double> GaussianConvert::positionAngleRange(const Quantum<Double>& pa)
{
   Double pa2 = pa.getValue(Unit("rad"));
   Double pa3 = GaussianConvert::positionAngleRange(pa2);
//
   Quantum<Double> pa4(pa3, Unit("rad"));
   pa4.convert(pa.getFullUnit());
   return pa4;
}


Double GaussianConvert::positionAngleRange(Double pa)
//
// Put in the range 0->pi 
//
{
   Double pa2 = fmod(pa, C::pi);
   if (pa2 < 0.0) pa2 += C::pi;
   return pa2;
}


void GaussianConvert::checkWorldAxes()
{
   if (itsWorldAxes.nelements() != 2) {
      throw(AipsError("GaussianConvert - worldAxes must be of length 2"));
   }
   if (itsWorldAxes(0) >= itsCSys.nWorldAxes()) {
      throw(AipsError("worldAxes(0) is invalid"));
   }
   if (itsWorldAxes(1) >= itsCSys.nWorldAxes()) {
      throw(AipsError("worldAxes(1) is invalid"));
   }
   if (itsWorldAxes(0) == itsWorldAxes(1)) {
      throw(AipsError("worldAxes must be different"));
   }
//
   Unit u0(itsCSys.worldAxisUnits()(itsWorldAxes(0)));
   Unit u1(itsCSys.worldAxisUnits()(itsWorldAxes(1)));
   if (u0 != u1) {
      String msg("GaussianConvert::checkWorldAxes - units of specified axes must be dimensionally consistent");
      throw(AipsError(msg));
   }
}

void GaussianConvert::checkCoordinateSystem()
{
   if (itsCSys.nWorldAxes() < 2) {
      String msg("GaussianConvert::checkCoordinateSystem - the coordinate system must have at least 2 world axes");
      throw(AipsError(msg));
   }
}

