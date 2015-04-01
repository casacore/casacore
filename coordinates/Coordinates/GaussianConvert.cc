//# GaussianConvert.cc: Class to convert between pixel and world coordinates for Gaussians
//# Copyright (C) 1997,1998,1999,2000,2001
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
//# $Id$
//#

#include <casacore/coordinates/Coordinates/GaussianConvert.h>
 
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/coordinates/Coordinates/CoordinateSystem.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/Utilities/Assert.h>
 
#include <casacore/casa/iostream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

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
   if (!itsCSys.setWorldAxisUnits(units)) {
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
   if (!itsCSys.setWorldAxisUnits(units)) {
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


Bool GaussianConvert::toWorld(Vector<Quantum<Double> >& world,
                              const Vector<Double>& pixel)
{
   if (!itsValid) {
      itsErrorMessage = "the converter state is invalid; use setCoordinateSystem and/or setWorldAxes";
      return False;
   }
//
   if (pixel.nelements() != 2) {
      itsErrorMessage = "the pixel vector must have 2 elements";
      return False;
   }
//
   Vector<Double> pixel2(itsCSys.referencePixel().copy());
   Int pixelAxis0 = itsCSys.worldAxisToPixelAxis(itsWorldAxes(0));
   if (pixelAxis0==-1) {
      itsErrorMessage = "the first world axis has no corresponding pixel axis";
      return False;
   }
   Int pixelAxis1 = itsCSys.worldAxisToPixelAxis(itsWorldAxes(1));
   if (pixelAxis1==-1) {
      itsErrorMessage = "the second world axis has no corresponding pixel axis";
      return False;
   }
   pixel2(pixelAxis0) = pixel(0);
   pixel2(pixelAxis1) = pixel(1);
//
   Vector<Double> world2;
   if (!itsCSys.toWorld(world2, pixel2)) {
      itsErrorMessage = "failed to convert to world because" + itsCSys.errorMessage();
      return False;
   }
//
// Assign
//
   Vector<Quantum<Double> > world3(2);
   {
      Quantum<Double> tmp(world2(itsWorldAxes(0)), 
                          itsCSys.worldAxisUnits()(itsWorldAxes(0)));
      String unit;
      if (world.nelements() >= 1) unit = world(0).getUnit();
      if (!unit.empty()) tmp.convert(Unit(unit));
      world3(0) = tmp;
   }
   {
      Quantum<Double> tmp(world2(itsWorldAxes(1)), 
                          itsCSys.worldAxisUnits()(itsWorldAxes(1)));
      String unit;
      if (world.nelements() >= 2) unit = world(1).getUnit();
      if (!unit.empty()) tmp.convert(Unit(unit));
      world3(1) = tmp;
   }
   world.resize(2);
   world(0) = world3(0);
   world(1) = world3(1);
//
   return True;
}


Bool GaussianConvert::toPixel(Vector<Double>& pixel,
                              const Vector<Quantum<Double> >& world)
{
   if (!itsValid) {
      itsErrorMessage = "the converter state is invalid; use setCoordinateSystem and/or setWorldAxes";
      return False;
   }
//
   if (world.nelements() != 2) {
      itsErrorMessage = "the world vector must have 2 elements";
      return False;
   }
//
   Vector<Double> world2(itsCSys.referenceValue().copy());
   Vector<String> units(itsCSys.worldAxisUnits());
//
   {
      Quantum<Double> tmp = world(0);
      tmp.convert(Unit(units(itsWorldAxes(0))));
      world2(itsWorldAxes(0)) = tmp.getValue();
   }
   {
      Quantum<Double> tmp = world(1);
      tmp.convert(Unit(units(itsWorldAxes(1))));
      world2(itsWorldAxes(1)) = tmp.getValue();
   }
//
   if (!itsCSys.toPixel(pixel, world2)) {
      itsErrorMessage = "failed to convert to pixel because" + itsCSys.errorMessage();
      return False;
   }
//
   return True;
}


// Private functions

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
   Double pa3 = positionAngleRange(pa2);
//
   positionAngleOut.setValue(pa3);
   positionAngleOut.setUnit(Unit("rad"));
   positionAngleOut.convert(Unit(unitPA));
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

} //# NAMESPACE CASACORE - END

