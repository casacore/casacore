//# GaussianConvert.cc: Class to convert between pixel and world coordinates for Gaussians
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
   if (itsWorldAxes.nelements() != 2) {
      throw(AipsError("GaussianConvert - worldAxes must be of length 2"));
   }
//
   Unit u0(itsCSys.worldAxisUnits()(itsWorldAxes(0)));
   Unit u1(itsCSys.worldAxisUnits()(itsWorldAxes(1)));
   if (u0 != u1) {
      throw(AipsError("GaussianConvert - units of specified axes must be dimensioanlly consistent"));
   }
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
   if (itsWorldAxes.nelements()==2) itsValid = True;  
}

void GaussianConvert::setWorldAxes (const Vector<uInt>& worldAxes)
{
   itsWorldAxes.resize(0);
   itsWorldAxes = worldAxes;
   if (itsCSys.nCoordinates()!=0) itsValid = True;  
}

Bool GaussianConvert::toWorld(Quantum<Double>& majorAxisOut, Quantum<Double>& minorAxisOut,
                              Quantum<Double>& positionAngleOut, Double majorAxisIn,
                              Double minorAxisIn, const Quantum<Double>& positionAngleIn)
{
   if (!itsValid) {
      itsErrorMessage = "the converter state is invalid; use setCoordinateSystem and/or setWorldAxes";
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

// Find scale factors
 
   Vector<Double> deltas(2);
   deltas(0) = 1.0 / itsCSys.increment()(itsWorldAxes(0));
   deltas(1) = 1.0 / itsCSys.increment()(itsWorldAxes(1));
//  
   Double sinpa = sin(positionAngleIn.getValue("rad"));
   Double cospa = cos(positionAngleIn.getValue("rad"));
//
   Double tmp = majorAxisIn * sqrt(square(cospa/deltas(1)) + square(sinpa/deltas(0)));
   majorAxisOut.setValue(tmp);
   majorAxisOut.setUnit(Unit(unitAxes));
//
   tmp = minorAxisIn * sqrt(square(cospa/deltas(0)) + square(sinpa/deltas(1)));
   minorAxisOut.setValue(tmp);
   minorAxisOut.setUnit(Unit(unitAxes));
//
   convertPositionAngle(positionAngleOut, positionAngleIn, deltas);
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

// Convert axes to same units
 
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

// Find scale factors
 
   Vector<Double> deltas(2);
   deltas(0) = itsCSys.increment()(itsWorldAxes(0));
   deltas(1) = itsCSys.increment()(itsWorldAxes(1));
//  
   Double sinpa = sin(positionAngleIn.getValue("rad"));
   Double cospa = cos(positionAngleIn.getValue("rad"));
//
   majorAxisOut = majorAxisIn.getValue() * sqrt(square(cospa/deltas(1)) + square(sinpa/deltas(0)));
   minorAxisOut = minorAxisIn.getValue() * sqrt(square(cospa/deltas(0)) + square(sinpa/deltas(1)));
//
   convertPositionAngle(positionAngleOut, positionAngleIn, deltas);
//
   return True;
}


void GaussianConvert::convertPositionAngle(Quantum<Double>& paOut, 
                                           const Quantum<Double>& paIn,
                                           const Vector<Double>& deltas)
{

//  FInd output units

   String unitPA = paOut.getUnit();
   if (unitPA.length()==0) unitPA = paIn.getUnit();


// Put position angle into the range 0 -> pi (same as that
// returned by Gaussian2D functional)

   Double pa = paIn.getValue(Unit("rad"));
   Double pa2 = remainder(pa, C::pi);
   if (pa2 >  0.5*C::pi) pa2 -= C::pi;
   if (pa2 < -0.5*C::pi) pa2 += C::pi;
   if (abs(pa2) <= 0.25*C::pi) {
      pa2 = atan(deltas(1)/deltas(0) * tan(pa2));
   } else {
      pa2 = 0.5*C::pi - atan(deltas(0)/deltas(1) * tan(0.5*C::pi - pa2));
      if (pa2 >= 0.5*C::pi) pa2 -= C::pi;
   }
   pa2 += (C::pi)/2;
//
   paOut.setValue(pa2);
   paOut.setUnit(Unit("rad"));
   paOut.convert(Unit(unitPA));
}


