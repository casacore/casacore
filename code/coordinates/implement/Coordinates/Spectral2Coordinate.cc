//# Spectral2Coordinate.cc: this defines Measures related SpectralCoordinate functions
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

#include <trial/Coordinates/SpectralCoordinate.h>
#include <aips/Arrays/Vector.h>
#include <aips/Measures/VelocityMachine.h>
#include <aips/Measures/MFrequency.h>
#include <aips/Quanta/MVFrequency.h>
#include <aips/Quanta/Quantum.h>
#include <aips/Utilities/String.h>


Bool SpectralCoordinate::toWorld(MFrequency& world, 
				 Double pixel) const
{
    static MVFrequency world_tmp;
    Bool ok = toWorld(world_tmp, pixel);
    if (ok) {
       world.set(world_tmp, MFrequency::Ref(type_p));
    }
    return ok;
}

Bool SpectralCoordinate::toWorld(MVFrequency& world, 
				 Double pixel) const
{
    static Double world_tmp;
    static Quantum<Double> q_tmp;
//
    Bool ok = toWorld(world_tmp, pixel);
    if (ok) {
       const Unit& units = Unit(worldAxisUnits()(0));
       q_tmp.setValue(world_tmp);
       q_tmp.setUnit(units);
       world = MVFrequency(q_tmp);
    }
    return ok;
}

Bool SpectralCoordinate::toPixel(Double& pixel,
                                 const MFrequency& world) const
{
    return toPixel(pixel, world.getValue());
}
 
Bool SpectralCoordinate::toPixel(Double& pixel,
                                 const MVFrequency& world) const
{
   static Double world_tmp;
   const Unit& units = Unit(worldAxisUnits()(0));
   world_tmp = world.get(units).getValue();
   return toPixel(pixel, world_tmp);
}
 
Bool SpectralCoordinate::pixelToVelocity (Quantum<Double>& velocity, Double pixel, 
                                          const String& velUnit, MDoppler::Types velType) 
{
   static Double world;
   if (!toWorld(world, pixel)) return False;
   return frequencyToVelocity (velocity, world, velUnit, velType);
}

Bool SpectralCoordinate::pixelToVelocity (Vector<Double>& velocity, const Vector<Double>& pixel, 
                                          const String& velUnit, MDoppler::Types velType) 
{
   velocity.resize(pixel.nelements());
   makeVelocityMachine(velUnit, velType);
//
   static Double world;
   static Quantum<Double> velQ;
   for (uInt i=0; i<pixel.nelements(); i++) {
      if (!toWorld(world, pixel(i))) return False;
      velQ = pVelocityMachine_p->makeVelocity(world);
      velocity(i) = velQ.getValue();
   }
   return True;
}

Bool SpectralCoordinate::frequencyToVelocity (Quantum<Double>& velocity, Double frequency, 
                                              const String& velUnit, MDoppler::Types velType) 
{
   makeVelocityMachine(velUnit, velType);
   velocity = pVelocityMachine_p->makeVelocity(frequency);
   MVFrequency mvf(frequency);
//
   return True;
}


Bool SpectralCoordinate::frequencyToVelocity (Vector<Double>& velocity, const Vector<Double>& frequency, 
                                              const String& velUnit, MDoppler::Types velType)
{
   velocity.resize(frequency.nelements());
   makeVelocityMachine(velUnit, velType);
//
   static Quantum<Double> velQ;
   for (uInt i=0; i<frequency.nelements(); i++) {
      velQ = pVelocityMachine_p->makeVelocity(frequency(i));
      velocity(i) = velQ.getValue();
   }
   return True;
}

Bool SpectralCoordinate::frequencyToVelocity (Quantum<Double>& velocity, const MFrequency& frequency, 
                                              const String& velUnit, MDoppler::Types velType) 
{
   return frequencyToVelocity(velocity, frequency.getValue(), velUnit, velType);
}

Bool SpectralCoordinate::frequencyToVelocity (Quantum<Double>& velocity, const MVFrequency& frequency, 
                                              const String& velUnit, MDoppler::Types velType)
{
   makeVelocityMachine(velUnit, velType);
   velocity = pVelocityMachine_p->operator()(frequency);
   return True;
}

void SpectralCoordinate::makeVelocityMachine (const String& velUnit, MDoppler::Types velType)
{
   if (pVelocityMachine_p==0) {
      Unit freqUnit(worldAxisUnits()(0));
      Double rF0 = restFrequency();
      Quantum<Double> rF1(rF0, freqUnit);
//
      pVelocityMachine_p = 
         new VelocityMachine(MFrequency::Ref(frequencySystem()), freqUnit,
                             MVFrequency(rF1), MDoppler::Ref(velType), Unit(velUnit));
   } else {
      pVelocityMachine_p->set(Unit(velUnit));
      pVelocityMachine_p->set(MDoppler::Ref(velType));
   }
}

