


//# Spectral2Coordinate.cc: this defines Measures related SpectralCoordinate functions
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
//#
//#
//# $Id$

#include <trial/Coordinates/SpectralCoordinate.h>

#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/Vector.h>
#include <aips/Measures/VelocityMachine.h>
#include <aips/Measures/MFrequency.h>
#include <aips/Measures/MDoppler.h>
#include <aips/Quanta/MVFrequency.h>
#include <aips/Quanta/Quantum.h>
#include <aips/Quanta/Unit.h>
#include <aips/Utilities/String.h>


SpectralCoordinate::SpectralCoordinate(MFrequency::Types freqType,
                                       MDoppler::Types velType,
                                       const Vector<Double>& velocities,
                                       const String& velUnit,
                                       Double restFrequency = 0.0)
: Coordinate(),
  type_p(freqType),
  restfreq_p(restFrequency),
  pVelocityMachine_p(0),
  prefVelType_p(MDoppler::RADIO),
  prefSpecUnit_p("")
{
    
// Convert to frequency.  We can't use the built in function velocityToFrequency
// because it requires the coordinate to be fully constructed.
                                       
      Unit freqUnit("Hz");
      Quantum<Double> rF(restFrequency, freqUnit);
//
      pVelocityMachine_p = new VelocityMachine(MFrequency::Ref(freqType), freqUnit,
                                               MVFrequency(rF), MDoppler::Ref(velType), 
                                               Unit(velUnit));
      Quantum<Vector<Double> > frequencies = pVelocityMachine_p->makeFrequency(velocities);
  
// Construct
   
      Vector<Double> channels(velocities.nelements());
      indgen(channels);
      worker_p = TabularCoordinate(channels, frequencies.getValue(), "Hz", "Frequency");
}                                      
 

Bool SpectralCoordinate::toWorld(MFrequency& world, 
				 Double pixel) const
{
    static MVFrequency world_tmp;
    if (toWorld(world_tmp, pixel)) {
       world.set(world_tmp, MFrequency::Ref(type_p));
       return True;
    }
    return False;
}

Bool SpectralCoordinate::toWorld(MVFrequency& world, 
				 Double pixel) const
{
    static Double world_tmp;
    static Quantum<Double> q_tmp;
//
    if (toWorld(world_tmp, pixel)) {
       q_tmp.setValue(world_tmp);
       q_tmp.setUnit(Unit(worldAxisUnits()(0)));
       world = MVFrequency(q_tmp);
       return True;
    }
    return False;
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
   world_tmp = world.get(Unit(worldAxisUnits()(0))).getValue();
   return toPixel(pixel, world_tmp);
}
 
Bool SpectralCoordinate::pixelToVelocity (Quantum<Double>& velocity, Double pixel, 
                                          const String& velUnit, MDoppler::Types velType) 
{
   static Double world;
   if (!toWorld(world, pixel)) return False;
   return frequencyToVelocity (velocity, world, velUnit, velType);
}

Bool SpectralCoordinate::pixelToVelocity (Double& velocity, Double pixel, 
                                          const String& velUnit, MDoppler::Types velType) 
{
   makeVelocityMachine(velUnit, velType);
//
   static Double world;
   if (!toWorld(world, pixel)) return False;
   velocity = pVelocityMachine_p->makeVelocity(world).getValue();
//
   return True;
}

Bool SpectralCoordinate::pixelToVelocity (Vector<Double>& velocity, const Vector<Double>& pixel, 
                                          const String& velUnit, MDoppler::Types velType) 
{
   velocity.resize(pixel.nelements());
   makeVelocityMachine(velUnit, velType);
//
   static Double world;
   for (uInt i=0; i<pixel.nelements(); i++) {
      if (!toWorld(world, pixel(i))) return False;
      velocity(i) = pVelocityMachine_p->makeVelocity(world).getValue();
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


Bool SpectralCoordinate::frequencyToVelocity (Double& velocity, Double frequency, 
                                              const String& velUnit, MDoppler::Types velType)
{
   makeVelocityMachine(velUnit, velType);
//
   static Quantum<Double> velQ;
   velocity = pVelocityMachine_p->makeVelocity(frequency).getValue();
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
      velocity(i) = pVelocityMachine_p->makeVelocity(frequency(i)).getValue();
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

Bool SpectralCoordinate::velocityToPixel (Double& pixel, Double velocity, 
                                          const String& velUnit, MDoppler::Types velType) 
{
   makeVelocityMachine(velUnit, velType);
//
   static Double frequency;
   if (!velocityToFrequency(frequency, velocity)) return False;
   return toPixel(pixel, frequency);
}

Bool SpectralCoordinate::velocityToPixel (Vector<Double>& pixel, const Vector<Double>& velocity, 
                                          const String& velUnit, MDoppler::Types velType) 
{
   pixel.resize(velocity.nelements());
   makeVelocityMachine(velUnit, velType);
//
   static Double frequency, pix;
   for (uInt i=0; i<velocity.nelements(); i++) {
      if (!velocityToFrequency(frequency, velocity(i))) return False;
      if (!toPixel(pix, frequency)) return False;
      pixel(i) = pix;
   }
   return True;
}

Bool SpectralCoordinate::velocityToFrequency (Double& frequency, Double velocity, 
                                              const String& velUnit, MDoppler::Types velType)
{
   makeVelocityMachine(velUnit, velType);
//
   frequency = pVelocityMachine_p->makeFrequency (velocity).getValue();
   return True;
}

Bool SpectralCoordinate::velocityToFrequency (Vector<Double>& frequency, const Vector<Double>& velocity, 
                                              const String& velUnit, MDoppler::Types velType)
{
   frequency.resize(velocity.nelements());
   makeVelocityMachine(velUnit, velType);
//
   for (uInt i=0; i<velocity.nelements(); i++) {
      frequency(i) = pVelocityMachine_p->makeFrequency (velocity(i)).getValue();
   }
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
