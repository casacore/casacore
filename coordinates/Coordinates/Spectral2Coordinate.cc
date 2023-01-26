//# Spectral2Coordinate.cc: this defines Measures related SpectralCoordinate functions
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


#include <casacore/coordinates/Coordinates/SpectralCoordinate.h>

#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayAccessor.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/coordinates/Coordinates/CoordinateUtil.h>
#include <casacore/fits/FITS/FITSSpectralUtil.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/measures/Measures/VelocityMachine.h>
#include <casacore/measures/Measures/MeasConvert.h>
#include <casacore/measures/Measures/MFrequency.h>
#include <casacore/measures/Measures/MDoppler.h>
#include <casacore/casa/Quanta/MVFrequency.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/casa/Quanta/Unit.h>
#include <casacore/casa/BasicSL/String.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN


bool SpectralCoordinate::toWorld(MFrequency& world, 
				 double pixel) const
{
    static MVFrequency world_tmp;
    if (toWorld(world_tmp, pixel)) {
       world.set(world_tmp, MFrequency::Ref(type_p));
       return true;
    }
    return false;
}

bool SpectralCoordinate::toWorld(MVFrequency& world, 
				 double pixel) const
{
    double world_tmp;
    static Quantum<double> q_tmp;
//
    if (toWorld(world_tmp, pixel)) {
       q_tmp.setValue(world_tmp);
       q_tmp.setUnit(Unit(worldAxisUnits()(0)));
       world = MVFrequency(q_tmp);
       return true;
    }
    return false;
}

bool SpectralCoordinate::toPixel(double& pixel,
                                 const MFrequency& world) const
{
    return toPixel(pixel, world.getValue());
}
 
bool SpectralCoordinate::toPixel(double& pixel,
                                 const MVFrequency& world) const
{
   double world_tmp;
   world_tmp = world.get(Unit(worldAxisUnits()(0))).getValue();
   return toPixel(pixel, world_tmp);
}
 
bool SpectralCoordinate::pixelToVelocity (Quantum<double>& velocity, double pixel) const
{
   double world;
   if (!toWorld(world, pixel)) return false;
   return frequencyToVelocity (velocity, world);
}

bool SpectralCoordinate::pixelToVelocity (double& velocity, double pixel) const
{
   double world;
   if (!toWorld(world, pixel)) return false;
   velocity = pVelocityMachine_p->makeVelocity(world).getValue();
//
   if(isNaN(velocity)){
     set_error("velocity is NaN");
     return false;
   }
   else{
     return true;
   }
}

bool SpectralCoordinate::pixelToVelocity (Vector<double>& velocity, const Vector<double>& pixel) const
{
   velocity.resize(pixel.nelements());

// Perhaps its faster to make a vector of world and do them 
// all in one go in the machine ?  Haven't tested it.

   double world;
   for (uint32_t i=0; i<pixel.nelements(); i++) {
      if (!toWorld(world, pixel(i))) return false;
      velocity(i) = pVelocityMachine_p->makeVelocity(world).getValue();
   }
//
   if(isNaN(velocity(0))){
     set_error("velocity is NaN");
     return false;
   }
   else{
     return true;
   }
}

bool SpectralCoordinate::frequencyToVelocity (Quantum<double>& velocity, double frequency) const
{
   velocity = pVelocityMachine_p->makeVelocity(frequency);
   MVFrequency mvf(frequency);
//
   if(isNaN(velocity.getValue())){
     set_error("velocity is NaN");
     return false;
   }
   else{
     return true;
   }
}


bool SpectralCoordinate::frequencyToVelocity (double& velocity, double frequency) const
{
   static Quantum<double> t;
   t = pVelocityMachine_p->makeVelocity(frequency);
   velocity = t.getValue();
   if(isNaN(velocity)){
     set_error("velocity is NaN");
     return false;
   }
   else{
     return true;
   }
}

bool SpectralCoordinate::frequencyToVelocity (Vector<double>& velocity, const Vector<double>& frequency) const

{
   velocity.resize(frequency.nelements());
   velocity = pVelocityMachine_p->makeVelocity(frequency).getValue();
//
   if(isNaN(velocity(0))){
     set_error("velocity is NaN");
     return false;
   }
   else{
     return true;
   }
}

bool SpectralCoordinate::frequencyToVelocity (Quantum<double>& velocity, const MFrequency& frequency) const
{
   return frequencyToVelocity(velocity, frequency.getValue());
}

bool SpectralCoordinate::frequencyToVelocity (Quantum<double>& velocity, const MVFrequency& frequency) const
{
   velocity = pVelocityMachine_p->operator()(frequency);
   if(isNaN(velocity.getValue())){
     set_error("velocity is NaN");
     return false;
   }
   else {
     return true;
   }
}

bool SpectralCoordinate::frequencyToWavelength (Vector<double>& wavelength, const Vector<double>& frequency) const
{
   wavelength.resize(frequency.nelements());

   // wave = C::c/freq * 1/to_hz_p * 1/to_m_p
   double factor = C::c/to_hz_p/to_m_p;
   bool rval=true;
   for(uint32_t i=0; i<frequency.nelements(); i++){
     if(frequency(i)>0.){
       wavelength(i) = factor/frequency(i);
     }
     else{
       wavelength(i) = HUGE_VAL;
       set_error("input frequency is <= 0");
       rval = false;
     }
   }
   return rval;
}
  
bool SpectralCoordinate::frequencyToAirWavelength (Vector<double>& wavelength, const Vector<double>& frequency) const
{
   wavelength.resize(frequency.nelements());

   // airwave = C::c/freq * 1/to_hz_p * 1/to_m_p/refractive_index
   double factor = C::c/to_hz_p/to_m_p;
   bool rval = true;
   for(uint32_t i=0; i<frequency.nelements(); i++){
     if(frequency(i)>0.){
       double vacWave = factor/frequency(i);
       //cout << "toWave: vacWave " << vacWave << " to_m_p " << to_m_p << endl;
       wavelength(i) = vacWave/FITSSpectralUtil::refractiveIndex(vacWave* 1E6 * to_m_p);
       //cout << "toWave air wave " << wavelength(i) << endl;
     }
     else{
       wavelength(i) = HUGE_VAL;
       set_error("input frequency is <= 0");
       rval = false;
     }
   }
   return rval;
}


bool SpectralCoordinate::airWavelengthToFrequency (Vector<double>& frequency, const Vector<double>& airWavelength) const
{
   frequency.resize(airWavelength.nelements());

   // freq = C::c/wave * 1/to_hz_p * 1/to_m_p, wave = n(airwave)*airwave
   double factor = C::c/to_hz_p/to_m_p;
   bool rval  = true;
   for(uint32_t i=0; i<airWavelength.nelements(); i++){
  
     if(airWavelength(i)>0.){
       double lambda_um = airWavelength(i) * 1E6L * to_m_p; // in micrometers
       frequency(i) = factor/airWavelength(i)/FITSSpectralUtil::refractiveIndex(lambda_um);
       //cout << "toFreq: air wave " << airWavelength(i) << " lambda_um " << lambda_um << endl;
       //cout << "toFreq: freq " << frequency(i) << endl;
     }
     else{
       frequency(i) = HUGE_VAL;
       set_error("input frequency is <= 0");
       rval = false;
     }
   }
   return rval;
}

bool SpectralCoordinate::wavelengthToFrequency (Vector<double>& frequency, const Vector<double>& wavelength) const
{
   // since the functional form of the conversion is identical, we can reuse the inverse function
  return frequencyToWavelength(frequency,wavelength);
}


bool SpectralCoordinate::velocityToPixel (double& pixel, double velocity) const
{
   double frequency;
   if (!velocityToFrequency(frequency, velocity)) return false;
   return toPixel(pixel, frequency);
}

bool SpectralCoordinate::velocityToPixel (Vector<double>& pixel, const Vector<double>& velocity) const
{
   pixel.resize(velocity.nelements());
   double frequency, pix;
   for (uint32_t i=0; i<velocity.nelements(); i++) {
      if (!velocityToFrequency(frequency, velocity(i))) return false;
      if (!toPixel(pix, frequency)) return false;
      pixel(i) = pix;
   }
//
   return true;
}

bool SpectralCoordinate::velocityToFrequency (double& frequency, double velocity) const
{
   frequency = pVelocityMachine_p->makeFrequency (velocity).getValue();

   if(frequency<=0.){
     set_error("frequency <= 0");
     return false;
   }
   else {
     return true;
   }
}

bool SpectralCoordinate::velocityToFrequency (Vector<double>& frequency,  
                                              const Vector<double>& velocity) const
{
   frequency.resize(velocity.nelements());
   for (uint32_t i=0; i<velocity.nelements(); i++) {
      frequency(i) = pVelocityMachine_p->makeFrequency (velocity(i)).getValue();
   }
//
   if(frequency(0)<=0.){
     set_error("frequency <= 0");
     return false;
   }
   else {
     return true;
   }

}

void SpectralCoordinate::makeVelocityMachine (const String& velUnit, 
                                              MDoppler::Types velType,
                                              const Unit& freqUnit, 
                                              MFrequency::Types freqType,
                                              double restFreq)
{
   Quantum<double> rF(restFreq, freqUnit);
   pVelocityMachine_p = new VelocityMachine(MFrequency::Ref(freqType), freqUnit,
                                            MVFrequency(rF), MDoppler::Ref(velType), 
                                            Unit(velUnit));
}


void SpectralCoordinate::updateVelocityMachine (const String& velUnit, 
                                               MDoppler::Types velType)
{
   if (pVelocityMachine_p->getDopplerUnits().getName() != velUnit) {
      pVelocityMachine_p->set(Unit(velUnit));
   }
   if (MDoppler::castType(pVelocityMachine_p->getDopplerReference().getType()) != velType) {
      pVelocityMachine_p->set(MDoppler::Ref(velType));
   }
}


int32_t SpectralCoordinate::makeConversionMachines (MFrequency::Types type, 
                                                MFrequency::Types conversionType,
                                                const MEpoch& epoch, 
                                                const MPosition& position,
                                                const MDirection& direction)
{
   LogIO os(LogOrigin("SpectralCoordinate", "makeConversionMachines"));

// Clean up extent machines

   deleteConversionMachines();

// If the types are the same, don't make the machines.

   if (conversionType==type_p) return 2;

// It is assumed the passed in Measures are viable

   pConversionMachineTo_p = new MFrequency::Convert();
   bool ok1 = CoordinateUtil::makeFrequencyMachine (os, *pConversionMachineTo_p,
                                                    conversionType, type,
                                                    direction, direction, 
                                                    epoch, epoch,
                                                    position, position);
//
   pConversionMachineFrom_p = new MFrequency::Convert();
   bool ok2  = CoordinateUtil::makeFrequencyMachine (os, *pConversionMachineFrom_p,
                                                     type, conversionType, 
                                                     direction, direction,
                                                     epoch, epoch,
                                                     position, position);
//
   if (!ok1 || !ok2) {

// This means the trial conversions when the machines were made failed.
// Usually this means the rest frequency or radial velocity was required

      deleteConversionMachines();
      return -1;
   } else if (pConversionMachineTo_p->isNOP() && pConversionMachineFrom_p->isNOP()) {

// If the machines are noOps, delete them

      deleteConversionMachines();
      return 3;
   } else {

// Set up units so we can just use doubles in conversions
 
      String unit = worldAxisUnits()(0);
      pConversionMachineTo_p->set(Unit(unit));
      pConversionMachineFrom_p->set(Unit(unit));
   }
//
   return 1;
}

void SpectralCoordinate::convertTo (Vector<double>& world) const
{
  if (pConversionMachineTo_p) {
    for(uint32_t i=0; i<world.size(); i++){
      world[i]  = (*pConversionMachineTo_p)(world[i]).get(unit_p).getValue();
    }
  }
}

void SpectralCoordinate::convertFrom (Vector<double>& world) const
{

  if (pConversionMachineFrom_p) {
    for(uint32_t i=0; i<world.size(); i++){
      world[i]  = (*pConversionMachineFrom_p)(world[i]).get(unit_p).getValue();
    }
  }
}

} //# NAMESPACE CASACORE - END

