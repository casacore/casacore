//# SpectralCoordinate.cc: this defines SpectralCoordinate
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
//#
//#
//# $Id$



#include <coordinates/Coordinates/SpectralCoordinate.h>
#include <coordinates/Coordinates/TabularCoordinate.h>
#include <coordinates/Coordinates/LinearXform.h>
#include <coordinates/Coordinates/LinearCoordinate.h>

#include <casa/Arrays/Vector.h>
#include <casa/Arrays/Matrix.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/ArrayUtil.h>
#include <casa/Utilities/Assert.h>
#include <casa/Containers/Record.h>
#include <fits/FITS/FITSSpectralUtil.h>
#include <fits/FITS/FITSKeywordUtil.h>
#include <scimath/Functionals/ScalarSampledFunctional.h>
#include <casa/BasicMath/Math.h>
#include <measures/Measures/MFrequency.h>
#include <measures/Measures/MeasureHolder.h>
#include <measures/Measures/VelocityMachine.h>
#include <casa/Quanta/Quantum.h>
#include <casa/Containers/RecordInterface.h>
#include <casa/Logging/LogIO.h>
#include <casa/Logging/LogOrigin.h>

#include <casa/sstream.h>
#include <casa/iostream.h>

namespace casa { //# NAMESPACE CASA - BEGIN


SpectralCoordinate::SpectralCoordinate()
: Coordinate(),
  pTabular_p(0),
  type_p(MFrequency::TOPO), 
  conversionType_p(type_p),
  restfreqs_p(0),
  restfreqIdx_p(0),
  pConversionMachineTo_p(0),
  pConversionMachineFrom_p(0),
  pVelocityMachine_p(0),
  velType_p(MDoppler::RADIO),
  velUnit_p("km/s"),
  unit_p(Unit("Hz")),
  axisName_p("Frequency"),
  formatUnit_p("")
{
   restfreqs_p.resize(1);
   restfreqs_p(0) = 0.0;
   makeVelocityMachine (velUnit_p, velType_p, unit_p,
                        type_p, restfreqs_p(restfreqIdx_p));
//
   makeWCS(wcs_p, String("FREQ"), 0.0, 0.0, 1.0, 1.0, restfreqs_p(0));
   to_hz_p = 1.0;
//
   setDefaultWorldMixRanges();
}


SpectralCoordinate::SpectralCoordinate(MFrequency::Types type,
				       Double refVal, Double inc, 
                                       Double refPix, Double restFrequency)
: Coordinate(),
  pTabular_p(0),
  type_p(type), 
  conversionType_p(type_p),
  restfreqs_p(0),
  restfreqIdx_p(0),
  pConversionMachineTo_p(0),
  pConversionMachineFrom_p(0),
  pVelocityMachine_p(0),
  velType_p(MDoppler::RADIO),
  velUnit_p("km/s"),
  unit_p(Unit("Hz")),
  axisName_p("Frequency"),
  formatUnit_p("")
{
  ///   AlwaysAssert(restFrequency>=0.0, AipsError);
   restfreqs_p.resize(1);
   restfreqs_p(0) = max(0.0, restFrequency);
//
   makeVelocityMachine (velUnit_p, velType_p, unit_p,
                        type_p, restfreqs_p(restfreqIdx_p));
//
   makeWCS(wcs_p, String("FREQ"), refPix, refVal, inc, 1.0, restfreqs_p(0));
   to_hz_p = 1.0;
//
   setDefaultWorldMixRanges();
}





SpectralCoordinate::SpectralCoordinate(MFrequency::Types type, 
                                       const Quantum<Double>& refVal,
                                       const Quantum<Double>& inc,
                                       Double refPix, 
                                       const Quantum<Double>& restFrequency)
: Coordinate(),
  pTabular_p(0),
  type_p(type),
  conversionType_p(type_p),
  restfreqs_p(0),
  restfreqIdx_p(0),
  pConversionMachineTo_p(0),
  pConversionMachineFrom_p(0),
  pVelocityMachine_p(0),
  velType_p(MDoppler::RADIO),
  velUnit_p("km/s"),
  unit_p(Unit("Hz")),
  axisName_p("Frequency"),
  formatUnit_p("")
{
   Unit hz("Hz");
   if (!refVal.isConform(hz)) {
      throw(AipsError("Unit of reference frequency is not consistent with Hz"));
   }
   if (!inc.isConform(hz)) {
      throw(AipsError("Unit of frequency increment is not consistent with Hz"));
   }
   if (!restFrequency.isConform(hz)) {
      throw(AipsError("Unit of rest frequency is not consistent with Hz"));
   }
//
   AlwaysAssert(restFrequency.getValue(hz)>=0.0, AipsError);
   restfreqs_p.resize(1);
   restfreqs_p(0) = max(0.0, restFrequency.getValue(hz));
//
   makeVelocityMachine (velUnit_p, velType_p, unit_p,
                        type_p, restfreqs_p(restfreqIdx_p));
//
   makeWCS(wcs_p, String("FREQ"), refPix, refVal.getValue(hz), inc.getValue(hz), 
           1.0, restfreqs_p(0));
   to_hz_p = 1.0;
//
   setDefaultWorldMixRanges();
}



SpectralCoordinate::SpectralCoordinate(MFrequency::Types type, 
                                       const Vector<Double> &freqs,
                                       Double restFrequency)
: Coordinate(),
  pTabular_p(0),
  type_p(type), 
  conversionType_p(type_p),
  restfreqs_p(0),
  restfreqIdx_p(0),
  pConversionMachineTo_p(0),
  pConversionMachineFrom_p(0),
  pVelocityMachine_p(0),
  velType_p(MDoppler::RADIO),
  velUnit_p("km/s"),
  unit_p(Unit("Hz")),
  axisName_p("Frequency"),
  formatUnit_p("")
{
   AlwaysAssert(restFrequency>=0.0, AipsError);
   restfreqs_p.resize(1);
   restfreqs_p(0) = max(0.0, restFrequency);
//
   Vector<Double> channels(freqs.nelements());
   indgen(channels);
   pTabular_p = new TabularCoordinate(channels, freqs, "Hz", "Frequency");
   to_hz_p = 1.0;
//
   makeVelocityMachine (velUnit_p, velType_p, unit_p,
                        type_p, restfreqs_p(restfreqIdx_p));
//
   wcs_p.flag = -1;                // Uninitialized
//
   setDefaultWorldMixRanges();
}


SpectralCoordinate::SpectralCoordinate(MFrequency::Types type, 
                                       const Quantum<Vector<Double> >& freqs,
                                       const Quantum<Double>& restFrequency)
: Coordinate(),
  pTabular_p(0),
  type_p(type),
  conversionType_p(type_p),
  restfreqs_p(0),
  restfreqIdx_p(0),
  pConversionMachineTo_p(0),
  pConversionMachineFrom_p(0),
  pVelocityMachine_p(0),
  velType_p(MDoppler::RADIO),
  velUnit_p("km/s"),
  unit_p(Unit("Hz")),
  axisName_p("Frequency"),
  formatUnit_p("")
{
   Unit hz("Hz");
   if (!freqs.isConform(hz)) {
      throw(AipsError("Unit of frequencies is not consistent with Hz"));
   }
   if (!restFrequency.isConform(hz)) {
      throw(AipsError("Unit of rest frequency is not consistent with Hz"));
   }
//
   AlwaysAssert(restFrequency.getValue(hz)>=0.0, AipsError);
   restfreqs_p.resize(1);
   restfreqs_p(0) = max(0.0, restFrequency.getValue(hz));
//
   Vector<Double> freqs2 = freqs.getValue(hz);
   Vector<Double> channels(freqs2.nelements());
   indgen(channels);
   pTabular_p = new TabularCoordinate(channels, freqs2, "Hz", "Frequency");
   to_hz_p = 1.0;
//
   makeVelocityMachine (velUnit_p, velType_p, unit_p,
                        type_p, restfreqs_p(restfreqIdx_p));
//
   wcs_p.flag = -1;                // Uninitialized
//
   setDefaultWorldMixRanges();
}


SpectralCoordinate::SpectralCoordinate(MFrequency::Types freqType,
                                       MDoppler::Types velType,
                                       const Vector<Double>& velocities,
                                       const String& velUnit,
                                       Double restFrequency )
: Coordinate(),
  pTabular_p(0),
  type_p(freqType),
  conversionType_p(type_p),
  restfreqs_p(0),
  restfreqIdx_p(0),
  pConversionMachineTo_p(0),
  pConversionMachineFrom_p(0),
  pVelocityMachine_p(0),
  velType_p(MDoppler::RADIO),
  velUnit_p("km/s"),
  unit_p("Hz"),
  axisName_p("Frequency"),
  formatUnit_p("")
{
   restfreqs_p.resize(1);
   restfreqs_p(0) = restFrequency;
   
// Convert to frequency

   makeVelocityMachine (velUnit, velType, String("Hz"), freqType, restFrequency);
   Quantum<Vector<Double> > frequencies = pVelocityMachine_p->makeFrequency(velocities);

// Make Tabular spectral coordinate

   Vector<Double> channels(velocities.nelements());
   indgen(channels);
   pTabular_p = new TabularCoordinate(channels, frequencies.getValue(), "Hz", "Frequency");
   to_hz_p = 1.0;

// Now remake Velocity Machine to be consistent with state

   delete pVelocityMachine_p;
   makeVelocityMachine (velUnit_p, velType_p, unit_p,
                        type_p, restfreqs_p(restfreqIdx_p));
//
   wcs_p.flag = -1;                // Uninitialized
//
   setDefaultWorldMixRanges();
}


SpectralCoordinate::SpectralCoordinate (MFrequency::Types type, const ::wcsprm& wcs, Bool oneRel)
: Coordinate(),
  pTabular_p(0),
  type_p(type), 
  conversionType_p(type_p),
  restfreqs_p(0),
  restfreqIdx_p(0),
  pConversionMachineTo_p(0),
  pConversionMachineFrom_p(0),
  pVelocityMachine_p(0),
  velType_p(MDoppler::RADIO),
  velUnit_p("km/s"),
  unit_p(Unit("Hz")),
  axisName_p("Frequency"),
  formatUnit_p("")
{

// Check holds only spectral wcs structure


// Copy wcs structure

   wcs_p.flag = -1;
   int err = wcscopy (1, &(wcs), &wcs_p);
   if (err != 0) {
      String errmsg = "wcs wcscopy_error: ";
      errmsg += wcscopy_errmsg[err];
      throw(AipsError(errmsg));
   }
   set_wcs(wcs_p);
   to_hz_p = 1.0;

// Make 0-relative

   if (oneRel) {
      wcs_p.crpix[0] -= 1.0;
   }

// Rest frequency

   restfreqs_p.resize(1);
   restfreqs_p(0) = max(0.0, wcs.restfrq);

// Velocity machine

   makeVelocityMachine (velUnit_p, velType_p, unit_p,
                        type_p, restfreqs_p(restfreqIdx_p));

// Set name to something from wcs structure (from ctypes) ?

//
   setDefaultWorldMixRanges();
}


SpectralCoordinate::SpectralCoordinate(const SpectralCoordinate &other)
: Coordinate(other),
  pTabular_p(0),
  pConversionMachineTo_p(0),
  pConversionMachineFrom_p(0),
  pVelocityMachine_p(0)
{
   wcs_p.flag = -1;                // Uninitialized
   copy(other);
}



SpectralCoordinate& SpectralCoordinate::operator=(const SpectralCoordinate &other)
{
    if (this != &other) {
        Coordinate::operator=(other);
        copy(other);
    }
    return *this;
}

SpectralCoordinate::~SpectralCoordinate()
{
   deleteConversionMachines();
   deleteVelocityMachine();
   if (pTabular_p) {
      delete pTabular_p;
      pTabular_p = 0;
   }
   if (wcs_p.flag != -1) {    
      wcsfree (&wcs_p);   
   }
}  


Coordinate::Type SpectralCoordinate::type() const
{
    return Coordinate::SPECTRAL;
}

String SpectralCoordinate::showType() const
{
    return String("Spectral");
}

uInt SpectralCoordinate::nPixelAxes() const
{
    return 1;
}

uInt SpectralCoordinate::nWorldAxes() const
{
    return 1;
}


Bool SpectralCoordinate::toWorld (Vector<Double> &world,
                                  const Vector<Double> &pixel) const
{

// Convert to World (Hz)

    Bool ok = True;
    if (pTabular_p) {
       ok = pTabular_p->toWorld(world, pixel);
       if (!ok) set_error(pTabular_p->errorMessage());
    } else {
       ok = toWorldWCS (world, pixel, wcs_p);
    }
    if (!ok) return False;

// Convert to correct units from Hz

    toCurrent(world);

// Convert to output reference type

    convertTo(world);    
//
    return ok;
}




Bool SpectralCoordinate::toWorld(Double& world, const Double& pixel) const
{
    static Vector<Double> pixel_tmp1(1);
    static Vector<Double> world_tmp1(1);
//
    pixel_tmp1[0] = pixel;
    if (toWorld(world_tmp1, pixel_tmp1)) {
       world = world_tmp1[0];
       return True;
    } else {
       return False;
    }
}



Bool SpectralCoordinate::toPixel (Vector<Double> &pixel,
                                  const Vector<Double> &world) const
{
    static Vector<Double> world_tmp1(1);
    DebugAssert(world.nelements()==1, AipsError);
    Bool ok = True;

// Convert from specified conversion reference type

    world_tmp1[0] = world[0];
    convertFrom(world_tmp1);

// Convert from current units to Hz 

    fromCurrent(world_tmp1);

// Convert to pixel

    if (pTabular_p) {
       ok = pTabular_p->toPixel(pixel, world_tmp1);
       if (!ok) set_error(pTabular_p->errorMessage());
    } else {
       ok = toPixelWCS (pixel, world_tmp1, wcs_p);
    }
//
    return ok;
}



Bool SpectralCoordinate::toPixel(Double& pixel, const Double& world) const
{
    static Vector<Double> pixel_tmp2(1);
    static Vector<Double> world_tmp2(1);
//
    world_tmp2[0] = world;
    if (toPixel(pixel_tmp2, world_tmp2)) {
       pixel = pixel_tmp2(0);
       return True;
    } else {
       return False;
    }
}


Bool SpectralCoordinate::toWorldMany (Matrix<Double>& world,
                                      const Matrix<Double>& pixel,
                                      Vector<Bool>& failures) const
{

// Convert to world (Hz)

   Bool ok = True;
   if (pTabular_p) {
      ok = pTabular_p->toWorldMany(world, pixel, failures);
      if (!ok) set_error(pTabular_p->errorMessage());
   } else {
      ok = toWorldManyWCS (world, pixel, failures, wcs_p);
   }
   if (!ok) return False;

// Convert to current units from wcs units

   toCurrentMany (world, toCurrentFactors());

// Convert to specified conversion reference type

   if (pConversionMachineTo_p) convertToMany(world);
//
   return True;
}


Bool SpectralCoordinate::toPixelMany (Matrix<Double>& pixel,
                                      const Matrix<Double>& world,
                                      Vector<Bool>& failures) const
{
    uInt nWorld = nWorldAxes();
    AlwaysAssert(world.nrow()==nWorld, AipsError);
    
// Copy input as we have to convert it to all sorts of things
       
    Matrix<Double> world2(world.copy());
  
// Convert from specified conversion reference type

    if (pConversionMachineTo_p) convertFromMany (world2);
    
// Convert from current units to wcs units (Hz)

    fromCurrentMany (world2, toCurrentFactors());

// Convert to pixel

    Bool ok = True;
    if (pTabular_p) {
       pTabular_p->toPixelMany(pixel, world2, failures);
       if (!ok) set_error(pTabular_p->errorMessage());
    } else {        
       ok = toPixelManyWCS (pixel, world2, failures, wcs_p);
    }
//
    return ok;
}





Vector<String> SpectralCoordinate::worldAxisNames() const
{
    Vector<String> tmp(1);
    tmp[0] = axisName_p;
    return tmp;
}

Vector<String> SpectralCoordinate::worldAxisUnits() const
{
    Vector<String> tmp(1);
    tmp(0) = unit_p.getName();
    return tmp;
}

Vector<Double> SpectralCoordinate::referencePixel() const
{
    if (pTabular_p) {
       return pTabular_p->referencePixel();
    } else {
       Vector<Double> crpix(1);
       crpix[0] = wcs_p.crpix[0];
       return crpix;
    }
}


Matrix<Double> SpectralCoordinate::linearTransform() const
{
    if (pTabular_p) {
       return pTabular_p->linearTransform();
    } else {
       Matrix<Double> tmp(1,1);
       tmp(0,0) = wcs_p.pc[0];
       return tmp;
    }
}

Vector<Double> SpectralCoordinate::increment() const
{

// Get in Hz

    Vector<Double> value(1);
    if (pTabular_p) {
       value= pTabular_p->increment(); 
    } else {
       value[0] = wcs_p.cdelt[0];
    }

// Convert to current units

    toCurrent (value);
//
    return value;
}


Vector<Double> SpectralCoordinate::referenceValue() const
{

// Get in Hz

    Vector<Double> value(1);
    if (pTabular_p) {
       value= pTabular_p->referenceValue(); 
    } else {
       value[0] = wcs_p.crval[0];
    }

// Convert to current units

    toCurrent (value);
//
    return value;
}

Bool SpectralCoordinate::setWorldAxisNames(const Vector<String>& names)
{  
    Bool ok = (names.nelements()==1);
    if (!ok) {
       set_error ("names vector must be of length 1");
    } else {
       axisName_p = names[0];
    }
    return ok;
}


Bool SpectralCoordinate::setWorldAxisUnits(const Vector<String>& units)
{  
    if (!(units.nelements()==1)) {
       set_error("units vector must be of length 1");
       return False;
    }

// Find scale factor to convert old to new

    String error;
    Vector<Double> factor;
    Bool ok = find_scale_factor(error, factor, units, worldAxisUnits());
    if (ok) {

// Set new unit 

      unit_p = Unit(units[0]);

// The increment and reference value are *always* stored in the
// wcs struct (or TabCoord) in Hz.  All we have to do is indicate
// that the conversion from current units to Hz has changed

      to_hz_p /= factor[0];

// Scale rest frequencies

      restfreqs_p *= factor[0];

// Update Velocity machines

      pVelocityMachine_p->set(unit_p);
      if (pConversionMachineTo_p && pConversionMachineTo_p) {
         pConversionMachineTo_p->set(unit_p);
         pConversionMachineFrom_p->set(unit_p);
      }
    } else {
      set_error(error);
    }
//
    return ok;
}



Bool SpectralCoordinate::setVelocity (const String& velUnit,
                                      MDoppler::Types velType)
{
   static const Unit unitsKMS_b(String("km/s"));
   if (!velUnit.empty()) {
      Unit unit(velUnit);
      if (unit!=unitsKMS_b) {
         set_error("Unit must be empty or consistent with m/s");
         return False; 
      }
      velUnit_p = velUnit;
   }
   velType_p = velType;
   updateVelocityMachine(velUnit_p, velType_p);
//
   return True;
}

Bool SpectralCoordinate::setReferenceConversion (MFrequency::Types conversionType,
                                                 const MEpoch& epoch, const MPosition& position,
                                                 const MDirection& direction)
{
// See if something to do

   if (conversionType_p==conversionType) return True;
//
   Int ok = makeConversionMachines(type_p, conversionType, epoch, position, direction);
   if (ok==-1) {

// Trial conversion failed.  The machines will be deleted so we must set the
// conversion machines back to what they were before this calamity.

      makeConversionMachines(type_p, conversionType_p, epoch_p, 
                             position_p, direction_p);
      return False;
   }
//
   conversionType_p = conversionType;
   epoch_p = epoch;
   position_p = position;
   direction_p = direction;   
//
   return True;
}


Bool SpectralCoordinate::setReferencePixel(const Vector<Double> &refPix)
{
    if (!(refPix.nelements()==nPixelAxes())) {
       set_error("reference pixels vector must be of length 1");
       return False;
    }
//
    Bool ok= True;
    if (pTabular_p) {
       ok = pTabular_p->setReferencePixel(refPix);
       if (!ok) set_error (pTabular_p->errorMessage());
    } else {

// Set WCS card

       wcs_p.crpix[0] = refPix[0];

// Tell WCS

       set_wcs(wcs_p);
    }
//
    return ok;
}

Bool SpectralCoordinate::setLinearTransform(const Matrix<Double> &xform)
{
    Bool ok = (xform.nrow()==1 && xform.ncolumn()==1);
    if (!ok) {
       set_error("linear transform matrix has wrong shape");
       return False;
    }
//
    if (pTabular_p) {
       ok = pTabular_p->setLinearTransform(xform);
       if (!ok) set_error(pTabular_p->errorMessage());
    } else {

// Set PC card

       wcs_p.pc[0] = xform(0,0);
       set_wcs(wcs_p);
    }
//
    return ok;
}


Bool SpectralCoordinate::setIncrement (const Vector<Double>& incr)
{
    if (!(incr.nelements()==nWorldAxes())) {
       set_error("increment vector must be of length 1");
       return False;
    }

// Convert to Hz

    Vector<Double> value(incr.copy());
    fromCurrent (value);

// Now set

    Bool ok= True;
    if (pTabular_p) {
       ok = pTabular_p->setIncrement(value);
       if (!ok) set_error (pTabular_p->errorMessage());
    } else {

// Set WCS card

       wcs_p.cdelt[0] = value[0];
       set_wcs(wcs_p);
    }
//
    return ok;
}



Bool SpectralCoordinate::setReferenceValue(const Vector<Double>& refval)
{
    if (!(refval.nelements()==nWorldAxes())) {
       set_error("reference value vector must be of length 1");
       return False;
    }

// Convert to Hz

    Vector<Double> value(refval.copy());
    fromCurrent (value);
//
    Bool ok= True;
    if (pTabular_p) {
       ok = pTabular_p->setReferenceValue(value);
       if (!ok) set_error (pTabular_p->errorMessage());
    } else {

// Set WCS card

       wcs_p.crval[0] = value[0];
       set_wcs(wcs_p);
    }
//
    return ok;
}


Double SpectralCoordinate::restFrequency() const
{
    return restfreqs_p(restfreqIdx_p);
}

Vector<Double> SpectralCoordinate::pixelValues() const
{
    if (pTabular_p) {
       return pTabular_p->pixelValues();
    } else {
       Vector<Double> pixels;
       return pixels;
    }
}

Vector<Double> SpectralCoordinate::worldValues() const
{
    Vector<Double> worlds;
    if (pTabular_p) {
       worlds = pTabular_p->worldValues();    // Hz
       toCurrent(worlds);
    }
//
    return worlds;
}

MFrequency::Types SpectralCoordinate::frequencySystem(Bool showConversion) const
{
    if (showConversion) {
       return conversionType_p;
    } else {
       return type_p;
    }         
}

void  SpectralCoordinate::setFrequencySystem(MFrequency::Types type)
{
    if (type==type_p) return;
//   
    MFrequency::Types oldType = type_p;
    type_p = type;
    deleteVelocityMachine();
    makeVelocityMachine (String("km/s"), velType_p, unit_p,
                         type_p, restfreqs_p(restfreqIdx_p));

// The conversion machines are no longer viable. However, it is 
// is risky to re-create the machines with the new type_p.  This 
// is because the only way to ensure epoch_p, position_p, direction_p 
// are valid (default construction values are arbitrary)  is for
// the user to have called setReferenceConversion. Now initially,
// conversionType_p = type_p.  If the user changes type_p to
// something else, and then we remake the machine, that would 
// use whatever values are in the above Measures, and that
// could still be the default values.  So better is to turn off
// the current conversion, and demand the user re-issues the
// setReferenceConversion function

   if (oldType != conversionType_p) {
      LogIO os(LogOrigin("SpectralCoordinate", "setFrequencySystem"));
      os << LogIO::WARN << "Resetting the conversion frequency system " << MFrequency::showType(conversionType_p) << endl;
      os << "to the new native frequency system " << MFrequency::showType(type_p) << endl;
      os << "You must explicitly reset the conversion frequency system if desired" << LogIO::POST;
   }
//
   deleteConversionMachines();
   conversionType_p = type_p;
}


Bool SpectralCoordinate::setRestFrequency(Double newFrequency, Bool append)
{
    newFrequency = max(0.0, newFrequency);
    if (append) {
       uInt n = restfreqs_p.nelements();
       restfreqs_p.resize(n+1, True);
       restfreqs_p(n) = newFrequency;
       restfreqIdx_p = n;
    } else {
       restfreqs_p(restfreqIdx_p) = newFrequency;
    }

// Update velocity machine with the active rest frequency

    Quantum<Double> rf(restfreqs_p(restfreqIdx_p), unit_p);
    pVelocityMachine_p->set(MVFrequency(rf));

// Update wcs struct with the active rest frequency

    wcs_p.restfrq = rf.getValue(Unit("Hz"));
//
    return True;
}

void SpectralCoordinate::setRestFrequencies(const Vector<Double>& restFrequencies,
                                            uInt which, Bool append)
{
   for (uInt i=0; i<restFrequencies.nelements(); i++) {
      AlwaysAssert(restFrequencies(i)>=0.0, AipsError);
   }
//
   if (append) {
      Vector<Double> tmp = concatenateArray (restfreqs_p, restFrequencies);
      restfreqs_p.resize(0);      
      restfreqs_p = tmp;
   } else {
      restfreqs_p.resize(0);
      restfreqs_p = restFrequencies;
   }
//
   AlwaysAssert(which<restfreqs_p.nelements(), AipsError);
   selectRestFrequency(which);
}

void SpectralCoordinate::selectRestFrequency(Double restFrequency)
{
   AlwaysAssert(restFrequency >= 0.0, AipsError);
   uInt which = 0;
   Double d, diff = 1.0e99;
   for (uInt i=0; i<restfreqs_p.nelements(); i++) {
      d = abs(restfreqs_p(i) - restFrequency);
      if (d < diff) {
         which = i;
         diff = d;
      }
   }
//
   selectRestFrequency(which);
}

void SpectralCoordinate::selectRestFrequency(uInt which)
{
   AlwaysAssert(which<restfreqs_p.nelements(), AipsError)
//
   restfreqIdx_p = which;
   Quantum<Double> rf(restfreqs_p(restfreqIdx_p), unit_p);
   pVelocityMachine_p->set(MVFrequency(rf));

// Update wcs struct with the active rest frequency

   wcs_p.restfrq = rf.getValue(Unit("Hz"));

}


Bool SpectralCoordinate::near(const Coordinate& other, Double tol) const
{
   Vector<Int> excludeAxes;
   return near(other, excludeAxes, tol);
}


Bool SpectralCoordinate::near(const Coordinate& other,
                              const Vector<Int>& excludeAxes,
                              Double tol) const
{
   if (this->type() != other.type()) {
      set_error("Comparison is not with another SpectralCoordinate");
      return False;
   }
//
   const SpectralCoordinate& sCoord = dynamic_cast<const SpectralCoordinate&>(other);

// Type

   if (type_p != sCoord.frequencySystem()) {
      set_error("The SpectralCoordinates have differing frequency systems");
      return False;
   }

// Rest freq

   if (!casa::near(restFrequency(), sCoord.restFrequency(), tol)) {
      set_error("The SpectralCoordinates have differing active rest frequencies");
      return False;
   }

// Perhaps we shouldn't check the lists of rest frequencies. 
// Does it really matter ?

   const Vector<Double>& rfs = sCoord.restFrequencies();
   if (restfreqs_p.nelements() != rfs.nelements()) {
      set_error("The SpectralCoordinates have differing numbers of rest frequencies");
      return False;
   }
//
   for (uInt i=0; i<restfreqs_p.nelements(); i++) {
      if (!casa::near(restfreqs_p(i),rfs(i),tol)) {
         set_error("The SpectralCoordinates have differing lists of rest frequencies");
         return False;
      }
   }

// Conversion type
                
   if (conversionType_p != sCoord.conversionType_p) {
            
// Should this be an error or a warning ?
       
      set_error("The SpectralCoordinates have differing conversion types");
      return False;
   }

// Number of pixel and world axes 
   
   AlwaysAssert(nPixelAxes()==nWorldAxes(), AipsError);
   Bool exclude(False);
   const uInt nExcl = excludeAxes.nelements();
   if (nExcl > 0) {
      if (excludeAxes(0)) exclude = True;
   }
      
// Check names
   
   ostringstream oss;
   if (!exclude) {
      if (axisName_p != sCoord.axisName_p) {
         set_error(String("The SpectralCoordinates have differing axis names"));
         return False;
      }
   }

// Unit

   if (unit_p != sCoord.unit_p) {
      set_error (String("The SpectralCoordinates have differing units"));
      return False;
   }


// Reference Value

   {
      const Vector<Double>& thisVal = referenceValue();   
      const Vector<Double>& thatVal = sCoord.referenceValue();
      if (!exclude) {
         if (!casa::near(thisVal[0],thatVal[0])) {
            set_error(String("The SpectralCoordinates have differing reference values"));
            return False;
         }
      }
   }

// LinearXForm components

   {
      LinearXform thisVal(referencePixel(), increment(), linearTransform());
      LinearXform thatVal(sCoord.referencePixel(), sCoord.increment(), sCoord.linearTransform());
      if (!(thisVal.near(thatVal, excludeAxes))) {
         set_error(String("The SpectralCoordinates have differing LinearXform components"));
         return False;
      }
   }

// Velocity Stuff

   if (velType_p != sCoord.velType_p) {
      set_error("The SpectralCoordinates have differing velocity types");
      return False;
   }
   if (velUnit_p != sCoord.velUnit_p) {
      set_error("The SpectralCoordinates have differing velocity units");
      return False;
   }
//
   return True;
}



Bool SpectralCoordinate::save(RecordInterface &container,
			    const String &fieldName) const
{
    Bool ok = (!container.isDefined(fieldName));
    if (ok) {
        String system = MFrequency::showType(type_p);
//
	Record subrec;
        subrec.define ("version", 2);            // Original unversioned was v 1 !
	subrec.define("system", system);
	subrec.define("restfreq", restFrequency());
        subrec.define("restfreqs", restFrequencies());
        subrec.define("velType", Int(velType_p));
        subrec.define("velUnit", velUnit_p);
        subrec.define("formatUnit", formatUnit_p);

// We may have TC (for tabular coordinates) or not.

        if (pTabular_p) {
           ok = (pTabular_p->save(subrec, "tabular"));   // Always Hz
        } else {
           ok = wcsSave (subrec, wcs_p, "wcs");          // Always Hz
        }
        if (!ok) return False;
//
        subrec.define("unit", worldAxisUnits()(0));
        subrec.define("name", axisName_p);

// Conversion machine state

        String error;
        Record subrec2;
        {
           MeasureHolder mh(direction_p);
           Record subrec3;
           mh.toRecord (error, subrec3);
           subrec2.defineRecord("direction", subrec3);
        }
        {
           MeasureHolder mh(position_p);
           Record subrec3;
           mh.toRecord (error, subrec3);
           subrec2.defineRecord("position", subrec3);
        }
        {
           MeasureHolder mh(epoch_p);
           Record subrec3;
           mh.toRecord (error, subrec3);
           subrec2.defineRecord("epoch", subrec3);
        }
        String conversionType = MFrequency::showType(conversionType_p);
        subrec2.define("system", conversionType);
        subrec.defineRecord("conversion", subrec2);
//
	container.defineRecord(fieldName, subrec);
    }
    return ok;
}

SpectralCoordinate* SpectralCoordinate::restore(const RecordInterface &container,
                                                const String &fieldName)
//
// The SpectralCOordinate changed from always holding a TabularCoordinate
// (which held either a tabular or non-tabular coordinate) to holding a 
// TC (for tabular coordinates only) or a wcs struct (non-tabular coordinates).  
// Hence the different code depending on the version of the record
//
{
    if (! container.isDefined(fieldName)) {
	return 0;
    }
    Record subrec(container.asRecord(fieldName));
//
    if (!subrec.isDefined("version")) {
      return restoreVersion1(subrec);       // Original V 1
    } else {
      Int v;
      subrec.get("version", v);
      if (v==2) {
         return restoreVersion2(subrec);       // Current  V 2
      } else {
         return 0;
      }
    }
}



SpectralCoordinate* SpectralCoordinate::restoreVersion1 (const RecordInterface& subrec)
{

//cerr << "Enter SC::restoreVersion1" << endl;
    
// We should probably do more type-checking as well as checking
// for existence of the fields.

    if (!subrec.isDefined("system")) {
	return 0;
    }
//
    String system;
    subrec.get("system", system);
    MFrequency::Types freqSys;

    if (system == "LSR") {

// LSR is perpetuated in old images but is now deprecated in Measures
// So we must still read old ones not handled by MFrequency::getType

      freqSys = MFrequency::LSRK;
    } else {
      if (!MFrequency::getType(freqSys, system)) return 0;
    }
//
    if (!subrec.isDefined("restfreq")) {
	return 0;
    }
    Double restfreq;
    subrec.get("restfreq", restfreq);

// Get TC

    if (!subrec.isDefined("tabular")) {
       return 0;
    }
    TabularCoordinate* pTabular = TabularCoordinate::restore(subrec, "tabular");
    if (pTabular==0) return 0;

// Get stuff

    String unit = pTabular->worldAxisUnits()(0);

// Create new SpectralCoordinate  (will be in Hz regarldess of unit)

    Bool ok(True);
    SpectralCoordinate* pSpectral = 0;
    Unit qUnit(unit);
    Quantum<Double> qRestFreq(restfreq, qUnit);
    const Vector<Double>& worlds = pTabular->worldValues();
    if (worlds.nelements() > 0) {
       Quantum<Vector<Double> > qWorlds(worlds, qUnit);
       pSpectral = new SpectralCoordinate (freqSys, qWorlds, qRestFreq);

// Set units first !

       ok = pSpectral->setWorldAxisUnits(pTabular->worldAxisUnits());
       ok = pSpectral->setReferencePixel(pTabular->referencePixel());
       ok = pSpectral->setReferenceValue(pTabular->referenceValue());
    } else {
       Quantum<Double> qcrval(pTabular->referenceValue()(0), qUnit);       
       Quantum<Double> qcdelt(pTabular->increment()(0), qUnit);
       Double crpix(pTabular->referencePixel()(0));
       pSpectral = new SpectralCoordinate (freqSys, qcrval, qcdelt, crpix, qRestFreq);
       ok = pSpectral->setWorldAxisUnits(pTabular->worldAxisUnits());
    }
    AlwaysAssert(pSpectral, AipsError);

// Set PC matrix.  I can't imagine anyone would really set this
// but you never know...

    pSpectral->setLinearTransform(pTabular->linearTransform());

// Set name

    pSpectral->setWorldAxisNames(pTabular->worldAxisNames());
    delete pTabular;
    pTabular = 0;
//
    String formatUnit("");
    if (subrec.isDefined("formatUnit")) {                      // optional
       formatUnit = subrec.asString("formatUnit");
    }
    pSpectral->setFormatUnit(formatUnit);

// Velocity 

    restoreVelocity(pSpectral, subrec);

// Multiple Rest Frequencies

    restoreRestFrequencies (pSpectral, subrec, restfreq);

// Conversion state

    restoreConversion (pSpectral, subrec);
//
    return pSpectral;
}




SpectralCoordinate* SpectralCoordinate::restoreVersion2 (const RecordInterface& subrec)
{

// cerr << "Enter SC::restoreVersion2" << endl;
    
// We should probably do more type-checking as well as checking
// for existence of the fields.

    if (!subrec.isDefined("system")) {
	return 0;
    }
//
    String system;
    subrec.get("system", system);
    MFrequency::Types freqSys;
//
    if (system == "LSR") {

// LSR is perpetuated in old images but is now deprecated in Measures
// So we must still read old ones not handled by MFrequency::getType

      freqSys = MFrequency::LSRK;
    } else {
      if (!MFrequency::getType(freqSys, system)) return 0;
    }
//
    if (!subrec.isDefined("restfreq")) {
	return 0;
    }
    Double restfreq;
    subrec.get("restfreq", restfreq);

// Get unit 

    String unit;
    if (!subrec.isDefined("unit")) {
       return 0;
    }
    subrec.get("unit", unit);      

// Get name

    String name;
    if (!subrec.isDefined("name")) {
       return 0;
    }
    subrec.get("name", name);      

// Create SC from TC or wcs structure 

    Unit qUnit(unit);
    Quantum<Double> qRestFreq(restfreq, qUnit);
//
    Bool ok(True);
    SpectralCoordinate* pSpectral = 0;
    if (subrec.isDefined("tabular")) {

// Reconstitute the TC (will be Hz)

       TabularCoordinate* pTabular = TabularCoordinate::restore(subrec, "tabular");
       if (pTabular == 0) return 0;

// Create SC (will be in Hz regarldess of units)

       Quantum<Vector<Double> > qWorlds(pTabular->worldValues(), 
                                        Unit(pTabular->worldAxisUnits()(0)));
       pSpectral = new SpectralCoordinate (freqSys, qWorlds, qRestFreq);
       AlwaysAssert(pSpectral, AipsError);
//
       ok = pSpectral->setReferencePixel(pTabular->referencePixel());
       ok = pSpectral->setReferenceValue(pTabular->referenceValue());   // Hz
       ok = pSpectral->setLinearTransform(pTabular->linearTransform());  
       delete pTabular;
       pTabular = 0;
    } else if (subrec.isDefined("wcs")) {
       Double crval, crpix, cdelt, pc;
       String ctype;
       if (!wcsRestore (crval, crpix, cdelt, pc, ctype, 
                        subrec.asRecord("wcs"))) return 0;

// Make SC, will be in Hz regardless of units

       Quantum<Double> qcrval(crval, qUnit);       
       Quantum<Double> qcdelt(cdelt, qUnit);
       pSpectral = new SpectralCoordinate (freqSys, qcrval, qcdelt, 
                                           crpix, qRestFreq);
       AlwaysAssert(pSpectral, AipsError);
//
       Matrix<Double> xform(1,1);
       xform = pc;
       ok = pSpectral->setLinearTransform(xform);
    } else {
       return 0;
    }

// Now set the actual units which will reset all of the (correct ?) internals

    Vector<String> tmp(1);
    tmp[0] = unit;
    pSpectral->setWorldAxisUnits(tmp);

// Name

    tmp[0] = name;
    pSpectral->setWorldAxisNames(tmp);
//
    String formatUnit("");
    if (subrec.isDefined("formatUnit")) {                      // optional
       formatUnit = subrec.asString("formatUnit");
    }
    pSpectral->setFormatUnit(formatUnit);
       
// Set CTYPE.  What to do with this I don't know yet...
// It is not actually captured in the interface anywhere.
// When finally we can make a wcsprm from a funny Spectral
// FITS card with CTYPE set have to decide what to do...


// Velocity 

    restoreVelocity(pSpectral, subrec);

// Multiple Rest Frequencies

    restoreRestFrequencies (pSpectral, subrec, restfreq);

// Conversion state

    restoreConversion (pSpectral, subrec);
//
    return pSpectral;
}



void SpectralCoordinate::restoreVelocity (SpectralCoordinate*& pSpectral,
                                          const RecordInterface& subrec)
//
// Velocity handling.
//
{

// velType was added after the initial deployment so its optional

    MDoppler::Types velType=MDoppler::RADIO;                // Must match what's defined
    String velUnit("km/s");                                 // in  Constructors
    if (subrec.isDefined("velType")) {                      // optional
       velType = static_cast<MDoppler::Types>(subrec.asInt("velType"));
    } else if (subrec.isDefined("prefVelType")) {           // name changed
       velType = static_cast<MDoppler::Types>(subrec.asInt("prefVelType"));
    }
//
    if (subrec.isDefined("velUnit")) {                 // optional
       velUnit = subrec.asString("velUnit");
    } else if (subrec.isDefined("prefVelUnit")) {      // name changed
       velUnit = subrec.asString("prefVelUnit");
    }
//
    pSpectral->setVelocity(velUnit, velType);                 // Updates Velocity Machine
}


void SpectralCoordinate::restoreRestFrequencies (SpectralCoordinate*& pSpectral,
                                                 const RecordInterface& subrec,
                                                 Double restfreq)
//
// Rest frequency handling
//
{

// Multiple rest frequencies were added after initial deployment

    if (subrec.isDefined("restfreqs")) {                   // optional
       Vector<Double> restFreqs;
       subrec.get("restfreqs", restFreqs);

// Old images might have a negative restfreq. Don't propagate that

        for (uInt i=0; i<restFreqs.nelements(); i++) {
           restFreqs(i) = max(0.0,restFreqs(i));
        }
//
       pSpectral->setRestFrequencies(restFreqs, 0, False);
       pSpectral->selectRestFrequency(restfreq);
    } else {
       pSpectral->setRestFrequency(restfreq, False);           // Updates Velocity Machine
    }
}


void SpectralCoordinate::restoreConversion (SpectralCoordinate*& pSpectral,
                                            const RecordInterface& subrec)
//
// Get Conversion state
//
{

// The conversion state was added after initial deployment

  if (subrec.isDefined("conversion")) {         
     Record subrec2 = subrec.asRecord("conversion");
//
     String tmp = subrec2.asString("system");
     MFrequency::Types conversionFreqSys;
     if (!MFrequency::getType(conversionFreqSys, tmp)) {
        conversionFreqSys = pSpectral->frequencySystem();
     }
//
     String error;
     MeasureHolder mhD;
     if (!mhD.fromRecord(error,subrec2.asRecord("direction"))) {
        delete pSpectral;
        throw(AipsError(error));         
     }
//
     MeasureHolder mhP;
     if (!mhP.fromRecord(error,subrec2.asRecord("position"))) {
        delete pSpectral;
        throw(AipsError(error));         
     }
//
     MeasureHolder mhE;
     if (!mhE.fromRecord(error,subrec2.asRecord("epoch"))) {
        delete pSpectral;
        throw(AipsError(error));         
     }

// Set the conversion state

     if (!pSpectral->setReferenceConversion (conversionFreqSys, 
                                             mhE.asMEpoch(),
                                             mhP.asMPosition(),
                                             mhD.asMDirection())) {
        delete pSpectral;
        throw (AipsError("Failed to set conversion layer state"));
     }
   }
}


Coordinate *SpectralCoordinate::clone() const
{
    return new SpectralCoordinate(*this);
}


void SpectralCoordinate::toFITS(RecordInterface &header, uInt whichAxis, 
		LogIO &logger, Bool oneRelative, 
                Bool preferVelocity,  Bool opticalVelDef) const
{
    const Double offset(1.0*Int(oneRelative == True));

    logger << LogOrigin("SpectralCoordinate", "toFITS", WHERE);

    // Verify that the required headers exist and are the right type
    AlwaysAssert(header.isDefined("ctype") && 
                 header.dataType("ctype") == TpArrayString &&
		 header.shape("ctype").nelements() == 1 &&
                 header.shape("ctype")(0) > Int(whichAxis), AipsError);
    AlwaysAssert(header.isDefined("crval") && 
                 header.dataType("crval") == TpArrayDouble &&
		 header.shape("crval").nelements() == 1 && 
                 header.shape("crval")(0) > Int(whichAxis), AipsError);
    AlwaysAssert(header.isDefined("crpix") && 
		 header.dataType("crpix") == TpArrayDouble &&
		 header.shape("crpix").nelements() == 1 &&
		 header.shape("crpix")(0) > Int(whichAxis), AipsError);
    AlwaysAssert(header.isDefined("cdelt") && 
		 header.dataType("cdelt") == TpArrayDouble &&
		 header.shape("cdelt").nelements() == 1 &&
		 header.shape("cdelt")(0) > Int(whichAxis), AipsError);

    Vector<String> ctype, cunit;
    Vector<Double> crval, cdelt, crpix;
    header.get("ctype", ctype);
    header.get("crval", crval);
    header.get("crpix", crpix);
    header.get("cdelt", cdelt);

    if (header.isDefined("cunit")) {
	AlwaysAssert(header.dataType("cunit") == TpArrayString &&
		     header.shape("cunit").nelements() == 1 &&
		     header.shape("cunit")(0) > Int(whichAxis), AipsError);
	header.get("cunit", cunit);
    }

// If we are from a table, report how non-linear we are. At some point
// we should worry about nonlinear frequency axes more (e.g. they might
// be regularly gridded in lambda or velocities).

    if (pixelValues().nelements() != 0) {
	Vector<Double> pixel = pixelValues();
	Vector<Double> world = worldValues();
	Double crpix, cdelt, crval;
	crpix = referencePixel()(0);
	cdelt = increment()(0);
	crval = referenceValue()(0);
	Double maxDeviation = 0.0;
	Vector<Double> tmpworld(1), tmppixel(1);
	for (uInt i=0; i<pixel.nelements(); i++) {
	    tmppixel(0) = pixel(i);
	    Bool ok = toWorld(tmpworld, tmppixel);
	    if (!ok) {
		logger << LogIO::SEVERE << "Error calculating deviations "
		    "from linear" << errorMessage() << LogIO::POST;
		break;
	    }
	    Double actual = tmpworld(0);
	    Double linear = crval + cdelt*(pixel(i) - crpix);
	    maxDeviation = max(abs(actual-linear), maxDeviation);
	    if (maxDeviation != 0.0) {
		logger << LogIO::SEVERE << "Error in linearizing frequency "
		    "axis for FITS is " << maxDeviation << " " <<
		    worldAxisUnits()(0) << LogIO::POST;
	    }
	}
    }

// Wacky capitalization to avoid running into other variables

    String Ctype;
    Double Crval, Cdelt, Crpix, Altrval, Altrpix;
    Int Velref;
    Bool HaveAlt;
    Double Restfreq = Quantity(restfreqs_p(restfreqIdx_p),  // Canonicalize
			       worldAxisUnits()(0)).getBaseValue();
    Double RefFreq = Quantity(referenceValue()(0), 
			      worldAxisUnits()(0)).getBaseValue();
    Double FreqInc = Quantity(increment()(0), 
			      worldAxisUnits()(0)).getBaseValue();
    MDoppler::Types VelPreference = opticalVelDef ? MDoppler::OPTICAL :
	MDoppler::RADIO;
    AlwaysAssert(FITSSpectralUtil::toFITSHeader(Ctype, Crval, Cdelt, Crpix, HaveAlt, Altrval,
						Altrpix, Velref, Restfreq, logger,
						RefFreq, referencePixel()(0) + offset,
  					        FreqInc, type_p, preferVelocity,
						VelPreference), AipsError);

    ctype(whichAxis) = Ctype;
    crval(whichAxis) = Crval;
    crpix(whichAxis) = Crpix;
    cdelt(whichAxis) = Cdelt;
    if (cunit.nelements() > 0) {
	if (Ctype.contains("VELO") || Ctype.contains("FELO")) {
	    cunit(whichAxis) = "M/S";
	} else if (Ctype.contains("FREQ")) {
	    cunit(whichAxis) = "HZ";
	} else {
	    AlwaysAssert(0, AipsError); // NOTREACHED
	}
    }

    if (Restfreq > 0) {
	header.define("restfreq", Restfreq);
	header.setComment("restfreq", "Rest Frequency (Hz)");
    }
    if (HaveAlt) {
	header.define("altrval", Altrval);
	header.setComment("altrval", "Alternate frequency reference value");
	header.define("altrpix", Altrpix);
	header.setComment("altrpix", "Alternate frequency reference pixel");
	header.define("velref", Velref);
	header.setComment("velref", "1 LSR, 2 HEL, 3 OBS, +256 Radio");
	// the following agree with the current usage in FITSSpectralUtil
	// which in turn follows from Greisen, Paper III.  On the other
	// hand, that usage as applied here, to VELREF, is unlikely to
	// be understood by other FITS readers.  Still, its better than
	// doing nothing for these rest frames until the convention in
	// Paper III or its successor is formally adopted.
	FITSKeywordUtil::addComment(header, 
          "casacore non-standard usage: 4 LSD, 5 GEO, 6 SOU, 7 GAL");
    }

    // OK, put the primary header information back
    header.define("ctype", ctype);
    header.define("crval", crval);
    header.define("crpix", crpix);
    header.define("cdelt", cdelt);
    if (cunit.nelements() > 0) {
	header.define("cunit", cunit);
    }
}

// Bool SpectralCoordinate::fromFITSOld(SpectralCoordinate &out, String &,
// 				  const RecordInterface &header, 
// 				  uInt whichAxis, LogIO &logger,
// 				  Bool oneRelative)
// {
//     Int spectralAxis;
//     Double referenceChannel, referenceFrequency, deltaFrequency;
//     Vector<Double> frequencies;
//     MFrequency::Types refFrame;
//     MDoppler::Types velocityPreference;
//     Double restFrequency;
// //    
//     Bool ok = FITSSpectralUtil::fromFITSHeader(spectralAxis,
// 					       referenceChannel,
// 					       referenceFrequency,
// 					       deltaFrequency,
// 					       frequencies,
// 					       refFrame,
// 					       velocityPreference,
// 					       restFrequency,
// 					       logger,
// 					       header,
// 					       'c',
// 					       oneRelative);
// //
//     if (casa::near(deltaFrequency,Double(0.0), Double(1.0e-6))) {
//        logger << LogIO::WARN << "The increment is zero.  Arbitrarily setting to 10% of the reference value" << LogIO::POST;
//        deltaFrequency = referenceFrequency / 10.0;
//     }
//     restFrequency = max(0.0, restFrequency);
// //
//     if (ok) {
//        if (spectralAxis == Int(whichAxis)) {
//           SpectralCoordinate tmp(refFrame, referenceFrequency, deltaFrequency, 
//                                  referenceChannel, restFrequency);
//           out = tmp;
//        } else {
//           logger << LogIO::SEVERE << "Disgreement about where the spectral axis is. " <<
// 	    spectralAxis << " vs. " << whichAxis << LogIO::POST;
//           ok = False;
//        }
//     }
// //					       
//     return ok;
// }


Coordinate* SpectralCoordinate::makeFourierCoordinate (const Vector<Bool>& axes,
                                                       const Vector<Int>& shape) const
// 
// axes says which axes in the coordinate are to be transformed
// shape is the shape of the image for all axes in this coordinate
//
{
   if (pTabular_p) {
      set_error("Cannot Fourier Transform a non-linear SpectralCoordinate");
      return 0;
   }
//  
   if (axes.nelements() != 1) {
      set_error ("Invalid number of specified axes");
      return 0;
   }
   if (shape.nelements() != 1) {
      set_error ("Invalid number of elements in shape");
      return 0;
   }
//
   if (!axes[0]) {
      set_error ("You have not specified any axes to transform");
      return 0;      
   }
//
   const Vector<String>& units = worldAxisUnits();
   const Vector<String>& names = worldAxisNames();
//
   Vector<String> unitsCanon(units.copy());
   Vector<String> unitsOut(units.copy());
   Vector<String> namesOut(names.copy());
//
   fourierUnits(namesOut[0], unitsOut[0], unitsCanon[0], Coordinate::SPECTRAL, 0,
                units[0], names[0]); 
        
// Make a copy of ourselves so we can change the units (else we would
// need to make this a non-const function)
    
    SpectralCoordinate sc(*this);
    if (!sc.setWorldAxisUnits(unitsCanon)) {
      set_error ("Could not set world axis units");
      return 0;      
    }
      
// Set the Fourier coordinate parameters.  This does not yet handle
// the pc matrix being anything other than unity...
    
   Vector<Double> crval(sc.referenceValue().copy());
   Vector<Double> crpix(sc.referencePixel().copy());
   Vector<Double> cdelt(sc.increment().copy());   
   crval[0] = 0.0; 
   cdelt[0] = 1.0 / (shape(0) * cdelt(0));
   crpix[0] = Int(shape(0)/2);
    
// Now create the new output LinearCoordinate

    Matrix<Double> pc(1, 1);
    pc = 0.0;
    pc.diagonal() = 1.0;
    return new LinearCoordinate(namesOut, unitsOut, crval, cdelt, pc, crpix);
}
        


void SpectralCoordinate::deleteVelocityMachine ()
{
   if (pVelocityMachine_p) {
      delete pVelocityMachine_p;  
      pVelocityMachine_p = 0;
   }
}

void SpectralCoordinate::deleteConversionMachines()
{
   if (pConversionMachineTo_p) {
      delete pConversionMachineTo_p;
      pConversionMachineTo_p = 0;
   }
//
   if (pConversionMachineFrom_p) {
      delete pConversionMachineFrom_p;
      pConversionMachineFrom_p = 0;
   }
}


Bool SpectralCoordinate::setFormatUnit (const String& unit)
{
   const Unit unitHZ(String("Hz"));      
   const Unit unitKMS(String("km/s"));      
   Unit t(unit);
   if (t != unitHZ && t != unitKMS) {
      return False;
   }
//
   formatUnit_p = unit;
   return True;
}

String SpectralCoordinate::format (String& units,
                                   Coordinate::formatType format,
                                   Double worldValue,
                                   uInt worldAxis,
                                   Bool isAbsolute,
                                   Bool showAsAbsolute,
                                   Int precision)
{
   AlwaysAssert(worldAxis < nWorldAxes(), AipsError);
    
// Check format
                                   
   Coordinate::formatType form = format;
   checkFormat (form, showAsAbsolute);
                                   
// Set default precision
                                   
   Int prec = precision;
   if (prec < 0) getPrecision(prec, form, showAsAbsolute, -1, -1, -1);

// If units are empty use formatUnit_p unit.   If that's
// empty use natuive world unit.
// If given units are not consistent with native units
// then see if they are velocity.  If so, convert to
// desired units.
  
   static const Unit unitsHZ(String("Hz"));      
   static const Unit unitsKMS_c(String("km/s"));      
   static Quantum<Double> qVel;
   static Quantum<Double> qFreq;
   static Vector<Double> world;

// Use default format unit (which itself may be empty) if empty

   if (units.empty()) {
      units = formatUnit_p;
   }
   Unit unit(units);
//
   String theString;
   if (units.empty() || unit == unitsHZ) {

// Requested unit is empty or consistent with Hz.  

      theString = Coordinate::format(units, form, worldValue, worldAxis,
                                     isAbsolute, showAsAbsolute, precision);
   } else {

// Is unit sensible ?

      if (unit != unitsKMS_c) {
        throw(AipsError("Requested units must be consistent with km/s or Hz for a SpectralCoordinate"));
      }

// Requested unit is consistent with km/s

      world.resize(nWorldAxes());
      String tunits(units);

// We must convert to absolute first (regardless of how we want
// to see the value) as we are formatting in velocity units

      if (!isAbsolute) {
         world = 0.0;
         world(worldAxis) = worldValue; 
         makeWorldAbsolute(world);
         worldValue = world(worldAxis);
      }
//
      if (showAsAbsolute) {
         if (!frequencyToVelocity (qVel, worldValue)) {
            theString = "Fail";
            return theString;
         }

// Convert from velUnit_p (used in f2v) to desired unit

         worldValue = qVel.getValue(unit);
      } else {

// Find relative coordinate in km/s consistent units

         static Vector<Double> vel(2), freq2(2);
         freq2(0) = referenceValue()(worldAxis);
         freq2(1) = worldValue;
         if (!frequencyToVelocity(vel, freq2)) {
            theString = "Fail";
            return theString;
         }

// Convert from velUnit_p (used in f2v) to desired unit

         Quantum<Double> t(vel[1]-vel[0], Unit(velUnit_p)); // rel=abs-ref
         worldValue = t.getValue(unit);
      }
//
      ostringstream oss;
      if (form == Coordinate::MIXED) {
         oss << worldValue;
      } else if (form == Coordinate::SCIENTIFIC) {
         oss.setf(ios::scientific, ios::floatfield);
         oss.precision(prec);
         oss << worldValue;
      } else if (form == Coordinate::FIXED) {
         oss.setf(ios::fixed, ios::floatfield);
         oss.precision(prec);
         oss << worldValue;
      }
      theString = String(oss);
    }
//
   return theString;
}
  

void SpectralCoordinate::checkFormat(Coordinate::formatType& format,
                                     const Bool ) const
{  

// Absolute or offset is irrelevant

   if (format != Coordinate::SCIENTIFIC &&
       format != Coordinate::FIXED) format = Coordinate::DEFAULT;
//
   if (format == Coordinate::DEFAULT) format = Coordinate::MIXED;
}

const Vector<Double>& SpectralCoordinate::restFrequencies() const
{
   return restfreqs_p;
}



String SpectralCoordinate::formatRestFrequencies () const
{  
   const Vector<Double>& rfs = restFrequencies();
   Double rf = restFrequency();
   String unit = worldAxisUnits()(0);
   const uInt n = rfs.nelements();
//
   if (n==0) return String("");
         
// It should never be that the active rest frequency is zero
// but there is more than one.  Zero is often used when making
// a continuum SpectralCoordinate where the restfreq is irrelevant
   
   ostringstream oss;
   if (rf > 0.0) {
      oss << "Rest frequency      : " << rf;
//
      if (n > 1) {
         oss << " [";
         uInt j = 0;
         for (uInt i=0; i<n; i++) {
            if (!casa::near(rfs(i), rf)) {
               if (j > 0) oss << ", ";
               oss << rfs(i);
               j++;
            }
         }
         oss << "]";
      }
//
      oss << " " << unit;
   }   
//
   return String(oss);
}



void SpectralCoordinate::makeWCS(::wcsprm& wcs, const String& ctype, Double refPix,
                                 Double refVal, Double inc,
                                 Double pc, Double restFreq)
{
    wcs.flag = -1;
    int iret = wcsini(1, 1, &wcs);
    if (iret != 0) {
        String errmsg = "wcs wcsini_error: ";
        errmsg += wcsini_errmsg[iret];
        throw(AipsError(errmsg));
    }

// Fill it in

    wcs.pc[0] = pc;
    wcs.crpix[0] = refPix;
    wcs.cdelt[0] = inc;
    wcs.crval[0] = refVal;
    wcs.restfrq = restFreq;
    strcpy (wcs.ctype[0], ctype.chars());

// Unit currently ignored; Hz assumed

/*
    String unit("Hz");
    strcpy (wcs.cunit[0], unit.chars());
*/

    
// Fill in the wcs structure
  
    if (int iret = wcsset(&wcs)) {
        String errmsg = "wcs wcsset_error: ";
        errmsg += wcsset_errmsg[iret];
        throw(AipsError(errmsg));
    }
}



Bool SpectralCoordinate::wcsSave (RecordInterface& rec, const ::wcsprm& wcs,  
                                  const String& fieldName) const
//
// Save the things that come out of the wcs structure
//
{
    Bool ok = (!rec.isDefined(fieldName));
//
    String ctype(wcs.ctype[0], 9);
    if (ok) {
       Record subrec;
       subrec.define("crval", referenceValue()(0));
       subrec.define("crpix", referencePixel()(0));
       subrec.define("cdelt", increment()(0));
       subrec.define("pc", linearTransform()(0,0));
       subrec.define("ctype", ctype);
//
       rec.defineRecord(fieldName, subrec);
    }
    return ok;
}

Bool SpectralCoordinate::wcsRestore (Double& crval, Double& crpix, Double& cdelt,
                                     Double& pc, String& ctype,
                                     const RecordInterface& rec)
{
   if (rec.isDefined("crval")) {
      rec.get("crval", crval);
   } else {
      return False;
   }
//
   if (rec.isDefined("crpix")) {
      rec.get("crpix", crpix);
   } else {
      return False;
   }
//
   if (rec.isDefined("cdelt")) {
      rec.get("cdelt", cdelt);
   } else {
      return False;
   }
//
   if (rec.isDefined("pc")) {
      rec.get("pc", pc);
   } else {
      return False;
   }
//
   if (rec.isDefined("ctype")) {
      rec.get("ctype", ctype);
   } else {
      return False;
   }
//
   return True;
}

void SpectralCoordinate::toCurrent(Vector<Double>& value) const
{
    value /= to_hz_p;
}


void SpectralCoordinate::fromCurrent(Vector<Double>& value) const
{
    value *= to_hz_p;
}


const Vector<Double> SpectralCoordinate::toCurrentFactors () const
{
    Vector<Double> t(1);
    t[0] = 1.0 / to_hz_p;
    return t;
}





void SpectralCoordinate::copy (const SpectralCoordinate &other)
{

   type_p = other.type_p;
   to_hz_p = other.to_hz_p;
//
   restfreqs_p.resize(0);
   restfreqs_p = other.restfreqs_p;
//
   restfreqIdx_p = other.restfreqIdx_p;

// Clean up first

   if (pTabular_p) {
      delete pTabular_p; 
      pTabular_p = 0;
   } 
   if (wcs_p.flag != -1) {    
       wcsfree (&wcs_p);
   }

// Copy TabularCoordinate or wcs structure. Only one of the two
// is allocated at any given time.

    if (other.pTabular_p) {
       pTabular_p = new TabularCoordinate(*(other.pTabular_p));
    } else {
       int err = wcscopy (1, &(other.wcs_p), &wcs_p);
       if (err != 0) {
          String errmsg = "wcs wcscopy_error: ";
          errmsg += wcscopy_errmsg[err];
          throw(AipsError(errmsg));
       }
       set_wcs(wcs_p);
    }

    conversionType_p = other.conversionType_p;
    direction_p = other.direction_p;
    position_p = other.position_p;
    epoch_p = other.epoch_p;
//
    velType_p = other.velType_p;
    velUnit_p = other.velUnit_p;
    unit_p = other.unit_p;
    axisName_p = other.axisName_p;
    formatUnit_p = other.formatUnit_p;

// Machines

    makeConversionMachines(type_p, conversionType_p, epoch_p, position_p,
                           direction_p);
//
    deleteVelocityMachine();
    if (other.pVelocityMachine_p) {
       pVelocityMachine_p = new VelocityMachine(*(other.pVelocityMachine_p));
    }
}

} //# NAMESPACE CASA - END

