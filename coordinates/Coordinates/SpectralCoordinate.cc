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



#include <casacore/coordinates/Coordinates/SpectralCoordinate.h>
#include <casacore/coordinates/Coordinates/TabularCoordinate.h>
#include <casacore/coordinates/Coordinates/LinearXform.h>
#include <casacore/coordinates/Coordinates/LinearCoordinate.h>

#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/fits/FITS/FITSSpectralUtil.h>
#include <casacore/fits/FITS/FITSKeywordUtil.h>
#include <casacore/scimath/Functionals/ScalarSampledFunctional.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/measures/Measures/MFrequency.h>
#include <casacore/measures/Measures/MeasureHolder.h>
#include <casacore/measures/Measures/VelocityMachine.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/casa/Containers/RecordInterface.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/Logging/LogOrigin.h>

#include <casacore/casa/sstream.h>
#include <casacore/casa/iostream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN


SpectralCoordinate::SpectralCoordinate()
: Coordinate(),
  type_p(MFrequency::TOPO), 
  conversionType_p(type_p),
  restfreqs_p(0),
  restfreqIdx_p(0),
  pConversionMachineTo_p(0),
  pConversionMachineFrom_p(0),
  pVelocityMachine_p(0),
  velType_p(MDoppler::RADIO),
  velUnit_p("km/s"),
  waveUnit_p("mm"),
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
   to_m_p = 0.001;
   nativeType_p = SpectralCoordinate::FREQ;
//
   setDefaultWorldMixRanges();
}


SpectralCoordinate::SpectralCoordinate(MFrequency::Types type,
		                                 double refVal, double inc,
		                                 double refPix, double restFrequency)
: Coordinate(),
  type_p(type), 
  conversionType_p(type_p),
  restfreqs_p(0),
  restfreqIdx_p(0),
  pConversionMachineTo_p(0),
  pConversionMachineFrom_p(0),
  pVelocityMachine_p(0),
  velType_p(MDoppler::RADIO),
  velUnit_p("km/s"),
  waveUnit_p("mm"),
  unit_p(Unit("Hz")),
  axisName_p("Frequency"),
  formatUnit_p("")
{
//
   restfreqs_p.resize(1);
   restfreqs_p(0) = max(0.0, restFrequency);
//
   makeVelocityMachine (velUnit_p, velType_p, unit_p,
                        type_p, restfreqs_p(restfreqIdx_p));
//
   makeWCS(wcs_p, String("FREQ"), refPix, refVal, inc, 1.0, restfreqs_p(0));
   to_hz_p = 1.0;
   to_m_p = 0.001;
   nativeType_p = SpectralCoordinate::FREQ;
//
   setDefaultWorldMixRanges();
}





SpectralCoordinate::SpectralCoordinate(MFrequency::Types type, 
                                       const Quantum<double>& refVal,
                                       const Quantum<double>& inc,
                                       double refPix, 
                                       const Quantum<double>& restFrequency)
: Coordinate(),
  type_p(type),
  conversionType_p(type_p),
  restfreqs_p(0),
  restfreqIdx_p(0),
  pConversionMachineTo_p(0),
  pConversionMachineFrom_p(0),
  pVelocityMachine_p(0),
  velType_p(MDoppler::RADIO),
  velUnit_p("km/s"),
  waveUnit_p("mm"),
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
   to_m_p = 0.001;
   nativeType_p = SpectralCoordinate::FREQ;
//
   setDefaultWorldMixRanges();
}



SpectralCoordinate::SpectralCoordinate(
	MFrequency::Types type,  const Vector<double> &freqs,
	double restFrequency)
	: Coordinate(), type_p(type),
	  conversionType_p(type_p), restfreqs_p(0),
	  restfreqIdx_p(0), pConversionMachineTo_p(0),
	  pConversionMachineFrom_p(0), pVelocityMachine_p(0),
	  velType_p(MDoppler::RADIO), velUnit_p("km/s"),
	  waveUnit_p("mm"), unit_p(Unit("Hz")),
	  axisName_p("Frequency"), formatUnit_p("") {
	AlwaysAssert(restFrequency>=0.0, AipsError);
	restfreqs_p.resize(1);
	restfreqs_p(0) = max(0.0, restFrequency);
	_setTabulatedFrequencies(freqs);
	to_hz_p = 1.0;
	to_m_p = 0.001;
	nativeType_p = SpectralCoordinate::FREQ;
	makeVelocityMachine(
		velUnit_p, velType_p, unit_p,
		type_p, restfreqs_p(restfreqIdx_p)
	);
	wcs_p.flag = -1;                // Uninitialized
	setDefaultWorldMixRanges();
}


SpectralCoordinate::SpectralCoordinate(
	MFrequency::Types type,
	const Quantum<Vector<double> >& freqs,
	const Quantum<double>& restFrequency
)
	: Coordinate(), type_p(type),
	  conversionType_p(type_p), restfreqs_p(0),
	  restfreqIdx_p(0), pConversionMachineTo_p(0),
	  pConversionMachineFrom_p(0),
	  pVelocityMachine_p(0), velType_p(MDoppler::RADIO),
	  velUnit_p("km/s"), waveUnit_p("mm"),
	  unit_p(Unit("Hz")), axisName_p("Frequency"),
	  formatUnit_p("") {
	Unit hz("Hz");
	if (!freqs.isConform(hz)) {
		throw(AipsError("Unit of frequencies is not consistent with Hz"));
	}
	if (!restFrequency.isConform(hz)) {
		throw(AipsError("Unit of rest frequency is not consistent with Hz"));
	}
	AlwaysAssert(restFrequency.getValue(hz)>=0.0, AipsError);
	restfreqs_p.resize(1);
	restfreqs_p(0) = max(0.0, restFrequency.getValue(hz));
	Vector<double> freqs2 = freqs.getValue(hz);
	_setTabulatedFrequencies(freqs2);
	to_hz_p = 1.0;
	to_m_p = 0.001;
	nativeType_p = SpectralCoordinate::FREQ;
	makeVelocityMachine(
		velUnit_p, velType_p, unit_p,
		type_p, restfreqs_p(restfreqIdx_p)
	);
	wcs_p.flag = -1;                // Uninitialized
	setDefaultWorldMixRanges();
}


SpectralCoordinate::SpectralCoordinate(
	MFrequency::Types freqType,
	MDoppler::Types velType,
	const Vector<double>& velocities,
	const String& velUnit,
	double restFrequency
)
	: Coordinate(),
	  type_p(freqType), conversionType_p(type_p),
	  restfreqs_p(0), restfreqIdx_p(0),
	  pConversionMachineTo_p(0), pConversionMachineFrom_p(0),
	  pVelocityMachine_p(0), velType_p(velType),
	  velUnit_p("km/s"), waveUnit_p("mm"), unit_p("Hz"),
	  axisName_p("Frequency"), formatUnit_p("") {
	restfreqs_p.resize(1);
	restfreqs_p(0) = restFrequency;
   
	// Convert to frequency

	makeVelocityMachine(
		velUnit, velType, String("Hz"), freqType, restFrequency
	);
	Quantum<Vector<double> > frequencies
		= pVelocityMachine_p->makeFrequency(velocities);

	_setTabulatedFrequencies(frequencies.getValue());
	to_hz_p = 1.0;
	to_m_p = 0.001;
	if (velType == MDoppler::OPTICAL) {
		nativeType_p = SpectralCoordinate::VOPT;
	}
	else {
		nativeType_p = SpectralCoordinate::VRAD;
	}

        // Now remake Velocity Machine to be consistent with state

	deleteVelocityMachine();
	makeVelocityMachine (
		velUnit_p, velType_p, unit_p,
		type_p, restfreqs_p(restfreqIdx_p)
	);
	wcs_p.flag = -1;                // Uninitialized
	setDefaultWorldMixRanges();
}

SpectralCoordinate::SpectralCoordinate(
	MFrequency::Types freqType,
	const Vector<double>& wavelengths,
	const String&waveUnit, double restFrequency,
	bool inAir
)
	: Coordinate(), type_p(freqType),
	  conversionType_p(type_p), restfreqs_p(0),
	  restfreqIdx_p(0), pConversionMachineTo_p(0),
	  pConversionMachineFrom_p(0),
	  pVelocityMachine_p(0), velType_p(MDoppler::RADIO),
	  velUnit_p("km/s"), waveUnit_p("mm"), unit_p("Hz"),
	  axisName_p("Frequency"), formatUnit_p("") {
	restfreqs_p.resize(1);
	restfreqs_p(0) = restFrequency;

	to_hz_p = 1.;
	to_m_p = 0.001;
   
	// Convert to frequency

	if(!setWavelengthUnit(waveUnit)){
		throw(AipsError("Wavelength unit is not consistent with m"));
	}
     
	Vector<double> frequencies;
	if(inAir){
		airWavelengthToFrequency(frequencies, wavelengths);
		nativeType_p = SpectralCoordinate::AWAV;
	}
	else{
		wavelengthToFrequency(frequencies, wavelengths);
		nativeType_p = SpectralCoordinate::WAVE;
	}


	_setTabulatedFrequencies(frequencies);

        // Now remake Velocity Machine to be consistent with state

	deleteVelocityMachine();
	makeVelocityMachine (
		velUnit_p, velType_p, unit_p,
        type_p, restfreqs_p(restfreqIdx_p)
	);
	wcs_p.flag = -1;                // Uninitialized
	setDefaultWorldMixRanges();
}


SpectralCoordinate::SpectralCoordinate (MFrequency::Types type, const ::wcsprm& wcs, bool oneRel)
: Coordinate(),
  type_p(type), 
  conversionType_p(type_p),
  restfreqs_p(0),
  restfreqIdx_p(0),
  pConversionMachineTo_p(0),
  pConversionMachineFrom_p(0),
  pVelocityMachine_p(0),
  velType_p(MDoppler::RADIO),
  velUnit_p("km/s"),
  waveUnit_p("mm"),
  unit_p(Unit("Hz")),
  axisName_p("Frequency"),
  formatUnit_p("")
{

// Check holds only spectral wcs structure


// Copy wcs structure

   wcs_p.flag = -1;
   copy_wcs(wcs, wcs_p);
   set_wcs(wcs_p);
   to_hz_p = 1.0;
   to_m_p = 0.001;

// Make 0-relative

   if (oneRel) {
      wcs_p.crpix[0] -= 1.0;
   }

// Rest frequency

   restfreqs_p.resize(1);
   restfreqs_p(0) = max(0.0, wcs.restfrq);

   nativeType_p = SpectralCoordinate::FREQ;

// Velocity machine

   makeVelocityMachine (velUnit_p, velType_p, unit_p,
                        type_p, restfreqs_p(restfreqIdx_p));

// Set name to something from wcs structure (from ctypes) ?

   setDefaultWorldMixRanges();
}


SpectralCoordinate::SpectralCoordinate(const SpectralCoordinate &other)
: Coordinate(other),
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

uint32_t SpectralCoordinate::nPixelAxes() const
{
    return 1;
}

uint32_t SpectralCoordinate::nWorldAxes() const
{
    return 1;
}


bool SpectralCoordinate::toWorld (Vector<double> &world,
                                  const Vector<double> &pixel, bool useConversionFrame) const
{

// Convert to World (Hz)

    bool ok = true;
    if (_tabular.ptr()) {
       ok = _tabular->toWorld(world, pixel);
       if (!ok) set_error(_tabular->errorMessage());
    } else {
       ok = toWorldWCS (world, pixel, wcs_p);
    }
    if (!ok) return false;

// Convert to correct units from Hz

    toCurrent(world);

// Convert to output reference type

    if (useConversionFrame) {
        convertTo(world);
    }
//
    return ok;
}

bool SpectralCoordinate::toWorld(double& world, const double& pixel) const
{
    thread_local static Vector<double> pixel_tmp1(1);
    thread_local static Vector<double> world_tmp1(1);
//
    pixel_tmp1[0] = pixel;
    if (toWorld(world_tmp1, pixel_tmp1)) {
       world = world_tmp1[0];
       return true;
    } else {
       return false;
    }
}



bool SpectralCoordinate::toPixel (Vector<double> &pixel,
                                  const Vector<double> &world) const
{
    thread_local static Vector<double> world_tmp1(1);
    DebugAssert(world.nelements()==1, AipsError);
    bool ok = true;

// Convert from specified conversion reference type

    world_tmp1[0] = world[0];
    convertFrom(world_tmp1);

// Convert from current units to Hz 

    fromCurrent(world_tmp1);

// Convert to pixel

    if (_tabular.ptr()) {
       ok = _tabular->toPixel(pixel, world_tmp1);
       if (!ok) set_error(_tabular->errorMessage());
    } else {
       ok = toPixelWCS (pixel, world_tmp1, wcs_p);
    }
//
    return ok;
}



bool SpectralCoordinate::toPixel(double& pixel, const double& world) const
{
    static Vector<double> pixel_tmp2(1);
    static Vector<double> world_tmp2(1);
//
    world_tmp2[0] = world;
    if (toPixel(pixel_tmp2, world_tmp2)) {
       pixel = pixel_tmp2(0);
       return true;
    } else {
       return false;
    }
}


bool SpectralCoordinate::toWorldMany (Matrix<double>& world,
                                      const Matrix<double>& pixel,
                                      Vector<bool>& failures) const
{

// Convert to world (Hz)

   bool ok = true;
   if (_tabular.ptr()) {
      ok = _tabular->toWorldMany(world, pixel, failures);
      if (!ok) set_error(_tabular->errorMessage());
   } else {
      ok = toWorldManyWCS (world, pixel, failures, wcs_p);
   }
   if (!ok) return false;

// Convert to current units from wcs units

   toCurrentMany (world, toCurrentFactors());

// Convert to specified conversion reference type

   if (pConversionMachineTo_p) convertToMany(world);
//
   return true;
}


bool SpectralCoordinate::toPixelMany (Matrix<double>& pixel,
                                      const Matrix<double>& world,
                                      Vector<bool>& failures) const
{
    uint32_t nWorld = nWorldAxes();
    AlwaysAssert(world.nrow()==nWorld, AipsError);
    
// Copy input as we have to convert it to all sorts of things
       
    Matrix<double> world2(world.copy());
  
// Convert from specified conversion reference type

    if (pConversionMachineTo_p) convertFromMany (world2);
    
// Convert from current units to wcs units (Hz)

    fromCurrentMany (world2, toCurrentFactors());

// Convert to pixel

    bool ok = true;
    if (_tabular.ptr()) {
       _tabular->toPixelMany(pixel, world2, failures);
       if (!ok) set_error(_tabular->errorMessage());
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

Vector<double> SpectralCoordinate::referencePixel() const
{
    if (_tabular.ptr()) {
       return _tabular->referencePixel();
    } else {
       Vector<double> crpix(1);
       crpix[0] = wcs_p.crpix[0];
       return crpix;
    }
}


Matrix<double> SpectralCoordinate::linearTransform() const
{
    if (_tabular.ptr()) {
       return _tabular->linearTransform();
    } else {
       Matrix<double> tmp(1,1);
       tmp(0,0) = wcs_p.pc[0];
       return tmp;
    }
}

Vector<double> SpectralCoordinate::increment() const
{

// Get in Hz

    Vector<double> value(1);
    if (_tabular.ptr()) {
       value= _tabular->increment();
    } else {
       value[0] = wcs_p.cdelt[0];
    }

// Convert to current units

    toCurrent (value);
//
    return value;
}


Vector<double> SpectralCoordinate::referenceValue() const
{

// Get in Hz

    Vector<double> value(1);
    if (_tabular.ptr()) {
       value= _tabular->referenceValue();
    } else {
       value[0] = wcs_p.crval[0];
    }

// Convert to current units

    toCurrent (value);
//
    return value;
}

bool SpectralCoordinate::setWorldAxisNames(const Vector<String>& names)
{  
    bool ok = (names.nelements()==1);
    if (!ok) {
       set_error ("names vector must be of length 1");
    } else {
       axisName_p = names[0];
    }
    return ok;
}


bool SpectralCoordinate::setWorldAxisUnits(const Vector<String>& units)
{  
    if (!(units.nelements()==1)) {
       set_error("units vector must be of length 1");
       return false;
    }

// Find scale factor to convert old to new

    String error;
    Vector<double> factor;
    bool ok = find_scale_factor(error, factor, units, worldAxisUnits());
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



bool SpectralCoordinate::setVelocity (const String& velUnit,
                                      MDoppler::Types velType)
{
   static const Unit unitsKMS_b(String("km/s"));
   if (!velUnit.empty()) {
      Unit unit(velUnit);
      if (unit!=unitsKMS_b) {
         set_error("Unit must be empty or consistent with m/s");
         return false; 
      }
      velUnit_p = velUnit;
   }
   velType_p = velType;
   updateVelocityMachine(velUnit_p, velType_p);
//
   return true;
}

bool SpectralCoordinate::setWavelengthUnit(const String& waveUnit)
{

   static const Unit unitsM_b(String("m"));

   String wu = waveUnit;

   if (wu.empty()) {
     wu = "mm"; // the default
   }
   Unit unit(wu);
   if (unit!=unitsM_b) {
     set_error("Unit must be empty or consistent with m");
     return false; 
   }
   
   String error;
   Vector<double> factor;
   Vector<String> outUnit(1,"m");
   Vector<String> inUnit(1,wu);
   if(!find_scale_factor(error, factor, outUnit, inUnit)){
     set_error(error);
     return false;
   }
   to_m_p = factor(0);
   waveUnit_p = wu;
   return true;
}

bool SpectralCoordinate::setNativeType(const SpectralCoordinate::SpecType spcType)
{
	// just copy that over
	nativeType_p = spcType;

	//
	return true;
}

//static bool stringtoSpecType(SpecType &specType, const String &stypeString) const;
//String SpectralCoordinate::specTypetoString(SpecType specType)
bool SpectralCoordinate::specTypetoString(String &stypeString, const SpecType &specType)
{
	bool rvalue=true;

	switch (specType)
	{
	case FREQ:
		stypeString = String("frequency");
		break;
	case VRAD:
		stypeString = String("radio velocity");
		break;
	case VOPT:
		stypeString = String("optical velocity");
		break;
	case BETA:
		stypeString = String("true");
		break;
	case WAVE:
		stypeString = String("wavelength");
		break;
	case AWAV:
		stypeString = String("air wavelength");
		break;
	default:
		rvalue=false;
	}

	return rvalue;
}

//static bool stringtoSpecType(SpecType &specType, const String &stypeString) const;
//SpectralCoordinate::SpecType SpectralCoordinate::stringtoSpecType(String stypeString)
bool SpectralCoordinate::stringtoSpecType(SpecType &specType, const String &stypeString)
{

	if (!stypeString.compare("frequency")){
		specType = FREQ;
		return true;
	}
	else if (!stypeString.compare("radio velocity")){
		specType = VRAD;
		return true;
	}
	else if (!stypeString.compare("optical velocity")){
		specType = VOPT;
		return true;
	}
	else if (!stypeString.compare("true")){
		specType = BETA;
		return true;
	}
	else if (!stypeString.compare("wavelength")){
		specType = WAVE;
		return true;
	}
	else if (!stypeString.compare("air wavelength")){
		specType = AWAV;
		return true;
	}
	else
	{
		return false;
	}

	// should never get to here
	return false;
}

bool SpectralCoordinate::setReferenceConversion (MFrequency::Types conversionType,
                                                 const MEpoch& epoch, const MPosition& position,
                                                 const MDirection& direction)
{
// See if something to do

   if (conversionType_p==conversionType) return true;
//
   int32_t ok = makeConversionMachines(type_p, conversionType, epoch, position, direction);
   if (ok==-1) {

// Trial conversion failed.  The machines will be deleted so we must set the
// conversion machines back to what they were before this calamity.

      makeConversionMachines(type_p, conversionType_p, epoch_p, 
                             position_p, direction_p);
      return false;
   }
//
   conversionType_p = conversionType;
   epoch_p = epoch;
   position_p = position;
   direction_p = direction;   
//
   return true;
}


bool SpectralCoordinate::setReferencePixel(const Vector<double> &refPix)
{
    if (!(refPix.nelements()==nPixelAxes())) {
       set_error("reference pixels vector must be of length 1");
       return false;
    }
//
    bool ok= true;
    if (_tabular.ptr()) {
       ok = _tabular->setReferencePixel(refPix);
       if (!ok) set_error (_tabular->errorMessage());
    } else {

// Set WCS card

       wcs_p.crpix[0] = refPix[0];

// Tell WCS

       set_wcs(wcs_p);
    }
//
    return ok;
}

bool SpectralCoordinate::setLinearTransform(const Matrix<double> &xform)
{
    bool ok = (xform.nrow()==1 && xform.ncolumn()==1);
    if (!ok) {
       set_error("linear transform matrix has wrong shape");
       return false;
    }
//
    if (_tabular.ptr()) {
       ok = _tabular->setLinearTransform(xform);
       if (!ok) set_error(_tabular->errorMessage());
    } else {

// Set PC card

       wcs_p.pc[0] = xform(0,0);
       set_wcs(wcs_p);
    }
//
    return ok;
}


bool SpectralCoordinate::setIncrement (const Vector<double>& incr)
{
    if (!(incr.nelements()==nWorldAxes())) {
       set_error("increment vector must be of length 1");
       return false;
    }

// Convert to Hz

    Vector<double> value(incr.copy());
    fromCurrent (value);

// Now set

    bool ok= true;
    if (_tabular.ptr()) {
       ok = _tabular->setIncrement(value);
       if (!ok) set_error (_tabular->errorMessage());
    } else {

// Set WCS card

       wcs_p.cdelt[0] = value[0];
       set_wcs(wcs_p);
    }
//
    return ok;
}



bool SpectralCoordinate::setReferenceValue(const Vector<double>& refval)
{
    if (!(refval.nelements()==nWorldAxes())) {
       set_error("reference value vector must be of length 1");
       return false;
    }

// Convert to Hz

    Vector<double> value(refval.copy());
    fromCurrent (value);
//
    bool ok= true;
    if (_tabular.ptr()) {
       ok = _tabular->setReferenceValue(value);
       if (!ok) set_error (_tabular->errorMessage());
    } else {

// Set WCS card

       wcs_p.crval[0] = value[0];
       set_wcs(wcs_p);
    }
//
    return ok;
}


double SpectralCoordinate::restFrequency() const
{
    return restfreqs_p(restfreqIdx_p);
}

Vector<double> SpectralCoordinate::pixelValues() const
{
    if (_tabular.ptr()) {
       return _tabular->pixelValues();
    } else {
       Vector<double> pixels;
       return pixels;
    }
}

Vector<double> SpectralCoordinate::worldValues() const
{
    Vector<double> worlds;
    if (_tabular.ptr()) {
       worlds = _tabular->worldValues();    // Hz
       toCurrent(worlds);
    }
//
    return worlds;
}

MFrequency::Types SpectralCoordinate::frequencySystem(bool showConversion) const
{
    if (showConversion) {
       return conversionType_p;
    } else {
       return type_p;
    }         
}

void  SpectralCoordinate::setFrequencySystem(MFrequency::Types type, bool verbose)
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

   if (verbose && oldType != conversionType_p) {
      LogIO os(LogOrigin("SpectralCoordinate", "setFrequencySystem"));
      os << LogIO::WARN << "Resetting the conversion frequency system " << MFrequency::showType(conversionType_p) << endl;
      os << "to the new native frequency system " << MFrequency::showType(type_p) << endl;
      os << "You must explicitly reset the conversion frequency system if desired" << LogIO::POST;
   }
//
   deleteConversionMachines();
   conversionType_p = type_p;
}

  bool SpectralCoordinate::transformFrequencySystem(MFrequency::Types type,
						    const MEpoch& epoch, const MPosition& position,
						    const MDirection& direction){

  bool rval=true;

  MFrequency::Types nativeCtype = frequencySystem(false);

  if (type != nativeCtype) {

    MFrequency::Types origType; // original type of the conversion layer
    MEpoch origEpoch;
    MPosition origPosition;
    MDirection origDirection;
    getReferenceConversion(origType, origEpoch, origPosition, origDirection);
    // use the reference conversion layer to do the transformation 
    if(origType!=type){
      if(!setReferenceConversion(type, epoch, position, direction)){
	setReferenceConversion(origType, origEpoch, origPosition, origDirection);
	return false;
      }
    }

    if(pixelValues().nelements() > 1){ // we have a tabular spectral coordinate
      
      Vector<String> oldunits(worldAxisUnits());
      Vector<String> tmpunits(1,"Hz"); // need freqs in Hz for setTabulatedFrequencies
      setWorldAxisUnits(tmpunits);
      Vector<double> tpixels = _tabular->pixelValues();
      Vector<double> newFreqs(tpixels.size());
      toWorld(newFreqs, tpixels);
      _setTabulatedFrequencies(newFreqs);
      setWorldAxisUnits(oldunits);

      Vector<double> newCrval(1, newFreqs[0]);
      setReferenceValue(newCrval);
      if(tpixels[tpixels.size()-1]-tpixels[0] != 0.){
	Vector<double> newCdelt(1, (newFreqs[tpixels.size()-1]-newFreqs[0])/(tpixels[tpixels.size()-1]-tpixels[0]));
	setIncrement(newCdelt); 
      }
      Vector<double> newRefPix(1, tpixels[0]);
      setReferencePixel(newRefPix);
    }
    else{ // not tabular: only need to change ctype, crval, cdelt
      Vector<double> newCrval(1,0.);
      toWorld(newCrval[0], referencePixel()[0]);
      
      double tmpWorld=0.; 
      toWorld(tmpWorld, referencePixel()[0]+1);
      Vector<double> newCdelt(1, tmpWorld-newCrval[0]);
      
      setReferenceValue(newCrval);
      setIncrement(newCdelt); 
    }

    setFrequencySystem(type, false);  
    if(origType!=type){
      rval = setReferenceConversion(origType, origEpoch, origPosition, origDirection);
    }
  }

  return rval;
}


bool SpectralCoordinate::setRestFrequency(double newFrequency, bool append)
{
    newFrequency = max(0.0, newFrequency);
    if (append) {
       uint32_t n = restfreqs_p.nelements();
       restfreqs_p.resize(n+1, true);
       restfreqs_p(n) = newFrequency;
       restfreqIdx_p = n;
    } else {
       restfreqs_p(restfreqIdx_p) = newFrequency;
    }

// Update velocity machine with the active rest frequency

    Quantum<double> rf(restfreqs_p(restfreqIdx_p), unit_p);
    pVelocityMachine_p->set(MVFrequency(rf));

// Update wcs struct with the active rest frequency

    wcs_p.restfrq = rf.getValue(Unit("Hz"));
//
    return true;
}

void SpectralCoordinate::setRestFrequencies(const Vector<double>& restFrequencies,
                                            uint32_t which, bool append)
{
   for (uint32_t i=0; i<restFrequencies.nelements(); i++) {
      AlwaysAssert(restFrequencies(i)>=0.0, AipsError);
   }
//
   if (append) {
      Vector<double> tmp = concatenateArray (restfreqs_p, restFrequencies);
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

void SpectralCoordinate::selectRestFrequency(double restFrequency)
{
   AlwaysAssert(restFrequency >= 0.0, AipsError);
   uint32_t which = 0;
   double d, diff = 1.0e99;
   for (uint32_t i=0; i<restfreqs_p.nelements(); i++) {
      d = abs(restfreqs_p(i) - restFrequency);
      if (d < diff) {
         which = i;
         diff = d;
      }
   }
//
   selectRestFrequency(which);
}

void SpectralCoordinate::selectRestFrequency(uint32_t which)
{
   AlwaysAssert(which<restfreqs_p.nelements(), AipsError)
//
   restfreqIdx_p = which;
   Quantum<double> rf(restfreqs_p(restfreqIdx_p), unit_p);
   pVelocityMachine_p->set(MVFrequency(rf));

// Update wcs struct with the active rest frequency

   wcs_p.restfrq = rf.getValue(Unit("Hz"));

}


bool SpectralCoordinate::near(const Coordinate& other, double tol) const
{
   Vector<int32_t> excludeAxes;
   return near(other, excludeAxes, tol);
}


bool SpectralCoordinate::near(const Coordinate& other,
                              const Vector<int32_t>& excludeAxes,
                              double tol) const
{
   if (this->type() != other.type()) {
      set_error("Comparison is not with another SpectralCoordinate");
      return false;
   }
//
   const SpectralCoordinate& sCoord = dynamic_cast<const SpectralCoordinate&>(other);

// Type

   if (type_p != sCoord.frequencySystem()) {
      set_error("The SpectralCoordinates have differing frequency systems");
      return false;
   }

// Rest freq

   if (!casacore::near(restFrequency(), sCoord.restFrequency(), tol)) {
      set_error("The SpectralCoordinates have differing active rest frequencies");
      return false;
   }

// Perhaps we shouldn't check the lists of rest frequencies. 
// Does it really matter ?

   const Vector<double>& rfs = sCoord.restFrequencies();
   if (restfreqs_p.nelements() != rfs.nelements()) {
      set_error("The SpectralCoordinates have differing numbers of rest frequencies");
      return false;
   }
//
   for (uint32_t i=0; i<restfreqs_p.nelements(); i++) {
      if (!casacore::near(restfreqs_p(i),rfs(i),tol)) {
         set_error("The SpectralCoordinates have differing lists of rest frequencies");
         return false;
      }
   }

// Conversion type
                
   if (conversionType_p != sCoord.conversionType_p) {
            
// Should this be an error or a warning ?
       
      set_error("The SpectralCoordinates have differing conversion types");
      return false;
   }

// Number of pixel and world axes 
   
   AlwaysAssert(nPixelAxes()==nWorldAxes(), AipsError);
   bool exclude(false);
   const uint32_t nExcl = excludeAxes.nelements();
   if (nExcl > 0) {
      if (excludeAxes(0)) exclude = true;
   }
      
// Check names
   
   ostringstream oss;
   if (!exclude) {
      if (axisName_p != sCoord.axisName_p) {
         set_error(String("The SpectralCoordinates have differing axis names"));
         return false;
      }
   }

// Unit

   if (unit_p != sCoord.unit_p) {
      set_error (String("The SpectralCoordinates have differing units"));
      return false;
   }


// Reference Value

   {
      const Vector<double>& thisVal = referenceValue();   
      const Vector<double>& thatVal = sCoord.referenceValue();
      if (!exclude) {
         if (!casacore::near(thisVal[0],thatVal[0])) {
            set_error(String("The SpectralCoordinates have differing reference values"));
            return false;
         }
      }
   }
// LinearXForm components
//Tabular spectral coordinates with 1 channel has increment 0. by definition in TabularCoordinates !
//so linear transform test is bound to fail so skip for that case
   if( !(_tabular.ptr() && (_tabular->pixelValues()).nelements()==1))
     {
      LinearXform thisVal(referencePixel(), increment(), linearTransform());
      LinearXform thatVal(sCoord.referencePixel(), sCoord.increment(), sCoord.linearTransform());
      if (!(thisVal.near(thatVal, excludeAxes))) {
         set_error(String("The SpectralCoordinates have differing LinearXform components"));
         return false;
      }
   }

// Velocity Stuff

   if (velType_p != sCoord.velType_p) {
      set_error("The SpectralCoordinates have differing velocity types");
      return false;
   }
   if (velUnit_p != sCoord.velUnit_p) {
      set_error("The SpectralCoordinates have differing velocity units");
      return false;
   }
//

   return true;
}

bool SpectralCoordinate::save(RecordInterface &container,
			    const String &fieldName) const
{
    bool ok = (!container.isDefined(fieldName));
    if (ok) {
        String system = MFrequency::showType(type_p);
//
	Record subrec;
	subrec.define ("version",   2);            // Original unversioned was v 1 !
	subrec.define("system",     system);
	subrec.define("restfreq",   restFrequency());
	subrec.define("restfreqs",  restFrequencies());
	subrec.define("velType",    int32_t(velType_p));
	subrec.define("nativeType", int32_t(nativeType_p));
	subrec.define("velUnit",    velUnit_p);
	subrec.define("waveUnit",   waveUnit_p);
	subrec.define("formatUnit", formatUnit_p);

// We may have TC (for tabular coordinates) or not.

	if (_tabular.ptr()) {
		ok = (_tabular->save(subrec, "tabular"));   // Always Hz
	} else {
		ok = wcsSave (subrec, wcs_p, "wcs");          // Always Hz
	}
	if (!ok) return false;
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
      int32_t v;
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
    double restfreq;
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

    SpectralCoordinate* pSpectral = 0;
    Unit qUnit(unit);
    Quantum<double> qRestFreq(restfreq, qUnit);
    const Vector<double>& worlds = pTabular->worldValues();
    if (worlds.nelements() > 0) {
       Quantum<Vector<double> > qWorlds(worlds, qUnit);
       pSpectral = new SpectralCoordinate (freqSys, qWorlds, qRestFreq);

// Set units first !

       pSpectral->setWorldAxisUnits(pTabular->worldAxisUnits());
       pSpectral->setReferencePixel(pTabular->referencePixel());
       pSpectral->setReferenceValue(pTabular->referenceValue());
    } else {
       Quantum<double> qcrval(pTabular->referenceValue()(0), qUnit);       
       Quantum<double> qcdelt(pTabular->increment()(0), qUnit);
       double crpix(pTabular->referencePixel()(0));
       pSpectral = new SpectralCoordinate (freqSys, qcrval, qcdelt, crpix, qRestFreq);
       pSpectral->setWorldAxisUnits(pTabular->worldAxisUnits());
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
    double restfreq;
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
    Quantum<double> qRestFreq(restfreq, qUnit);
//
    SpectralCoordinate* pSpectral = 0;
    if (subrec.isDefined("tabular")) {

// Reconstitute the TC (will be Hz)

       TabularCoordinate* pTabular = TabularCoordinate::restore(subrec, "tabular");
       if (pTabular == 0) return 0;

// Create SC (will be in Hz regardless of units)

       Quantum<Vector<double> > qWorlds(pTabular->worldValues(), 
                                        Unit(pTabular->worldAxisUnits()(0)));
       pSpectral = new SpectralCoordinate (freqSys, qWorlds, qRestFreq);
       AlwaysAssert(pSpectral, AipsError);
//
       pSpectral->setReferencePixel(pTabular->referencePixel());
       pSpectral->setReferenceValue(pTabular->referenceValue());   // Hz
       pSpectral->setLinearTransform(pTabular->linearTransform());  
       delete pTabular;
       pTabular = 0;
    } else if (subrec.isDefined("wcs")) {
       double crval, crpix, cdelt, pc;
       String ctype;
       if (!wcsRestore (crval, crpix, cdelt, pc, ctype, 
                        subrec.asRecord("wcs"))) return 0;

// Make SC, will be in Hz regardless of units

       Quantum<double> qcrval(crval, qUnit);       
       Quantum<double> qcdelt(cdelt, qUnit);
       pSpectral = new SpectralCoordinate (freqSys, qcrval, qcdelt, 
                                           crpix, qRestFreq);
       AlwaysAssert(pSpectral, AipsError);
//
       Matrix<double> xform(1,1);
       xform = pc;
       pSpectral->setLinearTransform(xform);
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

// Wavelength conversion
    
    String waveUnit("mm"); 
    if (subrec.isDefined("waveUnit")) {                      // optional
       formatUnit = subrec.asString("waveUnit");
    }
    pSpectral->setWavelengthUnit(waveUnit);

    SpectralCoordinate::SpecType spcType = SpectralCoordinate::FREQ;
    if (subrec.isDefined("nativeType")) {                    // optional
   	 spcType = static_cast<SpectralCoordinate::SpecType>(subrec.asInt("nativeType"));
    }
    pSpectral->setNativeType(spcType);


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
                                                 double restfreq)
//
// Rest frequency handling
//
{

// Multiple rest frequencies were added after initial deployment

    if (subrec.isDefined("restfreqs")) {                   // optional
      Vector<double> restFreqs(subrec.toArrayDouble("restfreqs"));

// Old images might have a negative restfreq. Don't propagate that

        for (uint32_t i=0; i<restFreqs.nelements(); i++) {
           restFreqs(i) = max(0.0,restFreqs(i));
        }
//
       pSpectral->setRestFrequencies(restFreqs, 0, false);
       pSpectral->selectRestFrequency(restfreq);
    } else {
       pSpectral->setRestFrequency(restfreq, false);           // Updates Velocity Machine
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


void SpectralCoordinate::toFITS(RecordInterface &header, uint32_t whichAxis, 
				LogIO &logger, bool oneRelative, 
				bool preferVelocity,  bool opticalVelDef, 
				bool preferWavelength, bool airWaveDef) const
{
    const double offset(1.0*int32_t(oneRelative == true));

    logger << LogOrigin("SpectralCoordinate", "toFITS", WHERE);

    if(preferVelocity && preferWavelength){
      throw AipsError("Cannot export spectral axis for velocity AND wavelength. You have to choose one.");
    }

    // Verify that the required headers exist and are the right type
    AlwaysAssert(header.isDefined("ctype") && 
                 header.dataType("ctype") == TpArrayString &&
		 header.shape("ctype").nelements() == 1 &&
                 header.shape("ctype")(0) > int32_t(whichAxis), AipsError);
    AlwaysAssert(header.isDefined("crval") && 
                 header.dataType("crval") == TpArrayDouble &&
		 header.shape("crval").nelements() == 1 && 
                 header.shape("crval")(0) > int32_t(whichAxis), AipsError);
    AlwaysAssert(header.isDefined("crpix") && 
		 header.dataType("crpix") == TpArrayDouble &&
		 header.shape("crpix").nelements() == 1 &&
		 header.shape("crpix")(0) > int32_t(whichAxis), AipsError);
    AlwaysAssert(header.isDefined("cdelt") && 
		 header.dataType("cdelt") == TpArrayDouble &&
		 header.shape("cdelt").nelements() == 1 &&
		 header.shape("cdelt")(0) > int32_t(whichAxis), AipsError);

    Vector<String> ctype, cunit;

    header.get("ctype", ctype);
    Vector<double> crval(header.toArrayDouble("crval"));
    Vector<double> crpix(header.toArrayDouble("crpix"));
    Vector<double> cdelt(header.toArrayDouble("cdelt"));

    if (header.isDefined("cunit")) {
	AlwaysAssert(header.dataType("cunit") == TpArrayString &&
		     header.shape("cunit").nelements() == 1 &&
		     header.shape("cunit")(0) > int32_t(whichAxis), AipsError);
	header.get("cunit", cunit);
    }

    String Ctype, Cunit, Specsys;
    double Crval, Cdelt, Crpix, Altrval, Altrpix;
    int32_t Velref;
    bool HaveAlt;
    double Restfreq = Quantity(restfreqs_p(restfreqIdx_p),  // Canonicalize
			       worldAxisUnits()(0)).getBaseValue();
    double RefFreq = Quantity(referenceValue()(0), 
			      worldAxisUnits()(0)).getBaseValue();
    double FreqInc = Quantity(increment()(0), 
			      worldAxisUnits()(0)).getBaseValue();
    double RefPix = referencePixel()(0) + offset;

    double linTrans = linearTransform()(0,0); // always one-dimensional

    MDoppler::Types VelPreference = opticalVelDef ? MDoppler::OPTICAL : MDoppler::RADIO;

    // Determine possible changes to RefFreq etc. and check if we are linear in the preferred quantity.
    // If not, give a warning.

    // Fill pixel numbers
    Vector<double> pixel;    

    if (pixelValues().nelements() > 1) { // tabular axis
      pixel.assign(pixelValues());
      Vector<double> vf0, vf1;
      if(!toWorld(vf0, Vector<double>(1,pixel(0))) || !toWorld(vf1, Vector<double>(1,pixel(1)))){
	logger << LogIO::SEVERE << "Error calculating deviations from linear" 
	       << errorMessage() << LogIO::POST;
      }
      convertFrom(vf0);
      convertFrom(vf1);
      RefFreq = vf0(0); // value in Hz in native reference frame
      FreqInc = vf1(0) - RefFreq; // dto.
      RefPix = pixel(0) + offset;
    }
    else{
      uint32_t nEl = 0;
      if(header.isDefined("naxis") && 
	 header.dataType("naxis") == TpArrayInt &&
	 header.shape("naxis").nelements() == 1 &&
	 header.shape("naxis")(0) > int32_t(whichAxis)){
 	Vector<int32_t> naxis(header.toArrayInt("naxis"));
	nEl = naxis(whichAxis);
      }
      pixel.resize(nEl);
      for(uint32_t i=0; i<nEl; i++){
	pixel(i) = double(i); 
      }
    }

    double maxDeviation = 0.0;
    double gridSpacing = 1E99;
    Vector<double> vfx;
    double fx;
    for (uint32_t i=0; i<pixel.nelements(); i++) {
      bool ok = toWorld(vfx,  Vector<double>(1,pixel(i)));
      if (!ok) {
	logger << LogIO::SEVERE << "Error calculating deviations "
	  "from linear" << errorMessage() << LogIO::POST;
	break;
      }
      convertFrom(vfx); // to native reference frame 
      fx = vfx(0);

      // frequencies
      double actual = fx; // value in Hz
      double linear = RefFreq + FreqInc*(linTrans*pixel(i)-(RefPix-offset)); // also in Hz
      gridSpacing = FreqInc;      

      if(preferWavelength){ // check if we are linear in wavelength
	if(actual>0. && RefFreq>0. && (RefFreq+FreqInc)>0.){
	  actual = C::c/actual;
	  linear = C::c/RefFreq + (C::c/(RefFreq+FreqInc) - C::c/RefFreq)*(linTrans*pixel(i) - (RefPix-offset));
	  gridSpacing = -(C::c/(RefFreq+FreqInc) - C::c/RefFreq);
	}
	else{
	  logger << LogIO::SEVERE << "Zero or negative frequency." << LogIO::POST;
	  break;
	}
      }
      else if(preferVelocity && opticalVelDef){ // optical velocity
	if(actual>0. && RefFreq>0.){
	  double refVelocity = -C::c * (1.0 - Restfreq / RefFreq);
	  double velocityIncrement = -C::c * (1.0 - Restfreq / (RefFreq + FreqInc)) - refVelocity;
	  actual = -C::c * (1.0 - Restfreq / actual); 
	  linear = refVelocity + velocityIncrement * (linTrans*pixel(i) - (RefPix-offset));
	  gridSpacing = -velocityIncrement;
	}
	else{
	  logger << LogIO::SEVERE << "Zero or negative frequency."  << LogIO::POST;
	  break;
	}
      }
      //else {} // radio velocity or frequency, both linear in frequency
	  
      if(maxDeviation<abs(actual-linear)){
	maxDeviation = abs(actual-linear);
      }

    } // end for
    if (maxDeviation>0. && gridSpacing>0. && maxDeviation/gridSpacing>1E-3) {
      string sUnit = "Hz";
      if(preferWavelength){
	sUnit = "m";
      }
      else if(preferVelocity && opticalVelDef){
	sUnit = "m/s";
      }
      logger << LogIO::WARN << "Spectral axis is non-linear in the requested output quantity" << endl
	     << "but CASA can presently only write linear axes to FITS." << endl
	     << "In this image, the maximum deviation from linearity is " << maxDeviation << " " << sUnit << endl 
	     << " or " << maxDeviation/gridSpacing*100. << "% of the grid spacing." << LogIO::POST;
    }


    AlwaysAssert(FITSSpectralUtil::toFITSHeader(Ctype, Crval, Cdelt, Crpix, Cunit, HaveAlt, Altrval,
						Altrpix, Velref, Restfreq, Specsys, 
						logger,
						RefFreq, RefPix,
  					        FreqInc, type_p, preferVelocity,
						VelPreference, preferWavelength, airWaveDef), AipsError);

    ctype(whichAxis) = Ctype;
    crval(whichAxis) = Crval;
    crpix(whichAxis) = Crpix;
    cdelt(whichAxis) = Cdelt;
    if (cunit.nelements() > 0) {
	if (Ctype.contains("VELO") || Ctype.contains("FELO")|| 
	    Ctype.contains("VRAD")|| Ctype.contains("VOPT")) {
	    cunit(whichAxis) = "m/s";
	} else if (Ctype.contains("FREQ")) {
	    cunit(whichAxis) = "Hz";
	} else if (Ctype.contains("WAVE")|| Ctype.contains("AWAV")) {
	    cunit(whichAxis) = Cunit;
	} else {
	    AlwaysAssert(0, AipsError); // NOTREACHED
	}
    }

    if (Restfreq > 0) {
	header.define("restfrq", Restfreq); // FITS standard v3.0 is RESTFRQ, no longer RESTFREQ
	header.setComment("restfrq", "Rest Frequency (Hz)"); 
	if(!Specsys.empty()){
	  header.define("specsys", Specsys);
	  header.setComment("specsys", "Spectral reference frame"); 
	}
    }
    if (HaveAlt && !preferWavelength) { // alternate representation not valid for ctype WAVE
	header.define("altrval", Altrval);
	header.setComment("altrval", "Alternate frequency reference value");
	header.define("altrpix", Altrpix);
	header.setComment("altrpix", "Alternate frequency reference pixel");
	header.define("velref", Velref);
	header.setComment("velref", "1 LSR, 2 HEL, 3 OBS, +256 Radio");
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


Coordinate* SpectralCoordinate::makeFourierCoordinate (const Vector<bool>& axes,
                                                       const Vector<int32_t>& shape) const
// 
// axes says which axes in the coordinate are to be transformed
// shape is the shape of the image for all axes in this coordinate
//
{
   if (_tabular.ptr()) {
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
    
   Vector<double> crval(sc.referenceValue().copy());
   Vector<double> crpix(sc.referencePixel().copy());
   Vector<double> cdelt(sc.increment().copy());   
   crval[0] = 0.0; 
   cdelt[0] = 1.0 / (shape(0) * cdelt(0));
   crpix[0] = int32_t(shape(0)/2);
    
// Now create the new output LinearCoordinate

    Matrix<double> pc(1, 1);
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


bool SpectralCoordinate::setFormatUnit (const String& unit)
{
   const Unit unitHZ(String("Hz"));      
   const Unit unitKMS(String("km/s"));      
   const Unit unitM(String("m"));      
   Unit t(unit);
   if (t != unitHZ && t != unitKMS && t != unitM) {
      return false;
   }
//
   formatUnit_p = unit;
   return true;
}

String SpectralCoordinate::format (String& units,
                                   Coordinate::formatType format,
                                   double worldValue,
                                   uint32_t worldAxis,
                                   bool isAbsolute,
                                   bool showAsAbsolute,
                                   int32_t precision, bool usePrecForMixed) const
{
   AlwaysAssert(worldAxis < nWorldAxes(), AipsError);
    
// Check format
                                   
   Coordinate::formatType form = format;
   checkFormat (form, showAsAbsolute);
                                   
// Set default precision
                                   
   int32_t prec = precision;
   if (prec < 0) getPrecision(prec, form, showAsAbsolute, -1, -1, -1);

// If units are empty use formatUnit_p unit.   If that's
// empty use natuive world unit.
// If given units are not consistent with native units
// then see if they are velocity.  If so, convert to
// desired units.
  
   static const Unit unitsHZ(String("Hz"));      
   static const Unit unitsKMS_c(String("km/s"));      
   static const Unit unitsM_c(String("m"));      
   static Quantum<double> qVel;
   //   static Quantum<double> qFreq;
   static Vector<double> vWave;
   static Vector<double> world;

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
                                     isAbsolute, showAsAbsolute, precision, usePrecForMixed);
   } 
   else { // unit not frequency

   	if (unit == unitsKMS_c) { // unit consistent with velocty

   		world.resize(nWorldAxes());

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

   			static Vector<double> vel(2), freq2(2);
   			freq2(0) = referenceValue()(worldAxis);
   			freq2(1) = worldValue;
   			if (!frequencyToVelocity(vel, freq2)) {
   				theString = "Fail";
   				return theString;
   			}
	 
// Convert from velUnit_p (used in f2v) to desired unit
	 
   			Quantum<double> t(vel[1]-vel[0], Unit(velUnit_p)); // rel=abs-ref
   			worldValue = t.getValue(unit);
   		}

   	}
   	else{ // unit should be wavelength

   		if (unit != unitsM_c) {
   			throw(AipsError("Requested units must be consistent with km/s, m, or Hz for a SpectralCoordinate"));
   		}
       
// Requested unit is consistent with m

   		world.resize(nWorldAxes());
   		// new start
   		vWave.resize(nWorldAxes());
   		// new end

// We must convert to absolute first (regardless of how we want
// to see the value) as we are formatting in wavelength units

   		if (!isAbsolute) {
   			world = 0.0;
   			world(worldAxis) = worldValue;
   			makeWorldAbsolute(world);
   			worldValue = world(worldAxis);
   		}

   		// new start
		world = 0.0;
		vWave = 0.0;
		world(worldAxis) = worldValue;
		if (nativeType_p == SpectralCoordinate::AWAV)
		  frequencyToAirWavelength(vWave, world);
		else
		  frequencyToWavelength(vWave, world);
		Quantity tmpI=Quantity(vWave(worldAxis), waveUnit_p);
		worldValue = tmpI.get(unit).getValue();
   		// new end

   		if (!showAsAbsolute) {

		  // new start
		  // Find relative coordinate in m consistent units
		  world(worldAxis) = referenceValue()(worldAxis);
		  if (nativeType_p == SpectralCoordinate::AWAV)
		    frequencyToAirWavelength(vWave, world);
		  else
		    frequencyToWavelength(vWave, world);
		  Quantity tmpI=Quantity(vWave(worldAxis), waveUnit_p);
		  worldValue = worldValue - tmpI.get(unit).getValue(); // subtract reference
		  // new end

   		}

     } // end if
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
                                     const bool ) const
{  

// Absolute or offset is irrelevant

   if (format != Coordinate::SCIENTIFIC &&
       format != Coordinate::FIXED) format = Coordinate::DEFAULT;
//
   if (format == Coordinate::DEFAULT) format = Coordinate::MIXED;
}

const Vector<double>& SpectralCoordinate::restFrequencies() const
{
   return restfreqs_p;
}



String SpectralCoordinate::formatRestFrequencies () const
{  
   const Vector<double>& rfs = restFrequencies();
   double rf = restFrequency();
   String unit = worldAxisUnits()(0);
   const uint32_t n = rfs.nelements();
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
         uint32_t j = 0;
         for (uint32_t i=0; i<n; i++) {
            if (!casacore::near(rfs(i), rf)) {
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



void SpectralCoordinate::makeWCS(::wcsprm& wcs, const String& ctype, double refPix,
                                 double refVal, double inc,
                                 double pc, double restFreq)
{
    wcs.flag = -1;
    init_wcs(wcs, 1);

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
    set_wcs(wcs);
}



bool SpectralCoordinate::wcsSave (RecordInterface& rec, const ::wcsprm& wcs,  
                                  const String& fieldName) const
//
// Save the things that come out of the wcs structure
//
{
    bool ok = (!rec.isDefined(fieldName));
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

bool SpectralCoordinate::wcsRestore (double& crval, double& crpix, double& cdelt,
                                     double& pc, String& ctype,
                                     const RecordInterface& rec)
{
   if (rec.isDefined("crval")) {
      rec.get("crval", crval);
   } else {
      return false;
   }
//
   if (rec.isDefined("crpix")) {
      rec.get("crpix", crpix);
   } else {
      return false;
   }
//
   if (rec.isDefined("cdelt")) {
      rec.get("cdelt", cdelt);
   } else {
      return false;
   }
//
   if (rec.isDefined("pc")) {
      rec.get("pc", pc);
   } else {
      return false;
   }
//
   if (rec.isDefined("ctype")) {
      rec.get("ctype", ctype);
   } else {
      return false;
   }
//
   return true;
}

void SpectralCoordinate::toCurrent(Vector<double>& value) const
{
    value /= to_hz_p;
}


void SpectralCoordinate::fromCurrent(Vector<double>& value) const
{
    value *= to_hz_p;
}


const Vector<double> SpectralCoordinate::toCurrentFactors () const
{
    Vector<double> t(1);
    t[0] = 1.0 / to_hz_p;
    return t;
}





void SpectralCoordinate::copy (const SpectralCoordinate &other) {
   type_p = other.type_p;
   to_hz_p = other.to_hz_p;
   to_m_p = other.to_m_p;
   restfreqs_p.resize(0);
   restfreqs_p = other.restfreqs_p;
   restfreqIdx_p = other.restfreqIdx_p;

// Clean up first


   if (wcs_p.flag != -1) {    
       wcsfree (&wcs_p);
   }

// Copy TabularCoordinate or wcs structure. Only one of the two
// is allocated at any given time.

    if (other._tabular.ptr()) {
       _tabular.reset(new TabularCoordinate(*(other._tabular)));
    }
    else {
    	if (_tabular.ptr()) {
    		_tabular.reset(0);
    	}
       copy_wcs(other.wcs_p, wcs_p);
       set_wcs(wcs_p);
    }

    conversionType_p = other.conversionType_p;
    direction_p = other.direction_p;
    position_p = other.position_p;
    epoch_p = other.epoch_p;
    velType_p = other.velType_p;
    velUnit_p = other.velUnit_p;
    waveUnit_p = other.waveUnit_p;
    nativeType_p = other.nativeType_p;
    unit_p = other.unit_p;
    axisName_p = other.axisName_p;
    formatUnit_p = other.formatUnit_p;

// Machines

    makeConversionMachines(type_p, conversionType_p, epoch_p, position_p,
                           direction_p);
    deleteVelocityMachine();
    if (other.pVelocityMachine_p) {
       pVelocityMachine_p = new VelocityMachine(*(other.pVelocityMachine_p));
    }
}


void SpectralCoordinate::_setTabulatedFrequencies(const Vector<double>& freqs) {
	Vector<double> channels(freqs.nelements());
	indgen(channels);
	_tabular.reset(new TabularCoordinate(channels, freqs, "Hz", "Frequency"));
}

ostream& SpectralCoordinate::print(ostream& os) const {
    os << "tabular " << _tabular.ptr() << endl;
    os << "to_hz_p " <<  to_hz_p << endl;
    os << "to_m_p " << to_m_p << endl;
    os << "type_p " << MFrequency::showType(type_p) << endl;
    os << "conversionType_p " << MFrequency::showType(conversionType_p) << endl;
    os << "restfreqs_p " << restfreqs_p << endl;
    os << "restfreqIdx_p " << restfreqIdx_p << endl;
    os << "pConversionMachineTo_p " << pConversionMachineTo_p << endl;
    os << "pConversionMachineFrom_p " << pConversionMachineFrom_p << endl;
    os << "pVelocityMachine_p " <<  pVelocityMachine_p << endl;
    os << "velType_p " << velType_p << endl;
    os << "velUnit_p " << velUnit_p << endl;
    os << "waveUnit_p " << waveUnit_p << endl;
    os << "nativeType_p " << nativeType_p << endl;
    os << "unit_p " << unit_p.getName() << endl;
    os << "increment " << increment() << endl;
    os << "axisName_p " << axisName_p << endl;
    os << "formatUnit_p " << formatUnit_p << endl;
    os << "direction_p " <<  direction_p << endl;
    os << "position_p " << position_p << endl;
    os << "epoch_p " << epoch_p << endl;
    return os;

}

bool SpectralCoordinate::isTabular() const {
	return _tabular.ptr();
}

ostream &operator<<(ostream &os, const SpectralCoordinate& spcoord) {
	return spcoord.print(os);
}


} //# NAMESPACE CASACORE - END

