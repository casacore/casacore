//# SkyCompRep.cc:  this defines SkyCompRep
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002
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
//# $Id$

#include <components/ComponentModels/ComponentShape.h>
#include <components/ComponentModels/ComponentType.h>
#include <components/ComponentModels/ConstantSpectrum.h>
#include <components/ComponentModels/PointShape.h>
#include <components/ComponentModels/GaussianBeam.h>
#include <components/ComponentModels/GaussianShape.h>
#include <components/ComponentModels/DiskShape.h>
#include <components/ComponentModels/LimbDarkenedDiskShape.h>
#include <components/ComponentModels/SkyCompRep.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/Cube.h>
#include <casa/Arrays/Matrix.h>
#include <casa/Arrays/MatrixIter.h>
#include <casa/Arrays/Vector.h>
#include <casa/Containers/Record.h>
#include <casa/Containers/RecordFieldId.h>
#include <casa/Containers/RecordInterface.h>
#include <coordinates/Coordinates/CoordinateSystem.h>
#include <coordinates/Coordinates/DirectionCoordinate.h>
#include <coordinates/Coordinates/SpectralCoordinate.h>
#include <casa/Exceptions/Error.h>
#include <casa/Logging/LogIO.h>
#include <casa/Logging/LogOrigin.h>
#include <casa/BasicMath/Math.h>
#include <casa/BasicSL/Complex.h>
#include <measures/Measures/MDirection.h>
#include <measures/Measures/MFrequency.h>
#include <measures/Measures/Stokes.h>
#include <casa/Quanta/MVAngle.h>
#include <casa/Quanta/Quantum.h>
#include <casa/Quanta/Unit.h>
#include <casa/Quanta/UnitMap.h>
#include <casa/Utilities/Assert.h>
#include <casa/Utilities/DataType.h>
#include <components/ComponentModels/SpectralModel.h>
#include <casa/iostream.h>

namespace casa { //# NAMESPACE CASA - BEGIN

SkyCompRep::SkyCompRep() 
  :itsShapePtr(new PointShape),
   itsSpectrumPtr(new ConstantSpectrum),
   itsFlux(),
   itsLabel(),
   itsOptParms()

{
	AlwaysAssert(ok(), AipsError);

}

SkyCompRep::SkyCompRep(const ComponentType::Shape& shape)
  :itsShapePtr(ComponentType::construct(shape)),
   itsSpectrumPtr(new ConstantSpectrum),
   itsFlux(),
   itsLabel(),
   itsOptParms()
{
	AlwaysAssert(ok(), AipsError);
}

SkyCompRep::SkyCompRep(const ComponentType::Shape& shape,
		       const ComponentType::SpectralShape& spectrum)
  :itsShapePtr(ComponentType::construct(shape)),
   itsSpectrumPtr(ComponentType::construct(spectrum)),
   itsFlux(),
   itsLabel(),
   itsOptParms()
{
	AlwaysAssert(ok(), AipsError);
}

SkyCompRep::SkyCompRep(const Flux<Double>& flux,
		       const ComponentShape& shape, 
		       const SpectralModel& spectrum)
  :itsShapePtr(shape.clone()),
   itsSpectrumPtr(spectrum.clone()),
   itsFlux(flux.copy()),
   itsLabel(),
   itsOptParms()
{
  AlwaysAssert(ok(), AipsError);
}

SkyCompRep::SkyCompRep(const SkyCompRep& other) 
  :SkyCompBase(),
   itsShapePtr(other.itsShapePtr->clone()),
   itsSpectrumPtr(other.itsSpectrumPtr->clone()),
   itsFlux(other.itsFlux.copy()),
   itsLabel(other.itsLabel),
   itsOptParms(other.itsOptParms)
{
  AlwaysAssert(ok(), AipsError);
}

SkyCompRep::~SkyCompRep() {}

SkyCompRep& SkyCompRep::operator=(const SkyCompRep& other) {
  if (this != &other) {
    itsShapePtr = other.itsShapePtr->clone();
    itsSpectrumPtr = other.itsSpectrumPtr->clone();
    itsFlux = other.itsFlux.copy();
    itsLabel = other.itsLabel;
    itsOptParms = other.itsOptParms;
  }
  AlwaysAssert(ok(), AipsError);
  return *this;
}

const Flux<Double>& SkyCompRep::flux() const {
  return itsFlux;
}

Flux<Double>& SkyCompRep::flux() {
  return itsFlux;
}

const ComponentShape& SkyCompRep::shape() const {
  DebugAssert(ok(), AipsError);
  return *itsShapePtr;
}

ComponentShape& SkyCompRep::shape() {
  return *itsShapePtr;
}

void SkyCompRep::setShape(const ComponentShape& newShape) {
  itsShapePtr = newShape.clone();
}

SpectralModel& SkyCompRep::spectrum() {
  return *itsSpectrumPtr;
}

const SpectralModel& SkyCompRep::spectrum() const {
  return *itsSpectrumPtr;
}

void SkyCompRep::setSpectrum(const SpectralModel& newSpectrum) {
  itsSpectrumPtr = newSpectrum.clone();
}

String& SkyCompRep::label() {
  return itsLabel;
}

const String& SkyCompRep::label() const {
  return itsLabel;
}

Vector<Double>& SkyCompRep::optionalParameters() {
  return itsOptParms;
}

const Vector<Double>& SkyCompRep::optionalParameters() const {
  return itsOptParms;
}

Bool SkyCompRep::isPhysical() const {
  Flux<Double> compFlux = flux().copy();
  compFlux.convertPol(ComponentType::STOKES);
  const Vector<DComplex>& iquv = compFlux.value();
  const DComplex& i = iquv(0);
  const DComplex& q = iquv(1);
  const DComplex& u = iquv(2);
  const DComplex& v = iquv(3);
  if (!nearAbs(i.imag(), 0.0) || 
      !nearAbs(q.imag(), 0.0) || 
      !nearAbs(u.imag(), 0.0) || 
      !nearAbs(v.imag(), 0.0) ) {
    return False;
  }
  if (square(i.real()) < 
      square(q.real()) + square(u.real()) + square(v.real()) ) {
    return False;
  }
  return True;
}

Flux<Double> SkyCompRep::sample(const MDirection& direction, 
				const MVAngle& pixelLatSize,
				const MVAngle& pixelLongSize,
				const MFrequency& centerFrequency) const {
  Double scale = itsShapePtr->sample(direction, pixelLatSize, pixelLongSize);
  //scale *= itsSpectrumPtr->sample(centerFrequency);
  Flux<Double> flux = itsFlux.copy();
  Vector<Double> iquv(4);
  flux.value(iquv);
  itsSpectrumPtr->sampleStokes(centerFrequency, iquv);
  flux.setValue(iquv);
  flux.convertPol(itsFlux.pol());
  flux.scaleValue(scale, scale, scale, scale);
  return flux;
}

void SkyCompRep::sample(Cube<Double>& samples, const Unit& reqUnit,
			const Vector<MVDirection>& directions, 
			const MeasRef<MDirection>& dirRef, 
			const MVAngle& pixelLatSize, 
			const MVAngle& pixelLongSize, 
			const Vector<MVFrequency>& frequencies,
			const MeasRef<MFrequency>& freqRef) const {
  const uInt nDirSamples = directions.nelements();
  const uInt nFreqSamples = frequencies.nelements();
  Flux<Double> f = itsFlux.copy();
  f.convertUnit(reqUnit);
  Vector<Double> fluxVal(4);
  f.value(fluxVal);
  /*const Double i = fluxVal(0);
  const Double q = fluxVal(1);
  const Double u = fluxVal(2);
  const Double v = fluxVal(3);
  */

  Vector<Double> dirScales(nDirSamples);
  itsShapePtr->sample(dirScales, directions, dirRef,
 		      pixelLatSize, pixelLongSize);

  Vector<Vector<Double> > freqIQUV(nFreqSamples);
  freqIQUV.set(fluxVal);

  //itsSpectrumPtr->sample(freqScales, frequencies, freqRef);
  itsSpectrumPtr->sampleStokes(freqIQUV, frequencies, freqRef); 
  for (uInt d = 0; d < nDirSamples; d++) {
	  const Double thisDirScale = dirScales(d);
	  if (thisDirScale != 0) {
		  for (uInt f = 0; f < nFreqSamples; f++) {
		    //const Double thisScale = thisDirScale* freqScales(f);
		    for (uInt stok=0; stok <4; ++stok){
		      samples(stok, d, f) += thisDirScale * freqIQUV(f)(stok);
		    }
		  }
	  }
  }
}

Flux<Double> SkyCompRep::visibility(const Vector<Double>& uvw,
				    const Double& frequency) const {
  Flux<Double> flux = itsFlux.copy();
  Double scale = itsShapePtr->visibility(uvw, frequency).real();
  MFrequency freq(Quantity(frequency, "Hz"));
  //scale *= itsSpectrumPtr->sample(freq);
  Vector<Double> iquv(4);
  flux.value(iquv);
  itsSpectrumPtr->sampleStokes(freq, iquv);
  flux.setValue(iquv);
  flux.convertPol(itsFlux.pol());
  flux.scaleValue(scale, scale, scale, scale);
  return flux;
}

void SkyCompRep::visibility(Cube<DComplex>& visibilities,
			    const Matrix<Double>& uvws,
			    const Vector<Double>& frequencies) const {
  const uInt nFreq = frequencies.nelements();
  const uInt nVis = uvws.ncolumn();

  Vector<Double> uvw(3);
  //Block<DComplex> flux(4);
  Vector<Double> iquv(4);
  Flux<Double> flux=itsFlux.copy();
  flux.value(iquv);
    /*for (uInt p = 0; p < 4; p++) {
    flux[p] = itsFlux.value(p);
  }
    */

  Vector<Vector<Double> >fIQUV(frequencies.nelements());
  fIQUV.set(iquv);
  Vector<MVFrequency> mvFreq(frequencies.nelements());
  for (uInt f = 0; f < nFreq; f++) {
    mvFreq(f)=MVFrequency(frequencies(f));
  }
  
  // It's not clear how we would get the right information down here.
  // In any event, it's probably not a pressing concern for most cases.

  //Indeed ...write something complex and make believe that 
  // that transformation from different frames can happen and let it bite when some 
  // poor sucker try to use it
  //At least for now making it that the frequency is expected to be in the frame of 
  // the component
  MeasRef<MFrequency> measRef(itsSpectrumPtr->refFrequency().getRef()); 
  //itsSpectrumPtr->sample(fscale, mvFreq, measRef);
  itsSpectrumPtr->sampleStokes(fIQUV, mvFreq, measRef);
  Vector<Flux<Double> > stokesFlux(nFreq);
  for (uInt f=0; f < nFreq; ++f){
    stokesFlux(f)=Flux<Double>(fIQUV(f));
    stokesFlux(f).convertPol(itsFlux.pol());
  }
  Matrix<DComplex> scales(nVis, nFreq);
  itsShapePtr->visibility(scales, uvws, frequencies);
  /*Matrix<DComplex> scales2(nFreq, nVis);
  for(uInt k=0; k < nFreq; ++k ){
    for (uInt j=0; j < nVis; ++j){
      scales2(k,j)=scales(j,k)*fscale(k);
    }
  }
  for (uInt p = 0; p < 4;  ++p) {
    visibilities.yzPlane(p) = flux[p]*scales2;
  }
  */
  for (uInt v = 0; v < nVis; ++v) {
    for (uInt f=0; f < nFreq; ++f ){
      for (uInt p = 0; p < 4;  ++p) {
	visibilities(p, f, v)=scales(v, f)*stokesFlux[f].value(p);
      }
    }
  }


 /*
  for (uInt v = 0; v < nVis; v++) {
    uvw=uvws.column(v);
    Matrix<Complex> plane;
    plane.reference(visibilities.xyPlane(v));
    // Scale by the specified frequency behaviour 
    for (uInt f = 0; f < nFreq; f++) {
      Double scale = itsShapePtr->visibility(uvw, frequencies(f)).real();
      scale *= fscale[f];
      for (uInt p = 0; p < 4; p++) {
	visibilities(p, f, v) = flux[p] * scale;
      }
    }
  }
    */


}

Bool SkyCompRep::fromRecord(String& errorMessage,
			    const RecordInterface& record) {
  {
    const String fluxString("flux");
    if (record.isDefined(fluxString)) {
      const RecordFieldId flux(fluxString);
      if (record.dataType(flux) != TpRecord) {
	errorMessage += "The 'flux' field must be a record\n";
	return False;
      }
      const Record& fluxRec = record.asRecord(flux);
      if (!itsFlux.fromRecord(errorMessage, fluxRec)) {
	errorMessage += "Problem parsing the 'flux' field\n";
	return False;
      }
    } else {
      LogIO logErr(LogOrigin("SkyCompRep", "fromRecord()"));
      logErr << LogIO::WARN 
	     << "The component does not have a 'flux' field." << endl
	     << "The default is 1.0 Jy in I and 0.0 in Q, U & V"
	     << LogIO::POST;
      itsFlux = Flux<Double>(1);
    }
  }
  {
    const String shapeString("shape");
    if (record.isDefined(shapeString)) {
      const RecordFieldId shape(shapeString);
      if (record.dataType(shape) != TpRecord) {
	errorMessage += "\nThe 'shape' field must be a record";
	return False;
      }      
      const Record& shapeRec = record.asRecord(shape);
      const ComponentType::Shape recType = 
	ComponentShape::getType(errorMessage, shapeRec);
      if (recType >= ComponentType::UNKNOWN_SHAPE) {
	errorMessage += String("Cannot create a component with a '" +
			       ComponentType::name(recType) + "' shape\n");
	return False;
      }
      if (recType != itsShapePtr->type()) {
	ComponentShape* newShape = ComponentType::construct(recType);
	AlwaysAssert(newShape != 0, AipsError);
	setShape(*newShape);
	delete newShape;
      }
      if (!itsShapePtr->fromRecord(errorMessage, shapeRec)) {
	errorMessage += "Problem parsing the 'shape' field\n";
	return False;
      }
    } else {
      LogIO logErr(LogOrigin("SkyCompRep", "fromRecord()"));
      logErr << LogIO::WARN 
	     << "The component does not have a 'shape' field." << endl
	     << "The default is a point component at the J2000 north pole"
	     << LogIO::POST;
      const Unit deg("deg");
      itsShapePtr = new PointShape(MDirection(Quantum<Double>(0.0, deg),
					      Quantum<Double>(90.0, deg),
					      MDirection::J2000));
    }
  }
  {
    const String spectrumString("spectrum");
    if (record.isDefined(spectrumString)) {
      const RecordFieldId spectrum(spectrumString);
      if (record.dataType(spectrum) != TpRecord) {
	errorMessage += "\nThe 'spectrum' field must be a record";
	return False;
      }      
      const Record& spectrumRec = record.asRecord(spectrum);
      const ComponentType::SpectralShape recType = 
	SpectralModel::getType(errorMessage, spectrumRec);
      if (recType >= ComponentType::UNKNOWN_SPECTRAL_SHAPE) {
	errorMessage += String("Cannot create a component with a '" +
			       ComponentType::name(recType) + "' spectrum\n");
	return False;
      }
      if (recType != itsSpectrumPtr->type()) {
	SpectralModel* newSpectrum = ComponentType::construct(recType);
	AlwaysAssert(newSpectrum != 0, AipsError);
	setSpectrum(*newSpectrum);
	delete newSpectrum;
      }
      if (!itsSpectrumPtr->fromRecord(errorMessage, spectrumRec)) {
	return False;
      }
    } else {
      LogIO logErr(LogOrigin("SkyCompRep", "fromRecord()"));
      logErr << LogIO::WARN 
	     << "The component does not have a 'spectrum' field." << endl
	     << "The default is a constant spectrum"
	     << LogIO::POST;
      itsSpectrumPtr = new ConstantSpectrum;
    }
  }
  {
    const String labelString("label");
    if (record.isDefined(labelString)) {
      const RecordFieldId label(labelString);
      if (record.dataType(label) != TpString) {
	errorMessage += "\nThe 'label' field must be a string";
	return False;
      }      
      if (record.shape(label) != IPosition(1,1)) {
	errorMessage += "\nThe 'label' field must have only 1 element";
	return False;
      } 
      itsLabel = record.asString(label);
    }
  }
  { 
    const String optParmString("optionalParameters");
    if (record.isDefined(optParmString)) {
      const RecordFieldId optionalParameters(optParmString);
      if (record.dataType(optionalParameters) != TpArrayDouble) {
        errorMessage += "\nThe 'optionalParameters' field must be a Double Array";
        return False;
      } 
      itsOptParms = record.asArrayDouble(optionalParameters);
    }
  }
  return True;
}

Bool SkyCompRep::toRecord(String& errorMessage, 
			  RecordInterface& record) const {
  {
    Record fluxRec;
    if (!itsFlux.toRecord(errorMessage, fluxRec)) {
      return False;
    }
    record.defineRecord(RecordFieldId("flux"), fluxRec);
  }
  {
    Record shapeRec;
    if (!itsShapePtr->toRecord(errorMessage, shapeRec)) {
      return False;
    }
    record.defineRecord(RecordFieldId("shape"), shapeRec);
  }
  {
    Record spectrumRec;
    if (!itsSpectrumPtr->toRecord(errorMessage, spectrumRec)) {
      return False;
    }
    record.defineRecord(RecordFieldId("spectrum"), spectrumRec);
  }
  {
    Record optParmRec;
    if (!itsOptParms.empty()) {
      record.define(RecordFieldId("optionalParameters"), itsOptParms);
    }
  }
  record.define(RecordFieldId("label"), itsLabel);
  return True;
}

Bool SkyCompRep::ok() const {
  if (itsShapePtr.null()) {
    LogIO logErr(LogOrigin("SkyCompRep", "ok()"));
    logErr << LogIO::SEVERE << "Shape pointer is null"
           << LogIO::POST;
    return False;
  }
  if (itsShapePtr->ok() == False) {
    LogIO logErr(LogOrigin("SkyCompRep", "ok()"));
    logErr << LogIO::SEVERE << "The component shape is not ok"
           << LogIO::POST;
    return False;
  }
  if (itsSpectrumPtr.null()) {
    LogIO logErr(LogOrigin("SkyCompRep", "ok()"));
    logErr << LogIO::SEVERE << "Spectrum pointer is null"
           << LogIO::POST;
    return False;
  }
  if (itsSpectrumPtr->ok() == False) {
    LogIO logErr(LogOrigin("SkyCompRep", "ok()"));
    logErr << LogIO::SEVERE << "The component spectrum is not ok"
           << LogIO::POST;
    return False;
  }
  return True;
}



void SkyCompRep::fromPixel (
		Double& facToJy, const Vector<Double>& parameters,
		const Unit& brightnessUnitIn,
        const GaussianBeam& restoringBeam,
        const CoordinateSystem& cSys,
        ComponentType::Shape componentShape,
        Stokes::StokesTypes stokes
) {
// 
// pars(0) = Flux    Jy
// pars(1) = x cen   abs pix
// pars(2) = y cen   abs pix
// pars(3) = major   pix
// pars(4) = minor   pix
// pars(5) = pa radians
//
   LogIO os(LogOrigin("SkyCompRep", "fromPixel()"));
      
   ThrowIf(
		   ! cSys.hasDirectionCoordinate(),
		   "CoordinateSystem does not contain a DirectionCoordinate"
   );
   const DirectionCoordinate& dirCoord = cSys.directionCoordinate();
//
// We need to find the ratio that converts the input peak brightness
// from whatevers/per whatever to Jy per whatever.  E.g. mJy/beam to Jy/beam.  
// This ratio is passed back (for scaling errors) and is needed regardless of 
// the component type.  

   facToJy = SkyCompRep::convertToJy (brightnessUnitIn);

// Now proceed with type dependent conversions
   if (componentShape==ComponentType::POINT) {
      ThrowIf(
    		  parameters.nelements() != 3,
    	  "Wrong number of parameters for Point shape"
       );
      Vector<Double> pars(2);
      pars(0) = parameters(1);
      pars(1) = parameters(2);
      PointShape pointShape;
      pointShape.fromPixel(pars, dirCoord);
      setShape(pointShape);
//
      Quantum<Double> value(parameters(0)*facToJy, Unit("Jy"));
      itsFlux.setUnit(Unit("Jy"));
      itsFlux.setValue (value, stokes);
   } else if (componentShape==ComponentType::GAUSSIAN || componentShape==ComponentType::DISK) {
      ThrowIf(
         parameters.nelements() != 6,
         "Wrong number of parameters for Gaussian or Point shape"
     );

// Do x,y,major,minor,pa
      Vector<Double> pars(5);
      for (uInt i=0; i<5; i++) {
    	  pars(i) = parameters(i+1);
      }
      Quantum<Double> majorAxis, minorAxis, pa;
      if (componentShape==ComponentType::GAUSSIAN) {
         GaussianShape shp;
         shp.fromPixel (pars, dirCoord);
         setShape(shp);
         majorAxis = shp.majorAxis();
         minorAxis = shp.minorAxis();
         pa = shp.positionAngle();
      } else {
         DiskShape shp;
         shp.fromPixel (pars, dirCoord);
         setShape(shp);
         majorAxis = shp.majorAxis();
         minorAxis = shp.minorAxis();
         pa = shp.positionAngle();
      }
      Quantum<Double> peakFlux(parameters(0), brightnessUnitIn);
      Quantum<Double> integralFlux = 
           SkyCompRep::peakToIntegralFlux (dirCoord, componentShape, peakFlux,
                                           majorAxis, minorAxis, restoringBeam);
      itsFlux.setUnit(integralFlux.getFullUnit());   
      itsFlux.setValue (integralFlux, stokes);
   }


// Spectrum; assumed constant !
   ConstantSpectrum constSpec;
   if (cSys.hasSpectralAxis()) {
         SpectralCoordinate specCoord = cSys.spectralCoordinate();
         MFrequency mFreq;
         ThrowIf(
            ! specCoord.toWorld(mFreq, 0.0),
            "SpectralCoordinate conversion failed because "
               + specCoord.errorMessage()
            );
            constSpec.setRefFrequency(mFreq);
      }
   setSpectrum(constSpec);
}

Vector<Double> SkyCompRep::toPixel (const Unit& brightnessUnitOut,
                                    const GaussianBeam& restoringBeam,
                                    const CoordinateSystem& cSys,
                                    Stokes::StokesTypes stokes) const
//  
// pars(0) = FLux     Jy
// pars(1) = x cen    abs pix
// pars(2) = y cen    abs pix
// pars(3) = major    pix
// pars(4) = minor    pix
// pars(5) = pa radians
//
{
   LogIO os(LogOrigin("SkyCompRep", "toPixel()"));

// Find DirectionCoordinate
  
   Int dirCoordinate = cSys.findCoordinate(Coordinate::DIRECTION);
   if (dirCoordinate==-1) {
      os << "CoordinateSystem does not contain a DirectionCoordinate" << LogIO::EXCEPTION;
   } 
   const DirectionCoordinate& dirCoord = cSys.directionCoordinate(dirCoordinate);
 
// Do x,y, and possibly major,minor,pa (disk/gaussian)

   const ComponentShape& componentShape = shape();
   Vector<Double> pars = componentShape.toPixel(dirCoord);
   Vector<Double> parameters(1+pars.nelements());
   for (uInt i=0; i<pars.nelements(); i++) parameters[i+1] = pars[i];

// Now do Flux

   ComponentType::Shape type = componentShape.type();
   if (type==ComponentType::POINT) {
      Flux<Double> f = flux();
      Quantum<Double> fluxPeak = f.value (stokes, True);
      parameters(0) = fluxPeak.getValue();                    // Jy
   } else if (type==ComponentType::GAUSSIAN || type==ComponentType::DISK) {
      
// Define /beam and /pixel units.

      Bool integralInJy = True;
      Unit brightnessUnits = defineBrightnessUnits(os, brightnessUnitOut, dirCoord,
                                                               restoringBeam, integralInJy);
                                              
// Get Flux (integral) for particular Stokes. 

      Flux<Double> f = flux();
      Quantum<Double> fluxIntegral = f.value (stokes, True);
   
// Find peak value. Because we have defined /beam and /pixel units
// above we can use Quanta mathematics to get the answer we want

      Double fac;
      if (type==ComponentType::GAUSSIAN) { 
         fac = C::pi / 4.0 / log(2.0);
      } else if (type==ComponentType::DISK) {
         fac = C::pi;
      } else {
         fac = 1.0;
      }
//
      const TwoSidedShape& ts = dynamic_cast<const TwoSidedShape&>(componentShape);
      Quantum<Double> major2 = ts.majorAxis();
      major2.convert(Unit("rad"));
      Quantum<Double> minor2 = ts.minorAxis();
      minor2.convert(Unit("rad"));
//
      Quantum<Double> tmp = major2 * minor2;
      tmp.scale(fac);
      Quantum<Double> fluxPeak = fluxIntegral / tmp;           // /beam or /pixel units divided out here
      fluxPeak.convert(brightnessUnits);
      parameters(0) = fluxPeak.getValue();
      
// Undefine /beam and /pixel units

      SkyCompRep::undefineBrightnessUnits();
   }
   return parameters;
}   
   
Unit SkyCompRep::defineBrightnessUnits (
	LogIO& os, const Unit& brightnessUnitIn,
	const DirectionCoordinate& dirCoord,
	const GaussianBeam& restoringBeam,
	const Bool integralIsJy
) {
	// Define "pixel" units
	Vector<String> units(2);
	units.set("rad");
	DirectionCoordinate dirCoord2 = dirCoord;
	dirCoord2.setWorldAxisUnits(units);
	Vector<Double> inc = dirCoord2.increment();

	// Define "beam" units if needed
	// ugh this gets confusing because of the static nature of UnitMap. In some
	// cases elsewhere beam and pixel are dimensionless
	if (brightnessUnitIn.getName().contains("beam")) {
		if (! restoringBeam.isNull()) {
			// GaussianBeam rB = restoringBeam;
			// Double fac = C::pi / 4.0 / log(2.0) * rB.getMajor().getValue(Unit("rad")) * rB.getMinor().getValue(Unit("rad"));
			Double fac = restoringBeam.getArea("rad2");
            UnitMap::putUser("beam", UnitVal(fac,String("rad2")));
		}
		else {
			throw AipsError("No beam defined even though the image brightness units are " + brightnessUnitIn.getName());
		}
	}
	UnitMap::putUser("pixel", UnitVal(abs(inc(0)*inc(1)), String("rad2")));

	// We must tell the old unit that it has been redefined

	// The need to do things this way rather than a simple
	// Unit unitOut = brightnessUnit is unclear to me but it is necessary,
	// at least it keeps the unit tests passing.
	// dmehring 04may2012
	Unit unitOut(brightnessUnitIn.getName());

	// Check integral units

	if (integralIsJy) {
		if (unitOut.empty()) {
			os << LogIO::WARN << "There are no image brightness units, assuming Jy/pixel"
				<< LogIO::POST;
			unitOut = Unit("Jy/pixel");
		}
		else {
			Quantity t0(1.0, unitOut);
			Quantity t1(1.0, Unit("rad2"));
			Quantity t2 = t0 * t1;
			if (!t2.isConform(Unit("Jy"))) {
				os << LogIO::WARN << "The image units '" << unitOut.getName() << "' are not consistent " << endl;
				os << "with Jy when integrated over the sky.  Assuming Jy/pixel" << LogIO::POST;
				unitOut = Unit("Jy/pixel");
			}
		}
	}
	return unitOut;
}  

void SkyCompRep::undefineBrightnessUnits()
{          
   UnitMap::removeUser("beam");
   UnitMap::removeUser("pixel");
   UnitMap::clearCache();
}          

Quantity SkyCompRep::peakToIntegralFlux (
	const DirectionCoordinate& dirCoord,
	const ComponentType::Shape componentShape,
	const Quantum<Double>& peakFlux,
	const Quantum<Double>& majorAxis,
	const Quantum<Double>& minorAxis,
	const GaussianBeam& restoringBeam) {
	LogIO os(LogOrigin("SkyCompRep", "peakToIntegralFlux()"));
      
	// Define /beam and /pixel units.

	Unit unitIn = peakFlux.getFullUnit();
	String unitName = unitIn.getName();
	Bool integralIsJy = ! (unitName == "Jy/beam.km/s" || unitName == "K");

	Unit brightnessUnit = SkyCompRep::defineBrightnessUnits(
		os, unitIn, dirCoord, restoringBeam, integralIsJy
	);
	// Scale to integrated

	Quantity tmp(peakFlux.getValue(), brightnessUnit);
	if (componentShape==ComponentType::GAUSSIAN) {
		tmp.scale(C::pi / 4.0 / log(2.0));
	}
	else if (componentShape==ComponentType::DISK) {
		tmp.scale(C::pi);
	}
	else {
		SkyCompRep::undefineBrightnessUnits();
		os << "Unrecognized shape for flux density conversion" << LogIO::EXCEPTION;
	}
	Quantity fluxIntegral;
	Quantity majorAxis2 = majorAxis;
	Quantity minorAxis2 = minorAxis;
	majorAxis2.convert(Unit("rad"));
	minorAxis2.convert(Unit("rad"));
	fluxIntegral = tmp * majorAxis2 * minorAxis2;
	if (fluxIntegral.isConform(Unit("Jy"))) {
		fluxIntegral.convert("Jy");
	}
	else if (fluxIntegral.isConform(Unit("Jy.m/s"))) {
		fluxIntegral.convert("Jy.km/s");
	}
	else if (fluxIntegral.isConform(Unit("K.rad2"))) {
	    // do nothing, units are OK as is
    }
	else {
		os << LogIO::WARN << "Cannot convert units of Flux integral to Jy - will assume Jy"
			<< LogIO::POST;
		fluxIntegral.setUnit(Unit("Jy"));
	}
	SkyCompRep::undefineBrightnessUnits();
	return fluxIntegral;
}

Quantity SkyCompRep::integralToPeakFlux (
	const DirectionCoordinate& dirCoord,
	const ComponentType::Shape componentShape,
	const Quantity& integralFlux,
	const Unit& brightnessUnit,
	const Quantity& majorAxis,
	const Quantity& minorAxis,
	const GaussianBeam& restoringBeam
) {
	LogIO os(LogOrigin("SkyCompRep", "integralToPeakFlux()"));
	Quantity tmp(integralFlux);
	if (tmp.isConform(Unit("Jy"))) {
		tmp.convert("Jy");
	}
	else if (tmp.isConform(Unit("Jy.m/s"))) {
		tmp.convert("Jy.km/s");
	}
	else if (tmp.isConform(Unit("K.rad2"))) {
	    // do nothing units are OK as is
    }
	else {
		os << "Cannot convert units of Flux integral (" + integralFlux.getUnit() + ") to Jy"
			<< LogIO::EXCEPTION;
	}
	// Define /beam and /pixel units.

	String unitName = brightnessUnit.getName();
	Bool integralIsJy = ! (unitName == "Jy/beam.km/s" || unitName == "K");
	Unit unitIn = SkyCompRep::defineBrightnessUnits(
		os, brightnessUnit, dirCoord,
		restoringBeam, integralIsJy
	);

	// Convert integral to Jy

	// Now scale

	if (componentShape==ComponentType::GAUSSIAN) {
		tmp.scale(4.0 * log(2.0) / C::pi);
	}
	else if (componentShape==ComponentType::DISK) {
		tmp.scale(1.0 / C::pi);
	}
	else {
		SkyCompRep::undefineBrightnessUnits();
		os << "Unrecognized shape for flux density conversion" << LogIO::EXCEPTION;
	}

	// And divide out shape

	Quantity peakFlux;
	Quantity majorAxis2(majorAxis);
	Quantity minorAxis2(minorAxis);
	majorAxis2.convert(Unit("rad"));
	minorAxis2.convert(Unit("rad"));
	peakFlux = tmp / majorAxis2 / minorAxis2;
	peakFlux.convert(unitIn);
	SkyCompRep::undefineBrightnessUnits();
	return peakFlux;
}

Double SkyCompRep::convertToJy (const Unit& brightnessUnit)
{
   LogIO os(LogOrigin("SkyCompRep", __FUNCTION__));
// We need to find the ratio that converts the input peak brightness
// from whatevers/per whatever to Jy per whatever.  E.g. mJy/beam
// to Jy/beam.  This ratio is passed back (for scaling errors) and
// is needed regardless of the component type.  So we start by
// Defining /beam and /pixel units to be dimensionless
   Unit unitIn = brightnessUnit;
   // This is not thread safe, but in general anything
   // that relies on UnitMap is not because of UnitMap's
   // static nature
   SkyCompRep::undefineBrightnessUnits();

   UnitMap::putUser("pixel", UnitVal(1.0,String("")));
   UnitMap::putUser("beam",  UnitVal(1.0,String("")));
   unitIn = Unit(unitIn.getName());                // Tell system to update this unit
   Quantum<Double> tmp(1.0, unitIn);
   Double facToJy = 1.0;
   if (tmp.isConform(Unit("Jy"))) {
      Quantum<Double> tmp2(tmp);
      tmp2.convert("Jy");   
      facToJy = tmp2.getValue() / tmp.getValue();
   }
   else if (tmp.isConform(Unit("Jy.m/s"))) {
	   Quantum<Double> tmp2(tmp);
	   tmp2.convert("Jy.km/s");
	   facToJy = tmp2.getValue() / tmp.getValue();
   }
   else if (tmp.isConform(Unit("K"))) {
   	   Quantum<Double> tmp2(tmp);
   	   tmp2.convert("K");
   	   facToJy = tmp2.getValue() / tmp.getValue();
   }
   else {
      os << LogIO::WARN << "Cannot convert units of brightness to Jy - will assume Jy"
         << LogIO::POST;
      facToJy = 1.0;
   }
      
// Undefine /beam and /pixel

   SkyCompRep::undefineBrightnessUnits();
   return facToJy;
}

} //# NAMESPACE CASA - END

