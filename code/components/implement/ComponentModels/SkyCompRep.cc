//# SkyCompRep.cc:  this defines SkyCompRep
//# Copyright (C) 1996,1997,1998,1999,2000
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

#include <trial/ComponentModels/ComponentShape.h>
#include <trial/ComponentModels/ComponentType.h>
#include <trial/ComponentModels/ConstantSpectrum.h>
#include <trial/ComponentModels/PointShape.h>
#include <trial/ComponentModels/GaussianShape.h>
#include <trial/ComponentModels/SkyCompRep.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/Cube.h>
#include <aips/Arrays/Vector.h>
#include <aips/Containers/Record.h>
#include <aips/Containers/RecordFieldId.h>
#include <aips/Containers/RecordInterface.h>
#include <trial/Coordinates/CoordinateSystem.h>
#include <trial/Coordinates/DirectionCoordinate.h>
#include <trial/Coordinates/SpectralCoordinate.h>
#include <aips/Exceptions/Error.h>
#include <aips/Logging/LogIO.h>
#include <aips/Logging/LogOrigin.h>
#include <aips/Mathematics/Math.h>
#include <aips/Mathematics/Complex.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MFrequency.h>
#include <aips/Measures/Stokes.h>
#include <aips/Quanta/MVAngle.h>
#include <aips/Quanta/Quantum.h>
#include <aips/Quanta/Unit.h>
#include <aips/Quanta/UnitMap.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/DataType.h>
#include <trial/ComponentModels/SpectralModel.h>

SkyCompRep::SkyCompRep() 
  :itsShapePtr(new PointShape),
   itsSpectrumPtr(new ConstantSpectrum),
   itsFlux(),
   itsLabel()
{
  AlwaysAssert(ok(), AipsError);
}

SkyCompRep::SkyCompRep(const ComponentType::Shape& shape)
  :itsShapePtr(ComponentType::construct(shape)),
   itsSpectrumPtr(new ConstantSpectrum),
   itsFlux(),
   itsLabel()
{
  AlwaysAssert(ok(), AipsError);
}

SkyCompRep::SkyCompRep(const ComponentType::Shape& shape,
		       const ComponentType::SpectralShape& spectrum)
  :itsShapePtr(ComponentType::construct(shape)),
   itsSpectrumPtr(ComponentType::construct(spectrum)),
   itsFlux(),
   itsLabel()
{
  AlwaysAssert(ok(), AipsError);
}

SkyCompRep::SkyCompRep(const Flux<Double>& flux,
		       const ComponentShape& shape, 
		       const SpectralModel& spectrum)
  :itsShapePtr(shape.clone()),
   itsSpectrumPtr(spectrum.clone()),
   itsFlux(flux.copy()),
   itsLabel()
{
  AlwaysAssert(ok(), AipsError);
}

SkyCompRep::SkyCompRep(const SkyCompRep& other) 
  :itsShapePtr(other.itsShapePtr->clone()),
   itsSpectrumPtr(other.itsSpectrumPtr->clone()),
   itsFlux(other.itsFlux.copy()),
   itsLabel(other.itsLabel)
{
  AlwaysAssert(ok(), AipsError);
}

SkyCompRep::~SkyCompRep() {
  DebugAssert(ok(), AipsError);
}

SkyCompRep& SkyCompRep::operator=(const SkyCompRep& other) {
  if (this != &other) {
    itsShapePtr = other.itsShapePtr->clone();
    itsSpectrumPtr = other.itsSpectrumPtr->clone();
    itsFlux = other.itsFlux.copy();
    itsLabel = other.itsLabel;
  }
  AlwaysAssert(ok(), AipsError);
  return *this;
}

const Flux<Double>& SkyCompRep::flux() const {
  DebugAssert(ok(), AipsError);
  return itsFlux;
}

Flux<Double>& SkyCompRep::flux() {
  DebugAssert(ok(), AipsError);
  return itsFlux;
}

const ComponentShape& SkyCompRep::shape() const {
  DebugAssert(ok(), AipsError);
  return *itsShapePtr;
}

ComponentShape& SkyCompRep::shape() {
  DebugAssert(ok(), AipsError);
  return *itsShapePtr;
}

void SkyCompRep::setShape(const ComponentShape& newShape) {
  DebugAssert(ok(), AipsError);
  itsShapePtr = newShape.clone();
}

SpectralModel& SkyCompRep::spectrum() {
  DebugAssert(ok(), AipsError);
  return *itsSpectrumPtr;
}

const SpectralModel& SkyCompRep::spectrum() const {
  DebugAssert(ok(), AipsError);
  return *itsSpectrumPtr;
}

void SkyCompRep::setSpectrum(const SpectralModel& newSpectrum) {
  DebugAssert(ok(), AipsError);
  itsSpectrumPtr = newSpectrum.clone();
}

String& SkyCompRep::label() {
  DebugAssert(ok(), AipsError);
  return itsLabel;
}

const String& SkyCompRep::label() const {
  DebugAssert(ok(), AipsError);
  return itsLabel;
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
  DebugAssert(ok(), AipsError);
  Double scale = itsShapePtr->sample(direction, pixelLatSize, pixelLongSize);
  scale *= itsSpectrumPtr->sample(centerFrequency);
  Flux<Double> flux = itsFlux.copy();
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
  DebugAssert(ok(), AipsError);
  const uInt nDirSamples = directions.nelements();
  DebugAssert(samples.nrow() == 4, AipsError);
  DebugAssert(samples.ncolumn() == nDirSamples, AipsError);
  const uInt nFreqSamples = frequencies.nelements();
  DebugAssert(samples.nplane() == nFreqSamples, AipsError);
  DebugAssert(pixelLatSize.radian() > 0.0, AipsError);
  DebugAssert(pixelLongSize.radian() > 0.0, AipsError);
  
  Flux<Double> f = itsFlux.copy();
  f.convertUnit(reqUnit);
  Vector<Double> fluxVal(4);
  f.value(fluxVal);
  const Double i = fluxVal(0);
  const Double q = fluxVal(1);
  const Double u = fluxVal(2);
  const Double v = fluxVal(3);
  
  Vector<Double> dirScales(nDirSamples);
  itsShapePtr->sample(dirScales, directions, dirRef,
 		      pixelLatSize, pixelLongSize);
  Vector<Double> freqScales(nFreqSamples);
  itsSpectrumPtr->sample(freqScales, frequencies, freqRef);

  for (uInt f = 0; f < nFreqSamples; f++) {
    const Double thisFreqScale = freqScales(f);
    for (uInt d = 0; d < nDirSamples; d++) {
      const Double thisScale = dirScales(d) * thisFreqScale;
      samples(0, d, f) += thisScale * i;
      samples(1, d, f) += thisScale * q;
      samples(2, d, f) += thisScale * u;
      samples(3, d, f) += thisScale * v;
    }
  }
}

Flux<Double> SkyCompRep::visibility(const Vector<Double>& uvw,
				    const Double& frequency) const {
  DebugAssert(ok(), AipsError);
  Flux<Double> flux = itsFlux.copy();
  Double scale = itsShapePtr->visibility(uvw, frequency).real();
  flux.scaleValue(scale, scale, scale, scale);
  // I should scale by the frequency here also but I need to consult with Tim
  // first.
  return flux;
}

void SkyCompRep::visibility(Cube<DComplex>& visibilities,
			    const Matrix<Double>& uvws,
			    const Vector<Double>& frequencies) const {
  DebugAssert(ok(), AipsError);
  DebugAssert(uvws.nrow() == 3, AipsError);
  DebugAssert(visibilities.nrow() == 4, AipsError);
  const uInt nFreq = frequencies.nelements();
  DebugAssert(visibilities.ncolumn() == nFreq, AipsError);
  const uInt nVis = uvws.ncolumn();
  DebugAssert(visibilities.nplane() == nVis, AipsError);
  DebugAssert(itsShapePtr->isSymmetric(), AipsError);

  Vector<Double> uvw(3);
  Block<DComplex> flux(4);
  for (uInt p = 0; p < 4; p++) {
    flux[p] = itsFlux.value(p);
  }
  for (uInt v = 0; v < nVis; v++) {
    for (uInt u = 0; u < 3; u++) {
      uvw(u) = uvws(u, v);
    }
    for (uInt f = 0; f < nFreq; f++) {
      const Double scale = itsShapePtr->visibility(uvw, frequencies(f)).real();
      // I should scale by the frequency here also but I need to consult with
      // Tim first.
      for (uInt p = 0; p < 4; p++) {
	visibilities(p, f, v) = flux[p] * scale;
      }
    }
  }
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
  record.define(RecordFieldId("label"), itsLabel);
  DebugAssert(ok(), AipsError);
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

void SkyCompRep::fromPixel (const Vector<Double>& parameters,
                            const Unit& brightnessUnitIn,
                            const Vector<Quantum<Double> >& restoringBeam,
                            const CoordinateSystem& cSys,
                            ComponentType::Shape componentShape,
                            Stokes::StokesTypes stokes,
                            Bool xIsLong)
// 
// pars(0) = Flux    Jy
// pars(1) = x cen   abs pix
// pars(2) = y cen   abs pix
// pars(3) = major   pix
// pars(4) = minor   pix
// pars(5) = pa radians
//
{

// Check number of parameters
  
   LogIO os(LogOrigin("SkyCompRep", "fromPixel()"));
   if (componentShape==ComponentType::GAUSSIAN) {
      if (parameters.nelements()!=6) {
         os << "Wrong number of parameters for Gaussian model" << LogIO::EXCEPTION;
      }
   } else if (componentShape==ComponentType::DISK) {
      if (parameters.nelements()!=6) {
         os << "Wrong number of parameters for Disk model" << LogIO::EXCEPTION;
      }
   } else if (componentShape==ComponentType::POINT) {
      if (parameters.nelements()!=3) {
         os << "Wrong number of parameters for Point model" << LogIO::EXCEPTION;
      }
   } else {
      os << "Unknown Component shape" << LogIO::EXCEPTION;
   }

      
// Find DirectionCoordinate

   Int dirCoordinate = cSys.findCoordinate(Coordinate::DIRECTION);
   if (dirCoordinate==-1) {
      os << "CoordinateSystem does not contain a DirectionCoordinate" << LogIO::EXCEPTION;
   }
   DirectionCoordinate dirCoord = cSys.directionCoordinate(dirCoordinate);
   Vector<String> axisUnits(2);
   axisUnits.set("rad");
   if (!dirCoord.setWorldAxisUnits(axisUnits)) {
      os << "Failed to set DirectionCoordinate axis units to radians" << LogIO::EXCEPTION;
   }
   Vector<Int> dirPixelAxes = cSys.pixelAxes(dirCoordinate);
//  
   uInt whereIsX = 0;
   uInt whereIsY = 1;
   if (!xIsLong) {
      whereIsX = 1;
      whereIsY = 0;
   }
      
// Position; absolute pixels in image are converted to an MDirection

   Vector<Double> world;
   Vector<Double> pixelRef(2);
   MDirection directionRef;
   pixelRef(whereIsX) = parameters(1);
   pixelRef(whereIsY) = parameters(2);
   if (!dirCoord.toWorld(directionRef, pixelRef)) {
      os << "DirectionCoordinate conversion failed because "
         << dirCoord.errorMessage() << LogIO::EXCEPTION;
   }
         
// Spectrum   
         
   ConstantSpectrum constSpec;
   Int specCoordinate = cSys.findCoordinate(Coordinate::SPECTRAL);
   if (specCoordinate!=-1) {
      Vector<Int> specAxes = cSys.pixelAxes(specCoordinate);
      if (specAxes.nelements() > 1) {
         os << LogIO::WARN
            << "This image has a SpectralCoordinate with > 1 axes.  I cannot handle that"
            << endl;
         os << "The image will be treated as if it had no SpectralCorodinate" << LogIO::POST;
      } else {
   
// If the subImage has a SpectralCoordinate, there is only one Spectral pixel (with
// pixel coordinate 0.0) in that subImage (because region is 2D in DirectionCoordinate).
// Find its frequency.
      
         SpectralCoordinate specCoord = cSys.spectralCoordinate(specCoordinate);
         MFrequency mFreq;
         if (!specCoord.toWorld(mFreq, 0.0)) {
            os << "SpectralCoordinate conversion failed because "
               << specCoord.errorMessage() << LogIO::EXCEPTION;
         } else {
            constSpec.setRefFrequency(mFreq);
         }
      }
   }
   setSpectrum(constSpec);

//

   Unit unitIn = brightnessUnitIn;
   if (componentShape==ComponentType::POINT) {
      
// Direction

      PointShape pointShape(directionRef);
      setShape(pointShape);

// Flux. Define /beam and /pixel units to be dimensionless

      UnitMap::putUser("pixel", UnitVal(1.0,String("")));
      UnitMap::putUser("beam",  UnitVal(1.0,String("")));
      unitIn = Unit(unitIn.getName());                // Tell system to update this unit
//
      itsFlux.setUnit(Unit("Jy"));
      Quantum<Double> peak2(parameters(0), unitIn);
      Quantum<Double> value(1.0,Unit("Jy"));
      if (peak2.isConform(Unit("Jy"))) {
         Double valIn = peak2.getValue();
         peak2.convert("Jy");   
         Double fac = peak2.getValue() / valIn;
         value.setValue(parameters(0) * fac);
      } else {
         os << LogIO::SEVERE << "Cannot convert units of brightness to Jy - will assume Jy"
            << LogIO::POST;
         value.setValue(parameters(0));
      }
      itsFlux.setValue (value, stokes);
      
// Undefine /beam and /pixel

      SkyCompRep::undefineBrightnessUnits();
   } else if (componentShape==ComponentType::GAUSSIAN || componentShape==ComponentType::DISK) {

// Define /beam and /pixel units.

      Bool integralIsJy = True;
      Unit brightnessUnit = SkyCompRep::defineBrightnessUnits(os, brightnessUnitIn, cSys, restoringBeam, integralIsJy);

// Shape.  First get position angle relative to the coordinate frame
// (i.e. not relative to the vertical/horizontal pixel coordinate frame)
// Find tip of major and minor axes
   
      Double cospa = cos(parameters(5));
      Double sinpa = sin(parameters(5));
      Double z = parameters(3) / 2.0;
      Double x = -z * sinpa;
      Double y =  z * cospa;  
//
      MDirection directionMajor;
      Vector<Double> pixelMajor(2);
      pixelMajor(whereIsX) = parameters(1) + x;
      pixelMajor(whereIsY) = parameters(2) + y;
      if (!dirCoord.toWorld(directionMajor, pixelMajor)) {
         os << "DirectionCoordinate conversion failed because "
            << dirCoord.errorMessage() << LogIO::EXCEPTION;
      }
//
      z = parameters(4) / 2.0;
      x = z * cospa;
      y = z * sinpa;
      MDirection directionMinor;
      Vector<Double> pixelMinor(2);
      pixelMinor(whereIsX) = parameters(1) + x;
      pixelMinor(whereIsY) = parameters(2) + y;
      if (!dirCoord.toWorld(directionMinor, pixelMinor)) {
         os << "DirectionCoordinate conversion failed because "
            << dirCoord.errorMessage() << LogIO::EXCEPTION;
      }

// Find position angle between centre and major axis tip
      
      MVDirection mvdRef = directionRef.getValue();
      MVDirection mvdMajor = directionMajor.getValue();
      MVDirection mvdMinor = directionMinor.getValue();

// Separations

      Double tmp1 = 2 * mvdRef.separation(mvdMajor) * 3600 * 180.0 / C::pi;
      Double tmp2 = 2 * mvdRef.separation(mvdMinor) * 3600 * 180.0 / C::pi;
      Quantum<Double> fitMajorAxis(max(tmp1,tmp2), Unit("arcsec"));
      Quantum<Double> fitMinorAxis(min(tmp1,tmp2), Unit("arcsec"));
// 
      Double pa;
      if (tmp1 >= tmp2) {
         pa = mvdRef.positionAngle(mvdMajor);
      } else {
         pa = mvdRef.positionAngle(mvdMinor);  
      } 
      Quantum<Double> fitPA(pa, Unit("rad"));

// Note that the major and minor axes in pixels may have swapped when
// converted to world coordinates.  Function decodeSkyComponent must
// also take this into account when going the other way.
      
      GaussianShape gaussShape(directionRef, fitMajorAxis,
                               fitMinorAxis, fitPA);
      setShape(gaussShape);
      
// Component needs integrated flux.
   
      Quantum<Double> fitPeak(parameters(0), brightnessUnit);
      if (componentShape==ComponentType::GAUSSIAN) {  
         fitPeak.scale(C::pi / 4.0 / log(2.0));
      } else if (componentShape==ComponentType::DISK) { 
         fitPeak.scale(C::pi);
      }
      Quantum<Double> fitIntegral;
      fitMajorAxis.convert(Unit("rad"));
      fitMinorAxis.convert(Unit("rad"));
      fitIntegral = fitPeak * fitMajorAxis * fitMinorAxis;  
      if (fitIntegral.isConform(Unit("Jy"))) {
         fitIntegral.convert("Jy");
      } else {
         os << LogIO::SEVERE << "Cannot convert units of Flux integral to Jy - will assume Jy"
            << LogIO::POST;
         fitIntegral.setUnit(Unit("Jy"));
      }   
         
// Set flux

      itsFlux.setUnit(fitIntegral.getFullUnit());   
      itsFlux.setValue (fitIntegral, stokes);


// Undefine /beam and /pixel units

      SkyCompRep::undefineBrightnessUnits();
   }
}



Vector<Double> SkyCompRep::toPixel (const Unit& brightnessUnitIn,
                                    const Vector<Quantum<Double> >& restoringBeam,
                                    const CoordinateSystem& cSys,
                                    Stokes::StokesTypes stokes,
                                    Bool xIsLong) const
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
//
   const ComponentShape& componentShape = shape();
   ComponentType::Shape type = componentShape.type();
   Vector<Double> parameters(3);

// Get DirectionCoordinate for CS
      
   Int dirCoordinate = cSys.findCoordinate(Coordinate::DIRECTION);
   if (dirCoordinate==-1) {
      os << "There is no DirectionCoordinate in thie CoordinateSystem" << LogIO::EXCEPTION;
   }
   const DirectionCoordinate& dirCoord = cSys.directionCoordinate(dirCoordinate);
      
// Convert centre from an MDirection to absolute pixels

   const MDirection dirRef = componentShape.refDirection();
   Vector<Double> pixelCen(2);
   if (!dirCoord.toPixel(pixelCen, dirRef)) {
      os << "DirectionCoordinate conversion to pixel failed because "
         << dirCoord.errorMessage() << LogIO::EXCEPTION;
   }
   if (xIsLong) {
      parameters(1) = pixelCen(0);      // xcen
      parameters(2) = pixelCen(1);      // ycen
   } else {
      parameters(1) = pixelCen(1);
      parameters(2) = pixelCen(0);
   }

// Do the rest of the shape if there is any
      
   if (type==ComponentType::POINT) {
      Flux<Double> f = flux();
      Quantum<Double> fluxPeak = f.value (stokes, True);
      parameters(0) = fluxPeak.getValue();                    // Jy
   } else if (type==ComponentType::GAUSSIAN || type==ComponentType::DISK) {
      parameters.resize(6,True);
      
// Caste to get at actual shape
      
      const TwoSidedShape& ts = dynamic_cast<const TwoSidedShape&>(componentShape);
    
// Fish out major/minor axes and p.a.   
      
      Vector<Double> p = ts.toPixel (dirCoord);
      parameters(3) = p(0);
      parameters(4) = p(1);
      parameters(5) = p(2);
    
// Define /beam and /pixel units.

      Bool integralInJy = True;
      Unit brightnessUnits = SkyCompRep::defineBrightnessUnits(os, brightnessUnitIn, cSys, 
                                                               restoringBeam, integralInJy);
                                              
// Get Flux for particular Stokes. It is currently not possible to extract the appropriate
// Stokes Value with the Flux class.

      Flux<Double> f = flux();
      Quantum<Double> fluxIntegral = f.value (stokes, True);
   
// Convert to peak

      Double fac;
      if (type==ComponentType::GAUSSIAN) { 
         fac = C::pi / 4.0 / log(2.0);
      } else if (type==ComponentType::DISK) {
         fac = C::pi;
      } else {
         fac = 1.0;
      }
//
      Quantum<Double> major2 = ts.majorAxis();
      major2.convert(Unit("rad"));
      Quantum<Double> minor2 = ts.minorAxis();
      minor2.convert(Unit("rad"));
//
      Quantum<Double> tmp = major2 * minor2;
      tmp.scale(fac);
      Quantum<Double> fluxPeak = fluxIntegral / tmp;
      fluxPeak.convert(brightnessUnits);
      parameters(0) = fluxPeak.getValue();
      
// Undefine /beam and /pixel units

      SkyCompRep::undefineBrightnessUnits();
   }
   return parameters;
}   
   


Unit SkyCompRep::defineBrightnessUnits (LogIO& os, 
                                        const Unit& brightnessUnitIn,
                                        const CoordinateSystem& cSys,
                                        const Vector<Quantum<Double> >& restoringBeam,
                                        Bool integralIsJy)
{
   Int dirCoordinate = cSys.findCoordinate(Coordinate::DIRECTION);
   Unit unitOut = brightnessUnitIn;
              
// Define "pixel" units if we can
                  
   Vector<Double> inc;
   if (dirCoordinate>=0) {
      DirectionCoordinate dirCoord = cSys.directionCoordinate(dirCoordinate);
      Vector<String> units(2); 
      units.set("rad");
      dirCoord.setWorldAxisUnits(units);
      inc = dirCoord.increment();
      UnitMap::putUser("pixel", UnitVal(abs(inc(0)*inc(1)), String("rad2")));
   } 

// Define "beam" units if needed

   if (unitOut.getName().contains("beam")) {
      if (restoringBeam.nelements()==3) {
         Vector<Quantum<Double> > rB = restoringBeam.copy();
         rB(0).convert(Unit("rad"));
         rB(1).convert(Unit("rad"));
         Double fac = C::pi / 4.0 / log(2.0) * rB(0).getValue() * rB(1).getValue();
         UnitMap::putUser("beam", UnitVal(fac,String("rad2")));
      } else {
         if (dirCoordinate>=0) {
            os << LogIO::WARN
               << "No restoring beam defined even though the image brightness units contain a beam"
               << endl << "Assuming the restoring beam is one pixel" << LogIO::POST;
            UnitMap::putUser("beam", UnitVal(abs(inc(0)*inc(1)), String("rad2")));
         } else {
            os << "The image units contain a beam.  However, there is no restoring beam" << endl;
            os << "defined, and no DirectionCoordinate in the image" << LogIO::EXCEPTION;
         }
      }
   }

// We must tell the old unit that it has been redefined
        
   unitOut = Unit(unitOut.getName());
     
// Check integral units

   if (integralIsJy) {
     if (unitOut.empty()) {
        unitOut = Unit("Jy/pixel"); 
        if (dirCoordinate>=0) { 
           os << LogIO::WARN << "There are no image brightness units, assuming Jy/pixel" << LogIO::POST;
           unitOut = Unit("Jy/pixel");
        } else {
           os << "There are no image brightness units, we would like to assume Jy/pixel" << endl;
           os << "However, there is no DirectionCoordinate in the image either" << LogIO::EXCEPTION;
        }
     } else {
        Quantum<Double> t0(1.0, unitOut);
        Quantum<Double> t1(1.0, Unit("rad2"));
        Quantum<Double> t2 = t0 * t1;
        if (!t2.isConform(Unit("Jy"))) {
           if (dirCoordinate>=0) {
              os << LogIO::WARN << "The image units '" << unitOut.getName() << "' are not consistent " << endl;
              os << "with Jy when integrated over the sky.  Assuming Jy/pixel" << LogIO::POST;
              unitOut = Unit("Jy/pixel");
           } else {
              os << "The image units '" << unitOut.getName() << "' are not consistent " << endl;
              os << "with Jy when integrated over the sky.  We would like to assume Jy/pixel" << endl;
              os << "However, there is no DirectionCoordinate in the image either" << LogIO::EXCEPTION;
           }
        }
     }
   }
//
   return unitOut;
}  

void SkyCompRep::undefineBrightnessUnits()
{          
   UnitMap::removeUser("beam");
   UnitMap::removeUser("pixel");
   UnitMap::clearCache();
}          



// Local Variables: 
// compile-command: "gmake SkyCompRep"
// End: 
