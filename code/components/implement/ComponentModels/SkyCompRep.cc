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
#include <trial/ComponentModels/ConstantSpectrum.h>
#include <trial/ComponentModels/PointShape.h>
#include <trial/ComponentModels/SkyCompRep.h>
#include <trial/ComponentModels/SpectralModel.h>
#include <aips/Logging/LogIO.h>
#include <aips/Logging/LogOrigin.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Containers/RecordInterface.h>
#include <aips/Containers/RecordFieldId.h>
#include <aips/Containers/Record.h>
#include <aips/Exceptions/Error.h>
#include <aips/Measures/MDirection.h>
#include <aips/Quanta/Quantum.h>
#include <aips/Quanta/Unit.h>
#include <aips/Measures/MFrequency.h>
#include <aips/Quanta/MVAngle.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/DataType.h>

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

void SkyCompRep::sample(Matrix<Flux<Double> >& samples,
			const Vector<MVDirection>& directions, 
			const MeasRef<MDirection>& dirRef, 
			const MVAngle& pixelLatSize, 
			const MVAngle& pixelLongSize, 
			const Vector<MVFrequency>& frequencies,
			const MeasRef<MFrequency>& freqRef) const {
  DebugAssert(ok(), AipsError);
  const uInt nDirSamples = directions.nelements();
  DebugAssert(samples.nrow() == nDirSamples, AipsError);
  const uInt nFreqSamples = frequencies.nelements();
  DebugAssert(samples.ncolumn() == nFreqSamples, AipsError);
  DebugAssert(pixelLatSize.radian() > 0.0, AipsError);
  DebugAssert(pixelLongSize.radian() > 0.0, AipsError);
  
  const Vector<DComplex> fluxVal = itsFlux.value();
  const Unit fluxUnit = itsFlux.unit();
  const ComponentType::Polarisation fluxPol = itsFlux.pol();
  Vector<Double> dirScales(nDirSamples);
  itsShapePtr->sample(dirScales, directions, dirRef,
		      pixelLatSize, pixelLongSize);
  Vector<Double> freqScales(nFreqSamples);
  itsSpectrumPtr->sample(freqScales, frequencies, freqRef);

  for (uInt f = 0; f < nFreqSamples; f++) {
    const Double thisFreqScale = freqScales(f);
    for (uInt d = 0; d < nDirSamples; d++) {
      const Double thisScale = dirScales(d) * thisFreqScale;
      Flux<Double>& thisFlux = samples(d, f);
      if (near(thisScale, 0.0)) {
	thisFlux.setValue(0.0);
      } else {
	thisFlux.setValue(fluxVal);
	thisFlux.scaleValue(thisScale);
      }
      thisFlux.setUnit(fluxUnit);
      thisFlux.setPol(fluxPol);
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

// Local Variables: 
// compile-command: "gmake SkyCompRep"
// End: 
