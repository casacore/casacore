//# SkyComponent.cc:  this defines SkyComponent
//# Copyright (C) 1996,1997,1998
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

#include <trial/ComponentModels/SkyComponent.h>
#include <trial/ComponentModels/Flux.h>
#include <trial/ComponentModels/SkyCompRep.h>
#include <trial/ComponentModels/PointCompRep.h>
#include <trial/ComponentModels/SIPointCompRep.h>
#include <trial/ComponentModels/GaussianCompRep.h>
#include <trial/Images/ImageInterface.h>
#include <aips/Arrays/Vector.h>
#include <aips/Exceptions/Error.h>
#include <aips/Glish/GlishArray.h>
#include <aips/Glish/GlishRecord.h>
#include <aips/Glish/GlishValue.h>
#include <aips/Logging/LogOrigin.h>
#include <aips/Logging/LogIO.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MFrequency.h>
#include <aips/Measures/MVAngle.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/String.h>

SkyComponent::SkyComponent()
  :itsCompPtr(new PointCompRep) 
{
  AlwaysAssert(ok(), AipsError);
}

SkyComponent::SkyComponent(ComponentType::Shape shape) {
  switch (shape){
  case ComponentType::POINT:
    itsCompPtr = new PointCompRep; break;
  case ComponentType::GAUSSIAN:
    itsCompPtr = new GaussianCompRep; break;
  case ComponentType::UNKNOWN_SHAPE: {
    String errorMessage = "SkyComponent::Unable to construct a component of "
      + ComponentType::name(ComponentType::UNKNOWN_SHAPE) + " shape";
    throw(AipsError(errorMessage));
    }
    break;
  case ComponentType::NUMBER_SHAPES: {
    String errorMessage = "SkyComponent::Unable to construct a component of "
      + ComponentType::name(ComponentType::NUMBER_SHAPES) + " shape";
    throw(AipsError(errorMessage));
    }
    break;
  };    
  AlwaysAssert(ok(), AipsError);
}

SkyComponent::SkyComponent(ComponentType::Shape shape,
			   ComponentType::SpectralShape spectralModel) {
  if (spectralModel == ComponentType::SPECTRAL_INDEX) {
    switch (shape){
    case ComponentType::POINT:
      itsCompPtr = new SIPointCompRep; break;
    case ComponentType::GAUSSIAN: {
      String errorMessage = "SkyComponent::Unable to construct a component of "
	+ ComponentType::name(ComponentType::GAUSSIAN) + " shape with a "
	+ ComponentType::name(ComponentType::SPECTRAL_INDEX) 
	+ " spectral model";
      throw(AipsError(errorMessage));
    }
    break;
    case ComponentType::UNKNOWN_SHAPE: {
      String errorMessage = "SkyComponent::Unable to construct a component of "
	+ ComponentType::name(ComponentType::UNKNOWN_SHAPE) + " shape with a "
	+ ComponentType::name(ComponentType::SPECTRAL_INDEX) 
	+ " spectral model";
      throw(AipsError(errorMessage));
    }
    break;
    case ComponentType::NUMBER_SHAPES: {
      String errorMessage = "SkyComponent::Unable to construct a component of "
	+ ComponentType::name(ComponentType::NUMBER_SHAPES) + " shape with a "
	+ ComponentType::name(ComponentType::SPECTRAL_INDEX) 
	+ " spectral model";
      throw(AipsError(errorMessage));
    }
    break;
    };    
  } else {
    switch (shape){
    case ComponentType::POINT:
      itsCompPtr = new PointCompRep; break;
    case ComponentType::GAUSSIAN:
      itsCompPtr = new GaussianCompRep; break;
    case ComponentType::UNKNOWN_SHAPE: {
      String errorMessage = "SkyComponent::Unable to construct a component of "
	+ ComponentType::name(ComponentType::UNKNOWN_SHAPE) + " shape";
	throw(AipsError(errorMessage));
    }
    break;
    case ComponentType::NUMBER_SHAPES: {
      String errorMessage = "SkyComponent::Unable to construct a component of "
	+ ComponentType::name(ComponentType::NUMBER_SHAPES) + " shape";
	throw(AipsError(errorMessage));
    }
    break;
    };    
  }
  AlwaysAssert(ok(), AipsError);
}
SkyComponent::SkyComponent(const SkyComponent & other) 
  :itsCompPtr(other.itsCompPtr)
{ 
  AlwaysAssert(ok(), AipsError);
}

SkyComponent::~SkyComponent() {
  DebugAssert(ok(), AipsError);
}

SkyComponent & SkyComponent::operator=(const SkyComponent & other) {
  if (this != &other)
    itsCompPtr = other.itsCompPtr;
  // This call to ok is restricted to the base class as the operator= function
  // is called by derived classes before the derived object is fully
  // constructed.
  DebugAssert(SkyComponent::ok(), AipsError);
  return *this;
}

void SkyComponent::sample(Vector<Double> & result, 
			  const MDirection & sampleDir, 
			  const MVAngle & pixelSize) const {
  itsCompPtr->sample(result, sampleDir, pixelSize);
  DebugAssert(ok(), AipsError);
}

void SkyComponent::project(ImageInterface<Float> & plane) const {
  itsCompPtr->project(plane);
  DebugAssert(ok(), AipsError);
}

Flux<Double> & SkyComponent::flux() {
  DebugAssert(ok(), AipsError);
  return itsCompPtr->flux();
}

const Flux<Double> & SkyComponent::flux() const {
  DebugAssert(ok(), AipsError);
  return itsCompPtr->flux();
}

void SkyComponent::setDirection(const MDirection & newDir) {
  itsCompPtr->setDirection(newDir);
  DebugAssert(ok(), AipsError);
}

void SkyComponent::direction(MDirection & compDir) const {
  itsCompPtr->direction(compDir);
  DebugAssert(ok(), AipsError);
}

Flux<Double> SkyComponent::visibility(const Vector<Double> & uvw,
				      const Double & frequency) const {
  DebugAssert(ok(), AipsError);
  return itsCompPtr->visibility(uvw, frequency);
}

void SkyComponent::setLabel(const String & newLabel) {
  itsCompPtr->setLabel(newLabel);
  DebugAssert(ok(), AipsError);
}

void SkyComponent::label(String & compLabel) const {
  itsCompPtr->label(compLabel);
  DebugAssert(ok(), AipsError);
}

uInt SkyComponent::nParameters() const {
  DebugAssert(ok(), AipsError);
  return itsCompPtr->nParameters();
}

void SkyComponent::setParameters(const Vector<Double> & newParms) {
  itsCompPtr->setParameters(newParms);
  DebugAssert(ok(), AipsError);
}

void SkyComponent::parameters(Vector<Double> & compParms) const {
  itsCompPtr->parameters(compParms);
  DebugAssert(ok(), AipsError);
}

ComponentType::Shape SkyComponent::shape() const {
  // This call to ok is restricted to the base class as the shape()
  // function is called by derived classes before the derived object is fully
  // constructed.
  DebugAssert(SkyComponent::ok(), AipsError);
  return itsCompPtr->shape();
}

ComponentType::SpectralShape SkyComponent::spectralShape() const {
  DebugAssert(ok(), AipsError);
  return itsCompPtr->spectralShape();
}

void SkyComponent::setRefFrequency(const MFrequency & newRefFreq) {
  itsCompPtr->setRefFrequency(newRefFreq);
  DebugAssert(ok(), AipsError);
}

const MFrequency & SkyComponent::refFrequency() const {
  DebugAssert(ok(), AipsError);
  return itsCompPtr->refFrequency();
}

Double SkyComponent::scale(const MFrequency & sampleFreq) const {
  DebugAssert(ok(), AipsError);
  return itsCompPtr->scale(sampleFreq);
}

Flux<Double> SkyComponent::sample(const MFrequency & sampleFreq) const {
  DebugAssert(ok(), AipsError);
  return itsCompPtr->sample(sampleFreq);
}

uInt SkyComponent::nSpectralParameters() const {
  DebugAssert(ok(), AipsError);
  return itsCompPtr->nSpectralParameters();
}

void SkyComponent::setSpectralParameters(const Vector<Double> & newParms) {
  itsCompPtr->setSpectralParameters(newParms);
  DebugAssert(ok(), AipsError);
}

void SkyComponent::spectralParameters(Vector<Double> & compParms) const {
  itsCompPtr->spectralParameters(compParms);
  DebugAssert(ok(), AipsError);
}

Bool SkyComponent::fromRecord(String & errorMessage, 
			      const GlishRecord & record) {
  DebugAssert(ok(), AipsError);
  // First check that the shapes match;
  if (!checkShape(errorMessage, record)) return False;
  if (!checkSpectralShape(errorMessage, record)) return False;
  return itsCompPtr->fromRecord(errorMessage, record);
}

Bool SkyComponent::toRecord(String & errorMessage,
			    GlishRecord & record) const {
  DebugAssert(ok(), AipsError);
  return itsCompPtr->toRecord(errorMessage, record);
}

ComponentType::Shape SkyComponent::getShape(String & errorMessage,
					    const GlishRecord & record) {
  if (!record.exists("shape")) {
    errorMessage += "\nAssuming the component has a point shape";
    return ComponentType::POINT;
  }
  if (record.get("shape").type() != GlishValue::RECORD) {
    errorMessage += "\nThe 'shape' field must be a record";
    return ComponentType::UNKNOWN_SHAPE;
  }
  const GlishRecord shapeRec = record.get("shape");
  if (!shapeRec.exists("type")) {
    errorMessage += "\nThe shape record does not have a 'type' field";
    return ComponentType::UNKNOWN_SHAPE;
  }
  if (shapeRec.get("type").type() != GlishValue::ARRAY) {
    errorMessage += "\nThe 'type' field cannot be a record";
    return ComponentType::UNKNOWN_SHAPE;
  }
  const GlishArray typeField = shapeRec.get("type");
  if (typeField.elementType() != GlishArray::STRING) {
    errorMessage += "\nThe 'type' field must be a string";
    return ComponentType::UNKNOWN_SHAPE;
  }
  const IPosition shape = typeField.shape();
  if (shape.nelements() != 1 || shape.product() != 1) {
    errorMessage +="\nThe 'type' field must be a vector with only one element";
    return ComponentType::UNKNOWN_SHAPE;
  }
  String typeString;
  if (!typeField.get(typeString)) {
    errorMessage += "\nCould not read the 'type' field for an unknown reason";
    return ComponentType::UNKNOWN_SHAPE;
  }
  return ComponentType::shape(typeString);
}

ComponentType::SpectralShape SkyComponent::
getSpectralShape(String & errorMessage, const GlishRecord & record) {
  if (!record.exists("spectrum")) {
    errorMessage += "\nAssuming the component has a constant spectrum";
    return ComponentType::CONSTANT_SPECTRUM;
  } if (record.get("spectrum").type() != GlishValue::RECORD) {
    errorMessage += "\nThe 'spectrum' field must be a record";
    return ComponentType::UNKNOWN_SPECTRAL_SHAPE;
  }
  const GlishRecord spectrumRec = record.get("spectrum");
  if (!spectrumRec.exists("type")) {
    errorMessage += "\nThe spectrum record does not have a 'type' field";
    return ComponentType::UNKNOWN_SPECTRAL_SHAPE;
  }
  if (spectrumRec.get("type").type() != GlishValue::ARRAY) {
    errorMessage += "\nThe 'type' field cannot be a record";
    return ComponentType::UNKNOWN_SPECTRAL_SHAPE;
  }
  const GlishArray typeField = spectrumRec.get("type");
  if (typeField.elementType() != GlishArray::STRING) {
    errorMessage += "\nThe 'type' field must be a string";
    return ComponentType::UNKNOWN_SPECTRAL_SHAPE;
  }
  const IPosition shape = typeField.shape();
  if (shape.nelements() != 1 || shape.product() != 1) {
    errorMessage +="\nThe 'type' field must be a vector with only one element";
    return ComponentType::UNKNOWN_SPECTRAL_SHAPE;
  }
  String typeString;
  if (!typeField.get(typeString)) {
    errorMessage += "\nCould not read the 'type' field for an unknown reason";
    return ComponentType::UNKNOWN_SPECTRAL_SHAPE;
  }
  return ComponentType::spectralShape(typeString);
}

SkyComponent SkyComponent::copy() const {
  DebugAssert(ok(), AipsError);
  SkyComponent newComp(shape(), spectralShape());
  {
    newComp.flux() = flux().copy();
  }
  {
    MDirection thisDirection;
    direction(thisDirection);
    newComp.setDirection(thisDirection);
  }
  {
    Vector<Double> thisParameters(nParameters());
    parameters(thisParameters);
    newComp.setParameters(thisParameters);
  }
  {
    newComp.setRefFrequency(refFrequency());
  }
  {
    Vector<Double> thisParameters(nSpectralParameters());
    spectralParameters(thisParameters);
    newComp.setSpectralParameters(thisParameters);
  }
  {
    String thisLabel("");
    label(thisLabel);
    newComp.setLabel(thisLabel);
  }
  return newComp;
}

Bool SkyComponent::ok() const {
  if (itsCompPtr.null()) {
    LogIO logErr(LogOrigin("SkyComponent", "ok()"));
    logErr << LogIO::SEVERE << "Internal pointer is not pointing to anything"
           << LogIO::POST;
    return False;
  }
  if (!itsCompPtr->ok()) {
    LogIO logErr(LogOrigin("SkyComponent", "ok()"));
    logErr << LogIO::SEVERE << "Component representation is not ok"
           << LogIO::POST;
    return False;
  }
  return True;
}

SkyComponent::SkyComponent(SkyCompRep * rawPtr)
  :itsCompPtr(rawPtr) {
}

SkyCompRep * SkyComponent::rawPtr() {
  return &(*itsCompPtr);
}

const SkyCompRep * SkyComponent::rawPtr() const {
  return &(*itsCompPtr);
}

Bool SkyComponent::checkShape(String & errorMessage, 
			      const GlishRecord & record) const {
  ComponentType::Shape recordType = getShape(errorMessage, record);
  if (recordType == ComponentType::UNKNOWN_SHAPE) {
    return False;
  }
  if (recordType != shape()) {
    errorMessage += 
      String("\nThe record is for a component with a ") 
      + ComponentType::name(recordType)
      + String(" shape and cannot be assigned to a SkyComponent with a ")
      + ComponentType::name(shape()) + String(" shape");
    return False;
  }
  return True;
}

Bool SkyComponent::checkSpectralShape(String & errorMessage, 
				      const GlishRecord & record) const {
  ComponentType::SpectralShape recordType = 
    getSpectralShape(errorMessage, record);
  if (recordType == ComponentType::UNKNOWN_SPECTRAL_SHAPE) {
    return False;
  }
  if (recordType != spectralShape()) {
    errorMessage += 
      String("\nThe record is for a component with a ") 
      + ComponentType::name(recordType)
      + String(" spectrum and cannot be assigned to a SkyComponent with a ")
      + ComponentType::name(spectralShape()) + String(" spectrum");
    return False;
  }
  return True;
}
// Local Variables: 
// compile-command: "gmake OPTLIB=1 SkyComponent"
// End: 
