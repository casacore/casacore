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
#include <trial/ComponentModels/SkyCompRep.h>
#include <trial/ComponentModels/Flux.h>
// #include <trial/ComponentModels/PointCompRep.h>
// #include <trial/ComponentModels/SIPointCompRep.h>
// #include <trial/ComponentModels/GaussianCompRep.h>
// #include <trial/Images/ImageInterface.h>
#include <aips/Arrays/Vector.h>
#include <aips/Exceptions/Error.h>
#include <aips/Containers/RecordInterface.h>
#include <aips/Logging/LogOrigin.h>
#include <aips/Logging/LogIO.h>
#include <aips/Measures/MDirection.h>
// #include <aips/Measures/MFrequency.h>
#include <aips/Measures/MVAngle.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/String.h>

SkyComponent::SkyComponent()
  :itsCompPtr(new SkyCompRep) 
{
  DebugAssert(ok(), AipsError);
}

SkyComponent::SkyComponent(ComponentType::Shape shape)   
  :itsCompPtr(new SkyCompRep(shape, ComponentType::CONSTANT_SPECTRUM))
{
  DebugAssert(ok(), AipsError);
}

SkyComponent::SkyComponent(ComponentType::Shape shape,
			   ComponentType::SpectralShape spectralModel) 
  :itsCompPtr(new SkyCompRep(shape, spectralModel))
{
  DebugAssert(ok(), AipsError);
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
  DebugAssert(ok(), AipsError);
  return *this;
}

Flux<Double> & SkyComponent::flux() {
  DebugAssert(ok(), AipsError);
  return itsCompPtr->flux();
}

const Flux<Double> & SkyComponent::flux() const {
  DebugAssert(ok(), AipsError);
  return itsCompPtr->flux();
}

ComponentType::Shape SkyComponent::shape() const {
  DebugAssert(ok(), AipsError);
  return itsCompPtr->shape();
}


void SkyComponent::setRefDirection(const MDirection & newDirection) {
  itsCompPtr->setRefDirection(newDirection);
  DebugAssert(ok(), AipsError);
}

const MDirection & SkyComponent::refDirection() const {
  DebugAssert(ok(), AipsError);
  return itsCompPtr->refDirection();
}

Flux<Double> SkyComponent::sample(const MDirection & sampleDir, 
				  const MVAngle & pixelSize) const {
  DebugAssert(ok(), AipsError);
  return itsCompPtr->sample(sampleDir, pixelSize);
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

const String & SkyComponent::label() const {
  return itsCompPtr->label();
  DebugAssert(ok(), AipsError);
}

uInt SkyComponent::nShapeParameters() const {
  DebugAssert(ok(), AipsError);
  return itsCompPtr->nShapeParameters();
}

void SkyComponent::setShapeParameters(const Vector<Double> & newParms) {
  itsCompPtr->setShapeParameters(newParms);
  DebugAssert(ok(), AipsError);
}

void SkyComponent::shapeParameters(Vector<Double> & compParms) const {
  itsCompPtr->shapeParameters(compParms);
  DebugAssert(ok(), AipsError);
}

ComponentType::SpectralShape SkyComponent::spectralShape() const {
  DebugAssert(ok(), AipsError);
  return itsCompPtr->spectralShape();
}

void SkyComponent::setRefFrequency(const MFrequency & newFrequency) {
  itsCompPtr->setRefFrequency(newFrequency);
  DebugAssert(ok(), AipsError);
}

const MFrequency & SkyComponent::refFrequency() const {
  DebugAssert(ok(), AipsError);
  return itsCompPtr->refFrequency();
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
 			      const RecordInterface & record) {
  DebugAssert(ok(), AipsError);
  return itsCompPtr->fromRecord(errorMessage, record);
}

Bool SkyComponent::toRecord(String & errorMessage,
 			    RecordInterface & record) const {
  DebugAssert(ok(), AipsError);
  return itsCompPtr->toRecord(errorMessage, record);
}

SkyComponent SkyComponent::copy() const {
  DebugAssert(ok(), AipsError);
  SkyComponent newComp(shape(), spectralShape());
  {
    newComp.flux() = flux().copy();
  }
  {
    newComp.setRefDirection(refDirection());
  }
  {
    Vector<Double> thisParameters(nShapeParameters());
    shapeParameters(thisParameters);
    newComp.setShapeParameters(thisParameters);
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
    newComp.setLabel(label());
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

// void SkyComponent::project(ImageInterface<Float> & plane) const {
//   itsCompPtr->project(plane);
//   DebugAssert(ok(), AipsError);
// }

// Local Variables: 
// compile-command: "gmake OPTLIB=1 SkyComponent"
// End: 
