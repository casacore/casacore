//# SkyComponent.cc:  this defines SkyComponent
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2003
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

#include <components/ComponentModels/SkyComponent.h>
#include <components/ComponentModels/ComponentShape.h>
#include <components/ComponentModels/Flux.h>
#include <components/ComponentModels/SkyCompRep.h>
#include <components/ComponentModels/SpectralModel.h>
#include <casa/Arrays/Vector.h>
#include <casa/Containers/RecordInterface.h>
#include <casa/Exceptions/Error.h>
#include <casa/Logging/LogIO.h>
#include <casa/Logging/LogOrigin.h>
#include <measures/Measures/MDirection.h>
#include <measures/Measures/MFrequency.h>
#include <casa/Quanta/MVAngle.h>
#include <casa/Utilities/Assert.h>
#include <casa/BasicSL/String.h>

namespace casa { //# NAMESPACE CASA - BEGIN

SkyComponent::SkyComponent()
  :itsCompPtr(new SkyCompRep) 
{
  DebugAssert(ok(), AipsError);
}

SkyComponent::SkyComponent(const ComponentType::Shape& shape)   
  :itsCompPtr(new SkyCompRep(shape))
{
  DebugAssert(ok(), AipsError);
}

SkyComponent::SkyComponent(const ComponentType::Shape& shape,
			   const ComponentType::SpectralShape& spectralModel) 
  :itsCompPtr(new SkyCompRep(shape, spectralModel))
{
  DebugAssert(ok(), AipsError);
}

SkyComponent::SkyComponent(const Flux<Double>& flux,
			   const ComponentShape& shape, 
			   const SpectralModel& spectrum)
  :itsCompPtr(new SkyCompRep(flux, shape, spectrum))
{
  DebugAssert(ok(), AipsError);
}

SkyComponent::SkyComponent(const SkyComponent& other) 
  :SkyCompBase(other),
   itsCompPtr (other.itsCompPtr)
{ 
  DebugAssert(ok(), AipsError);
}

SkyComponent::~SkyComponent() {
  DebugAssert(ok(), AipsError);
}

SkyComponent& SkyComponent::operator=(const SkyComponent& other) {
  if (this != &other)
    itsCompPtr = other.itsCompPtr;
  DebugAssert(ok(), AipsError);
  return *this;
}

Flux<Double>& SkyComponent::flux() {
  DebugAssert(ok(), AipsError);
  return itsCompPtr->flux();
}

const Flux<Double>& SkyComponent::flux() const {
  DebugAssert(ok(), AipsError);
  return itsCompPtr->flux();
}

const ComponentShape& SkyComponent::shape() const {
  DebugAssert(ok(), AipsError);
  return itsCompPtr->shape();
}

ComponentShape& SkyComponent::shape() {
  DebugAssert(ok(), AipsError);
  return itsCompPtr->shape();
}

void SkyComponent::setShape(const ComponentShape& newShape) {
  DebugAssert(ok(), AipsError);
  itsCompPtr->setShape(newShape);
}

const SpectralModel& SkyComponent::spectrum() const {
  DebugAssert(ok(), AipsError);
  return itsCompPtr->spectrum();
}

SpectralModel& SkyComponent::spectrum() {
  DebugAssert(ok(), AipsError);
  return itsCompPtr->spectrum();
}

void SkyComponent::setSpectrum(const SpectralModel& newSpectrum) {
  DebugAssert(ok(), AipsError);
  itsCompPtr->setSpectrum(newSpectrum);
}

String& SkyComponent::label() {
  DebugAssert(ok(), AipsError);
  return itsCompPtr->label();
}

const String& SkyComponent::label() const {
  DebugAssert(ok(), AipsError);
  return itsCompPtr->label();
}

Bool SkyComponent::isPhysical() const {
  DebugAssert(ok(), AipsError);
  return itsCompPtr->isPhysical();
}

Flux<Double> SkyComponent::sample(const MDirection& direction, 
			      const MVAngle& pixelLatSize, 
			      const MVAngle& pixelLongSize, 
			      const MFrequency& centerFrequency) const {
  DebugAssert(ok(), AipsError);
  return itsCompPtr->sample(direction, pixelLatSize, pixelLongSize, 
			    centerFrequency);
}

void SkyComponent::sample(Cube<Double>& samples, const Unit& reqUnit,
			  const Vector<MVDirection>& directions, 
			  const MeasRef<MDirection>& dirRef, 
			  const MVAngle& pixelLatSize, 
			  const MVAngle& pixelLongSize, 
			  const Vector<MVFrequency>& frequencies,
			  const MeasRef<MFrequency>& freqRef) const {
  DebugAssert(ok(), AipsError);
  itsCompPtr->sample(samples, reqUnit,
		     directions, dirRef, pixelLatSize, pixelLongSize,
		     frequencies, freqRef);
}

Flux<Double> SkyComponent::visibility(const Vector<Double>& uvw,
 				      const Double& frequency) const {
  DebugAssert(ok(), AipsError);
  return itsCompPtr->visibility(uvw, frequency);
}

void SkyComponent::visibility(Cube<DComplex>& visibilities,
			      const Matrix<Double>& uvws,
			      const Vector<Double>& frequencies) const {
  DebugAssert(ok(), AipsError);
  itsCompPtr->visibility(visibilities, uvws, frequencies);
}

Bool SkyComponent::fromRecord(String& errorMessage, 
 			      const RecordInterface& record) {
  DebugAssert(ok(), AipsError);
  return itsCompPtr->fromRecord(errorMessage, record);
}

Bool SkyComponent::toRecord(String& errorMessage,
 			    RecordInterface& record) const {
  DebugAssert(ok(), AipsError);
  return itsCompPtr->toRecord(errorMessage, record);
}

SkyComponent SkyComponent::copy() const {
  DebugAssert(ok(), AipsError);
  SkyComponent newComp(flux().copy(), shape(), spectrum());
  newComp.label() = label();
  return newComp;
}

void SkyComponent::fromPixel (Double& fluxRatio, const Vector<Double>& parameters,
                              const Unit& brightnessUnitIn,
                              const Vector<Quantum<Double> >& restoringBeam,
                              const CoordinateSystem& cSys,
                              ComponentType::Shape componentShape,
                              Stokes::StokesTypes stokes)
{
   itsCompPtr->fromPixel(fluxRatio, parameters, brightnessUnitIn, restoringBeam,
                         cSys, componentShape, stokes);
}

Vector<Double> SkyComponent::toPixel (const Unit& brightnessUnitIn,
                                      const Vector<Quantum<Double> >& restoringBeam,
                                      const CoordinateSystem& cSys,
                                      Stokes::StokesTypes stokes) const
{
   return itsCompPtr->toPixel(brightnessUnitIn, restoringBeam,
                              cSys, stokes);
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
// Local Variables: 
// compile-command: "gmake SkyComponent"
// End: 

} //# NAMESPACE CASA - END

