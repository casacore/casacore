//# SkyComponent.cc:  this defines SkyComponent
//# Copyright (C) 1996,1997
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
#include <trial/ComponentModels/PointCompRep.h>
#include <trial/Images/ImageInterface.h>
#include <aips/Arrays/Vector.h>
#include <aips/Exceptions/Error.h>
#include <aips/Logging/LogOrigin.h>
#include <aips/Logging/LogIO.h>
#include <aips/Measures/MDirection.h>
#include <aips/Utilities/Assert.h>

SkyComponent::SkyComponent()
  :theCompPtr(new PointCompRep) 
{
  AlwaysAssert(ok(), AipsError);
};

SkyComponent::SkyComponent(ComponentType::Type type) {
  switch (type){
  case ComponentType::POINT:
    theCompPtr = new PointCompRep; break;
  case ComponentType::GAUSSIAN:
    theCompPtr = new GaussianCompRep; break;
  };    
  AlwaysAssert(ok(), AipsError);
};

SkyComponent::SkyComponent(const SkyComponent & other) 
  :theCompPtr(other.theCompPtr)
{ 
  AlwaysAssert(ok(), AipsError);
};

SkyComponent::~SkyComponent() {
  DebugAssert(ok(), AipsError);
};

SkyComponent & SkyComponent::operator=(const SkyComponent & other) {
  if (this != &other)
    theCompPtr = other.theCompPtr;
  // This call to ok is restricted to the base class as the operator= function
  // is called by derived classes before the derived object is fully
  // constructed.
  DebugAssert(SkyComponent::ok(), AipsError);
  return *this;
};

void SkyComponent::sample(Vector<Double> & result, 
			  const MDirection & samplePos) const {
  theCompPtr->sample(result, samplePos);
  DebugAssert(ok(), AipsError);
};

void SkyComponent::project(ImageInterface<Float> & plane) const {
  theCompPtr->project(plane);
  DebugAssert(ok(), AipsError);
};

void SkyComponent::setFlux(const Vector<Double> & newFlux) {
  theCompPtr->setFlux(newFlux);
  DebugAssert(ok(), AipsError);
};

void SkyComponent::flux(Vector<Double> & compFlux) const {
  theCompPtr->flux(compFlux);
  DebugAssert(ok(), AipsError);
};

void SkyComponent::setPosition(const MDirection & newPos) {
  theCompPtr->setPosition(newPos);
  DebugAssert(ok(), AipsError);
};

void SkyComponent::position(MDirection & compPos) const {
  theCompPtr->position(compPos);
  DebugAssert(ok(), AipsError);
};

uInt SkyComponent::nParameters() const {
  DebugAssert(ok(), AipsError);
  return theCompPtr->nParameters();
};

void SkyComponent::setParameters(const Vector<Double> & newParms) {
  theCompPtr->setParameters(newParms);
  DebugAssert(ok(), AipsError);
};

void SkyComponent::parameters(Vector<Double> & compParms) const {
  theCompPtr->parameters(compParms);
  DebugAssert(ok(), AipsError);
};

ComponentType::Type SkyComponent::type() const {
  // This call to ok is restricted to the base class as the type() function is
  // called by derived classes before the derived object is fully constructed.
  DebugAssert(SkyComponent::ok(), AipsError);
  return theCompPtr->type();
};

SkyComponent SkyComponent::copy() const {
  DebugAssert(ok(), AipsError);
  SkyComponent newComp(type());
  {
    Vector<Double> thisFlux;
    flux(thisFlux);
    newComp.setFlux(thisFlux);
  }
  {
    MDirection thisPosition;
    position(thisPosition);
    newComp.setPosition(thisPosition);
  }
  {
    Vector<Double> thisParameters;
    parameters(thisParameters);
    newComp.setParameters(thisParameters);
  }
  return newComp;
};

Bool SkyComponent::ok() const {
  if (theCompPtr.null() == True) {
    LogIO logErr(LogOrigin("SkyComponent", "ok()"));
    logErr << LogIO::SEVERE << "Internal pointer is not pointing to anything"
           << LogIO::POST;
    return False;
  }
  if (theCompPtr->ok() == False) {
    LogIO logErr(LogOrigin("SkyComponent", "ok()"));
    logErr << LogIO::SEVERE << "Component representation is not ok"
           << LogIO::POST;
    return False;
  }
  return True;
};

SkyComponent::SkyComponent(SkyCompRep * rawPtr)
  :theCompPtr(rawPtr) {
};

SkyCompRep * SkyComponent::rawPtr() {
  return &(*theCompPtr);
};

const SkyCompRep * SkyComponent::rawPtr() const {
  return &(*theCompPtr);
};

// Local Variables: 
// compile-command: "gmake OPTLIB=1 SkyComponent"
// End: 
