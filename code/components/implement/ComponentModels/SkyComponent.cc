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

// Local Variables: 
// compile-command: "gmake OPTLIB=1 SkyComponent"
// End: 
