//# PointShape.cc:
//# Copyright (C) 1998,1999
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

#include <trial/ComponentModels/PointShape.h>
#include <trial/ComponentModels/Flux.h>
#include <aips/Arrays/Vector.h>
#include <aips/Containers/RecordInterface.h>
#include <aips/Containers/RecordFieldId.h>
#include <aips/Exceptions/Error.h>
#include <aips/Mathematics/Complex.h>
#include <aips/Measures/MCDirection.h>
#include <aips/Quanta/MVAngle.h>
#include <aips/Measures/MeasConvert.h>
#include <aips/Quanta/Quantum.h>
#include <aips/Quanta/QMath.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/String.h>

PointShape::PointShape() 
  :ComponentShape()
{
  DebugAssert(ok(), AipsError);
}

PointShape::PointShape(const MDirection& direction)
  :ComponentShape(direction)
{
  DebugAssert(ok(), AipsError);
}

PointShape::PointShape(const PointShape& other)
  :ComponentShape(other)
{
  DebugAssert(ok(), AipsError);
}

PointShape::~PointShape() {
  DebugAssert(ok(), AipsError);
}

PointShape& PointShape::operator=(const PointShape& other) {
  ComponentShape::operator=(other);
  DebugAssert(ok(), AipsError);
  return *this;
}

ComponentType::Shape PointShape::type() const {
  DebugAssert(ok(), AipsError);
  return ComponentType::POINT;
}

void PointShape::sample(Flux<Double>& flux, const MDirection& direction, 
			const MVAngle& pixelSize) const {
  DebugAssert(ok(), AipsError);
  // Convert direction to the same frame as the reference direction
  if ((MDirection::Types) direction.getRef().getType() != refDirFrame()) {
    const MVDirection dirVal = 
      MDirection::Convert(direction, refDirFrame())().getValue();
    if (refDirValue().separation(dirVal) > pixelSize.radian()/2.0) {
      flux.scaleValue(0.0, 0.0, 0.0, 0.0);
    }
  } else {
    if (refDirValue().separation(direction.getValue()) > 
	pixelSize.radian()/2.0) {
      flux.scaleValue(0.0, 0.0, 0.0, 0.0);
    }
  }
}

void PointShape::multiSample(Vector<Double>& scale, 
			     const Vector<MVDirection>& directions, 
			     const MVAngle& pixelSize) const {
  DebugAssert(ok(), AipsError);
  const uInt nSamples = directions.nelements();
  if (scale.nelements() == 0) scale.resize(nSamples);
  DebugAssert(scale.nelements() == nSamples, AipsError);

  Double separation;
  const Double pixSize = pixelSize.radian()/2.0;
  for (uInt i = 0; i < nSamples; i++) {
    separation = refDirValue().separation(directions(i));
    if (separation < pixSize) {
      scale(i) = 1.0;
    } else {
      scale(i) = 0.0;
    }
  }
}

void PointShape::visibility(Flux<Double>&, const Vector<Double>&,
			    const Double&) const {
  // Nothing needs to be done!
  DebugAssert(ok(), AipsError);
}

ComponentShape* PointShape::clone() const {
  DebugAssert(ok(), AipsError);
  ComponentShape* tmpPtr = new PointShape(*this);
  AlwaysAssert(tmpPtr != 0, AipsError);
  return tmpPtr;
}

uInt PointShape::nParameters() const {
  DebugAssert(ok(), AipsError);
  return 0;
}

void PointShape::setParameters(const Vector<Double>& newParms) {
  DebugAssert(newParms.nelements() == nParameters(), AipsError);
  DebugAssert(ok(), AipsError);
  // Suppress compiler warning about unused variable
  if (&newParms == 0) {}; 
}

void PointShape::parameters(Vector<Double>& compParms) const {
  DebugAssert(ok(), AipsError);
  DebugAssert(compParms.nelements() == nParameters(), AipsError);
  // Suppress compiler warning about unused variable
  if (&compParms == 0) {}; 
}

Bool PointShape::fromRecord(String& errorMessage,
			    const RecordInterface& record) {
  DebugAssert(ok(), AipsError);
  return ComponentShape::fromRecord(errorMessage, record);
}

Bool PointShape::toRecord(String& errorMessage,
			  RecordInterface& record) const {
  DebugAssert(ok(), AipsError);
  return ComponentShape::toRecord(errorMessage, record);
}

Bool PointShape::convertUnit(String&, const RecordInterface&) {
  DebugAssert(ok(), AipsError);
  return True;
}

Bool PointShape::ok() const {
  return ComponentShape::ok();
}
// Local Variables: 
// compile-command: "gmake OPTLIB=1 PointShape"
// End: 
