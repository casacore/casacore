//# PointShape.cc:
//# Copyright (C) 1998
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

#ifdef __GNUG__
typedef MeasConvert<MDirection,MVDirection,MCDirection> 
        gpp_measconvert_mdirection_mvdirection_mcdirection;
#endif

PointShape::PointShape() 
  :itsDir(),
   itsDirValue(itsDir.getValue()),
   itsRefFrame((MDirection::Types) itsDir.getRef().getType())
{
  DebugAssert(ok(), AipsError);
}

PointShape::PointShape(const MDirection & direction)
  :itsDir(direction),
   itsDirValue(itsDir.getValue()),
   itsRefFrame((MDirection::Types) itsDir.getRef().getType())
{
  DebugAssert(ok(), AipsError);
}

PointShape::PointShape(const PointShape & other)
  :itsDir(other.itsDir),
   itsDirValue(other.itsDirValue),
   itsRefFrame(other.itsRefFrame)
{
  DebugAssert(ok(), AipsError);
}

PointShape::~PointShape() {
  DebugAssert(ok(), AipsError);
}

PointShape & PointShape::operator=(const PointShape & other) {
  if (this != &other) {
    itsDir = other.itsDir;
    itsDirValue = other.itsDirValue;
    itsRefFrame = other.itsRefFrame;
  }
  DebugAssert(ok(), AipsError);
  return *this;
}

ComponentType::Shape PointShape::type() const {
  DebugAssert(ok(), AipsError);
  return ComponentType::POINT;
}

void PointShape::setRefDirection(const MDirection & newRefDir) {
  itsDir = newRefDir;
  itsDirValue = newRefDir.getValue();
  itsRefFrame = (MDirection::Types) newRefDir.getRef().getType();
  DebugAssert(ok(), AipsError);
}

const MDirection & PointShape::refDirection() const {
  DebugAssert(ok(), AipsError);
  return itsDir;
}

void PointShape::sample(Flux<Double> & flux, const MDirection & direction, 
			const MVAngle & pixelSize) const {
  DebugAssert(ok(), AipsError);
  MVDirection dirVal = direction.getValue();
  // Convert direction to the same frame as the reference direction
  if ((MDirection::Types) direction.getRef().getType() != itsRefFrame) {
    dirVal = MDirection::Convert(direction, itsRefFrame)().getValue();
  }
  if (!itsDirValue.near(dirVal, pixelSize.get()/2.0)) {
    flux.scaleValue(0.0, 0.0, 0.0, 0.0);
  }
}

void PointShape::visibility(Flux<Double> & flux, const Vector<Double> & uvw,
			    const Double & frequency) const {
  DebugAssert(ok(), AipsError);
  if (&frequency == 0) {}; // Suppress compiler warning about unused variable
  if (&uvw == 0) {}; // Suppress compiler warning about unused variable
  if (&flux == 0) {}; // Suppress compiler warning about unused variable
}

ComponentShape * PointShape::clone() const {
  DebugAssert(ok(), AipsError);
  ComponentShape * tmpPtr = new PointShape(*this);
  AlwaysAssert(tmpPtr != 0, AipsError);
  return tmpPtr;
}

uInt PointShape::nParameters() const {
  DebugAssert(ok(), AipsError);
  return 0;
}

void PointShape::setParameters(const Vector<Double> & newParms) {
  DebugAssert(newParms.nelements() == nParameters(), AipsError);
  DebugAssert(ok(), AipsError);
}

void PointShape::parameters(Vector<Double> & compParms) const {
  DebugAssert(ok(), AipsError);
  DebugAssert(compParms.nelements() == nParameters(), AipsError);
}

Bool PointShape::fromRecord(String & errorMessage,
			    const RecordInterface & record) {
  if (!ComponentShape::readDir(errorMessage, record)) return False;
  DebugAssert(ok(), AipsError);
  return True;
}

Bool PointShape::toRecord(String & errorMessage,
			  RecordInterface & record) const {
  DebugAssert(ok(), AipsError);
  record.define(RecordFieldId("type"), ComponentType::name(type()));
  if (!ComponentShape::addDir(errorMessage, record)) return False;
  return True;
}

Bool PointShape::convertUnit(String & errorMessage,
			     const RecordInterface & record) {
  // Suppress compiler warning about unused variables
  if (&errorMessage == 0) {}; 
  if (&record == 0) {};
  DebugAssert(ok(), AipsError);
  return True;
}

Bool PointShape::ok() const {
  return True;
}
// Local Variables: 
// compile-command: "gmake OPTLIB=1 PointShape"
// End: 
