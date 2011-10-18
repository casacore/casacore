//# PointShape.cc:
//# Copyright (C) 1998,1999,2000
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

#include <components/ComponentModels/PointShape.h>
#include <components/ComponentModels/Flux.h>
#include <casa/Arrays/Vector.h>
#include <casa/Containers/RecordFieldId.h>
#include <casa/Containers/RecordInterface.h>
#include <casa/Exceptions/Error.h>
#include <casa/BasicSL/Complex.h>
#include <casa/BasicMath/Math.h>
#include <measures/Measures/MCDirection.h>
#include <measures/Measures/MeasConvert.h>
#include <casa/Quanta/MVAngle.h>
#include <casa/Quanta/QMath.h>
#include <casa/Quanta/Quantum.h>
#include <casa/Utilities/Assert.h>
#include <casa/BasicSL/String.h>

namespace casa { //# NAMESPACE CASA - BEGIN

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

Double PointShape::sample(const MDirection& direction, 
			  const MVAngle& pixelLatSize,
			  const MVAngle& pixelLongSize) const {
  DebugAssert(ok(), AipsError);
  const MDirection& compDir(refDirection());
  const MDirection::Ref& compDirFrame(compDir.getRef());
  const MDirection::MVType* compDirValue = &(compDir.getValue());
  Bool deleteValue = False;
  // Convert direction to the same frame as the reference direction
  if (ComponentShape::differentRefs(direction.getRef(), compDirFrame)) {
    compDirValue = new MDirection::MVType
      (MDirection::Convert(compDir, direction.getRef())().getValue());
    deleteValue = True;
  }
//
  const Double latSize = abs(pixelLatSize.radian());
  const Double longSize = abs(pixelLongSize.radian());
  const Double nearSize = max(latSize, longSize);
//
  const MDirection::MVType& dirValue = direction.getValue();
//
  Double retVal = dirIsInPixel (longSize, latSize, nearSize, dirValue, compDirValue);
//
  if (deleteValue) delete compDirValue;
  return retVal;
}


void PointShape::sample(Vector<Double>& scale, 
			const Vector<MDirection::MVType>& directions, 
			const MDirection::Ref& refFrame, 
			const MVAngle& pixelLatSize,
			const MVAngle& pixelLongSize) const 
{
  DebugAssert(ok(), AipsError);
  const uInt nSamples = directions.nelements();
  DebugAssert(scale.nelements() == nSamples, AipsError);
  
  const MDirection& compDir(refDirection());
  const MDirection::Ref& compDirFrame(compDir.getRef());
  const MDirection::MVType* compDirValue = &(compDir.getValue());
  Bool deleteValue = False;
  // Convert direction to the same frame as the reference direction
  if (refFrame != compDirFrame) {
    compDirValue = new MDirection::MVType
      (MDirection::Convert(compDir, refFrame)().getValue());
    deleteValue = True;
  }
//
  const Double longSize = abs(pixelLongSize.radian());
  const Double latSize = abs(pixelLatSize.radian());
  const Double nearSize = max(latSize, longSize);
  scale = 0.0;
//
  for (uInt i = 0; i < nSamples; i++) {
    const MDirection::MVType& dirValue = directions(i);
    scale(i) = dirIsInPixel (longSize, latSize, nearSize, dirValue, compDirValue);
  }
  if (deleteValue) delete compDirValue;
}

DComplex PointShape::visibility(const Vector<Double>&,
				const Double&) const {
  DebugAssert(ok(), AipsError);
  return DComplex(1.0, 0.0);
}

void PointShape::visibility(Vector<DComplex>& scale, 
			    const Matrix<Double>&,
			    const Double&) const {
  DebugAssert(ok(), AipsError);
  scale = DComplex(1.0, 0.0);
}

void PointShape::visibility(Matrix<DComplex>& scale, const Matrix<Double>& uvw,
			    const Vector<Double> & freq) const {
  DebugAssert(ok(), AipsError);
  scale.resize(uvw.ncolumn(), freq.nelements());
  scale = DComplex(1.0, 0.0);
}

Bool PointShape::isSymmetric() const {
  DebugAssert(ok(), AipsError);
  return True;
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

Vector<Double> PointShape::parameters() const {
  DebugAssert(ok(), AipsError);
  return Vector<Double>(0);
}

void PointShape::setErrors(const Vector<Double>& newErrors) {
  DebugAssert(newErrors.nelements() == nParameters(), AipsError);
  DebugAssert(ok(), AipsError);
  // Suppress compiler warning about unused variable
  if (&newErrors == 0) {}; 
}

Vector<Double> PointShape::errors() const {
  DebugAssert(ok(), AipsError);
  return Vector<Double>(0);
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

const ComponentShape* PointShape::getPtr() const {
    return this;
}


Double PointShape::dirIsInPixel (Double longSize, Double latSize, Double nearSize,
                                 const MDirection::MVType& dirValue,
                                 const MDirection::MVType* compDirValue) const
{                                 
  Double retVal = 0.0;
//
  const Double separation = abs(compDirValue->separation(dirValue));
  if (separation <= nearSize) {
     const Double pa = compDirValue->positionAngle(dirValue);
//
     if (abs(separation*sin(pa)) <= longSize/2.0 &&
         abs(separation*cos(pa)) <= latSize/2.0) {
       retVal = 1.0;
     }
  }
  return retVal;
}

String PointShape::sizeToString() const {
	return "Point";
}



// Local Variables: 
// compile-command: "gmake PointShape"
// End: 

} //# NAMESPACE CASA - END

