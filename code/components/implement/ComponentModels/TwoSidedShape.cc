//# TwoSidedShape.cc:
//# Copyright (C) 1999,2000
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

#include <trial/ComponentModels/TwoSidedShape.h>
#include <aips/Arrays/Vector.h>
#include <aips/Containers/Record.h>
#include <aips/Containers/RecordFieldId.h>
#include <aips/Containers/RecordInterface.h>
#include <aips/Exceptions/Error.h>
#include <aips/Logging/LogIO.h>
#include <aips/Logging/LogOrigin.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Quanta/Quantum.h>
#include <aips/Quanta/QuantumHolder.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/String.h>

TwoSidedShape::~TwoSidedShape() {
  DebugAssert(ok(), AipsError);
}

TwoSidedShape& TwoSidedShape::operator=(const TwoSidedShape& other) {
  if (this != &other) {
    ComponentShape::operator=(other);
    itsMajUnit = other.itsMajUnit;
    itsMinUnit = other.itsMinUnit;
    itsPaUnit = other.itsPaUnit;
  }
  DebugAssert(ok(), AipsError);
  return *this;
}

void TwoSidedShape::setWidth(const Quantum<Double>& majorAxis,
			     const Quantum<Double>& minorAxis, 
			     const Quantum<Double>& positionAngle) {
  itsMajUnit = majorAxis.getFullUnit();
  itsMinUnit = minorAxis.getFullUnit();
  itsPaUnit = positionAngle.getFullUnit();
  const Unit rad("rad");
  setWidthInRad(majorAxis.getValue(rad), minorAxis.getValue(rad), 
		positionAngle.getValue(rad));
  DebugAssert(ok(), AipsError);
}

void TwoSidedShape::setWidth(const Quantum<Double>& majorAxis,
			     const Double axialRatio, 
			     const Quantum<Double>& positionAngle) {
  itsMinUnit = itsMajUnit = majorAxis.getFullUnit();
  itsPaUnit = positionAngle.getFullUnit();
  const Unit rad("rad");
  const Double majWidth = majorAxis.getValue(rad);
  setWidthInRad(majWidth, majWidth*axialRatio, positionAngle.getValue(rad));
  DebugAssert(ok(), AipsError);
}

Quantum<Double> TwoSidedShape::majorAxis() const {
  Quantum<Double> retVal(majorAxisInRad(), "rad");
  retVal.convert(itsMajUnit);
  return retVal;
}

Quantum<Double> TwoSidedShape::minorAxis() const {
  Quantum<Double> retVal(minorAxisInRad(), "rad");
  retVal.convert(itsMinUnit);
  return retVal;
}

Quantum<Double> TwoSidedShape::positionAngle() const {
  Quantum<Double> retVal(positionAngleInRad(), "rad");
  retVal.convert(itsPaUnit);
  return retVal;
}

Double TwoSidedShape::axialRatio() const {
  return minorAxisInRad()/majorAxisInRad();
}

void TwoSidedShape::sample(Vector<Double>& scale, 
			   const Vector<MDirection::MVType>& directions, 
			   const MDirection::Ref& refFrame,
			   const MVAngle& pixelLatSize,
			   const MVAngle& pixelLongSize) const {
  ComponentShape::sample(scale, directions, refFrame, pixelLatSize,
			 pixelLongSize);
}

void TwoSidedShape::visibility(Vector<DComplex>& scale,
			       const Matrix<Double>& uvw,
			       const Double& frequency) const {
  ComponentShape::visibility(scale, uvw, frequency);
}

Bool TwoSidedShape::isSymmetric() const {
  DebugAssert(ok(), AipsError);
  return True;
}

uInt TwoSidedShape::nParameters() const {
  DebugAssert(ok(), AipsError);
  return 3;
}

void TwoSidedShape::setParameters(const Vector<Double>& newParms) {
  DebugAssert(newParms.nelements() == nParameters(), AipsError);
  DebugAssert(newParms(0) >= newParms(1), AipsError);
  DebugAssert(abs(newParms(2)) <= C::_2pi, AipsError);
  setWidthInRad(newParms(0), newParms(1), newParms(2));
  DebugAssert(ok(), AipsError);
}

void TwoSidedShape::parameters(Vector<Double>& compParms) const {
  DebugAssert(compParms.nelements() == nParameters(), AipsError);
  DebugAssert(ok(), AipsError);
  compParms(0) = majorAxisInRad();
  compParms(1) = minorAxisInRad();
  compParms(2) = positionAngleInRad();
}

Bool TwoSidedShape::fromRecord(String& errorMessage,
			       const RecordInterface& record) {
  if (!ComponentShape::fromRecord(errorMessage, record)) return False;
  const Unit rad("rad");
  Quantum<Double> majorAxis;
  {
    const String fieldString("majoraxis");
    if (!record.isDefined(fieldString)) {
      errorMessage += "The 'majoraxis' field does not exist\n";
      return False;
    }
    const RecordFieldId field(fieldString);
    if (!(record.dataType(field) == TpRecord || 
	  ((record.dataType(field) == TpString) && 
	   (record.shape(field) == IPosition(1,1))))) {
      errorMessage += "The 'majoraxis' field must be a record\n";
      errorMessage += "or a string (but not a vector of strings)\n";
      return False;
    }
    QuantumHolder qHolder;
    if (record.dataType(field) == TpString) {
      if (!qHolder.fromString(errorMessage, record.asString(field))) {
	errorMessage += "Problem parsing the 'majoraxis' string\n";
	return False;
      }
    } else if (!qHolder.fromRecord(errorMessage, record.asRecord(field))) {
      errorMessage += "Problem parsing the 'majoraxis' record\n";
      return False;
    }
    if (!(qHolder.isScalar() && qHolder.isReal())) {
      errorMessage += "The 'majoraxis' field is not a quantity\n";
      return False;
    }
    majorAxis = qHolder.asQuantumDouble();
    if (majorAxis.getFullUnit() != rad) {
      errorMessage += "The 'majoraxis' field must have angular units\n";
      return False;
    }
  }
  Quantum<Double> minorAxis;
  {
    const String fieldString("minoraxis");
    if (!record.isDefined(fieldString)) {
      errorMessage += "The 'minoraxis' field does not exist\n";
      return False;
    }
    const RecordFieldId field(fieldString);
    if (!(record.dataType(field) == TpRecord || 
	  ((record.dataType(field) == TpString) && 
	   (record.shape(field) == IPosition(1,1))))) {
      errorMessage += "The 'minoraxis' field must be a record\n";
      errorMessage += "or a string (but not a vector of strings)\n";
      return False;
    }      
    QuantumHolder qHolder;
    if (record.dataType(field) == TpString) {
      if (!qHolder.fromString(errorMessage, record.asString(field))) {
	errorMessage += "Problem parsing the 'minoraxis' string\n";
	return False;
      }
    } else if (!qHolder.fromRecord(errorMessage, record.asRecord(field))) {
      errorMessage += "Problem parsing the 'minoraxis' record\n";
      return False;
    }
    if (!(qHolder.isScalar() && qHolder.isReal())) {
      errorMessage += "The 'minoraxis' field is not a quantity\n";
      return False;
    }
    minorAxis = qHolder.asQuantumDouble();
    if (minorAxis.getFullUnit() != rad) {
      errorMessage += "The 'minoraxis' field must have angular units\n";
      return False;
    }
  }
  Quantum<Double> pa;
  {
    const String fieldString("positionangle");
    if (!record.isDefined(fieldString)) {
      errorMessage += "The 'positionangle' field does not exist\n";
      return False;
    }
    const RecordFieldId field(fieldString);
    if (!(record.dataType(field) == TpRecord || 
	  ((record.dataType(field) == TpString) && 
	   (record.shape(field) == IPosition(1,1))))) {
      errorMessage += "The 'positionangle' field must be a record\n";
      errorMessage += "or a string (but not a vector of strings)\n";
      return False;
    }      
    QuantumHolder qHolder;
    if (record.dataType(field) == TpString) {
      if (!qHolder.fromString(errorMessage, record.asString(field))) {
	errorMessage += "Problem parsing the 'positionangle' string\n";
	return False;
      }
    } else if (!qHolder.fromRecord(errorMessage, record.asRecord(field))) {
      errorMessage += "Problem parsing the 'positionangle' record\n";
      return False;
    }
    if (!(qHolder.isScalar() && qHolder.isReal())) {
      errorMessage += "The 'positionangle' field is not a quantity\n";
      return False;
    }
    pa = qHolder.asQuantumDouble();
    if (pa.getFullUnit() != rad) {
      errorMessage += "The 'positionangle' field must have angular units\n";
      return False;
    }
  }
  const Double minorRad = minorAxis.getValue(rad);
  const Double majorRad = majorAxis.getValue(rad);
  if (majorRad < minorRad) {
    if (near(majorRad, minorRad, 1E-9)) {
// assume they are meant to be the same and precision has got lost somewhere. 
      majorAxis.setValue(minorAxis.getValue(majorAxis.getFullUnit()));
    } else {
      errorMessage += "The major axis cannot be smaller than the minor axis\n";
      return False;
    }
  }
  setWidth(majorAxis, minorAxis, pa);
  DebugAssert(ok(), AipsError);
  return True;
}

Bool TwoSidedShape::toRecord(String& errorMessage,
			     RecordInterface& record) const {
  DebugAssert(ok(), AipsError);
  if (!ComponentShape::toRecord(errorMessage, record)) return False;
  {
    const QuantumHolder qHolder(majorAxis());
    Record qRecord;
    if (!qHolder.toRecord(errorMessage, qRecord)) {
      errorMessage += "Cannot convert the major axis to a record\n";
      return False;
    }
    record.defineRecord(RecordFieldId("majoraxis"), qRecord);
  }
  {
    const QuantumHolder qHolder(minorAxis());
    Record qRecord;
    if (!qHolder.toRecord(errorMessage, qRecord)) {
      errorMessage += "Cannot convert the minor axis to a record\n";
      return False;
    }
    record.defineRecord(RecordFieldId("minoraxis"), qRecord);
  }
  {
    const QuantumHolder qHolder(positionAngle());
    Record qRecord;
    if (!qHolder.toRecord(errorMessage, qRecord)) {
      errorMessage += "Cannot convert the position angle to a record\n";
      return False;
    }
    record.defineRecord(RecordFieldId("positionangle"), qRecord);
  }
  return True;
}

Bool TwoSidedShape::convertUnit(String& errorMessage,
 				const RecordInterface& record) {
  const Unit deg("deg");
  {
    const String fieldString("majoraxis");
    if (!record.isDefined(fieldString)) {
      errorMessage += "The 'majoraxis' field does not exist\n";
      return False;
    }
    const RecordFieldId field(fieldString);
    if (!((record.dataType(field) == TpString) && 
	  (record.shape(field) == IPosition(1,1)))) {
      errorMessage += "The 'majoraxis' field must be a string\n";
      errorMessage += "(but not a vector of strings)\n";
      return False;
    }
    const Unit unit = Unit(record.asString(field));
    if (unit != deg) {
      errorMessage += 
	"Cannot convert the major axis width to a non angular unit";
      return False;
    }
    itsMajUnit = unit;
  }
  {
    const String fieldString("minoraxis");
    if (!record.isDefined(fieldString)) {
      errorMessage += "The 'minoraxis' field does not exist\n";
      return False;
    }
    const RecordFieldId field(fieldString);
    if (!((record.dataType(field) == TpString) && 
	  (record.shape(field) == IPosition(1,1)))) {
      errorMessage += "The 'minoraxis' field must be a string\n";
      errorMessage += "(but not a vector of strings)\n";
      return False;
    }
    const Unit unit = Unit(record.asString(field));
    if (unit != deg) {
      errorMessage += 
	"Cannot convert the minor axis width to a non angular unit";
      return False;
    }
    itsMinUnit = unit;
  }
  {
    const String fieldString("positionangle");
    if (!record.isDefined(fieldString)) {
      errorMessage += "The 'positionangle' field does not exist\n";
      return False;
    }
    const RecordFieldId field(fieldString);
    if (!((record.dataType(field) == TpString) && 
	  (record.shape(field) == IPosition(1,1)))) {
      errorMessage += "The 'positionangle' field must be a string\n";
      errorMessage += "(but not a vector of strings)\n";
      return False;
    }
    const Unit unit = Unit(record.asString(field));
    if (unit != deg) {
      errorMessage += 
	"Cannot convert the position angle to a non angular unit";
      return False;
    }
    itsPaUnit = unit;
  }
  DebugAssert(ok(), AipsError);
  return True;
}

Bool TwoSidedShape::ok() const {
  // The LogIO class is only constructed if an error is detected for
  // performance reasons. Both function static and file static variables
  // where considered and rejected for this purpose.
  if (!ComponentShape::ok()) return False;
  const Unit deg("deg");
  if (itsMajUnit != deg) {
    LogIO logErr(LogOrigin("TwoSidedCompRep", "ok()"));
    logErr << LogIO::SEVERE << "The major axis does not have angular units."
           << LogIO::POST;
    return False;
  }
  if (itsMinUnit != deg) {
    LogIO logErr(LogOrigin("TwoSidedCompRep", "ok()"));
    logErr << LogIO::SEVERE << "The minor axis does not have angular units."
           << LogIO::POST;
    return False;
  }
  if (itsPaUnit != deg) {
    LogIO logErr(LogOrigin("TwoSidedCompRep", "ok()"));
    logErr << LogIO::SEVERE <<"The position angle does not have angular units."
           << LogIO::POST;
    return False;
  }
  return True;
}

TwoSidedShape::TwoSidedShape()
  :ComponentShape(),
   itsMajUnit("arcmin"),
   itsMinUnit("arcmin"),
   itsPaUnit("deg")
{
  DebugAssert(ok(), AipsError);
}

TwoSidedShape::TwoSidedShape(const MDirection& direction, 
			     const Unit& majorAxisUnit,
			     const Unit& minorAxisUnit, 
			     const Unit& paUnit) 
  :ComponentShape(direction),
   itsMajUnit(majorAxisUnit),
   itsMinUnit(minorAxisUnit),
   itsPaUnit(paUnit)
{
}

TwoSidedShape::TwoSidedShape(const TwoSidedShape& other) 
  :ComponentShape(other),
   itsMajUnit(other.itsMajUnit),
   itsMinUnit(other.itsMinUnit),
   itsPaUnit(other.itsPaUnit)
{
  DebugAssert(ok(), AipsError);
}

// Local Variables: 
// compile-command: "gmake OPTLIB=1 TwoSidedShape"
// End: 
