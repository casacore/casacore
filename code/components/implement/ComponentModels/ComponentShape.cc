//# ComponentShape.cc:
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

#include <trial/ComponentModels/ComponentShape.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/Vector.h>
#include <aips/Containers/Record.h>
#include <aips/Containers/RecordFieldId.h>
#include <aips/Containers/RecordInterface.h>
#include <aips/Exceptions/Error.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Logging/LogIO.h>
#include <aips/Logging/LogOrigin.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MeasureHolder.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/DataType.h>
#include <aips/Utilities/String.h>

ComponentShape::ComponentShape() 
  :itsDir(),
   itsDirValue(itsDir.getValue()),
   itsRefFrame((MDirection::Types) itsDir.getRef().getType())
{
  DebugAssert(ok(), AipsError);
}

ComponentShape::ComponentShape(const MDirection& direction)
  :itsDir(direction),
   itsDirValue(itsDir.getValue()),
   itsRefFrame((MDirection::Types) itsDir.getRef().getType())
{
  DebugAssert(ok(), AipsError);
}

ComponentShape::ComponentShape(const ComponentShape& other)
  :itsDir(other.itsDir),
   itsDirValue(other.itsDirValue),
   itsRefFrame(other.itsRefFrame)
{
  DebugAssert(ok(), AipsError);
}


ComponentShape::~ComponentShape() {
}

ComponentShape& ComponentShape::operator=(const ComponentShape& other) {
  if (this != &other) {
    itsDir = other.itsDir;
    itsDirValue = other.itsDirValue;
    itsRefFrame = other.itsRefFrame;
  }
  DebugAssert(ok(), AipsError);
  return *this;
}

void ComponentShape::setRefDirection(const MDirection& newRefDir) {
  itsDir = newRefDir;
  itsDirValue = newRefDir.getValue();
  itsRefFrame = (MDirection::Types) newRefDir.getRef().getType();
  DebugAssert(ok(), AipsError);
}

const MDirection& ComponentShape::refDirection() const {
  DebugAssert(ok(), AipsError);
  return itsDir;
}

const MVDirection& ComponentShape::refDirValue() const {
  DebugAssert(ok(), AipsError);
  return itsDirValue;
}

const MDirection::Types& ComponentShape::refDirFrame() const {
  DebugAssert(ok(), AipsError);
  return itsRefFrame;
}

Bool ComponentShape::fromRecord(String& errorMessage,
				const RecordInterface& record) {
  ComponentType::Shape thisType = getType(errorMessage, record);
  if ( thisType != type()) {
    errorMessage += String("The 'type' field, in the shape record,") + 
      String(" is not the expected value of '") + 
      ComponentType::name(type()) + String("'\n");
  }
  if (!readDir(errorMessage, record)) return False;
  DebugAssert(ok(), AipsError);
  return True;
}

Bool ComponentShape::toRecord(String& errorMessage,
			      RecordInterface& record) const {
  record.define(RecordFieldId("type"), ComponentType::name(type()));
  if (!addDir(errorMessage, record)) return False;
  DebugAssert(ok(), AipsError);
  return True;
}

Bool ComponentShape::ok() const {
  if (!allNearAbs(itsDir.getValue().get(), itsDirValue.get(), 1E-10)) {
    LogIO logErr(LogOrigin("ComponentShape", "ok()"));
    logErr << LogIO::SEVERE 
	   << "Cached direction value does not agree with the actual value."
           << LogIO::POST;
    return False;
  }
  if ((MDirection::Types) itsDir.getRef().getType() != itsRefFrame) {
    LogIO logErr(LogOrigin("ComponentShape", "ok()"));
    logErr << LogIO::SEVERE 
	   << "Cached direction reference frame does not agree "
	   << "with the actual value."
           << LogIO::POST;
    return False;
  }
  return True;
}

ComponentType::Shape ComponentShape::getType(String& errorMessage,
					     const RecordInterface& record) {
  const String typeString("type");
  if (!record.isDefined(typeString)) {
    errorMessage += 
      String("The 'shape' record does not have a 'type' field.\n");
    return ComponentType::UNKNOWN_SHAPE;
  }
  const RecordFieldId type(typeString);
  if (record.dataType(type) != TpString) {
    errorMessage += String("The 'type' field, in the shape record,") + 
      String(" must be a String\n");
    return ComponentType::UNKNOWN_SHAPE;
  }      
  if (record.shape(type) != IPosition(1,1)) {
    errorMessage += String("The 'type' field, in the shape record,") + 
      String(" must have only 1 element\n");
    return ComponentType::UNKNOWN_SHAPE;
  }      
  const String& typeVal = record.asString(type);
  return ComponentType::shape(typeVal);
}

Bool ComponentShape::readDir(String& errorMessage,
			     const RecordInterface& record) {
  const String dirString("direction");
  if (!record.isDefined(dirString)) {
    errorMessage += "The 'direction' field does not exist\n";
    return False;
  }
  const RecordFieldId direction(dirString);
  if (record.dataType(direction) != TpRecord) {
    if ((record.dataType(direction) == TpString) && 
	(record.shape(direction) == IPosition(1,1)) &&
	(record.asString(direction) == String("current"))) {
      return True;
    } else {
      errorMessage += "The 'direction' field must be a record\n";
      errorMessage += "or the string 'current' (to use the current value)\n";
      return False;
    }
  }
  const Record& dirRecord = record.asRecord(direction);
  MeasureHolder mh;
  if (!mh.fromRecord(errorMessage, dirRecord)) {
    errorMessage += "Could not parse the reference direction\n";
    return False;
  }
  if (!mh.isMDirection()) {
    errorMessage += "The reference direction is not a direction measure\n";
    return False;
  }
  setRefDirection(mh.asMDirection());
  return True;
}

Bool ComponentShape::addDir(String& errorMessage, 
			    RecordInterface& record) const {
  record.define(RecordFieldId("type"), ComponentType::name(type()));
  Record dirRecord;
  const MeasureHolder mh(refDirection());
  if (!mh.toRecord(errorMessage, dirRecord)) {
    errorMessage += "Could not convert the reference direction to a record\n";
    return False;
  }
  record.defineRecord(RecordFieldId("direction"), dirRecord);
  return True;
}

// Local Variables: 
// compile-command: "gmake OPTLIB=1 ComponentShape"
// End: 
