//# ComponentShape.cc:
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

#include <trial/ComponentModels/ComponentShape.h>
#include <trial/Measures/DOmeasures.h>
#include <trial/Tasking/MeasureParameterAccessor.h>
#include <trial/Tasking/ParameterSet.h>
#include <aips/Containers/Record.h>
#include <aips/Containers/RecordFieldId.h>
#include <aips/Containers/RecordInterface.h>
#include <aips/Exceptions/Error.h>
#include <aips/Glish/GlishRecord.h>
#include <aips/Measures/MDirection.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/String.h>

ComponentShape::~ComponentShape() {
  DebugAssert(ok(), AipsError);
}

void ComponentShape::refDirection(MDirection & refDir) const {
  DebugAssert(ok(), AipsError);
  refDir = refDirection();
}

void ComponentShape::scale(Double & scaleFactor, const MDirection & direction, 
			   const MVAngle & pixelSize) const {
  DebugAssert(ok(), AipsError);
  scaleFactor = scale(direction, pixelSize);
}

Bool ComponentShape::readDir(String & errorMessage,
			     const RecordInterface & record) {
  GlishRecord gRecord;
  gRecord.fromRecord(record);
  MeasureParameterAccessor<MDirection> mpa(String("direction"),
					   ParameterSet::In, &gRecord);
  if (!mpa.copyIn(errorMessage)) return False;
  setRefDirection(mpa());
  return True;
}

Bool ComponentShape::addDir(String & errorMessage, 
			    RecordInterface & record) const {
  if (errorMessage == ""); // Suppress compiler warning about unused variable
  GlishRecord gDirRecord = measures::toRecord(refDirection());
  Record dirRecord;
  gDirRecord.toRecord(dirRecord);
  record.defineRecord(RecordFieldId("direction"), dirRecord);
  return True;
}

// Local Variables: 
// compile-command: "gmake OPTLIB=1 ComponentShape"
// End: 
