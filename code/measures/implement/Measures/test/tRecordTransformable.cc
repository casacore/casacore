//# tRecordTransformable.cc:
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

#include <aips/aips.h>
#include <trial/Measures/MeasureHolder.h>
#include <trial/Utilities/RecordTransformable.h>
#include <aips/Containers/Record.h>
#include <aips/Containers/RecordInterface.h>
#include <aips/Exceptions/Error.h>
#include <aips/Exceptions/Excp.h>
#include <aips/Glish/GlishRecord.h>
#include <aips/Measures/MDirection.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/String.h>
#include <iostream.h>
void printAsRecord(const RecordTransformable & myClass);

int main() {
  try {
    Record rec;
    {
      MeasureHolder measure;
      {
 	const MDirection tmp;
 	measure = MeasureHolder(tmp);
      }
      printAsRecord(measure);
      String errorMessage;
      if (!measure.toRecord(errorMessage, rec)) {
 	throw(AipsError
 	      (String("Cannot convert class to a Record. The reason is:\n")
	       + errorMessage));
      }
    }
    AlwaysAssert(rec.isDefined("refer"), AipsError);
    AlwaysAssert(rec.isDefined("type"), AipsError);
    rec.define(RecordFieldId("refer"), String("b1950"));
    {
      MeasureHolder m;
      String errorMessage;
      if (!m.fromRecord(errorMessage, rec)) {
 	throw(AipsError
 	      (String("Cannot update class from a Record. The reason is:\n"
 		      + errorMessage)));
      }
      AlwaysAssert(m.isMDirection(), AipsError);
      MDirection md = m.asMDirection();
      AlwaysAssert(md.getRef().getType() == MDirection::B1950, AipsError);
    }
    {
      MeasureHolder m;
      String errorMessage;
      GlishRecord gRec;
      gRec.fromRecord(rec);
      if (!m.fromGlishRecord(errorMessage, gRec)) {
 	throw(AipsError
 	      (String("Cannot update class from a GlishRecord. The reason is:\n"
 		      + errorMessage)));
      }
      AlwaysAssert(m.isMDirection(), AipsError);
      MDirection md = m.asMDirection();
      AlwaysAssert(md.getRef().getType() == MDirection::B1950, AipsError);
    }
  }
  catch (AipsError x) {
    cerr << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  } end_try;
  cout << "OK" << endl;
  return 0;
}

void printAsRecord(const RecordTransformable & myClass) {
  String errorMessage;
  GlishRecord rec;
  if (!myClass.toGlishRecord(errorMessage, rec)) {
    cout << "Cannot convert class to a Record. The reason is:" << endl; 
    cout << errorMessage << endl;
  } else {
    cout << rec.format() << endl;
  }
}

// Local Variables: 
// compile-command: "gmake OPTLIB=1 tRecordTransformable"
// End: 
