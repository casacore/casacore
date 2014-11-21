//# tRecordTransformable.cc: Test program for class RecordTransformable
//# Copyright (C) 1998,1999,2000,2001,2003
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

#include <casacore/casa/Utilities/RecordTransformable.h>
#include <casacore/measures/Measures/MeasureHolder.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Containers/RecordInterface.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/measures/Measures/MDirection.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
int main() {
  try {
    Record rec;
    {
      MeasureHolder measure;
      {
 	const MDirection tmp;
 	measure = MeasureHolder(tmp);
      }
      String errorMessage;
      if (!measure.toRecord(errorMessage, rec)) {
 	throw(AipsError
 	      (String("Cannot convert class to a Record. The reason is:\n")
	       + errorMessage));
      }
      AlwaysAssert(measure.ident() == "meas", AipsError);
      AlwaysAssert(measure.RecordTransformable::ident() == "", AipsError);
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
  }
  catch (AipsError x) {
    cerr << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  } 
  cout << "OK" << endl;
  return 0;
}
