//# tMSDataDescColumns.cc:
//# Copyright (C) 2000
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
#include <aips/Exceptions/Error.h>
#include <aips/MeasurementSets/NewMSDataDescColumns.h>
#include <aips/MeasurementSets/NewMSDataDescription.h>
#include <aips/Tables/SetupNewTab.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/String.h>
#include <iostream.h>

void putData(NewMSDataDescColumns& cols) {
  // test the spectralWindowId functions.
  cols.spectralWindowId().put(0, 0);
  cols.spectralWindowId().put(4, 1);
  // test the polarizationId functions.
  cols.polarizationId().put(0, 1);
  cols.polarizationId().put(4, 3);
  // test the flagRow functions.
  cols.flagRow().put(0, False);
  cols.flagRow().put(4, True);
}

void getData(const RONewMSDataDescColumns& cols) {
  // test the spectralWindowId functions.
  AlwaysAssert(cols.spectralWindowId()(0) == 0, AipsError);
  AlwaysAssert(cols.spectralWindowId()(4) == 1, AipsError);
  // test the polarizationId functions.
  AlwaysAssert(cols.polarizationId()(0) == 1, AipsError);
  AlwaysAssert(cols.polarizationId()(4) == 3, AipsError);
  // test the flagRow functions.
  AlwaysAssert(cols.flagRow()(0) == False, AipsError);
  AlwaysAssert(cols.flagRow()(4) == True, AipsError);
  // Check the optional columns do not exist
  AlwaysAssert(cols.lagId().isNull() == True, AipsError);
}

int main() {
  try {
    const String filename = "tMSDataDescColumns_tmp.table";
    { // Check the RW class
      SetupNewTable setup(filename, NewMSDataDescription::requiredTableDesc(), 
			  Table::New);
      NewMSDataDescription table(setup, 5);
      // Check the constructor
      NewMSDataDescColumns cols(table);
      // test the nrow function.
      AlwaysAssert(cols.nrow() == 5, AipsError);
      // Put data into the table
      putData(cols);
      // Check the data is still there
      getData(cols);
    } // Close the table

    {// Check the RO class
      const NewMSDataDescription table(filename, Table::Old);
      // Check the constructor
      const RONewMSDataDescColumns cols(table);
      // Check the data is still there
      getData(cols);
    }
    {// Delete the table
      NewMSDataDescription table(filename, Table::Old);
      table.markForDelete();
    }
  }
  catch (AipsError x) {
    cerr << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  }
  cout << "OK" << endl;
}

// Local Variables: 
// compile-command: "gmake OPTLIB=1 XLIBLIST=0 tMSDataDescColumns"
// End: 
