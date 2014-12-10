//# tUDFMSCal.cc: Test program for class UDFMSCal
//# Copyright (C) 2012
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

#include <casacore/derivedmscal/DerivedMC/Register.h>
#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/TaQL/TableParse.h>
#include <iostream>
#include <sstream>

using namespace casacore;
using namespace std;

void createTable()
{
  // Create main 'MS-like' table.
  const uInt nant=5;
  TableDesc td(MS::requiredTableDesc());
  SetupNewTable setup("tUDFMSCal_tmp.tab", td, Table::New);
  MeasurementSet ms(setup);
  ms.createDefaultSubtables(Table::New);
  ScalarColumn<Int> a1col(ms, "ANTENNA1");
  ScalarColumn<Int> a2col(ms, "ANTENNA2");
  Int row=0;
  for (uInt i=0; i<nant; ++i) {
    for (uInt j=i; j<nant; ++j) {
      ms.addRow();
      a1col.put (row, i);
      a2col.put (row, j);
      ++row;
    }
  }
  // Fill the ANTENNA subtable.
  Table anttab(ms.antenna());
  anttab.addRow (nant);
  ScalarColumn<String> namecol(anttab, "NAME");
  for (uInt i=0; i<nant; ++i) {
    ostringstream ostr;
    ostr << "RT" << i;
    namecol.put (i, ostr.str());
  }
}

void testUDF()
{
  Table tab = tableCommand("select from tUDFMSCal_tmp.tab "
                           "where mscal.baseline('RT[2-4]')").table();
  Vector<Int> a1 (ScalarColumn<Int>(tab, "ANTENNA1").getColumn());
  Vector<Int> a2 (ScalarColumn<Int>(tab, "ANTENNA2").getColumn());
  for (uInt i=0; i<tab.nrow(); ++i) {
    cout << a1[i] << ' '<< a2[i] << endl;
  }
}


int main()
{
  try {
    // Register the UDFs.
    register_derivedmscal();
    // Create the table.
    createTable();
    // Execute a TaQL command.
    testUDF();
  } catch (std::exception& x) {
    cout << "Unexpected exception: " << x.what() << endl;
    return 1;
  }
  return 0;
}
