//# tFailoverTable.cc: Test program for failover tables
//# Copyright (C) 2020
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/DataMan/StandardStMan.h>
#include <casacore/tables/DataMan/TiledColumnStMan.h>
#include <casacore/tables/DataMan/VirtualTaQLColumn.h>
#include <casacore/tables/DataMan/StandardStManAccessor.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/sstream.h>
#include <cstdlib>    // sleep

using namespace casacore;

// This program tests the table failover mode.
// It creates a description and a table.
// It reads back the table.

// Create the description of the table.
// Optionally a variable length string column is added (which is not failover proof).
TableDesc credes (Bool addString)
{
  TableDesc txp;
  txp.addColumn (ScalarColumnDesc<Int> ("col1"));
  txp.addColumn (ScalarColumnDesc<double> ("col2"));
  txp.addColumn (ScalarColumnDesc<float> ("col3"));
  txp.addColumn (ScalarColumnDesc<Complex> ("col4"));
  ScalarColumnDesc<String> col5("col5");
  col5.setMaxLength (10);
  txp.addColumn (col5);
  txp.addColumn (ArrayColumnDesc<Int> ("arr1", IPosition(2,3,4)));
  if (addString) {
    txp.addColumn (ScalarColumnDesc<String> ("col6"));
  }
  txp.addColumn (ScalarColumnDesc<String> ("col7"));
  return txp;
}

// Write data into the table.
Bool cretab (Bool addString, uInt nrrow, uInt ssmNrow,
             const String& nameExt, Bool wait)
{
  SetupNewTable newtab ("tFailoverTable_tmp.tab" + nameExt, credes(addString),
                        Table::New, StorageOption::Failover);
  StandardStMan ssm("ssm", -Int(ssmNrow));
  TiledColumnStMan tcs("tcs", IPosition(3,3,4,5));
  VirtualTaQLColumn vtc("'str1'");
  newtab.bindAll (ssm);
  newtab.bindColumn ("arr1", tcs);
  newtab.bindColumn ("col7", vtc);
  Table tab;
  try {
    tab = Table(newtab);
  } catch (const TableError& x) {
    cout << x.what() << endl;
    return False;
  }
  ROStandardStManAccessor acc(tab, "col1", True);
  acc.showBaseStatistics (cout);
  acc.showIndexStatistics (cout);
  AlwaysAssertExit (! tab.canRemoveRow());
  AlwaysAssertExit (! tab.canRemoveColumn(Vector<String>(1, "col1")));
  AlwaysAssertExit (! tab.canRenameColumn("newname"));
  ScalarColumn<Int>     col1 (tab, "col1");
  ScalarColumn<double>  col2 (tab, "col2");
  ScalarColumn<float>   col3 (tab, "col3");
  ScalarColumn<Complex> col4 (tab, "col4");
  ScalarColumn<String>  col5 (tab, "col5");
  ArrayColumn<Int>      cola (tab, "arr1");
  String s("0123456789");
  Array<Int> arr1(IPosition(2,3,4));
  indgen(arr1);
  for (uInt i=0; i<nrrow; i++) {
    tab.addRow (1);
    col1.put (i, (100000+i) % 10);
    col2.put (i, (i+1) % 105);
    col3.put (i, (i+2) % 75);
    col4.put (i, i+3);
    col5.put (i, s.substr(0,i%10));
    cola.put (i, arr1);
    arr1 += 1;
  }
  cout << "  Filling done" << endl;
  acc.showBaseStatistics (cout);
  acc.showIndexStatistics (cout);
  RODataManAccessor acca(tab, "arr1", True);
  acca.showCacheStatistics (cout);
  if (wait) {
    cerr << "Waiting 10 seconds to be killed to get improperly ended table ..." << endl;
    sleep(10);
  }
  // Row removal should fail.
  try {
    tab.removeRow (0);
  } catch (const TableError& x) {
    cout << x.what() << endl;
  }
  return True;
}

void readtab (uInt nrrow, const String& nameExt)
{
  Table tab("tFailoverTable_tmp.tab" + nameExt, Table::Old);
  cout << "Table opened with " << tab.nrow() << " rows out of " << nrrow << endl;
  ScalarColumn<Int>     col1 (tab, "col1");
  ScalarColumn<double>  col2 (tab, "col2");
  ScalarColumn<float>   col3 (tab, "col3");
  ScalarColumn<Complex> col4 (tab, "col4");
  ScalarColumn<String>  col5 (tab, "col5");
  ScalarColumn<String>  col7 (tab, "col7");
  ArrayColumn<Int>      cola (tab, "arr1");
  ROStandardStManAccessor acc(tab, "col1", True);
  acc.showBaseStatistics (cout);
  acc.showIndexStatistics (cout);
  RODataManAccessor acca(tab, "arr1", True);
  acca.showCacheStatistics (cout);
  String s("0123456789");
  Array<Int> arr1(IPosition(2,3,4));
  indgen(arr1);
  for (uInt i=0; i<tab.nrow(); i++) {
    AlwaysAssertExit (col1(i) == (100000+i) % 10);
    AlwaysAssertExit (col2(i) == (i+1) % 105);
    AlwaysAssertExit (col3(i) == (i+2) % 75);
    AlwaysAssertExit (col4(i) == Complex(i+3));
    AlwaysAssertExit (col5(i) == s.substr(0,i%10));
    AlwaysAssertExit (col7(i) == "str1");
    AlwaysAssertExit (allEQ (cola(i), arr1));
    arr1 += 1;
  }
}

int main (int argc, const char* argv[])
{
  // This program tests the table failover mode.
  // It writes a table in a normal way and read it back.
  // Thereafter it writes all rows, but waits until killed, so the table is
  // not closed properly.
  // A next run in readonly mode will read back that table to see if failover works.
  // 
  // It can be run as:
  //   tFailoverTable [arg]
  // If arg starts with 'r', the program will only read back tFailoverTable_tmp.tab3
  // which is the output from the killed run.
  // Otherwise if arg is given, it contains the nr of rows to write in an SSM bucket.
  // This can be used to create an improperly closed table where one of the
  // storage managers contains 0 rows.
  uInt nrow = 500;
  uInt ssmNrow = 32;
  Bool readOnly = False;
  if (argc > 1) {
    if (argv[1][0] == 'r') {
      readOnly = True;
    } else {
      ssmNrow = atoi(argv[1]);
    }
  }
  if (! readOnly) {
    // Create failover table with variable length strings that will fail.
    AlwaysAssertExit (! cretab(True, nrow, ssmNrow, "1", False));
    // Create failover table without such strings with proper end.
    AlwaysAssertExit (cretab(False, nrow, ssmNrow, "2", False));
    readtab (nrow, "2");
    // Create failover table without such strings that waits till killed, so
    // the table is not ended properly. The SSM bucket is small, so most of the
    // rows are written into it.
    AlwaysAssertExit (cretab(False, nrow, ssmNrow, "3", True));
  }
  readtab (nrow, "3");
  return 0;          // successfully executed
}
