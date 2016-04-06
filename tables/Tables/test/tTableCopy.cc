//# tTableCopy.cc: Test program for copying tables
//# Copyright (C) 2006
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

#include <casacore/tables/Tables.h>
#include <stdexcept>
#include <iostream>
using namespace casacore;
using namespace std;

// Remove the dirname from the table name in an error message.
String removeDir (const String& msg)
{
  String s = msg;
  s.gsub (Regex("/.*/t"), "t");
  return s;
}

// Test modifying the dminfo record.
void testDM()
{
    TableDesc td;
    td.addColumn(ScalarColumnDesc<Int>("col1"));
    td.addColumn(ScalarColumnDesc<Int>("col2"));
    td.addColumn(ScalarColumnDesc<Int>("col3"));
    td.addColumn(ArrayColumnDesc<Int>("col4"));
    // Now create a new table from the description.
    SetupNewTable aNewTab("tTableCopy_tmp.dm", td, Table::New);
    Table tabl(aNewTab);
    Record dminfo = tabl.dataManagerInfo();
    cout << dminfo;
    Vector<String> remCols1 =
      TableCopy::removeDminfoColumns (dminfo, Vector<String>(1, "col1"), "Standard");
    cout << dminfo << remCols1 << endl;
    Vector<String> remCols2 =
      TableCopy::removeDminfoColumns (dminfo, Vector<String>(1, "col1"));
    cout << dminfo << remCols2 << endl;
    TableCopy::setTiledStMan (dminfo, Vector<String>(1, "col3"),
                              "TiledShapeStMan", "TSMData", IPosition(3,3,4,5));
    cout << dminfo << endl;
}

void testCloneColumn()
{
  cout << "testCloneColumn ..." << endl;
  // First create a table.
  TableDesc td;
  td.addColumn (ArrayColumnDesc<Complex>("DATA", 1));
  td.addColumn (ScalarColumnDesc<String>("SCALAR", 1));
  td.addColumn (ArrayColumnDesc<Int>("ARRAY", 0));
  SetupNewTable newtab("tTableCopy_tmp.data", td, Table::New);
  StandardStMan ssm;
  TiledShapeStMan tsm("DATA_stm", IPosition(2,8,2));
  newtab.bindAll (ssm);
  newtab.bindColumn ("DATA", tsm);
  Table tab(newtab, 4);
  ArrayColumn<Complex> col(tab, "DATA");
  for (uInt row=0; row<tab.nrow(); ++row) {
    Vector<Complex> vec(10*row+1);
    indgen (vec);
    if (row != 2) {
      // Keep a row cell without an array.
      col.put (row, vec);
    }
  }
  // Now clone the column and copy the data.
  TableCopy::cloneColumn (tab, "DATA", tab, "DATA1");
  TableCopy::cloneColumn (tab, "DATA", tab, "DATA2", "Data2StMan");
  TableCopy::cloneColumnTyped<DComplex> (tab, "DATA", tab, "DATA3");
  TableCopy::cloneColumnTyped<Int> (tab, "SCALAR", tab, "SCALAR3");
  cout << tab.dataManagerInfo() << endl;
  TableCopy::copyColumnData (tab, "DATA", tab, "DATA1");
  TableCopy::copyColumnData (tab, "DATA", tab, "DATA3");
  // Check if the data are the same.
  ArrayColumn<Complex> col1(tab, "DATA1");
  ArrayColumn<DComplex> col3(tab, "DATA3");
  for (uInt row=0; row<tab.nrow(); ++row) {
    if (col.isDefined(row)) {
      Vector<Complex> vec(col(row));
      Vector<DComplex> vecd(vec.shape());
      convertArray (vecd, vec);
      AlwaysAssertExit (allEQ (col1(row), vec));
      AlwaysAssertExit (allEQ (col3(row), vecd));
    } else {
      AlwaysAssertExit (! col1.isDefined(row));
    }
  }
  // Initialize the array.
  TableCopy::fillColumnData (tab, "DATA2", Complex(-1,-2), tab, "DATA");
  // Initialize the scalar and other array.
  TableCopy::fillColumnData (tab, "SCALAR", "str");
  TableCopy::fillColumnData (tab, "SCALAR3", 2);
  TableCopy::fillArrayColumn (tab, "ARRAY", Vector<Int>(3,2));
  // Check if the data are correct.
  ArrayColumn<Complex> col2(tab, "DATA2");
  ScalarColumn<String> cols(tab, "SCALAR");
  ScalarColumn<Int> cols3(tab, "SCALAR3");
  ArrayColumn<Int> cola(tab, "ARRAY");
  for (uInt row=0; row<tab.nrow(); ++row) {
    if (col.isDefined(row)) {
      AlwaysAssertExit (col.shape(row).isEqual (col2.shape(row)));
      AlwaysAssertExit (allEQ (col2(row), Complex(-1,-2)));
    } else {
      AlwaysAssertExit (! col2.isDefined(row));
    }
    AlwaysAssertExit (cols(row) == "str");
    AlwaysAssertExit (cols3(row) == 2);
    AlwaysAssertExit (allEQ (cola(row), Vector<Int>(3,2)));
  }
}


int main (int argc, const char* argv[])
{
  Table::TableType ttyp = Table::Plain;
  if (argc > 1) {
    if (String(argv[1]) == String("m")) {
      ttyp = Table::Memory;
    }
  }
  Bool noRows = False;
  if (argc > 2  &&  String(argv[2]) == String("n")) {
    noRows = True;
  }
  try {
    TableDesc td("", "1", TableDesc::Scratch);
    td.comment() = "A SDMemTable";
    td.addColumn(ScalarColumnDesc<Int>("Test"));
    // Now create a new table from the description.
    SetupNewTable aNewTab("tTableCopy_tmp.tbl", td, Table::New);
    Table tabl(aNewTab, ttyp, 0);
    tabl.addRow();
    
    TableDesc std("", "1", TableDesc::Scratch);
    std.comment() = "A SubTable";
    SetupNewTable newTab(tabl.tableName()+"/SUBTABLE", std, Table::New);
    Table stabl(newTab, ttyp, 0);
    stabl.addRow();
    tabl.rwKeywordSet().defineTable("SUBTABLE", stabl);
    
    tabl.copy("tTableCopy_tmp.newtbl", Table::New, noRows);
    Table t("tTableCopy_tmp.newtbl");

    cout << removeDir(tabl.tableName()) << endl;
    cout << removeDir(tabl.keywordSet().asTable("SUBTABLE").tableName())
	 << endl;
    cout << removeDir(t.tableName()) << endl;
    cout << removeDir(t.keywordSet().asTable("SUBTABLE").tableName()) << endl;
    cout << tabl.nrow() << ' ' << stabl.nrow() << ' '
	 << t.nrow() << ' ' << t.keywordSet().asTable("SUBTABLE").nrow() << ' '
	 << tabl.tableType() << ' ' << t.tableType()
	 << endl;

    if (argc <= 1) {
      testDM();
      testCloneColumn();
    }
  } catch (const exception& x) {
    cout << x.what() << endl;
    return 1;
  }
  return 0;
}
