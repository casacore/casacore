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

#include <tables/Tables.h>
#include <stdexcept>
#include <iostream>
using namespace casa;
using namespace std;

// Remove the dirname from the table name in an error message.
String removeDir (const String& msg)
{
  String s = msg;
  s.gsub (Regex("/.*/t"), "t");
  return s;
}

int main (int argc, const char* argv[])
{
  Table::TableType ttyp = Table::Plain;
  if (argc > 1  &&  String(argv[1]) == String("m")) {
    ttyp = Table::Memory;
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
  } catch (exception& x) {
    cout << x.what() << endl;
    return 1;
  }
  return 0;
}
