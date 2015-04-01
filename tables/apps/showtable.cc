//# showtable.cc: This program shows table info and contents.
//# Copyright (C) 2011
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

#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/TaQL/TableParse.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/Tables/TableColumn.h>
#include <casacore/casa/OS/Directory.h>
#include <casacore/casa/Inputs/Input.h>
#include <stdexcept>
#include <iostream>
#include <cstdlib>          // for mkstemp
#include <casacore/casa/string.h>    // for strerror
#include <errno.h>

using namespace casacore;
using namespace std;


void showKeys (const Table& table, Bool showtabkey, Bool showcolkey,
               Int maxval)
{
  Bool shown = False;
  if (showtabkey) {
    if (table.keywordSet().size() > 0) {
      cout << "  Table Keywords" << endl;
      table.keywordSet().print (cout, maxval, "    ");
      cout << endl;
      shown = True;
    }
  }
  if (showcolkey) {
    Vector<String> colNames (table.tableDesc().columnNames());
    for (uInt i=0; i<colNames.size(); ++i) {
      TableRecord keys (TableColumn(table, colNames[i]).keywordSet());
      if (keys.size() > 0) {
        cout << "  Column " << colNames[i] << endl;
        keys.print (cout, maxval, "    ");
        cout << endl;
        shown = True;
      }
    }
  }
  if (!shown) {
    cout << endl;
  }
}


int main (int argc, char* argv[])
{
  try {
    // Read the input parameters.
    Input inputs(1);
    inputs.version("2013Oct16GvD");
    inputs.create("in", "", "Input table", "string");
    inputs.create("dm", "T", "Show data manager info?", "bool");
    inputs.create("col", "T", "Show column info?", "bool");
    inputs.create("tabkey", "F", "Show table keywords?", "bool");
    inputs.create("colkey", "F", "Show column keywords?", "bool");
    inputs.create("maxval", "25", "Max nr of array values to show", "int");
    inputs.create("sub", "F", "Show info for all subtables?", "bool");
    inputs.create("sort", "F", "Sort columns in alphabetical order?", "bool");
    inputs.create("browse", "F", "Browse contents of table?", "bool");
    inputs.create("selcol", "", "TaQL column selection string", "string");
    inputs.create("selrow", "", "TaQL row selection string", "string");
    inputs.create("selsort", "", "TaQL sort string", "string");
    inputs.readArguments(argc, argv);

    // Get and check the input specification.
    String in (inputs.getString("in"));
    if (in.empty()) {
      throw AipsError(" an input table name must be given");
    }
    Bool showdm     = inputs.getBool("dm");
    Bool showcol    = inputs.getBool("col");
    Bool showtabkey = inputs.getBool("tabkey");
    Bool showcolkey = inputs.getBool("colkey");
    Int  maxval     = inputs.getInt ("maxval");
    Bool showsub    = inputs.getBool("sub");
    Bool sortcol    = inputs.getBool("sort");
    Bool browse     = inputs.getBool("browse");
    String selcol  (inputs.getString("selcol"));
    String selrow  (inputs.getString("selrow"));
    String selsort (inputs.getString("selsort"));

    // Do the selection if needed.
    Table table(in);
    Table seltab(table);
    if (! (selcol.empty() && selrow.empty() && selsort.empty())) {
      String command ("select ");
      if (!selcol.empty()) {
        command += selcol;
      }
      command += " from " + in;
      if (!selrow.empty()) {
        command += " where " + selrow;
      }
      if (!selsort.empty()) {
        command += " orderby " + selsort;
      }
      clog << "TaQL command = " << command << endl;
      seltab = tableCommand (command);
    }
    // Show the table structure.
    table.showStructure (cout, showdm, showcol, showsub, sortcol);
    if (showtabkey || showcolkey) {
      // Show table and/or column keywords.
      cout << endl
           << "Keywords of main table " << endl
           << "----------------------" << endl;
      showKeys (seltab, showtabkey, showcolkey, maxval);
      if (showsub) {
        // Also show them in the subtables.
        TableRecord keyset (table.keywordSet());
        for (uInt i=0; i<keyset.nfields(); ++i) {
          if (keyset.dataType(i) == TpTable) {
            Table tab(keyset.asTable(i));
            // Do not show keywords if the subtable references the parent table.
            if (! tab.isSameRoot (table)) {
              cout << "Keywords of subtable " << keyset.name(i) << endl
                   << "--------------------" << endl;
              showKeys (keyset.asTable(i), showtabkey, showcolkey, maxval);
            }
          }
        }
      }
    }
    if (browse) {
      // Need to make table persistent for casabrowser.
      String tmpName;
      if (seltab.tableName() != table.tableName()) {
	// g++ gives a deprecated warning for tempnam.
        // Therefore we use mkstemp and close/unlink the file immediately.
        char tmpnm[] = "/tmp/shtabXXXXXX";
        int fd = mkstemp(tmpnm);
        tmpName = tmpnm;
        cout << "tmpnm="<<tmpName<<endl;
        // Close and delete the file.
        ::close(fd);
        ::unlink(tmpnm);
        seltab.rename (tmpName, Table::New);
      }
      clog << "Starting casabrowser " << seltab.tableName() << " ..." << endl;
      if (! system (("casabrowser " + seltab.tableName()).chars())) {
	clog << "Could not start casabrowser; " << strerror(errno) << endl;
      }
      if (!tmpName.empty()) {
        seltab = Table();  // close table
        clog << "Removing temporary table " << tmpName << endl;
        // Use Directory instead of Table::deleteTable because the
        // latter tests if the table is writable, which it is not if
        // the underlying table is ot writable.
        Directory dir(tmpName);
        dir.removeRecursive();
      }
    }
  } catch (std::exception& x) {
    cerr << x.what() << endl;
    return 1;
  }
  return 0;
}
