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

#include <tables/Tables/Table.h>
#include <tables/Tables/TableParse.h>
#include <casa/Inputs/Input.h>
#include <stdexcept>
#include <iostream>

using namespace casa;
using namespace std;

int main (int argc, char* argv[])
{
  // Read the input parameters.
  Input inputs(1);
  inputs.version("2012Nov14GvD");
  inputs.create("in", "", "Input table", "string");
  inputs.create("dm", "T", "Show data manager info?", "bool");
  inputs.create("col", "T", "Show column info?", "bool");
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
  Bool showdm  = inputs.getBool("dm");
  Bool showcol = inputs.getBool("col");
  Bool showsub = inputs.getBool("sub");
  Bool sortcol = inputs.getBool("sort");
  Bool browse  = inputs.getBool("browse");
  String selcol  (inputs.getString("selcol"));
  String selrow  (inputs.getString("selrow"));
  String selsort (inputs.getString("selsort"));

  try {
    String tmpName;
    Table table(in);
    Table seltab(table);
    if (! (selcol.empty() && selrow.empty() && selsort.empty())) {
      if (!browse) {
        clog << "selection ignored; it is only useful if browse=T" << endl;
      } else {
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
        if (seltab.tableName() != table.tableName()) {
          char* tmpnm = tempnam("/tmp", "showtable_");
          tmpName = tmpnm;
          free (tmpnm);
        }
      }
    }
    table.showStructure (cout, showdm, showcol, showsub, sortcol);
    if (browse) {
      // Need to make table persistent for casabrowser.
      if (!tmpName.empty()) {
        seltab.rename (tmpName, Table::New);
      }
      clog << "Starting casabrowser " << seltab.tableName() << " ..." << endl;
      system (("casabrowser " + in).chars());
      if (!tmpName.empty()) {
        clog << "Removing temporary table " << seltab.tableName() << endl;
        Table::deleteTable(tmpName);
      }
    }
  } catch (std::exception& x) {
    cerr << x.what() << endl;
    return 1;
  }
  return 0;
}
