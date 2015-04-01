//# MeasTable.cc: MeasTable provides Measure computing database data
//# Copyright (C) 1995-1999,2000-2004
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

#include <casacore/measures/Measures/MeasIERS.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Containers/RecordField.h>

using namespace casacore;

int main (int argc, char* argv[])
{
  String tableName("Observatories");
  if (argc > 1) {
    tableName = argv[1];
  }
  try {
    if (tableName=="help" || tableName=="-h" || tableName=="--help") {
      cerr << endl;
      cerr << "This program shows the location where a Measures table is found."
           << endl;
      cerr << "Run as:   findmeastable tabletype" << endl;
      cerr << "  default tabletype is Observatories" << endl;
      cerr << endl;
      cerr << "  E.g.    findmeastable IGRF" << endl;
      cerr << endl;
      return 0;
    }
    String tableNameLC = tableName;
    tableNameLC.downcase();
    Table table;
    TableRecord kws;
    ROTableRow row;
    RORecordFieldPtr<Double> rfp[1];
    String vs;
    Double dt;
    Int N = 0;
    String rfn[1];
    if (!MeasIERS::getTable(table, kws, row, rfp, vs, dt, N, rfn,
                            tableName,
			    "measures."+tableNameLC+".directory", "")) {
      cerr << tableName << " measures table could not be found" << endl;
      return 1;
    }
    cout << "Measures table " << tableName<< " found as "
         << table.tableName() << endl;
  } catch (std::exception& x) {
    cerr << "Error: " << x.what() << endl;\
    return 1;
  }
  return 0;
}
