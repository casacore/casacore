//# TableCopy.h: Class with static functions for copying a table
//# Copyright (C) 2001
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


//# Includes
#include <aips/Tables/TableCopy.h>
#include <aips/Tables/SetupNewTab.h>
#include <aips/Tables/TableRow.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/TableColumn.h>
#include <aips/Tables/TableError.h>
#include <aips/Containers/Record.h>
#include <aips/Arrays/Vector.h>
#include <aips/OS/Path.h>
#include <aips/Utilities/String.h>


Table TableCopy::makeEmptyTable (const String& newName, const Table& tab,
				 Table::TableOption option, Bool replaceTSM,
				 Bool noRows)
{
  SetupNewTable newtab (newName, tab.actualTableDesc(), Table::New);
  // Get the data manager info.
  // If needed, replace all TiledDataStMan by TiledShapeStMan.
  Record dminfo = tab.dataManagerInfo();
  if (replaceTSM) {
    for (uInt i=0; i<dminfo.nfields(); i++) {
      Record& rec = dminfo.rwSubRecord(i);
      if (rec.asString("TYPE") == "TiledDataStMan") {
	rec.define("TYPE", "TiledShapeStMan");
      }
    }
  }
  newtab.bindCreate (dminfo);
  return Table(newtab, (noRows ? 0 : tab.nrow()));
}

   
void TableCopy::copyRows (Table& out, const Table& in, uInt startout,
			  uInt startin, uInt nrrow)
{
  // Check if startin and nrrow are correct for input.
  if (startin + nrrow > in.nrow()) {
    throw TableError ("TableCopy: startin+nrrow exceed nr of input rows");
  }
  // Get all stored columns in the output table.
  TableRow outrow(out, True);
  Vector<String> columns = outrow.columnNames();
  const TableDesc& tdesc = in.tableDesc();
  // Only copy the columns that exist in the input table.
  Vector<String> cols(columns.nelements());
  uInt nrcol = 0;
  for (uInt i=0; i<columns.nelements(); i++) {
    if (tdesc.isColumn (columns(i))) {
      cols(nrcol++) = columns(i);
    }
  }
  cols.resize (nrcol, True);
  // Add rows as needed.
  if (startout + nrrow > out.nrow()) {
    out.addRow (startout + nrrow - out.nrow());
  }
  ROTableRow inrow(in, cols);
  for (uInt i=0; i<nrrow; i++) {
    inrow.get (startin + i);
    outrow.put (startout + i, inrow.record(), False);
  }
  out.flush();
}

void TableCopy::copyInfo (Table& out, const Table& in)
{
  out.tableInfo() = in.tableInfo();
  out.flushTableInfo();
}

void TableCopy::copySubTables (Table& out, const Table& in)
{
  copySubTables (out.rwKeywordSet(), in.keywordSet(), out.tableName());
  const TableDesc& outDesc = out.tableDesc();
  const TableDesc& inDesc = in.tableDesc();
  for (uInt i=0; i<outDesc.ncolumn(); i++) {
    const String& name = outDesc[i].name();
    if (inDesc.isColumn(name)) {
      TableColumn outCol(out, name);
      ROTableColumn inCol(in, name);
      copySubTables (outCol.rwKeywordSet(), inCol.keywordSet(),
		     out.tableName());
    }
  }
  out.flush(); 
}

void TableCopy::copySubTables (TableRecord& outKeys,
			       const TableRecord& inKeys,
			       const String& outName)
{
  for (uInt i=0; i<inKeys.nfields(); i++) {
    if (inKeys.type(i) == TpTable) {
      Table inTab = inKeys.asTable(i);
      String newName = outName + '/' + Path(inTab.tableName()).baseName();
      inTab.deepCopy (newName, Table::New);
      outKeys.defineTable (inKeys.name(i), Table(newName));
    }
  }
}
