//# TableCopy.h: Class with static functions for copying a table
//# Copyright (C) 2001,2002,2003
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
#include <tables/Tables/TableCopy.h>
#include <tables/Tables/SetupNewTab.h>
#include <tables/Tables/TableRow.h>
#include <tables/Tables/TableDesc.h>
#include <tables/Tables/TableColumn.h>
#include <tables/Tables/TableError.h>
#include <casa/Containers/Record.h>
#include <casa/Arrays/Vector.h>
#include <casa/OS/Path.h>
#include <casa/BasicSL/String.h>


namespace casa { //# NAMESPACE CASA - BEGIN

Table TableCopy::makeEmptyTable (const String& newName,
				 const Record& dataManagerInfo,
				 const Table& tab,
				 Table::TableOption option, Bool replaceTSM,
				 Bool noRows)
{
  TableDesc tabDesc = tab.actualTableDesc();
  Record dminfo (dataManagerInfo);
  if (dminfo.nfields() == 0) {
    dminfo = tab.dataManagerInfo();
  }
  if (replaceTSM) {
    adjustTSM (tabDesc, dminfo);
  }
  SetupNewTable newtab (newName, tabDesc, Table::New);
  newtab.bindCreate (dminfo);
  return Table(newtab, (noRows ? 0 : tab.nrow()));
}

Table TableCopy::makeEmptyMemoryTable (const String& newName,
				       const Table& tab,
				       Bool noRows)
{
  TableDesc tabDesc = tab.actualTableDesc();
  SetupNewTable newtab (newName, tabDesc, Table::New);
  return Table(newtab, Table::Memory, (noRows ? 0 : tab.nrow()));
}

void TableCopy::adjustTSM (TableDesc& tabDesc, Record& dminfo)
{
  Vector<String> dataNames, coordNames, idNames;
  // Keep track of hypercolumns to be changed.
  Vector<String> hcChange;
  uInt nrhc = 0;
  // Loop through all hypercolumn descriptions.
  Vector<String> hcNames = tabDesc.hypercolumnNames();
  for (uInt i=0; i<hcNames.nelements(); i++) {
    // Find the hypercolumn in the dminfo.
    // If found, adjust if needed.
    for (uInt j=0; j<dminfo.nfields(); j++) {
      const Record& rec = dminfo.subRecord(j);
      if (rec.asString("NAME") == hcNames(i)) {
	if (rec.asString("TYPE") == "TiledDataStMan") {
	  // Replace TiledDataStMan by TiledShapeStMan.
	  Record& rwrec = dminfo.rwSubRecord(j);
	  rwrec.define("TYPE", "TiledShapeStMan");
	  // Get hypercolumn description.
	  tabDesc.hypercolumnDesc (hcNames(i), dataNames,
				   coordNames, idNames);
	  uInt nrid = idNames.nelements();
	  if (nrid > 0) {
	    // The hypercolumn definition contains ID columns, so it
	    // has to be changed later in the TableDesc.
	    hcChange.resize (nrhc+1, True);
	    hcChange(nrhc++) = hcNames(i);
	    // Keep the dminfo columns which are not an ID column.
	    Vector<String> colNames = rec.asArrayString("COLUMNS");
	    Vector<String> colsout(colNames.nelements());
	    uInt nrout = 0;
	    for (uInt k=0; k<colNames.nelements(); k++) {
	      Bool found = False;
	      for (uInt k1=0; k1<idNames.nelements(); k1++) {
		if (colNames(k) == idNames(k1)) {
		  found = True;
		  break;
		}
	      }
	      if (!found) {
		colsout(nrout++) = colNames(k);
	      }
	    }
	    colsout.resize (nrout, True);
	    rwrec.define ("COLUMNS", colsout);
	  }
	}	  
	break;
      }
    }
  }
  if (nrhc > 0) {
    tabDesc.removeIDhypercolumns (hcChange);
  }
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
  copySubTables (out.rwKeywordSet(), in.keywordSet(), out.tableName(),
		 out.tableType(), in);
  const TableDesc& outDesc = out.tableDesc();
  const TableDesc& inDesc = in.tableDesc();
  for (uInt i=0; i<outDesc.ncolumn(); i++) {
    const String& name = outDesc[i].name();
    if (inDesc.isColumn(name)) {
      TableColumn outCol(out, name);
      ROTableColumn inCol(in, name);
      copySubTables (outCol.rwKeywordSet(), inCol.keywordSet(),
		     out.tableName(), out.tableType(), in);
    }
  }
  out.flush(); 
}

void TableCopy::copySubTables (TableRecord& outKeys,
			       const TableRecord& inKeys,
			       const String& outName,
			       Table::TableType outType,
			       const Table& in)
{
  for (uInt i=0; i<inKeys.nfields(); i++) {
    if (inKeys.type(i) == TpTable) {
      Table inTab = inKeys.asTable(i);
      // If the table to be copied has the same root as the main input table,
      // we do not make a copy. This is needed to avoid the recursive copy
      // of SORTED_TABLE in a MeasurementSet.
      if (inTab.isSameRoot (in)) {
	String keyName = inKeys.name(i);
	if (outKeys.isDefined (keyName)) {
	  outKeys.removeField (keyName);
	}
      } else {
	String newName = outName + '/' + Path(inTab.tableName()).baseName();
	Table outTab;
	if (outType == Table::Memory) {
	  outTab = inTab.copyToMemoryTable (newName);
	} else {
	  inTab.deepCopy (newName, Table::New);
	  outTab = Table(newName);
	}
	outKeys.defineTable (inKeys.name(i), outTab);
      }
    }
  }
}

} //# NAMESPACE CASA - END

