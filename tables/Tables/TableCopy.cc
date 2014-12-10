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
#include <casacore/tables/Tables/TableCopy.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/TableRow.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/TableColumn.h>
#include <casacore/tables/Tables/TableLocker.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/tables/DataMan/DataManager.h>
#include <casacore/tables/DataMan/DataManInfo.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Containers/SimOrdMap.h>
#include <casacore/casa/Utilities/LinearSearch.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/OS/Path.h>
#include <casacore/casa/BasicSL/String.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

Table TableCopy::makeEmptyTable (const String& newName,
				 const Record& dataManagerInfo,
				 const Table& tab,
				 Table::TableOption option,
				 Table::EndianFormat endianFormat,
				 Bool replaceTSM,
				 Bool noRows)
{
  TableDesc tabDesc = tab.actualTableDesc();
  Record dminfo (dataManagerInfo);
  if (dminfo.nfields() == 0) {
    // No new dminfo given, so use existing.
    dminfo = tab.dataManagerInfo();
  } else {
    // Set data manager group in description to actual group.
    // Also remove possible obsolete hypercolumn definitions.
    DataManInfo::adjustDesc (tabDesc, dminfo);
  }
  if (replaceTSM) {
    // Replace possible usage of TiledDataStMan by TiledShapeStMan.
    DataManInfo::adjustTSM (tabDesc, dminfo);
  }
  // Replace non-writable storage managers by StandardStMan.
  // This is for instance needed for LofarStMan.
  dminfo = DataManInfo::adjustStMan (dminfo, "StandardStMan", True);
  SetupNewTable newtab (newName, tabDesc, option);
  newtab.bindCreate (dminfo);
  return Table(newtab, (noRows ? 0 : tab.nrow()), False, endianFormat);
}

Table TableCopy::makeEmptyMemoryTable (const String& newName,
				       const Table& tab,
				       Bool noRows)
{
  TableDesc tabDesc = tab.actualTableDesc();
  Record dminfo = tab.dataManagerInfo();
  SetupNewTable newtab (newName, tabDesc, Table::New);
  newtab.bindCreate (dminfo);
  return Table(newtab, Table::Memory, (noRows ? 0 : tab.nrow()));
}

void TableCopy::copyRows (Table& out, const Table& in, uInt startout,
			  uInt startin, uInt nrrow, Bool flush)
{
  // Check if startin and nrrow are correct for input.
  if (startin + nrrow > in.nrow()) {
    throw TableError ("TableCopy: startin+nrrow exceed nr of input rows");
  }
  // Get all columns in the output table.
  // If there are multiple columns, only take the stored ones.
  TableRow outrow(out, out.tableDesc().ncolumn() > 1);
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
  if (nrcol > 0) {
    cols.resize (nrcol, True);
    // Add rows as needed.
    if (startout + nrrow > out.nrow()) {
      out.addRow (startout + nrrow - out.nrow());
    }
    ROTableRow inrow(in, cols);
    outrow = TableRow(out, cols);
    for (uInt i=0; i<nrrow; i++) {
      inrow.get (startin + i);
      outrow.put (startout + i, inrow.record(), inrow.getDefined(), False);
    }
    if (flush) {
      out.flush();
    }
  }
}

void TableCopy::copyInfo (Table& out, const Table& in)
{
  out.tableInfo() = in.tableInfo();
  out.flushTableInfo();
}

void TableCopy::copySubTables (Table& out, const Table& in, Bool noRows,
                               const Block<String>& omit)
{
  copySubTables (out.rwKeywordSet(), in.keywordSet(), out.tableName(),
		 out.tableType(), in, noRows, omit);
  const TableDesc& outDesc = out.tableDesc();
  const TableDesc& inDesc = in.tableDesc();
  for (uInt i=0; i<outDesc.ncolumn(); i++) {
    // Only writable columns can have keywords defined, thus subtables.
    if (out.isColumnWritable(i)) {
      const String& name = outDesc[i].name();
      if (inDesc.isColumn(name)) {
	TableColumn outCol(out, name);
	TableColumn inCol(in, name);
	copySubTables (outCol.rwKeywordSet(), inCol.keywordSet(),
		       out.tableName(), out.tableType(), in, noRows, omit);
      }
    }
  }
  out.flush(); 
}

void TableCopy::copySubTables (TableRecord& outKeys,
			       const TableRecord& inKeys,
			       const String& outName,
			       Table::TableType outType,
			       const Table& in,
			       Bool noRows,
                               const Block<String>& omit)
{
  for (uInt i=0; i<inKeys.nfields(); i++) {
    if (inKeys.type(i) == TpTable) {
      Table inTab = inKeys.asTable(i);
      // Skip a subtable that has to be omitted.
      if (linearSearchBrackets1 (omit, inKeys.name(i)) >= 0) {
        continue;
      }
      // Lock the subtable in case not locked yet.
      // Note it will keep the lock if already locked.
      TableLocker locker(inTab, FileLocker::Read);
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
	  outTab = inTab.copyToMemoryTable (newName, noRows);
	} else {
	  inTab.deepCopy (newName, Table::New, False,
			  Table::AipsrcEndian, noRows);
	  outTab = Table(newName);
	}
	outKeys.defineTable (inKeys.name(i), outTab);
      }
    }
  }
}

} //# NAMESPACE CASACORE - END

