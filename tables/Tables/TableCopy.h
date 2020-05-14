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

#ifndef TABLES_TABLECOPY_H
#define TABLES_TABLECOPY_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/DataManInfo.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Containers/Record.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Class with static functions for copying a table.
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> Table
// </prerequisite>

// <synopsis> 
// TableCopy is a class for making a deep copy of a table.
// The table can be a PlainTable or a RefTable.
// It contains the following static functions:
// <ol>
//  <li> <src>makeEmptyTable</src> creates a new table using the
//       description and storage managers of the input table.
//       By default TiledDataStMan (which is more or less obsolete) will
//       be replaced by TiledShapeStMan.
//       By default the new table contains the same number of rows as the
//       existing table.
//  <li> <src>copyRows</src> copies the data of one to another table.
//       It is possible to specify where to start in the input and output.
//  <li> <src>CopyInfo</src> copies the table info data.
//  <li> <src>copySubTables</src> copies all the subtables in table and
//       column keywords. It is done recursively.
// </ol>
// </synopsis> 

//# <todo asof="$DATE:$">
//# </todo>


class TableCopy
{
public:
  // Make an (empty) table with the given description.
  // If the description contains no columns, the description of the input
  // table is used, so it has the same keywords and columns as the input one.
  // The data managers can be given in the dataManagerInfo record.
  // If it is empty, the info is taken from the input table.
  // <br>Non-writable storage managers (like LofarStMan) are by default replaced
  // by StandardStMan. If <src>replaceMSM</src> is set, MemoryStMan is also
  // replaced by StandardStMan.
  // <br>By default, the TiledDataStMan will be replaced by the TiledShapeStMan.
  // <br>By default, the new table has the same nr of rows as the input table.
  // If <src>noRows=True</src> is given, it does not contain any row.
  static Table makeEmptyTable (const String& newName,
			       const Record& dataManagerInfo,
			       const Table& tab,
			       Table::TableOption option,
			       Table::EndianFormat endianFormat,
			       Bool replaceTSM = True,
			       Bool noRows = False,
                               const StorageOption& = StorageOption());

  // Make an (empty) memory table with the same layout as the input one.
  // It has the same keywords and columns as the input one.
  // By default, the new table has the same nr of rows as the input table.
  // If <src>noRows=True</src> is given, it does not contain any row.
  static Table makeEmptyMemoryTable (const String& newName,
				     const Table& tab,
				     Bool noRows = False);

  // Copy rows from the input to the output.
  // By default all rows will be copied starting at row 0 of the output.
  // Rows will be added to the output table as needed.
  // The output table will by default be flushed after the rows are copied.
  // <br> All columns in Table <src>out</src> will be filled from the
  // column with the same name in table <src>in</src>. In principle only
  // stored columns will be filled; however if the output table has only
  // one column, it can also be a virtual one.
  // <group>
  static void copyRows (Table& out, const Table& in, Bool flush=True)
    { copyRows (out, in, 0, 0, in.nrow(), flush); }
  static void copyRows (Table& out, const Table& in,
			rownr_t startout, rownr_t startin, rownr_t nrrow,
                        Bool flush=True);
  // </group>

  // Copy the table info block from input to output table.
  static void copyInfo (Table& out, const Table& in);

  // Copy all subtables (in table and column keywords) from input to
  // output table.
  // Subtables of which the keyword name matches an omit value are skipped.
  // Optionally the row contents are not copied.
  static void copySubTables (Table& out, const Table& in, Bool noRows=False,
			     const Block<String>& omit=Block<String>());

  // Copy the subtables in the given keywordset to the output keywordset
  // in the table with the given name.
  // Subtables of which the keyword name matches an omit value are skipped.
  // Optionally the row contents are not copied.
  static void copySubTables (TableRecord& outKeys,
			     const TableRecord& inKeys,
			     const String& outName,
			     Table::TableType outType,
			     const Table& in,
			     Bool noRows=False,
			     const Block<String>& omit=Block<String>());

  // Clone a column in the from table to a new column in the to table.
  // The new column gets the same table description and data manager as the
  // from column. It has to get a unique data manager name. If not given,
  // it is the new column name.
  static void cloneColumn (const Table& fromTable,
                           const String& fromColumn,
                           Table& toTable,
                           const String& newColumn,
                           const String& dataManagerName = String());
  // Cloning as above, but the data type is set to the template parameter.
  template<typename T> 
  static void cloneColumnTyped (const Table& fromTable,
                                const String& fromColumn,
                                Table& toTable,
                                const String& newColumn,
                                const String& dataManagerName = String());

  // Copy the data from one column to another.
  // It can be used after function cloneColumn to populate the new column.
  // Note that the data types of the column do not need to match; data type
  // promotion is done if needed.
  // <br>The <src>preserveTileShape</src> argument tells if the original
  // tile shape is kept if a tiled data manager is used. If False, the
  // default tile shape of the data manager is used.
  // <note role=tip>
  // Note that a TaQL command can be used to fill a column in any way.
  // For example, fill toColumn with the real part of a complex fromColumn:
  // <srcblock>
  //   Block<Table> tables(2);
  //   tables[0] = toTable;
  //   tables[1] = fromTable;
  //   tableCommand ("update $1 set toColumn=real(t2.fromColumn) from $2 t2",
  //                 tables);
  // </srcblock>
  // When copying a column in a straightforward way, the TaQL way is about 25%
  // slower than using the function <src>copyColumnData</src>.
  // </note>
  static void copyColumnData (const Table& fromTable,
                              const String& fromColumn,
                              Table& toTable,
                              const String& toColumn,
                              Bool preserveTileShape=True);

  // Fill the table column with the given array.
  // The template type must match the column data type.
  template<typename T>
  static void fillArrayColumn (Table& table, const String& column,
                               const Array<T>& value);

  // Fill the table column with the given value.
  // If the column contains arrays, the arrays are filled with the value.
  // The template type must match the column data type.
  template<typename T>
  static void fillColumnData (Table& table, const String& column,
                              const T& value);
  // Specialization to handle a C-string correctly.
  static void fillColumnData (Table& table, const String& column,
                              const char* value)
    { fillColumnData (table, column, String(value)); }

  // Fill the table column with the given value.
  // The column must contain arrays. The arrays get the shape of the
  // corresponding row in the fromColumn in the fromTable.
  // It can be used after function cloneColumn to initialize the new column.
  // The template type must match the column data type.
  template<typename T>
  static void fillColumnData (Table& table, const String& column,
                              const T& value,
                              const Table& fromTable, const String& fromColumn,
                              Bool preserveTileShape=True);
  // Specialization to handle a C-string correctly.
  static void fillColumnData (Table& table, const String& column,
                              const char* value,
                              const Table& fromTable, const String& fromColumn,
                              Bool preserveTileShape=True)
    { fillColumnData (table, column, String(value), fromTable, fromColumn,
                      preserveTileShape); }
                              

  // Replace TiledDataStMan by TiledShapeStMan in the DataManagerInfo record.
  // Since TiledShapeStMan does not support ID columns, they are
  // adjusted as well in tabDesc and dminfo.
  static void adjustTSM (TableDesc& tabDesc, Record& dminfo)
    { DataManInfo::adjustTSM (tabDesc, dminfo); }

  // Replace non-writable storage managers by StandardStMan. This is needed
  // for special storage managers like LofarStMan.
  static Record adjustStMan (const Record& dminfo)
    { return DataManInfo::adjustStMan (dminfo, "StandardStMan"); }

  // Set the data managers of the given column(s) to the given tiled storage
  // manager (normally TiledShapeStMan or TiledColumnStMan).
  // The columns are combined in a single storage manager, so the function
  // has to be called multiple times if, say, one per column is needed.
  // The columns already having a tiled storage manager are not changed.
  static void setTiledStMan (Record& dminfo, const Vector<String>& columns,
                             const String& dmType, const String& dmName,
                             const IPosition& defaultTileShape)
    { DataManInfo::setTiledStMan (dminfo, columns, dmType, dmName,
                                  defaultTileShape); }

  // Remove the columns from the dminfo record and return a vector with the
  // names of the columns actually removed.
  // The columns having a data manager matching <src>keepType</src> are not
  // removed. Matching means that the beginning of the data manager name
  // have to match, so "Tiled" matches all tiled storagemanagers.
  static Vector<String> removeDminfoColumns (Record& dminfo,
                                             const Vector<String>& columns,
                                             const String& keepType= String())
    { return DataManInfo::removeDminfoColumns (dminfo, columns, keepType); }

  // Adjust the data manager types and groups and the
  // hypercolumn definitions to the actual data manager info.
  static void adjustDesc (TableDesc& tabDesc, const Record& dminfo)
    { DataManInfo::adjustDesc (tabDesc, dminfo); }

private:
  static void doCloneColumn (const Table& fromTable, const String& fromColumn,
                             Table& toTable, const ColumnDesc& newColumn,
                             const String& dataManagerName);
};



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/tables/Tables/TableCopy.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
