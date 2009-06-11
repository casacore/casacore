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
#include <tables/Tables/Table.h>


namespace casa { //# NAMESPACE CASA - BEGIN

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
  // By default, the TiledDataStMan will be replaced by the TiledShapeStMan.
  // By default, the new table has the same nr of rows as the input table.
  // If <src>noRows=True</src> is given, it does not contain any row.
  static Table makeEmptyTable (const String& newName,
			       const Record& dataManagerInfo,
			       const Table& tab,
			       Table::TableOption option,
			       Table::EndianFormat endianFormat,
			       Bool replaceTSM = True,
			       Bool noRows = False);

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
  // <br> All columns in Table <src>out</src> will be filled from the
  // column with the same name in table <src>in</src>. In principle only
  // stored columns will be filled; however if the output table has only
  // one column, it can also be a virtual one.
  // <group>
  static void copyRows (Table& out, const Table& in)
    { copyRows (out, in, 0, 0, in.nrow()); }
  static void copyRows (Table& out, const Table& in,
			uInt startout, uInt startin, uInt nrrow);
  // </group>

  // Copy the table info block from input to output table.
  static void copyInfo (Table& out, const Table& in);

  // Copy all subtables (in table and column keywords) from input to
  // output table.
  // Optionally the row contents are not copied.
  static void copySubTables (Table& out, const Table& in, Bool noRows=False);

  // Copy the subtables in the given keywordset to the output keywordset
  // in the table with the given name.
  static void copySubTables (TableRecord& outKeys,
			     const TableRecord& inKeys,
			     const String& outName,
			     Table::TableType outType,
			     const Table& in,
			     Bool noRows=False);

  // Replace TiledDataStMan by TiledShapeStMan in the DataManagerInfo record.
  // Since TiledShapeStMan does not support ID columns, they are
  // adjusted as well in tabDesc and dminfo.
  static void adjustTSM (TableDesc& tabDesc, Record& dminfo);

  // Replace non-writable storage managers by StandardStMan. This is needed
  // for special storage managers like LofarStMan.
  static Record adjustStMan (const Record& dminfo);

  // Adjust the data manager types and groups and the
  // hypercolumn definitions to the actual data manager info.
  static void adjustDesc (TableDesc& tabDesc, const Record& dminfo);
};



} //# NAMESPACE CASA - END

#endif
