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

#if !defined(AIPS_TABLECOPY_H)
#define AIPS_TABLECOPY_H


//# Includes
#include <aips/Tables/Table.h>


// <summary>
// Class with static functions for copying a table.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="">
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
  // Make an (empty) table with the same layout as the input one.
  // It has the same keywords, columns, and data managers as the
  // input one.
  // By default, the TiledDataStMan will be replaced by the TiledShapeStMan.
  // By default, the new table has the same nr of rows as the input table.
  // If <src>noRows=True</src> is given, it does not contain any row.
  static Table makeEmptyTable (const String& newName,
			       const Record& dataManagerInfo,
			       const Table& tab,
			       Table::TableOption option,
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
  static void copySubTables (Table& out, const Table& in);

  // Copy the subtables in the given keywordset to the output keywordset
  // in the table with the given name.
  static void copySubTables (TableRecord& outKeys,
			     const TableRecord& inKeys,
			     const String& outName,
			     Table::TableType outType,
			     const Table& in);

  // Replace TiledDataStMan by TiledShapeStMan in the DataManagerInfo record.
  // Since TiledShapeStMan does not support ID columns, they are
  // adjusted as well in tabDesc and dminfo.
  static void adjustTSM (TableDesc& tabDesc, Record& dminfo);
};


#endif
