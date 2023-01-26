//# DataManInfo.h: Class with static functions to manipulate a datamanager info record
//# Copyright (C) 2001,2002,2003,2009
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

#ifndef TABLES_DATAMANINFO_H
#define TABLES_DATAMANINFO_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations.
class TableDesc;
class Table;
class Record;


// <summary>
// Class with static functions to manipulate a datamanager record.
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tTableCopy.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> Table
// </prerequisite>

// <synopsis> 
// DataManInfo is a class to manipulate a datamanager info record and/or table
// description. A datamanager info record tells how the columns are stored.
// It is a Record containing the following fields. If omitted, a default is used.
// <ul>
//  <li> TYPE: data manager type (default StandardStMan)
//  <li> NAME: unique data manager name
//  <li> COLUMNS: string vector containing columns stored with this data manager
//  <li> SPEC: subrecord containing data manager specific parameters
// </ul>
// Often an existing table description and datamanager info record are used to
// construct a new table, but it might be necessary to change it somewhat.
// <ul>
//  <li> Remove hypercolumn definitions from a table description. They are
//       not needed anymore and can be a burden.
//  <li> Replace non-writable storage managers (like LofarStMan) by a
//       writable one.
//  <li> Replace the deprecated TiledDataStMan by TiledShapeStMan.
//  <li> Merge two datamanager info records.
// </ul>
// </synopsis> 

//# <todo asof="$DATE:$">
//# </todo>


class DataManInfo
{
public:
  // Remove hypercolumn definitions from the table description.
  static void removeHypercolumns (TableDesc& tabDesc);

  // Replace TiledDataStMan by TiledShapeStMan in the DataManagerInfo record.
  // Since TiledShapeStMan does not support ID columns, they are
  // adjusted as well in tabDesc and dminfo.
  static void adjustTSM (TableDesc& tabDesc, Record& dminfo);

  // Replace non-writable storage managers by the given storage manager
  // (usually StandardStMan or IncrementalStMan).
  // It is possible to specify the new data manager type to use.
  // This is needed for special storage managers like LofarStMan.
  // If replaceMSM is set, MemoryStMan is also replaced.
  static Record adjustStMan (const Record& dminfo, const String& dmType,
                             bool replaceMSM = true);

  // Ensure all data manager names in <src>dminfo</src> are unique by
  // adding a unique suffix as needed (using function <src>uniqueName</src>).
  // Empty names are set to the name of the first column (DM if no columns).
  static void makeUniqueNames (Record& dminfo);

  // Return a unique data manager name by testing if the name already
  // exist in of the the dm-s in the <src>dminfo</src> record.
  // If so, a suffix _i is added where i makes the name unique.
  // The excludeDM-th dm is excluded, so comparing to itself can be avoided.
  static String uniqueName (const Record& dminfo, const String& name,
                            int32_t excludeDM=-1);

  // Merge the second DataManagerInfo record into the first one.
  // If the same column occurs in both records, the second one is used.
  // Columns having the same data manager name are combined in one data manager.
  // If the second one has no name, it is considered to be equal to the first
  // data manager of that type.
  static void mergeInfo (Record&, const Record&);

  // Finalize the merge by merging the dminfo record with the table description
  // to create the final dminfo record.
  // The final dminfo record gets all columns in the TableDesc object.
  // The given dminfo object is leading in determining a column's data manager.
  // If not present, the data manager type given in the TableDesc is used.
  // If empty, StandardStMan is used.
  static Record finalizeMerge (const TableDesc&, const Record& dminfo);

  // Adapt data manager names in dminfo if already used in the table.
  static void adaptNames (Record& dminfo, const Table&);

  // Set the data managers of the given column(s) to the given tiled storage
  // manager (normally TiledShapeStMan or TiledColumnStMan).
  // The columns are combined in a single storage manager, so the function
  // has to be called multiple times if, say, one per column is needed.
  // The columns already having a tiled storage manager are not changed.
  static void setTiledStMan (Record& dminfo, const Vector<String>& columns,
                             const String& dmType, const String& dmName,
                             const IPosition& defaultTileShape);

  // Remove the given columns from the dminfo record and return a vector
  // containing the names of the columns actually removed.
  // The columns having a data manager matching <src>keepType</src> are not
  // removed. Matching means that the beginning of the data manager name
  // has to match, so "Tiled" matches all tiled storagemanagers.
  static Vector<String> removeDminfoColumns (Record& dminfo,
                                             const Vector<String>& columns,
                                             const String& keepType= String());

  // Adjust the data manager types and groups and the
  // hypercolumn definitions to the actual data manager info.
  static void adjustDesc (TableDesc& tabDesc, const Record& dminfo);

  // Show the Table IO statistics.
  static void showDataManStats (const Table&, ostream&);

private:
  // Merge the column info of data manager definitions.
  // It is used by <src>mergeInfo</src> to merge the new dm definitions into
  // the existing one defined in <src>dminfo</src>. It is called for each new
  // dm, whose name/type already exists as the dmindex-th record in dminfo.
  // It does two things:
  // <ul>
  //  <li>Columns mentioned in newdm are removed from dm definitions in dminfo.
  //  <li>Columns in the dmindex-th dminfo record are merged into newdm,
  //      so mergeInfo can redefine that dm in the overall dminfo.
  // </ul>
  static void mergeColumns (Record& dminfo, uint32_t dmindex, Record& newdm);
};



} //# NAMESPACE CASACORE - END

#endif
