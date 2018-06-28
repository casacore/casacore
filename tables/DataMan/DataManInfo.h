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
//#
//# $Id$

#ifndef TABLES_DATAMANINFO_H
#define TABLES_DATAMANINFO_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Vector.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations.
class TableDesc;
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
// DataManInfo is a class to manipulate a datamanager info record and table
// descriptions.
// Often an existing table description and datamanager info record is used to
// construct a new table, but it might be necessary to change it somewhat.
// <ul>
//  <li> Remove hypercolumn definitions from a table description. They are
//       not needed anymore and can be a burden.
//  <li> Replace non-writable storage managers (like LofarStMan) by a
//       writable one.
//  <li> Replace the deprecated TiledDataStMan by TiledShapeStMan.
// </ul>
//
// Such things might be necessary in a number
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
                             Bool replaceMSM = True);

  // Set the data managers of the given column(s) to the given tiled storage
  // manager (normally TiledShapeStMan or TiledColumnStMan).
  // The columns are combined in a single storage manager, so the function
  // has to be called multiple times if, say, one per column is needed.
  // The columns already having a tiled storage manager are not changed.
  static void setTiledStMan (Record& dminfo, const Vector<String>& columns,
                             const String& dmType, const String& dmName,
                             const IPosition& defaultTileShape);

  // Remove the columns from the dminfo record and return a vector with the
  // names of the columns actually removed.
  // The columns having a data manager matching <src>keepType</src> are not
  // removed. Matching means that the beginning of the data manager name
  // have to match, so "Tiled" matches all tiled storagemanagers.
  static Vector<String> removeDminfoColumns (Record& dminfo,
                                             const Vector<String>& columns,
                                             const String& keepType= String());

  // Adjust the data manager types and groups and the
  // hypercolumn definitions to the actual data manager info.
  static void adjustDesc (TableDesc& tabDesc, const Record& dminfo);
};



} //# NAMESPACE CASACORE - END

#endif
