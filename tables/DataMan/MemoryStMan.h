//# MemoryStMan.h: Storage manager for tables using memory
//# Copyright (C) 2003
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

#ifndef TABLES_MEMORYSTMAN_H
#define TABLES_MEMORYSTMAN_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/MSMBase.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Memory-based table storage manager class
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=DataManager>DataManager</linkto>
//   <li> <linkto class=MSMColumn>MSMColumn</linkto>
// </prerequisite>

// <synopsis> 
// MemoryStMan is a table storage manager based in memory.
// It holds all data in the columns in memory and deletes them
// when the table gets closed.
// It contains pointers to the underlying MSMColumn objects,
// which do the actual data handling.
//
// The Memory storage manager does fully support addition and removal
// of rows and columns.
//
// The primary use of this storage manager is for a memory-based table,
// but it can also be used for temporary columns in disk-based tables.
// When reopening a disk-based table, possible columns stored with
// MemoryStMan will be initialized to 0.
// An important issue is synchronizing tables containing MemoryStMan
// storage managers in case of concurrent access. Because its data are
// not stored on disk, there is no way to synchronize the data if another
// process changed data or added or deleted rows. If the number or rows
// has changed, rows will be added or deleted as needed. Row deletion
// will be done at the end of the table.
// </synopsis> 

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>


class MemoryStMan: public MSMBase
{
public:
  // Create an Memory storage manager.
  // Its name will be blank.
  MemoryStMan();

  // Create an Memory storage manager with the given name.
  // Its name can be used later in e.g. Table::addColumn to
  // add a column to this storage manager.
  MemoryStMan (const String& storageManagerName);

  ~MemoryStMan();
};



} //# NAMESPACE CASACORE - END

#endif
