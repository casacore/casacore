//# TableSyncData.h: Class to hold table synchronization data
//# Copyright (C) 1997,1999
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

#ifndef TABLES_TABLESYNCDATA_H
#define TABLES_TABLESYNCDATA_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/IO/MemoryIO.h>
#include <casacore/casa/IO/AipsIO.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary> 
// Class to hold table synchronization data.
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tTable" demos="">
// </reviewed>

// <prerequisite> 
//    <li> class <linkto class=Table>TableLockData</linkto>
// </prerequisite>

// <synopsis> 
// This class keeps counters to synchronize the table data when a table
// is locked or unlocked.
// <br>
// A few counters are kept by this class:
// <ul>
//  <li> The numbers of rows in the table.
//  <li> The number of columns in the table.
//  <li> The table change counter.
//  <li> A change counter per data manager.
// </ul>
// When a lock on the table is acquired, it reads the sync data from the
// lock file and determines if anything has changed. If so, the necessary
// steps are taken to reread the table data when needed.
// <br>
// When a lock on the table is released, it updates and writes the sync data
// which tells if table data have changed.
// <p>
// This class can also be used for the synchronization of tables and
// external fillers (see class
// <linkto class=ExternalLockSync>ExternalLockSync</linkto>). For this
// purpose it is sufficient to store the number of rows.
// </synopsis>


class TableSyncData
{
public: 
    TableSyncData();

    ~TableSyncData();

    // Update the synchronization data and write it into the MemoryIO object.
    // This function is called when a table flush is done to reflect
    // if anything has changed compared to the previous flush.
    void write (uInt nrrow, uInt nrcolumn, Bool tableChanged,
		const Block<Bool>& dataManChanged);

    // Update the synchronization data and write it into the MemoryIO object.
    // This function should be used by an external filler when it flushes
    // its data.
    void write (uInt nrrow);

    // Read the synchronization data from the MemoryIO object.
    // This function is called when a lock is acquired to see if
    // table data has to be reread.
    // <br>It returns False when the MemoryIO object is empty.
    Bool read (uInt& nrrow, uInt& nrcolumn, Bool& tableChanged,
	       Block<Bool>& dataManChanged);

    // Get the MemoryIO object.
    // This is used to let <src>LockFile</src> read or write the
    // synchronization data into it.
    MemoryIO& memoryIO();

    // Get the modify counter.
    uInt getModifyCounter() const;


private:
    // Copy constructor is forbidden.
    TableSyncData (const TableSyncData& that);

    // Assignment is forbidden.
    TableSyncData& operator= (const TableSyncData& that);


    //# Member variables.
    uInt        itsNrrow;
    Int         itsNrcolumn;
    uInt        itsModifyCounter;
    uInt        itsTableChangeCounter;
    Block<uInt> itsDataManChangeCounter;
    MemoryIO    itsMemIO;
    AipsIO      itsAipsIO;
};



inline MemoryIO& TableSyncData::memoryIO()
{
    return itsMemIO;
}
inline uInt TableSyncData::getModifyCounter() const
{
    return itsModifyCounter;
}




} //# NAMESPACE CASACORE - END

#endif
