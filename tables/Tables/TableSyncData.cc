//# TableSyncData.cc: Class to hold table synchronization data
//# Copyright (C) 1997,1999,2001,2005
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


//# Includes
#include <casacore/tables/Tables/TableSyncData.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/tables/DataMan/DataManager.h>
#include <casacore/casa/Containers/BlockIO.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

TableSyncData::TableSyncData()
: itsNrrow              (0),
  itsNrcolumn           (-1),
  itsModifyCounter      (0),
  itsTableChangeCounter (0)
{
    itsAipsIO.open (&itsMemIO);
}

TableSyncData::~TableSyncData()
{
    itsAipsIO.close();
}

void TableSyncData::write (rownr_t nrrow, uint32_t nrcolumn, bool tableChanged,
			   const Block<bool>& dataManChanged)
{
    // Increment change counter when the table has changed.
    bool changed = false;
    itsNrrow    = nrrow;
    itsNrcolumn = nrcolumn;
    if (tableChanged) {
	itsTableChangeCounter++;
	changed = true;
    }
    // Increment a counter when a data manager has changed.
    // Resize and initialize the block when needed.
    uint32_t ndmOld = itsDataManChangeCounter.nelements();
    uint32_t ndmNew = dataManChanged.nelements();
    if (ndmNew != ndmOld) {
	itsDataManChangeCounter.resize (ndmNew, true, true);
	for (uint32_t i=ndmOld; i<ndmNew; i++) {
	    itsDataManChangeCounter[i] = 0;
	}
    }
    for (uint32_t i=0; i<ndmNew; i++) {
	if (dataManChanged[i]) {
	    itsDataManChangeCounter[i]++;
	    changed = true;
	}
    }
    // Increment modify counter if anything has changed.
    if (changed) {
	itsModifyCounter++;
    }
    // Now write the data into the memoryIO object.
    // Use 32-bit for the row number if it fits.
    // First clear it.
    itsMemIO.clear();
    if (itsNrrow > DataManager::MAXROWNR32) {
      itsAipsIO.putstart ("sync", 2);
      itsAipsIO << itsNrrow;
    } else {
      itsAipsIO.putstart ("sync", 1);
      itsAipsIO << uint32_t(itsNrrow);
    }
    itsAipsIO << itsNrcolumn;
    itsAipsIO << itsModifyCounter;
    if (itsNrcolumn >= 0) {
	itsAipsIO << itsTableChangeCounter;
	itsAipsIO << itsDataManChangeCounter;
    }
    itsAipsIO.putend();
}

void TableSyncData::write (rownr_t nrrow)
{
    itsModifyCounter++;
    itsNrrow = nrrow;
    itsNrcolumn = -1;
    // Now write the data into the memoryIO object.
    // Use 32-bit for the row number if it fits.
    // First clear it.
    itsMemIO.clear();
    if (itsNrrow > DataManager::MAXROWNR32) {
      itsAipsIO.putstart ("sync", 2);
      itsAipsIO << itsNrrow;
    } else {
      itsAipsIO.putstart ("sync", 1);
      itsAipsIO << uint32_t(itsNrrow);
    }
    itsAipsIO << itsNrcolumn;
    itsAipsIO << itsModifyCounter;
    itsAipsIO.putend();
}

bool TableSyncData::read (rownr_t& nrrow, uint32_t& nrcolumn, bool& tableChanged,
			  Block<bool>& dataManChanged)
{
    // Read the data into the memoryIO object.
    // When no columns, don't read the remaining part (then it is used
    // by an external filler).
    int32_t nrcol = -1;
    if (itsMemIO.length() > 0) {
        uint version = itsAipsIO.getstart ("sync");
        if (version > 2) {
          throw TableError ("TableSyncData version " + String::toString(version) +
                            " not supported by this version of Casacore");
        }
        if (version == 1) {
          uint32_t n;
          itsAipsIO >> n;
          nrrow = n;
        } else {
          itsAipsIO >> nrrow;
        }
	itsAipsIO >> nrcol;
	itsAipsIO >> itsModifyCounter;
    }
    if (nrcol < 0) {
	tableChanged = true;
	dataManChanged.set (true);
	if (itsMemIO.length() > 0) {
	    itsAipsIO.getend();
	    return true;                       // not empty
	}
	nrcolumn = 0;
	return false;                          // empty MemoryIO object
    }
    nrcolumn = nrcol;
    // The table has changed when the change counter has changed.
    uint32_t tableChangeCounter;
    Block<uint32_t> dataManChangeCounter;
    itsAipsIO >> tableChangeCounter;
    itsAipsIO >> dataManChangeCounter;
    itsAipsIO.getend();
    tableChanged =  (tableChangeCounter != itsTableChangeCounter);
    itsTableChangeCounter = tableChangeCounter;
    // A data manager has changed when its change counter has changed.
    // Increment a change counter when a data manager has changed.
    // Resize and initialize the array when needed.
    uint32_t ndmOld = itsDataManChangeCounter.nelements();
    uint32_t ndmNew = dataManChangeCounter.nelements();
    dataManChanged.resize (ndmNew, true, false);
    dataManChanged.set (false);
    if (ndmNew != ndmOld) {
	itsDataManChangeCounter.resize (ndmNew, true, true);
	for (uint32_t i=ndmOld; i<ndmNew; i++) {
	    dataManChanged[i] = true;
	    itsDataManChangeCounter[i] = dataManChangeCounter[i];
	}
    }
    for (uint32_t i=0; i<ndmNew; i++) {
	if (dataManChangeCounter[i] != itsDataManChangeCounter[i]) {
	    dataManChanged[i] = true;
	    itsDataManChangeCounter[i] = dataManChangeCounter[i];
	}
    }
    return true;
}


} //# NAMESPACE CASACORE - END

