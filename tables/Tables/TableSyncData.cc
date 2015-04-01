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
//#
//# $Id$


//# Includes
#include <casacore/tables/Tables/TableSyncData.h>
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

void TableSyncData::write (uInt nrrow, uInt nrcolumn, Bool tableChanged,
			   const Block<Bool>& dataManChanged)
{
    // Increment change counter when the table has changed.
    Bool changed = False;
    itsNrrow    = nrrow;
    itsNrcolumn = nrcolumn;
    if (tableChanged) {
	itsTableChangeCounter++;
	changed = True;
    }
    // Increment a counter when a data manager has changed.
    // Resize and initialize the block when needed.
    uInt i;
    uInt ndmOld = itsDataManChangeCounter.nelements();
    uInt ndmNew = dataManChanged.nelements();
    if (ndmNew != ndmOld) {
	itsDataManChangeCounter.resize (ndmNew, True, True);
	for (i=ndmOld; i<ndmNew; i++) {
	    itsDataManChangeCounter[i] = 0;
	}
    }
    for (i=0; i<ndmNew; i++) {
	if (dataManChanged[i]) {
	    itsDataManChangeCounter[i]++;
	    changed = True;
	}
    }
    // Increment modify counter if anything has changed.
    if (changed) {
	itsModifyCounter++;
    }
    // Now write the data into the memoryIO object.
    // First clear it.
    itsMemIO.clear();
    itsAipsIO.putstart ("sync" ,1);
    itsAipsIO << itsNrrow;
    itsAipsIO << itsNrcolumn;
    itsAipsIO << itsModifyCounter;
    if (itsNrcolumn >= 0) {
	itsAipsIO << itsTableChangeCounter;
	itsAipsIO << itsDataManChangeCounter;
    }
    itsAipsIO.putend();
}

void TableSyncData::write (uInt nrrow)
{
    itsModifyCounter++;
    itsNrrow = nrrow;
    itsNrcolumn = -1;
    // Now write the data into the memoryIO object.
    // First clear it.
    itsMemIO.clear();
    itsAipsIO.putstart ("sync" ,1);
    itsAipsIO << itsNrrow;
    itsAipsIO << itsNrcolumn;
    itsAipsIO << itsModifyCounter;
    itsAipsIO.putend();
}

Bool TableSyncData::read (uInt& nrrow, uInt& nrcolumn, Bool& tableChanged,
			  Block<Bool>& dataManChanged)
{
    // Read the data into the memoryIO object.
    // When no columns, don't read the remaining part (then it is used
    // by an external filler).
    uInt i;
    Int nrcol = -1;
    if (itsMemIO.length() > 0) {
	itsAipsIO.getstart ("sync");
	itsAipsIO >> nrrow;
	itsAipsIO >> nrcol;
	itsAipsIO >> itsModifyCounter;
    }
    if (nrcol < 0) {
	tableChanged = True;
	dataManChanged.set (True);
	if (itsMemIO.length() > 0) {
	    itsAipsIO.getend();
	    return True;                       // not empty
	}
	nrcolumn = 0;
	return False;                          // empty MemoryIO object
    }
    nrcolumn = nrcol;
    // The table has changed when the change counter has changed.
    uInt tableChangeCounter;
    Block<uInt> dataManChangeCounter;
    itsAipsIO >> tableChangeCounter;
    itsAipsIO >> dataManChangeCounter;
    itsAipsIO.getend();
    tableChanged =  (tableChangeCounter != itsTableChangeCounter);
    itsTableChangeCounter = tableChangeCounter;
    // A data manager has changed when its change counter has changed.
    // Increment a change counter when a data manager has changed.
    // Resize and initialize the array when needed.
    uInt ndmOld = itsDataManChangeCounter.nelements();
    uInt ndmNew = dataManChangeCounter.nelements();
    dataManChanged.resize (ndmNew, True, False);
    dataManChanged.set (False);
    if (ndmNew != ndmOld) {
	itsDataManChangeCounter.resize (ndmNew, True, True);
	for (i=ndmOld; i<ndmNew; i++) {
	    dataManChanged[i] = True;
	    itsDataManChangeCounter[i] = dataManChangeCounter[i];
	}
    }
    for (i=0; i<ndmNew; i++) {
	if (dataManChangeCounter[i] != itsDataManChangeCounter[i]) {
	    dataManChanged[i] = True;
	    itsDataManChangeCounter[i] = dataManChangeCounter[i];
	}
    }
    return True;
}


} //# NAMESPACE CASACORE - END

