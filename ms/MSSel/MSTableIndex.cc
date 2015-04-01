//# MSTableIndex.cc:  this defined MSTableIndex
//# Copyright (C) 2000, 2001
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

#include <casacore/ms/MSSel/MSTableIndex.h>

#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Containers/RecordDesc.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/Slice.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/tables/Tables/ColumnsIndex.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/BasicMath/Math.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

MSTableIndex::MSTableIndex()
    : timeVals_p(0), intervalVals_p(0), key_p(0), time_p(0.0), interval_p(0.0),
      lastTime_p(0.0), lastInterval_p(0.0), lastNearest_p(0), nearestFound_p(False), 
      nearestReady_p(False), nrows_p(0), hasChanged_p(True), index_p(0), 
      hasTime_p(False), hasInterval_p(False)
{;}

MSTableIndex::MSTableIndex(const Table &subTable,
                           const Vector<String> &indexCols,
                           ColumnsIndex::Compare* compareFunction)
    : timeVals_p(0), intervalVals_p(0), key_p(0), time_p(0.0), interval_p(0.0),
      lastTime_p(0.0), lastInterval_p(0.0), lastNearest_p(0), nearestFound_p(False), 
      nearestReady_p(False), nrows_p(0), hasChanged_p(True), index_p(0), 
      hasTime_p(False), hasInterval_p(False)
{
    attach(subTable, indexCols, compareFunction);
}

MSTableIndex::MSTableIndex(const MSTableIndex &other)
    : timeVals_p(0), intervalVals_p(0), key_p(0), time_p(0.0), interval_p(0.0),
      lastTime_p(0.0), lastInterval_p(0.0), lastNearest_p(0), nearestFound_p(False), 
      nearestReady_p(False), nrows_p(0), hasChanged_p(True), index_p(0), 
      hasTime_p(False), hasInterval_p(False)
{
    *this = other;
}

MSTableIndex::~MSTableIndex()
{
    clear();
}

MSTableIndex &MSTableIndex::operator=(const MSTableIndex &other)
{
    if (this != &other) {
	clear();
	if (!other.tab_p.isNull()) {
	    tab_p = other.tab_p;
	    timeColumn_p.reference(other.timeColumn_p);
	    intervalColumn_p.reference(other.intervalColumn_p);
	    timeVec_p = other.timeVec_p;
	    if (other.timeVals_p) {
		timeVals_p = timeVec_p.getStorage(deleteItTime_p);
	    }
	    intervalVec_p = other.intervalVec_p;
	    if (other.intervalVals_p) {
		intervalVals_p = intervalVec_p.getStorage(deleteItInterval_p);
	    }
	    
	    hasTime_p = other.hasTime_p;
	    hasInterval_p = other.hasInterval_p;
	    // there might not be any index columns 
	    if (other.key_p) {
		key_p = new Record(*other.key_p);
		AlwaysAssert(key_p, AipsError);
		index_p = new ColumnsIndex(*other.index_p);
		AlwaysAssert(index_p, AipsError);
		makeKeys();
		lastKeys_p = other.lastKeys_p;
	    }
	    time_p = other.time_p;
	    interval_p = other.interval_p;
	    lastTime_p = other.lastTime_p;
	    lastInterval_p = other.lastInterval_p;
	    lastSearch_p = other.lastSearch_p;
	    lastNearest_p = other.lastNearest_p;
	    nearestFound_p = other.nearestFound_p;
	    nearestReady_p = other.nearestReady_p;
	    nrows_p = other.nrows_p;
	    hasChanged_p = other.hasChanged_p;
	}
    }
    return *this;
}

void MSTableIndex::attach(const Table &subTable,
			  const Vector<String> &indexCols,
                          ColumnsIndex::Compare* compareFunction)
{
    clear();
    tab_p = subTable;
    // is there a TIME column
    hasTime_p = tab_p.tableDesc().isColumn("TIME");
    // is there an INTERVAL column, there must also be a TIME
    hasInterval_p = hasTime_p && tab_p.tableDesc().isColumn("INTERVAL");
    uInt nkeys = indexCols.nelements();

    if (hasTime_p) {
	timeColumn_p.attach(tab_p, "TIME");
	// fish out the values
	timeVec_p = timeColumn_p.getColumn();
	timeVals_p = timeVec_p.getStorage(deleteItTime_p);

	// interval requires a TIME
	if (hasInterval_p) {
	    intervalColumn_p.attach(tab_p, "INTERVAL");
	    // fish out the values
	    intervalVec_p = intervalColumn_p.getColumn();
	    intervalVals_p = intervalVec_p.getStorage(deleteItInterval_p);
	}
    }

    if (indexCols.nelements() > 0) {    
	index_p = new ColumnsIndex(tab_p, indexCols, compareFunction);
	AlwaysAssert(index_p, AipsError);

	RecordDesc keyDesc;
	for (uInt i=0;i<nkeys;i++) keyDesc.addField(indexCols(i), TpInt);
	key_p =new Record(keyDesc);
	AlwaysAssert(key_p, AipsError);
	
	makeKeys();

	lastKeys_p = 0;
	nrows_p = tab_p.nrow();
    } else if (!hasTime_p) {
	// There's nothing here that we know how to search on
	clear();
    }
}

void MSTableIndex::setChanged()
{
    hasChanged_p = True;
    if (index_p) index_p->setChanged();
}

Vector<uInt> MSTableIndex::getRowNumbers()
{
    getInternals();
    return lastSearch_p;
}

uInt MSTableIndex::getNearestRow(Bool &found)
{
    // getInternals ensures that lastSearch_p is the match to the integer keys
    getInternals();
    if (!nearestReady_p) {
	// search for nearest one
	nearestFound_p = False;
	lastNearest_p = 0;
	if (lastSearch_p.nelements() > 0) {
	    if (!hasTime_p) {
		// just integer keys, there should be just one value, just return
		// the first one if there is one
		nearestFound_p = True;
		lastNearest_p = lastSearch_p(0);
	    } else {
		if (hasInterval_p) {
		    if (intervalVals_p[lastSearch_p(0)] == 0) {
			// no time dependence, should just be one value although
			// we don't check for that here, return the first one
			// found
			lastNearest_p = lastSearch_p(0);
			nearestFound_p = True;
		    } else {
			// strict time search
			nearestTime();
		    }
		} else {
		    // strict time search
		    nearestTime();
		}
	    }
	}
	nearestReady_p = True;
    }
    found = nearestFound_p;
    return lastNearest_p;
}

void MSTableIndex::nearestTime()
{
    // this is only called when we know it is a strict time search and there
    // are elements in lastSearch_p, etc, etc.
    // this should probably be done with a call to binSearch
    Int thisElem = 0;
    Int nElem = lastSearch_p.nelements();
    Bool deleteIt;
    const uInt *rowPtr = lastSearch_p.getStorage(deleteIt);
    while (!nearestFound_p && thisElem < nElem) {
	uInt thisRow = rowPtr[thisElem];
	// needs column unit conversion here to seconds
	nearestFound_p = time_p < timeVals_p[thisRow];
	thisElem++;
    }
    if (nearestFound_p) {
	thisElem--;
	// thisElem is the element where time_p became less that the timeColumn at that row
	// so, is it closer to thisElem or the one before
	if (thisElem <= 0) {
	    thisElem = 0;
	} else {
	    Double lowDiff = time_p - timeVals_p[rowPtr[thisElem-1]];
	    Double highDiff = timeVals_p[rowPtr[thisElem]] - time_p;
	    thisElem = lowDiff > highDiff ? thisElem : thisElem-1;
	}
    } else if (nElem > 0) {
	// just return the last one
	thisElem = nElem-1;
	nearestFound_p = True;
    }
    lastNearest_p = rowPtr[thisElem];
    // okay, we now know where the nearest time is, but is it really the one that
    // we wanted.
    if (hasInterval_p && intervalVals_p[lastNearest_p] == -1) {
	// this is an indeterminate interval
	if (time_p < timeVals_p[lastNearest_p] && !near(time_p, timeVals_p[lastNearest_p])) {
	    // we actually want the previous one - assumes that they are all indeterminate
	    if (thisElem == 0) {
		// there is no match possible here
		nearestFound_p = False;
	    } else {
		lastNearest_p = rowPtr[thisElem-1];
	    }
	} // we have the correct one
    } else {
	// final check to make sure the intervals satisfy the criteria
	Double thisLowTime, thisHighTime;
	Double searchLowTime, searchHighTime;
	if (hasInterval_p) {
	    Double width = intervalVals_p[lastNearest_p];
	    thisLowTime = timeVals_p[lastNearest_p] - width/2.0;
	    thisHighTime = thisLowTime + width;
	} else {
	    thisLowTime = thisHighTime = timeVals_p[lastNearest_p];
	}
	searchLowTime = time_p - interval_p/2.0;
	searchHighTime = searchLowTime + interval_p;
	if (thisHighTime < searchLowTime || thisLowTime > searchHighTime) {
	    // out of range, no match possible
	    nearestFound_p = False;
	}
	// If this were in a separate function, some code duplication could
	// be avoided.
	if (!nearestFound_p) {
	    // it might belong to a neighboring interval
	    if (hasInterval_p) {
		nearestFound_p = True;
		if (time_p<timeVals_p[lastNearest_p]) lastNearest_p--;
		else lastNearest_p++;
		if (lastNearest_p >= 0 && lastNearest_p < nElem) {
		    // double check
		    Double width = intervalVals_p[lastNearest_p];
		    thisLowTime = timeVals_p[lastNearest_p] - width/2.0;
		    thisHighTime = thisLowTime + width;
		    searchLowTime = time_p - interval_p/2.0;
		    searchHighTime = searchLowTime + interval_p;
		    if (thisHighTime < searchLowTime || thisLowTime > searchHighTime) {
			// out of range, no match possible
			nearestFound_p = False;
		    }
		} else {
		    // nope, it really isn't there
		    if (lastNearest_p < 0) lastNearest_p = 0;
		    else lastNearest_p = nElem -1;
		    nearestFound_p = False;
		}
	    }
	}
    }
	    
    lastSearch_p.freeStorage(rowPtr, deleteIt);
}

void MSTableIndex::makeKeys()
{
    // resize as appropriate
    uInt nKeys = key_p->nfields();
    intKeys_p.resize(nKeys);
    lastKeys_p.resize(nKeys);
    indexKeys_p.resize(index_p->accessKey().nfields());

    for (uInt i=0;i<nKeys;i++) {
	intKeys_p[i].attachToRecord(*key_p, i);
	indexKeys_p[i].attachToRecord(index_p->accessKey(), i);
    }

    lastKeys_p = -1;
}

void MSTableIndex::clear() 
{
    hasTime_p = hasInterval_p = nearestFound_p = nearestReady_p = False;
    delete index_p;
    index_p = 0;
    indexKeys_p.resize(0);

    delete key_p;
    key_p = 0;
    intKeys_p.resize(0);

    nrows_p = 0;
    hasChanged_p = True;

    lastSearch_p.resize(0);

    lastNearest_p = 0;
    lastKeys_p.resize(0);

    time_p = interval_p = 0.0;

    tab_p = Table();
}

void MSTableIndex::getInternals()
{
    if (!isNull() && (hasChanged_p ||
	tab_p.nrow() != nrows_p ||
	keysChanged())) {
	nrows_p = tab_p.nrow();
	if (index_p) {
	    uInt nkeys = intKeys_p.nelements();
	    lastKeys_p.resize(nkeys);
	    for (uInt i=0;i<nkeys;i++) {
		Int thisKey = *(intKeys_p[i]);
		*(indexKeys_p[i]) = thisKey;
		lastKeys_p(i) = thisKey;
	    }
	    lastSearch_p.resize(0);
	    lastSearch_p = index_p->getRowNumbers();
	} else if (hasTime_p) {
	    // all rows match at this point
	    lastSearch_p.resize(nrows_p);
	    indgen(lastSearch_p);
	} // nothing can match, lastSearch_p should already have zero elements
	lastTime_p = time_p;
	lastInterval_p = interval_p;
	nearestReady_p = False;
	hasChanged_p = False;
    }
}

Bool MSTableIndex::keysChanged()
{
    Bool result = False;
    for (uInt i=0;i<intKeys_p.nelements();i++) {
	if (*(intKeys_p[i]) != lastKeys_p(i)) {
	    result = True;
	    break;
	}
    }
    if (!result && hasTime_p && time_p != lastTime_p) result = True;
    if (!result && hasInterval_p && interval_p != lastInterval_p) result = True;
    return result;
}


} //# NAMESPACE CASACORE - END

