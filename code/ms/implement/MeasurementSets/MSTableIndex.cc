//# NewMSTableIndex.cc:  this defined NewMSTableIndex
//# Copyright (C) 2000
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

#include <trial/MeasurementSets/NewMSTableIndex.h>

#include <aips/Containers/Record.h>
#include <aips/Containers/RecordDesc.h>
#include <aips/Exceptions/Error.h>
#include <aips/Tables/ColumnsIndex.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/String.h>

NewMSTableIndex::NewMSTableIndex()
    : key_p(0), time_p(0.0), interval_p(0.0), lastTime_p(0.0), lastInterval_p(0.0),
      lastNearest_p(0), nearestFound_p(False), nearestReady_p(False), nrows_p(0),
      hasChanged_p(True), index_p(0), hasTime_p(False), hasInterval_p(False)
{;}

NewMSTableIndex::NewMSTableIndex(const Table &subTable,
				 const Vector<String> &indexCols)
    : key_p(0), time_p(0.0), interval_p(0.0), lastTime_p(0.0), lastInterval_p(0.0),
      lastNearest_p(0), nearestFound_p(False), nearestReady_p(False), nrows_p(0),
      hasChanged_p(True), index_p(0), hasTime_p(False), hasInterval_p(False)
{
    attach(subTable, indexCols);
}

NewMSTableIndex::NewMSTableIndex(const NewMSTableIndex &other)
    : key_p(0), time_p(0.0), interval_p(0.0), lastTime_p(0.0), lastInterval_p(0.0),
      lastNearest_p(0), nearestFound_p(False), nearestReady_p(False), nrows_p(0),
      hasChanged_p(True), index_p(0), hasTime_p(False), hasInterval_p(False)
{
    *this = other;
}

NewMSTableIndex::~NewMSTableIndex()
{
    clear();
}

NewMSTableIndex &NewMSTableIndex::operator=(const NewMSTableIndex &other)
{
    if (this != &other) {
	clear();
	tab_p = other.tab_p;
	key_p = new Record(*other.key_p);
	AlwaysAssert(key_p, AipsError);
	index_p = new ColumnsIndex(*other.index_p);
	AlwaysAssert(index_p, AipsError);
	hasTime_p = other.hasTime_p;
	hasInterval_p = other.hasInterval_p;
	makeKeys();
	time_p = other.time_p;
	interval_p = other.interval_p;
	lastKeys_p = other.lastKeys_p;
	lastTime_p = other.lastTime_p;
	lastInterval_p = other.lastInterval_p;
	lastSearch_p = other.lastSearch_p;
	lastNearest_p = other.lastNearest_p;
	nearestFound_p = other.nearestFound_p;
	nearestReady_p = other.nearestReady_p;
	nrows_p = other.nrows_p;
	hasChanged_p = other.hasChanged_p;
    }
    return *this;
}

void NewMSTableIndex::attach(const Table &subTable,
			     const Vector<String> &indexCols)
{
    clear();
    tab_p = subTable;
    // is there a TIME column
    hasTime_p = tab_p.tableDesc().isColumn("TIME");
    // is there an INTERVAL column, there must also be a TIME
    hasInterval_p = hasTime_p && tab_p.tableDesc().isColumn("INTERVAL");
    uInt nkeys = indexCols.nelements();
    uInt nextraKeys = 0;
    if (hasInterval_p) nextraKeys = 2;
    else if (hasTime_p) nextraKeys = 1;

    Vector<String> fullIndexCols(nkeys + nextraKeys);
    if (nkeys > 0) fullIndexCols(Slice(0,nkeys)) = indexCols;
    if (hasTime_p) {
	fullIndexCols(nkeys) = "TIME";
	// attach the time column here
	timeColumn_p.attach(tab_p, "TIME");
    }
    if (hasInterval_p) fullIndexCols(nkeys+1) = "INTERVAL";

    if (fullIndexCols.nelements() > 0) {    
	index_p = new ColumnsIndex(tab_p, fullIndexCols, NewMSTableIndex::compare);
	AlwaysAssert(index_p, AipsError);

	RecordDesc keyDesc;
	for (uInt i=0;i<nkeys;i++) keyDesc.addField(indexCols(i), TpInt);
	key_p =new Record(keyDesc);
	AlwaysAssert(key_p, AipsError);
	
	makeKeys();

	lastKeys_p = 0;
	nrows_p = tab_p.nrow();
    } else {
	// if there is nothing in fullIndexCols by now, there is no way to index this,
	// so treat this as being unattached
	clear();
    }
}

void NewMSTableIndex::setChanged()
{
    hasChanged_p = True;
    if (index_p) index_p->setChanged();
}

Vector<uInt> NewMSTableIndex::getRowNumbers()
{
    getInternals();
    return lastSearch_p;
}

uInt NewMSTableIndex::getNearestRow(Bool &found)
{
    getInternals();
    if (!nearestReady_p) {
	// search for nearest one
	nearestFound_p = False;
	lastNearest_p = 0;
	if (!hasTime_p) {
	    // just integer keys, there should be just one value, just return
	    // the first one if there is one
	    if (lastSearch_p.nelements() > 0) {
		nearestFound_p = True;
		lastNearest_p = lastSearch_p(0);
	    } // otherwise, do nothing
	} else {
	    uInt thisElem = 0;
	    uInt nElem = lastSearch_p.nelements();
	    while (!nearestFound_p && thisElem < nElem) {
		uInt thisRow = lastSearch_p(thisElem);
		// needs column unit conversion here to seconds
		nearestFound_p = time_p < timeColumn_p(thisRow);
		thisElem++;
	    }
	    if (nearestFound_p) {
		thisElem--;
		// thisElem is the element where time_p became less that the timeColumn at that row
		// so, is it closer to thisElem or the one before
		if (thisElem == 0) {
		    lastNearest_p = lastSearch_p(0);
		} else {
		    Double lowDiff = time_p - timeColumn_p(thisElem-1);
		    Double highDiff = timeColumn_p(thisElem) - time_p;
		    lastNearest_p = lowDiff < highDiff ? lastSearch_p(thisElem-1) : lastSearch_p(thisElem);
		}
	    } else if (nElem > 0) {
	      // just return the last one
	      nearestFound_p = True;
	      lastNearest_p = lastSearch_p(nElem-1);
	    }
	}
	nearestReady_p = True;
    }
    found = nearestFound_p;
    return lastNearest_p;
}

void NewMSTableIndex::makeKeys()
{
    // resize as appropriate
    uInt nKeys = key_p->nfields();
    intKeys_p.resize(nKeys);
    lastKeys_p.resize(nKeys);
    if (hasInterval_p) {
	upperIndexKeys_p.resize(index_p->accessKey().nfields());
    }
    lowerIndexKeys_p.resize(index_p->accessKey().nfields());

    for (uInt i=0;i<nKeys;i++) {
	intKeys_p[i].attachToRecord(*key_p, i);
	if (hasInterval_p) {
	    upperIndexKeys_p[i].attachToRecord(index_p->accessUpperKey(), i);
	    lowerIndexKeys_p[i].attachToRecord(index_p->accessLowerKey(), i);
	} else {
	    lowerIndexKeys_p[i].attachToRecord(index_p->accessKey(), i);
	}
    }

    lastKeys_p = -1;
    if (hasTime_p) {
	if (hasInterval_p) {
	    upperTimeKey_p.attachToRecord(index_p->accessUpperKey(), "TIME");
	    lowerTimeKey_p.attachToRecord(index_p->accessLowerKey(), "TIME");
	} else {
	    lowerTimeKey_p.attachToRecord(index_p->accessKey(), "TIME");
	}
    }
}

void NewMSTableIndex::clear() 
{
    hasTime_p = hasInterval_p = nearestFound_p = nearestReady_p = False;
    delete index_p;
    index_p = 0;
    upperIndexKeys_p.resize(0);
    lowerIndexKeys_p.resize(0);

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

void NewMSTableIndex::getInternals()
{
    if (!isNull() && (hasChanged_p ||
	tab_p.nrow() != nrows_p ||
	keysChanged())) {
	uInt nkeys = intKeys_p.nelements();
	lastKeys_p.resize(nkeys);
	for (uInt i=0;i<nkeys;i++) {
	    Int thisKey = *(intKeys_p[i]);
	    *(lowerIndexKeys_p[i]) = thisKey;
	    if (hasInterval_p) {
		*(upperIndexKeys_p[i]) = thisKey;
	    }
	    lastKeys_p(i) = thisKey;
	}
	if (hasTime_p) {
	    if (hasInterval_p) {
		*lowerTimeKey_p = time_p-interval_p/2;
		*upperTimeKey_p = time_p+interval_p/2;
	    } else {
		*lowerTimeKey_p = time_p;
	    }
	}
	lastSearch_p.resize(0);
	if (hasInterval_p) {
	    lastSearch_p = index_p->getRowNumbers(True, True);
	} else {
	    lastSearch_p = index_p->getRowNumbers();
	}
	lastTime_p = time_p;
	lastInterval_p = interval_p;
	nearestReady_p = False;
	hasChanged_p = False;
    }
}

Bool NewMSTableIndex::keysChanged()
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

Bool NewMSTableIndex::okDataTypes(const Block<Int> &dataTypes) {
    Bool result = True;
    Bool doubleFound = False;
    uInt nDouble = 0;
    uInt nfield = dataTypes.nelements();
    uInt i=0;
    while (result && i<nfield) {
	if (dataTypes[i] != TpInt && dataTypes[i] != TpDouble) {
	    result = False;
	} else if (dataTypes[i] == TpDouble) {
	    if (doubleFound && nDouble > 2) {
		result = False;
	    } else {
		doubleFound = True;
		nDouble++;
	    }
	}
	i++;
    }
    return result;
}

Int NewMSTableIndex::compare(const Block<void *>& fieldPtrs,
			     const Block<void *>& dataPtrs,
			     const Block<Int> &dataTypes,
			     Int index)
{
    // the keys are assumed to be first the integer keys followed optionally
    // by a TIME key followed optionally by an INTERVAL key, although the
    // value of this INTERVAL key is never used.  Rather, the INTERVAL in the
    // dataPtr is used during the comparison.
    DebugAssert(okDataTypes(dataTypes), AipsError);
    
    Bool timeHandled = False;
    uInt nfield = dataTypes.nelements();
    uInt lastField = nfield;
    if (lastField > 0) lastField--;
    for (uInt i=0; i<nfield; i++) {
	switch (dataTypes[i]) {
	case TpInt:
	    {
		const Int left = *(*(RecordFieldPtr<Int>*)(fieldPtrs[i]));
		const Int right = ((const Int*)(dataPtrs[i]))[index];
		if (left < right) {
		    return -1;
		} else if (left > right) {
		    return 1;
		} 
	    }
	    break;
	case TpDouble:
	    {
		if (!timeHandled) {
		    // this must be the time, get the key
		    const Double key = *(*(RecordFieldPtr<Double>*)(fieldPtrs[i]));
		    // get the time
		    const Double time = ((const Double*)(dataPtrs[i]))[index];
		    // if this isn't at the end, the next one must be an interval
		    if (i < lastField) {
			const Double width = ((const Double*)(dataPtrs[i+1]))[index];
			if (width < 0) {
			    // nope, just TIME, no interval
			    if (key < time) {
				return -1;
			    } else if (key > time) {
				return 1;
			    }
			} else {
			    const Double start = time - width/2;
			    const Double end = time + width/2;
			    if (key < start) {
				return -1;
			    } else if (key > end) {
				return 1;
			    }
			}
		    } else {
			// just TIME, no interval
			if (key < time) {
			    return -1;
			} else if (key > time) {
			    return 1;
			}
		    }
		    timeHandled = True;
		} // otherwise, ignore it, its the interval and we already have used it
	    }
	    break;
	default:
	    // this should never happen
	    throw(AipsError("myCompare: unexpected data type"));
	}
    }
    return 0;
}
