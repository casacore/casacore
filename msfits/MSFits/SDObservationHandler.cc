//# SDObservationHandler.cc: an OBSERVATION handler for SDFITS data  
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
#include <casacore/msfits/MSFits/SDObservationHandler.h>

#include <casacore/tables/Tables/ColumnsIndex.h>
#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/ms/MeasurementSets/MSObsColumns.h>
#include <casacore/ms/MeasurementSets/MSObservation.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/casa/BasicMath/Math.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

SDObservationHandler::SDObservationHandler() 
    : index_p(0), msObs_p(0), msObsCols_p(0), rownr_p(-1)
{;}

SDObservationHandler::SDObservationHandler(MeasurementSet &ms, Vector<Bool> &handledCols,
				   const Record &row) 
    : index_p(0), msObs_p(0), msObsCols_p(0), rownr_p(-1)
{
    initAll(ms, handledCols, row);
}

SDObservationHandler::SDObservationHandler(const SDObservationHandler &other) 
    : index_p(0), msObs_p(0), msObsCols_p(0), rownr_p(-1)
{
    *this = other;
}

SDObservationHandler &SDObservationHandler::operator=(const SDObservationHandler &other)
{
    if (this != &other) {
	clearAll();
	index_p = new ColumnsIndex(*(other.index_p));
	AlwaysAssert(index_p, AipsError);
	// need to avoid the assignment operator here because we want
	// this to point to the field in index_p, not in other.index_p
	telescopeKey_p.attachToRecord(index_p->accessKey(),
				      MSObservation::columnName(MSObservation::TELESCOPE_NAME));
	observerKey_p.attachToRecord(index_p->accessKey(),
				     MSObservation::columnName(MSObservation::OBSERVER));
	projectKey_p.attachToRecord(index_p->accessKey(),
				      MSObservation::columnName(MSObservation::PROJECT));
	if (index_p->accessKey().fieldNumber("NS_OBSID") >= 0) {
	    ns_obsidKey_p.attachToRecord(index_p->accessKey(), "NS_OBSID");
	}
	releaseDateKey_p.attachToRecord(index_p->accessKey(),
					MSObservation::columnName(MSObservation::RELEASE_DATE));
	flagRowKey_p.attachToRecord(index_p->accessKey(),
				    MSObservation::columnName(MSObservation::FLAG_ROW));
	msObs_p = new MSObservation(*(other.msObs_p));
	AlwaysAssert(msObs_p, AipsError);
	msObsCols_p = new MSObservationColumns(*msObs_p);
	AlwaysAssert(msObsCols_p, AipsError);
	if (ns_obsidKey_p.isAttached()) {
	    nsObsIdCol_p.attach(*msObs_p, "NS_OBSID");
	}
	
	rownr_p = other.rownr_p;
	
	// this should point to the same field as that in other
	observer_p = other.observer_p;
	projid_p = other.projid_p;
	obsid_p = other.obsid_p;
	releaseDate_p = other.releaseDate_p;
	flagRow_p = other.flagRow_p;
	timeRange_p = other.timeRange_p;
    }
    return *this;
}

void SDObservationHandler::attach(MeasurementSet &ms, Vector<Bool> &handledCols, const Record &row)
{
    clearAll();
    initAll(ms, handledCols, row);
}

void SDObservationHandler::resetRow(const Record &row) 
{
    clearRow();
    Vector<Bool> dummyHandled;
    initRow(dummyHandled, row);
}

void SDObservationHandler::fill(const Record &, const String &telescopeName,
				const Vector<Double> &timeRange)
{
    // don't bother unless there is something there
    if (msObs_p) {
	// NS_OBSID key must be set first since it might cause a new column
	// to be add which will require that the index be remade, which will
	// un-attach the other index key field pointers
	if (obsid_p.isAttached() && (*obsid_p).length() > 0) {
	    if (!ns_obsidKey_p.isAttached()) {
		// need to add a new column to hold this field
		msObs_p->addColumn(ScalarColumnDesc<String>("NS_OBSID",
							    "SDFITS OBSID keyword/column value"));
		nsObsIdCol_p.attach(*msObs_p, "NS_OBSID");
		// and renake the index with this column
		makeIndex();
	    }
	    *ns_obsidKey_p = *obsid_p;
	}
	*telescopeKey_p = telescopeName;
	if (observer_p.isAttached()) {
	    // fill the key with the observer name
	    *observerKey_p = *observer_p;
	} else {
	    // just use an empty string as the observer key
	    *observerKey_p = "";
	}
	if (projid_p.isAttached()) {
	    // fill the key with the observer name
	    *projectKey_p = *projid_p;
	} else {
	    // just use an empty string as the project key
	    *projectKey_p = "";
	}
	if (releaseDate_p.isAttached()) {
	    *releaseDateKey_p = *releaseDate_p;
	} else {
	    // use a time of 0.0 as the default release date
	    *releaseDateKey_p = 0.0;
	}
	if (flagRow_p.isAttached()) {
	    *flagRowKey_p = *flagRow_p;
	} else {
	    // default is not flag the row
	    *flagRowKey_p = False;
	}
	Bool found = False;
	uInt whichRow =0;

	// if there is a time range field, there may be more than one matching row
	if (timeRange_p.isAttached()) {
	    Vector<uInt> rows = index_p->getRowNumbers();
	    uInt whichElement = 0;
	    while (!found && whichElement < rows.nelements()) {
		whichRow = rows(whichElement++);
		if (allEQ(*timeRange_p, msObsCols_p->timeRange()(whichRow))) found = True;
	    }
	} else {
	    // otherwise, there should be just a single match
	    whichRow = index_p->getRowNumber(found);
	}

	if (found) {
	    // we have a winner
	    rownr_p = whichRow;
	    if (!timeRange_p.isAttached()) {
		updateTimeRange(timeRange);
	    }
	} else {
	    // we need to add one
	    rownr_p = msObs_p->nrow();
	    msObs_p->addRow();
	    Vector<String> emptySVec(1);
	    msObsCols_p->flagRow().put(rownr_p, *flagRowKey_p);
	    msObsCols_p->log().put(rownr_p, emptySVec);
	    msObsCols_p->observer().put(rownr_p, *observerKey_p);
	    msObsCols_p->project().put(rownr_p, *projectKey_p);
	    msObsCols_p->releaseDate().put(rownr_p, *releaseDateKey_p);
	    msObsCols_p->schedule().put(rownr_p,emptySVec);
	    msObsCols_p->scheduleType().put(rownr_p,"");
	    msObsCols_p->telescopeName().put(rownr_p, *telescopeKey_p);
	    if (timeRange_p.isAttached()) {
		msObsCols_p->timeRange().put(rownr_p, *timeRange_p);
	    } else {
		msObsCols_p->timeRange().put(rownr_p, timeRange);
	    }
	    // the NS_OBSID column if available
	    if (!nsObsIdCol_p.isNull()) {
		nsObsIdCol_p.put(rownr_p, *ns_obsidKey_p);
	    }
	}
    }
}

void SDObservationHandler::clearAll()
{
    delete index_p;
    index_p = 0;

    delete msObs_p;
    msObs_p = 0;

    delete msObsCols_p;
    msObsCols_p = 0;

    clearRow();
}

void SDObservationHandler::clearRow()
{
    observer_p.detach();
    projid_p.detach();
    obsid_p.detach();
    releaseDate_p.detach();
    flagRow_p.detach();
    timeRange_p.detach();

    rownr_p = -1;
}

void SDObservationHandler::initAll(MeasurementSet &ms, Vector<Bool> &handledCols, const Record &row)
{
    msObs_p = new MSObservation(ms.observation());
    AlwaysAssert(msObs_p, AipsError);

    msObsCols_p = new MSObservationColumns(*msObs_p);
    AlwaysAssert(msObsCols_p, AipsError);

    if (msObs_p->tableDesc().isColumn(String("NS_OBSID"))) {
	nsObsIdCol_p.attach(*msObs_p, "NS_OBSID");
    }

    makeIndex();
    initRow(handledCols, row);
}

void SDObservationHandler::makeIndex() 
{
    // ensure that any existing index is first deleted
    delete index_p;
    index_p = 0;

    Int nKeys = 5;
    if (!nsObsIdCol_p.isNull()) nKeys++;

    Vector<String> keys(nKeys);
    keys(0) = MSObservation::columnName(MSObservation::TELESCOPE_NAME);
    keys(1) = MSObservation::columnName(MSObservation::OBSERVER);
    keys(2) = MSObservation::columnName(MSObservation::PROJECT);
    keys(3) = MSObservation::columnName(MSObservation::RELEASE_DATE);
    keys(4) = MSObservation::columnName(MSObservation::FLAG_ROW);
    if (!nsObsIdCol_p.isNull()) {
	keys(5) = "NS_OBSID";
    }
    index_p = new ColumnsIndex(*msObs_p, keys);
    AlwaysAssert(index_p, AipsError);
    

    telescopeKey_p.attachToRecord(index_p->accessKey(),
				  MSObservation::columnName(MSObservation::TELESCOPE_NAME));
    observerKey_p.attachToRecord(index_p->accessKey(),
				 MSObservation::columnName(MSObservation::OBSERVER));
    projectKey_p.attachToRecord(index_p->accessKey(),
				MSObservation::columnName(MSObservation::PROJECT));
    releaseDateKey_p.attachToRecord(index_p->accessKey(),
				    MSObservation::columnName(MSObservation::RELEASE_DATE));
    flagRowKey_p.attachToRecord(index_p->accessKey(),
				MSObservation::columnName(MSObservation::FLAG_ROW));
    if (!nsObsIdCol_p.isNull()) {
	ns_obsidKey_p.attachToRecord(index_p->accessKey(),"NS_OBSID");
    }
}

void SDObservationHandler::initRow(Vector<Bool> &handledCols, const Record &row)
{
    AlwaysAssert(handledCols.nelements()==row.description().nfields(), AipsError);

    if (row.fieldNumber("OBSERVER") >= 0) {
	observer_p.attachToRecord(row, "OBSERVER");
	handledCols(row.fieldNumber("OBSERVER")) = True;
    }

    if (row.fieldNumber("PROJID") >= 0) {
	projid_p.attachToRecord(row, "PROJID");
	handledCols(row.fieldNumber("PROJID")) = True;
    }

    if (row.fieldNumber("OBSID") >= 0) {
	obsid_p.attachToRecord(row, "OBSID");
	handledCols(row.fieldNumber("OBSID")) = True;
    }

    if (row.fieldNumber("OBSERVATION_FLAG_ROW") >= 0) {
	flagRow_p.attachToRecord(row, "OBSERVATION_FLAG_ROW");
	handledCols(row.fieldNumber("OBSERVATION_FLAG_ROW")) = True;
    }

    if (row.fieldNumber("OBSERVATION_RELEASE_DATE") >= 0) {
	releaseDate_p.attachToRecord(row, "OBSERVATION_RELEASE_DATE");
	handledCols(row.fieldNumber("OBSERVATION_RELEASE_DATE")) = True;
    }

    if (row.fieldNumber("OBSERVATION_TIME_RANGE") >= 0) {
	timeRange_p.attachToRecord(row, "OBSERVATION_TIME_RANGE");
	handledCols(row.fieldNumber("OBSERVATION_TIME_RANGE")) = True;
    }

    // ignore MAIN_OBSERVATION_ID
    if (row.fieldNumber("MAIN_OBSERVATION_ID") >= 0) {
	handledCols(row.fieldNumber("MAIN_OBSERVATION_ID")) = True;
    }

    // row number isn't set until the following fill
    rownr_p = -1;
}

void SDObservationHandler::updateTimeRange(const Vector<Double> &timeRange)
{
    if (rownr_p >= 0) {
	Vector<Double> oldTimeRange = msObsCols_p->timeRange()(rownr_p);
	oldTimeRange(0) = min(oldTimeRange(0),timeRange(0));
	oldTimeRange(1) = max(oldTimeRange(1),timeRange(1));
	msObsCols_p->timeRange().put(rownr_p, oldTimeRange);
    }
}

} //# NAMESPACE CASACORE - END

