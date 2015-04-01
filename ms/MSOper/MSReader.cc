//# MSReader.cc: read from an MS, coordinating all of the subtables
//# Copyright (C) 2000,2002
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

#include <casacore/ms/MSOper/MSReader.h>

#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/measures/Measures/MEpoch.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Utilities/Assert.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

MSReader::MSReader(const MeasurementSet &ms)
    : itsMS(ms), itsMSCols(ms), itsSecUnit("s"), itsIds(ms), itsTabId(-1),
      itsMainId(-1), itsAnt1Id(-1), itsAnt2Id(-1), itsDDId(-1), itsDopplerId(-1),
      itsFeed1Id(-1), itsFeed2Id(-1), itsFieldId(-1), itsFlagCmdId(-1),
      itsFreqOffsetId(-1), itsObsId(-1), itsPointing1Id(-1), itsPointing2Id(-1), 
      itsPolId(-1), itsProcId(-1), itsSourceId(-1), itsSpwId(-1), itsStateId(-1), 
      itsSyscal1Id(-1), itsSyscal2Id(-1), itsWeather1Id(-1), itsWeather2Id(-1)
{
    // assign indexes to every table to start with
    TableRecord kwSet(itsMS.keywordSet());
    uInt idCount = 0;
    for (uInt i=0;i<kwSet.nfields();i++) {
	if (kwSet.type(i) == TpTable) {
	    itsTabId.define(kwSet.name(i),idCount);
	    idCount++;
	} // ignore all other keywords
    }

    // need independent rows for antenna1 and antenna2 and feed1 and feed2

    // ANTENNA1 == ANTENNA
    // ANTENNA is required
    itsAnt1Id = itsTabId("ANTENNA");
    DebugAssert(itsAnt1Id>=0, AipsError);
    itsTabId.define("ANTENNA1", itsAnt1Id);
    // ANTENNA2 gets a new number
    itsTabId.define("ANTENNA2", idCount++);
    itsAnt2Id = itsTabId("ANTENNA2");
    
    // FEED1 == FEED
    // FEED is required
    itsFeed1Id = itsTabId("FEED");
    DebugAssert(itsFeed1Id>=0, AipsError);
    itsTabId.define("FEED1", itsFeed1Id);
    // FEED2 gets a new number
    itsTabId.define("FEED2", idCount++);
    itsFeed2Id = itsTabId("FEED2");

    // there are two POINTING lookups, one for each possible antenna
    // POINTING is a required table
    itsPointing1Id = itsTabId("POINTING");
    DebugAssert(itsPointing1Id>=0, AipsError);
    itsTabId.define("POINTING1", itsPointing1Id);

    // POINTING2 gets a new number
    itsTabId.define("POINTING2", idCount++);
    itsPointing2Id = itsTabId("POINTING2");

    // there are two SYSCAL lookups, one for each possible antenna
    // SYSCAL is an optional table
    itsSyscal1Id = itsTabId("SYSCAL");
    if (itsSyscal1Id >= 0) {
	itsTabId.define("SYSCAL1", itsSyscal1Id);
	// SYSCAL2 gets a new number
	itsTabId.define("SYSCAL2", idCount++);
	itsSyscal2Id = itsTabId("SYSCAL2");
    }

    // there are two WEATHER lookups, one for each possible antenna
    // WEATHER is an optional table
    itsWeather1Id = itsTabId("WEATHER");
    if (itsWeather1Id >= 0) {
	itsTabId.define("WEATHER1", itsWeather1Id);
	// WEATHER2 gets a new number
	itsTabId.define("WEATHER2", idCount++);
	itsWeather2Id = itsTabId("WEATHER2");
    }

    // And the MAIN table needs a presence in the various blocks
    itsTabId.define("MAIN", idCount++);
    itsMainId = itsTabId("MAIN");

    // at this point, we know the size of the things we need == idCount
    Vector<Bool> handledTab(idCount, False);

    itsIndexes.resize(idCount);
    itsTabRows.resize(idCount);
    itsRowNumbers.resize(idCount);
    itsTableNames.resize(idCount);
    itsRowNumbers = -1;

    // MAIN table, no index, just the table row
    itsTabRows[itsMainId] = ROTableRow(itsMS);
    handledTab(itsMainId) = True;

    // ANTENNA1 - no index, indexed simply via ANTENNA1 value
    itsTabRows[itsAnt1Id] = ROTableRow(itsMS.antenna());
    handledTab(itsAnt1Id) = True;
    
    // ANTENNA2 - no index, indexed simply via ANTENNA2 value
    itsTabRows[itsAnt2Id] = ROTableRow(itsMS.antenna());
    handledTab(itsAnt2Id) = True;

    // DATA_DESCRIPTION - no index, indexed simply via DATA_DESC_ID value
    // This table is required.
    itsDDId = itsTabId("DATA_DESCRIPTION");
    DebugAssert(itsDDId>=0, AipsError);
    itsTabRows[itsDDId] = ROTableRow(itsMS.dataDescription());
    handledTab(itsDDId) = True;

    // DOPPLER - has a special index.  This table is OPTIONAL
    itsDopplerId = itsTabId("DOPPLER");
    if (itsDopplerId >= 0) {
	itsDopplerIndex.attach(itsMS.doppler());
	itsTabRows[itsDopplerId] = ROTableRow(itsMS.doppler());
	handledTab(itsDopplerId) = True;
    }

    // FEED1 - indexed
    itsFeed1Index.attach(itsMS.feed());
    itsTabRows[itsFeed1Id] = ROTableRow(itsMS.feed());
    handledTab(itsFeed1Id) = True;


    // FEED2 - indexed
    itsFeed2Index.attach(itsMS.feed());
    itsTabRows[itsFeed2Id] = ROTableRow(itsMS.feed());
    handledTab(itsFeed2Id) = True;

    // FIELD - no index, indexed simply via FIELD_ID value
    // This table is required.
    itsFieldId = itsTabId("FIELD");
    DebugAssert(itsFieldId>=0, AipsError);
    itsTabRows[itsFieldId] = ROTableRow(itsMS.field());
    handledTab(itsFieldId) = True;

    // FLAG_CMD, simple time and interval MSTableIndex
    // This table is required
    itsFlagCmdId = itsTabId("FLAG_CMD");
    DebugAssert(itsFlagCmdId>=0, AipsError);
    itsIndexes[itsFlagCmdId] = MSTableIndex(itsMS.flagCmd(),Vector<String>());
    itsTabRows[itsFlagCmdId] = ROTableRow(itsMS.flagCmd());
    handledTab(itsFlagCmdId) = True;

    // FREQ_OFFSET - indexed
    // This table is optional
    itsFreqOffsetId = itsTabId("FREQ_OFFSET");
    if (itsFreqOffsetId >= 0) {
	itsFreqOffIndex.attach(itsMS.freqOffset());
	itsTabRows[itsFreqOffsetId] = ROTableRow(itsMS.freqOffset());
	handledTab(itsFreqOffsetId) = True;
    }

    // HISTORY - not handled here, this is a required table
    // make sure its marked as undefined in the ID map and mark it as handled here
    Int histId = itsTabId("HISTORY");
    if (histId >= 0) {
	itsTabId.define("HISTORY",-1);
	handledTab(histId) = True;
    }

    // OBSERVATION - not indexed, this is a required table
    itsObsId = itsTabId("OBSERVATION");
    DebugAssert(itsObsId>=0, AipsError);
    itsTabRows[itsObsId] = ROTableRow(itsMS.observation());
    handledTab(itsObsId) = True;

    // POINTING1 and POINTING2 - indexed, this is a required table
    // Allready have itsPointing1Id and itsPointing2Id
    itsPointing1Index.attach(itsMS.pointing());
    itsTabRows[itsPointing1Id] = ROTableRow(itsMS.pointing());
    handledTab(itsPointing1Id) = True;
    itsPointing2Index.attach(itsMS.pointing());
    itsTabRows[itsPointing2Id] = ROTableRow(itsMS.pointing());
    handledTab(itsPointing2Id) = True;

    // POLARIZATION - not indexed, this is a required table
    itsPolId = itsTabId("POLARIZATION");
    DebugAssert(itsPolId>=0, AipsError);
    itsTabRows[itsPolId] = ROTableRow(itsMS.polarization());
    handledTab(itsPolId) = True;

    // PROCESSOR - not indexed, this is a required table
    itsProcId = itsTabId("PROCESSOR");
    DebugAssert(itsProcId>=0, AipsError);
    itsTabRows[itsProcId] = ROTableRow(itsMS.processor());
    handledTab(itsProcId) = True;

    // SOURCE - indexed, this is an optional table
    itsSourceId = itsTabId("SOURCE");
    if (itsSourceId >= 0) {
	itsSourceIndex.attach(itsMS.source());
	itsTabRows[itsSourceId] = ROTableRow(itsMS.source());
	handledTab(itsSourceId) = True;
    }

    // SPECTRAL_WINDOW - not indexed, this is a required table
    itsSpwId = itsTabId("SPECTRAL_WINDOW");
    DebugAssert(itsSpwId>=0, AipsError);
    itsTabRows[itsSpwId] = ROTableRow(itsMS.spectralWindow());
    handledTab(itsSpwId) = True;

    // STATE - not indexed, this is an optional table
    itsStateId = itsTabId("STATE");
    if (itsStateId >= 0) {
	itsTabRows[itsStateId] = ROTableRow(itsMS.state());
	handledTab(itsStateId) = True;
    }

    // SYSCAL1 and SYSCAL2 - indexed, this is an optional table
    // already know itsSyscal1Id and itsSyscal2Id
    if (itsSyscal1Id >= 0) {
	itsSyscal1Index.attach(itsMS.sysCal());
	itsTabRows[itsSyscal1Id] = ROTableRow(itsMS.sysCal());
	handledTab(itsSyscal1Id) = True;
	// SYSCAL2 must exist if SYSCAL1 exists
	itsSyscal2Index.attach(itsMS.sysCal());
	itsTabRows[itsSyscal2Id] = ROTableRow(itsMS.sysCal());
	handledTab(itsSyscal2Id) = True;
    }

    // WEATHER1 and WEATHER2 - indexed, this is an optional table
    // already know itsWeather1Id and itsWeather2Id
    if (itsWeather1Id >= 0) {
	itsWeather1Index.attach(itsMS.weather());
	itsTabRows[itsWeather1Id] = ROTableRow(itsMS.weather());
	handledTab(itsWeather1Id) = True;
	// WEATHER2 must exist if WEATHER1 exists
	itsWeather2Index.attach(itsMS.weather());
	itsTabRows[itsWeather2Id] = ROTableRow(itsMS.weather());
	handledTab(itsWeather2Id) = True;
    }

    // and now, for everything not handled above, also fill in itsTableNames
    Vector<String> tableNames(idCount);
    for (uInt i=0; i<itsTabId.ndefined(); i++) {
	Int tabId = itsTabId.getVal(i);
	if (tabId >= 0) {
	    String tabName = itsTabId.getKey(i);
	    tableNames(tabId) = tabName;
	    if (!handledTab(tabId)) {
		itsIndexes[tabId].attach(kwSet.asTable(tabName), Vector<String>());
		itsTabRows[tabId] = ROTableRow(kwSet.asTable(tabName));
		handledTab(tabId) = True;
	    }
	}
    }
    // copy the non-empty values in tableNames to itsTableNames
    uInt nameCount = 0;
    for (uInt i=0;i<tableNames.nelements();i++) {
	if (tableNames(i).length() > 0) {
	    itsTableNames(nameCount++) = tableNames(i);
	}
    }
    itsTableNames.resize(nameCount, True);
}


void MSReader::gotoRow(uInt which) 
{
    // give up if this isn't a valid row.  Perhaps this should do something more
    // obnoxious, like make this a boolean fn and return False?
    if (which >= itsMS.nrow()) return;

    // don't do anything if which is the same as the previous call.
    // This will have problems is the MS has been written to in the meantime.
    if (itsRowNumbers[itsMainId] >= 0 && uInt(itsRowNumbers[itsMainId]) == which) return;

    itsRowNumbers = -1;

    itsTabRows[itsMainId].get(which);
    itsRowNumbers[itsMainId] = which;

    // simple indexes first
    Int ant1Id = itsIds.antenna1(which);
    if (ant1Id >= 0) {
	itsTabRows[itsAnt1Id].get(ant1Id);
	itsRowNumbers[itsAnt1Id] = ant1Id;
    }
    Int ant2Id = itsIds.antenna1(which);
    if (ant2Id >= 0) {
	itsTabRows[itsAnt2Id].get(ant2Id);
	itsRowNumbers[itsAnt2Id] = ant2Id;
    }
    Int ddId = itsIds.dataDescId(which);
    if (ddId >= 0) {
	itsTabRows[itsDDId].get(ddId);
	itsRowNumbers[itsDDId] = ddId;
    }
    Int obsId = itsIds.observationId(which);
    if (obsId >= 0) {
	itsTabRows[itsObsId].get(obsId);
	itsRowNumbers[itsObsId] = obsId;
    }
    Int polId = itsIds.polarizationId(which);
    if (polId >= 0) {
	itsTabRows[itsPolId].get(polId);
	itsRowNumbers[itsPolId] = polId;
    }
    Int spwId = itsIds.spectralWindowId(which);
    if (spwId >= 0) {
	itsTabRows[itsSpwId].get(spwId);
	itsRowNumbers[itsSpwId] = spwId;
    }
    Int fieldId = itsIds.fieldId(which);
    if (fieldId >= 0) {
	itsTabRows[itsFieldId].get(fieldId);
	itsRowNumbers[itsFieldId] = fieldId;
    }
    Int procId = itsIds.processorId(which);
    if (procId >= 0) {
	itsTabRows[itsProcId].get(procId);
	itsRowNumbers[itsProcId] = procId;
    }
    Int stateId = itsIds.stateId(which);
    if (stateId >= 0) {
	itsTabRows[itsStateId].get(stateId);
	itsRowNumbers[itsStateId] = stateId;
    }

    // and the ones with specific indexes
    // these all need the time and interval
    const MEpoch time = itsMSCols.timeMeas()(which);
    const Quantity interval = itsMSCols.intervalQuant()(which);
    Double stime = time.getValue().getTime().getValue(itsSecUnit);
    Double sint = interval.getValue(itsSecUnit);

    // DOPPLER - optional
    Bool found;
    if (itsDopplerId >= 0) {
	itsDopplerIndex.dopplerId() = itsIds.dopplerId(which);
	itsDopplerIndex.sourceId() = itsIds.sourceId(which);
	// doppler does not use time or interval as keys
	Int dopRow = itsDopplerIndex.getNearestRow(found);
	if (found) {
	    itsTabRows[itsDopplerId].get(dopRow);
	    itsRowNumbers[itsDopplerId] = dopRow;
	}
    }

    // FEED1, with ANTENNA1
    itsFeed1Index.antennaId() = ant1Id;
    Int feed1 = itsMSCols.feed1()(which);
    Int feed2 = itsMSCols.feed2()(which);
    itsFeed1Index.feedId() = feed1;
    itsFeed1Index.spectralWindowId() = spwId;
    itsFeed1Index.time() = stime;
    itsFeed1Index.interval() = sint;
    Int feedRow = itsFeed1Index.getNearestRow(found);
    if (found) {
	itsTabRows[itsFeed1Id].get(feedRow);
	itsRowNumbers[itsFeed1Id] = feedRow;
    }
    // can this just be reused
    if (ant1Id != ant2Id || feed1 != feed2) {
	itsFeed2Index.antennaId() = ant2Id;
	itsFeed2Index.feedId() = feed2;
	itsFeed2Index.spectralWindowId() = spwId;
	itsFeed2Index.time() = stime;
	itsFeed2Index.interval() = sint;
	feedRow = itsFeed2Index.getNearestRow(found);
    }
    if (found) {
	itsTabRows[itsFeed2Id].get(feedRow);
	itsRowNumbers[itsFeed2Id] = feedRow;
    }

    // FLAG_CMD - handled generically in the itsIndexes block
    
    // FREQ_OFFSET - optional
    if (itsFreqOffsetId >= 0) {
	itsFreqOffIndex.antenna1Id() = ant1Id;
	itsFreqOffIndex.antenna2Id() = ant2Id;
	// I don't really understand why there is only one FEED_ID here
	itsFreqOffIndex.feedId() = feed1;
	itsFreqOffIndex.time() = stime;
	itsFreqOffIndex.interval() = sint;
	Int foffRow = itsFreqOffIndex.getNearestRow(found);
	if (found) {
	    itsTabRows[itsFreqOffsetId].get(foffRow);
	    itsRowNumbers[itsFreqOffsetId] = foffRow;
	}
    }

    // POINTING1 - required, this depends on ANTENNA1
    itsPointing1Index.antennaId() = ant1Id;
    itsPointing1Index.time() = stime;
    itsPointing1Index.interval() = sint;
    Int pointRow = itsPointing1Index.getNearestRow(found);
    if (found) {
	itsTabRows[itsPointing1Id].get(pointRow);
	itsRowNumbers[itsPointing1Id] = pointRow;
    }
    // can we reuse this for POINTING2?
    if (ant1Id != ant2Id) {
	itsPointing2Index.antennaId() = ant2Id;
	itsPointing2Index.time() = stime;
	itsPointing2Index.interval() = sint;
	pointRow = itsPointing2Index.getNearestRow(found);
    }
    if (found) {
	itsTabRows[itsPointing2Id].get(pointRow);
	itsRowNumbers[itsPointing2Id] = pointRow;
    }

    // SOURCE - optional
    if (itsSourceId >= 0) {
	itsSourceIndex.sourceId() = itsIds.sourceId(which);
	itsSourceIndex.spectralWindowId() = itsIds.spectralWindowId(which);
	itsSourceIndex.time() = stime;
	itsSourceIndex.interval() = sint;
	Int sourceRow = itsSourceIndex.getNearestRow(found);
	if (found) {
	    itsTabRows[itsSourceId].get(sourceRow);
	    itsRowNumbers[itsSourceId] = sourceRow;
	}
    }

    // SYSCAL1 - optional
    if (itsSyscal1Id >= 0) {
	itsSyscal1Index.antennaId() = ant1Id;
	itsSyscal1Index.feedId() = feed1;
	itsSyscal1Index.spectralWindowId() = spwId;
	itsSyscal1Index.time() = stime;
	itsSyscal1Index.interval() = sint;
	Int syscalRow = itsSyscal1Index.getNearestRow(found);
	if (found) {
	    itsTabRows[itsSyscal1Id].get(syscalRow);
	    itsRowNumbers[itsSyscal1Id] = syscalRow;
	}

	// if SYSCAL1 exists, SYSCAL2 must exist and SYSCAL2 can't
	// exist unless SYSCAL1 exists
	// Can we re-use that row
	if (ant1Id != ant2Id || feed1 != feed2) {
	    itsSyscal2Index.antennaId() = ant2Id;
	    itsSyscal2Index.feedId() = feed2;
	    itsSyscal2Index.spectralWindowId() = spwId;
	    itsSyscal2Index.time() = stime;
	    itsSyscal2Index.interval() = sint;
	    syscalRow = itsSyscal2Index.getNearestRow(found);
	}
	if (found) {
	    itsTabRows[itsSyscal2Id].get(syscalRow);
	    itsRowNumbers[itsSyscal2Id] = syscalRow;
	}
    }

    // WEATHER1- optional
    if (itsWeather1Id >= 0) {
	itsWeather1Index.antennaId() = ant1Id;
	itsWeather1Index.time() = stime;
	itsWeather1Index.interval() = sint;
	Int weatherRow = itsWeather1Index.getNearestRow(found);
	if (found) {
	    itsTabRows[itsWeather1Id].get(weatherRow);
	    itsRowNumbers[itsWeather1Id] = weatherRow;
	}

	// if WEATHER1 exists, WEATHER2 must exist and WEATHER2 can't
	// exist unless WEATHER1 exists
	// Can we re-use that row
	if (ant1Id != ant2Id) {
	    itsWeather2Index.antennaId() = ant2Id;
	    itsWeather2Index.time() = stime;
	    itsWeather2Index.interval() = sint;
	    weatherRow = itsWeather2Index.getNearestRow(found);
	}
	if (found) {
	    itsTabRows[itsWeather2Id].get(weatherRow);
	    itsRowNumbers[itsWeather2Id] = weatherRow;
	}
    }

    // any think in itsIndexes
    for (uInt i=0;i<itsIndexes.nelements();i++) {
	if (!itsIndexes[i].isNull()) {
	    itsIndexes[i].time() = stime;
	    itsIndexes[i].interval() = sint;
	    Int thisRow = itsIndexes[i].getNearestRow(found);
	    if (found) {
		itsTabRows[i].get(thisRow);
		itsRowNumbers[i] = thisRow;
	    }
	}
    }
}

const RecordInterface &MSReader::tableRow(const String &name) const
{
    if (!itsTabId.isDefined(name)) return emptyRecord;
    Int tabId = itsTabId(name);
    return itsTabRows[tabId].record();
}

Int MSReader::rowNumber(const String &name) const
{
    if (!itsTabId.isDefined(name)) return -1;
    Int tabId = itsTabId(name);
    return itsRowNumbers[tabId];
}

// Return a reference to the named subtable
const Table &MSReader::table(const String &name) const
{
    if (!itsTabId.isDefined(name)) return emptyTable;
    Int tabId = itsTabId(name);
    return itsTabRows[tabId].table();
}

} //# NAMESPACE CASACORE - END

