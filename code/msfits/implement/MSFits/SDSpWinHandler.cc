//# SDSpWindowFiller.cc: an SPECTRAL_WINDOW filler for SDFITS data  
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
#include <trial/MeasurementSets/SDSpWinHandler.h>

#include <aips/Tables/ColumnsIndex.h>
#include <aips/MeasurementSets/MeasurementSet.h>
#include <aips/MeasurementSets/MSSpWindowColumns.h>
#include <aips/MeasurementSets/MSSpectralWindow.h>
#include <aips/Containers/Record.h>
#include <aips/Tables/Table.h>
#include <aips/Arrays/Vector.h>
#include <aips/Measures/MPosition.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>
#include <aips/Utilities/String.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/Slice.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/ScaColDesc.h>
#include <aips/Tables/SetupNewTab.h>
#include <aips/Tables/TableLock.h>
#include <aips/Arrays/ArrayUtil.h>
#include <aips/Arrays/ArrayLogical.h>

SDSpWindowHandler::SDSpWindowHandler()
    : index_p(0), theCache_p(0), msSpWin_p(0), msSpWinCols_p(0), nextCacheRow_p(0),
      cacheSize_p(1000), rownr_p(-1), obsfreqField_p(-1), bandwidField_p(-1),
      freqresField_p(-1)
{;}

SDSpWindowHandler::SDSpWindowHandler(MeasurementSet &ms, Vector<Bool> &handledCols,
				     const Record &row) 
    : index_p(0), theCache_p(0), msSpWin_p(0), msSpWinCols_p(0), nextCacheRow_p(0),
      cacheSize_p(1000), rownr_p(-1), obsfreqField_p(-1), bandwidField_p(-1),
      freqresField_p(-1)
{
    initAll(ms, handledCols, row);
}

SDSpWindowHandler::SDSpWindowHandler(const SDSpWindowHandler &other) 
    : index_p(0), theCache_p(0), msSpWin_p(0), msSpWinCols_p(0), nextCacheRow_p(0),
      cacheSize_p(1000), rownr_p(-1), obsfreqField_p(-1), bandwidField_p(-1),
      freqresField_p(-1)
{
    *this = other;
}

SDSpWindowHandler &SDSpWindowHandler::operator=(const SDSpWindowHandler &other)
{
    if (this != &other) {
	clearAll();
	index_p = new ColumnsIndex(*(other.index_p));
	AlwaysAssert(index_p, AipsError);
	theCache_p = new Table(*(other.theCache_p));
	AlwaysAssert(theCache_p, AipsError);
	// need to avoid the assignment operator here because we want
	// this to point to the field in index_p, not in other.index_p
	nchanKey_p.attachToRecord(index_p->accessKey(), "NCHAN");
	freqRefTypeKey_p.attachToRecord(index_p->accessKey(), "FREQREFTYPE");
	ifConvChainKey_p.attachToRecord(index_p->accessKey(), "IF_CONV_CHAIN");
	refFreqKey_p.attachToRecord(index_p->accessKey(), "REFFREQ");
	bwKey_p.attachToRecord(index_p->accessKey(), "BW");
	f0Key_p.attachToRecord(index_p->accessKey(), "F0");
	fdeltKey_p.attachToRecord(index_p->accessKey(), "FDELT");
	freqresKey_p.attachToRecord(index_p->accessKey(), "FREQRES");

	// reattach the table pointers as well
	idCol_p.attach(*theCache_p, "ID");
	nchanCol_p.attach(*theCache_p, "NCHAN");
	freqRefTypeCol_p.attach(*theCache_p, "FREQREFTYPE");
	ifConvChainCol_p.attach(*theCache_p, "IF_CONV_CHAIN");
	refFreqCol_p.attach(*theCache_p, "REFFREQ");
	bwCol_p.attach(*theCache_p, "BW");
	f0Col_p.attach(*theCache_p, "F0");
	fdeltCol_p.attach(*theCache_p, "FDELT");
	freqresCol_p.attach(*theCache_p, "FREQRES");

	msSpWin_p = new MSSpectralWindow(*(other.msSpWin_p));
	AlwaysAssert(msSpWin_p, AipsError);
	msSpWinCols_p = new MSSpWindowColumns(*msSpWin_p);
	AlwaysAssert(msSpWinCols_p, AipsError);

	nextCacheRow_p = other.nextCacheRow_p;
	cacheSize_p = other.cacheSize_p;

	rownr_p = other.rownr_p;

	obsfreqField_p = other.obsfreqField_p;
	bandwidField_p = other.bandwidField_p;
	freqresField_p = other.freqresField_p;

	spWinIdField_p = other.spWinIdField_p;
	ifConvChainField_p = other.ifConvChainField_p;
    }
    return *this;
}

void SDSpWindowHandler::attach(MeasurementSet &ms, Vector<Bool> &handledCols, const Record &row)
{
    clearAll();
    initAll(ms, handledCols, row);
}

void SDSpWindowHandler::resetRow(const Record &row) 
{
    clearRow();
    Vector<Bool> dummyHandled;
    initRow(dummyHandled, row);
}

void SDSpWindowHandler::fill(const Record &row, const Vector<Double> &frequency,
			     Double originalFreqAtPix0, Double originalFreqDelt,
			     Int freqRefType)
{
    // don't bother unless there is something there
    if (msSpWin_p) {
	*nchanKey_p = frequency.nelements();
	*freqRefTypeKey_p = freqRefType;
	if (obsfreqField_p >= 0) {
	    // assumes units are Hz (fix that later)
	    *refFreqKey_p = row.asDouble(obsfreqField_p);
	} else {
	    *refFreqKey_p = 0.0;
	}
	if (bandwidField_p >= 0) {
	    *bwKey_p = row.asDouble(bandwidField_p);
	} else {
	    *bwKey_p = 0.0;
	}
	if (freqresField_p >= 0) {
	    *freqresKey_p = row.asDouble(freqresField_p);
	} else {
	    *freqresKey_p = 0.0;
	}
	if (ifConvChainField_p.isAttached()) {
	    *ifConvChainKey_p = *ifConvChainField_p;
	} else {
	    *ifConvChainKey_p = -1;
	}
	*f0Key_p = originalFreqAtPix0;
	*fdeltKey_p = originalFreqDelt;
	Bool found;
	// there can only be one match if this exists in the cache
	uInt cacheRow = index_p->getRowNumber(found);
	if (found) {
	    rownr_p = idCol_p.asInt(cacheRow);
	} else {
	    // either way we need to calculate the widths and resolution here
	    Int nchan = *nchanKey_p;
	    Vector<Double> chWidth(nchan);
	    if (nchan > 2) {
		chWidth = frequency;
		chWidth(Slice(1,(nchan-2))) =
		    0.5 * (chWidth(Slice(2,(nchan-2))) - chWidth(Slice(0,(nchan-2))));
	    } 
	    if (nchan > 1) {
		chWidth(nchan-1) = frequency(nchan-1)-frequency(nchan-2);
	    }
	    if (nchan > 0) {
		chWidth(0) = frequency(1) - frequency(0);
	    }
	    chWidth = abs(chWidth);
	    Vector<Double> freqres(nchan);
	    if (freqresField_p >= 0) {
		freqres = *freqresKey_p;
	    } else {
		// just reuse the computed channel widths
		freqres = chWidth;
	    }
	    // one last try, if this is from a MS, try the indicated row in the main table
	    if (spWinIdField_p.isAttached() && *spWinIdField_p >= 0 && uInt(*spWinIdField_p) < msSpWin_p->nrow()) {
		Int rownr = *spWinIdField_p;
		found = msSpWinCols_p->numChan()(rownr) == nchan;
		found = found && msSpWinCols_p->refFrequency()(rownr) == *refFreqKey_p;
		found = found && allEQ(msSpWinCols_p->chanFreq()(rownr),frequency);
		found = found && allEQ(msSpWinCols_p->chanWidth()(rownr), chWidth);
		found = found && msSpWinCols_p->measFreqRef()(rownr) == freqRefType;
		found = found && allEQ(msSpWinCols_p->resolution()(rownr), freqres);
		found = found && msSpWinCols_p->totalBandwidth()(rownr) ==  *bwKey_p;
		found = found && msSpWinCols_p->ifConvChain()(rownr) == *ifConvChainKey_p;
		if (found) rownr_p = rownr;
	    }
	    if (!found) {
		// we need to add one 
		rownr_p = msSpWin_p->nrow();
		msSpWin_p->addRow();
		msSpWinCols_p->numChan().put(rownr_p, nchan);
		msSpWinCols_p->name().put(rownr_p, "");
		msSpWinCols_p->refFrequency().put(rownr_p, *refFreqKey_p);
		msSpWinCols_p->chanFreq().put(rownr_p, frequency);
		msSpWinCols_p->chanWidth().put(rownr_p, chWidth);
		msSpWinCols_p->measFreqRef().put(rownr_p, freqRefType);
		msSpWinCols_p->effectiveBW().put(rownr_p, chWidth);
		msSpWinCols_p->resolution().put(rownr_p, freqres);
		msSpWinCols_p->totalBandwidth().put(rownr_p, *bwKey_p);
		msSpWinCols_p->netSideband().put(rownr_p, -1);
		msSpWinCols_p->ifConvChain().put(rownr_p, *ifConvChainKey_p);
		msSpWinCols_p->freqGroup().put(rownr_p, -1);
		msSpWinCols_p->freqGroupName().put(rownr_p,"");
		msSpWinCols_p->flagRow().put(rownr_p, False);
	    }

	    // either way, update the cache
	    if (theCache_p->nrow() < cacheSize_p) theCache_p->addRow();
	    if (nextCacheRow_p >= cacheSize_p) nextCacheRow_p = 0;
	    idCol_p.putScalar(nextCacheRow_p, spWindowId());
	    nchanCol_p.putScalar(nextCacheRow_p, nchan);
	    freqRefTypeCol_p.putScalar(nextCacheRow_p, freqRefType);
	    refFreqCol_p.putScalar(nextCacheRow_p, *refFreqKey_p);
	    bwCol_p.putScalar(nextCacheRow_p, *bwKey_p);
	    f0Col_p.putScalar(nextCacheRow_p, *f0Key_p);
	    fdeltCol_p.putScalar(nextCacheRow_p, *fdeltKey_p);
	    freqresCol_p.putScalar(nextCacheRow_p, *freqresKey_p);
	    ifConvChainCol_p.putScalar(nextCacheRow_p, *ifConvChainKey_p);

	    nextCacheRow_p++;
	}
    }
}

void SDSpWindowHandler::clearAll()
{
    delete index_p;
    index_p = 0;

    delete theCache_p;
    theCache_p = 0;

    delete msSpWin_p;
    msSpWin_p = 0;

    delete msSpWinCols_p;
    msSpWinCols_p = 0;

    nextCacheRow_p = 0;
    
    clearRow();
}

void SDSpWindowHandler::clearRow()
{
    obsfreqField_p = bandwidField_p = freqresField_p = -1;
    spWinIdField_p.detach();
    ifConvChainField_p.detach();
    rownr_p = -1;
}

void SDSpWindowHandler::initAll(MeasurementSet &ms, Vector<Bool> &handledCols, 
				const Record &row)
{
    msSpWin_p = new MSSpectralWindow(ms.spectralWindow());
    AlwaysAssert(msSpWin_p, AipsError);

    msSpWinCols_p = new MSSpWindowColumns(*msSpWin_p);
    AlwaysAssert(msSpWinCols_p, AipsError);

    // construct a cache table with zero rows
    TableDesc td;
    td.addColumn(ScalarColumnDesc<Int>("ID"));
    td.addColumn(ScalarColumnDesc<Int>("NCHAN"));
    td.addColumn(ScalarColumnDesc<Int>("FREQREFTYPE"));
    td.addColumn(ScalarColumnDesc<Double>("REFFREQ"));
    td.addColumn(ScalarColumnDesc<Double>("BW"));
    td.addColumn(ScalarColumnDesc<Double>("F0"));
    td.addColumn(ScalarColumnDesc<Double>("FDELT"));
    td.addColumn(ScalarColumnDesc<Double>("FREQRES"));
    td.addColumn(ScalarColumnDesc<Int>("IF_CONV_CHAIN"));
    SetupNewTable newTab("",td,Table::Scratch);
    theCache_p = new Table(newTab, TableLock::PermanentLocking);
    AlwaysAssert(theCache_p, AipsError);

    // and attach the columns
    idCol_p.attach(*theCache_p, "ID");
    nchanCol_p.attach(*theCache_p, "NCHAN");
    freqRefTypeCol_p.attach(*theCache_p, "FREQREFTYPE");
    refFreqCol_p.attach(*theCache_p, "REFFREQ");
    bwCol_p.attach(*theCache_p, "BW");
    f0Col_p.attach(*theCache_p, "F0");
    fdeltCol_p.attach(*theCache_p, "FDELT");
    freqresCol_p.attach(*theCache_p, "FREQRES");
    ifConvChainCol_p.attach(*theCache_p, "IF_CONV_CHAIN");

    // create the index
    index_p = new ColumnsIndex(*theCache_p,
			       stringToVector("NCHAN,FREQREFTYPE,REFFREQ,BW,F0,FDELT,FREQRES,IF_CONV_CHAIN"));
    AlwaysAssert(index_p, AipsError);
    // and attach the key fields
    nchanKey_p.attachToRecord(index_p->accessKey(), "NCHAN");
    freqRefTypeKey_p.attachToRecord(index_p->accessKey(), "FREQREFTYPE");
    refFreqKey_p.attachToRecord(index_p->accessKey(), "REFFREQ");
    bwKey_p.attachToRecord(index_p->accessKey(), "BW");
    f0Key_p.attachToRecord(index_p->accessKey(), "F0");
    fdeltKey_p.attachToRecord(index_p->accessKey(), "FDELT");
    freqresKey_p.attachToRecord(index_p->accessKey(), "FREQRES");
    ifConvChainKey_p.attachToRecord(index_p->accessKey(), "IF_CONV_CHAIN");

    nextCacheRow_p = 0;

    initRow(handledCols, row);
}

void SDSpWindowHandler::initRow(Vector<Bool> &handledCols, const Record &row)
{
    AlwaysAssert(handledCols.nelements()==row.description().nfields(), AipsError);

    obsfreqField_p = row.fieldNumber("OBSFREQ");
    if (obsfreqField_p >= 0) handledCols(obsfreqField_p) = True;

    bandwidField_p = row.fieldNumber("BANDWID");
    if (bandwidField_p < 0) {
	// allow for this alternative, older, spelling
	bandwidField_p = row.fieldNumber("BANDWIDT");
    }
    if (bandwidField_p >= 0) handledCols(bandwidField_p) = True;

    freqresField_p = row.fieldNumber("FREQRES");
    if (freqresField_p >= 0) handledCols(freqresField_p) = True;

    if (row.fieldNumber("MAIN_SPECTRAL_WINDOW_ID") >= 0 && row.dataType("MAIN_SPECTRAL_WINDOW_ID") == TpInt) {
	spWinIdField_p.attachToRecord(row,"MAIN_SPECTRAL_WINDOW_ID");
	handledCols(row.fieldNumber("MAIN_SPECTRAL_WINDOW_ID")) = True;
    }

    if (row.fieldNumber("SPECTRAL_WINDOW_IF_CONV_CHAIN") >= 0 && row.dataType("SPECTRAL_WINDOW_IF_CONV_CHAIN") == TpInt) {
	ifConvChainField_p.attachToRecord(row,"SPECTRAL_WINDOW_IF_CONV_CHAIN");
	handledCols(row.fieldNumber("SPECTRAL_WINDOW_IF_CONV_CHAIN")) = True;
    }

    // ignore this field, produced by ms2sdfits for MS version 1, it doesn't carry any additional information
    if (row.fieldNumber("SPECTRAL_WINDOW_NUM_CHAN") >= 0) 
	handledCols(row.fieldNumber("SPECTRAL_WINDOW_NUM_CHAN")) = True;

    // row number isn't set until the following fill
    rownr_p = -1;
}
