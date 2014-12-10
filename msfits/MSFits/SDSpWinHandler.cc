//# SDSpWindowFiller.cc: an SPECTRAL_WINDOW filler for SDFITS data  
//# Copyright (C) 2000,2001,2002
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
#include <casacore/msfits/MSFits/SDSpWinHandler.h>

#include <casacore/tables/Tables/ColumnsIndex.h>
#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/ms/MeasurementSets/MSSpWindowColumns.h>
#include <casacore/ms/MeasurementSets/MSSpectralWindow.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/measures/Measures/MPosition.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/Slice.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/TableLock.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Arrays/ArrayLogical.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

SDSpWindowHandler::SDSpWindowHandler()
    : fNCachePtr_p(0), f0CachePtr_p(0), bwCachePtr_p(0), index_p(0), theCache_p(0),
      msSpWin_p(0), msSpWinCols_p(0), nextCacheRow_p(0), cacheSize_p(1000), rownr_p(-1), 
      bandwidField_p(-1), freqresField_p(-1)
{;}

SDSpWindowHandler::SDSpWindowHandler(MeasurementSet &ms, Vector<Bool> &handledCols,
				     const Record &row) 
    : fNCachePtr_p(0), f0CachePtr_p(0), bwCachePtr_p(0), index_p(0), theCache_p(0),
      msSpWin_p(0), msSpWinCols_p(0), nextCacheRow_p(0), cacheSize_p(1000), rownr_p(-1), 
      bandwidField_p(-1), freqresField_p(-1)
{
    initAll(ms, handledCols, row);
}

SDSpWindowHandler::SDSpWindowHandler(const SDSpWindowHandler &other) 
     : fNCachePtr_p(0), f0CachePtr_p(0), bwCachePtr_p(0), index_p(0), theCache_p(0),
      msSpWin_p(0), msSpWinCols_p(0), nextCacheRow_p(0), cacheSize_p(1000), rownr_p(-1), 
      bandwidField_p(-1), freqresField_p(-1)
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
	fNCache_p.resize(other.fNCache_p.nelements());
	fNCache_p = other.fNCache_p;
	fNCachePtr_p = fNCache_p.getStorage(deleteItFN_p);
	f0Cache_p.resize(other.f0Cache_p.nelements());
	f0Cache_p = other.f0Cache_p;
	f0CachePtr_p = f0Cache_p.getStorage(deleteItF0_p);
	bwCache_p.resize(other.bwCache_p.nelements());
	bwCache_p = other.bwCache_p;
	bwCachePtr_p = bwCache_p.getStorage(deleteItBw_p);
	// need to avoid the assignment operator here because we want
	// this to point to the field in index_p, not in other.index_p
	nchanKey_p.attachToRecord(index_p->accessKey(), "NCHAN");
	freqRefTypeKey_p.attachToRecord(index_p->accessKey(), "FREQREFTYPE");
	ifConvChainKey_p.attachToRecord(index_p->accessKey(), "IF_CONV_CHAIN");
	freqGroupKey_p.attachToRecord(index_p->accessKey(), "FREQ_GROUP");
	netSidebandKey_p.attachToRecord(index_p->accessKey(), "NET_SIDEBAND");
	flagRowKey_p.attachToRecord(index_p->accessKey(), "FLAG_ROW");

	// reattach the table pointers as well
	idCol_p.attach(*theCache_p, "ID");
	nchanCol_p.attach(*theCache_p, "NCHAN");
	freqRefTypeCol_p.attach(*theCache_p, "FREQREFTYPE");
	ifConvChainCol_p.attach(*theCache_p, "IF_CONV_CHAIN");
	freqGroupCol_p.attach(*theCache_p, "FREQ_GROUP");
	netSidebandCol_p.attach(*theCache_p, "NET_SIDEBAND");
	flagRowCol_p.attach(*theCache_p, "FLAG_ROW");

	msSpWin_p = new MSSpectralWindow(*(other.msSpWin_p));
	AlwaysAssert(msSpWin_p, AipsError);
	msSpWinCols_p = new MSSpWindowColumns(*msSpWin_p);
	AlwaysAssert(msSpWinCols_p, AipsError);

	nextCacheRow_p = other.nextCacheRow_p;
	cacheSize_p = other.cacheSize_p;

	rownr_p = other.rownr_p;

	spWinIdField_p = other.spWinIdField_p;
	ifConvChainField_p = other.ifConvChainField_p;
	freqGroupField_p = other.freqGroupField_p;
	netSidebandField_p = other.netSidebandField_p;
	flagRowField_p = other.flagRowField_p;
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
			     Double refFrequency, Double originalFreqDelt,
			     Int freqRefType)
{
    // don't bother unless there is something there
    if (msSpWin_p) {
	// this is arbitrary  we have match if things are within this fraction of a channel
	Double chanTol = 0.001;
	*nchanKey_p = frequency.nelements();
	*freqRefTypeKey_p = freqRefType;
	Double thisFN, thisF0, thisBW;
	thisFN = thisF0 = thisBW = 0.0;
	if (bandwidField_p >= 0) {
	    thisBW = row.asDouble(bandwidField_p);
	} 
	if (*nchanKey_p > 0) {
	    thisF0 = frequency(0);
	    if (*nchanKey_p > 1) {
		thisFN = frequency(*nchanKey_p-1);
		if (thisBW == 0.0) thisBW = abs(thisFN - thisF0);
	    }
	}
	Double thisFreqRes = 0.0;
	if (freqresField_p >= 0) {
	    thisFreqRes = row.asDouble(freqresField_p);
	} 
	if (ifConvChainField_p.isAttached()) {
	    *ifConvChainKey_p = *ifConvChainField_p;
	} else {
	    *ifConvChainKey_p = -1;
	}
	if (freqGroupField_p.isAttached()) {
	    *freqGroupKey_p = *freqGroupField_p;
	} else {
	    *freqGroupKey_p = -1;
	}
	if (netSidebandField_p.isAttached()) {
	    *netSidebandKey_p = *netSidebandField_p;
	} else {
	    *netSidebandKey_p = -1;
	}
	Bool found = False;
	// find any potential matches
	Vector<uInt> cacheRows = index_p->getRowNumbers();
	if (cacheRows.nelements()>0) {
	    // do the fN, f0, and bw also match
	    const uInt *rowPtr;
	    Bool deleteItRows;
	    rowPtr = cacheRows.getStorage(deleteItRows);
	    uInt i = 0;
	    while (i<cacheRows.nelements() && !found) {
		uInt rownr = rowPtr[i];
		found = (abs((bwCachePtr_p[i]-thisBW)/originalFreqDelt) < chanTol);
		found = found && (abs((f0CachePtr_p[i]-thisF0)/originalFreqDelt) < chanTol);
		found = found && (abs((fNCachePtr_p[i]-thisFN)/originalFreqDelt) < chanTol);
		if (found) {
		    rownr_p = Int(rownr);
		}
		i++;
	    }
	}
	if (!found) {
 	    // not found in the cache, may try to look for it in a specific place if this
	    // originally came from a MS, otherwise we'll just add it in.
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
		freqres = thisFreqRes;
	    } else {
		// just reuse the computed channel widths
		freqres = chWidth;
	    }
	    // one last try, if this is from a MS, try the indicated row in the main table
	    if (spWinIdField_p.isAttached() && *spWinIdField_p >= 0 && uInt(*spWinIdField_p) < msSpWin_p->nrow()) {
		Int rownr = *spWinIdField_p;
		found = msSpWinCols_p->numChan()(rownr) == nchan;
		found = found && msSpWinCols_p->refFrequency()(rownr) == refFrequency;
		// for SDFITS, these test should be sufficient - i.e. only necessary to look
		// around the first and last channels, not all of them
		if (found) {
		    IPosition beg(msSpWinCols_p->chanFreq()(rownr).shape()), end;
		    end = beg-1;
		    beg = 0;
		    if (nchan > 1) {
			Double shift = abs((msSpWinCols_p->chanFreq()(rownr)(beg)-thisF0)/chWidth(0));
			if (nchan > 2) {
			    shift = max(shift,
					abs((msSpWinCols_p->chanFreq()(rownr)(end)-thisFN)/chWidth(nchan-1)));
			}
			found = shift < chanTol;
		    } else if (nchan == 1) {
			found = near(msSpWinCols_p->chanFreq()(rownr)(beg),thisF0);
		    }
		}
		// if it passed the frequency test, there should be no need to check the channel
		// widths since they have the same nchan and the same first an last channel value
		// the widths should be the same
		found = found && msSpWinCols_p->measFreqRef()(rownr) == freqRefType;
		found = found && near(msSpWinCols_p->totalBandwidth()(rownr), thisBW);
		found = found && msSpWinCols_p->ifConvChain()(rownr) == *ifConvChainKey_p;
		found = found && msSpWinCols_p->freqGroup()(rownr) == *freqGroupKey_p;
		found = found && msSpWinCols_p->netSideband()(rownr) == *netSidebandKey_p;
		found = found && msSpWinCols_p->flagRow()(rownr) == *flagRowKey_p;
		if (found) rownr_p = rownr;
	    }
	    if (!found) {
		// we need to add one 
		rownr_p = msSpWin_p->nrow();
		msSpWin_p->addRow();
		msSpWinCols_p->numChan().put(rownr_p, nchan);
		msSpWinCols_p->name().put(rownr_p, "");
		msSpWinCols_p->refFrequency().put(rownr_p, refFrequency);
		msSpWinCols_p->chanFreq().put(rownr_p, frequency);
		msSpWinCols_p->chanWidth().put(rownr_p, chWidth);
		msSpWinCols_p->measFreqRef().put(rownr_p, freqRefType);
		msSpWinCols_p->effectiveBW().put(rownr_p, chWidth);
		msSpWinCols_p->resolution().put(rownr_p, freqres);
		msSpWinCols_p->totalBandwidth().put(rownr_p, thisBW);
		msSpWinCols_p->netSideband().put(rownr_p, *netSidebandKey_p);
		msSpWinCols_p->ifConvChain().put(rownr_p, *ifConvChainKey_p);
		msSpWinCols_p->freqGroup().put(rownr_p, *freqGroupKey_p);
		msSpWinCols_p->freqGroupName().put(rownr_p,"");
		msSpWinCols_p->flagRow().put(rownr_p, *flagRowKey_p);
	    }

	    // either way, update the cache
	    if (theCache_p->nrow() < cacheSize_p) theCache_p->addRow();
	    if (nextCacheRow_p >= cacheSize_p) nextCacheRow_p = 0;
	    idCol_p.putScalar(nextCacheRow_p, spWindowId());
	    nchanCol_p.putScalar(nextCacheRow_p, nchan);
	    freqRefTypeCol_p.putScalar(nextCacheRow_p, freqRefType);
	    ifConvChainCol_p.putScalar(nextCacheRow_p, *ifConvChainKey_p);
	    freqGroupCol_p.putScalar(nextCacheRow_p, *freqGroupKey_p);
	    netSidebandCol_p.putScalar(nextCacheRow_p, *netSidebandKey_p);
	    flagRowCol_p.putScalar(nextCacheRow_p, *flagRowKey_p);

	    bwCachePtr_p[nextCacheRow_p] = thisBW;
	    f0CachePtr_p[nextCacheRow_p] = thisF0;
	    fNCachePtr_p[nextCacheRow_p] = thisFN;

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

    fNCache_p.putStorage(fNCachePtr_p, deleteItFN_p);
    f0Cache_p.putStorage(f0CachePtr_p, deleteItF0_p);
    bwCache_p.putStorage(bwCachePtr_p, deleteItBw_p);

    fNCachePtr_p = f0CachePtr_p = bwCachePtr_p = 0;

    delete msSpWin_p;
    msSpWin_p = 0;

    delete msSpWinCols_p;
    msSpWinCols_p = 0;

    nextCacheRow_p = 0;
    
    clearRow();
}

void SDSpWindowHandler::clearRow()
{
    bandwidField_p = freqresField_p = -1;
    spWinIdField_p.detach();
    ifConvChainField_p.detach();
    freqGroupField_p.detach();
    netSidebandField_p.detach();
    flagRowField_p.detach();
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
    td.addColumn(ScalarColumnDesc<Int>("IF_CONV_CHAIN"));
    td.addColumn(ScalarColumnDesc<Int>("FREQ_GROUP"));
    td.addColumn(ScalarColumnDesc<Int>("NET_SIDEBAND"));
    td.addColumn(ScalarColumnDesc<Bool>("FLAG_ROW"));
    SetupNewTable newTab("",td,Table::Scratch);
    theCache_p = new Table(newTab, TableLock::PermanentLocking);
    AlwaysAssert(theCache_p, AipsError);

    // and attach the columns
    idCol_p.attach(*theCache_p, "ID");
    nchanCol_p.attach(*theCache_p, "NCHAN");
    freqRefTypeCol_p.attach(*theCache_p, "FREQREFTYPE");
    ifConvChainCol_p.attach(*theCache_p, "IF_CONV_CHAIN");
    freqGroupCol_p.attach(*theCache_p, "FREQ_GROUP");
    netSidebandCol_p.attach(*theCache_p, "NET_SIDEBAND");
    flagRowCol_p.attach(*theCache_p, "FLAG_ROW");

    // and the floating point things we cache on the side
    fNCache_p.resize(cacheSize_p);
    f0Cache_p.resize(cacheSize_p);
    bwCache_p.resize(cacheSize_p);
    fNCachePtr_p = fNCache_p.getStorage(deleteItFN_p);
    f0CachePtr_p = f0Cache_p.getStorage(deleteItF0_p);
    bwCachePtr_p = bwCache_p.getStorage(deleteItBw_p);

    // create the index
    index_p = new ColumnsIndex(*theCache_p,
			       stringToVector("NCHAN,FREQREFTYPE,IF_CONV_CHAIN,FREQ_GROUP,NET_SIDEBAND,FLAG_ROW"));
    AlwaysAssert(index_p, AipsError);
    // and attach the key fields
    nchanKey_p.attachToRecord(index_p->accessKey(), "NCHAN");
    freqRefTypeKey_p.attachToRecord(index_p->accessKey(), "FREQREFTYPE");
    ifConvChainKey_p.attachToRecord(index_p->accessKey(), "IF_CONV_CHAIN");
    freqGroupKey_p.attachToRecord(index_p->accessKey(), "FREQ_GROUP");
    netSidebandKey_p.attachToRecord(index_p->accessKey(), "NET_SIDEBAND");
    flagRowKey_p.attachToRecord(index_p->accessKey(), "FLAG_ROW");

    nextCacheRow_p = 0;

    initRow(handledCols, row);
}

void SDSpWindowHandler::initRow(Vector<Bool> &handledCols, const Record &row)
{
    AlwaysAssert(handledCols.nelements()==row.description().nfields(), AipsError);

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

    if (row.fieldNumber("SPECTRAL_WINDOW_FREQ_GROUP") >= 0 && row.dataType("SPECTRAL_WINDOW_FREQ_GROUP") == TpInt) {
	freqGroupField_p.attachToRecord(row,"SPECTRAL_WINDOW_FREQ_GROUP");
	handledCols(row.fieldNumber("SPECTRAL_WINDOW_FREQ_GROUP")) = True;
    }

    if (row.fieldNumber("SPECTRAL_WINDOW_NET_SIDEBAND") >= 0 && row.dataType("SPECTRAL_WINDOW_NET_SIDEBAND") == TpInt) {
	netSidebandField_p.attachToRecord(row,"SPECTRAL_WINDOW_NET_SIDEBAND");
	handledCols(row.fieldNumber("SPECTRAL_WINDOW_NET_SIDEBAND")) = True;
    }

    if (row.fieldNumber("SPECTRAL_WINDOW_FLAG_ROW") >= 0 && row.dataType("SPECTRAL_WINDOW_FLAG_ROW") == TpBool) {
	flagRowField_p.attachToRecord(row,"SPECTRAL_WINDOW_FLAG_ROW");
	handledCols(row.fieldNumber("SPECTRAL_WINDOW_FLAG_ROW")) = True;
    }

    // ignore this field, produced by ms2sdfits for MS version 1, it doesn't carry any additional information
    if (row.fieldNumber("SPECTRAL_WINDOW_NUM_CHAN") >= 0) 
	handledCols(row.fieldNumber("SPECTRAL_WINDOW_NUM_CHAN")) = True;

    // row number isn't set until the following fill
    rownr_p = -1;
}

} //# NAMESPACE CASACORE - END

