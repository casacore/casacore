//# SDSysCalHandler.cc: a SYSCAL handler for SDFITS data  
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
#include <trial/MeasurementSets/SDSysCalHandler.h>

#include <aips/MeasurementSets/NewMeasurementSet.h>
#include <aips/MeasurementSets/NewMSSysCalColumns.h>
#include <aips/MeasurementSets/NewMSSysCal.h>
#include <aips/Containers/Record.h>
#include <aips/Arrays/Vector.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>
#include <aips/Utilities/String.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Arrays/ArrayLogical.h>

SDSysCalHandler::SDSysCalHandler() 
    : msSysCal_p(0), msSysCalCols_p(0), rownr_p(-1), nrecpt_p(0),
      tcalId_p(-1), tsysId_p(-1), trxId_p(-1), hasTsysCol_p(False),
      hasTcalCol_p(False), hasTrxCol_p(False)
{;}

SDSysCalHandler::SDSysCalHandler(NewMeasurementSet &ms, Vector<Bool> &handledCols, 
				 const Record &row)
    : msSysCal_p(0), msSysCalCols_p(0), rownr_p(-1), nrecpt_p(0),
      tcalId_p(-1), tsysId_p(-1), trxId_p(-1), hasTsysCol_p(False),
      hasTcalCol_p(False), hasTrxCol_p(False)
{
    initAll(ms, handledCols, row);
}

SDSysCalHandler::SDSysCalHandler(const SDSysCalHandler &other) 
    : msSysCal_p(0), msSysCalCols_p(0), rownr_p(-1), nrecpt_p(0),
      tcalId_p(-1), tsysId_p(-1), trxId_p(-1), hasTsysCol_p(False),
      hasTcalCol_p(False), hasTrxCol_p(False)
{
    *this = other;
}

SDSysCalHandler &SDSysCalHandler::operator=(const SDSysCalHandler &other)
{
    if (this != &other) {
	clearAll();
	msSysCal_p = new NewMSSysCal(*(other.msSysCal_p));
	AlwaysAssert(msSysCal_p, AipsError);
	msSysCalCols_p = new NewMSSysCalColumns(*msSysCal_p);
	AlwaysAssert(msSysCalCols_p, AipsError);
	rownr_p = other.rownr_p;
	nrecpt_p = other.nrecpt_p;
	tcalId_p = other.tcalId_p;
	tsysId_p = other.tsysId_p;
	trxId_p = other.trxId_p;
	hasTsysCol_p = other.hasTsysCol_p;
	hasTcalCol_p = other.hasTcalCol_p;
	hasTrxCol_p = other.hasTrxCol_p;
	intervalField_p = other.intervalField_p;
	timeField_p = other.timeField_p;
	phaseDiffField_p = other.phaseDiffField_p;
	tcalFlagField_p = other.tcalFlagField_p;
	trxFlagField_p = other.trxFlagField_p;
	tsysFlagField_p = other.tsysFlagField_p;
	tcalField_p = other.tcalField_p;
	trxField_p = other.trxField_p;
	tsysField_p = other.tsysField_p;
    }
    return *this;
}

void SDSysCalHandler::attach(NewMeasurementSet &ms, Vector<Bool> &handledCols, 
			     const Record &row)
{
    clearAll();
    initAll(ms, handledCols, row);
}

void SDSysCalHandler::resetRow(const Record &row)
{
    clearRow();
    Vector<Bool> dummyHandledCols;
    initRow(dummyHandledCols, row);
}

void SDSysCalHandler::fill(const Record &row, Int antennaId, Int feedId, Int spectralWindowId,
			   Double time, Vector<Double> timeRange, uInt numReceptors)
{
    // don't bother unless there is something there
    if (msSysCal_p) {
	Vector<Float> tsys(numReceptors), tcal(numReceptors), trx(numReceptors);
	Bool tsysFlag, tcalFlag, trxFlag;
	tsysFlag = tcalFlag = trxFlag = False;
	tsys = tcal = trx = 0.0;
	// prefer the MS TSYS, TCAL, TRX since they have the correct dimensionality
	// but also fall back to SDFITS single values if Nr inferred from the MS is inconsistent
	if (tsysField_p.isAttached() && ((*tsysField_p).nelements() == numReceptors)) {
	    tsys = *tsysField_p;
	} else if (tsysId_p >= 0) {
	    tsys = row.asFloat(tsysId_p);
	}
	if (tsysFlagField_p.isAttached()) tsysFlag = *tsysFlagField_p;
	if (tcalField_p.isAttached() && ((*tcalField_p).nelements() == numReceptors)) {
	    tcal = *tcalField_p;
	} else if (tcalId_p >= 0) {
	    tcal = row.asFloat(tcalId_p);
	}
	if (tcalFlagField_p.isAttached()) tcalFlag = *tcalFlagField_p;
	if (trxField_p.isAttached() && ((*trxField_p).nelements() == numReceptors)) {
	    trx = *tcalField_p;
	} else if (trxId_p >= 0) {
	    trx = row.asFloat(trxId_p);
	}
	if (trxFlagField_p.isAttached()) trxFlag = *trxFlagField_p;
	Bool newRow = rownr_p < 0;
	newRow = newRow || numReceptors != nrecpt_p;
	if (!newRow && hasTsysCol_p) {
	    newRow = tsysFlag != msSysCalCols_p->tsysFlag()(rownr_p);
	    newRow = newRow || !allEQ(tsys, msSysCalCols_p->tsys()(rownr_p));
	}
	if (!newRow && hasTcalCol_p) {
	    newRow = tcalFlag != msSysCalCols_p->tcalFlag()(rownr_p);
	    newRow = newRow || !allEQ(tcal, msSysCalCols_p->tcal()(rownr_p));
	}
	if (!newRow && hasTrxCol_p) {
	    newRow = trxFlag != msSysCalCols_p->trxFlag()(rownr_p);
	    newRow = newRow || !allEQ(trx, msSysCalCols_p->trx()(rownr_p));
	}
	newRow = newRow || numReceptors != nrecpt_p;
	Double interval = timeRange(1) - timeRange(0);
	// former MS time or the time used in this function argument?
	Double thisTime = time;
	if (!newRow) {
	    if (timeField_p.isAttached()) {
		thisTime = *timeField_p;
		newRow = !near(msSysCalCols_p->time()(rownr_p), thisTime);
		if (!newRow && intervalField_p.isAttached()) {
		    interval = *intervalField_p;
		}
		newRow = !near(msSysCalCols_p->interval()(rownr_p), interval);
	    } else {
		Double rowTime = msSysCalCols_p->time()(rownr_p);
		Double maxTime = rowTime + msSysCalCols_p->interval()(rownr_p)/2.0;
		newRow = newRow || 
		    (!near(rowTime,thisTime) && (!timeRange(0) < maxTime || !near(timeRange(0), maxTime)));
	    }
	}
	newRow = newRow || msSysCalCols_p->antennaId()(rownr_p) != antennaId || 
	    msSysCalCols_p->feedId()(rownr_p) != feedId ||
	    msSysCalCols_p->spectralWindowId()(rownr_p) != spectralWindowId;
	if (!newRow && phaseDiffField_p.isAttached() && !near(*phaseDiffField_p, 0.0) && 
	    !isNaN(*phaseDiffField_p) && !isInf(*phaseDiffField_p)) {
	    // we seem to have a valid phase diff value
	    newRow = msSysCalCols_p->phaseDiff().isNull();
	    newRow = !newRow && msSysCalCols_p->phaseDiff()(rownr_p) != *phaseDiffField_p;
	    // is it flagged
	    // newRow != True here -> PHASE_DIFF col must exist -> PHASE_DIFF_FLAG must also exist
	    newRow = !newRow && phaseDiffFlagField_p.isAttached() &&
		*phaseDiffFlagField_p == msSysCalCols_p->phaseDiffFlag()(rownr_p);
	}
	if (newRow) {
	    // fill it
	    rownr_p = msSysCal_p->nrow();
	    msSysCal_p->addRow();
	    nrecpt_p = numReceptors;
	    msSysCalCols_p->antennaId().put(rownr_p,antennaId);
	    msSysCalCols_p->feedId().put(rownr_p,feedId);
	    msSysCalCols_p->spectralWindowId().put(rownr_p, spectralWindowId);
	    msSysCalCols_p->time().put(rownr_p, thisTime);
	    msSysCalCols_p->interval().put(rownr_p, interval);
	    if (hasTsysCol_p) {
		msSysCalCols_p->tsys().put(rownr_p, tsys);
		msSysCalCols_p->tsysFlag().put(rownr_p, tsysFlag);
	    }
	    if (hasTcalCol_p) {
		msSysCalCols_p->tcal().put(rownr_p, tcal);
		msSysCalCols_p->tcalFlag().put(rownr_p, tcalFlag);
	    }
	    if (hasTrxCol_p) {
		msSysCalCols_p->trx().put(rownr_p, trx);
		msSysCalCols_p->trxFlag().put(rownr_p, trxFlag);
	    }
	    if (phaseDiffField_p.isAttached()) {
		if (msSysCalCols_p->phaseDiff().isNull()) {
		    if (!near(*phaseDiffField_p, 0.0) && 
			!isNaN(*phaseDiffField_p) && !isInf(*phaseDiffField_p)) {
			// need to add this column
			delete msSysCalCols_p;
			msSysCalCols_p = 0;
			TableDesc td;
			NewMSSysCal::addColumnToDesc(td, NewMSSysCal::PHASE_DIFF);
			NewMSSysCal::addColumnToDesc(td, NewMSSysCal::PHASE_DIFF_FLAG);
			msSysCal_p->addColumn(td[0]);
			msSysCal_p->addColumn(td[1]);
			msSysCalCols_p = new NewMSSysCalColumns(*msSysCal_p);
			AlwaysAssert(msSysCalCols_p, AipsError);
			msSysCalCols_p->phaseDiff().put(rownr_p, *phaseDiffField_p);
			if (phaseDiffFlagField_p.isAttached()) {
			    msSysCalCols_p->phaseDiffFlag().put(rownr_p, *phaseDiffFlagField_p);
			} else {
			    msSysCalCols_p->phaseDiffFlag().put(rownr_p, False);
			}
		    } 
		} else {
		    msSysCalCols_p->phaseDiff().put(rownr_p, *phaseDiffField_p);
		    if (phaseDiffFlagField_p.isAttached()) {
			msSysCalCols_p->phaseDiffFlag().put(rownr_p, *phaseDiffFlagField_p);
		    } else {
			msSysCalCols_p->phaseDiffFlag().put(rownr_p, False);
		    }
		}
	    }
	} else {
	    // reuse this row, make sure that the time range is fully set
	    // and place the time in the center of it
	    Double rowTime = msSysCalCols_p->time()(rownr_p);
	    Double rowInterval = msSysCalCols_p->interval()(rownr_p);
	    Double minTime, maxTime;
	    minTime = min(time-interval/2.0, rowTime-rowInterval/2.0);
	    maxTime = max(time+interval/2.0, rowTime+rowInterval/2.0);
	    msSysCalCols_p->time().put(rownr_p, (maxTime+minTime)/2.0);
	    msSysCalCols_p->interval().put(rownr_p, (maxTime-minTime));
	}
    }
}

void SDSysCalHandler::clearAll()
{
    delete msSysCal_p;
    msSysCal_p = 0;

    delete msSysCalCols_p;
    msSysCalCols_p = 0;

    clearRow();
}

void SDSysCalHandler::clearRow()
{
    tcalId_p = tsysId_p = trxId_p = -1;
    intervalField_p.detach();
    timeField_p.detach();
    phaseDiffField_p.detach();
    phaseDiffFlagField_p.detach();
    tcalFlagField_p.detach();
    trxFlagField_p.detach();
    tsysFlagField_p.detach();
    tcalField_p.detach();
    trxField_p.detach();
    tsysField_p.detach();
}

void SDSysCalHandler::initAll(NewMeasurementSet &ms, Vector<Bool> &handledCols,
			      const Record &row)
{
    msSysCal_p = new NewMSSysCal(ms.sysCal());
    AlwaysAssert(msSysCal_p, AipsError);

    initRow(handledCols, row);

    // do we need to add any optional columns
    TableDesc td;
    if (tsysId_p >= 0 || tsysField_p.isAttached()) {
	hasTsysCol_p = True;
	NewMSSysCal::addColumnToDesc(td,NewMSSysCal::TSYS);
	NewMSSysCal::addColumnToDesc(td,NewMSSysCal::TSYS_FLAG);
    }
    if (tcalId_p >= 0 || tcalField_p.isAttached()) {
	hasTcalCol_p = True;
	NewMSSysCal::addColumnToDesc(td,NewMSSysCal::TCAL);
	NewMSSysCal::addColumnToDesc(td,NewMSSysCal::TCAL_FLAG);
    }
    if (trxId_p >= 0 || trxField_p.isAttached()) {
	hasTrxCol_p = True;
	NewMSSysCal::addColumnToDesc(td,NewMSSysCal::TRX);
	NewMSSysCal::addColumnToDesc(td,NewMSSysCal::TRX_FLAG);
    }
    for (uInt i=0;i<td.ncolumn();i++) {
	msSysCal_p->addColumn(td[i]);
    }

    msSysCalCols_p = new NewMSSysCalColumns(*msSysCal_p);
    AlwaysAssert(msSysCalCols_p, AipsError);

    nrecpt_p = 0;
    rownr_p = -1;
}

void SDSysCalHandler::initRow(Vector<Bool> &handledCols, const Record &row)
{
    tcalId_p = row.fieldNumber("TCAL");
    if (tcalId_p >= 0) handledCols(tcalId_p) = True;
    tsysId_p = row.fieldNumber("TSYS");
    if (tsysId_p >= 0) handledCols(tsysId_p) = True;    
    trxId_p = row.fieldNumber("TRX");
    if (trxId_p >= 0) handledCols(trxId_p) = True;
    
    Int tmp;
    tmp = row.fieldNumber("SYSCAL_INTERVAL");
    if (tmp >= 0 && row.dataType(tmp) == TpDouble) {
	intervalField_p.attachToRecord(row, tmp);
	handledCols(tmp) = True;
    }
    tmp = row.fieldNumber("SYSCAL_TIME");
    if (tmp >= 0 && row.dataType(tmp) == TpDouble) {
	timeField_p.attachToRecord(row, tmp);
	handledCols(tmp) = True;
    }
    tmp = row.fieldNumber("SYSCAL_PHASE_DIFF");
    if (tmp >= 0 && row.dataType(tmp) == TpFloat) {
	phaseDiffField_p.attachToRecord(row, tmp);
	handledCols(tmp) = True;
    }
    tmp = row.fieldNumber("SYSCAL_PHASE_DIFF_FLAG");
    if (tmp >= 0 && row.dataType(tmp) == TpBool) {
	phaseDiffFlagField_p.attachToRecord(row, tmp);
	handledCols(tmp) = True;
    }
    tmp = row.fieldNumber("SYSCAL_TCAL");
    if (tmp >= 0 && row.dataType(tmp) == TpArrayFloat) {
	tcalField_p.attachToRecord(row, tmp);
	handledCols(tmp) = True;
    }
    tmp = row.fieldNumber("SYSCAL_TCAL_FLAG");
    if (tmp >= 0 && row.dataType(tmp) == TpBool) {
	tcalFlagField_p.attachToRecord(row, tmp);
	handledCols(tmp) = True;
    }
    tmp = row.fieldNumber("SYSCAL_TRX");
    if (tmp >= 0 && row.dataType(tmp) == TpArrayFloat) {
	trxField_p.attachToRecord(row, tmp);
	handledCols(tmp) = True;
    }
    tmp = row.fieldNumber("SYSCAL_TRX_FLAG");
    if (tmp >= 0 && row.dataType(tmp) == TpBool) {
	trxFlagField_p.attachToRecord(row, tmp);
	handledCols(tmp) = True;
    }
    tmp = row.fieldNumber("SYSCAL_TSYS");
    if (tmp >= 0 && row.dataType(tmp) == TpArrayFloat) {
	tsysField_p.attachToRecord(row, tmp);
	handledCols(tmp) = True;
    }
    tmp = row.fieldNumber("SYSCAL_TSYS_FLAG");
    if (tmp >= 0 && row.dataType(tmp) == TpBool) {
	tsysFlagField_p.attachToRecord(row, tmp);
	handledCols(tmp) = True;
    }
    // ignore this field as it add no useful additional information
    if (row.fieldNumber("SYSCAL_NUM_RECEPTORS")) handledCols(row.fieldNumber("SYSCAL_NUM_RECEPTORS")) = True;
}
