//# SDFeedHandler.cc: an FEED handler for SDFITS data  
//# Copyright (C) 2000,2001,2003
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
#include <casacore/msfits/MSFits/SDFeedHandler.h>

#include <casacore/tables/Tables/ColumnsIndex.h>
#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/ms/MeasurementSets/MSFeedColumns.h>
#include <casacore/ms/MeasurementSets/MSFeed.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/measures/Measures/Stokes.h>
#include <casacore/casa/Containers/SimOrdMap.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/tables/Tables/TableDesc.h>

#include <casacore/casa/sstream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

SDFeedHandler::SDFeedHandler() 
    : index_p(0), msFeed_p(0), msFeedCols_p(0), feedId_p(-1), nextFeedId_p(0), nrecpt_p(0)
{;}

SDFeedHandler::SDFeedHandler(MeasurementSet &ms, Vector<Bool> &handledCols, const Record &row) 
    : index_p(0), msFeed_p(0), msFeedCols_p(0), feedId_p(-1), nextFeedId_p(0), nrecpt_p(0)
{
    initAll(ms, handledCols, row);
}

SDFeedHandler::SDFeedHandler(const SDFeedHandler &other) 
    : index_p(0), msFeed_p(0), msFeedCols_p(0), feedId_p(-1), nextFeedId_p(0), nrecpt_p(0)
{
    *this = other;
}

SDFeedHandler &SDFeedHandler::operator=(const SDFeedHandler &other)
{
    if (this != &other) {
	clearAll();
	index_p = new ColumnsIndex(*(other.index_p));
	AlwaysAssert(index_p, AipsError);
	// need to avoid the assignment operator here because we want
	// this to point to the field in index_p, not in other.index_p
	numRecpKey_p.attachToRecord(index_p->accessKey(),
				    MSFeed::columnName(MSFeed::NUM_RECEPTORS));
	msFeed_p = new MSFeed(*(other.msFeed_p));
	AlwaysAssert(msFeed_p, AipsError);
	msFeedCols_p = new MSFeedColumns(*msFeed_p);
	AlwaysAssert(msFeedCols_p, AipsError);
	feedId_p = other.feedId_p;
	nextFeedId_p = other.nextFeedId_p;
	nrecpt_p = other.nrecpt_p;
	feed1Field_p = other.feed1Field_p;
	feed2Field_p = other.feed2Field_p;
	beamIdField_p = other.beamIdField_p;
	phasedFeedIdField_p = other.phasedFeedIdField_p;
	numReceptorsField_p = other.numReceptorsField_p;
	intervalField_p =  other.intervalField_p;
	timeField_p = other.timeField_p; 
	beamOffsetField_p = other.beamOffsetField_p;
	positionField_p = other.positionField_p;
	receptorAngleField_p = other.receptorAngleField_p;
	scaReceptorAngleField_p = other.scaReceptorAngleField_p;
	polResponseField_p = other.polResponseField_p;
	polarizationTypeField_p = other.polarizationTypeField_p;
    }
    return *this;
}

void SDFeedHandler::attach(MeasurementSet &ms, Vector<Bool> &handledCols, const Record &row)
{
    clearAll();
    initAll(ms, handledCols, row);
}

void SDFeedHandler::resetRow(const Record &row)
{
    clearRow();
    Vector<Bool> dummyCols(row.nfields());
    initRow(dummyCols, row);
}

void SDFeedHandler::fill(const Record &, Int antennaId, Int spwinId, const Vector<Int> &stokes)
{
    // don't bother unless there is something there
    if (msFeed_p) {
	Vector<String> polType;
	stokesToPolType(stokes, polType);
	*numRecpKey_p = polType.nelements();
	nrecpt_p = *numRecpKey_p;
	Bool found = False;
	feedId_p = -1;
	Vector<uInt> foundRows = index_p->getRowNumbers();
	uInt whichOne = 0;
	// this is True if the row has probably come from a MS AND FEED1 == FEED2
	// When true, fill will try and reuse the same feed number if possible
	Bool doMSCheck = feed1Field_p.isAttached() && feed2Field_p.isAttached() && *feed1Field_p == *feed2Field_p;
	// also, ignore the MS row columns if NUM_RECEPTORS there doesn't match numRecpKey_p
	doMSCheck = doMSCheck && numReceptorsField_p.isAttached() && *numReceptorsField_p == *numRecpKey_p;
	// also, ignore the MS row columns if POLARIZATION_TYPE doesn't match polType
	if (doMSCheck && polarizationTypeField_p.isAttached()) {
	    // turn this into an array
	    istringstream istr(String((*polarizationTypeField_p).chars(), (*polarizationTypeField_p).length()));
	    Array<String> polTypeArr;
	    // decode it - [#,#,#,#...] - individual brackets separated by commas
	    istr >> polTypeArr;
	    // if polTypeArr is empty, interpret that as sufficient unknown values so
	    // that polTypeArr has same shape as polType
	    if (polTypeArr.nelements() == 0) {
		polTypeArr.resize(polType.shape());
		polTypeArr = Stokes::name(Stokes::Undefined);
	    }
	    doMSCheck = allEQ(polType, polTypeArr);
	}
	while (!found && whichOne<foundRows.nelements()) {
	    // these must all have the required number of receptors
	    uInt thisRow = foundRows(whichOne);
	    if (allEQ(polType, msFeedCols_p->polarizationType()(thisRow))) {
		// we can reuse this feed id, at least
		feedId_p = msFeedCols_p->feedId()(thisRow);
		// the antennaId and spwinId need to match to reuse this row
		if (msFeedCols_p->antennaId()(thisRow) == antennaId &&
		    msFeedCols_p->spectralWindowId()(thisRow) == spwinId) {
		    // we have a winner
		    found = True;
		    if (doMSCheck) {
			// double check to see if this row matches the one in the table
			if (found && beamIdField_p.isAttached()) { 
			    found = *beamIdField_p == msFeedCols_p->beamId()(thisRow);
			}
			if (found && phasedFeedIdField_p.isAttached() && !msFeedCols_p->phasedFeedId().isNull()) { 
			    found = *phasedFeedIdField_p == msFeedCols_p->phasedFeedId()(thisRow);
			}
			if (found && intervalField_p.isAttached()) { 
			    found = *intervalField_p == msFeedCols_p->interval()(thisRow);
			}
			if (found && timeField_p.isAttached()) { 
			    found = *timeField_p == msFeedCols_p->time()(thisRow);
			}
			if (found && beamOffsetField_p.isAttached()) { 
			    found = allEQ(*beamOffsetField_p,msFeedCols_p->beamOffset()(thisRow));
			}
			if (found && positionField_p.isAttached()) { 
			    found = allEQ(*positionField_p,msFeedCols_p->position()(thisRow));
			}
			if (found && receptorAngleField_p.isAttached()) { 
			    found = allEQ(*receptorAngleField_p,msFeedCols_p->receptorAngle()(thisRow));
			}
			if (found && scaReceptorAngleField_p.isAttached()) { 
			    found = allEQ(msFeedCols_p->receptorAngle()(thisRow),*scaReceptorAngleField_p);
			}
			if (found && polResponseField_p.isAttached()) { 
			    found = allEQ(*polResponseField_p,msFeedCols_p->polResponse()(thisRow));
			}
		    }
		}
	    }
	    if (!found) whichOne++;
	}
	// if it was found, nothing else to do
	if (!found) {
	    // we need to add one
	    Int newRow = msFeed_p->nrow();
	    if (doMSCheck) {
		// we're reusing what is in the row from a previous MS
		// the feed number
		// but only if the feed is still < 0 -> no match yet so we can use this one
		if (feedId_p < 0) feedId_p = *feed1Field_p;
		// make sure we can't ever automatically reuse this one by accident
		if (feedId_p >= nextFeedId_p) nextFeedId_p = feedId_p + 1;
	    } else {
		// new feed id needed?
		if (feedId_p < 0) feedId_p = nextFeedId_p++;
	    }
	    msFeed_p->addRow();
	    msFeedCols_p->antennaId().put(newRow, antennaId);
	    msFeedCols_p->feedId().put(newRow, feedId_p);
	    msFeedCols_p->spectralWindowId().put(newRow, spwinId);
	    if (timeField_p.isAttached()) {
		msFeedCols_p->time().put(newRow, *timeField_p);
	    } else {
		msFeedCols_p->time().put(newRow, 0.0);
	    }
	    if (intervalField_p.isAttached()) {
		msFeedCols_p->interval().put(newRow, *intervalField_p);
	    } else {
		msFeedCols_p->interval().put(newRow, 0.0);
	    }
	    msFeedCols_p->numReceptors().put(newRow, *numRecpKey_p);
	    if (beamIdField_p.isAttached()) {
		msFeedCols_p->beamId().put(newRow, *beamIdField_p);
	    } else {
		msFeedCols_p->beamId().put(newRow, -1);
	    }
	    if (beamOffsetField_p.isAttached()) {
		msFeedCols_p->beamOffset().put(newRow, *beamOffsetField_p);
	    } else {
		msFeedCols_p->beamOffset().put(newRow, Matrix<Double>(2,*numRecpKey_p,0.0));
	    }
	    msFeedCols_p->polarizationType().put(newRow, polType);
	    if (polResponseField_p.isAttached()) {
		msFeedCols_p->polResponse().put(newRow, *polResponseField_p);
	    } else {
		Matrix<Complex> polResponse(*numRecpKey_p, *numRecpKey_p, 0.0);
		// assume no cross talk
		polResponse.diagonal() = 1.0;
		msFeedCols_p->polResponse().put(newRow, polResponse);
	    }
	    if (positionField_p.isAttached()) {
		msFeedCols_p->position().put(newRow, *positionField_p);
	    } else {
		msFeedCols_p->position().put(newRow, Vector<Double>(3,0.0));
	    }
	    if (receptorAngleField_p.isAttached()) {
		msFeedCols_p->receptorAngle().put(newRow, *receptorAngleField_p);
	    } else if (scaReceptorAngleField_p.isAttached()) {
		msFeedCols_p->receptorAngle().put(newRow, Vector<Double>(*numRecpKey_p, *scaReceptorAngleField_p));
	    } else {
		msFeedCols_p->receptorAngle().put(newRow, Vector<Double>(*numRecpKey_p, 0.0));
	    }
	    if (phasedFeedIdField_p.isAttached()) {
		if (msFeedCols_p->phasedFeedId().isNull() && *phasedFeedIdField_p >= 0) {
		    // add this optional column when necessary
		    delete msFeedCols_p;
		    msFeedCols_p = 0;
		    TableDesc td;
		    MSFeed::addColumnToDesc(td, MSFeed::PHASED_FEED_ID);
		    msFeed_p->addColumn(td[0]);
		    msFeedCols_p = new MSFeedColumns(*msFeed_p);
		    AlwaysAssert(msFeedCols_p, AipsError);
		}
		if (!msFeedCols_p->phasedFeedId().isNull()) {
		    msFeedCols_p->phasedFeedId().put(newRow, *phasedFeedIdField_p);
		}
	    }
	}
    }
}

void SDFeedHandler::clearAll()
{
    delete index_p;
    index_p = 0;

    delete msFeed_p;
    msFeed_p = 0;

    delete msFeedCols_p;
    msFeedCols_p = 0;

    feedId_p = -1;
    nextFeedId_p = 0;

    nrecpt_p = 0;

    clearRow();
}

void SDFeedHandler::clearRow()
{
    feedId_p = -1;

    feed1Field_p.detach(); 
    feed2Field_p.detach(); 
    beamIdField_p.detach(); 
    phasedFeedIdField_p.detach(); 
    numReceptorsField_p.detach();
    intervalField_p.detach(); 
    timeField_p.detach(); 
    beamOffsetField_p.detach(); 
    positionField_p.detach(); 
    receptorAngleField_p.detach();
    scaReceptorAngleField_p.detach();
    polResponseField_p.detach();
    polarizationTypeField_p.detach();
}

void SDFeedHandler::initAll(MeasurementSet &ms, Vector<Bool> &handledCols, const Record &row)
{
    msFeed_p = new MSFeed(ms.feed());
    AlwaysAssert(msFeed_p, AipsError);

    msFeedCols_p = new MSFeedColumns(*msFeed_p);
    AlwaysAssert(msFeedCols_p, AipsError);

    index_p = new ColumnsIndex(*msFeed_p, 
			       MSFeed::columnName(MSFeed::NUM_RECEPTORS));
    AlwaysAssert(index_p, AipsError);
    
    numRecpKey_p.attachToRecord(index_p->accessKey(),
				MSFeed::columnName(MSFeed::NUM_RECEPTORS));
    feedId_p = -1;
    nextFeedId_p = 0;
    nrecpt_p = 0;

    initRow(handledCols, row);
}

void SDFeedHandler::initRow(Vector<Bool> &handledCols, const Record &row)
{
    AlwaysAssert(handledCols.nelements()==row.description().nfields(), AipsError);

    if (row.fieldNumber("MAIN_FEED1") >= 0 && row.dataType("MAIN_FEED1") == TpInt) {
	feed1Field_p.attachToRecord(row, "MAIN_FEED1");
	handledCols(row.fieldNumber("MAIN_FEED1")) = True;
    }
    if (row.fieldNumber("MAIN_FEED2") >= 0 && row.dataType("MAIN_FEED2") == TpInt) {
	feed2Field_p.attachToRecord(row, "MAIN_FEED2");
	handledCols(row.fieldNumber("MAIN_FEED2")) = True;
    }
    if (row.fieldNumber("FEED_BEAM_ID") >= 0 && row.dataType("FEED_BEAM_ID") == TpInt) {
	beamIdField_p.attachToRecord(row, "FEED_BEAM_ID");
	handledCols(row.fieldNumber("FEED_BEAM_ID")) = True;
    }
    if (row.fieldNumber("FEED_PHASED_FEED_ID") >= 0 && row.dataType("FEED_PHASED_FEED_ID") == TpInt) {
	phasedFeedIdField_p.attachToRecord(row, "FEED_PHASED_FEED_ID");
	handledCols(row.fieldNumber("FEED_PHASED_FEED_ID")) = True;
    }
    if (row.fieldNumber("FEED_NUM_RECEPTORS") >= 0 && row.dataType("FEED_NUM_RECEPTORS") == TpInt) {
	numReceptorsField_p.attachToRecord(row, "FEED_NUM_RECEPTORS");
	handledCols(row.fieldNumber("FEED_NUM_RECEPTORS")) = True;
    }
    if (row.fieldNumber("FEED_INTERVAL") >= 0 && row.dataType("FEED_INTERVAL") == TpDouble) {
	intervalField_p.attachToRecord(row, "FEED_INTERVAL");
	handledCols(row.fieldNumber("FEED_INTERVAL")) = True;
    }
    if (row.fieldNumber("FEED_TIME") >= 0 && row.dataType("FEED_TIME") == TpDouble ) {
	timeField_p.attachToRecord(row, "FEED_TIME");
	handledCols(row.fieldNumber("FEED_TIME")) = True;
    }
    if (row.fieldNumber("FEED_BEAM_OFFSET") >= 0 && row.dataType("FEED_BEAM_OFFSET") == TpArrayDouble) {
	beamOffsetField_p.attachToRecord(row, "FEED_BEAM_OFFSET");
	handledCols(row.fieldNumber("FEED_BEAM_OFFSET")) = True;
    }
    if (row.fieldNumber("FEED_POSITION") >= 0 && row.dataType("FEED_POSITION") == TpArrayDouble) {
	positionField_p.attachToRecord(row, "FEED_POSITION");
	handledCols(row.fieldNumber("FEED_POSITION")) = True;
    }
    if (row.fieldNumber("FEED_RECEPTOR_ANGLE") >= 0) {
	if (row.dataType("FEED_RECEPTOR_ANGLE") == TpArrayDouble) {
	    receptorAngleField_p.attachToRecord(row, "FEED_RECEPTOR_ANGLE");
	    handledCols(row.fieldNumber("FEED_RECEPTOR_ANGLE")) = True;
	} else if (row.dataType("FEED_RECEPTOR_ANGLE") == TpDouble) {
	    scaReceptorAngleField_p.attachToRecord(row, "FEED_RECEPTOR_ANGLE");
	    handledCols(row.fieldNumber("FEED_RECEPTOR_ANGLE")) = True;
	}
    }
    if (row.fieldNumber("FEED_POL_RESPONSE") >= 0 && row.dataType("FEED_POL_RESPONSE") == TpArrayComplex) {
	polResponseField_p.attachToRecord(row, "FEED_POL_RESPONSE");
	handledCols(row.fieldNumber("FEED_POL_RESPONSE")) = True;
    }
    if (row.fieldNumber("FEED_POLARIZATION_TYPE") >= 0 && row.dataType("FEED_POLARIZATION_TYPE") == TpString) {
	polarizationTypeField_p.attachToRecord(row, "FEED_POLARIZATION_TYPE");
	handledCols(row.fieldNumber("FEED_POLARIZATION_TYPE")) = True;
    }
}


void SDFeedHandler::stokesToPolType(const Vector<Int> &stokes, Vector<String> &polType)
{
    SimpleOrderedMap<String, Int> polTypeMap(-1);
    for (uInt i=0;i<stokes.nelements();i++) {
	String type1, type2;
	switch (Stokes::type(stokes(i))) {
	case Stokes::RR:
	    type1 = "R";
	    type2 = type1;
	    break;
	case Stokes::RL:
	    type1 = "R";
	    type2 = "L";
	break;
	case Stokes::LR:
	    type1 = "L";
	    type2 = "R";
	    break;
	case Stokes::LL:
	    type1 = "L";
	    type2 = type1;
	    break;
	case Stokes::XX:
	    type1 = "X";
	    type2 = type1;
	    break;
	case Stokes::XY:
	    type1 = "X";
	    type2= "Y";
	    break;
	case Stokes::YX:
	    type1 = "Y";
	    type2 = "X";
	    break;
	case Stokes::YY:
	    type1 = "Y";
	    type2 = type1;
	    break;
	case Stokes::RX:
	    type1 = "R";
	    type2 = "X";
	    break;
	case Stokes::RY:
	    type1 = "R";
	    type2 = "Y";
	    break;
	case Stokes::LX:
	    type1 = "L";
	    type2 = "X";
	    break;
	case Stokes::LY:
	    type1 = "L";
	    type2 = "Y";
	    break;
	case Stokes::XR:
	    type1 = "X";
	    type2 = "R";
	    break;
	case Stokes::YR:
	    type1 = "Y";
	    type2 = "R";
	    break;
	case Stokes::XL:
	    type1 = "X";
	    type2 = "L";
	    break;
	case Stokes::YL:
	    type1 = "Y";
	    type2 = "L";
	    break;
	case Stokes::PP:
	    type1 = "PP";
	    type2 = type1;
	    break;
	case Stokes::PQ:
	    type1 = "P";
	    type2 = "Q";
	    break;
	case Stokes::QP:
	    type1 = "Q";
	    type2 = "P";
	    break;
	case Stokes::QQ:
	    type1 = "Q";
	    type2 = type1;
	    break;
	default:
	    type1 = Stokes::name(Stokes::type(stokes(i)));
	    type2 = type1;
	    break;
	}

	if (!polTypeMap.isDefined(type1)) {
	    polTypeMap.define(type1, polTypeMap.ndefined());
	}
	if (!polTypeMap.isDefined(type2)) {
	    polTypeMap.define(type2, polTypeMap.ndefined());
	}
    }
    polType.resize(polTypeMap.ndefined());
    for (uInt i=0;i<polTypeMap.ndefined();i++) {
	polType(i) = polTypeMap.getKey(i);
    }
}

} //# NAMESPACE CASACORE - END

