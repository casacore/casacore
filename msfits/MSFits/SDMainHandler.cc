//# SDMainHandler.cc: a MAIN handler for SDFITS data  
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
#include <casacore/msfits/MSFits/SDMainHandler.h>

#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/ms/MeasurementSets/MSMainColumns.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/measures/Measures/MEpoch.h>
#include <casacore/casa/Quanta/MVTime.h>
#include <casacore/casa/Containers/Record.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

SDMainHandler::SDMainHandler() 
    : ms_p(0), msCols_p(0),
      scanNumberId_p(-1), arrayIdId_p(-1), sigmaId_p(-1), flagRowId_p(-1),
      intervalId_p(-1), weightId_p(-1), flagId_p(-1), timeCentroidId_p(-1)
{;}

SDMainHandler::SDMainHandler(MeasurementSet &ms, Vector<Bool> &handledCols, const Record &row)
    : ms_p(0), msCols_p(0),
      scanNumberId_p(-1), arrayIdId_p(-1), sigmaId_p(-1), flagRowId_p(-1),
      intervalId_p(-1), weightId_p(-1), flagId_p(-1), timeCentroidId_p(-1)
{
    initAll(ms, handledCols, row);
}

SDMainHandler::SDMainHandler(const SDMainHandler &other) 
    : ms_p(0), msCols_p(0),
      scanNumberId_p(-1), arrayIdId_p(-1), sigmaId_p(-1), flagRowId_p(-1),
      intervalId_p(-1), weightId_p(-1), flagId_p(-1), timeCentroidId_p(-1)
{
    *this = other;
}

SDMainHandler &SDMainHandler::operator=(const SDMainHandler &other)
{
    if (this != &other) {
	clearAll();
	ms_p = new MeasurementSet(*(other.ms_p));
	AlwaysAssert(ms_p, AipsError);
	msCols_p = new MSMainColumns(*ms_p);
	AlwaysAssert(msCols_p, AipsError);
	scanNumberId_p = other.scanNumberId_p;
	arrayIdId_p = other.arrayIdId_p;
	sigmaId_p = other.sigmaId_p;
	flagRowId_p = other.flagRowId_p;
	intervalId_p = other.intervalId_p;
	weightId_p = other.weightId_p;
	flagId_p = other.flagId_p;
	timeCentroidId_p = other.timeCentroidId_p;
    }
    return *this;
}

void SDMainHandler::attach(MeasurementSet &ms, Vector<Bool> &handledCols, const Record &row)
{
    clearAll();
    initAll(ms, handledCols, row);
}

void SDMainHandler::resetRow(const Record &row)
{
    clearRow();
    Vector<Bool> dummyHandledCols;
    initRow(dummyHandledCols, row);
}

void SDMainHandler::fill(const Record &row, const MEpoch &time, Int antennaId, Int feedId,
			 Int dataDescId, Int fieldId, const MVTime &exposure, 
			 Int observationId, const Matrix<Float> &floatData)
{
    // don't bother unless there is something there
    if (ms_p) {
	// fill it
	Int rownr = ms_p->nrow();
	ms_p->addRow();

	Int ncorr = floatData.nrow();

	msCols_p->timeMeas().put(rownr, time);
	msCols_p->antenna1().put(rownr,antennaId);
	msCols_p->antenna2().put(rownr,antennaId);
	msCols_p->feed1().put(rownr,feedId);
	msCols_p->feed2().put(rownr,feedId);
	msCols_p->dataDescId().put(rownr, dataDescId);
	msCols_p->processorId().put(rownr, -1);
	msCols_p->fieldId().put(rownr, fieldId);
	Double texp = exposure.get("s").getValue();
	if (intervalId_p >= 0) {
	    msCols_p->interval().put(rownr, row.asDouble(intervalId_p));
	} else {
	    msCols_p->interval().put(rownr, texp);
	}
	msCols_p->exposure().put(rownr, texp);
	Int scanNumber = -1;
	if (scanNumberId_p >= 0) {
	    switch (scanNumberType_p) {
	    case TpInt:
	    case TpShort:
		scanNumber = row.asInt(scanNumberId_p);
		break;
	    case TpDouble:
	    case TpFloat:
		scanNumber = Int(row.asDouble(scanNumberId_p)+0.5);
		break;
	    default:
		// a warning should be issued when the type is initially determined
		scanNumber = -1;
		break;
	    }
	}
	msCols_p->scanNumber().put(rownr, scanNumber);
	if (arrayIdId_p>=0) {
	    msCols_p->arrayId().put(rownr, row.asInt(arrayIdId_p));
	} else {
	    msCols_p->arrayId().put(rownr, -1);
	}
	msCols_p->observationId().put(rownr, observationId);
	msCols_p->stateId().put(rownr, -1);
	msCols_p->uvw().put(rownr, Vector<Double>(3,0.0));
	msCols_p->floatData().put(rownr, floatData);
	if (sigmaId_p >= 0) {
	    msCols_p->sigma().put(rownr, row.asArrayFloat(sigmaId_p));
	} else {
	    // should this be TSYS and exposure based?
	    msCols_p->sigma().put(rownr, Vector<Float>(ncorr, 1.0));
	}
	if (weightId_p >= 0) {
	    msCols_p->weight().put(rownr, row.asArrayFloat(weightId_p));
	} else {
	    msCols_p->weight().put(rownr, Vector<Float>(ncorr, 1.0));
	}
	if (flagId_p >= 0) {
	    msCols_p->flag().put(rownr, row.asArrayBool(flagId_p));
	} else {
	    msCols_p->flag().put(rownr, Matrix<Bool>(floatData.shape(), False));
	}
	if (timeCentroidId_p >= 0) {
	    msCols_p->timeCentroid().put(rownr, row.asDouble(timeCentroidId_p));
	} else {
	    msCols_p->timeCentroid().put(rownr,msCols_p->time()(rownr));
	}
	IPosition emptyFlagCatShape(3,0);
	emptyFlagCatShape(0) = ncorr;
	emptyFlagCatShape(1) = floatData.ncolumn();
	msCols_p->flagCategory().put(rownr, Array<Bool>(emptyFlagCatShape));
	if (flagRowId_p >= 0) {
	    msCols_p->flagRow().put(rownr, row.asBool(flagRowId_p));
	} else {
	    msCols_p->flagRow().put(rownr, False);
	}
    }
}

void SDMainHandler::clearAll()
{
    delete ms_p;
    ms_p = 0;

    delete msCols_p;
    msCols_p = 0;

    clearRow();
}

void SDMainHandler::clearRow()
{
    scanNumberId_p = arrayIdId_p = sigmaId_p = flagRowId_p = intervalId_p = 
	weightId_p = flagId_p = timeCentroidId_p = -1;
}

void SDMainHandler::initAll(MeasurementSet &ms, Vector<Bool> &handledCols, const Record &row)
{
    ms_p = new MeasurementSet(ms);
    AlwaysAssert(ms_p, AipsError);

    initRow(handledCols, row);

    msCols_p = new MSMainColumns(*ms_p);
    AlwaysAssert(msCols_p, AipsError);
}

void SDMainHandler::initRow(Vector<Bool> &handledCols, const Record &row)
{
    scanNumberId_p = row.fieldNumber("SCAN");
    if (scanNumberId_p >= 0) {
	handledCols(scanNumberId_p) = True;
	scanNumberType_p = row.dataType(scanNumberId_p);
    }
    arrayIdId_p = row.fieldNumber("MAIN_ARRAY_ID");
    if (arrayIdId_p >= 0) handledCols(arrayIdId_p) = True;
    sigmaId_p = row.fieldNumber("MAIN_SIGMA");
    if (sigmaId_p >= 0) handledCols(sigmaId_p) = True;
    flagRowId_p = row.fieldNumber("MAIN_FLAG_ROW");
    if (flagRowId_p >= 0) handledCols(flagRowId_p) = True;
    intervalId_p = row.fieldNumber("MAIN_INTERVAL");
    if (intervalId_p >= 0) handledCols(intervalId_p) = True;
    weightId_p = row.fieldNumber("MAIN_WEIGHT");
    if (weightId_p >= 0) handledCols(weightId_p) = True;
    flagId_p = row.fieldNumber("MAIN_FLAG");
    if (flagId_p >= 0) handledCols(flagId_p) = True;
    timeCentroidId_p = row.fieldNumber("MAIN_TIME_CENTROID");
    if (timeCentroidId_p >= 0) handledCols(timeCentroidId_p) = True;

    // RADECSYS is fully covered elsewhere, ignore it if it exists
    if (row.fieldNumber("RADECSYS") >= 0) handledCols(row.fieldNumber("RADECSYS")) = True;

    // the following fields generated when MS v 1 was converted to an SDFITS file are ignored
    // There is no CORRELATOR table in MS 2 and it should never have been used for SD data
    // in MS 1.
    if (row.fieldNumber("MAIN_CORRELATOR_ID") >= 0) 
	handledCols(row.fieldNumber("MAIN_CORRELATOR_ID")) = True;
    // there is no PULSAR_BIN in MS 2 and its unlikely it will have been used by
    // single dish data in MS 1
    if (row.fieldNumber("MAIN_PULSAR_BIN") >= 0) 
	handledCols(row.fieldNumber("MAIN_PULSAR_BIN")) = True;
}

} //# NAMESPACE CASACORE - END

