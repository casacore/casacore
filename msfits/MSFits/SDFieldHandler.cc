//# SDFieldHandler.cc: a FIELD handler for SDFITS data  
//# Copyright (C) 2000,2001
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
#include <casacore/msfits/MSFits/SDFieldHandler.h>

#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/ms/MeasurementSets/MSFieldColumns.h>
#include <casacore/ms/MeasurementSets/MSField.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/tables/Tables/ColumnsIndex.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

SDFieldHandler::SDFieldHandler() 
    : msField_p(0), msFieldCols_p(0), rownr_p(-1), index_p(0)
{;}

SDFieldHandler::SDFieldHandler(MeasurementSet &ms, Vector<Bool> &handledCols, const Record &row) 
    : msField_p(0), msFieldCols_p(0), rownr_p(-1), index_p(0)
{
    initAll(ms, handledCols, row);
}

SDFieldHandler::SDFieldHandler(const SDFieldHandler &other) 
    : msField_p(0), msFieldCols_p(0), rownr_p(-1), index_p(0)
{
    *this = other;
}

SDFieldHandler &SDFieldHandler::operator=(const SDFieldHandler &other)
{
    if (this != &other) {
	clearAll();
	msField_p = new MSField(*(other.msField_p));
	AlwaysAssert(msField_p, AipsError);
	msFieldCols_p = new MSFieldColumns(*msField_p);
	AlwaysAssert(msFieldCols_p, AipsError);
	rownr_p = other.rownr_p;
	fieldIdField_p = other.fieldIdField_p;
	codeField_p = other.codeField_p;
	nameField_p = other.nameField_p;
	timeField_p = other.timeField_p;
	delayDirField_p = other.delayDirField_p;
	delayDirRateField_p = other.delayDirRateField_p;
	phaseDirField_p = other.phaseDirField_p;
	phaseDirRateField_p = other.phaseDirRateField_p;
	referenceDirField_p = other.referenceDirField_p;
	referenceDirRateField_p = other.referenceDirRateField_p;
	flagRowField_p = other.flagRowField_p;
	delete index_p;
	index_p = new ColumnsIndex(*msField_p, stringToVector("NAME,SOURCE_ID,TIME"));
	AlwaysAssert(index_p, AipsError);
	// attach the keys
	nameKey_p.attachToRecord(index_p->accessKey(),"NAME");
	sourceIdKey_p.attachToRecord(index_p->accessKey(),"SOURCE_ID");
	timeKey_p.attachToRecord(index_p->accessKey(),"TIME");
	*nameKey_p = *other.nameKey_p;
	*sourceIdKey_p = *other.sourceIdKey_p;
	*timeKey_p = *other.timeKey_p;
    }
    return *this;
}

void SDFieldHandler::attach(MeasurementSet &ms, Vector<Bool> &handledCols, const Record &row)
{
    clearAll();
    initAll(ms, handledCols, row);
}

void SDFieldHandler::resetRow(const Record &row)
{
    clearRow();
    Vector<Bool> dummyCols(row.nfields());
    initRow(dummyCols, row);
}

void SDFieldHandler::fill(const Record &, const String &name, Int directionRefType,
			  const Matrix<Double> &directionPoly, Double time, Int sourceId)
{
    // don't bother unless there is something there
    if (msField_p) {
	Bool found = False;
	Bool checkPhase, checkRef;
	checkPhase = checkRef = False;
	Matrix<Double> dirPoly = directionPoly;
	Matrix<Double> phasePoly = directionPoly;
	Matrix<Double> referencePoly = directionPoly;
	Int npoly = dirPoly.nrow() - 1;

	// adjustments to the above given possible former MS columns
	if (delayDirField_p.isAttached()) {
	    // old MS 1 is always accompanied by a delayDirRateField_p
	    if (delayDirRateField_p.isAttached()) {
		// only use this if the rate is non-zero AND non-inf AND not a NaN
		Vector<Double> ddRate(*delayDirRateField_p);
		Double d0, d1;
		d0 = ddRate(0);
		d1 = ddRate(1);
		if (!near(d0,0.0) && !near(d1,0.0) && !isInf(d0) && !isInf(d1) &&
		    !isNaN(d0) && !isNaN(d1)) {
		    npoly = 1;
		}
		dirPoly.resize(2,npoly+1);
		dirPoly.column(0) = *delayDirField_p;
		if (npoly == 1) dirPoly.column(1) = ddRate;
	    } else {
		dirPoly.resize((*delayDirField_p).shape());
		dirPoly = *delayDirField_p;
		npoly = dirPoly.nrow() - 1;
	    }
	}
	if (phaseDirField_p.isAttached()) {
	    checkPhase = True;
	    // old MS 1 is always accompanied by a phaseDirRateField_p
	    if (phaseDirRateField_p.isAttached()) {
		// only use this if the rate is non-zero AND non-inf AND not a NaN
		Vector<Double> pdRate(*phaseDirRateField_p);
		Double p0, p1;
		p0 = pdRate(0);
		p1 = pdRate(1);
		if (!near(p0,0.0) && !near(p1,0.0) && !isInf(p0) && !isInf(p1) &&
		    !isNaN(p0) && !isNaN(p1)) {
		    npoly = 1;
		}
		phasePoly.resize(2,npoly+1);
		phasePoly.column(0) = *phaseDirField_p;
		if (npoly == 1) phasePoly.column(1) = pdRate;
	    } else {
		phasePoly.resize((*phaseDirField_p).shape());
		phasePoly = *phaseDirField_p;
		npoly = dirPoly.nrow() - 1;
	    }
	}
	if (referenceDirField_p.isAttached()) {
	    checkRef = True;
	    // old MS 1 is always accompanied by a referenceDirRateField_p
	    if (referenceDirRateField_p.isAttached()) {
		// only use this if the rate is non-zero AND non-inf AND not a NaN
		Vector<Double> rdRate(*referenceDirRateField_p);
		Double r0, r1;
		r0 = rdRate(0);
		r1 = rdRate(1);
		if (!near(r0,0.0) && !near(r1,0.0) && !isInf(r0) && !isInf(r1) &&
		    !isNaN(r0) && !isNaN(r1)) {
		    npoly = 1;
		}
		referencePoly.resize(2,npoly+1);
		referencePoly.column(0) = *referenceDirField_p;
		if (npoly == 1) referencePoly.column(1) = rdRate;
	    } else {
		referencePoly.resize((*referenceDirField_p).shape());
		referencePoly = *referenceDirField_p;
		npoly = dirPoly.nrow() - 1;
	    }
	}
	
	if (fieldIdField_p.isAttached() && *fieldIdField_p >= 0) {
	    // see if this row can be reused
	    Int thisRow = *fieldIdField_p;
	    Bool found = thisRow >= 0 && uInt(thisRow) < msField_p->nrow();
	    found = found && msFieldCols_p->sourceId()(thisRow) == sourceId;
	    if (found && codeField_p.isAttached()) {
		found = *codeField_p == msFieldCols_p->code()(thisRow);
	    }
	    if (found && nameField_p.isAttached()) {
		found = name == *nameField_p && 
		    *nameField_p == msFieldCols_p->name()(thisRow);
	    }
	    if (found && timeField_p.isAttached()) {
		found = time == *timeField_p &&
		    *timeField_p == msFieldCols_p->time()(thisRow);
	    }
	    if (found && flagRowField_p.isAttached()) {
		found = *flagRowField_p == msFieldCols_p->flagRow()(thisRow);
	    }
	    found = found && npoly == msFieldCols_p->numPoly()(thisRow);
	    found = found && allEQ(dirPoly,msFieldCols_p->delayDir()(thisRow));
	    found = found && checkPhase && 
		allEQ(phasePoly, msFieldCols_p->phaseDir()(thisRow));
	    found = found && checkRef && 
		allEQ(referencePoly, msFieldCols_p->referenceDir()(thisRow));
	    if (found) rownr_p = thisRow;
	}
	if (!found) {
	    // try and look for it 
	    *nameKey_p = name;
	    *sourceIdKey_p = sourceId;
	    *timeKey_p = time;
	    Vector<uInt> rows = index_p->getRowNumbers();
	    uInt i=0;
	    while (i<rows.nelements() && !found) {
		uInt thisRow = rows(i);
		found = npoly == msFieldCols_p->numPoly()(thisRow);
		found = found && allEQ(msFieldCols_p->delayDir()(thisRow),dirPoly);
		// that is enough for a standard SDFITS fill, the following additional
		// tests are done for the case where this SDFITS originated as a MS
		// either as version 1 or 2
		if (found && codeField_p.isAttached()) {
		    found = msFieldCols_p->code()(thisRow) == *codeField_p;
		}
		if (found && checkPhase) {
		    found = allEQ(msFieldCols_p->phaseDir()(thisRow),phasePoly);
		}
		if (found && checkRef) {
		    found = allEQ(msFieldCols_p->referenceDir()(thisRow),referencePoly);
		}
		if (found && flagRowField_p.isAttached()) {
		    found = msFieldCols_p->flagRow()(thisRow) == *flagRowField_p;
		}
		if (found) rownr_p = thisRow;
		else i++;
	    }
	}
	if (!found) {
	    // add it in
	    rownr_p = msField_p->nrow();
	    if (rownr_p ==0) {
		// set the column direction references to the value of this direction
		msFieldCols_p->delayDirMeasCol().setDescRefCode(directionRefType);
		msFieldCols_p->phaseDirMeasCol().setDescRefCode(directionRefType);
		msFieldCols_p->referenceDirMeasCol().setDescRefCode(directionRefType);
	    }
	    msField_p->addRow();
	    msFieldCols_p->name().put(rownr_p, name);
	    if (codeField_p.isAttached()) {
		msFieldCols_p->code().put(rownr_p,*codeField_p);
	    } else {
		msFieldCols_p->code().put(rownr_p,"");
	    }
	    msFieldCols_p->time().put(rownr_p, time);
	    msFieldCols_p->numPoly().put(rownr_p, npoly);
	    msFieldCols_p->delayDir().put(rownr_p, dirPoly);
	    msFieldCols_p->phaseDir().put(rownr_p, phasePoly);
	    msFieldCols_p->referenceDir().put(rownr_p, referencePoly); 
	    msFieldCols_p->sourceId().put(rownr_p, sourceId);
	    if (flagRowField_p.isAttached()) {
		msFieldCols_p->flagRow().put(rownr_p, *flagRowField_p);
	    } else {
		msFieldCols_p->flagRow().put(rownr_p, False);
	    }
	}
    }
}

void SDFieldHandler::clearAll()
{
    delete msField_p;
    msField_p = 0;

    delete msFieldCols_p;
    msFieldCols_p = 0;

    delete index_p;
    index_p = 0;

    clearRow();
}
 
void SDFieldHandler::clearRow()
{
    rownr_p = -1;
    fieldIdField_p.detach();
    codeField_p.detach();
    nameField_p.detach();
    timeField_p.detach();
    delayDirField_p.detach();
    delayDirRateField_p.detach();
    phaseDirField_p.detach();
    phaseDirRateField_p.detach();
    referenceDirField_p.detach();
    referenceDirRateField_p.detach();
    flagRowField_p.detach();
}

void SDFieldHandler::initAll(MeasurementSet &ms, Vector<Bool> &handledCols, const Record &row)
{
    msField_p = new MSField(ms.field());
    AlwaysAssert(msField_p, AipsError);

    msFieldCols_p = new MSFieldColumns(*msField_p);
    AlwaysAssert(msFieldCols_p, AipsError);

    index_p = new ColumnsIndex(*msField_p, stringToVector("NAME,SOURCE_ID,TIME"));
    AlwaysAssert(index_p, AipsError);
    nameKey_p.attachToRecord(index_p->accessKey(),"NAME");
    sourceIdKey_p.attachToRecord(index_p->accessKey(),"SOURCE_ID");
    timeKey_p.attachToRecord(index_p->accessKey(),"TIME");

    initRow(handledCols, row);
}

void SDFieldHandler::initRow(Vector<Bool> &handledCols, const Record &row)
{
    rownr_p = -1;

    if (row.fieldNumber("MAIN_FIELD_ID") >= 0 && row.dataType("MAIN_FIELD_ID") == TpInt) {
	fieldIdField_p.attachToRecord(row,"MAIN_FIELD_ID");
	handledCols(row.fieldNumber("MAIN_FIELD_ID")) = True;
    }
    if (row.fieldNumber("FIELD_CODE") >= 0 && row.dataType("FIELD_CODE") == TpString) {
	codeField_p.attachToRecord(row,"FIELD_CODE");
	handledCols(row.fieldNumber("FIELD_CODE")) = True;
    }
    if (row.fieldNumber("FIELD_NAME") >= 0 && row.dataType("FIELD_NAME") == TpString) {
	nameField_p.attachToRecord(row,"FIELD_NAME");
	handledCols(row.fieldNumber("FIELD_NAME")) = True;
    }
    if (row.fieldNumber("FIELD_TIME") >= 0 && row.dataType("FIELD_TIME") == TpDouble) {
	timeField_p.attachToRecord(row,"FIELD_TIME");
	handledCols(row.fieldNumber("FIELD_TIME")) = True;
    }
    if (row.fieldNumber("FIELD_DELAY_DIR") >= 0 && row.dataType("FIELD_DELAY_DIR") == TpArrayDouble) {
	delayDirField_p.attachToRecord(row,"FIELD_DELAY_DIR");
	handledCols(row.fieldNumber("FIELD_DELAY_DIR")) = True;
    }
    if (row.fieldNumber("FIELD_DELAY_DIR_RATE") >= 0 && row.dataType("FIELD_DELAY_DIR_RATE") == TpArrayDouble) {
	delayDirRateField_p.attachToRecord(row,"FIELD_DELAY_DIR_RATE");
	handledCols(row.fieldNumber("FIELD_DELAY_DIR_RATE")) = True;
    }
    if (row.fieldNumber("FIELD_PHASE_DIR") >= 0 && row.dataType("FIELD_PHASE_DIR") == TpArrayDouble) {
	phaseDirField_p.attachToRecord(row,"FIELD_PHASE_DIR");
	handledCols(row.fieldNumber("FIELD_PHASE_DIR")) = True;
    }
    if (row.fieldNumber("FIELD_PHASE_DIR_RATE") >= 0 && row.dataType("FIELD_PHASE_DIR_RATE") == TpArrayDouble) {
	phaseDirRateField_p.attachToRecord(row,"FIELD_PHASE_DIR_RATE");
	handledCols(row.fieldNumber("FIELD_PHASE_DIR_RATE")) = True;
    }
    if (row.fieldNumber("FIELD_REFERENCE_DIR") >= 0 && row.dataType("FIELD_REFERENCE_DIR") == TpArrayDouble) {
	referenceDirField_p.attachToRecord(row,"FIELD_REFERENCE_DIR");
	handledCols(row.fieldNumber("FIELD_REFERENCE_DIR")) = True;
    }
    if (row.fieldNumber("FIELD_REFERENCE_DIR_RATE") >= 0 && 
	row.dataType("FIELD_REFERENCE_DIR_RATE") == TpArrayDouble) {
	referenceDirRateField_p.attachToRecord(row,"FIELD_REFERENCE_DIR_RATE");
	handledCols(row.fieldNumber("FIELD_REFERENCE_DIR_RATE")) = True;
    }
    if (row.fieldNumber ("FIELD_FLAG_ROW") >= 0 &&
	row.dataType("FIELD_FLAG_ROW") == TpBool) {
	flagRowField_p.attachToRecord(row, "FIELD_FLAG_ROW");
	handledCols(row.fieldNumber("FIELD_FLAG_ROW")) = True;
    }
}

} //# NAMESPACE CASACORE - END

