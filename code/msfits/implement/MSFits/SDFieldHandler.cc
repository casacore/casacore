//# SDFieldHandler.cc: a FIELD handler for SDFITS data  
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
#include <trial/MeasurementSets/SDFieldHandler.h>

#include <aips/MeasurementSets/NewMeasurementSet.h>
#include <aips/MeasurementSets/NewMSFieldColumns.h>
#include <aips/MeasurementSets/NewMSField.h>
#include <aips/Containers/Record.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>
#include <aips/Utilities/String.h>
#include <aips/Arrays/ArrayLogical.h>

SDFieldHandler::SDFieldHandler() 
    : msField_p(0), msFieldCols_p(0), rownr_p(-1)
{;}

SDFieldHandler::SDFieldHandler(NewMeasurementSet &ms, Vector<Bool> &handledCols, const Record &row) 
    : msField_p(0), msFieldCols_p(0), rownr_p(-1)
{
    initAll(ms, handledCols, row);
}

SDFieldHandler::SDFieldHandler(const SDFieldHandler &other) 
    : msField_p(0), msFieldCols_p(0), rownr_p(-1)
{
    *this = other;
}

SDFieldHandler &SDFieldHandler::operator=(const SDFieldHandler &other)
{
    if (this != &other) {
	clearAll();
	msField_p = new NewMSField(*(other.msField_p));
	AlwaysAssert(msField_p, AipsError);
	msFieldCols_p = new NewMSFieldColumns(*msField_p);
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
    }
    return *this;
}

void SDFieldHandler::attach(NewMeasurementSet &ms, Vector<Bool> &handledCols, const Record &row)
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

void SDFieldHandler::fill(const Record &row, const String &name, Int directionRefType,
			  const Matrix<Double> &directionPoly, Double time, Int sourceId)
{
    // don't bother unless there is something there
    if (msField_p) {
	// almost always add a new row
	Bool canReuse = False;
	Matrix<Double> dirPoly = directionPoly;
	Matrix<Double> phasePoly = directionPoly;
	Matrix<Double> referencePoly = directionPoly;
	Int npoly = dirPoly.nrow() - 1;
	if (fieldIdField_p.isAttached() && *fieldIdField_p >= 0) {
	    // see if this row can be reused
	    Int thisRow = *fieldIdField_p;
	    Bool canReuse = thisRow >= 0 && uInt(thisRow) < msField_p->nrow();
	    canReuse = canReuse && msFieldCols_p->sourceId()(thisRow) == sourceId;
	    if (canReuse && codeField_p.isAttached()) {
		canReuse = canReuse && *codeField_p == msFieldCols_p->code()(thisRow);
	    }
	    if (canReuse && nameField_p.isAttached()) {
		canReuse = canReuse && name == *nameField_p && 
		    *nameField_p == msFieldCols_p->name()(thisRow);
	    }
	    if (canReuse && timeField_p.isAttached()) {
		canReuse = canReuse && time == *timeField_p &&
		    *timeField_p == msFieldCols_p->time()(thisRow);
	    }
	    // these checks serve two purposes, to see if the data can be reused by also as
	    // a first check on the contents of the *rateFields and the npoly that they imply.
	    if (delayDirField_p.isAttached()) {
		// old MS version 1 is always accompanied by delayDirRateField_p
		if (delayDirRateField_p.isAttached()) {
		    // only use this if the rates here are non-zero AND non-inf AND not a NaN
		    Vector<Double> ddRate_p(*delayDirRateField_p);
		    Double d0, d1;
		    d0 = ddRate_p(0);
		    d1 = ddRate_p(1);
		    if (!near(d0,0.0) && !near(d1,0.0) && !isInf(d0) && !isInf(d1) && !isNaN(d0) && !isNaN(d1)) {
			npoly = 1;
		    }
		    if (canReuse) {
			Matrix<Double> thisDelayDirPoly = msFieldCols_p->delayDir()(thisRow);
			canReuse = canReuse && allEQ(*delayDirField_p, thisDelayDirPoly.column(0)) &&
			    allEQ(*delayDirField_p, dirPoly.column(0));
			if (npoly == 1) {
			    canReuse = canReuse && thisDelayDirPoly.nrow() == 2;
			    canReuse = canReuse && allEQ(*delayDirRateField_p, thisDelayDirPoly.column(1));
			}
		    }
		} else {
		    // assume its from MS 2, already a poly
		    npoly = (*delayDirField_p).shape()(0) - 1;
		    canReuse = canReuse && allEQ(*delayDirField_p, msFieldCols_p->delayDir()(thisRow));
		}
	    }
	    // check npoly value, now that it is known
	    canReuse = canReuse && npoly == msFieldCols_p->numPoly()(thisRow);
	    if (phaseDirField_p.isAttached()) {
		// old MS version 1 is always accompanied by phaseDirRateField_p
		if (phaseDirRateField_p.isAttached()) {
		    // only use this if the rates here are non-zero AND non-inf AND not a NaN
		    Vector<Double> pdRate_p(*phaseDirRateField_p);
		    Double d0, d1;
		    d0 = pdRate_p(0);
		    d1 = pdRate_p(1);
		    if (!near(d0,0.0) && !near(d1,0.0) && !isInf(d0) && !isInf(d1) && !isNaN(d0) && !isNaN(d1)) {
			npoly = 1;
		    }
		    if (canReuse) {
			Matrix<Double> thisPhaseDirPoly = msFieldCols_p->phaseDir()(thisRow);
			canReuse = canReuse && allEQ(*phaseDirField_p, thisPhaseDirPoly.column(0)) &&
			    allEQ(*phaseDirField_p, phasePoly.column(0));
			if (npoly == 1) {
			    canReuse = canReuse && thisPhaseDirPoly.nrow() == 2;
			    canReuse = canReuse && allEQ(*phaseDirRateField_p, thisPhaseDirPoly.column(1));
			}
		    }
		} else {
		    // assume its from MS 2, already a poly
		    canReuse = canReuse && npoly == (*phaseDirField_p).shape()(0) - 1;
		    canReuse = canReuse && allEQ(*phaseDirField_p, msFieldCols_p->phaseDir()(thisRow));
		}
	    }
	    if (referenceDirField_p.isAttached()) {
		// old MS version 1 is always accompanied by referenceDirRateField_p
		if (referenceDirRateField_p.isAttached()) {
		    // only use this if the rates here are non-zero AND non-inf AND not a NaN
		    Vector<Double> rdRate_p(*referenceDirRateField_p);
		    Double d0, d1;
		    d0 = rdRate_p(0);
		    d1 = rdRate_p(1);
		    if (!near(d0,0.0) && !near(d1,0.0) && !isInf(d0) && !isInf(d1) && !isNaN(d0) && !isNaN(d1)) {
			npoly = 1;
		    }
		    if (canReuse) {
			Matrix<Double> thisReferenceDirPoly = msFieldCols_p->referenceDir()(thisRow);
			canReuse = canReuse && allEQ(*referenceDirField_p, thisReferenceDirPoly.column(0)) &&
			    allEQ(*referenceDirField_p, referencePoly.column(0));
			if (npoly == 1) {
			    canReuse = canReuse && thisReferenceDirPoly.nrow() == 2;
			    canReuse = canReuse && allEQ(*referenceDirRateField_p, thisReferenceDirPoly.column(1));
			}
		    }
		} else {
		    // assume its from MS 2, already a poly
		    canReuse = canReuse && npoly == (*referenceDirField_p).shape()(0) - 1;
		    canReuse = canReuse && allEQ(*referenceDirField_p, msFieldCols_p->referenceDir()(thisRow));
		}
	    }
	    if (canReuse) rownr_p = thisRow;
	}
	if (!canReuse) {
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
	    // any adjustments to dirPoly
	    if (delayDirField_p.isAttached()) {
		// MS1 or 2?
		if (delayDirRateField_p.isAttached()) {
		    // a valid rate?
		    if (npoly == 1) {
			// add it in
			Vector<Double> dir = dirPoly.column(0);
			dirPoly.resize(2,2);
			dirPoly.column(0) = dir;
			dirPoly.column(1) = *delayDirRateField_p;
		    } // otherwise, use the direction poly as given
		} else {
		    // must be MS 2, use it as it is
		    dirPoly.resize((*delayDirField_p).shape());
		    dirPoly = *delayDirField_p;
		}
	    } 
	    msFieldCols_p->delayDir().put(rownr_p, dirPoly);
	    // any adjustments to phasePoly
	    if (phaseDirField_p.isAttached()) {
		// MS1 or 2?
		if (phaseDirRateField_p.isAttached()) {
		    // a valid rate?
		    if (npoly == 1) {
			// add it in
			Vector<Double> dir = dirPoly.column(0);
			dirPoly.resize(2,2);
			dirPoly.column(0) = dir;
			dirPoly.column(1) = *phaseDirRateField_p;
		    } // otherwise, use the direction poly as given
		} else {
		    // must be MS 2, use it as it is
		    dirPoly.resize((*phaseDirField_p).shape());
		    dirPoly = *phaseDirField_p;
		}
	    } 
	    msFieldCols_p->phaseDir().put(rownr_p, phasePoly);
	    // any adjustments to referencePoly
	    if (referenceDirField_p.isAttached()) {
		// MS1 or 2?
		if (referenceDirRateField_p.isAttached()) {
		    // a valid rate?
		    if (npoly == 1) {
			// add it in
			Vector<Double> dir = dirPoly.column(0);
			dirPoly.resize(2,2);
			dirPoly.column(0) = dir;
			dirPoly.column(1) = *referenceDirRateField_p;
		    } // otherwise, use the direction poly as given
		} else {
		    // must be MS 2, use it as it is
		    dirPoly.resize((*referenceDirField_p).shape());
		    dirPoly = *referenceDirField_p;
		}
	    } 
	    msFieldCols_p->referenceDir().put(rownr_p, referencePoly); 
	    msFieldCols_p->sourceId().put(rownr_p, sourceId);
	}
    }
}

void SDFieldHandler::clearAll()
{
    delete msField_p;
    msField_p = 0;

    delete msFieldCols_p;
    msFieldCols_p = 0;

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
}

void SDFieldHandler::initAll(NewMeasurementSet &ms, Vector<Bool> &handledCols, const Record &row)
{
    msField_p = new NewMSField(ms.field());
    AlwaysAssert(msField_p, AipsError);

    msFieldCols_p = new NewMSFieldColumns(*msField_p);
    AlwaysAssert(msFieldCols_p, AipsError);

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
}
