//# SDPointingHandler.cc: an POINTING handler for SDFITS data  
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
#include <trial/MeasurementSets/SDPointingHandler.h>

#include <aips/MeasurementSets/NewMeasurementSet.h>
#include <aips/MeasurementSets/NewMSPointingColumns.h>
#include <aips/MeasurementSets/NewMSPointing.h>
#include <aips/Containers/Record.h>
#include <aips/Arrays/Vector.h>
#include <aips/Tables/ArrayColumn.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>
#include <aips/Utilities/String.h>
#include <aips/Measures/Stokes.h>
#include <aips/Mathematics/Math.h>
#include <aips/Measures/MeasRef.h>
#include <aips/Measures/MeasFrame.h>
#include <aips/Measures/MeasConvert.h>
#include <aips/Arrays/ArrayLogical.h>

SDPointingHandler::SDPointingHandler() 
    : msPointing_p(0), msPointingCols_p(0), antId_p(-1), time_p(0.0),
      minTime_p(0.0), maxTime_p(0.0), directionRate_p(2), name_p(""), rownr_p(-1)
{;}

SDPointingHandler::SDPointingHandler(NewMeasurementSet &ms, Vector<Bool> &handledCols,
				     const Record &row) 
    : msPointing_p(0), msPointingCols_p(0), antId_p(-1), time_p(0.0),
      minTime_p(0.0), maxTime_p(0.0), directionRate_p(2), name_p(""), rownr_p(-1)
{
    initAll(ms, handledCols, row);
}

SDPointingHandler::SDPointingHandler(const SDPointingHandler &other) 
    : msPointing_p(0), msPointingCols_p(0), antId_p(-1), time_p(0.0),
      minTime_p(0.0), maxTime_p(0.0), directionRate_p(2), name_p(""), rownr_p(-1)
{
    *this = other;
}

SDPointingHandler &SDPointingHandler::operator=(const SDPointingHandler &other)
{
    if (this != &other) {
	clearAll();
	msPointing_p = new NewMSPointing(*(other.msPointing_p));
	AlwaysAssert(msPointing_p, AipsError);
	msPointingCols_p = new NewMSPointingColumns(*msPointing_p);
	AlwaysAssert(msPointingCols_p, AipsError);
	antId_p = other.antId_p;
	time_p = other.time_p;
	minTime_p = other.minTime_p;
	maxTime_p = other.maxTime_p;
	direction_p = other.direction_p;
	directionRate_p = other.directionRate_p;
 	name_p = other.name_p;
	rownr_p = other.rownr_p;
	objectField_p = other.objectField_p;
	pointingDirRateField_p = other.pointingDirRateField_p;
    }
    return *this;
}

void SDPointingHandler::attach(NewMeasurementSet &ms, Vector<Bool> &handledCols, 
			       const Record &row)
{
    clearAll();
    initAll(ms, handledCols, row);
}

void SDPointingHandler::fill(const Record &, Int antennaId, Double time, 
			     const Vector<Double> &timeRange, const MDirection &direction,
			     const MeasFrame &frame)
{
    // don't bother unless there is something there
    if (msPointing_p) {
	String name = "";
	if (objectField_p.isAttached()) name = *objectField_p;
	Bool newRow = rownr_p < 0;
	newRow = newRow || name != name_p;
	newRow = newRow || antennaId != antId_p;
	newRow = newRow || 
	    direction.getRef() != direction_p.getRef() ||
	    direction.getValue() != direction_p.getValue();
	newRow = newRow || 
	    (time != time_p && (!timeRange(0) < maxTime_p || !near(timeRange(0),maxTime_p)));
	if (!newRow && pointingDirRateField_p.isAttached()) {
	    newRow = !allEQ(*pointingDirRateField_p, directionRate_p);
	}
	if (newRow) {
	    // a new row is in order
	    rownr_p = msPointing_p->nrow();
	    if (rownr_p == 0) {
		// set the column direction reference to the value of this direction
		dirColRef_p = direction_p.getRef();
		msPointingCols_p->directionMeasCol().setDescRefCode(dirColRef_p.getType());
		msPointingCols_p->targetMeasCol().setDescRefCode(dirColRef_p.getType());
	    }
	    msPointing_p->addRow();
	    antId_p = antennaId;
	    ntimes_p = 1;
	    time_p = time;
	    minTime_p = timeRange(0);
	    maxTime_p = timeRange(1);
	    direction_p = direction;
	    name_p = name;
	    msPointingCols_p->antennaId().put(rownr_p, antId_p);
	    msPointingCols_p->time().put(rownr_p, time_p);
	    msPointingCols_p->interval().put(rownr_p, (maxTime_p-minTime_p));
	    msPointingCols_p->name().put(rownr_p, name_p);
	    msPointingCols_p->timeOrigin().put(rownr_p, 0.0);
	    // direction is tricky
	    Int npoly = 0;
	    if (pointingDirRateField_p.isAttached()) {
		directionRate_p = *pointingDirRateField_p;
		// only add this if the rates here are non-zero AND non-inf AND not a NaN
		Double d0 = directionRate_p(0);
		Double d1 = directionRate_p(1);
	        if (!near(d0,0.0) && !near(d1,0.0) && !isInf(d0) && !isInf(d1) && !isNaN(d0) && !isNaN(d1)) {
		    npoly = 1;
		}
	    }
	    msPointingCols_p->numPoly().put(rownr_p, npoly);
	    Vector<MDirection> dirs(npoly+1);
	    dirs(0) = direction_p;
	    if (npoly == 1) {
		// assumes the direction reference is the same as for dirs(0)
		dirs(1) = MDirection(Quantum<Vector<Double> >(*pointingDirRateField_p), direction_p.getRef());
	    }
	    if (dirColRef_p != direction_p.getRef()) {
		MDirection::Ref mref(dirColRef_p);
		mref.set(frame);
		dirs(0) = MDirection::Convert(dirs(0), mref)();
		// I'm not sure how the polynomial terms convert
	    }
		    
	    msPointingCols_p->directionMeasCol().put(rownr_p, dirs);
	    // reuse the direction here
	    msPointingCols_p->targetMeasCol().put(rownr_p, dirs);
	    // assume it was tracking
	    msPointingCols_p->tracking().put(rownr_p, True);
	    // extraction the direction poly for use by the FIELD table as necessary
	    directionPoly_p = msPointingCols_p->direction()(rownr_p);
	} else {
	    // re-use this row, make sure the range now encompasses this range
	    // also average times
	    time_p = time_p*ntimes_p + time;
	    ntimes_p++;
	    time_p /= ntimes_p;
	    minTime_p = min(timeRange(0), minTime_p);
	    maxTime_p = max(timeRange(1), maxTime_p);
	    msPointingCols_p->time().put(rownr_p, time_p);
	    msPointingCols_p->interval().put(rownr_p, (maxTime_p-minTime_p));
	}
    }
}

void SDPointingHandler::clearAll()
{
    delete msPointing_p;
    msPointing_p = 0;

    delete msPointingCols_p;
    msPointingCols_p = 0;

    clearRow();
}

void SDPointingHandler::clearRow()
{
    rownr_p = -1;
    objectField_p.detach();
    pointingDirRateField_p.detach();
}

void SDPointingHandler::initAll(NewMeasurementSet &ms, Vector<Bool> &handledCols,
				const Record &row)
{
    msPointing_p = new NewMSPointing(ms.pointing());
    AlwaysAssert(msPointing_p, AipsError);

    msPointingCols_p = new NewMSPointingColumns(*msPointing_p);
    AlwaysAssert(msPointingCols_p, AipsError);

    antId_p = -1;
    ntimes_p = 0;
    time_p = minTime_p = maxTime_p = 0.0;
    direction_p = MDirection();
    name_p = "";

    initRow(handledCols, row);
}

void SDPointingHandler::initRow(Vector<Bool> &handledCols, const Record &row)
{
    rownr_p = -1;
    if (row.fieldNumber("OBJECT")) {
	objectField_p.attachToRecord(row, "OBJECT");
	handledCols(row.fieldNumber("OBJECT")) = True;
    }
    if (row.fieldNumber("FIELD_POINTING_DIR_RATE") && row.dataType("FIELD_POINTING_DIR_RATE") == TpArrayDouble) {
	pointingDirRateField_p.attachToRecord(row, "FIELD_POINTING_DIR_RATE");
	handledCols(row.fieldNumber("FIELD_POINTING_DIR_RATE")) = True;
    }
}
