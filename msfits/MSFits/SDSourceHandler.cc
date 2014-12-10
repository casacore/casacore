//# SDSourceFiller.cc: an SOURCE filler for SDFITS data  
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
#include <casacore/msfits/MSFits/SDSourceHandler.h>

#include <casacore/tables/Tables/ColumnsIndex.h>
#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/ms/MeasurementSets/MSSourceColumns.h>
#include <casacore/ms/MeasurementSets/MSSource.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/casa/Arrays/ArrayLogical.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

SDSourceHandler::SDSourceHandler() 
    : index_p(0), msSource_p(0), msSourceCols_p(0), sourceId_p(-1),
      nextSourceId_p(0), restfreq_p(-1), vframe_p(-1),
      hasTransition_p(False), hasRestFreq_p(False), hasSysVel_p(False),
      hasPosition_p(False)
{;}

SDSourceHandler::SDSourceHandler(MeasurementSet &ms, Vector<Bool> &handledCols,
				   const Record &row) 
    : index_p(0), msSource_p(0), msSourceCols_p(0), sourceId_p(-1),
      nextSourceId_p(0), restfreq_p(-1), vframe_p(-1),
      hasTransition_p(False), hasRestFreq_p(False), hasSysVel_p(False),
      hasPosition_p(False)
{
    initAll(ms, handledCols, row);
}

SDSourceHandler::SDSourceHandler(const SDSourceHandler &other) 
    : index_p(0), msSource_p(0), msSourceCols_p(0), sourceId_p(-1),
      nextSourceId_p(0), restfreq_p(-1), vframe_p(-1),
      hasTransition_p(False), hasRestFreq_p(False), hasSysVel_p(False),
      hasPosition_p(False)
{
    *this = other;
}

SDSourceHandler &SDSourceHandler::operator=(const SDSourceHandler &other)
{
    if (this != &other) {
	clearAll();
	index_p = new ColumnsIndex(*(other.index_p));
	AlwaysAssert(index_p, AipsError);
	// need to avoid the assignment operator here because we want
	// this to point to the field in index_p, not in other.index_p
	nameKey_p.attachToRecord(index_p->accessKey(),
				 MSSource::columnName(MSSource::NAME));
	codeKey_p.attachToRecord(index_p->accessKey(),
				 MSSource::columnName(MSSource::CODE));
	msSource_p = new MSSource(*(other.msSource_p));
	AlwaysAssert(msSource_p, AipsError);
	msSourceCols_p = new MSSourceColumns(*msSource_p);
	AlwaysAssert(msSourceCols_p, AipsError);
	
	sourceId_p = other.sourceId_p;
	nextSourceId_p = other.nextSourceId_p;
	
	// this should point to the same field as that in other
	restfreq_p = other.restfreq_p;
	vframe_p = other.vframe_p;
	transiti_p = other.transiti_p;
	object_p = other.object_p;
	obsmode_p = other.obsmode_p;

	hasTransition_p = other.hasTransition_p;
	hasRestFreq_p = other.hasRestFreq_p;
	hasSysVel_p = other.hasSysVel_p;
        hasPosition_p = other.hasPosition_p;

	calibrationGroupField_p = other.calibrationGroupField_p;
	pulsarIdField_p = other.pulsarIdField_p;
	timeField_p = other.timeField_p;
	intervalField_p = other.intervalField_p;
	directionField_p = other.directionField_p;
	positionField_p = other.positionField_p;
	properMotionField_p = other.properMotionField_p;
    }
    return *this;
}

void SDSourceHandler::attach(MeasurementSet &ms, Vector<Bool> &handledCols, const Record &row)
{
    clearAll();
    initAll(ms, handledCols, row);
}

void SDSourceHandler::resetRow(const Record &row) 
{
    clearRow();
    Vector<Bool> dummyHandled;
    initRow(dummyHandled, row);
}

void SDSourceHandler::fill(const Record &row, Int spectralWindowId)
{
    // don't bother unless there is something there
    if (msSource_p) {
	if (object_p.isAttached()) {
	    *nameKey_p = *object_p;
	} else {
	    *nameKey_p = "";
	}
	if (obsmode_p.isAttached()) {
	    *codeKey_p = *obsmode_p;
	} else {
	    *codeKey_p = "";
	}
	uInt rownr = 0;
	Vector<uInt> foundRows = index_p->getRowNumbers();
	Bool rowFound, sourceFound;
	rowFound = sourceFound = False;
	
	String transition = "";
	if (transiti_p.isAttached()) transition = *transiti_p;
	if (molecule_p.isAttached()) {
	    // always add the "," delimiter so that the molecule string can be recovered on the end
	    // assumes that the molecule string has no commas in it.
	    transition += ", ";
	    transition += *molecule_p;
	}
	Double restfreq = 0.0;
	if (restfreq_p >= 0) restfreq = row.asDouble(restfreq_p);
	Double sysvel = 0.0;
	if (vframe_p >= 0) {
	    sysvel = row.asDouble(vframe_p);
	}
	if (foundRows.nelements() > 0) {
	    // we have at least 1 candidate, look for a matching spectral window ID
	    uInt whichOne = 0;
	    while (!rowFound && whichOne<foundRows.nelements()) {
		// A source ID probably matches if TIME, INTERVAL, DIRECTION, POSITION, PROPER_MOTION,
		// SYSVEL, and PULSAR_ID match
		rownr = foundRows(whichOne);
		rowFound = True;
		if (rowFound && calibrationGroupField_p.isAttached()) {
		    rowFound = msSourceCols_p->calibrationGroup()(rownr) == *calibrationGroupField_p;
		}
		if (rowFound && timeField_p.isAttached()) {
		    rowFound = msSourceCols_p->time()(rownr) == *timeField_p;
		}
		if (rowFound && intervalField_p.isAttached()) {
		    rowFound = msSourceCols_p->interval()(rownr) == *intervalField_p;
		}
		if (rowFound && directionField_p.isAttached()) {
		    rowFound = allEQ(msSourceCols_p->direction()(rownr),*directionField_p);
		}
		if (rowFound && properMotionField_p.isAttached()) {
		    rowFound = allEQ(msSourceCols_p->properMotion()(rownr),*properMotionField_p);
		}
		if (rowFound && hasSysVel_p) {
		    rowFound = allEQ(msSourceCols_p->sysvel()(rownr), sysvel);
		}
                if (rowFound && hasPosition_p) {
                    rowFound = allEQ(msSourceCols_p->position()(rownr), *positionField_p);
                }
		if (rowFound && pulsarIdField_p.isAttached()) {
		    if (msSourceCols_p->pulsarId().isNull()) rowFound = !(*pulsarIdField_p>=0);
		    else rowFound = msSourceCols_p->pulsarId()(rownr) == *pulsarIdField_p;
		}
		// if we're here, the source ID is probably okay
		if (rowFound) {
		    sourceFound = True;
		    sourceId_p = msSourceCols_p->sourceId()(rownr);
		    // we still might not have the right row, though
		    rowFound = spectralWindowId == msSourceCols_p->spectralWindowId()(foundRows(whichOne));
		    if (rowFound && hasTransition_p) {
			rowFound = allEQ(msSourceCols_p->transition()(rownr), transition);
		    }
		    if (rowFound && hasRestFreq_p) {
			rowFound = allEQ(msSourceCols_p->restFrequency()(rownr), restfreq);
		    }
		}
		if (!rowFound) whichOne++;
		if (rowFound && hasSysVel_p) {
		    rowFound = allEQ(msSourceCols_p->sysvel()(rownr), sysvel);
		}
		if (rowFound && hasPosition_p) {
		    rowFound = allEQ(msSourceCols_p->position()(rownr), *positionField_p);
		}
		if (rowFound && pulsarIdField_p.isAttached()) {
		    if (msSourceCols_p->pulsarId().isNull()) rowFound = !(*pulsarIdField_p>=0);
		    else rowFound = msSourceCols_p->pulsarId()(rownr) == *pulsarIdField_p;
		}
		// if we're here, the source ID is probably okay
		if (rowFound) {
		    sourceFound = True;
		    sourceId_p = msSourceCols_p->sourceId()(rownr);
		    // we still might not have the right row, though
		    rowFound = spectralWindowId == msSourceCols_p->spectralWindowId()(foundRows(whichOne));
		    if (rowFound && hasTransition_p) {
			rowFound = allEQ(msSourceCols_p->transition()(rownr), transition);
		    }
		    if (rowFound && hasRestFreq_p) {
			rowFound = allEQ(msSourceCols_p->restFrequency()(rownr), restfreq);
		    }
		}
		if (!rowFound) whichOne++;
	    }
	}
	if (!rowFound) {
	    // we need to add one
	    rownr = msSource_p->nrow();
	    msSource_p->addRow();
	    if (!sourceFound) sourceId_p = nextSourceId_p++;

	    msSourceCols_p->sourceId().put(rownr,sourceId_p);
	    if (timeField_p.isAttached()) {
		msSourceCols_p->time().put(rownr,*timeField_p);
	    } else {
		msSourceCols_p->time().put(rownr,0.0);
	    }
	    if (intervalField_p.isAttached()) {
		msSourceCols_p->interval().put(rownr,*intervalField_p);
	    } else {
		msSourceCols_p->interval().put(rownr,0.0);
	    }
	    msSourceCols_p->spectralWindowId().put(rownr, spectralWindowId);
	    if (hasRestFreq_p || hasTransition_p || hasSysVel_p) {
		msSourceCols_p->numLines().put(rownr, 1);
	    } else {
		msSourceCols_p->numLines().put(rownr, 0);
	    }
	    if (hasTransition_p) {
		msSourceCols_p->transition().put(rownr,Vector<String>(1,transition));
	    }
	    if (hasRestFreq_p) {
		msSourceCols_p->restFrequency().put(rownr,Vector<Double>(1,restfreq));
	    }
	    if (hasSysVel_p) {
		msSourceCols_p->sysvel().put(rownr,Vector<Double>(1,sysvel));
	    }
            if (hasPosition_p) {
		msSourceCols_p->position().put(rownr,*positionField_p);
            }
	    String name = "";
	    if (object_p.isAttached()) name = *object_p;
	    msSourceCols_p->name().put(rownr,name);
	    if (calibrationGroupField_p.isAttached()) {
		msSourceCols_p->calibrationGroup().put(rownr, *calibrationGroupField_p);
	    } else {
		msSourceCols_p->calibrationGroup().put(rownr,-1);
	    }
	    String code = "";
	    if (obsmode_p.isAttached()) code = *obsmode_p;
	    msSourceCols_p->code().put(rownr,code);
	    if (directionField_p.isAttached()) {
		msSourceCols_p->direction().put(rownr,*directionField_p);
	    } else {
		msSourceCols_p->direction().put(rownr,Vector<Double>(2,0.0));
	    }
	    if (properMotionField_p.isAttached()) {
		msSourceCols_p->properMotion().put(rownr,*properMotionField_p);
	    } else {
		msSourceCols_p->properMotion().put(rownr,Vector<Double>(2,0.0));
	    }
	    if (pulsarIdField_p.isAttached()) {
		if (*pulsarIdField_p >= 0) {
		    if (msSourceCols_p->pulsarId().isNull()) {
			// add this column
			delete msSourceCols_p;
			msSourceCols_p = 0;
			TableDesc td;
			MSSource::addColumnToDesc(td, MSSource::PULSAR_ID);
			msSource_p->addColumn(td[0]);
			msSourceCols_p = new MSSourceColumns(*msSource_p);
			AlwaysAssert(msSourceCols_p, AipsError);
			msSourceCols_p->pulsarId().put(rownr, *pulsarIdField_p);
		    } else {
			msSourceCols_p->pulsarId().put(rownr, *pulsarIdField_p);
		    }
		}
	    }
	    // transition, rest_frequency, sysvel are inserted outside this loop
	} else {
	    // set the source ID to what was actually found
	  sourceId_p = msSourceCols_p->sourceId()(rownr);
	}
    }
}

void SDSourceHandler::clearAll()
{
    delete index_p;
    index_p = 0;

    delete msSource_p;
    msSource_p = 0;

    delete msSourceCols_p;
    msSourceCols_p = 0;

    sourceId_p = -1;
    nextSourceId_p = 0;

    clearRow();
}

void SDSourceHandler::clearRow()
{
    transiti_p.detach();
    molecule_p.detach();
    object_p.detach();
    obsmode_p.detach();
    restfreq_p = vframe_p = -1;
    hasTransition_p = hasRestFreq_p = hasSysVel_p = hasPosition_p = False;
    calibrationGroupField_p.detach();
    pulsarIdField_p.detach();
    timeField_p.detach();
    intervalField_p.detach();
    directionField_p.detach();
    positionField_p.detach();
    properMotionField_p.detach();
}

void SDSourceHandler::initAll(MeasurementSet &ms, Vector<Bool> &handledCols, 
			      const Record &row)
{
    msSource_p = new MSSource(ms.source());
    AlwaysAssert(msSource_p, AipsError);

    // things in row might trigger the need for optional columns
    initRow(handledCols, row);

    TableDesc td;
    if (restfreq_p >= 0) {
	MSSource::addColumnToDesc(td,MSSource::REST_FREQUENCY);
	hasRestFreq_p = True;
    }
    if (vframe_p >= 0) {
	MSSource::addColumnToDesc(td,MSSource::SYSVEL);
	hasSysVel_p = True;
    }
    if (transiti_p.isAttached() || molecule_p.isAttached()) {
	MSSource::addColumnToDesc(td, MSSource::TRANSITION);
	hasTransition_p = True;
    }
    if (positionField_p.isAttached()) {
        MSSource::addColumnToDesc(td,MSSource::POSITION);
        hasPosition_p = True;
    }
    // and add these columns in, if there any
    for (uInt i=0;i<td.ncolumn();i++) {
	msSource_p->addColumn(td[i],"StandardStMan", False);
    }

    msSourceCols_p = new MSSourceColumns(*msSource_p);
    AlwaysAssert(msSourceCols_p, AipsError);

    Vector<String> indexCols(2);
    indexCols(0) = MSSource::columnName(MSSource::NAME);
    indexCols(1) = MSSource::columnName(MSSource::CODE);
    index_p = new ColumnsIndex(*msSource_p, indexCols);
    AlwaysAssert(index_p, AipsError);
    
    nameKey_p.attachToRecord(index_p->accessKey(),
			     MSSource::columnName(MSSource::NAME));
    codeKey_p.attachToRecord(index_p->accessKey(),
			     MSSource::columnName(MSSource::CODE));
    sourceId_p = -1;
    nextSourceId_p = 0;
}

void SDSourceHandler::initRow(Vector<Bool> &handledCols, const Record &row)
{
    AlwaysAssert(handledCols.nelements()==row.description().nfields(), AipsError);

    restfreq_p = row.fieldNumber("RESTFREQ");
    if (restfreq_p >= 0) handledCols(restfreq_p) = True;

    // VFRAME == SYSVEL
    vframe_p = row.fieldNumber("VFRAME");
    if (vframe_p >= 0) handledCols(vframe_p) = True;
    
    Int tmp = row.fieldNumber("TRANSITI");
    if (tmp >= 0) {
	transiti_p.attachToRecord(row, tmp);
	handledCols(tmp) = True;
    }
    tmp = row.fieldNumber("MOLECULE");
    if (tmp >= 0) {
	molecule_p.attachToRecord(row, tmp);
	handledCols(tmp) = True;
    }

    tmp = row.fieldNumber("OBJECT");
    if (tmp >= 0) {
	object_p.attachToRecord(row, tmp);
	handledCols(tmp) = True;
    }
 
    tmp = row.fieldNumber("OBSMODE");
    if (tmp >= 0) {
	obsmode_p.attachToRecord(row, tmp);
	handledCols(tmp) = True;
    }

    tmp = row.fieldNumber("SOURCE_CALIBRATION_GROUP");
    if (tmp >= 0 && row.dataType(tmp) == TpInt) {
	calibrationGroupField_p.attachToRecord(row, tmp);
	handledCols(tmp) = True;
    }    

    tmp = row.fieldNumber("SOURCE_DIRECTION");
    if (tmp >= 0 && row.dataType(tmp) == TpArrayDouble) {
	directionField_p.attachToRecord(row, tmp);
	handledCols(tmp) = True;
    }    

    tmp = row.fieldNumber("SOURCE_INTERVAL");
    if (tmp >= 0 && row.dataType(tmp) == TpDouble) {
	intervalField_p.attachToRecord(row, tmp);
	handledCols(tmp) = True;
    }    

    tmp = row.fieldNumber("SOURCE_POSITION");
    if (tmp >= 0 && row.dataType(tmp) == TpArrayDouble) {
	positionField_p.attachToRecord(row, tmp);
	handledCols(tmp) = True;
    }    

    tmp = row.fieldNumber("SOURCE_PROPER_MOTION");
    if (tmp >= 0 && row.dataType(tmp) == TpArrayDouble) {
	properMotionField_p.attachToRecord(row, tmp);
	handledCols(tmp) = True;
    }    

    tmp = row.fieldNumber("SOURCE_TIME");
    if (tmp >= 0 && row.dataType(tmp) == TpDouble) {
	timeField_p.attachToRecord(row, tmp);
	handledCols(tmp) = True;
    }    

    tmp = row.fieldNumber("MAIN_PULSAR_ID");
    if (tmp >= 0 && row.dataType(tmp) == TpInt) {
	pulsarIdField_p.attachToRecord(row, tmp);
	handledCols(tmp) = True;
    }

    // these are ignored - they were produced via ms2sdfits with MS 1 and they contain
    // duplicate information found in other standard SDFITS columns
    tmp = row.fieldNumber("SOURCE_SYSVEL");
    if (tmp >= 0) handledCols(tmp) = True;
    tmp = row.fieldNumber("SPECTRAL_WINDOW_REST_FREQUENCY");
    if (tmp >= 0) handledCols(tmp) = True;
}

} //# NAMESPACE CASACORE - END

