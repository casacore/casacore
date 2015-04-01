//# SDHistoryHandler.cc: an HISTORY handler for SDFITS data  
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

//# Includes
#include <casacore/msfits/MSFits/SDHistoryHandler.h>

#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/ms/MeasurementSets/MSHistoryColumns.h>
#include <casacore/ms/MeasurementSets/MSHistory.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/fits/FITS/FITSDateUtil.h>
#include <casacore/measures/Measures/MEpoch.h>
#include <casacore/casa/Quanta/MVTime.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

SDHistoryHandler::SDHistoryHandler() 
    : msHis_p(0), msHisCols_p(0)
{;}

SDHistoryHandler::SDHistoryHandler(MeasurementSet &ms, const Vector<Bool> &handledCols,
				   const Record &row) 
    : msHis_p(0), msHisCols_p(0)
{
    initAll(ms, handledCols, row);
}

SDHistoryHandler::SDHistoryHandler(const SDHistoryHandler &other) 
    : msHis_p(0), msHisCols_p(0)
{
    *this = other;
}

SDHistoryHandler &SDHistoryHandler::operator=(const SDHistoryHandler &other)
{
    if (this != &other) {
	clearAll();
	msHis_p = new MSHistory(*(other.msHis_p));
	AlwaysAssert(msHis_p, AipsError);
	msHisCols_p = new MSHistoryColumns(*msHis_p);
	AlwaysAssert(msHisCols_p, AipsError);
	timesys_p = other.timesys_p;
    }
    return *this;
}

void SDHistoryHandler::attach(MeasurementSet &ms, Vector<Bool> &handledCols, 
			      const Record &row)
{
    clearAll();
    initAll(ms, handledCols, row);
}

void SDHistoryHandler::fill(const Record &, Int observationId,
			    const String &message, const String &priority)
{
    // this always just fills as is
    // don't bother unless there is something there
    if (msHis_p) {
	uInt rownr = msHis_p->nrow();
	msHis_p->addRow();
	// get the current time
	Quantity now;
	MVTime::read(now,"today");
	MEpoch::Types timesys = MEpoch::UTC;
	// see if there is a timesys pointer which might override this
	if (timesys_p.isAttached()) {
	    // there doesn't seem to be a simpler way
	    MVTime dummy;
	    if (!FITSDateUtil::fromFITS(dummy, timesys, "2000-01-01", *timesys_p)) {
		timesys = MEpoch::UTC;
	    }
	}
	msHisCols_p->timeMeas().put(rownr, MEpoch(now, timesys));
	msHisCols_p->observationId().put(rownr, observationId);
	msHisCols_p->message().put(rownr, message);
	msHisCols_p->priority().put(rownr, priority);
	msHisCols_p->objectId().put(rownr, -1);
	msHisCols_p->application().put(rownr, "");
	msHisCols_p->cliCommand().put(rownr, Vector<String>(1));
	msHisCols_p->appParams().put(rownr, Vector<String>(1));
    }
}

void SDHistoryHandler::clearAll()
{
    delete msHis_p;
    msHis_p = 0;
    
    delete msHisCols_p;
    msHisCols_p = 0;
    
    clearRow();
}

void SDHistoryHandler::clearRow()
{
    timesys_p.detach();
}

void SDHistoryHandler::initAll(MeasurementSet &ms, const Vector<Bool> &handledCols,
			       const Record &row)
{
    msHis_p = new MSHistory(ms.history());
    AlwaysAssert(msHis_p, AipsError);

    msHisCols_p = new MSHistoryColumns(*msHis_p);
    AlwaysAssert(msHisCols_p, AipsError);

    initRow(handledCols, row);
}

void SDHistoryHandler::initRow(const Vector<Bool> &, const Record &row)
{
    // look for a TIMESYS field in row
    if (row.fieldNumber("TIMESYS") >= 0) {
	timesys_p.attachToRecord(row, "TIMESYS");
    }
}

} //# NAMESPACE CASACORE - END

