//# SDDataDescHandler.cc: a DATA_DESCRIPTION handler for SDFITS data  
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
#include <casacore/msfits/MSFits/SDDataDescHandler.h>

#include <casacore/tables/Tables/ColumnsIndex.h>
#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/ms/MeasurementSets/MSDataDescColumns.h>
#include <casacore/ms/MeasurementSets/MSDataDescription.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

SDDataDescHandler::SDDataDescHandler() 
    : index_p(0), msDataDesc_p(0), msDataDescCols_p(0), rownr_p(-1)
{;}

SDDataDescHandler::SDDataDescHandler(MeasurementSet &ms) 
    : index_p(0), msDataDesc_p(0), msDataDescCols_p(0), rownr_p(-1)
{
    initAll(ms);
}

SDDataDescHandler::SDDataDescHandler(const SDDataDescHandler &other) 
    : index_p(0), msDataDesc_p(0), msDataDescCols_p(0), rownr_p(-1)
{
    *this = other;
}

SDDataDescHandler &SDDataDescHandler::operator=(const SDDataDescHandler &other)
{
    if (this != &other) {
	clearAll();
	index_p = new ColumnsIndex(*(other.index_p));
	AlwaysAssert(index_p, AipsError);
	// need to avoid the assignment operator here because we want
	// this to point to the field in index_p, not in other.index_p
	spwinIdKey_p.attachToRecord(index_p->accessKey(),
		     MSDataDescription::columnName(MSDataDescription::SPECTRAL_WINDOW_ID));
	polIdKey_p.attachToRecord(index_p->accessKey(),
		     MSDataDescription::columnName(MSDataDescription::POLARIZATION_ID));
	msDataDesc_p = new MSDataDescription(*(other.msDataDesc_p));
	AlwaysAssert(msDataDesc_p, AipsError);
	msDataDescCols_p = new MSDataDescColumns(*msDataDesc_p);
	AlwaysAssert(msDataDescCols_p, AipsError);
	rownr_p = other.rownr_p;
    }
    return *this;
}

void SDDataDescHandler::attach(MeasurementSet &ms, Vector<Bool> &, const Record &)
{
    clearAll();
    initAll(ms);
}

void SDDataDescHandler::fill(const Record &, Int spwinId, Int polId)
{
    // don't bother unless there is something there
    if (msDataDesc_p) {
	*spwinIdKey_p = spwinId;
	*polIdKey_p = polId;
	Bool found = False;
	uInt foundRow = index_p->getRowNumber(found);
	if (found) {
	    // we have a winner
	    rownr_p = foundRow;
	} else {
	    // we need to add one
	    rownr_p = msDataDesc_p->nrow();
	    msDataDesc_p->addRow();
	    msDataDescCols_p->spectralWindowId().put(rownr_p, *spwinIdKey_p);
	    msDataDescCols_p->polarizationId().put(rownr_p, *polIdKey_p);
	    msDataDescCols_p->flagRow().put(rownr_p, False);
	}
    }
}

void SDDataDescHandler::clearAll()
{
    delete index_p;
    index_p = 0;

    delete msDataDesc_p;
    msDataDesc_p = 0;

    delete msDataDescCols_p;
    msDataDescCols_p = 0;
}

void SDDataDescHandler::initAll(MeasurementSet &ms)
{
    msDataDesc_p = new MSDataDescription(ms.dataDescription());
    AlwaysAssert(msDataDesc_p, AipsError);

    msDataDescCols_p = new MSDataDescColumns(*msDataDesc_p);
    AlwaysAssert(msDataDescCols_p, AipsError);

    Vector<String> indexCols(2);
    indexCols(0) = MSDataDescription::columnName(MSDataDescription::SPECTRAL_WINDOW_ID);
    indexCols(1) = MSDataDescription::columnName(MSDataDescription::POLARIZATION_ID);
    index_p = new ColumnsIndex(*msDataDesc_p, indexCols);
    AlwaysAssert(index_p, AipsError);
    
    spwinIdKey_p.attachToRecord(index_p->accessKey(),
	       MSDataDescription::columnName(MSDataDescription::SPECTRAL_WINDOW_ID));
    polIdKey_p.attachToRecord(index_p->accessKey(),
	       MSDataDescription::columnName(MSDataDescription::POLARIZATION_ID));
    rownr_p = -1;
    // nothing depends directly on the row, no columns are handled here
}

} //# NAMESPACE CASACORE - END

