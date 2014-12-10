//# MSSpectralWindow.cc: The MeasurementSet FREQUENCY Table
//# Copyright (C) 1996,1998,1999,2000
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

#include <casacore/ms/MeasurementSets/MSSpectralWindow.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/Tables/ColDescSet.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/DataMan/StManAipsIO.h>
#include <casacore/tables/DataMan/ForwardCol.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

MSSpectralWindow::MSSpectralWindow():hasBeenDestroyed_p(True) { }

MSSpectralWindow::MSSpectralWindow(const String &tableName, TableOption option) 
    : MSTable<PredefinedColumns,
      PredefinedKeywords>(tableName, option),hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSSpectralWindow(String &, TableOption) - "
			 "table is not a valid MSSpectralWindow"));
}

MSSpectralWindow::MSSpectralWindow(const String& tableName, const String &tableDescName,
			       TableOption option)
    : MSTable<PredefinedColumns,
      PredefinedKeywords>(tableName, tableDescName,option),
      hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSSpectralWindow(String &, String &, TableOption) - "
			 "table is not a valid MSSpectralWindow"));
}

MSSpectralWindow::MSSpectralWindow(SetupNewTable &newTab, uInt nrrow,
			       Bool initialize)
    : MSTable<PredefinedColumns,
      PredefinedKeywords>(newTab, nrrow, initialize), 
      hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSSpectralWindow(SetupNewTable &, uInt, Bool) - "
			 "table is not a valid MSSpectralWindow"));
}

MSSpectralWindow::MSSpectralWindow(const Table &table)
    : MSTable<PredefinedColumns,
      PredefinedKeywords>(table), hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSSpectralWindow(const Table &) - "
			 "table is not a valid MSSpectralWindow"));
}

MSSpectralWindow::MSSpectralWindow(const MSSpectralWindow &other)
    : MSTable<PredefinedColumns,
      PredefinedKeywords>(other), 
      hasBeenDestroyed_p(False)
{
    // verify that other is valid
    if (&other != this) 
	if (! validate(this->tableDesc()))
	    throw (AipsError("MSSpectralWindow(const MSSpectralWindow &) - "
			     "table is not a valid MSSpectralWindow"));
}

MSSpectralWindow::~MSSpectralWindow()
{
// check to make sure that this MSSpectralWindow is still valid
    if (!hasBeenDestroyed_p &&  !validate()) {
	hasBeenDestroyed_p = True;
	// the table is otherwise OK, so ensure that it is written if necessary
	this->flush();
	// now we can thrown an exception
	throw (AipsError("~MSSpectralWindow() - "
			 "Table written is not a valid MSSpectralWindow"));
    }
    // if we get to here, let nature take its course
    // this should not be necessary, but do it for insurance anyway
    hasBeenDestroyed_p = True;
}


MSSpectralWindow& MSSpectralWindow::operator=(const MSSpectralWindow &other)
{
    if (&other != this) {
	MSTable<PredefinedColumns,
	PredefinedKeywords>::operator=(other);
	hasBeenDestroyed_p=other.hasBeenDestroyed_p;
    }
    return *this;
}

void MSSpectralWindow::init()
{
    if (! columnMap_p.ndefined()) {
      // the PredefinedColumns
      // 
      // ASSOC_NATURE
      colMapDef(ASSOC_NATURE,"ASSOC_NATURE", TpArrayString,
		"Nature of association with other spectral window","","");
      // ASSOC_SPW_ID
      colMapDef(ASSOC_SPW_ID,"ASSOC_SPW_ID",TpArrayInt,
		"Associated spectral window id","","");
      // BBC_NO
      colMapDef(BBC_NO,"BBC_NO",TpInt,
		"Baseband converter number","","");
      // BBC_SIDEBAND
      colMapDef(BBC_SIDEBAND,"BBC_SIDEBAND",TpInt,
		"BBC sideband","","");
      // CHAN_FREQ
      colMapDef(CHAN_FREQ,"CHAN_FREQ", TpArrayDouble,
		"Center frequencies for each channel in the data matrix",
		"Hz","Frequency");
      // CHAN_WIDTH
      colMapDef(CHAN_WIDTH,"CHAN_WIDTH",TpArrayDouble,
		"Channel width for each channel","Hz","");
      // DOPPLER_ID
      colMapDef(DOPPLER_ID,"DOPPLER_ID",TpInt,
		"Doppler Id, points to DOPPLER table","","");
      // EFFECTIVE_BW
      colMapDef(EFFECTIVE_BW,"EFFECTIVE_BW",TpArrayDouble,
		"Effective noise bandwidth of each channel","Hz","");
      // FLAG_ROW
      colMapDef(FLAG_ROW,"FLAG_ROW",TpBool,
		"Row flag","","");
      // FREQ_GROUP
      colMapDef(FREQ_GROUP,"FREQ_GROUP",TpInt,
		"Frequency group","","");
      // FREQ_GROUP_NAME
      colMapDef(FREQ_GROUP_NAME,"FREQ_GROUP_NAME",TpString,
		"Frequency group name","","");
      // IF_CONV_CHAIN
      colMapDef(IF_CONV_CHAIN, "IF_CONV_CHAIN", TpInt,
		"The IF conversion chain number","","");
      // MEAS_FREQ_REF
      colMapDef(MEAS_FREQ_REF,"MEAS_FREQ_REF",TpInt,
		"Frequency Measure reference","","");
      // NAME
      colMapDef(NAME,"NAME",TpString,
		"Spectral window name","","");
      // NET_SIDEBAND
      colMapDef(NET_SIDEBAND,"NET_SIDEBAND",TpInt,
		"Net sideband","","");
      // NUM_CHAN
      colMapDef(NUM_CHAN, "NUM_CHAN", TpInt,
		"Number of spectral channels","","");
      // RECEIVER_ID
      colMapDef(RECEIVER_ID,"RECEIVER_ID",TpInt,
		"Receiver Id for this spectral window","","");
      // REF_FREQUENCY
      colMapDef(REF_FREQUENCY, "REF_FREQUENCY", TpDouble,
		"The reference frequency",
		"Hz","Frequency");
      // RESOLUTION
      colMapDef(RESOLUTION, "RESOLUTION", TpArrayDouble,
		"The effective noise bandwidth for each channel",
		"Hz","");
      // TOTAL_BANDWIDTH
      colMapDef(TOTAL_BANDWIDTH, "TOTAL_BANDWIDTH", TpDouble,
		"The total bandwidth for this window","Hz","");
      // PredefinedKeywords
      
      // init requiredTableDesc
	TableDesc requiredTD;
	// all required keywords
	uInt i;
	for (i = UNDEFINED_KEYWORD+1;
	     i <= NUMBER_PREDEFINED_KEYWORDS; i++) {
	    addKeyToDesc(requiredTD, PredefinedKeywords(i));
	}
	
	// all required columns 

	// set up the TableMeasure columns with variable reference
	// first add the variable ref column
	addColumnToDesc(requiredTD, MEAS_FREQ_REF);
	addColumnToDesc(requiredTD, CHAN_FREQ,1,"MEAS_FREQ_REF");
	addColumnToDesc(requiredTD, REF_FREQUENCY,-1,"MEAS_FREQ_REF");

	// define columns with known dimensionality
	addColumnToDesc(requiredTD, CHAN_WIDTH,1);
	addColumnToDesc(requiredTD, EFFECTIVE_BW,1);
	addColumnToDesc(requiredTD, RESOLUTION,1);
	for (i = UNDEFINED_COLUMN+1; 
	     i <= NUMBER_REQUIRED_COLUMNS; i++) {
	    addColumnToDesc(requiredTD, PredefinedColumns(i));
	}


	requiredTD_p=new TableDesc(requiredTD);
	
    }
}

	
MSSpectralWindow MSSpectralWindow::referenceCopy(const String& newTableName, 
				     const Block<String>& writableColumns) const
{
    return MSSpectralWindow(MSTable<PredefinedColumns,PredefinedKeywords>::
		     referenceCopy(newTableName,writableColumns));
}

} //# NAMESPACE CASACORE - END

