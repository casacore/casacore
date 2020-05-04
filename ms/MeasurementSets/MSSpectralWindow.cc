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
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

MSSpectralWindow::MSSpectralWindow():hasBeenDestroyed_p(True) { }

MSSpectralWindow::MSSpectralWindow(const String &tableName, TableOption option) 
    : MSTable<MSSpectralWindowEnums>(tableName, option),hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSSpectralWindow(String &, TableOption) - "
			 "table is not a valid MSSpectralWindow"));
}

MSSpectralWindow::MSSpectralWindow(const String& tableName, const String &tableDescName,
			       TableOption option)
    : MSTable<MSSpectralWindowEnums>(tableName, tableDescName,option),
      hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSSpectralWindow(String &, String &, TableOption) - "
			 "table is not a valid MSSpectralWindow"));
}

MSSpectralWindow::MSSpectralWindow(SetupNewTable &newTab, rownr_t nrrow,
			       Bool initialize)
    : MSTable<MSSpectralWindowEnums>(newTab, nrrow, initialize), 
      hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSSpectralWindow(SetupNewTable &, rownr_t, Bool) - "
			 "table is not a valid MSSpectralWindow"));
}

MSSpectralWindow::MSSpectralWindow(const Table &table)
    : MSTable<MSSpectralWindowEnums>(table), hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSSpectralWindow(const Table &) - "
			 "table is not a valid MSSpectralWindow"));
}

MSSpectralWindow::MSSpectralWindow(const MSSpectralWindow &other)
    : MSTable<MSSpectralWindowEnums>(other), 
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
    if (!hasBeenDestroyed_p  &&  !validate()) {
	// the table is otherwise OK, so ensure that it is written if necessary
	this->flush();
        LogIO os;
        os << LogIO::WARN
           << "~MSSpectralWindow() - Table written is not a valid MSSpectralWindow"
           << LogIO::POST;
    }
    hasBeenDestroyed_p = True;
}


MSSpectralWindow& MSSpectralWindow::operator=(const MSSpectralWindow &other)
{
    if (&other != this) {
	MSTable<MSSpectralWindowEnums>::operator=(other);
	hasBeenDestroyed_p=other.hasBeenDestroyed_p;
    }
    return *this;
}

MSTableMaps MSSpectralWindow::initMaps()
{
  MSTableMaps maps;
  // the PredefinedColumns
  // 
  // ASSOC_NATURE
  colMapDef(maps, ASSOC_NATURE,"ASSOC_NATURE", TpArrayString,
            "Nature of association with other spectral window","","");
  // ASSOC_SPW_ID
  colMapDef(maps, ASSOC_SPW_ID,"ASSOC_SPW_ID",TpArrayInt,
            "Associated spectral window id","","");
  // BBC_NO
  colMapDef(maps, BBC_NO,"BBC_NO",TpInt,
            "Baseband converter number","","");
  // BBC_SIDEBAND
  colMapDef(maps, BBC_SIDEBAND,"BBC_SIDEBAND",TpInt,
            "BBC sideband","","");
  // CHAN_FREQ
  colMapDef(maps, CHAN_FREQ,"CHAN_FREQ", TpArrayDouble,
            "Center frequencies for each channel in the data matrix",
            "Hz","Frequency");
  // CHAN_WIDTH
  colMapDef(maps, CHAN_WIDTH,"CHAN_WIDTH",TpArrayDouble,
            "Channel width for each channel","Hz","");
  // DOPPLER_ID
  colMapDef(maps, DOPPLER_ID,"DOPPLER_ID",TpInt,
            "Doppler Id, points to DOPPLER table","","");
  // EFFECTIVE_BW
  colMapDef(maps, EFFECTIVE_BW,"EFFECTIVE_BW",TpArrayDouble,
            "Effective noise bandwidth of each channel","Hz","");
  // FLAG_ROW
  colMapDef(maps, FLAG_ROW,"FLAG_ROW",TpBool,
            "Row flag","","");
  // FREQ_GROUP
  colMapDef(maps, FREQ_GROUP,"FREQ_GROUP",TpInt,
            "Frequency group","","");
  // FREQ_GROUP_NAME
  colMapDef(maps, FREQ_GROUP_NAME,"FREQ_GROUP_NAME",TpString,
            "Frequency group name","","");
  // IF_CONV_CHAIN
  colMapDef(maps, IF_CONV_CHAIN, "IF_CONV_CHAIN", TpInt,
            "The IF conversion chain number","","");
  // MEAS_FREQ_REF
  colMapDef(maps, MEAS_FREQ_REF,"MEAS_FREQ_REF",TpInt,
            "Frequency Measure reference","","");
  // NAME
  colMapDef(maps, NAME,"NAME",TpString,
            "Spectral window name","","");
  // NET_SIDEBAND
  colMapDef(maps, NET_SIDEBAND,"NET_SIDEBAND",TpInt,
            "Net sideband","","");
  // NUM_CHAN
  colMapDef(maps, NUM_CHAN, "NUM_CHAN", TpInt,
            "Number of spectral channels","","");
  // RECEIVER_ID
  colMapDef(maps, RECEIVER_ID,"RECEIVER_ID",TpInt,
            "Receiver Id for this spectral window","","");
  // REF_FREQUENCY
  colMapDef(maps, REF_FREQUENCY, "REF_FREQUENCY", TpDouble,
            "The reference frequency",
            "Hz","Frequency");
  // RESOLUTION
  colMapDef(maps, RESOLUTION, "RESOLUTION", TpArrayDouble,
            "The effective noise bandwidth for each channel",
            "Hz","");
  // TOTAL_BANDWIDTH
  colMapDef(maps, TOTAL_BANDWIDTH, "TOTAL_BANDWIDTH", TpDouble,
            "The total bandwidth for this window","Hz","");

  // PredefinedKeywords

  // init requiredTableDesc
  // all required keywords
  uInt i;
  for (i = UNDEFINED_KEYWORD+1;
       i <= NUMBER_PREDEFINED_KEYWORDS; i++) {
    addKeyToDesc(maps, PredefinedKeywords(i));
  }
  // all required columns 

  // set up the TableMeasure columns with variable reference
  // first add the variable ref column
  addColumnToDesc(maps, MEAS_FREQ_REF);
  addColumnToDesc(maps, CHAN_FREQ,1,"MEAS_FREQ_REF");
  addColumnToDesc(maps, REF_FREQUENCY,-1,"MEAS_FREQ_REF");

  // define columns with known dimensionality
  addColumnToDesc(maps, CHAN_WIDTH,1);
  addColumnToDesc(maps, EFFECTIVE_BW,1);
  addColumnToDesc(maps, RESOLUTION,1);
  for (i = UNDEFINED_COLUMN+1; 
       i <= NUMBER_REQUIRED_COLUMNS; i++) {
    addColumnToDesc(maps, PredefinedColumns(i));
  }

  return maps;
}

	
MSSpectralWindow MSSpectralWindow::referenceCopy(const String& newTableName, 
				     const Block<String>& writableColumns) const
{
    return MSSpectralWindow(MSTable<MSSpectralWindowEnums>::
		     referenceCopy(newTableName,writableColumns));
}

} //# NAMESPACE CASACORE - END

