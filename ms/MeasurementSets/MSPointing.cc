//# MSPointing.cc: The MeasurementSet POINTING Table
//# Copyright (C) 1999,2000
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

#include <casacore/ms/MeasurementSets/MSPointing.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/ColDescSet.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/DataMan/StManAipsIO.h>
#include <casacore/tables/DataMan/ForwardCol.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

MSPointing::MSPointing():hasBeenDestroyed_p(True) { }

MSPointing::MSPointing(const String &tableName, TableOption option) 
    : MSTable<PredefinedColumns,
      PredefinedKeywords>(tableName, option),hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSPointing(String &, TableOption) - "
			 "table is not a valid MSPointing"));
}

MSPointing::MSPointing(const String& tableName, const String &tableDescName,
			       TableOption option)
    : MSTable<PredefinedColumns,
      PredefinedKeywords>(tableName, tableDescName,option),
      hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSPointing(String &, String &, TableOption) - "
			 "table is not a valid MSPointing"));
}

MSPointing::MSPointing(SetupNewTable &newTab, uInt nrrow,
			       Bool initialize)
    : MSTable<PredefinedColumns,
      PredefinedKeywords>(newTab, nrrow, initialize), 
      hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSPointing(SetupNewTable &, uInt, Bool) - "
			 "table is not a valid MSPointing"));
}

MSPointing::MSPointing(const Table &table)
    : MSTable<PredefinedColumns,
      PredefinedKeywords>(table), hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSPointing(const Table &) - "
			 "table is not a valid MSPointing"));
}

MSPointing::MSPointing(const MSPointing &other)
    : MSTable<PredefinedColumns,
      PredefinedKeywords>(other), 
      hasBeenDestroyed_p(False)
{
    // verify that other is valid
    if (&other != this) 
	if (! validate(this->tableDesc()))
	    throw (AipsError("MSPointing(const MSPointing &) - "
			     "table is not a valid MSPointing"));
}

MSPointing::~MSPointing()
{
// check to make sure that this MSPointing is still valid
    if (!hasBeenDestroyed_p &&  !validate()) {
	hasBeenDestroyed_p = True;
	// the table is otherwise OK, so ensure that it is written if necessary
	this->flush();
	// now we can thrown an exception
	throw (AipsError("~MSPointing() - "
			 "Table written is not a valid MSPointing"));
    }
    // if we get to here, let nature take its course
    // this should not be necessary, but do it for insurance anyway
    hasBeenDestroyed_p = True;
}


MSPointing& MSPointing::operator=(const MSPointing &other)
{
    if (&other != this) {
	MSTable<PredefinedColumns,
	PredefinedKeywords>::operator=(other);
	hasBeenDestroyed_p=other.hasBeenDestroyed_p;
    }
    return *this;
}

void MSPointing::init()
{
    if (! columnMap_p.ndefined()) {
	// the PredefinedColumns
	// ANTENNA_ID
	colMapDef(ANTENNA_ID, "ANTENNA_ID", TpInt,
		  "Antenna Id","","");
	// DIRECTION
	colMapDef(DIRECTION, "DIRECTION", TpArrayDouble,
		  "Antenna pointing direction as polynomial in time","rad"
		  ,"Direction");
	// INTERVAL
	colMapDef(INTERVAL, "INTERVAL", TpDouble,
		  "Time interval","s","");
	// NAME
	colMapDef(NAME, "NAME", TpString,
		  "Pointing position name","","");
	// NUM_POLY
	colMapDef(NUM_POLY, "NUM_POLY", TpInt,
		  "Series order","","");
	// TARGET
	colMapDef(TARGET, "TARGET", TpArrayDouble,
		  "target direction as polynomial in time","rad"
		  ,"Direction");
	// TIME
	colMapDef(TIME, "TIME", TpDouble,
		  "Time interval midpoint","s","Epoch");
	// TIME_ORIGIN
	colMapDef(TIME_ORIGIN, "TIME_ORIGIN", TpDouble,
		  "Time origin for direction","s","Epoch");
	// TRACKING
	colMapDef(TRACKING, "TRACKING", TpBool,
		  "Tracking flag - True if on position","","");
	// ENCODER
	colMapDef(ENCODER, "ENCODER", TpArrayDouble,
		  "Encoder values","rad","Direction");
	// ON_SOURCE
	colMapDef(ON_SOURCE, "ON_SOURCE", TpBool,
		  "On source flag","","");
	// OVER_THE_TOP
	colMapDef(OVER_THE_TOP, "OVER_THE_TOP", TpBool,
		  "Antenna over the top","","");
	// POINTING_MODEL_ID
	colMapDef(POINTING_MODEL_ID,"POINTING_MODEL_ID",TpInt,
		  "Pointing model id","","");
	// POINTING_OFFSET
	colMapDef(POINTING_OFFSET, "POINTING_OFFSET", TpArrayDouble,
		  "A priori pointing correction as polynomial in time",
		  "rad","Direction");
	// SOURCE_OFFSET
	colMapDef(SOURCE_OFFSET, "SOURCE_OFFSET", TpArrayDouble,
		  "Offset from source as polynomial in time","rad","Direction");
	// PredefinedKeywords

	// init requiredTableDesc
	TableDesc requiredTD;
	// all required keywords
	// First define the columns with known dimensionality
	addColumnToDesc(requiredTD, DIRECTION, 2);
	uInt i;
	for (i = UNDEFINED_KEYWORD+1;
	     i <= NUMBER_PREDEFINED_KEYWORDS; i++) {
	    addKeyToDesc(requiredTD, PredefinedKeywords(i));
	}
	
	// all required columns 
	// Now define all other columns (duplicates are skipped)
	for (i = UNDEFINED_COLUMN+1; 
	     i <= NUMBER_REQUIRED_COLUMNS; i++) {
	    addColumnToDesc(requiredTD, PredefinedColumns(i));
	}
	requiredTD_p=new TableDesc(requiredTD);
    }
}

	
MSPointing MSPointing::referenceCopy(const String& newTableName, 
			       const Block<String>& writableColumns) const
{
    return MSPointing(MSTable<PredefinedColumns,PredefinedKeywords>::
		     referenceCopy(newTableName,writableColumns));
}

} //# NAMESPACE CASACORE - END

