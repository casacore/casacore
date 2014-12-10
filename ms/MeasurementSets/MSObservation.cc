//# MSObservation.cc: The MeasurementSet OBSERVATION Table
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

#include <casacore/ms/MeasurementSets/MSObservation.h>
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

MSObservation::MSObservation():hasBeenDestroyed_p(True) { }

MSObservation::MSObservation(const String &tableName, TableOption option) 
    : MSTable<PredefinedColumns,
      PredefinedKeywords>(tableName, option),hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSObservation(String &, TableOption) - "
			 "table is not a valid MSObservation"));
}

MSObservation::MSObservation(const String& tableName, const String &tableDescName,
			       TableOption option)
    : MSTable<PredefinedColumns,
      PredefinedKeywords>(tableName, tableDescName,option),
      hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSObservation(String &, String &, TableOption) - "
			 "table is not a valid MSObservation"));
}

MSObservation::MSObservation(SetupNewTable &newTab, uInt nrrow,
			       Bool initialize)
    : MSTable<PredefinedColumns,
      PredefinedKeywords>(newTab, nrrow, initialize), 
      hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSObservation(SetupNewTable &, uInt, Bool) - "
			 "table is not a valid MSObservation"));
}

MSObservation::MSObservation(const Table &table)
    : MSTable<PredefinedColumns,
      PredefinedKeywords>(table), hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSObservation(const Table &) - "
			 "table is not a valid MSObservation"));
}

MSObservation::MSObservation(const MSObservation &other)
    : MSTable<PredefinedColumns,
      PredefinedKeywords>(other), 
      hasBeenDestroyed_p(False)
{
    // verify that other is valid
    if (&other != this) 
	if (! validate(this->tableDesc()))
	    throw (AipsError("MSObservation(const MSObservation &) - "
			     "table is not a valid MSObservation"));
}

MSObservation::~MSObservation()
{
// check to make sure that this MSObservation is still valid
    if (!hasBeenDestroyed_p &&  !validate()) {
	hasBeenDestroyed_p = True;
	// the table is otherwise OK, so ensure that it is written if necessary
	this->flush();
	// now we can thrown an exception
	throw (AipsError("~MSObservation() - "
			 "Table written is not a valid MSObservation"));
    }
    // if we get to here, let nature take its course
    // this should not be necessary, but do it for insurance anyway
    hasBeenDestroyed_p = True;
}


MSObservation& MSObservation::operator=(const MSObservation &other)
{
    if (&other != this) {
	MSTable<PredefinedColumns,
	PredefinedKeywords>::operator=(other);
	hasBeenDestroyed_p=other.hasBeenDestroyed_p;
    }
    return *this;
}

void MSObservation::init()
{
    if (! columnMap_p.ndefined()) {
	// the PredefinedColumns
	// FLAG_ROW
	colMapDef(FLAG_ROW,"FLAG_ROW",TpBool,
		  "Row flag","","");
	// LOG
	colMapDef(LOG,"LOG",TpArrayString,
		  "Observing log","","");
	// OBSERVER
	colMapDef(OBSERVER, "OBSERVER", TpString,
		  "Name of observer(s)","","");
	// PROJECT
	colMapDef(PROJECT,"PROJECT",TpString,
		  "Project identification string","","");
	// RELEASE_DATE
	colMapDef(RELEASE_DATE,"RELEASE_DATE",TpDouble,
		  "Release date when data becomes public","s","Epoch");
	// SCHEDULE
	colMapDef(SCHEDULE,"SCHEDULE",TpArrayString,
		  "Observing schedule","","");
	// SCHEDULE_TYPE
	colMapDef(SCHEDULE_TYPE,"SCHEDULE_TYPE",TpString,
		  "Observing schedule type","","");
	// TELESCOPE_NAME
	colMapDef(TELESCOPE_NAME,"TELESCOPE_NAME",TpString,
		  "Telescope Name (e.g. WSRT, VLBA)");
	// TIME_RANGE
	colMapDef(TIME_RANGE,"TIME_RANGE",TpArrayDouble,
		  "Start and end of observation","s","Epoch");
	// PredefinedKeywords

	// init requiredTableDesc
	TableDesc requiredTD;
	// all required keywords
        // Define the columns with fixed size arrays
        IPosition shape(1,2);
        ColumnDesc::Option option=ColumnDesc::Direct;
	addColumnToDesc(requiredTD, TIME_RANGE, shape, option);
	// Define the columns with known dimensionality
	addColumnToDesc(requiredTD, LOG, 1);
	addColumnToDesc(requiredTD, SCHEDULE, 1);
	for (Int i = UNDEFINED_KEYWORD+1;
	     i <= NUMBER_PREDEFINED_KEYWORDS; i++) {
	    addKeyToDesc(requiredTD, PredefinedKeywords(i));
	}
	
	// all required columns 
	for (Int i = UNDEFINED_COLUMN+1; 
	     i <= NUMBER_REQUIRED_COLUMNS; i++) {
	    addColumnToDesc(requiredTD, PredefinedColumns(i));
	}
	requiredTD_p=new TableDesc(requiredTD);
    }
}

	
MSObservation MSObservation::referenceCopy(const String& newTableName, 
				    const Block<String>& writableColumns) const
{
  return MSObservation(MSTable<PredefinedColumns,PredefinedKeywords>::referenceCopy
		 (newTableName,writableColumns));
}

} //# NAMESPACE CASACORE - END

