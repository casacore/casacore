//# NewMSObservation.cc: The NewMeasurementSet OBSERVATION Table
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

#include <aips/MeasurementSets/NewMSObservation.h>
#include <aips/Utilities/String.h>
#include <aips/Tables/SetupNewTab.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/ColDescSet.h>
#include <aips/Tables/ScaColDesc.h>
#include <aips/Tables/ArrColDesc.h>
#include <aips/Tables/StManAipsIO.h>
#include <aips/Tables/ForwardCol.h>
#include <aips/Arrays/Vector.h>
#include <aips/Exceptions/Error.h>

NewMSObservation::NewMSObservation():hasBeenDestroyed_p(True) { }

NewMSObservation::NewMSObservation(const String &tableName, TableOption option) 
    : NewMSTable<PredefinedColumns,
      PredefinedKeywords>(tableName, option),hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("NewMSObservation(String &, TableOption) - "
			 "table is not a valid NewMSObservation"));
}

NewMSObservation::NewMSObservation(const String& tableName, const String &tableDescName,
			       TableOption option)
    : NewMSTable<PredefinedColumns,
      PredefinedKeywords>(tableName, tableDescName,option),
      hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("NewMSObservation(String &, String &, TableOption) - "
			 "table is not a valid NewMSObservation"));
}

NewMSObservation::NewMSObservation(SetupNewTable &newTab, uInt nrrow,
			       Bool initialize)
    : NewMSTable<PredefinedColumns,
      PredefinedKeywords>(newTab, nrrow, initialize), 
      hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("NewMSObservation(SetupNewTable &, uInt, Bool) - "
			 "table is not a valid NewMSObservation"));
}

NewMSObservation::NewMSObservation(const Table &table)
    : NewMSTable<PredefinedColumns,
      PredefinedKeywords>(table), hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("NewMSObservation(const Table &) - "
			 "table is not a valid NewMSObservation"));
}

NewMSObservation::NewMSObservation(const NewMSObservation &other)
    : NewMSTable<PredefinedColumns,
      PredefinedKeywords>(other), 
      hasBeenDestroyed_p(False)
{
    // verify that other is valid
    if (&other != this) 
	if (! validate(this->tableDesc()))
	    throw (AipsError("NewMSObservation(const NewMSObservation &) - "
			     "table is not a valid NewMSObservation"));
}

NewMSObservation::~NewMSObservation()
{
// check to make sure that this NewMSObservation is still valid
    if (!hasBeenDestroyed_p &&  !validate()) {
	hasBeenDestroyed_p = True;
	// the table is otherwise OK, so ensure that it is written if necessary
	this->flush();
	// now we can thrown an exception
	throw (AipsError("~NewMSObservation() - "
			 "Table written is not a valid NewMSObservation"));
    }
    // if we get to here, let nature take its course
    // this should not be necessary, but do it for insurance anyway
    hasBeenDestroyed_p = True;
}


NewMSObservation& NewMSObservation::operator=(const NewMSObservation &other)
{
    if (&other != this) {
	NewMSTable<PredefinedColumns,
	PredefinedKeywords>::operator=(other);
	hasBeenDestroyed_p=other.hasBeenDestroyed_p;
    }
    return *this;
}

void NewMSObservation::init()
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

	
NewMSObservation NewMSObservation::referenceCopy(const String& newTableName, 
				    const Block<String>& writableColumns) const
{
  return NewMSObservation(NewMSTable<PredefinedColumns,PredefinedKeywords>::referenceCopy
		 (newTableName,writableColumns));
}
