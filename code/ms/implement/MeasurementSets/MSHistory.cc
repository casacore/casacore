//# NewMSHistory.cc: The NewMeasurementSet HISTORY Table
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

#include <aips/MeasurementSets/NewMSHistory.h>
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

NewMSHistory::NewMSHistory():hasBeenDestroyed_p(True) { }

NewMSHistory::NewMSHistory(const String &tableName, TableOption option) 
    : NewMSTable<PredefinedColumns,
      PredefinedKeywords>(tableName, option),hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("NewMSHistory(String &, TableOption) - "
			 "table is not a valid NewMSHistory"));
}

NewMSHistory::NewMSHistory(const String& tableName, const String &tableDescName,
			       TableOption option)
    : NewMSTable<PredefinedColumns,
      PredefinedKeywords>(tableName, tableDescName,option),
      hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("NewMSHistory(String &, String &, TableOption) - "
			 "table is not a valid NewMSHistory"));
}

NewMSHistory::NewMSHistory(SetupNewTable &newTab, uInt nrrow,
			       Bool initialize)
    : NewMSTable<PredefinedColumns,
      PredefinedKeywords>(newTab, nrrow, initialize), 
      hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("NewMSHistory(SetupNewTable &, uInt, Bool) - "
			 "table is not a valid NewMSHistory"));
}

NewMSHistory::NewMSHistory(const Table &table)
    : NewMSTable<PredefinedColumns,
      PredefinedKeywords>(table), hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("NewMSHistory(const Table &) - "
			 "table is not a valid NewMSHistory"));
}

NewMSHistory::NewMSHistory(const NewMSHistory &other)
    : NewMSTable<PredefinedColumns,
      PredefinedKeywords>(other), 
      hasBeenDestroyed_p(False)
{
    // verify that other is valid
    if (&other != this) 
	if (! validate(this->tableDesc()))
	    throw (AipsError("NewMSHistory(const NewMSHistory &) - "
			     "table is not a valid NewMSHistory"));
}

NewMSHistory::~NewMSHistory()
{
// check to make sure that this NewMSHistory is still valid
    if (!hasBeenDestroyed_p &&  !validate()) {
	hasBeenDestroyed_p = True;
	// the table is otherwise OK, so ensure that it is written if necessary
	this->flush();
	// now we can thrown an exception
	throw (AipsError("~NewMSHistory() - "
			 "Table written is not a valid NewMSHistory"));
    }
    // if we get to here, let nature take its course
    // this should not be necessary, but do it for insurance anyway
    hasBeenDestroyed_p = True;
}


NewMSHistory& NewMSHistory::operator=(const NewMSHistory &other)
{
    if (&other != this) {
	NewMSTable<PredefinedColumns,
	PredefinedKeywords>::operator=(other);
	hasBeenDestroyed_p=other.hasBeenDestroyed_p;
    }
    return *this;
}

void NewMSHistory::init()
{
    if (! columnMap_p.ndefined()) {
      // the PredefinedColumns
      // APPLICATION
      colMapDef(APPLICATION,"APPLICATION",TpString,
		"Application name","","");
      // APP_PARAMS
      colMapDef(APP_PARAMS,"APP_PARAMS",TpArrayString,
		"Application parameters","","");
      // CLI_COMMAND
      colMapDef(CLI_COMMAND,"CLI_COMMAND",TpArrayString,
		"CLI command sequence","","");
      // MESSAGE
      colMapDef(MESSAGE,"MESSAGE",TpString,
		"Log message","","");
      // OBJECT_ID
      colMapDef(OBJECT_ID,"OBJECT_ID",TpInt,
		"Originating ObjectID","","");
      // OBSERVATION_ID
      colMapDef(OBSERVATION_ID, "OBSERVATION_ID", TpInt,
		"Observation id (index in OBSERVATION table)","","");
      // ORIGIN
      colMapDef(ORIGIN,"ORIGIN",TpString,
		"(Source code) origin from which message originated","","");
      // PRIORITY
      colMapDef(PRIORITY,"PRIORITY",TpString,
		"Message priority","","");
      // TIME
      colMapDef(TIME,"TIME",TpDouble,
		"Timestamp of message","s","Epoch");
      // PredefinedKeywords

	// init requiredTableDesc
	TableDesc requiredTD;
	// all required keywords
	uInt i;
	for (i = UNDEFINED_KEYWORD+1;
	     i <= NUMBER_PREDEFINED_KEYWORDS; i++) {
	    addKeyToDesc(requiredTD, PredefinedKeywords(i));
	}
	// define the columns with known dimensionality
	addColumnToDesc(requiredTD, APP_PARAMS, 1);
	addColumnToDesc(requiredTD, CLI_COMMAND, 1);
	// all required columns 
	for (i = UNDEFINED_COLUMN+1; 
	     i <= NUMBER_REQUIRED_COLUMNS; i++) {
	    addColumnToDesc(requiredTD, PredefinedColumns(i));
	}
	requiredTD_p=new TableDesc(requiredTD);
    }
}

	
NewMSHistory NewMSHistory::referenceCopy(const String& newTableName, 
				   const Block<String>& writableColumns) const
{
  return NewMSHistory(NewMSTable<PredefinedColumns,PredefinedKeywords>::referenceCopy
		 (newTableName,writableColumns));
}
