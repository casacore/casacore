//# NewMSFlagCmd.cc: The NewMeasurementSet FLAG_CMD Table
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

#include <aips/MeasurementSets/NewMSFlagCmd.h>
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

NewMSFlagCmd::NewMSFlagCmd():hasBeenDestroyed_p(True) { }

NewMSFlagCmd::NewMSFlagCmd(const String &tableName, TableOption option) 
    : NewMSTable<PredefinedColumns,
      PredefinedKeywords>(tableName, option),hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("NewMSFlagCmd(String &, TableOption) - "
			 "table is not a valid NewMSFlagCmd"));
}

NewMSFlagCmd::NewMSFlagCmd(const String& tableName, const String &tableDescName,
			       TableOption option)
    : NewMSTable<PredefinedColumns,
      PredefinedKeywords>(tableName, tableDescName,option),
      hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("NewMSFlagCmd(String &, String &, TableOption) - "
			 "table is not a valid NewMSFlagCmd"));
}

NewMSFlagCmd::NewMSFlagCmd(SetupNewTable &newTab, uInt nrrow,
			       Bool initialize)
    : NewMSTable<PredefinedColumns,
      PredefinedKeywords>(newTab, nrrow, initialize), 
      hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("NewMSFlagCmd(SetupNewTable &, uInt, Bool) - "
			 "table is not a valid NewMSFlagCmd"));
}

NewMSFlagCmd::NewMSFlagCmd(const Table &table)
    : NewMSTable<PredefinedColumns,
      PredefinedKeywords>(table), hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("NewMSFlagCmd(const Table &) - "
			 "table is not a valid NewMSFlagCmd"));
}

NewMSFlagCmd::NewMSFlagCmd(const NewMSFlagCmd &other)
    : NewMSTable<PredefinedColumns,
      PredefinedKeywords>(other), 
      hasBeenDestroyed_p(False)
{
    // verify that other is valid
    if (&other != this) 
	if (! validate(this->tableDesc()))
	    throw (AipsError("NewMSFlagCmd(const NewMSFlagCmd &) - "
			     "table is not a valid NewMSFlagCmd"));
}

NewMSFlagCmd::~NewMSFlagCmd()
{
// check to make sure that this NewMSFlagCmd is still valid
    if (!hasBeenDestroyed_p &&  !validate()) {
	hasBeenDestroyed_p = True;
	// the table is otherwise OK, so ensure that it is written if necessary
	this->flush();
	// now we can thrown an exception
	throw (AipsError("~NewMSFlagCmd() - "
			 "Table written is not a valid NewMSFlagCmd"));
    }
    // if we get to here, let nature take its course
    // this should not be necessary, but do it for insurance anyway
    hasBeenDestroyed_p = True;
}


NewMSFlagCmd& NewMSFlagCmd::operator=(const NewMSFlagCmd &other)
{
    if (&other != this) {
	NewMSTable<PredefinedColumns,
	PredefinedKeywords>::operator=(other);
	hasBeenDestroyed_p=other.hasBeenDestroyed_p;
    }
    return *this;
}

void NewMSFlagCmd::init()
{
    if (! columnMap_p.ndefined()) {
	// the PredefinedColumns
	// APPLIED
	colMapDef(APPLIED, "APPLIED", TpBool,
		  "True if flag has been applied to main table","","");
	// COMMAND
	colMapDef(COMMAND, "COMMAND", TpString,
		  "Flagging command","","");
	// INTERVAL
	colMapDef(INTERVAL,"INTERVAL", TpDouble,
		  "Time interval for which this flag is valid","s","");
	// LEVEL
	colMapDef(LEVEL, "LEVEL", TpInt,
		  "Flag level - revision level ","","");
	// REASON
	colMapDef(REASON, "REASON", TpString,
		  "Flag reason","","");
	// SEVERITY
	colMapDef(SEVERITY, "SEVERITY", TpInt,
		  "Severity code (0-10) ","","");
	// TIME
	colMapDef(TIME, "TIME", TpDouble,
		  "Midpoint of interval for which this flag is valid",
		  "s","Epoch");
	// TYPE
	colMapDef(TYPE, "TYPE", TpString,
		  "Type of flag (FLAG or UNFLAG)","","");

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
	// Now define all other columns (duplicates are skipped)
	for (i = UNDEFINED_COLUMN+1; 
	     i <= NUMBER_REQUIRED_COLUMNS; i++) {
	    addColumnToDesc(requiredTD, PredefinedColumns(i));
	}
	requiredTD_p=new TableDesc(requiredTD);
    }
}

	
NewMSFlagCmd NewMSFlagCmd::referenceCopy(const String& newTableName, 
			       const Block<String>& writableColumns) const
{
    return NewMSFlagCmd(NewMSTable<PredefinedColumns,PredefinedKeywords>::
		     referenceCopy(newTableName,writableColumns));
}

