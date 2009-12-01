//# MSFlagCmd.cc: The MeasurementSet FLAG_CMD Table
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

#include <ms/MeasurementSets/MSFlagCmd.h>
#include <casa/BasicSL/String.h>
#include <tables/Tables/SetupNewTab.h>
#include <tables/Tables/TableDesc.h>
#include <tables/Tables/ColDescSet.h>
#include <tables/Tables/ScaColDesc.h>
#include <tables/Tables/ArrColDesc.h>
#include <tables/Tables/StManAipsIO.h>
#include <tables/Tables/ForwardCol.h>
#include <casa/Arrays/Vector.h>
#include <casa/Exceptions/Error.h>

namespace casa { //# NAMESPACE CASA - BEGIN

MSFlagCmd::MSFlagCmd():hasBeenDestroyed_p(True) { }

MSFlagCmd::MSFlagCmd(const String &tableName, TableOption option) 
    : MSTable<PredefinedColumns,
      PredefinedKeywords>(tableName, option),hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSFlagCmd(String &, TableOption) - "
			 "table is not a valid MSFlagCmd"));
}

MSFlagCmd::MSFlagCmd(const String& tableName, const String &tableDescName,
			       TableOption option)
    : MSTable<PredefinedColumns,
      PredefinedKeywords>(tableName, tableDescName,option),
      hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSFlagCmd(String &, String &, TableOption) - "
			 "table is not a valid MSFlagCmd"));
}

MSFlagCmd::MSFlagCmd(SetupNewTable &newTab, uInt nrrow,
			       Bool initialize)
    : MSTable<PredefinedColumns,
      PredefinedKeywords>(newTab, nrrow, initialize), 
      hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSFlagCmd(SetupNewTable &, uInt, Bool) - "
			 "table is not a valid MSFlagCmd"));
}

MSFlagCmd::MSFlagCmd(const Table &table)
    : MSTable<PredefinedColumns,
      PredefinedKeywords>(table), hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSFlagCmd(const Table &) - "
			 "table is not a valid MSFlagCmd"));
}

MSFlagCmd::MSFlagCmd(const MSFlagCmd &other)
    : MSTable<PredefinedColumns,
      PredefinedKeywords>(other), 
      hasBeenDestroyed_p(False)
{
    // verify that other is valid
    if (&other != this) 
	if (! validate(this->tableDesc()))
	    throw (AipsError("MSFlagCmd(const MSFlagCmd &) - "
			     "table is not a valid MSFlagCmd"));
}

MSFlagCmd::~MSFlagCmd()
{
// check to make sure that this MSFlagCmd is still valid
    if (!hasBeenDestroyed_p &&  !validate()) {
	hasBeenDestroyed_p = True;
	// the table is otherwise OK, so ensure that it is written if necessary
	this->flush();
	// now we can thrown an exception
	throw (AipsError("~MSFlagCmd() - "
			 "Table written is not a valid MSFlagCmd"));
    }
    // if we get to here, let nature take its course
    // this should not be necessary, but do it for insurance anyway
    hasBeenDestroyed_p = True;
}


MSFlagCmd& MSFlagCmd::operator=(const MSFlagCmd &other)
{
    if (&other != this) {
	MSTable<PredefinedColumns,
	PredefinedKeywords>::operator=(other);
	hasBeenDestroyed_p=other.hasBeenDestroyed_p;
    }
    return *this;
}

void MSFlagCmd::init()
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

	
MSFlagCmd MSFlagCmd::referenceCopy(const String& newTableName, 
			       const Block<String>& writableColumns) const
{
    return MSFlagCmd(MSTable<PredefinedColumns,PredefinedKeywords>::
		     referenceCopy(newTableName,writableColumns));
}


} //# NAMESPACE CASA - END

