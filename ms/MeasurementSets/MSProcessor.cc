//# MSProcessor.cc: The MeasurementSet PROCESSOR Table
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

#include <casacore/ms/MeasurementSets/MSProcessor.h>
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

MSProcessor::MSProcessor():hasBeenDestroyed_p(True) { }

MSProcessor::MSProcessor(const String &tableName, TableOption option) 
    : MSTable<PredefinedColumns,
      PredefinedKeywords>(tableName, option),hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSProcessor(String &, TableOption) - "
			 "table is not a valid MSProcessor"));
}

MSProcessor::MSProcessor(const String& tableName, const String &tableDescName,
			       TableOption option)
    : MSTable<PredefinedColumns,
      PredefinedKeywords>(tableName, tableDescName,option),
      hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSProcessor(String &, String &, TableOption) - "
			 "table is not a valid MSProcessor"));
}

MSProcessor::MSProcessor(SetupNewTable &newTab, uInt nrrow,
			       Bool initialize)
    : MSTable<PredefinedColumns,
      PredefinedKeywords>(newTab, nrrow, initialize), 
      hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSProcessor(SetupNewTable &, uInt, Bool) - "
			 "table is not a valid MSProcessor"));
}

MSProcessor::MSProcessor(const Table &table)
    : MSTable<PredefinedColumns,
      PredefinedKeywords>(table), hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSProcessor(const Table &) - "
			 "table is not a valid MSProcessor"));
}

MSProcessor::MSProcessor(const MSProcessor &other)
    : MSTable<PredefinedColumns,
      PredefinedKeywords>(other), 
      hasBeenDestroyed_p(False)
{
    // verify that other is valid
    if (&other != this) 
	if (! validate(this->tableDesc()))
	    throw (AipsError("MSProcessor(const MSProcessor &) - "
			     "table is not a valid MSProcessor"));
}

MSProcessor::~MSProcessor()
{
// check to make sure that this MSProcessor is still valid
    if (!hasBeenDestroyed_p &&  !validate()) {
	hasBeenDestroyed_p = True;
	// the table is otherwise OK, so ensure that it is written if necessary
	this->flush();
	// now we can thrown an exception
	throw (AipsError("~MSProcessor() - "
			 "Table written is not a valid MSProcessor"));
    }
    // if we get to here, let nature take its course
    // this should not be necessary, but do it for insurance anyway
    hasBeenDestroyed_p = True;
}


MSProcessor& MSProcessor::operator=(const MSProcessor &other)
{
    if (&other != this) {
	MSTable<PredefinedColumns,
	PredefinedKeywords>::operator=(other);
	hasBeenDestroyed_p=other.hasBeenDestroyed_p;
    }
    return *this;
}

void MSProcessor::init()
{
    if (! columnMap_p.ndefined()) {
	// the PredefinedColumns
	// FLAG_ROW
	colMapDef(FLAG_ROW, "FLAG_ROW", TpBool,
		  "Row flag","","");
	// 
	colMapDef(MODE_ID, "MODE_ID", TpInt,
		  "Processor mode id","","");
	// PASS_ID
	colMapDef(PASS_ID, "PASS_ID", TpInt,
		  "Processor pass number","","");
	// TYPE
	colMapDef(TYPE, "TYPE", TpString,
		  "Processor type","","");
	// TYPE_ID
	colMapDef(TYPE_ID, "TYPE_ID", TpInt,
		  "Processor type id","","");
	// SUB_TYPE
	colMapDef(SUB_TYPE, "SUB_TYPE", TpString,
		  "Processor sub type","","");
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

	
MSProcessor MSProcessor::referenceCopy(const String& newTableName, 
			       const Block<String>& writableColumns) const
{
    return MSProcessor(MSTable<PredefinedColumns,PredefinedKeywords>::
		     referenceCopy(newTableName,writableColumns));
}

} //# NAMESPACE CASACORE - END

