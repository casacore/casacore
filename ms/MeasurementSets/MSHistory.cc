//# MSHistory.cc: The MeasurementSet HISTORY Table
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

#include <casacore/ms/MeasurementSets/MSHistory.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/ColDescSet.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/DataMan/StManAipsIO.h>
#include <casacore/tables/DataMan/ForwardCol.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

MSHistory::MSHistory()
  : hasBeenDestroyed_p(True)
{}

MSHistory::MSHistory(const String &tableName, TableOption option) 
  : MSTable<MSHistoryEnums>(tableName, option),
    hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSHistory(String &, TableOption) - "
			 "table is not a valid MSHistory"));
}

MSHistory::MSHistory(const String& tableName, const String &tableDescName,
			       TableOption option)
  : MSTable<MSHistoryEnums>(tableName, tableDescName,option),
    hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSHistory(String &, String &, TableOption) - "
			 "table is not a valid MSHistory"));
}

MSHistory::MSHistory(SetupNewTable &newTab, uInt nrrow,
			       Bool initialize)
    : MSTable<MSHistoryEnums>(newTab, nrrow, initialize), 
      hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSHistory(SetupNewTable &, uInt, Bool) - "
			 "table is not a valid MSHistory"));
}

MSHistory::MSHistory(const Table &table)
    : MSTable<MSHistoryEnums>(table), hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSHistory(const Table &) - "
			 "table is not a valid MSHistory"));
}

MSHistory::MSHistory(const MSHistory &other)
    : MSTable<MSHistoryEnums>(other), 
      hasBeenDestroyed_p(False)
{
    // verify that other is valid
    if (&other != this) 
	if (! validate(this->tableDesc()))
	    throw (AipsError("MSHistory(const MSHistory &) - "
			     "table is not a valid MSHistory"));
}

MSHistory::~MSHistory()
{
// check to make sure that this MSHistory is still valid
    if (!hasBeenDestroyed_p  &&  !validate()) {
	// the table is otherwise OK, so ensure that it is written if necessary
	this->flush();
        LogIO os;
        os << LogIO::WARN
           << "~MSHistory() - Table written is not a valid MSHistory"
           << LogIO::POST;
    }
    hasBeenDestroyed_p = True;
}


MSHistory& MSHistory::operator=(const MSHistory &other)
{
    if (&other != this) {
	MSTable<MSHistoryEnums>::operator=(other);
	hasBeenDestroyed_p=other.hasBeenDestroyed_p;
    }
    return *this;
}

MSTableMaps MSHistory::initMaps()
{
  MSTableMaps maps;
  // the PredefinedColumns
  // APPLICATION
  colMapDef(maps, APPLICATION,"APPLICATION",TpString,
            "Application name","","");
  // APP_PARAMS
  colMapDef(maps, APP_PARAMS,"APP_PARAMS",TpArrayString,
            "Application parameters","","");
  // CLI_COMMAND
  colMapDef(maps, CLI_COMMAND,"CLI_COMMAND",TpArrayString,
            "CLI command sequence","","");
  // MESSAGE
  colMapDef(maps, MESSAGE,"MESSAGE",TpString,
            "Log message","","");
  // OBJECT_ID
  colMapDef(maps, OBJECT_ID,"OBJECT_ID",TpInt,
            "Originating ObjectID","","");
  // OBSERVATION_ID
  colMapDef(maps, OBSERVATION_ID, "OBSERVATION_ID", TpInt,
            "Observation id (index in OBSERVATION table)","","");
  // ORIGIN
  colMapDef(maps, ORIGIN,"ORIGIN",TpString,
            "(Source code) origin from which message originated","","");
  // PRIORITY
  colMapDef(maps, PRIORITY,"PRIORITY",TpString,
            "Message priority","","");
  // TIME
  colMapDef(maps, TIME,"TIME",TpDouble,
            "Timestamp of message","s","Epoch");
  // PredefinedKeywords

  // init requiredTableDesc
  // all required keywords
  uInt i;
  for (i = UNDEFINED_KEYWORD+1;
       i <= NUMBER_PREDEFINED_KEYWORDS; i++) {
    addKeyToDesc(maps, PredefinedKeywords(i));
  }
  // define the columns with known dimensionality
  addColumnToDesc(maps, APP_PARAMS, 1);
  addColumnToDesc(maps, CLI_COMMAND, 1);
  // all required columns 
  for (i = UNDEFINED_COLUMN+1; 
       i <= NUMBER_REQUIRED_COLUMNS; i++) {
    addColumnToDesc(maps, PredefinedColumns(i));
  }

  return maps;
}

	
MSHistory MSHistory::referenceCopy(const String& newTableName, 
				   const Block<String>& writableColumns) const
{
  return MSHistory(MSTable<MSHistoryEnums>::referenceCopy
                   (newTableName,writableColumns));
}

} //# NAMESPACE CASACORE - END

