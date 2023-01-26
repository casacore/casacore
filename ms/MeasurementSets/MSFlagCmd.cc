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

#include <casacore/ms/MeasurementSets/MSFlagCmd.h>
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

MSFlagCmd::MSFlagCmd():hasBeenDestroyed_p(true) { }

MSFlagCmd::MSFlagCmd(const String &tableName, TableOption option) 
  : MSTable<MSFlagCmdEnums>(tableName, option),
    hasBeenDestroyed_p(false)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSFlagCmd(String &, TableOption) - "
			 "table is not a valid MSFlagCmd"));
}

MSFlagCmd::MSFlagCmd(const String& tableName, const String &tableDescName,
			       TableOption option)
    : MSTable<MSFlagCmdEnums>(tableName, tableDescName,option),
      hasBeenDestroyed_p(false)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSFlagCmd(String &, String &, TableOption) - "
			 "table is not a valid MSFlagCmd"));
}

MSFlagCmd::MSFlagCmd(SetupNewTable &newTab, rownr_t nrrow,
			       bool initialize)
    : MSTable<MSFlagCmdEnums>(newTab, nrrow, initialize), 
      hasBeenDestroyed_p(false)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSFlagCmd(SetupNewTable &, uint32_t, bool) - "
			 "table is not a valid MSFlagCmd"));
}

MSFlagCmd::MSFlagCmd(const Table &table)
    : MSTable<MSFlagCmdEnums>(table), hasBeenDestroyed_p(false)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSFlagCmd(const Table &) - "
			 "table is not a valid MSFlagCmd"));
}

MSFlagCmd::MSFlagCmd(const MSFlagCmd &other)
    : MSTable<MSFlagCmdEnums>(other), 
      hasBeenDestroyed_p(false)
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
    if (!hasBeenDestroyed_p  &&  !validate()) {
	// the table is otherwise OK, so ensure that it is written if necessary
	this->flush();
        LogIO os;
        os << LogIO::WARN
           << "~MSFlagCmd() - Table written is not a valid MSFlagCmd"
           << LogIO::POST;
    }
    hasBeenDestroyed_p = true;
}


MSFlagCmd& MSFlagCmd::operator=(const MSFlagCmd &other)
{
    if (&other != this) {
	MSTable<MSFlagCmdEnums>::operator=(other);
	hasBeenDestroyed_p=other.hasBeenDestroyed_p;
    }
    return *this;
}

MSTableMaps MSFlagCmd::initMaps()
{
  MSTableMaps maps;
  // the PredefinedColumns
  // APPLIED
  colMapDef(maps, APPLIED, "APPLIED", TpBool,
            "True if flag has been applied to main table","","");
  // COMMAND
  colMapDef(maps, COMMAND, "COMMAND", TpString,
            "Flagging command","","");
  // INTERVAL
  colMapDef(maps, INTERVAL,"INTERVAL", TpDouble,
            "Time interval for which this flag is valid","s","");
  // LEVEL
  colMapDef(maps, LEVEL, "LEVEL", TpInt,
            "Flag level - revision level ","","");
  // REASON
  colMapDef(maps, REASON, "REASON", TpString,
            "Flag reason","","");
  // SEVERITY
  colMapDef(maps, SEVERITY, "SEVERITY", TpInt,
            "Severity code (0-10) ","","");
  // TIME
  colMapDef(maps, TIME, "TIME", TpDouble,
            "Midpoint of interval for which this flag is valid",
            "s","Epoch");
  // TYPE
  colMapDef(maps, TYPE, "TYPE", TpString,
            "Type of flag (FLAG or UNFLAG)","","");
  
  // PredefinedKeywords

  // init requiredTableDesc
  // all required keywords
  uint32_t i;
  for (i = UNDEFINED_KEYWORD+1;
       i <= NUMBER_PREDEFINED_KEYWORDS; i++) {
    addKeyToDesc(maps, PredefinedKeywords(i));
  }
  // all required columns 
  // Now define all other columns (duplicates are skipped)
  for (i = UNDEFINED_COLUMN+1; 
       i <= NUMBER_REQUIRED_COLUMNS; i++) {
    addColumnToDesc(maps, PredefinedColumns(i));
  }

  return maps;
}

	
MSFlagCmd MSFlagCmd::referenceCopy(const String& newTableName, 
			       const Block<String>& writableColumns) const
{
    return MSFlagCmd(MSTable<MSFlagCmdEnums>::
		     referenceCopy(newTableName,writableColumns));
}


} //# NAMESPACE CASACORE - END

