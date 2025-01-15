//# MSState.cc: The MeasurementSet STATE Table
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#include <casacore/ms/MeasurementSets/MSState.h>

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

MSState::MSState():hasBeenDestroyed_p(True) { }

MSState::MSState(const String &tableName, 
				     TableOption option) 
    : MSTable<MSStateEnums>(tableName, option),hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSState(String &, TableOption) - "
			 "table is not a valid MSState"));
}

MSState::MSState(const String& tableName, const String &tableDescName,
			       TableOption option)
    : MSTable<MSStateEnums>(tableName, tableDescName,option),
      hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSState(String &, String &, TableOption) - "
			 "table is not a valid MSState"));
}

MSState::MSState(SetupNewTable &newTab, rownr_t nrrow,
			       Bool initialize)
    : MSTable<MSStateEnums>(newTab, nrrow, initialize), 
      hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSState(SetupNewTable &, rownr_t, Bool) - "
			 "table is not a valid MSState"));
}

MSState::MSState(const Table &table)
    : MSTable<MSStateEnums>(table), hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSState(const Table &) - "
			 "table is not a valid MSState"));
}

MSState::MSState(const MSState &other)
    : MSTable<MSStateEnums>(other), 
      hasBeenDestroyed_p(False)
{
    // verify that other is valid
    if (&other != this) 
	if (! validate(this->tableDesc()))
	    throw (AipsError("MSState(const MSState &) - "
			     "table is not a valid MSState"));
}

MSState::~MSState()
{
// check to make sure that this MSState is still valid
    if (!hasBeenDestroyed_p  &&  !validate()) {
	// the table is otherwise OK, so ensure that it is written if necessary
	this->flush();
        LogIO os;
        os << LogIO::WARN
           << "~MSState() - Table written is not a valid MSState"
           << LogIO::POST;
    }
    hasBeenDestroyed_p = True;
}


MSState& MSState::operator=(const MSState &other)
{
    if (&other != this) {
	MSTable<MSStateEnums>::operator=(other);
	hasBeenDestroyed_p=other.hasBeenDestroyed_p;
    }
    return *this;
}

MSTableMaps MSState::initMaps()
{
  MSTableMaps maps;
  // the PredefinedColumns
  // CAL
  colMapDef(maps, CAL,"CAL", TpDouble,
            "Noise calibration temperature","K","");
  // FLAG_ROW
  colMapDef(maps, FLAG_ROW,"FLAG_ROW", TpBool,
            "Row flag","","");
  // LOAD
  colMapDef(maps, LOAD,"LOAD", TpDouble,
            "Load temperature","K","");
  // OBS_MODE
  colMapDef(maps, OBS_MODE,"OBS_MODE", TpString,
            "Observing mode, e.g., OFF_SPECTRUM","","");
  // REF
  colMapDef(maps, REF,"REF", TpBool,
            "True for a reference observation","","");
  // SIG
  colMapDef(maps, SIG,"SIG", TpBool,
            "True for a source observation","","");
  // SUB_SCAN
  colMapDef(maps, SUB_SCAN,"SUB_SCAN", TpInt,
            "Sub scan number, relative to scan number","","");
  
  // PredefinedKeywords

  // init requiredTableDesc
  // all required keywords
  uInt i;
  for (i = UNDEFINED_KEYWORD+1;
       i <= NUMBER_PREDEFINED_KEYWORDS; i++) {
    addKeyToDesc(maps, PredefinedKeywords(i));
  }
  // all required columns 
  for (i = UNDEFINED_COLUMN+1; 
       i <= NUMBER_REQUIRED_COLUMNS; i++) {
    addColumnToDesc(maps, PredefinedColumns(i));
  }

  return maps;
}

	
MSState MSState::referenceCopy(const String& newTableName, 
		    const Block<String>& writableColumns) const
{
    return MSState(MSTable<MSStateEnums>::
		     referenceCopy(newTableName,writableColumns));
}

} //# NAMESPACE CASACORE - END

