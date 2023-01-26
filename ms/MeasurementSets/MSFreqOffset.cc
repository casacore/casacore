//# MSFreqOffset.cc: The MeasurementSet FREQ_OFFSET Table
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

#include <casacore/ms/MeasurementSets/MSFreqOffset.h>
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

MSFreqOffset::MSFreqOffset():hasBeenDestroyed_p(true) { }

MSFreqOffset::MSFreqOffset(const String &tableName, TableOption option) 
  : MSTable<MSFreqOffsetEnums>(tableName, option),
    hasBeenDestroyed_p(false)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSFreqOffset(String &, TableOption) - "
			 "table is not a valid MSFreqOffset"));
}

MSFreqOffset::MSFreqOffset(const String& tableName, const String &tableDescName,
			       TableOption option)
  : MSTable<MSFreqOffsetEnums>(tableName, tableDescName, option),
    hasBeenDestroyed_p(false)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSFreqOffset(String &, String &, TableOption) - "
			 "table is not a valid MSFreqOffset"));
}

MSFreqOffset::MSFreqOffset(SetupNewTable &newTab, rownr_t nrrow,
			       bool initialize)
  : MSTable<MSFreqOffsetEnums>(newTab, nrrow, initialize),
    hasBeenDestroyed_p(false)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSFreqOffset(SetupNewTable &, rownr_t, bool) - "
			 "table is not a valid MSFreqOffset"));
}

MSFreqOffset::MSFreqOffset(const Table &table)
  : MSTable<MSFreqOffsetEnums>(table),
    hasBeenDestroyed_p(false)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSFreqOffset(const Table &) - "
			 "table is not a valid MSFreqOffset"));
}

MSFreqOffset::MSFreqOffset(const MSFreqOffset &other)
  : MSTable<MSFreqOffsetEnums>(other),
    hasBeenDestroyed_p(false)
{
    // verify that other is valid
    if (&other != this) 
	if (! validate(this->tableDesc()))
	    throw (AipsError("MSFreqOffset(const MSFreqOffset &) - "
			     "table is not a valid MSFreqOffset"));
}

MSFreqOffset::~MSFreqOffset()
{
// check to make sure that this MSFreqOffset is still valid
    if (!hasBeenDestroyed_p  &&  !validate()) {
	// the table is otherwise OK, so ensure that it is written if necessary
	this->flush();
        LogIO os;
        os << LogIO::WARN
           << "~MSFreqOffset() - Table written is not a valid MSFreqOffset"
           << LogIO::POST;
    }
    hasBeenDestroyed_p = true;
}


MSFreqOffset& MSFreqOffset::operator=(const MSFreqOffset &other)
{
    if (&other != this) {
      MSTable<MSFreqOffsetEnums>::operator=(other);
      hasBeenDestroyed_p=other.hasBeenDestroyed_p;
    }
    return *this;
}

MSTableMaps MSFreqOffset::initMaps()
{
  MSTableMaps maps;
  // the PredefinedColumns
  // ANTENNA1
  colMapDef(maps, ANTENNA1, "ANTENNA1", TpInt,
            "Antenna1 id","","");
  // ANTENNA2
  colMapDef(maps, ANTENNA2, "ANTENNA2", TpInt,
            "Antenna2 id","","");
  // FEED_ID
  colMapDef(maps, FEED_ID, "FEED_ID", TpInt,
            "Feed id","","");
  // INTERVAL
  colMapDef(maps, INTERVAL, "INTERVAL", TpDouble,
            "Time interval","s","");
  // OFFSET
  colMapDef(maps, OFFSET, "OFFSET", TpDouble,
            "Frequency offset - antenna based","Hz","");
  // SPECTRAL_WINDOW_ID
  colMapDef(maps, SPECTRAL_WINDOW_ID, "SPECTRAL_WINDOW_ID", TpInt,
            "Spectral window id","","");
  // TIME
  colMapDef(maps, TIME, "TIME", TpDouble,
            "Midpoint of interval","s","Epoch");
  
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

	
MSFreqOffset MSFreqOffset::referenceCopy(const String& newTableName, 
			       const Block<String>& writableColumns) const
{
  return MSFreqOffset(MSTable<MSFreqOffsetEnums>::
                      referenceCopy(newTableName,writableColumns));
}


} //# NAMESPACE CASACORE - END

