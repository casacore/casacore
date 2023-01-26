//# MSFeed.cc: The MeasurementSet FEED Table
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

#include <casacore/ms/MeasurementSets/MSFeed.h>
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

MSFeed::MSFeed():hasBeenDestroyed_p(true) { }

MSFeed::MSFeed(const String &tableName, TableOption option) 
    : MSTable<MSFeedEnums>(tableName, option),hasBeenDestroyed_p(false)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSFeed(String &, TableOption) - "
			 "table is not a valid MSFeed"));
}

MSFeed::MSFeed(const String& tableName, const String &tableDescName,
			       TableOption option)
    : MSTable<MSFeedEnums>(tableName, tableDescName,option),
      hasBeenDestroyed_p(false)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSFeed(String &, String &, TableOption) - "
			 "table is not a valid MSFeed"));
}

MSFeed::MSFeed(SetupNewTable &newTab, rownr_t nrrow,
			       bool initialize)
    : MSTable<MSFeedEnums>(newTab, nrrow, initialize), 
      hasBeenDestroyed_p(false)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSFeed(SetupNewTable &, rownr_t, bool) - "
			 "table is not a valid MSFeed"));
}

MSFeed::MSFeed(const Table &table)
    : MSTable<MSFeedEnums>(table), hasBeenDestroyed_p(false)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSFeed(const Table &) - "
			 "table is not a valid MSFeed"));
}

MSFeed::MSFeed(const MSFeed &other)
    : MSTable<MSFeedEnums>(other), 
      hasBeenDestroyed_p(false)
{
    // verify that other is valid
    if (&other != this) 
	if (! validate(this->tableDesc()))
	    throw (AipsError("MSFeed(const MSFeed &) - "
			     "table is not a valid MSFeed"));
}

MSFeed::~MSFeed()
{
// check to make sure that this MSFeed is still valid
    if (!hasBeenDestroyed_p  &&  !validate()) {
	// the table is otherwise OK, so ensure that it is written if necessary
	this->flush();
        LogIO os;
        os << LogIO::WARN
           << "~MSFeed() - Table written is not a valid MSFeed"
           << LogIO::POST;
    }
    hasBeenDestroyed_p = true;
}


MSFeed& MSFeed::operator=(const MSFeed &other)
{
    if (&other != this) {
	MSTable<MSFeedEnums>::operator=(other);
	hasBeenDestroyed_p=other.hasBeenDestroyed_p;
    }
    return *this;
}

MSTableMaps MSFeed::initMaps()
{
  MSTableMaps maps;
  AlwaysAssert (maps.columnMap_p.empty(), AipsError);
  // the PredefinedColumns
  // ANTENNA_ID
  colMapDef(maps, ANTENNA_ID, "ANTENNA_ID", TpInt,
            "ID of antenna in this array","","");
  // BEAM_ID
  colMapDef(maps, BEAM_ID,"BEAM_ID",TpInt,
            "Id for BEAM model","","");
  // BEAM_OFFSET
  colMapDef(maps, BEAM_OFFSET,"BEAM_OFFSET",TpArrayDouble,
            "Beam position offset (on sky but in antenna"
            "reference frame)","rad","Direction");
  // FEED_ID
  colMapDef(maps, FEED_ID,"FEED_ID",TpInt,
            "Feed id","","");
  // FOCUS_LENGTH
  colMapDef(maps, FOCUS_LENGTH,"FOCUS_LENGTH",TpDouble,
            "Focus length","m","");
  // INTERVAL
  colMapDef(maps, INTERVAL,"INTERVAL",TpDouble,
            "Interval for which this set of parameters is accurate",
            "s","");
  // NUM_RECEPTORS
  colMapDef(maps, NUM_RECEPTORS,"NUM_RECEPTORS",TpInt,
            "Number of receptors on this feed (probably 1 or 2)","","");
  // PHASED_FEED_ID
  colMapDef(maps, PHASED_FEED_ID,"PHASED_FEED_ID",TpInt,
            "index into PHASED_FEED table (ignore if<0)","","");
  // POL_RESPONSE
  colMapDef(maps, POL_RESPONSE,"POL_RESPONSE",TpArrayComplex,
            "D-matrix i.e. leakage between two receptors","","");
  // POLARIZATION_TYPE
  colMapDef(maps, POLARIZATION_TYPE,"POLARIZATION_TYPE",TpArrayString,
            "Type of polarization to which a given RECEPTOR responds",
            "","");
  // POSITION
  colMapDef(maps, POSITION,"POSITION",TpArrayDouble,
            "Position of feed relative to feed reference position",
            "m","Position");
  // RECEPTOR_ANGLE
  colMapDef(maps, RECEPTOR_ANGLE,"RECEPTOR_ANGLE",TpArrayDouble,
            "The reference angle for polarization","rad","");
  // SPECTRAL_WINDOW_ID
  colMapDef(maps, SPECTRAL_WINDOW_ID,"SPECTRAL_WINDOW_ID",TpInt,
            "ID for this spectral window setup","","");
  // TIME
  colMapDef(maps, TIME,"TIME",TpDouble,
            "Midpoint of time for which this set of "
            "parameters is accurate","s","Epoch");
  
  // PredefinedKeywords

  // init requiredTableDesc
  // all required keywords
  uint32_t i;
  for (i = UNDEFINED_KEYWORD+1;
       i <= NUMBER_PREDEFINED_KEYWORDS; i++) {
    addKeyToDesc(maps, PredefinedKeywords(i));
  }
  // all required columns 
  // First define the columns with fixed size arrays
  IPosition shape(1,3);
  ColumnDesc::Option option=ColumnDesc::Direct;
  addColumnToDesc(maps, POSITION, shape, option);
  // define the columns with known dimensionality
  addColumnToDesc(maps, BEAM_OFFSET, 2);
  addColumnToDesc(maps, POLARIZATION_TYPE, 1);
  addColumnToDesc(maps, POL_RESPONSE, 2);
  addColumnToDesc(maps, RECEPTOR_ANGLE, 1);
  // Now define all other columns (duplicates are skipped)
  for (i = UNDEFINED_COLUMN+1; 
       i <= NUMBER_REQUIRED_COLUMNS; i++) {
    addColumnToDesc(maps, PredefinedColumns(i));
  }

  return maps;
}

MSFeed MSFeed::referenceCopy(const String& newTableName, 
			     const Block<String>& writableColumns) const
{
    return MSFeed(MSTable<MSFeedEnums>::referenceCopy
		  (newTableName,writableColumns));
}

} //# NAMESPACE CASACORE - END

