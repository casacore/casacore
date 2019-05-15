//# MSSource.cc: The MeasurementSet SOURCE Table
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

#include <casacore/ms/MeasurementSets/MSSource.h>
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

MSSource::MSSource():hasBeenDestroyed_p(True) { }

MSSource::MSSource(const String &tableName, TableOption option) 
  : MSTable<MSSourceEnums>(tableName, option),
    hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSSource(String &, TableOption) - "
			 "table is not a valid MSSource"));
}

MSSource::MSSource(const String& tableName, const String &tableDescName,
			       TableOption option)
    : MSTable<MSSourceEnums>(tableName, tableDescName,option),
      hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSSource(String &, String &, TableOption) - "
			 "table is not a valid MSSource"));
}

MSSource::MSSource(SetupNewTable &newTab, uInt nrrow,
			       Bool initialize)
    : MSTable<MSSourceEnums>(newTab, nrrow, initialize), 
      hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSSource(SetupNewTable &, uInt, Bool) - "
			 "table is not a valid MSSource"));
}

MSSource::MSSource(const Table &table)
    : MSTable<MSSourceEnums>(table), hasBeenDestroyed_p(False)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSSource(const Table &) - "
			 "table is not a valid MSSource"));
}

MSSource::MSSource(const MSSource &other)
    : MSTable<MSSourceEnums>(other), 
      hasBeenDestroyed_p(False)
{
    // verify that other is valid
    if (&other != this) 
	if (! validate(this->tableDesc()))
	    throw (AipsError("MSSource(const MSSource &) - "
			     "table is not a valid MSSource"));
}

MSSource::~MSSource()
{
// check to make sure that this MSSource is still valid
    if (!hasBeenDestroyed_p  &&  !validate()) {
	// the table is otherwise OK, so ensure that it is written if necessary
	this->flush();
        LogIO os;
        os << LogIO::WARN
           << "~MSSource() - Table written is not a valid MSSource"
           << LogIO::POST;
    }
    hasBeenDestroyed_p = True;
}


MSSource& MSSource::operator=(const MSSource &other)
{
    if (&other != this) {
	MSTable<MSSourceEnums>::operator=(other);
	hasBeenDestroyed_p=other.hasBeenDestroyed_p;
    }
    return *this;
}

MSTableMaps MSSource::initMaps()
{
  MSTableMaps maps;
  // the PredefinedColumns
  // CALIBRATION_GROUP 
  colMapDef(maps, CALIBRATION_GROUP, "CALIBRATION_GROUP", TpInt,
            "Number of grouping for calibration purpose.","","");
  // CODE
  colMapDef(maps, CODE, "CODE", TpString,
            "Special characteristics of source, "
            "e.g. Bandpass calibrator","","");
  // DIRECTION 
  colMapDef(maps, DIRECTION, "DIRECTION", TpArrayDouble,
            "Direction (e.g. RA, DEC).","rad","Direction");
  // INTERVAL
  colMapDef(maps, INTERVAL, "INTERVAL", TpDouble,
            "Interval of time for which this set of parameters "
            "is accurate","s","");
  // NAME
  colMapDef(maps, NAME, "NAME", TpString,
            "Name of source as given during observations","","");
  // NUM_LINES
  colMapDef(maps, NUM_LINES, "NUM_LINES", TpInt,
            "Number of spectral lines","","");
  // POSITION
  colMapDef(maps, POSITION, "POSITION", TpArrayDouble,
            "Position (e.g. for solar system objects",
            "m","Position");
  // PROPER_MOTION
  colMapDef(maps, PROPER_MOTION, "PROPER_MOTION", TpArrayDouble,
            "Proper motion","rad/s","");
  // PULSAR_ID
  colMapDef(maps, PULSAR_ID, "PULSAR_ID", TpInt,
            "Pulsar Id, pointer to pulsar table","","");
  // REST_FREQUENCY
  colMapDef(maps, REST_FREQUENCY, "REST_FREQUENCY", TpArrayDouble,
            "Line rest frequency","Hz","Frequency");
  // SOURCE_ID
  colMapDef(maps, SOURCE_ID, "SOURCE_ID", TpInt,
            "Source id","","");
  // SOURCE_MODEL
  colMapDef(maps, SOURCE_MODEL, "SOURCE_MODEL", TpRecord,
            "Component Source Model","","");
  // SPECTRAL_WINDOW_ID
  colMapDef(maps, SPECTRAL_WINDOW_ID,"SPECTRAL_WINDOW_ID",TpInt,
            "ID for this spectral window setup","","");
  // SYSVEL
  colMapDef(maps, SYSVEL, "SYSVEL", TpArrayDouble,
            "Systemic velocity at reference","m/s","Radialvelocity");
  // TIME
  colMapDef(maps, TIME, "TIME", TpDouble,
            "Midpoint of time for which this set of parameters "
            "is accurate.","s","Epoch");
  // TRANSITION
  colMapDef(maps, TRANSITION, "TRANSITION", TpArrayString,
            "Line Transition name","","");
  // PredefinedKeywords

  // init requiredTableDesc
  // all required keywords
  uInt i;
  for (i = UNDEFINED_KEYWORD+1;
       i <= NUMBER_PREDEFINED_KEYWORDS; i++) {
    addKeyToDesc(maps, PredefinedKeywords(i));
  }
  // all required columns 
  // First define the columns with fixed size arrays
  IPosition shape(1,2);
  ColumnDesc::Option option=ColumnDesc::Direct;
  addColumnToDesc(maps, DIRECTION, shape, option);
  addColumnToDesc(maps, PROPER_MOTION, shape, option);
  // Now define all other columns (duplicates are skipped)
  for (i = UNDEFINED_COLUMN+1; 
       i <= NUMBER_REQUIRED_COLUMNS; i++) {
    addColumnToDesc(maps, PredefinedColumns(i));
  }

  return maps;
}

	
MSSource MSSource::referenceCopy(const String& newTableName, 
				 const Block<String>& writableColumns) const
{
    return MSSource(MSTable<MSSourceEnums>::
		     referenceCopy(newTableName,writableColumns));
}

} //# NAMESPACE CASACORE - END

