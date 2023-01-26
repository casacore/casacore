//# MSWeather.cc: The MeasurementSet WEATHER Table
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

#include <casacore/ms/MeasurementSets/MSWeather.h>
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

MSWeather::MSWeather():hasBeenDestroyed_p(true) { }

MSWeather::MSWeather(const String &tableName, TableOption option) 
    : MSTable<MSWeatherEnums>(tableName, option),hasBeenDestroyed_p(false)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSWeather(String &, TableOption) - "
			 "table is not a valid MSWeather"));
}

MSWeather::MSWeather(const String& tableName, const String &tableDescName,
			       TableOption option)
    : MSTable<MSWeatherEnums>(tableName, tableDescName,option),
      hasBeenDestroyed_p(false)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSWeather(String &, String &, TableOption) - "
			 "table is not a valid MSWeather"));
}

MSWeather::MSWeather(SetupNewTable &newTab, rownr_t nrrow,
			       bool initialize)
    : MSTable<MSWeatherEnums>(newTab, nrrow, initialize), 
      hasBeenDestroyed_p(false)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSWeather(SetupNewTable &, rownr_t, bool) - "
			 "table is not a valid MSWeather"));
}

MSWeather::MSWeather(const Table &table)
    : MSTable<MSWeatherEnums>(table), hasBeenDestroyed_p(false)
{
    // verify that the now opened table is valid
    if (! validate(this->tableDesc()))
	throw (AipsError("MSWeather(const Table &) - "
			 "table is not a valid MSWeather"));
}

MSWeather::MSWeather(const MSWeather &other)
    : MSTable<MSWeatherEnums>(other), 
      hasBeenDestroyed_p(false)
{
    // verify that other is valid
    if (&other != this) 
	if (! validate(this->tableDesc()))
	    throw (AipsError("MSWeather(const MSWeather &) - "
			     "table is not a valid MSWeather"));
}

MSWeather::~MSWeather()
{
// check to make sure that this MSWeather is still valid
    if (!hasBeenDestroyed_p  &&  !validate()) {
	// the table is otherwise OK, so ensure that it is written if necessary
	this->flush();
        LogIO os;
        os << LogIO::WARN
           << "~MSWeather() - Table written is not a valid MSWeather"
           << LogIO::POST;
    }
    hasBeenDestroyed_p = true;
}


MSWeather& MSWeather::operator=(const MSWeather &other)
{
    if (&other != this) {
	MSTable<MSWeatherEnums>::operator=(other);
	hasBeenDestroyed_p=other.hasBeenDestroyed_p;
    }
    return *this;
}

MSTableMaps MSWeather::initMaps()
{
  MSTableMaps maps;
  // the PredefinedColumns
  // ANTENNA_ID
  colMapDef(maps, ANTENNA_ID, "ANTENNA_ID", TpInt,
            "Antenna number","","");
  // INTERVAL
  colMapDef(maps, INTERVAL, "INTERVAL", TpDouble,
            "Interval over which data is relevant","s","");
  // TIME
  colMapDef(maps, TIME, "TIME", TpDouble,
            "An MEpoch specifying the midpoint of the time for"
            "which data is relevant","s","Epoch");
  // DEW_POINT
  colMapDef(maps, DEW_POINT, "DEW_POINT", TpFloat,
            "Dew point","K","");
  // DEW_POINT_FLAG
  colMapDef(maps, DEW_POINT_FLAG, "DEW_POINT_FLAG", TpBool,
            "Flag for dew point","","");
  // H2O 
  colMapDef(maps, H2O, "H2O", TpFloat,
            "Average column density of water-vapor","m-2","");
  // H2O_FLAG
  colMapDef(maps, H2O_FLAG, "H2O_FLAG", TpBool,
            "Flag for average column density of water-vapor","","");
  // IONOS_ELECTRON
  colMapDef(maps, IONOS_ELECTRON, "IONOS_ELECTRON", TpFloat,
            "Average column density of electrons","m-2","");
  // IONOS_ELECTRON_FLAG
  colMapDef(maps, IONOS_ELECTRON_FLAG, "IONOS_ELECTRON_FLAG", TpBool,
            "Flag for average column density of electrons","","");
  // PRESSURE
  colMapDef(maps, PRESSURE, "PRESSURE", TpFloat,
            "Ambient atmospheric pressure","hPa","");
  // PRESSURE_FLAG
  colMapDef(maps, PRESSURE_FLAG, "PRESSURE_FLAG", TpBool,
            "Flag for ambient atmospheric pressure","","");
  // REL_HUMIDITY
  colMapDef(maps, REL_HUMIDITY, "REL_HUMIDITY", TpFloat,
            "Ambient relative humidity","%","");
  // REL_HUMIDITY_FLAG
  colMapDef(maps, REL_HUMIDITY_FLAG, "REL_HUMIDITY_FLAG", TpBool,
            "Flag for ambient relative humidity","","");
  // TEMPERATURE
  colMapDef(maps, TEMPERATURE, "TEMPERATURE", TpFloat,
            "Ambient Air Temperature for an antenna","K","");
  // TEMPERATURE_FLAG
  colMapDef(maps, TEMPERATURE_FLAG, "TEMPERATURE_FLAG", TpBool,
            "Flag for ambient Air Temperature for an antenna","","");
  // WIND_DIRECTION
  colMapDef(maps, WIND_DIRECTION, "WIND_DIRECTION", TpFloat,
            "Average wind direction","rad","");
  // WIND_DIRECTION_FLAG
  colMapDef(maps, WIND_DIRECTION_FLAG, "WIND_DIRECTION_FLAG", TpBool,
            "Flag for wind direction","","");
  // WIND_SPEED
  colMapDef(maps, WIND_SPEED, "WIND_SPEED", TpFloat,
            "Average wind speed","m/s","");
  // WIND_SPEED_FLAG
  colMapDef(maps, WIND_SPEED_FLAG, "WIND_SPEED_FLAG", TpBool,
            "Flag for wind speed","","");
  // PredefinedKeywords

  // init requiredTableDesc
  // all required keywords
  uint32_t i;
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

MSWeather MSWeather::referenceCopy(const String& newTableName, 
				   const Block<String>& writableColumns) const
{
    return MSWeather(MSTable<MSWeatherEnums>::
		     referenceCopy(newTableName,writableColumns));
}

} //# NAMESPACE CASACORE - END

