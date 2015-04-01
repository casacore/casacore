//# MSWeatherColumns.cc:  provides easy access to MeasurementSet columns
//# Copyright (C) 1996,1999,2000
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

#include <casacore/ms/MeasurementSets/MSWeatherColumns.h>
#include <casacore/ms/MeasurementSets/MSWeather.h>
#include <casacore/tables/Tables/ColDescSet.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

ROMSWeatherColumns::ROMSWeatherColumns(const MSWeather& msWeather):
  isNull_p(True),
  antennaId_p(),
  interval_p(),
  time_p(),
  dewPoint_p(),
  dewPointFlag_p(),
  H2O_p(),
  H2OFlag_p(),
  ionosElectron_p(),
  ionosElectronFlag_p(),
  pressure_p(),
  pressureFlag_p(),
  relHumidity_p(),
  relHumidityFlag_p(),
  temperature_p(),
  temperatureFlag_p(),
  windDirection_p(),
  windDirectionFlag_p(),
  windSpeed_p(),
  windSpeedFlag_p(),
  timeMeas_p(),
  intervalQuant_p(),
  timeQuant_p(),
  dewPointQuant_p(),
  H2OQuant_p(),
  ionosElectronQuant_p(),
  pressureQuant_p(),
  temperatureQuant_p(),
  windDirectionQuant_p(),
  windSpeedQuant_p()
{
  attach(msWeather);
}

ROMSWeatherColumns::~ROMSWeatherColumns() {}

ROMSWeatherColumns::ROMSWeatherColumns():
  isNull_p(True),
  antennaId_p(),
  interval_p(),
  time_p(),
  dewPoint_p(),
  dewPointFlag_p(),
  H2O_p(),
  H2OFlag_p(),
  ionosElectron_p(),
  ionosElectronFlag_p(),
  pressure_p(),
  pressureFlag_p(),
  relHumidity_p(),
  relHumidityFlag_p(),
  temperature_p(),
  temperatureFlag_p(),
  windDirection_p(),
  windDirectionFlag_p(),
  windSpeed_p(),
  windSpeedFlag_p(),
  timeMeas_p(),
  intervalQuant_p(),
  timeQuant_p(),
  dewPointQuant_p(),
  H2OQuant_p(),
  ionosElectronQuant_p(),
  pressureQuant_p(),
  temperatureQuant_p(),
  windDirectionQuant_p(),
  windSpeedQuant_p()
{
}

void ROMSWeatherColumns::attach(const MSWeather& msWeather)
{
  isNull_p = msWeather.isNull();
  if (!isNull()) {
    antennaId_p.attach(msWeather, MSWeather::
		       columnName(MSWeather::ANTENNA_ID));
    interval_p.attach(msWeather, MSWeather::
		      columnName(MSWeather::INTERVAL));
    time_p.attach(msWeather, MSWeather::
		  columnName(MSWeather::TIME));
    timeMeas_p.attach(msWeather, MSWeather::
		      columnName(MSWeather::TIME));
    intervalQuant_p.attach(msWeather, MSWeather::
			   columnName(MSWeather::INTERVAL));
    timeQuant_p.attach(msWeather, MSWeather::
		       columnName(MSWeather::TIME));
    const ColumnDescSet& cds = msWeather.tableDesc().columnDescSet();
    const String& dewPoint = MSWeather::columnName(MSWeather::DEW_POINT);
    if (cds.isDefined(dewPoint)) {
      dewPoint_p.attach(msWeather, dewPoint);
      dewPointQuant_p.attach(msWeather, dewPoint);
    }
    const String& dewPointFlag = MSWeather::
      columnName(MSWeather::DEW_POINT_FLAG);
    if (cds.isDefined(dewPointFlag)) {
      dewPointFlag_p.attach(msWeather, dewPointFlag);
    }
    const String& H2O = MSWeather::columnName(MSWeather::H2O);
    if (cds.isDefined(H2O)) {
      H2O_p.attach(msWeather, H2O);
      H2OQuant_p.attach(msWeather, H2O);
    }
    const String& H2OFlag = MSWeather::columnName(MSWeather::H2O_FLAG);
    if (cds.isDefined(H2OFlag)) H2OFlag_p.attach(msWeather, H2OFlag);
    const String& ionosElectron =
      MSWeather::columnName(MSWeather::IONOS_ELECTRON);
    if (cds.isDefined(ionosElectron)) {
      ionosElectron_p.attach(msWeather, ionosElectron);
      ionosElectronQuant_p.attach(msWeather, ionosElectron);
    }
    const String& ionosElectronFlag = 
      MSWeather::columnName(MSWeather::IONOS_ELECTRON_FLAG);
    if (cds.isDefined(ionosElectronFlag)) {
      ionosElectronFlag_p.attach(msWeather, ionosElectronFlag);
    }
    const String& pressure = MSWeather::columnName(MSWeather::PRESSURE);
    if (cds.isDefined(pressure)) {
      pressure_p.attach(msWeather, pressure);
      pressureQuant_p.attach(msWeather, pressure);
    }
    const String& pressureFlag =
      MSWeather::columnName(MSWeather::PRESSURE_FLAG);
    if (cds.isDefined(pressureFlag)) {
      pressureFlag_p.attach(msWeather, pressureFlag);
    }
    const String& relHumidity =
      MSWeather::columnName(MSWeather::REL_HUMIDITY);
    if (cds.isDefined(relHumidity)) {
      relHumidity_p.attach(msWeather, relHumidity);
    }
    const String& relHumidityFlag =
      MSWeather::columnName(MSWeather::REL_HUMIDITY_FLAG);
    if (cds.isDefined(relHumidityFlag)) {
      relHumidityFlag_p.attach(msWeather, relHumidityFlag);
    }
    const String& temperature =
      MSWeather::columnName(MSWeather::TEMPERATURE);
    if (cds.isDefined(temperature)) {
      temperature_p.attach(msWeather, temperature);
      temperatureQuant_p.attach(msWeather, temperature);
    }
    const String& temperatureFlag =
      MSWeather::columnName(MSWeather::TEMPERATURE_FLAG);
    if (cds.isDefined(temperatureFlag)) {
      temperatureFlag_p.attach(msWeather, temperatureFlag);
    }
    const String& windDirection = 
      MSWeather::columnName(MSWeather::WIND_DIRECTION);
    if (cds.isDefined(windDirection)) {
      windDirection_p.attach(msWeather, windDirection);
      windDirectionQuant_p.attach(msWeather, windDirection);
    }
    const String& windDirectionFlag = 
      MSWeather::columnName(MSWeather::WIND_DIRECTION_FLAG);
    if (cds.isDefined(windDirectionFlag)) {
      windDirectionFlag_p.attach(msWeather, windDirectionFlag);
    }
    const String& windSpeed =
      MSWeather::columnName(MSWeather::WIND_SPEED);
    if (cds.isDefined(windSpeed)) {
      windSpeed_p.attach(msWeather, windSpeed);
      windSpeedQuant_p.attach(msWeather, windSpeed);
    }
    const String& windSpeedFlag =
      MSWeather::columnName(MSWeather::WIND_SPEED_FLAG);
    if (cds.isDefined(windSpeedFlag)) 
      windSpeedFlag_p.attach(msWeather, windSpeedFlag);
  }
}

MSWeatherColumns::MSWeatherColumns(MSWeather& msWeather):
  ROMSWeatherColumns(),
  antennaId_p(),
  interval_p(),
  time_p(),
  dewPoint_p(),
  dewPointFlag_p(),
  H2O_p(),
  H2OFlag_p(),
  ionosElectron_p(),
  ionosElectronFlag_p(),
  pressure_p(),
  pressureFlag_p(),
  relHumidity_p(),
  relHumidityFlag_p(),
  temperature_p(),
  temperatureFlag_p(),
  windDirection_p(),
  windDirectionFlag_p(),
  windSpeed_p(),
  windSpeedFlag_p(),
  timeMeas_p(),
  intervalQuant_p(),
  timeQuant_p(),
  dewPointQuant_p(),
  H2OQuant_p(),
  ionosElectronQuant_p(),
  pressureQuant_p(),
  temperatureQuant_p(),
  windDirectionQuant_p(),
  windSpeedQuant_p()
{
  attach(msWeather);
}

MSWeatherColumns::~MSWeatherColumns() {}


void MSWeatherColumns::
setEpochRef(MEpoch::Types ref, Bool tableMustBeEmpty) {
  timeMeas_p.setDescRefCode(ref, tableMustBeEmpty);
}

MSWeatherColumns::MSWeatherColumns():
  ROMSWeatherColumns(),
  antennaId_p(),
  interval_p(),
  time_p(),
  dewPoint_p(),
  dewPointFlag_p(),
  H2O_p(),
  H2OFlag_p(),
  ionosElectron_p(),
  ionosElectronFlag_p(),
  pressure_p(),
  pressureFlag_p(),
  relHumidity_p(),
  relHumidityFlag_p(),
  temperature_p(),
  temperatureFlag_p(),
  windDirection_p(),
  windDirectionFlag_p(),
  windSpeed_p(),
  windSpeedFlag_p(),
  timeMeas_p(),
  intervalQuant_p(),
  timeQuant_p(),
  dewPointQuant_p(),
  H2OQuant_p(),
  ionosElectronQuant_p(),
  pressureQuant_p(),
  temperatureQuant_p(),
  windDirectionQuant_p(),
  windSpeedQuant_p()
{
}

void MSWeatherColumns::attach(MSWeather& msWeather)
{
  ROMSWeatherColumns::attach(msWeather);
  if (!isNull()) {
    antennaId_p.attach(msWeather, MSWeather::
		       columnName(MSWeather::ANTENNA_ID));
    interval_p.attach(msWeather, MSWeather::
		      columnName(MSWeather::INTERVAL));
    time_p.attach(msWeather, MSWeather::columnName(MSWeather::TIME));
    timeMeas_p.attach(msWeather, MSWeather::columnName(MSWeather::TIME));
    intervalQuant_p.attach(msWeather, MSWeather::
			   columnName(MSWeather::INTERVAL));
    timeQuant_p.attach(msWeather, MSWeather::
		       columnName(MSWeather::TIME));
    const ColumnDescSet& cds = msWeather.tableDesc().columnDescSet();
    const String& dewPoint = MSWeather::columnName(MSWeather::DEW_POINT);
    if (cds.isDefined(dewPoint)) {
      dewPoint_p.attach(msWeather, dewPoint);
      dewPointQuant_p.attach(msWeather, dewPoint);
    }
    const String& dewPointFlag =
      MSWeather::columnName(MSWeather::DEW_POINT_FLAG);
    if (cds.isDefined(dewPointFlag)) {
      dewPointFlag_p.attach(msWeather, dewPointFlag);
    }
    const String& H2O = MSWeather::columnName(MSWeather::H2O);
    if (cds.isDefined(H2O)) {
      H2O_p.attach(msWeather, H2O);
      H2OQuant_p.attach(msWeather, H2O);
    }
    const String& H2OFlag = MSWeather::columnName(MSWeather::H2O_FLAG);
    if (cds.isDefined(H2OFlag)) H2OFlag_p.attach(msWeather, H2OFlag);
    const String& ionosElectron =
      MSWeather::columnName(MSWeather::IONOS_ELECTRON);
    if (cds.isDefined(ionosElectron)) {
      ionosElectron_p.attach(msWeather, ionosElectron);
      ionosElectronQuant_p.attach(msWeather, ionosElectron);
    }
    const String& ionosElectronFlag = 
      MSWeather::columnName(MSWeather::IONOS_ELECTRON_FLAG);
    if (cds.isDefined(ionosElectronFlag)) {
      ionosElectronFlag_p.attach(msWeather, ionosElectronFlag);
    }
    const String& pressure = MSWeather::columnName(MSWeather::PRESSURE);
    if (cds.isDefined(pressure)) {
      pressure_p.attach(msWeather, pressure);
      pressureQuant_p.attach(msWeather, pressure);
    }
    const String& pressureFlag =
      MSWeather::columnName(MSWeather::PRESSURE_FLAG);
    if (cds.isDefined(pressureFlag)) {
      pressureFlag_p.attach(msWeather, pressureFlag);
    }
    const String& relHumidity =
      MSWeather::columnName(MSWeather::REL_HUMIDITY);
    if (cds.isDefined(relHumidity)) {
      relHumidity_p.attach(msWeather, relHumidity);
    }
    const String& relHumidityFlag =
      MSWeather::columnName(MSWeather::REL_HUMIDITY_FLAG);
    if (cds.isDefined(relHumidityFlag)) {
      relHumidityFlag_p.attach(msWeather, relHumidityFlag);
    }
    const String& temperature =
      MSWeather::columnName(MSWeather::TEMPERATURE);
    if (cds.isDefined(temperature)) {
      temperature_p.attach(msWeather, temperature);
      temperatureQuant_p.attach(msWeather, temperature);
    }
    const String& temperatureFlag =
      MSWeather::columnName(MSWeather::TEMPERATURE_FLAG);
    if (cds.isDefined(temperatureFlag)) {
      temperatureFlag_p.attach(msWeather, temperatureFlag);
    }
    const String& windDirection =
      MSWeather::columnName(MSWeather::WIND_DIRECTION);
    if (cds.isDefined(windDirection)) {
      windDirection_p.attach(msWeather, windDirection);
      windDirectionQuant_p.attach(msWeather, windDirection);
    }
    const String& windDirectionFlag = 
      MSWeather::columnName(MSWeather::WIND_DIRECTION_FLAG);
    if (cds.isDefined(windDirectionFlag)) {
      windDirectionFlag_p.attach(msWeather, windDirectionFlag);
    }
    const String& windSpeed =
      MSWeather::columnName(MSWeather::WIND_SPEED);
    if (cds.isDefined(windSpeed)) {
      windSpeed_p.attach(msWeather, windSpeed);
      windSpeedQuant_p.attach(msWeather, windSpeed);
    }
    const String& windSpeedFlag =
      MSWeather::columnName(MSWeather::WIND_SPEED_FLAG);
    if (cds.isDefined(windSpeedFlag)) {
      windSpeedFlag_p.attach(msWeather, windSpeedFlag);
    }
  }
}
// Local Variables: 
// compile-command: "gmake MSWeatherColumns"
// End: 

} //# NAMESPACE CASACORE - END

