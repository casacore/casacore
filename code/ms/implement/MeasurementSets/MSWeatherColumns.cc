//# NewMSWeatherColumns.cc:  provides easy access to NewMeasurementSet columns
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

//# Includes
#include <aips/MeasurementSets/NewMSWeatherColumns.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/ColDescSet.h>

NewMSWeatherColumns::NewMSWeatherColumns(NewMSWeather& msWeather):
isNull_p(msWeather.isNull())
{
  if (!isNull()) {
    antennaId_p.attach(msWeather,NewMSWeather::columnName(NewMSWeather::ANTENNA_ID));
    interval_p.attach(msWeather,NewMSWeather::columnName(NewMSWeather::INTERVAL));
    time_p.attach(msWeather,NewMSWeather::columnName(NewMSWeather::TIME));
    timeMeas_p.attach(msWeather,NewMSWeather::columnName(NewMSWeather::TIME));
    intervalQuant_p.attach(msWeather,NewMSWeather::columnName(NewMSWeather::INTERVAL));
    timeQuant_p.attach(msWeather,NewMSWeather::columnName(NewMSWeather::TIME));
    const ColumnDescSet& cds=msWeather.tableDesc().columnDescSet();
    const String& dewPoint=NewMSWeather::columnName(NewMSWeather::DEW_POINT);
    if (cds.isDefined(dewPoint)) {
      dewPoint_p.attach(msWeather,dewPoint);
      dewPointQuant_p.attach(msWeather,dewPoint);
    }
    const String& dewPointFlag=NewMSWeather::columnName(NewMSWeather::DEW_POINT_FLAG);
    if (cds.isDefined(dewPointFlag)) 
      dewPointFlag_p.attach(msWeather,dewPointFlag);
    const String& H2O=NewMSWeather::columnName(NewMSWeather::H2O);
    if (cds.isDefined(H2O)) {
      H2O_p.attach(msWeather,H2O);
      H2OQuant_p.attach(msWeather,H2O);
    }
    const String& H2OFlag=NewMSWeather::columnName(NewMSWeather::H2O_FLAG);
    if (cds.isDefined(H2OFlag)) H2OFlag_p.attach(msWeather,H2OFlag);
    const String& ionosElectron=NewMSWeather::columnName(NewMSWeather::IONOS_ELECTRON);
    if (cds.isDefined(ionosElectron)) {
      ionosElectron_p.attach(msWeather,ionosElectron);
      ionosElectronQuant_p.attach(msWeather,ionosElectron);
    }
    const String& ionosElectronFlag=
      NewMSWeather::columnName(NewMSWeather::IONOS_ELECTRON_FLAG);
    if (cds.isDefined(ionosElectronFlag)) 
      ionosElectronFlag_p.attach(msWeather,ionosElectronFlag);
    const String& pressure=NewMSWeather::columnName(NewMSWeather::PRESSURE);
    if (cds.isDefined(pressure)) {
      pressure_p.attach(msWeather,pressure);
      pressureQuant_p.attach(msWeather,pressure);
    }
    const String& pressureFlag=NewMSWeather::columnName(NewMSWeather::PRESSURE_FLAG);
    if (cds.isDefined(pressureFlag)) pressureFlag_p.attach(msWeather,pressureFlag);
    const String& relHumidity=NewMSWeather::columnName(NewMSWeather::REL_HUMIDITY);
    if (cds.isDefined(relHumidity)) relHumidity_p.attach(msWeather,relHumidity);
    const String& relHumidityFlag=NewMSWeather::columnName(NewMSWeather::REL_HUMIDITY_FLAG);
    if (cds.isDefined(relHumidityFlag)) relHumidityFlag_p.attach(msWeather,relHumidityFlag);
    const String& temperature=NewMSWeather::columnName(NewMSWeather::TEMPERATURE);
    if (cds.isDefined(temperature)) {
      temperature_p.attach(msWeather,temperature);
      temperatureQuant_p.attach(msWeather,temperature);
    }
    const String& temperatureFlag=NewMSWeather::columnName(NewMSWeather::TEMPERATURE_FLAG);
    if (cds.isDefined(temperatureFlag)) temperatureFlag_p.attach(msWeather,temperatureFlag);
    const String& windDirection=NewMSWeather::columnName(NewMSWeather::WIND_DIRECTION);
    if (cds.isDefined(windDirection)) {
      windDirection_p.attach(msWeather,windDirection);
      windDirectionQuant_p.attach(msWeather,windDirection);
    }
    const String& windDirectionFlag=
      NewMSWeather::columnName(NewMSWeather::WIND_DIRECTION_FLAG);
    if (cds.isDefined(windDirectionFlag))
      windDirectionFlag_p.attach(msWeather,windDirectionFlag);
    const String& windSpeed=NewMSWeather::columnName(NewMSWeather::WIND_SPEED);
    if (cds.isDefined(windSpeed)) {
      windSpeed_p.attach(msWeather,windSpeed);
      windSpeedQuant_p.attach(msWeather,windSpeed);
    }
    const String& windSpeedFlag=NewMSWeather::columnName(NewMSWeather::WIND_SPEED_FLAG);
    if (cds.isDefined(windSpeedFlag)) 
      windSpeedFlag_p.attach(msWeather,windSpeedFlag);
  }
}

NewMSWeatherColumns::~NewMSWeatherColumns() {}

RONewMSWeatherColumns::RONewMSWeatherColumns(const NewMSWeather& msWeather):
isNull_p(msWeather.isNull())
{
  if (!isNull()) {
    antennaId_p.attach(msWeather,NewMSWeather::columnName(NewMSWeather::ANTENNA_ID));
    interval_p.attach(msWeather,NewMSWeather::columnName(NewMSWeather::INTERVAL));
    time_p.attach(msWeather,NewMSWeather::columnName(NewMSWeather::TIME));
    timeMeas_p.attach(msWeather,NewMSWeather::columnName(NewMSWeather::TIME));
    intervalQuant_p.attach(msWeather,NewMSWeather::columnName(NewMSWeather::INTERVAL));
    timeQuant_p.attach(msWeather,NewMSWeather::columnName(NewMSWeather::TIME));
    const ColumnDescSet& cds=msWeather.tableDesc().columnDescSet();
    const String& dewPoint=NewMSWeather::columnName(NewMSWeather::DEW_POINT);
    if (cds.isDefined(dewPoint)) {
      dewPoint_p.attach(msWeather,dewPoint);
      dewPointQuant_p.attach(msWeather,dewPoint);
    }
    const String& dewPointFlag=NewMSWeather::columnName(NewMSWeather::DEW_POINT_FLAG);
    if (cds.isDefined(dewPointFlag)) 
      dewPointFlag_p.attach(msWeather,dewPointFlag);
    const String& H2O=NewMSWeather::columnName(NewMSWeather::H2O);
    if (cds.isDefined(H2O)) {
      H2O_p.attach(msWeather,H2O);
      H2OQuant_p.attach(msWeather,H2O);
    }
    const String& H2OFlag=NewMSWeather::columnName(NewMSWeather::H2O_FLAG);
    if (cds.isDefined(H2OFlag)) H2OFlag_p.attach(msWeather,H2OFlag);
    const String& ionosElectron=NewMSWeather::columnName(NewMSWeather::IONOS_ELECTRON);
    if (cds.isDefined(ionosElectron)) {
      ionosElectron_p.attach(msWeather,ionosElectron);
      ionosElectronQuant_p.attach(msWeather,ionosElectron);
    }
    const String& ionosElectronFlag=
      NewMSWeather::columnName(NewMSWeather::IONOS_ELECTRON_FLAG);
    if (cds.isDefined(ionosElectronFlag)) 
      ionosElectronFlag_p.attach(msWeather,ionosElectronFlag);
    const String& pressure=NewMSWeather::columnName(NewMSWeather::PRESSURE);
    if (cds.isDefined(pressure)) {
      pressure_p.attach(msWeather,pressure);
      pressureQuant_p.attach(msWeather,pressure);
    }
    const String& pressureFlag=NewMSWeather::columnName(NewMSWeather::PRESSURE_FLAG);
    if (cds.isDefined(pressureFlag)) pressureFlag_p.attach(msWeather,pressureFlag);
    const String& relHumidity=NewMSWeather::columnName(NewMSWeather::REL_HUMIDITY);
    if (cds.isDefined(relHumidity)) relHumidity_p.attach(msWeather,relHumidity);
    const String& relHumidityFlag=NewMSWeather::columnName(NewMSWeather::REL_HUMIDITY_FLAG);
    if (cds.isDefined(relHumidityFlag)) relHumidityFlag_p.attach(msWeather,relHumidityFlag);
    const String& temperature=NewMSWeather::columnName(NewMSWeather::TEMPERATURE);
    if (cds.isDefined(temperature)) {
      temperature_p.attach(msWeather,temperature);
      temperatureQuant_p.attach(msWeather,temperature);
    }
    const String& temperatureFlag=NewMSWeather::columnName(NewMSWeather::TEMPERATURE_FLAG);
    if (cds.isDefined(temperatureFlag)) temperatureFlag_p.attach(msWeather,temperatureFlag);
    const String& windDirection=NewMSWeather::columnName(NewMSWeather::WIND_DIRECTION);
    if (cds.isDefined(windDirection)) {
      windDirection_p.attach(msWeather,windDirection);
      windDirectionQuant_p.attach(msWeather,windDirection);
    }
    const String& windDirectionFlag=
      NewMSWeather::columnName(NewMSWeather::WIND_DIRECTION_FLAG);
    if (cds.isDefined(windDirectionFlag))
      windDirectionFlag_p.attach(msWeather,windDirectionFlag);
    const String& windSpeed=NewMSWeather::columnName(NewMSWeather::WIND_SPEED);
    if (cds.isDefined(windSpeed)) {
      windSpeed_p.attach(msWeather,windSpeed);
      windSpeedQuant_p.attach(msWeather,windSpeed);
    }
    const String& windSpeedFlag=NewMSWeather::columnName(NewMSWeather::WIND_SPEED_FLAG);
    if (cds.isDefined(windSpeedFlag)) 
      windSpeedFlag_p.attach(msWeather,windSpeedFlag);
  }
}

RONewMSWeatherColumns::~RONewMSWeatherColumns() {}


