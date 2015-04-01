//# SDWeatherHandler.cc: a WEATHER handler for SDFITS data  
//# Copyright (C) 2000,2001
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
#include <casacore/msfits/MSFits/SDWeatherHandler.h>

#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/ms/MeasurementSets/MSWeatherColumns.h>
#include <casacore/ms/MeasurementSets/MSWeather.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/casa/BasicMath/Math.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

SDWeatherHandler::SDWeatherHandler() 
    : msWeather_p(0), msWeatherCols_p(0), rownr_p(-1), humidityId_p(-1),
      tambientId_p(-1), pressureId_p(-1), dewpointId_p(-1), windspeeId_p(-1),
      winddireId_p(-1)
{;}

SDWeatherHandler::SDWeatherHandler(MeasurementSet &ms, Vector<Bool> &handledCols, 
				 const Record &row)
    : msWeather_p(0), msWeatherCols_p(0), rownr_p(-1), humidityId_p(-1),
      tambientId_p(-1), pressureId_p(-1),dewpointId_p(-1), windspeeId_p(-1),
      winddireId_p(-1)
{
    initAll(ms, handledCols, row);
}

SDWeatherHandler::SDWeatherHandler(const SDWeatherHandler &other) 
    : msWeather_p(0), msWeatherCols_p(0), rownr_p(-1), humidityId_p(-1),
      tambientId_p(-1), pressureId_p(-1), dewpointId_p(-1), windspeeId_p(-1), 
      winddireId_p(-1)
{
    *this = other;
}

SDWeatherHandler &SDWeatherHandler::operator=(const SDWeatherHandler &other)
{
    if (this != &other) {
	clearAll();
	msWeather_p = new MSWeather(*(other.msWeather_p));
	AlwaysAssert(msWeather_p, AipsError);
	msWeatherCols_p = new MSWeatherColumns(*msWeather_p);
	AlwaysAssert(msWeatherCols_p, AipsError);
	rownr_p = other.rownr_p;
	humidityId_p = other.humidityId_p;
	tambientId_p = other.tambientId_p;
	pressureId_p = other.pressureId_p;
	dewpointId_p = other.dewpointId_p;
	windspeeId_p = other.windspeeId_p;
	winddireId_p = other.winddireId_p;
	H2OField_p = other.H2OField_p;
	ionosElectronField_p = other.ionosElectronField_p;
	timeField_p = other.timeField_p;
	intervalField_p = other.intervalField_p;
	pressureField_p = other.pressureField_p;
	humidityField_p = other.humidityField_p;
	temperatureField_p = other.temperatureField_p;
	windDirField_p = other.windDirField_p;
	windSpeedField_p = other.windSpeedField_p;
    }
    return *this;
}

void SDWeatherHandler::attach(MeasurementSet &ms, Vector<Bool> &handledCols, 
			     const Record &row)
{
    clearAll();
    initAll(ms, handledCols, row);
}

void SDWeatherHandler::resetRow(const Record &row)
{
    clearRow();
    Vector<Bool> dummyHandledCols;
    initRow(dummyHandledCols, row);
}

void SDWeatherHandler::fill(const Record &row, Int antennaId, Double time, 
			    Vector<Double> &timeRange)
{
    // don't bother unless there is something there and something to add
    if (msWeather_p && 
	(humidityId_p >= 0 || tambientId_p >= 0 || pressureId_p >= 0 ||
	 dewpointId_p >= 0 || windspeeId_p >= 0 || winddireId_p >= 0 ||
	 (H2OField_p.isAttached() && !isNaN(*H2OField_p) && !isInf(*H2OField_p)) ||
	 (ionosElectronField_p.isAttached() && !isNaN(*ionosElectronField_p) && !isInf(*ionosElectronField_p)))) {

	Float thisHumidity, thisTambient, thisDewpoint, thisWindspee, thisWinddire, thisPressure;
	thisHumidity = thisTambient = thisDewpoint = thisWindspee = 
	    thisWinddire = thisPressure = 0.0;

	if (humidityId_p >= 0) thisHumidity = row.asFloat(humidityId_p);
	else if (humidityField_p.isAttached()) thisHumidity = *humidityField_p;

	if (tambientId_p >= 0) thisTambient = row.asFloat(tambientId_p);
	else if (temperatureField_p.isAttached()) thisTambient = *temperatureField_p;

	if (pressureId_p >= 0) thisPressure = row.asFloat(pressureId_p);
	else if (pressureField_p.isAttached()) thisPressure = *pressureField_p;

	if (dewpointId_p >= 0) thisDewpoint = row.asFloat(dewpointId_p);

	if (windspeeId_p >= 0) thisWindspee = row.asFloat(windspeeId_p);
	else if (windSpeedField_p.isAttached()) thisWindspee = *windSpeedField_p;

	if (winddireId_p >= 0) thisWinddire = row.asFloat(winddireId_p);
	else if (windDirField_p.isAttached()) thisWinddire = *windDirField_p;

	Bool newRow = rownr_p < 0;
	if (!newRow && !msWeatherCols_p->relHumidity().isNull()) {
	    newRow = thisHumidity != msWeatherCols_p->relHumidity()(rownr_p);
	}
	if (!newRow && !msWeatherCols_p->temperature().isNull()) {
	    newRow = thisTambient != msWeatherCols_p->temperature()(rownr_p);
	}
	if (!newRow && !msWeatherCols_p->pressure().isNull()) {
	    newRow = thisPressure != msWeatherCols_p->pressure()(rownr_p);
	}
	if (!newRow && !msWeatherCols_p->dewPoint().isNull()) {
	    newRow = thisDewpoint != msWeatherCols_p->dewPoint()(rownr_p);
	}
	if (!newRow && !msWeatherCols_p->windSpeed().isNull()) {
	    newRow = thisWindspee != msWeatherCols_p->windSpeed()(rownr_p);
	}
	if (!newRow && !msWeatherCols_p->windDirection().isNull()) {
	    newRow = thisWinddire != msWeatherCols_p->windDirection()(rownr_p);
	}
	if (!newRow && H2OField_p.isAttached() && !isNaN(*H2OField_p) && !isInf(*H2OField_p)) {
	    // we seem to have a valid H2O value
	    newRow = msWeatherCols_p->H2O().isNull();
	    newRow = !newRow && msWeatherCols_p->H2O()(rownr_p) != *H2OField_p;
	}
	if (!newRow && ionosElectronField_p.isAttached() && !isNaN(*ionosElectronField_p) && 
	    !isInf(*ionosElectronField_p)) {
	    // we seem to have a valid IONOS_ELECTRON value
	    newRow = msWeatherCols_p->ionosElectron().isNull();
	    newRow = !newRow && msWeatherCols_p->ionosElectron()(rownr_p) != *ionosElectronField_p;
	}
	newRow = newRow || antennaId != msWeatherCols_p->antennaId()(rownr_p);;

	Double interval = timeRange(1) - timeRange(0);
	Double thisTime = time;
	// or should former MS time and interval be used here instead
	if (timeField_p.isAttached()) {
	    thisTime = *timeField_p;
	    // MS interval can't exist without MS time
	    if (intervalField_p.isAttached()) {
		interval = *intervalField_p;
	    }
	}
	if (!newRow) {
	    // all of the fields except TIME and INTERVAL match.
	    // if the time falls within the row interval of the row time
	    // or the row time falls within the interval of time, then the rows overlap and
	    // can be reused
	    Double rowTime = msWeatherCols_p->time()(rownr_p);
	    Double rowInterval = msWeatherCols_p->interval()(rownr_p);
	    Double rid2 = rowInterval/2.0;
	    Double id2 = interval/2.0;
	    newRow = !(((time-id2)<(rowTime+rid2)) && 
		       ((rowTime-rid2)<(time+id2)));
	}

	if (newRow) {
	    // fill it
	    rownr_p = msWeather_p->nrow();
	    msWeather_p->addRow();
	    msWeatherCols_p->antennaId().put(rownr_p,antennaId);
	    msWeatherCols_p->time().put(rownr_p, thisTime);
	    msWeatherCols_p->interval().put(rownr_p, interval);
	    if (!msWeatherCols_p->relHumidity().isNull()) {
		msWeatherCols_p->relHumidity().put(rownr_p, thisHumidity);
		msWeatherCols_p->relHumidityFlag().put(rownr_p, False);
	    }
	    if (!msWeatherCols_p->temperature().isNull()) {
		msWeatherCols_p->temperature().put(rownr_p, thisTambient);
		msWeatherCols_p->temperatureFlag().put(rownr_p, False);
	    }
	    if (!msWeatherCols_p->pressure().isNull()) {
		msWeatherCols_p->pressure().put(rownr_p, thisPressure);
		msWeatherCols_p->pressureFlag().put(rownr_p, False);
	    }
	    if (!msWeatherCols_p->dewPoint().isNull()) {
		msWeatherCols_p->dewPoint().put(rownr_p, thisDewpoint);
		msWeatherCols_p->dewPointFlag().put(rownr_p, False);
	    }
	    if (!msWeatherCols_p->windSpeed().isNull()) {
		msWeatherCols_p->windSpeed().put(rownr_p, thisWindspee);
		msWeatherCols_p->windSpeedFlag().put(rownr_p, False);
	    }
	    if (!msWeatherCols_p->windDirection().isNull()) {
		msWeatherCols_p->windDirection().put(rownr_p, thisWinddire);
		msWeatherCols_p->windDirectionFlag().put(rownr_p, False);
	    }
	    if (H2OField_p.isAttached()) {
		if (msWeatherCols_p->H2O().isNull()) {
		    if (!isNaN(*H2OField_p) && !isInf(*H2OField_p)) {
			// need to add this column
			delete msWeatherCols_p;
			msWeatherCols_p = 0;
			TableDesc td;
			MSWeather::addColumnToDesc(td, MSWeather::H2O);
			MSWeather::addColumnToDesc(td, MSWeather::H2O_FLAG);
			msWeather_p->addColumn(td[0]);
			msWeather_p->addColumn(td[1]);
			msWeatherCols_p = new MSWeatherColumns(*msWeather_p);
			AlwaysAssert(msWeatherCols_p, AipsError);
			msWeatherCols_p->H2O().put(rownr_p, *H2OField_p);
			msWeatherCols_p->H2OFlag().put(rownr_p, False);
		    }
		} else {
		    msWeatherCols_p->H2O().put(rownr_p, *H2OField_p);
		    msWeatherCols_p->H2OFlag().put(rownr_p, False);
		}
	    }
	    if (ionosElectronField_p.isAttached()) {
		if (msWeatherCols_p->ionosElectron().isNull()) {
		    if (!isNaN(*ionosElectronField_p) && !isInf(*ionosElectronField_p)) {
			// need to add this column
			delete msWeatherCols_p;
			msWeatherCols_p = 0;
			TableDesc td;
			MSWeather::addColumnToDesc(td, MSWeather::IONOS_ELECTRON);
			MSWeather::addColumnToDesc(td, MSWeather::IONOS_ELECTRON_FLAG);
			msWeather_p->addColumn(td[0]);
			msWeather_p->addColumn(td[1]);
			msWeatherCols_p = new MSWeatherColumns(*msWeather_p);
			AlwaysAssert(msWeatherCols_p, AipsError);
			msWeatherCols_p->ionosElectron().put(rownr_p, *ionosElectronField_p);
			msWeatherCols_p->ionosElectronFlag().put(rownr_p, False);
		    }
		} else {
		    msWeatherCols_p->ionosElectron().put(rownr_p, *ionosElectronField_p);
		    msWeatherCols_p->ionosElectronFlag().put(rownr_p, False);
		}
	    }
	} else {
	    // reuse this row, make sure that the time range is fully set
	    // and place the time in the center of it
	    Double rowTime = msWeatherCols_p->time()(rownr_p);
	    Double rowInterval = msWeatherCols_p->interval()(rownr_p);
	    Double minTime, maxTime;
	    minTime = min(time-interval/2.0, rowTime-rowInterval/2.0);
	    maxTime = max(time+interval/2.0, rowTime+rowInterval/2.0);
	    msWeatherCols_p->time().put(rownr_p, (maxTime+minTime)/2.0);
	    msWeatherCols_p->interval().put(rownr_p, (maxTime-minTime));
	}
    }
}

void SDWeatherHandler::clearAll()
{
    delete msWeather_p;
    msWeather_p = 0;

    delete msWeatherCols_p;
    msWeatherCols_p = 0;

    clearRow();
}

void SDWeatherHandler::clearRow()
{
    humidityId_p = tambientId_p = pressureId_p = dewpointId_p = 
	windspeeId_p = winddireId_p = -1; 

    H2OField_p.detach();
    ionosElectronField_p.detach();
    timeField_p.detach();
    intervalField_p.detach();
    pressureField_p.detach();
    humidityField_p.detach();
    temperatureField_p.detach();
    windDirField_p.detach();
    windSpeedField_p.detach();
}

void SDWeatherHandler::initAll(MeasurementSet &ms, Vector<Bool> &handledCols,
			      const Record &row)
{
    msWeather_p = new MSWeather(ms.weather());
    AlwaysAssert(msWeather_p, AipsError);

    initRow(handledCols, row);

    // do we need to add any optional columns
    TableDesc td;
    if (humidityId_p >= 0 || humidityField_p.isAttached()) {
	MSWeather::addColumnToDesc(td,MSWeather::REL_HUMIDITY);
	MSWeather::addColumnToDesc(td,MSWeather::REL_HUMIDITY_FLAG);
    }
    if (tambientId_p >= 0 || temperatureField_p.isAttached()) {
	MSWeather::addColumnToDesc(td,MSWeather::TEMPERATURE);
	MSWeather::addColumnToDesc(td,MSWeather::TEMPERATURE_FLAG);
    }
    if (pressureId_p >= 0 || pressureField_p.isAttached()) {
	MSWeather::addColumnToDesc(td,MSWeather::PRESSURE);
	MSWeather::addColumnToDesc(td,MSWeather::PRESSURE_FLAG);
    }
    if (dewpointId_p >= 0) {
	MSWeather::addColumnToDesc(td,MSWeather::DEW_POINT);
	MSWeather::addColumnToDesc(td,MSWeather::DEW_POINT_FLAG);
    }
    if (windspeeId_p >= 0 || windSpeedField_p.isAttached()) {
	MSWeather::addColumnToDesc(td,MSWeather::WIND_SPEED);
	MSWeather::addColumnToDesc(td,MSWeather::WIND_SPEED_FLAG);
    }
    if (winddireId_p >= 0 || windDirField_p.isAttached()) {
	MSWeather::addColumnToDesc(td,MSWeather::WIND_DIRECTION);
	MSWeather::addColumnToDesc(td,MSWeather::WIND_DIRECTION_FLAG);
    }
    for (uInt i=0;i<td.ncolumn();i++) {
	msWeather_p->addColumn(td[i]);
    }

    msWeatherCols_p = new MSWeatherColumns(*msWeather_p);
    AlwaysAssert(msWeatherCols_p, AipsError);
}

void SDWeatherHandler::initRow(Vector<Bool> &handledCols, const Record &row)
{
    humidityId_p = row.fieldNumber("HUMIDITY");
    if (humidityId_p >= 0) handledCols(humidityId_p) = True;
    tambientId_p = row.fieldNumber("TAMBIENT");
    if (tambientId_p >= 0) handledCols(tambientId_p) = True;    
    pressureId_p = row.fieldNumber("PRESSURE");
    if (pressureId_p >= 0) handledCols(pressureId_p) = True;    
    dewpointId_p = row.fieldNumber("DEWPOINT");
    if (dewpointId_p >= 0) handledCols(dewpointId_p) = True;    
    windspeeId_p = row.fieldNumber("WINDSPEE");
    if (windspeeId_p >= 0) handledCols(windspeeId_p) = True;    
    winddireId_p = row.fieldNumber("WINDDIRE");
    if (winddireId_p >= 0) handledCols(winddireId_p) = True; 

    Int tmp;
    tmp = row.fieldNumber("WEATHER_H2O");
    if (tmp >= 0 && row.dataType(tmp) == TpFloat) {
	H2OField_p.attachToRecord(row, tmp);
	handledCols(tmp) = True;
    }
    tmp = row.fieldNumber("WEATHER_IONOS_ELECTRON");
    if (tmp >= 0 && row.dataType(tmp) == TpFloat) {
	ionosElectronField_p.attachToRecord(row, tmp);
	handledCols(tmp) = True;
    }
    tmp = row.fieldNumber("WEATHER_TIME");
    if (tmp >= 0 && row.dataType(tmp) == TpDouble) {
	timeField_p.attachToRecord(row, tmp);
	handledCols(tmp) = True;
    }
    tmp = row.fieldNumber("WEATHER_INTERVAL");
    if (tmp >= 0 && row.dataType(tmp) == TpDouble) {
	intervalField_p.attachToRecord(row, tmp);
	handledCols(tmp) = True;
    }
    tmp = row.fieldNumber("WEATHER_PRESSURE");
    if (tmp >= 0 && row.dataType(tmp) == TpFloat) {
	pressureField_p.attachToRecord(row, tmp);
	handledCols(tmp) = True;
    }
    tmp = row.fieldNumber("WEATHER_REL_HUMIDITY");
    if (tmp >= 0 && row.dataType(tmp) == TpFloat) {
	humidityField_p.attachToRecord(row, tmp);
	handledCols(tmp) = True;
    }
    tmp = row.fieldNumber("WEATHER_TEMPERATURE");
    if (tmp >= 0 && row.dataType(tmp) == TpFloat) {
	temperatureField_p.attachToRecord(row, tmp);
	handledCols(tmp) = True;
    }
    tmp = row.fieldNumber("WEATHER_WIND_DIRECTION");
    if (tmp >= 0 && row.dataType(tmp) == TpFloat) {
	windDirField_p.attachToRecord(row, tmp);
	handledCols(tmp) = True;
    }
    tmp = row.fieldNumber("WEATHER_WIND_SPEED");
    if (tmp >= 0 && row.dataType(tmp) == TpFloat) {
	windSpeedField_p.attachToRecord(row, tmp);
	handledCols(tmp) = True;
    }


    rownr_p = -1;
}

} //# NAMESPACE CASACORE - END

