//# NewMSWeatherColumns.h: provides easy access to NewMSWeather columns
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

#if !defined(AIPS_NewMSWEATHERCOLUMNS_H)
#define AIPS_NewMSWEATHERCOLUMNS_H

#include <aips/MeasurementSets/NewMSWeather.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Tables/ArrayColumn.h>
#include <aips/Quanta/Unit.h>
#include <aips/Quanta/Quantum.h>
#include <aips/TableMeasures/ScalarMeasColumn.h>
#include <aips/TableMeasures/ScalarQuantColumn.h>
class MEpoch;

// <summary>
// A convenience class to provide easy access to NewMSWeather columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> NewMSWeather
//   <li> ArrayColumn
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// NewMSWeatherColumns stands for NewMeasurementSet Weather Table columns.
// </etymology>
//
// <synopsis>
// This class provides access to the columns in the NewMSWeather Table,
// it does the declaration of all the Scalar and ArrayColumns with the
// correct types, so the application programmer doesn't have to
// worry about getting those right. There is an access function
// for every predefined column. Access to non-predefined columns will still
// have to be done with explicit declarations.
// See <linkto class=NewMSColumns> NewMSColumns</linkto> for an example.
// </synopsis>
//
// <motivation>
// See <linkto class=NewMSColumns> NewMSColumns</linkto> for the motivation.
// </motivation>

class NewMSWeatherColumns
{
public:

  NewMSWeatherColumns(NewMSWeather& msWeather);

  ~NewMSWeatherColumns();

  // Is this object defined? (NewMSWeather table is optional)
  Bool isNull() {return isNull_p;}
  
  // Access to columns
  ScalarColumn<Int>& antennaId() {return antennaId_p;}
  ScalarColumn<Double>& interval() {return interval_p;}
  ScalarColumn<Double>& time() {return time_p;}
  ScalarColumn<Float>& dewPoint() {return dewPoint_p;}
  ScalarColumn<Bool>& dewPointFlag() {return dewPointFlag_p;}
  ScalarColumn<Float>& H2O() {return H2O_p;}
  ScalarColumn<Bool>& H2OFlag() {return H2OFlag_p;}
  ScalarColumn<Float>& ionosElectron() {return ionosElectron_p;}
  ScalarColumn<Bool>& ionosElectronFlag() {return ionosElectronFlag_p;}
  ScalarColumn<Float>& pressure() {return pressure_p;}
  ScalarColumn<Bool>& pressureFlag() {return pressureFlag_p;}
  ScalarColumn<Float>& relHumidity() {return relHumidity_p;}
  ScalarColumn<Bool>& relHumidityFlag() {return relHumidityFlag_p;}
  ScalarColumn<Float>& temperature() {return temperature_p;}
  ScalarColumn<Bool>& temperatureFlag() {return temperatureFlag_p;}
  ScalarColumn<Float>& windDirection() {return windDirection_p;}
  ScalarColumn<Bool>& windDirectionFlag() {return windDirectionFlag_p;}
  ScalarColumn<Float>& windSpeed() {return windSpeed_p;}
  ScalarColumn<Bool>& windSpeedFlag() {return windSpeedFlag_p;}

  // Access to Measure columns
  ScalarMeasColumn<MEpoch>& timeMeas() { return timeMeas_p;}

  // Access to Quantum columns
  ScalarQuantColumn<Double>& intervalQuant() { return intervalQuant_p;}
  ScalarQuantColumn<Double>& timeQuant() { return timeQuant_p;}
  ScalarQuantColumn<Float>& dewPointQuant() { return dewPointQuant_p;}
  ScalarQuantColumn<Float>& H2OQuant() { return H2OQuant_p;}
  ScalarQuantColumn<Float>& ionosElectronQuant() { return ionosElectronQuant_p;}
  ScalarQuantColumn<Float>& pressureQuant() { return pressureQuant_p;}
  ScalarQuantColumn<Float>& temperatureQuant() { return temperatureQuant_p;}
  ScalarQuantColumn<Float>& windDirectionQuant() { return windDirectionQuant_p;}
  ScalarQuantColumn<Float>& windSpeedQuant() { return windSpeedQuant_p;}

private:

  Bool isNull_p;
  ScalarColumn<Int> antennaId_p;
  ScalarColumn<Double> interval_p;
  ScalarColumn<Double> time_p;
  ScalarColumn<Float> dewPoint_p;
  ScalarColumn<Bool> dewPointFlag_p;
  ScalarColumn<Float> H2O_p;
  ScalarColumn<Bool> H2OFlag_p;
  ScalarColumn<Float> ionosElectron_p;
  ScalarColumn<Bool> ionosElectronFlag_p;
  ScalarColumn<Float> pressure_p;
  ScalarColumn<Bool> pressureFlag_p;
  ScalarColumn<Float> relHumidity_p;
  ScalarColumn<Bool> relHumidityFlag_p;
  ScalarColumn<Float> temperature_p;
  ScalarColumn<Bool> temperatureFlag_p;
  ScalarColumn<Float> windDirection_p;
  ScalarColumn<Bool> windDirectionFlag_p;
  ScalarColumn<Float> windSpeed_p;
  ScalarColumn<Bool> windSpeedFlag_p;

  // Access to Measure columns
  ScalarMeasColumn<MEpoch> timeMeas_p;

  // Access to Quantum columns
  ScalarQuantColumn<Double> intervalQuant_p;
  ScalarQuantColumn<Double> timeQuant_p;
  ScalarQuantColumn<Float> dewPointQuant_p;
  ScalarQuantColumn<Float> H2OQuant_p;
  ScalarQuantColumn<Float> ionosElectronQuant_p;
  ScalarQuantColumn<Float> pressureQuant_p;
  ScalarQuantColumn<Float> temperatureQuant_p;
  ScalarQuantColumn<Float> windDirectionQuant_p;
  ScalarQuantColumn<Float> windSpeedQuant_p;
};

// <summary>
// A convenience class to provide easy access to NewMSWeather columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> NewMSWeather
//   <li> ArrayColumn
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// RONewMSWeatherColumns stands for Read-Only NewMeasurementSet Weather Table columns.
// </etymology>
//
// <synopsis>
// This class provides read-only access to the columns in the NewMSWeather Table.
// It does the declaration of all the Scalar and ArrayColumns with the
// correct types, so the application programmer doesn't have to
// worry about getting those right. There is an access function
// for every predefined column. Access to non-predefined columns will still
// have to be done with explicit declarations.
// See <linkto class=RONewMSColumns> RONewMSColumns</linkto> for an example.
// </synopsis>
//
// <motivation>
// See <linkto class=NewMSColumns> NewMSColumns</linkto> for the motivation.
// </motivation>

class RONewMSWeatherColumns
{
public:

  RONewMSWeatherColumns(const NewMSWeather& msWeather);

  ~RONewMSWeatherColumns();

  // Is this object defined? (NewMSWeather table is optional)
  Bool isNull() {return isNull_p;}
  
  // Access to columns
  const ROScalarColumn<Int>& antennaId() const {return antennaId_p;}
  const ROScalarColumn<Double>& interval() const {return interval_p;}
  const ROScalarColumn<Double>& time() const {return time_p;}
  const ROScalarColumn<Float>& dewPoint() const {return dewPoint_p;}
  const ROScalarColumn<Bool>& dewPointFlag() const {return dewPointFlag_p;}
  const ROScalarColumn<Float>& H2O() const {return H2O_p;}
  const ROScalarColumn<Bool>& H2OFlag() const {return H2OFlag_p;}
  const ROScalarColumn<Float>& ionosElectron() const {return ionosElectron_p;}
  const ROScalarColumn<Bool>& ionosElectronFlag() const {return ionosElectronFlag_p;}
  const ROScalarColumn<Float>& pressure() const {return pressure_p;}
  const ROScalarColumn<Bool>& pressureFlag() const {return pressureFlag_p;}
  const ROScalarColumn<Float>& relHumidity() const {return relHumidity_p;}
  const ROScalarColumn<Bool>& relHumidityFlag() const {return relHumidityFlag_p;}
  const ROScalarColumn<Float>& temperature() const {return temperature_p;}
  const ROScalarColumn<Bool>& temperatureFlag() const {return temperatureFlag_p;}
  const ROScalarColumn<Float>& windDirection() const {return windDirection_p;}
  const ROScalarColumn<Bool>& windDirectionFlag() const {return windDirectionFlag_p;}
  const ROScalarColumn<Float>& windSpeed() const {return windSpeed_p;}
  const ROScalarColumn<Bool>& windSpeedFlag() const {return windSpeedFlag_p;}

  // Access to Measure columns
  const ROScalarMeasColumn<MEpoch>& timeMeas() { return timeMeas_p;}

  // Access to Quantum columns
  const ROScalarQuantColumn<Double>& intervalQuant() const { return intervalQuant_p;}
  const ROScalarQuantColumn<Double>& timeQuant() const { return timeQuant_p;}
  const ROScalarQuantColumn<Float>& dewPointQuant() const { return dewPointQuant_p;}
  const ROScalarQuantColumn<Float>& H2OQuant() const { return H2OQuant_p;}
  const ROScalarQuantColumn<Float>& ionosElectronQuant() const { return ionosElectronQuant_p;}
  const ROScalarQuantColumn<Float>& pressureQuant() const { return pressureQuant_p;}
  const ROScalarQuantColumn<Float>& temperatureQuant() const { return temperatureQuant_p;}
  const ROScalarQuantColumn<Float>& windDirectionQuant() const { return windDirectionQuant_p;}
  const ROScalarQuantColumn<Float>& windSpeedQuant() const { return windSpeedQuant_p;}

private:

  Bool isNull_p;
  ROScalarColumn<Int> antennaId_p;
  ROScalarColumn<Double> interval_p;
  ROScalarColumn<Double> time_p;
  ROScalarColumn<Float> dewPoint_p;
  ROScalarColumn<Bool> dewPointFlag_p;
  ROScalarColumn<Float> H2O_p;
  ROScalarColumn<Bool> H2OFlag_p;
  ROScalarColumn<Float> ionosElectron_p;
  ROScalarColumn<Bool> ionosElectronFlag_p;
  ROScalarColumn<Float> pressure_p;
  ROScalarColumn<Bool> pressureFlag_p;
  ROScalarColumn<Float> relHumidity_p;
  ROScalarColumn<Bool> relHumidityFlag_p;
  ROScalarColumn<Float> temperature_p;
  ROScalarColumn<Bool> temperatureFlag_p;
  ROScalarColumn<Float> windDirection_p;
  ROScalarColumn<Bool> windDirectionFlag_p;
  ROScalarColumn<Float> windSpeed_p;
  ROScalarColumn<Bool> windSpeedFlag_p;

  // Access to Measure columns
  ROScalarMeasColumn<MEpoch> timeMeas_p;

  // Access to Quantum columns
  ROScalarQuantColumn<Double> intervalQuant_p;
  ROScalarQuantColumn<Double> timeQuant_p;
  ROScalarQuantColumn<Float> dewPointQuant_p;
  ROScalarQuantColumn<Float> H2OQuant_p;
  ROScalarQuantColumn<Float> ionosElectronQuant_p;
  ROScalarQuantColumn<Float> pressureQuant_p;
  ROScalarQuantColumn<Float> temperatureQuant_p;
  ROScalarQuantColumn<Float> windDirectionQuant_p;
  ROScalarQuantColumn<Float> windSpeedQuant_p;
};

#endif
