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

#if !defined(AIPS_NEWMSWEATHERCOLUMNS_H)
#define AIPS_NEWMSWEATHERCOLUMNS_H

#include <aips/aips.h>
#include <aips/Measures/MEpoch.h>
#include <aips/TableMeasures/ScalarMeasColumn.h>
#include <aips/TableMeasures/ScalarQuantColumn.h>
#include <aips/Tables/ScalarColumn.h>

class NewMSWeather;

// <summary>
// A class to provide easy read-only access to NewMSWeather columns
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
// RONewMSWeatherColumns stands for Read-Only NewMeasurementSet Weather Table
// columns.
// </etymology>
//
// <synopsis>
// This class provides read-only access to the columns in the NewMSWeather
// Table.  It does the declaration of all the Scalar and ArrayColumns with the
// correct types, so the application programmer doesn't have to worry about
// getting those right. There is an access function for every predefined
// column. Access to non-predefined columns will still have to be done with
// explicit declarations.  See <linkto class=RONewMSColumns>
// RONewMSColumns</linkto> for an example.
// </synopsis>
//
// <motivation>
// See <linkto class=NewMSColumns> NewMSColumns</linkto> for the motivation.
// </motivation>

class RONewMSWeatherColumns
{
public:
  // Create a columns object that accesses the data in the specified Table
  RONewMSWeatherColumns(const NewMSWeather& msWeather);

  // The destructor does nothing special
  ~RONewMSWeatherColumns();

  // Is this object defined? (NewMSWeather table is optional)
  Bool isNull() const {return isNull_p;}
  
  // Access to columns
  // <group>
  const ROScalarColumn<Int>& antennaId() const {return antennaId_p;}
  const ROScalarColumn<Double>& interval() const {return interval_p;}
  const ROScalarQuantColumn<Double>& intervalQuant() const {
    return intervalQuant_p;}
  const ROScalarColumn<Double>& time() const {return time_p;}
  const ROScalarQuantColumn<Double>& timeQuant() const {return timeQuant_p;}
  const ROScalarMeasColumn<MEpoch>& timeMeas() const {return timeMeas_p;}
  // </group>

  // Access to optional columns
  // <group>
  const ROScalarColumn<Float>& dewPoint() const {return dewPoint_p;}
  const ROScalarQuantColumn<Float>& dewPointQuant() const {
    return dewPointQuant_p;}
  const ROScalarColumn<Bool>& dewPointFlag() const {return dewPointFlag_p;}
  const ROScalarColumn<Float>& H2O() const {return H2O_p;}
  const ROScalarColumn<Bool>& H2OFlag() const {return H2OFlag_p;}
  const ROScalarQuantColumn<Float>& H2OQuant() const {return H2OQuant_p;}
  const ROScalarColumn<Float>& ionosElectron() const {return ionosElectron_p;}
  const ROScalarQuantColumn<Float>& ionosElectronQuant() const {
    return ionosElectronQuant_p;}
  const ROScalarColumn<Bool>& ionosElectronFlag() const {
    return ionosElectronFlag_p;}
  const ROScalarColumn<Float>& pressure() const {return pressure_p;}
  const ROScalarQuantColumn<Float>& pressureQuant() const {
    return pressureQuant_p;}
  const ROScalarColumn<Bool>& pressureFlag() const {return pressureFlag_p;}
  const ROScalarColumn<Float>& relHumidity() const {return relHumidity_p;}
  const ROScalarColumn<Bool>& relHumidityFlag() const {
    return relHumidityFlag_p;}
  const ROScalarColumn<Float>& temperature() const {return temperature_p;}
  const ROScalarQuantColumn<Float>& temperatureQuant() const {
    return temperatureQuant_p;}
  const ROScalarColumn<Bool>& temperatureFlag() const {
    return temperatureFlag_p;}
  const ROScalarColumn<Float>& windDirection() const {return windDirection_p;}
  const ROScalarQuantColumn<Float>& windDirectionQuant() const {
    return windDirectionQuant_p;}
  const ROScalarColumn<Bool>& windDirectionFlag() const {
    return windDirectionFlag_p;}
  const ROScalarColumn<Float>& windSpeed() const {return windSpeed_p;}
  const ROScalarQuantColumn<Float>& windSpeedQuant() const {
    return windSpeedQuant_p;}
  const ROScalarColumn<Bool>& windSpeedFlag() const {return windSpeedFlag_p;}
  // </group>

  // Convenience function that returns the number of rows in any of the
  // columns. Returns zero if the object is null.
  uInt nrow() const {return isNull() ? 0 : antennaId_p.nrow();}

protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  RONewMSWeatherColumns();

  //# attach this object to the supplied table.
  void attach(const NewMSWeather& msWeather);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  RONewMSWeatherColumns(const RONewMSWeatherColumns&);
  RONewMSWeatherColumns& operator=(const RONewMSWeatherColumns&);

  //# Check if any optional columns exist and if so attach them.
  void attachOptionalCols(const NewMSWeather& msWeather);

  //# Is the object not attached to a Table.
  Bool isNull_p;

  //# required columns
  ROScalarColumn<Int> antennaId_p;
  ROScalarColumn<Double> interval_p;
  ROScalarColumn<Double> time_p;
  //# optional columns
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

  //# Access to Measure columns
  ROScalarMeasColumn<MEpoch> timeMeas_p;

  //# Access to Quantum columns
  ROScalarQuantColumn<Double> intervalQuant_p;
  ROScalarQuantColumn<Double> timeQuant_p;
  //# optional Quantum columns
  ROScalarQuantColumn<Float> dewPointQuant_p;
  ROScalarQuantColumn<Float> H2OQuant_p;
  ROScalarQuantColumn<Float> ionosElectronQuant_p;
  ROScalarQuantColumn<Float> pressureQuant_p;
  ROScalarQuantColumn<Float> temperatureQuant_p;
  ROScalarQuantColumn<Float> windDirectionQuant_p;
  ROScalarQuantColumn<Float> windSpeedQuant_p;
};

// <summary>
// A class to provide easy read-write access to NewMSWeather columns
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

class NewMSWeatherColumns: public RONewMSWeatherColumns
{
public:
  // Create a columns object that accesses the data in the specified Table
  NewMSWeatherColumns(NewMSWeather& msWeather);

  // The destructor does nothing special
  ~NewMSWeatherColumns();

  // Read-write access to required columns
  // <group>
  ScalarColumn<Int>& antennaId() {return antennaId_p;}
  ScalarColumn<Double>& interval() {return interval_p;}
  ScalarQuantColumn<Double>& intervalQuant() {return intervalQuant_p;}
  ScalarColumn<Double>& time() {return time_p;}
  ScalarQuantColumn<Double>& timeQuant() {return timeQuant_p;}
  ScalarMeasColumn<MEpoch>& timeMeas() {return timeMeas_p;}
  // </group>

  // Read-write access to optional columns
  // <group>
  ScalarColumn<Float>& dewPoint() {return dewPoint_p;}
  ScalarQuantColumn<Float>& dewPointQuant() {return dewPointQuant_p;}
  ScalarColumn<Bool>& dewPointFlag() {return dewPointFlag_p;}
  ScalarColumn<Float>& H2O() {return H2O_p;}
  ScalarQuantColumn<Float>& H2OQuant() {return H2OQuant_p;}
  ScalarColumn<Bool>& H2OFlag() {return H2OFlag_p;}
  ScalarColumn<Float>& ionosElectron() {return ionosElectron_p;}
  ScalarQuantColumn<Float>& ionosElectronQuant() {return ionosElectronQuant_p;}
  ScalarColumn<Bool>& ionosElectronFlag() {return ionosElectronFlag_p;}
  ScalarColumn<Float>& pressure() {return pressure_p;}
  ScalarQuantColumn<Float>& pressureQuant() {return pressureQuant_p;}
  ScalarColumn<Bool>& pressureFlag() {return pressureFlag_p;}
  ScalarColumn<Float>& relHumidity() {return relHumidity_p;}
  ScalarColumn<Bool>& relHumidityFlag() {return relHumidityFlag_p;}
  ScalarColumn<Float>& temperature() {return temperature_p;}
  ScalarQuantColumn<Float>& temperatureQuant() {return temperatureQuant_p;}
  ScalarColumn<Bool>& temperatureFlag() {return temperatureFlag_p;}
  ScalarColumn<Float>& windDirection() {return windDirection_p;}
  ScalarQuantColumn<Float>& windDirectionQuant() {return windDirectionQuant_p;}
  ScalarColumn<Bool>& windDirectionFlag() {return windDirectionFlag_p;}
  ScalarColumn<Float>& windSpeed() {return windSpeed_p;}
  ScalarQuantColumn<Float>& windSpeedQuant() {return windSpeedQuant_p;}
  ScalarColumn<Bool>& windSpeedFlag() {return windSpeedFlag_p;}
  // </group>

  // Read-only access to required columns
  // <group>
  const ROScalarColumn<Int>& antennaId() const {
    return RONewMSWeatherColumns::antennaId();}
  const ROScalarColumn<Double>& interval() const {
    return RONewMSWeatherColumns::interval();}
  const ROScalarQuantColumn<Double>& intervalQuant() const {
    return RONewMSWeatherColumns::intervalQuant();}
  const ROScalarColumn<Double>& time() const {
    return RONewMSWeatherColumns::time();}
  const ROScalarQuantColumn<Double>& timeQuant() const {
    return RONewMSWeatherColumns::timeQuant();}
  const ROScalarMeasColumn<MEpoch>& timeMeas() const {
    return RONewMSWeatherColumns::timeMeas();}
  // </group>

  // Read-only access to optional columns
  // <group>
  const ROScalarColumn<Float>& dewPoint() const {
    return RONewMSWeatherColumns::dewPoint();}
  const ROScalarQuantColumn<Float>& dewPointQuant() const {
    return RONewMSWeatherColumns::dewPointQuant();}
  const ROScalarColumn<Bool>& dewPointFlag() const {
    return RONewMSWeatherColumns::dewPointFlag();}
  const ROScalarColumn<Float>& H2O() const {
    return RONewMSWeatherColumns::H2O();}
  const ROScalarColumn<Bool>& H2OFlag() const {
    return RONewMSWeatherColumns::H2OFlag();}
  const ROScalarQuantColumn<Float>& H2OQuant() const {
    return RONewMSWeatherColumns::H2OQuant();}
  const ROScalarColumn<Float>& ionosElectron() const {
    return RONewMSWeatherColumns::ionosElectron();}
  const ROScalarQuantColumn<Float>& ionosElectronQuant() const {
    return RONewMSWeatherColumns::ionosElectronQuant();}
  const ROScalarColumn<Bool>& ionosElectronFlag() const {
    return RONewMSWeatherColumns::ionosElectronFlag();}
  const ROScalarColumn<Float>& pressure() const {
    return RONewMSWeatherColumns::pressure();}
  const ROScalarQuantColumn<Float>& pressureQuant() const {
    return RONewMSWeatherColumns::pressureQuant();}
  const ROScalarColumn<Bool>& pressureFlag() const {
    return RONewMSWeatherColumns::pressureFlag();}
  const ROScalarColumn<Float>& relHumidity() const {
    return RONewMSWeatherColumns::relHumidity();}
  const ROScalarColumn<Bool>& relHumidityFlag() const {
    return RONewMSWeatherColumns::relHumidityFlag();}
  const ROScalarColumn<Float>& temperature() const {
    return RONewMSWeatherColumns::temperature();}
  const ROScalarQuantColumn<Float>& temperatureQuant() const {
    return RONewMSWeatherColumns::temperatureQuant();}
  const ROScalarColumn<Bool>& temperatureFlag() const {
    return RONewMSWeatherColumns::temperatureFlag();}
  const ROScalarColumn<Float>& windDirection() const {
    return RONewMSWeatherColumns::windDirection();}
  const ROScalarQuantColumn<Float>& windDirectionQuant() const {
    return RONewMSWeatherColumns::windDirectionQuant();}
  const ROScalarColumn<Bool>& windDirectionFlag() const {
    return RONewMSWeatherColumns::windDirectionFlag();}
  const ROScalarColumn<Float>& windSpeed() const {
    return RONewMSWeatherColumns::windSpeed();}
  const ROScalarQuantColumn<Float>& windSpeedQuant() const {
    return RONewMSWeatherColumns::windSpeedQuant();}
  const ROScalarColumn<Bool>& windSpeedFlag() const {
    return RONewMSWeatherColumns::windSpeedFlag();}
  // </group>

  // set the epoch type for the TIME column.
  // <note role=tip>
  // In principle this function can only be used if the table is empty,
  // otherwise already written values may thereafter have an incorrect
  // reference, offset, or unit.  However, it is possible that part of the
  // table gets written before these values are known.  In that case the
  // reference, offset, or units can be set by using a False
  // <src>tableMustBeEmpty</src> argument.
  // </note>
  void setEpochRef(MEpoch::Types ref, Bool tableMustBeEmpty=True);

protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  NewMSWeatherColumns();

  //# attach this object to the supplied table.
  void attach(NewMSWeather& msWeather);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  NewMSWeatherColumns(const NewMSWeatherColumns&);
  NewMSWeatherColumns& operator=(const NewMSWeatherColumns&);

  //# Check if any optional columns exist and if so attach them.
  void attachOptionalCols(NewMSWeather& msWeather);
  
  //# required columns
  ScalarColumn<Int> antennaId_p;
  ScalarColumn<Double> interval_p;
  ScalarColumn<Double> time_p;
  //# optional columns
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

  //# Access to Measure columns
  ScalarMeasColumn<MEpoch> timeMeas_p;

  //# Access to Quantum columns
  ScalarQuantColumn<Double> intervalQuant_p;
  ScalarQuantColumn<Double> timeQuant_p;
  //# optional Quantum columns
  ScalarQuantColumn<Float> dewPointQuant_p;
  ScalarQuantColumn<Float> H2OQuant_p;
  ScalarQuantColumn<Float> ionosElectronQuant_p;
  ScalarQuantColumn<Float> pressureQuant_p;
  ScalarQuantColumn<Float> temperatureQuant_p;
  ScalarQuantColumn<Float> windDirectionQuant_p;
  ScalarQuantColumn<Float> windSpeedQuant_p;
};
#endif
