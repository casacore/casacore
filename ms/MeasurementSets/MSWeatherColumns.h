//# MSWeatherColumns.h: provides easy access to MSWeather columns
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

#ifndef MS_MSWEATHERCOLUMNS_H
#define MS_MSWEATHERCOLUMNS_H

#include <casacore/casa/aips.h>
#include <casacore/measures/Measures/MEpoch.h>
#include <casacore/measures/Measures/MCEpoch.h>
#include <casacore/measures/TableMeasures/ScalarMeasColumn.h>
#include <casacore/measures/TableMeasures/ScalarQuantColumn.h>
#include <casacore/tables/Tables/ScalarColumn.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class MSWeather;

// <summary>
// A class to provide easy read-only access to MSWeather columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> MSWeather
//   <li> ArrayColumn
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// ROMSWeatherColumns stands for Read-Only MeasurementSet Weather Table
// columns.
// </etymology>
//
// <synopsis>
// This class provides read-only access to the columns in the MSWeather
// Table.  It does the declaration of all the Scalar and ArrayColumns with the
// correct types, so the application programmer doesn't have to worry about
// getting those right. There is an access function for every predefined
// column. Access to non-predefined columns will still have to be done with
// explicit declarations.  See <linkto class=ROMSColumns>
// ROMSColumns</linkto> for an example.
// </synopsis>
//
// <motivation>
// See <linkto class=MSColumns> MSColumns</linkto> for the motivation.
// </motivation>

class ROMSWeatherColumns
{
public:
  // Create a columns object that accesses the data in the specified Table
  ROMSWeatherColumns(const MSWeather& msWeather);

  // The destructor does nothing special
  ~ROMSWeatherColumns();

  // Is this object defined? (MSWeather table is optional)
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
  ROMSWeatherColumns();

  //# attach this object to the supplied table.
  void attach(const MSWeather& msWeather);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  ROMSWeatherColumns(const ROMSWeatherColumns&);
  ROMSWeatherColumns& operator=(const ROMSWeatherColumns&);

  //# Check if any optional columns exist and if so attach them.
  void attachOptionalCols(const MSWeather& msWeather);

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
// A class to provide easy read-write access to MSWeather columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> MSWeather
//   <li> ArrayColumn
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// MSWeatherColumns stands for MeasurementSet Weather Table columns.
// </etymology>
//
// <synopsis>
// This class provides access to the columns in the MSWeather Table,
// it does the declaration of all the Scalar and ArrayColumns with the
// correct types, so the application programmer doesn't have to
// worry about getting those right. There is an access function
// for every predefined column. Access to non-predefined columns will still
// have to be done with explicit declarations.
// See <linkto class=MSColumns> MSColumns</linkto> for an example.
// </synopsis>
//
// <motivation>
// See <linkto class=MSColumns> MSColumns</linkto> for the motivation.
// </motivation>

class MSWeatherColumns: public ROMSWeatherColumns
{
public:
  // Create a columns object that accesses the data in the specified Table
  MSWeatherColumns(MSWeather& msWeather);

  // The destructor does nothing special
  ~MSWeatherColumns();

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
    return ROMSWeatherColumns::antennaId();}
  const ROScalarColumn<Double>& interval() const {
    return ROMSWeatherColumns::interval();}
  const ROScalarQuantColumn<Double>& intervalQuant() const {
    return ROMSWeatherColumns::intervalQuant();}
  const ROScalarColumn<Double>& time() const {
    return ROMSWeatherColumns::time();}
  const ROScalarQuantColumn<Double>& timeQuant() const {
    return ROMSWeatherColumns::timeQuant();}
  const ROScalarMeasColumn<MEpoch>& timeMeas() const {
    return ROMSWeatherColumns::timeMeas();}
  // </group>

  // Read-only access to optional columns
  // <group>
  const ROScalarColumn<Float>& dewPoint() const {
    return ROMSWeatherColumns::dewPoint();}
  const ROScalarQuantColumn<Float>& dewPointQuant() const {
    return ROMSWeatherColumns::dewPointQuant();}
  const ROScalarColumn<Bool>& dewPointFlag() const {
    return ROMSWeatherColumns::dewPointFlag();}
  const ROScalarColumn<Float>& H2O() const {
    return ROMSWeatherColumns::H2O();}
  const ROScalarColumn<Bool>& H2OFlag() const {
    return ROMSWeatherColumns::H2OFlag();}
  const ROScalarQuantColumn<Float>& H2OQuant() const {
    return ROMSWeatherColumns::H2OQuant();}
  const ROScalarColumn<Float>& ionosElectron() const {
    return ROMSWeatherColumns::ionosElectron();}
  const ROScalarQuantColumn<Float>& ionosElectronQuant() const {
    return ROMSWeatherColumns::ionosElectronQuant();}
  const ROScalarColumn<Bool>& ionosElectronFlag() const {
    return ROMSWeatherColumns::ionosElectronFlag();}
  const ROScalarColumn<Float>& pressure() const {
    return ROMSWeatherColumns::pressure();}
  const ROScalarQuantColumn<Float>& pressureQuant() const {
    return ROMSWeatherColumns::pressureQuant();}
  const ROScalarColumn<Bool>& pressureFlag() const {
    return ROMSWeatherColumns::pressureFlag();}
  const ROScalarColumn<Float>& relHumidity() const {
    return ROMSWeatherColumns::relHumidity();}
  const ROScalarColumn<Bool>& relHumidityFlag() const {
    return ROMSWeatherColumns::relHumidityFlag();}
  const ROScalarColumn<Float>& temperature() const {
    return ROMSWeatherColumns::temperature();}
  const ROScalarQuantColumn<Float>& temperatureQuant() const {
    return ROMSWeatherColumns::temperatureQuant();}
  const ROScalarColumn<Bool>& temperatureFlag() const {
    return ROMSWeatherColumns::temperatureFlag();}
  const ROScalarColumn<Float>& windDirection() const {
    return ROMSWeatherColumns::windDirection();}
  const ROScalarQuantColumn<Float>& windDirectionQuant() const {
    return ROMSWeatherColumns::windDirectionQuant();}
  const ROScalarColumn<Bool>& windDirectionFlag() const {
    return ROMSWeatherColumns::windDirectionFlag();}
  const ROScalarColumn<Float>& windSpeed() const {
    return ROMSWeatherColumns::windSpeed();}
  const ROScalarQuantColumn<Float>& windSpeedQuant() const {
    return ROMSWeatherColumns::windSpeedQuant();}
  const ROScalarColumn<Bool>& windSpeedFlag() const {
    return ROMSWeatherColumns::windSpeedFlag();}
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
  MSWeatherColumns();

  //# attach this object to the supplied table.
  void attach(MSWeather& msWeather);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  MSWeatherColumns(const MSWeatherColumns&);
  MSWeatherColumns& operator=(const MSWeatherColumns&);

  //# Check if any optional columns exist and if so attach them.
  void attachOptionalCols(MSWeather& msWeather);
  
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

} //# NAMESPACE CASACORE - END

#endif
