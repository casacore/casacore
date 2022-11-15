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
// A class to provide easy access to MSWeather columns
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

class MSWeatherColumns
{
public:
  // Create a columns object that accesses the data in the specified Table
  MSWeatherColumns(const MSWeather& msWeather);

  // The destructor does nothing special
  ~MSWeatherColumns();

  // Is this object defined? (MSWeather table is optional)
  Bool isNull() const {return isNull_p;}
  
  // Access to required columns
  // <group>
  ScalarColumn<Int>& antennaId() {return antennaId_p;}
  ScalarColumn<Double>& interval() {return interval_p;}
  ScalarQuantColumn<Double>& intervalQuant() {return intervalQuant_p;}
  ScalarColumn<Double>& time() {return time_p;}
  ScalarQuantColumn<Double>& timeQuant() {return timeQuant_p;}
  ScalarMeasColumn<MEpoch>& timeMeas() {return timeMeas_p;}
  // </group>

  // Access to optional columns
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

  // Const access to columns
  // <group>
  const ScalarColumn<Int>& antennaId() const {return antennaId_p;}
  const ScalarColumn<Double>& interval() const {return interval_p;}
  const ScalarQuantColumn<Double>& intervalQuant() const {
    return intervalQuant_p;}
  const ScalarColumn<Double>& time() const {return time_p;}
  const ScalarQuantColumn<Double>& timeQuant() const {return timeQuant_p;}
  const ScalarMeasColumn<MEpoch>& timeMeas() const {return timeMeas_p;}
  // </group>

  // Access to optional columns
  // <group>
  const ScalarColumn<Float>& dewPoint() const {return dewPoint_p;}
  const ScalarQuantColumn<Float>& dewPointQuant() const {
    return dewPointQuant_p;}
  const ScalarColumn<Bool>& dewPointFlag() const {return dewPointFlag_p;}
  const ScalarColumn<Float>& H2O() const {return H2O_p;}
  const ScalarColumn<Bool>& H2OFlag() const {return H2OFlag_p;}
  const ScalarQuantColumn<Float>& H2OQuant() const {return H2OQuant_p;}
  const ScalarColumn<Float>& ionosElectron() const {return ionosElectron_p;}
  const ScalarQuantColumn<Float>& ionosElectronQuant() const {
    return ionosElectronQuant_p;}
  const ScalarColumn<Bool>& ionosElectronFlag() const {
    return ionosElectronFlag_p;}
  const ScalarColumn<Float>& pressure() const {return pressure_p;}
  const ScalarQuantColumn<Float>& pressureQuant() const {
    return pressureQuant_p;}
  const ScalarColumn<Bool>& pressureFlag() const {return pressureFlag_p;}
  const ScalarColumn<Float>& relHumidity() const {return relHumidity_p;}
  const ScalarColumn<Bool>& relHumidityFlag() const {
    return relHumidityFlag_p;}
  const ScalarColumn<Float>& temperature() const {return temperature_p;}
  const ScalarQuantColumn<Float>& temperatureQuant() const {
    return temperatureQuant_p;}
  const ScalarColumn<Bool>& temperatureFlag() const {
    return temperatureFlag_p;}
  const ScalarColumn<Float>& windDirection() const {return windDirection_p;}
  const ScalarQuantColumn<Float>& windDirectionQuant() const {
    return windDirectionQuant_p;}
  const ScalarColumn<Bool>& windDirectionFlag() const {
    return windDirectionFlag_p;}
  const ScalarColumn<Float>& windSpeed() const {return windSpeed_p;}
  const ScalarQuantColumn<Float>& windSpeedQuant() const {
    return windSpeedQuant_p;}
  const ScalarColumn<Bool>& windSpeedFlag() const {return windSpeedFlag_p;}
  // </group>

  // Convenience function that returns the number of rows in any of the
  // columns. Returns zero if the object is null.
  rownr_t nrow() const {return isNull() ? 0 : antennaId_p.nrow();}

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
  void attach(const MSWeather& msWeather);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  MSWeatherColumns(const MSWeatherColumns&);
  MSWeatherColumns& operator=(const MSWeatherColumns&);

  //# Check if any optional columns exist and if so attach them.
  void attachOptionalCols(const MSWeather& msWeather);
  
  //# Is the object not attached to a Table.
  Bool isNull_p;

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

//# Define the RO version for backward compatibility.
typedef MSWeatherColumns ROMSWeatherColumns;

} //# NAMESPACE CASACORE - END

#endif
