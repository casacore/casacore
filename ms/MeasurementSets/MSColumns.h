//# MSColumns.h: provides easy access to MeasurementSet columns
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

#ifndef MS_MSCOLUMNS_H
#define MS_MSCOLUMNS_H

#include <casacore/casa/aips.h>
#include <casacore/measures/Measures/MDirection.h>
#include <casacore/measures/Measures/MEpoch.h>
#include <casacore/measures/Measures/MCPosition.h>
#include <casacore/measures/TableMeasures/ArrayMeasColumn.h>
#include <casacore/measures/TableMeasures/ArrayQuantColumn.h>
#include <casacore/measures/TableMeasures/ScalarMeasColumn.h>
#include <casacore/measures/TableMeasures/ScalarQuantColumn.h>
#include <casacore/ms/MeasurementSets/MSAntennaColumns.h>
#include <casacore/ms/MeasurementSets/MSDataDescColumns.h>
#include <casacore/ms/MeasurementSets/MSDopplerColumns.h>
#include <casacore/ms/MeasurementSets/MSFeedColumns.h>
#include <casacore/ms/MeasurementSets/MSFieldColumns.h>
#include <casacore/ms/MeasurementSets/MSFlagCmdColumns.h>
#include <casacore/ms/MeasurementSets/MSFreqOffColumns.h>
#include <casacore/ms/MeasurementSets/MSHistoryColumns.h>
#include <casacore/ms/MeasurementSets/MSMainColumns.h>
#include <casacore/ms/MeasurementSets/MSObsColumns.h>
#include <casacore/ms/MeasurementSets/MSPointingColumns.h>
#include <casacore/ms/MeasurementSets/MSPolColumns.h>
#include <casacore/ms/MeasurementSets/MSProcessorColumns.h>
#include <casacore/ms/MeasurementSets/MSSourceColumns.h>
#include <casacore/ms/MeasurementSets/MSSpWindowColumns.h>
#include <casacore/ms/MeasurementSets/MSStateColumns.h>
#include <casacore/ms/MeasurementSets/MSSysCalColumns.h>
#include <casacore/ms/MeasurementSets/MSWeatherColumns.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class MeasurementSet;


// <summary>
// A class to provide easy access to MeasurementSet columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> MeasurementSet
//   <li> ArrayColumn
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// MSColumns stands for MeasurementSet Table columns.
// </etymology>
//
// <synopsis>
// This class provides access to all the subtables and direct access to all the
// columns in the MeasurementSet.  It does the declaration of all the Scalar
// and ArrayColumns with the correct types, so the application programmer
// doesn't have to worry about getting those right. There is an access function
// for every predefined column. Access to non-predefined columns will still
// have to be done with explicit declarations.
// </synopsis>
//
// <example>
// <srcblock>
// // use as follows
// MeasurementSet ms("myMS",Table::Update); 
// MSColumns msc(ms);
// // show data from row 5
// cout << msc.data()(5);
// // change name of antenna on row 3 in antenna table
// msc.antenna().name().put(3,"NewAnt-3");
// </srcblock>
// </example>
//
// <motivation>
// Having to type long lists of Scalar and Array column declarations gets
// very tedious. This class attempts to relieve some of that tedium, while
// at the same time concentrating all the declarations in one place,
// making Type errors in the column declaration (only caught at run-time) less
// probable. Type errors in the use of the columns is caught at compile
// time.
// </motivation>
//
// <todo asof="1997/02/01">
//   <li> We might decide to merge this class with the MeasurementSet
// </todo>

class MSColumns: public MSMainColumns
{
public:
  // Create a columns object that accesses the data in the specified MS
  MSColumns(const MeasurementSet& ms);

  // The destructor does nothing special
  ~MSColumns();

  // Access to required subtables
  // <group>
  MSAntennaColumns& antenna() {return antenna_p;}
  MSDataDescColumns& dataDescription() {return dataDesc_p;}
  MSFeedColumns& feed() {return feed_p;}
  MSFieldColumns& field() {return field_p;}
  MSFlagCmdColumns& flagCmd() {return flagCmd_p;}
  MSHistoryColumns& history() {return history_p;}
  MSObservationColumns& observation() {return observation_p;}
  MSPointingColumns& pointing() {return pointing_p;}
  MSPolarizationColumns& polarization() {return polarization_p;}
  MSProcessorColumns& processor() {return processor_p;}
  MSSpWindowColumns& spectralWindow() {return spectralWindow_p;}
  MSStateColumns& state() {return state_p;}
  // </group>

  // Access to optional subtables
  // <group>
  MSDopplerColumns& doppler() {return doppler_p;}
  MSFreqOffsetColumns& freqOffset() {return freqOffset_p;}
  MSSourceColumns& source() {return source_p;}
  MSSysCalColumns& sysCal() {return sysCal_p;}
  MSWeatherColumns& weather() {return weather_p;}
  // </group>

  // Access to required subtables
  // <group>
  const MSAntennaColumns& antenna() const {return antenna_p;}
  const MSDataDescColumns& dataDescription() const {return dataDesc_p;}
  const MSFeedColumns& feed() const {return feed_p;}
  const MSFieldColumns& field() const {return field_p;}
  const MSFlagCmdColumns& flagCmd() const {return flagCmd_p;}
  const MSHistoryColumns& history() const {return history_p;}
  const MSObservationColumns& observation() const {return observation_p;}
  const MSPointingColumns& pointing() const {return pointing_p;}
  const MSPolarizationColumns& polarization() const {
    return polarization_p;}
  const MSProcessorColumns& processor() const {return processor_p;}
  const MSSpWindowColumns& spectralWindow() const {
    return spectralWindow_p;}
  const MSStateColumns& state() const {return state_p;}
  // </group>

  // Access to optional subtables
  // <group>
  const MSDopplerColumns& doppler() const {return doppler_p;}
  const MSFreqOffsetColumns& freqOffset() const {return freqOffset_p;}
  const MSSourceColumns& source() const {return source_p;}
  const MSSysCalColumns& sysCal() const {return sysCal_p;}
  const MSWeatherColumns& weather() const {return weather_p;}
  // </group>

  // set the EPOCH reference type in all EPOCH columns in the MS. Note that
  // only a single EPOCH reference is allowed in the MS. This 
  // <note role=tip>
  // In principle this function can only be used if the table is empty,
  // otherwise already written values may thereafter have an incorrect
  // reference, offset, or unit.  However, it is possible that part of the
  // table gets written before these values are known.  In that case the
  // reference, offset, or units can be set by using a False
  // <src>tableMustBeEmpty</src> argument.
  // </note>
  void setEpochRef(MEpoch::Types ref, Bool tableMustBeEmpty=True);

  // set the DIRECTION reference type for FIELD, POINTING and SOURCE tables
  // (except for antenna frame directions). 
  void setDirectionRef(MDirection::Types ref);

private:
  // Access to subtables
  MSAntennaColumns antenna_p;
  MSDataDescColumns dataDesc_p;
  MSDopplerColumns doppler_p; //optional
  MSFeedColumns feed_p;
  MSFieldColumns field_p;
  MSFlagCmdColumns flagCmd_p;
  MSFreqOffsetColumns freqOffset_p; //optional
  MSHistoryColumns history_p;
  MSObservationColumns observation_p;
  MSPointingColumns pointing_p;
  MSPolarizationColumns polarization_p;
  MSProcessorColumns processor_p;
  MSSourceColumns source_p; // optional
  MSSpWindowColumns spectralWindow_p;
  MSStateColumns state_p;
  MSSysCalColumns sysCal_p; //optional
  MSWeatherColumns weather_p; //optional
};

//# Define the RO version for backward compatibility.
typedef MSColumns ROMSColumns;
  
//# Declare extern templates for often used types.
  extern template class ArrayMeasColumn<MDirection>;
  extern template class ScalarMeasColumn<MPosition>;
  extern template class ScalarMeasColumn<MEpoch>;
  extern template class ArrayQuantColumn<Double>;
  extern template class ScalarQuantColumn<Double>;

} //# NAMESPACE CASACORE - END

#endif


