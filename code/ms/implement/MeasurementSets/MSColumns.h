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
//#
//# $Id$

#if !defined(AIPS_MSCOLUMNS_H)
#define AIPS_MSCOLUMNS_H

#include <aips/aips.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MEpoch.h>
#include <aips/MeasurementSets/MSAntennaColumns.h>
#include <aips/MeasurementSets/MSDataDescColumns.h>
#include <aips/MeasurementSets/MSDopplerColumns.h>
#include <aips/MeasurementSets/MSFeedColumns.h>
#include <aips/MeasurementSets/MSFieldColumns.h>
#include <aips/MeasurementSets/MSFlagCmdColumns.h>
#include <aips/MeasurementSets/MSFreqOffColumns.h>
#include <aips/MeasurementSets/MSHistoryColumns.h>
#include <aips/MeasurementSets/MSMainColumns.h>
#include <aips/MeasurementSets/MSObsColumns.h>
#include <aips/MeasurementSets/MSPointingColumns.h>
#include <aips/MeasurementSets/MSPolColumns.h>
#include <aips/MeasurementSets/MSProcessorColumns.h>
#include <aips/MeasurementSets/MSSourceColumns.h>
#include <aips/MeasurementSets/MSSpWindowColumns.h>
#include <aips/MeasurementSets/MSStateColumns.h>
#include <aips/MeasurementSets/MSSysCalColumns.h>
#include <aips/MeasurementSets/MSWeatherColumns.h>

class MeasurementSet;

// <summary>
// A class to provide easy read-only access to MeasurementSet columns
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
// ROMSColumns stands for Read-Only MeasurementSet Table columns.
// </etymology>
//
// <synopsis>
// This class provides read-only access to the columns in the MeasurementSet.
// It does the declaration of all the Scalar and ArrayColumns with the
// correct types, so the application programmer doesn't have to
// worry about getting those right. There is an access function
// for every predefined column. Access to non-predefined columns will still
// have to be done with explicit declarations.
// </synopsis>
//
// <example>
// <srcblock>
// // use as follows
// MeasurementSet ms("myMS"); 
// ROMSColumns msc(ms);
// // show data from row 5
// cout << msc.data()(5);
// // show name of antenna on row 3 in antenna table
// cout << msc.antenna().name();
// </srcblock>
// </example>
//
// <motivation>
// See <linkto class=MSColumns> MSColumns</linkto> for the motivation.
// </motivation>
//
// <todo asof="1997/02/01">
//   <li> We might decide to merge all the MSColumn classes with the
//        corresponding MeasurementSet classes.
// </todo>

class ROMSColumns: public ROMSMainColumns
{
public:
  // Create a columns object that accesses the data in the specified MS
  ROMSColumns(const MeasurementSet& ms);

  // The destructor does nothing special
  ~ROMSColumns();

  // Access to required subtables
  // <group>
  const ROMSAntennaColumns& antenna() const {return antenna_p;}
  const ROMSDataDescColumns& dataDescription() const {return dataDesc_p;}
  const ROMSFeedColumns& feed() const {return feed_p;}
  const ROMSFieldColumns& field() const {return field_p;}
  const ROMSFlagCmdColumns& flagCmd() const {return flagCmd_p;}
  const ROMSHistoryColumns& history() const {return history_p;}
  const ROMSObservationColumns& observation() const {return observation_p;}
  const ROMSPointingColumns& pointing() const {return pointing_p;}
  const ROMSPolarizationColumns& polarization() const {
    return polarization_p;}
  const ROMSProcessorColumns& processor() const {return processor_p;}
  const ROMSSpWindowColumns& spectralWindow() const {
    return spectralWindow_p;}
  const ROMSStateColumns& state() const {return state_p;}
  // </group>

  // Access to optional subtables
  // <group>
  const ROMSDopplerColumns& doppler() const {return doppler_p;}
  const ROMSFreqOffsetColumns& freqOffset() const {return freqOffset_p;}
  const ROMSSourceColumns& source() const {return source_p;}
  const ROMSSysCalColumns& sysCal() const {return sysCal_p;}
  const ROMSWeatherColumns& weather() const {return weather_p;}
  // </group>

private:
  // Access to subtables
  ROMSAntennaColumns antenna_p;
  ROMSDataDescColumns dataDesc_p;
  ROMSDopplerColumns doppler_p; //optional
  ROMSFeedColumns feed_p;
  ROMSFieldColumns field_p;
  ROMSFlagCmdColumns flagCmd_p;
  ROMSFreqOffsetColumns freqOffset_p; //optional
  ROMSHistoryColumns history_p;
  ROMSObservationColumns observation_p;
  ROMSPointingColumns pointing_p;
  ROMSPolarizationColumns polarization_p;
  ROMSProcessorColumns processor_p;
  ROMSSourceColumns source_p; // optional
  ROMSSpWindowColumns spectralWindow_p;
  ROMSStateColumns state_p;
  ROMSSysCalColumns sysCal_p; //optional
  ROMSWeatherColumns weather_p; //optional
};

// <summary>
// A class to provide easy read-write access to MeasurementSet columns
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
  MSColumns(MeasurementSet& ms);

  // The destructor does nothing special
  ~MSColumns();

  // Read-write access to required subtables
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

  // Read-write access to optional subtables
  // <group>
  MSDopplerColumns& doppler() {return doppler_p;}
  MSFreqOffsetColumns& freqOffset() {return freqOffset_p;}
  MSSourceColumns& source() {return source_p;}
  MSSysCalColumns& sysCal() {return sysCal_p;}
  MSWeatherColumns& weather() {return weather_p;}
  // </group>

  // Read-only access to required subtables
  // <group>
  const ROMSAntennaColumns& antenna() const {return antenna_p;}
  const ROMSDataDescColumns& dataDescription() const {return dataDesc_p;}
  const ROMSFeedColumns& feed() const {return feed_p;}
  const ROMSFieldColumns& field() const {return field_p;}
  const ROMSFlagCmdColumns& flagCmd() const {return flagCmd_p;}
  const ROMSHistoryColumns& history() const {return history_p;}
  const ROMSObservationColumns& observation() const {return observation_p;}
  const ROMSPointingColumns& pointing() const {return pointing_p;}
  const ROMSPolarizationColumns& polarization() const {
    return polarization_p;}
  const ROMSProcessorColumns& processor() const {return processor_p;}
  const ROMSSourceColumns& source() const {return source_p;}
  const ROMSSpWindowColumns& spectralWindow() const {
    return spectralWindow_p;}
  const ROMSStateColumns& state() const {return state_p;}
  // </group>

  // Read-only access to optional subtables
  // <group>
  const ROMSDopplerColumns& doppler() const {return doppler_p;}
  const ROMSFreqOffsetColumns& freqOffset() const {return freqOffset_p;}
  const ROMSSysCalColumns& sysCal() const {return sysCal_p;}
  const ROMSWeatherColumns& weather() const {return weather_p;}
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
#endif


