//# NewMSColumns.h: provides easy access to NewMeasurementSet columns
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

#if !defined(AIPS_NEWMSCOLUMNS_H)
#define AIPS_NEWMSCOLUMNS_H

#include <aips/aips.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MEpoch.h>
#include <aips/MeasurementSets/NewMSAntennaColumns.h>
#include <aips/MeasurementSets/NewMSDataDescColumns.h>
#include <aips/MeasurementSets/NewMSDopplerColumns.h>
#include <aips/MeasurementSets/NewMSFeedColumns.h>
#include <aips/MeasurementSets/NewMSFieldColumns.h>
#include <aips/MeasurementSets/NewMSFlagCmdColumns.h>
#include <aips/MeasurementSets/NewMSFreqOffColumns.h>
#include <aips/MeasurementSets/NewMSHistoryColumns.h>
#include <aips/MeasurementSets/NewMSMainColumns.h>
#include <aips/MeasurementSets/NewMSObsColumns.h>
#include <aips/MeasurementSets/NewMSPointingColumns.h>
#include <aips/MeasurementSets/NewMSPolColumns.h>
#include <aips/MeasurementSets/NewMSProcessorColumns.h>
#include <aips/MeasurementSets/NewMSSourceColumns.h>
#include <aips/MeasurementSets/NewMSSpWindowColumns.h>
#include <aips/MeasurementSets/NewMSStateColumns.h>
#include <aips/MeasurementSets/NewMSSysCalColumns.h>
#include <aips/MeasurementSets/NewMSWeatherColumns.h>

class NewMeasurementSet;

// <summary>
// A class to provide easy read-only access to NewMeasurementSet columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> NewMeasurementSet
//   <li> ArrayColumn
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// RONewMSColumns stands for Read-Only NewMeasurementSet Table columns.
// </etymology>
//
// <synopsis>
// This class provides read-only access to the columns in the NewMeasurementSet.
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
// NewMeasurementSet ms("myNewMS"); 
// RONewMSColumns msc(ms);
// // show data from row 5
// cout << msc.data()(5);
// // show name of antenna on row 3 in antenna table
// cout << msc.antenna().name();
// </srcblock>
// </example>
//
// <motivation>
// See <linkto class=NewMSColumns> NewMSColumns</linkto> for the motivation.
// </motivation>
//
// <todo asof="1997/02/01">
//   <li> We might decide to merge all the NewMSColumn classes with the
//        corresponding NewMeasurementSet classes.
// </todo>

class RONewMSColumns: public RONewMSMainColumns
{
public:
  // Create a columns object that accesses the data in the specified MS
  RONewMSColumns(const NewMeasurementSet& ms);

  // The destructor does nothing special
  ~RONewMSColumns();

  // Access to required subtables
  // <group>
  const RONewMSAntennaColumns& antenna() const {return antenna_p;}
  const RONewMSDataDescColumns& dataDescription() const {return dataDesc_p;}
  const RONewMSFeedColumns& feed() const {return feed_p;}
  const RONewMSFieldColumns& field() const {return field_p;}
  const RONewMSFlagCmdColumns& flagCmd() const {return flagCmd_p;}
  const RONewMSHistoryColumns& history() const {return history_p;}
  const RONewMSObservationColumns& observation() const {return observation_p;}
  const RONewMSPointingColumns& pointing() const {return pointing_p;}
  const RONewMSPolarizationColumns& polarization() const {
    return polarization_p;}
  const RONewMSProcessorColumns& processor() const {return processor_p;}
  const RONewMSSourceColumns& source() const {return source_p;}
  const RONewMSSpWindowColumns& spectralWindow() const {
    return spectralWindow_p;}
  const RONewMSStateColumns& state() const {return state_p;}
  // </group>

  // Access to optional subtables
  // <group>
  const RONewMSDopplerColumns& doppler() const {return doppler_p;}
  const RONewMSFreqOffsetColumns& freqOffset() const {return freqOffset_p;}
  const RONewMSSysCalColumns& sysCal() const {return sysCal_p;}
  const RONewMSWeatherColumns& weather() const {return weather_p;}
  // </group>

private:
  // Access to subtables
  RONewMSAntennaColumns antenna_p;
  RONewMSDataDescColumns dataDesc_p;
  RONewMSDopplerColumns doppler_p; //optional
  RONewMSFeedColumns feed_p;
  RONewMSFieldColumns field_p;
  RONewMSFlagCmdColumns flagCmd_p;
  RONewMSFreqOffsetColumns freqOffset_p; //optional
  RONewMSHistoryColumns history_p;
  RONewMSObservationColumns observation_p;
  RONewMSPointingColumns pointing_p;
  RONewMSPolarizationColumns polarization_p;
  RONewMSProcessorColumns processor_p;
  RONewMSSourceColumns source_p;
  RONewMSSpWindowColumns spectralWindow_p;
  RONewMSStateColumns state_p;
  RONewMSSysCalColumns sysCal_p; //optional
  RONewMSWeatherColumns weather_p; //optional
};

// <summary>
// A class to provide easy read-write access to NewMeasurementSet columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> NewMeasurementSet
//   <li> ArrayColumn
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// NewMSColumns stands for NewMeasurementSet Table columns.
// </etymology>
//
// <synopsis>
// This class provides access to all the subtables and direct access to all the
// columns in the NewMeasurementSet.  It does the declaration of all the Scalar
// and ArrayColumns with the correct types, so the application programmer
// doesn't have to worry about getting those right. There is an access function
// for every predefined column. Access to non-predefined columns will still
// have to be done with explicit declarations.
// </synopsis>
//
// <example>
// <srcblock>
// // use as follows
// NewMeasurementSet ms("myNewMS",Table::Update); 
// NewMSColumns msc(ms);
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
//   <li> We might decide to merge this class with the NewMeasurementSet
// </todo>

class NewMSColumns: public NewMSMainColumns
{
public:
  // Create a columns object that accesses the data in the specified MS
  NewMSColumns(NewMeasurementSet& ms);

  // The destructor does nothing special
  ~NewMSColumns();

  // Read-write access to required subtables
  // <group>
  NewMSAntennaColumns& antenna() {return antenna_p;}
  NewMSDataDescColumns& dataDescription() {return dataDesc_p;}
  NewMSFeedColumns& feed() {return feed_p;}
  NewMSFieldColumns& field() {return field_p;}
  NewMSFlagCmdColumns& flagCmd() {return flagCmd_p;}
  NewMSHistoryColumns& history() {return history_p;}
  NewMSObservationColumns& observation() {return observation_p;}
  NewMSPointingColumns& pointing() {return pointing_p;}
  NewMSPolarizationColumns& polarization() {return polarization_p;}
  NewMSProcessorColumns& processor() {return processor_p;}
  NewMSSourceColumns& source() {return source_p;}
  NewMSSpWindowColumns& spectralWindow() {return spectralWindow_p;}
  NewMSStateColumns& state() {return state_p;}
  // </group>

  // Read-write access to optional subtables
  // <group>
  NewMSDopplerColumns& doppler() {return doppler_p;}
  NewMSFreqOffsetColumns& freqOffset() {return freqOffset_p;}
  NewMSSysCalColumns& sysCal() {return sysCal_p;}
  NewMSWeatherColumns& weather() {return weather_p;}
  // </group>

  // Read-only access to required subtables
  // <group>
  const RONewMSAntennaColumns& antenna() const {return antenna_p;}
  const RONewMSDataDescColumns& dataDescription() const {return dataDesc_p;}
  const RONewMSFeedColumns& feed() const {return feed_p;}
  const RONewMSFieldColumns& field() const {return field_p;}
  const RONewMSFlagCmdColumns& flagCmd() const {return flagCmd_p;}
  const RONewMSHistoryColumns& history() const {return history_p;}
  const RONewMSObservationColumns& observation() const {return observation_p;}
  const RONewMSPointingColumns& pointing() const {return pointing_p;}
  const RONewMSPolarizationColumns& polarization() const {
    return polarization_p;}
  const RONewMSProcessorColumns& processor() const {return processor_p;}
  const RONewMSSourceColumns& source() const {return source_p;}
  const RONewMSSpWindowColumns& spectralWindow() const {
    return spectralWindow_p;}
  const RONewMSStateColumns& state() const {return state_p;}
  // </group>

  //# Read-only access to optional subtables
  //# <group>
  //# These are commented out untill these classes are updated so that the rw
  //# class is derived from the ro one.
//#   const RONewMSDopplerColumns& doppler() const {return doppler_p;}
//#   const RONewMSFreqOffsetColumns& freqOffset() const {return freqOffset_p;}
//#   const RONewMSSysCalColumns& sysCal() const {return sysCal_p;}
//#   const RONewMSWeatherColumns& weather() const {return weather_p;}
  //# </group>

  // set the EPOCH reference type in all EPOCH columns in the NewMS. Note that
  // only a single EPOCH reference is allowed in the NewMS.
  void setEpochRef(MEpoch::Types ref);

  // set the DIRECTION reference type for FIELD, POINTING and SOURCE tables
  // (except for antenna frame directions). 
  void setDirectionRef(MDirection::Types ref);

private:
  // Access to subtables
  NewMSAntennaColumns antenna_p;
  NewMSDataDescColumns dataDesc_p;
  NewMSDopplerColumns doppler_p; //optional
  NewMSFeedColumns feed_p;
  NewMSFieldColumns field_p;
  NewMSFlagCmdColumns flagCmd_p;
  NewMSFreqOffsetColumns freqOffset_p; //optional
  NewMSHistoryColumns history_p;
  NewMSObservationColumns observation_p;
  NewMSPointingColumns pointing_p;
  NewMSPolarizationColumns polarization_p;
  NewMSProcessorColumns processor_p;
  NewMSSourceColumns source_p;
  NewMSSpWindowColumns spectralWindow_p;
  NewMSStateColumns state_p;
  NewMSSysCalColumns sysCal_p; //optional
  NewMSWeatherColumns weather_p; //optional
};
#endif


