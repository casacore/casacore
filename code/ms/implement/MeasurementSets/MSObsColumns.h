//# NewMSObservationColumns.h: provides easy access to NewMSObservation columns
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

#if !defined(AIPS_NEWMSOBSERVATIONCOLUMNS_H)
#define AIPS_NEWMSOBSERVATIONCOLUMNS_H

#include <aips/aips.h>
#include <aips/Measures/MEpoch.h>
#include <aips/TableMeasures/ArrayMeasColumn.h>
#include <aips/TableMeasures/ArrayQuantColumn.h>
#include <aips/TableMeasures/ScalarMeasColumn.h>
#include <aips/TableMeasures/ScalarQuantColumn.h>
#include <aips/Tables/ArrayColumn.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Utilities/String.h>

class NewMSObservation;

// <summary>
// A class to provide easy read-only access to NewMSObservation columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> NewMSObservation
//   <li> ArrayColumn
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// RONewMSObservationColumns stands for Read-Only NewMeasurementSet Observation
// Table columns.
// </etymology>
//
// <synopsis>
// This class provides read-only access to the columns in the NewMSObservation
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

class RONewMSObservationColumns
{
public:
  // Create a columns object that accesses the data in the specified Table
  RONewMSObservationColumns(const NewMSObservation& msObservation);

  // The destructor does nothing special
  ~RONewMSObservationColumns();

  // Access to required columns
  // <group>
  const ROScalarColumn<Bool>& flagRow() const {return flagRow_p;}
  const ROArrayColumn<String>& log() const {return log_p;}
  const ROScalarColumn<String>& observer() const {return observer_p;}
  const ROScalarColumn<String>& project() const {return project_p;}
  const ROScalarColumn<Double>& releaseDate() const {return releaseDate_p;}
  const ROScalarQuantColumn<Double>& releaseDateQuant() const {
    return releaseDateQuant_p;}
  const ROScalarMeasColumn<MEpoch>& releaseDateMeas() const {
    return releaseDateMeas_p;}
  const ROArrayColumn<String>& schedule() const {return schedule_p;}
  const ROScalarColumn<String>& scheduleType() const {return scheduleType_p;}
  const ROScalarColumn<String>& telescopeName() const {return telescopeName_p;}
  const ROArrayColumn<Double>& timeRange() const {return timeRange_p;}
  const ROArrayQuantColumn<Double>& timeRangeQuant() const {
    return timeRangeQuant_p;}
  const ROArrayMeasColumn<MEpoch>& timeRangeMeas() const {
    return timeRangeMeas_p;}
  // </group>

  // Convenience function that returns the number of rows in any of the columns
  uInt nrow() const {return flagRow_p.nrow();}

protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  RONewMSObservationColumns();

  //# attach this object to the supplied table.
  void attach(const NewMSObservation& msObservation);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  RONewMSObservationColumns(const RONewMSObservationColumns&);
  RONewMSObservationColumns& operator=(const RONewMSObservationColumns&);

  //# required columns
  ROScalarColumn<Bool> flagRow_p;
  ROArrayColumn<String> log_p;
  ROScalarColumn<String> observer_p;
  ROScalarColumn<String> project_p;
  ROScalarColumn<Double> releaseDate_p;
  ROArrayColumn<String> schedule_p;
  ROScalarColumn<String> scheduleType_p;
  ROScalarColumn<String> telescopeName_p;
  ROArrayColumn<Double> timeRange_p;

  //# Access to Measure columns
  ROScalarMeasColumn<MEpoch> releaseDateMeas_p;
  ROArrayMeasColumn<MEpoch> timeRangeMeas_p;

  //# Access to Quantum columns
  ROScalarQuantColumn<Double> releaseDateQuant_p;
  ROArrayQuantColumn<Double> timeRangeQuant_p;
};

// <summary>
// A class to provide easy read-write access to NewMSObservation columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> NewMSObservation
//   <li> ArrayColumn
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// NewMSObservationColumns stands for NewMeasurementSet Observation Table
// columns.
// </etymology>
//
// <synopsis>
// This class provides access to the columns in the NewMSObservation Table,
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

class NewMSObservationColumns: public RONewMSObservationColumns
{
public:
  // Create a columns object that accesses the data in the specified Table
  NewMSObservationColumns(NewMSObservation& msObservation);

  // The desctructor does nothing special
  ~NewMSObservationColumns();

  // Read-write access to required columns
  // <group>
  ScalarColumn<Bool>& flagRow() {return flagRow_p;}
  ArrayColumn<String>& log() {return log_p;}
  ScalarColumn<String>& observer() {return observer_p;}
  ScalarColumn<String>& project() {return project_p;}
  ScalarColumn<Double>& releaseDate() {return releaseDate_p;}
  ScalarQuantColumn<Double>& releaseDateQuant() {return releaseDateQuant_p;}
  ScalarMeasColumn<MEpoch>& releaseDateMeas() {return releaseDateMeas_p;}
  ArrayColumn<String>& schedule() {return schedule_p;}
  ScalarColumn<String>& scheduleType() {return scheduleType_p;}
  ScalarColumn<String>& telescopeName() {return telescopeName_p;}
  ArrayColumn<Double>& timeRange() {return timeRange_p;}
  ArrayQuantColumn<Double>& timeRangeQuant() {return timeRangeQuant_p;}
  ArrayMeasColumn<MEpoch>& timeRangeMeas() {return timeRangeMeas_p;}
  // </group>

  // Read-only access to required columns
  // <group>
  const ROScalarColumn<Bool>& flagRow() const {
    return RONewMSObservationColumns::flagRow();}
  const ROArrayColumn<String>& log() const {
    return RONewMSObservationColumns::log();}
  const ROScalarColumn<String>& observer() const {
    return RONewMSObservationColumns::observer();}
  const ROScalarColumn<String>& project() const {
    return RONewMSObservationColumns::project();}
  const ROScalarColumn<Double>& releaseDate() const {
    return RONewMSObservationColumns::releaseDate();}
  const ROScalarQuantColumn<Double>& releaseDateQuant() const {
    return RONewMSObservationColumns::releaseDateQuant();}
  const ROScalarMeasColumn<MEpoch>& releaseDateMeas() const {
    return RONewMSObservationColumns::releaseDateMeas();}
  const ROArrayColumn<String>& schedule() const {
    return RONewMSObservationColumns::schedule();}
  const ROScalarColumn<String>& scheduleType() const {
    return RONewMSObservationColumns::scheduleType();}
  const ROScalarColumn<String>& telescopeName() const {
    return RONewMSObservationColumns::telescopeName();}
  const ROArrayColumn<Double>& timeRange() const {
    return RONewMSObservationColumns::timeRange();}
  const ROArrayQuantColumn<Double>& timeRangeQuant() const {
    return RONewMSObservationColumns::timeRangeQuant();}
  const ROArrayMeasColumn<MEpoch>& timeRangeMeas() const {
    return RONewMSObservationColumns::timeRangeMeas();}
  // </group>

  // set the epoch type for the TIME_RANGE & RELEASE_DATE columns. This can
  // only be done when the table has no rows. Trying to do so at other times
  // will throw an exception.
  void setEpochRef(MEpoch::Types ref);

protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  NewMSObservationColumns();

  //# attach this object to the supplied table.
  void attach(NewMSObservation& msObservation);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  NewMSObservationColumns(const NewMSObservationColumns&);
  NewMSObservationColumns& operator=(const NewMSObservationColumns&);

  //# required columns
  ScalarColumn<Bool> flagRow_p;
  ArrayColumn<String> log_p;
  ScalarColumn<String> observer_p;
  ScalarColumn<String> project_p;
  ScalarColumn<Double> releaseDate_p;
  ArrayColumn<String> schedule_p;
  ScalarColumn<String> scheduleType_p;
  ScalarColumn<String> telescopeName_p;
  ArrayColumn<Double> timeRange_p;

  //# Access to Measure columns
  ScalarMeasColumn<MEpoch> releaseDateMeas_p;
  ArrayMeasColumn<MEpoch> timeRangeMeas_p;

  //# Access to Quantum columns
  ScalarQuantColumn<Double> releaseDateQuant_p;
  ArrayQuantColumn<Double> timeRangeQuant_p;
};
#endif
