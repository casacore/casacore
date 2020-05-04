//# MSObservationColumns.h: provides easy access to MSObservation columns
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

#ifndef MS_MSOBSCOLUMNS_H
#define MS_MSOBSCOLUMNS_H

#include <casacore/casa/aips.h>
#include <casacore/measures/Measures/MEpoch.h>
#include <casacore/measures/Measures/MCEpoch.h>
#include <casacore/measures/TableMeasures/ArrayMeasColumn.h>
#include <casacore/measures/TableMeasures/ArrayQuantColumn.h>
#include <casacore/measures/TableMeasures/ScalarMeasColumn.h>
#include <casacore/measures/TableMeasures/ScalarQuantColumn.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class MSObservation;

// <summary>
// A class to provide easy access to MSObservation columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> MSObservation
//   <li> ArrayColumn
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// MSObservationColumns stands for MeasurementSet Observation Table
// columns.
// </etymology>
//
// <synopsis>
// This class provides access to the columns in the MSObservation Table,
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

class MSObservationColumns
{
public:
  // Create a columns object that accesses the data in the specified Table
  MSObservationColumns(const MSObservation& msObservation);

  // The desctructor does nothing special
  ~MSObservationColumns();

  // Access to required columns
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

  // Const access to required columns
  // <group>
  const ScalarColumn<Bool>& flagRow() const {return flagRow_p;}
  const ArrayColumn<String>& log() const {return log_p;}
  const ScalarColumn<String>& observer() const {return observer_p;}
  const ScalarColumn<String>& project() const {return project_p;}
  const ScalarColumn<Double>& releaseDate() const {return releaseDate_p;}
  const ScalarQuantColumn<Double>& releaseDateQuant() const {
    return releaseDateQuant_p;}
  const ScalarMeasColumn<MEpoch>& releaseDateMeas() const {
    return releaseDateMeas_p;}
  const ArrayColumn<String>& schedule() const {return schedule_p;}
  const ScalarColumn<String>& scheduleType() const {return scheduleType_p;}
  const ScalarColumn<String>& telescopeName() const {return telescopeName_p;}
  const ArrayColumn<Double>& timeRange() const {return timeRange_p;}
  const ArrayQuantColumn<Double>& timeRangeQuant() const {
    return timeRangeQuant_p;}
  const ArrayMeasColumn<MEpoch>& timeRangeMeas() const {
    return timeRangeMeas_p;}
  // </group>

  // Convenience function that returns the number of rows in any of the columns
  rownr_t nrow() const {return flagRow_p.nrow();}

  // set the epoch type for the TIME_RANGE & RELEASE_DATE columns.
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
  MSObservationColumns();

  //# attach this object to the supplied table.
  void attach(const MSObservation& msObservation);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  MSObservationColumns(const MSObservationColumns&);
  MSObservationColumns& operator=(const MSObservationColumns&);

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

//# Define the RO version for backward compatibility.
typedef MSObservationColumns ROMSObservationColumns;

} //# NAMESPACE CASACORE - END

#endif
