//# NewMSFeedColumns.h: provides easy access to NewMSFeed columns
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

#if !defined(AIPS_NewMSFEEDCOLUMNS_H)
#define AIPS_NewMSFEEDCOLUMNS_H

#include <aips/MeasurementSets/NewMSFeed.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Tables/ArrayColumn.h>
#include <aips/Quanta/Unit.h>
#include <aips/Quanta/Quantum.h>
#include <aips/TableMeasures/ScalarMeasColumn.h>
#include <aips/TableMeasures/ArrayMeasColumn.h>
#include <aips/TableMeasures/ScalarQuantColumn.h>
#include <aips/TableMeasures/ArrayQuantColumn.h>

#include <aips/Measures/MDirection.h>
class MEpoch;
class MPosition;

// <summary>
// A convenience class to provide easy access to NewMSFeed columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> NewMSFeed
//   <li> ArrayColumn
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// NewMSFeedColumns stands for NewMeasurementSet Feed Table columns.
// </etymology>
//
// <synopsis>
// This class provides access to the columns in the NewMSFeed Table,
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

class NewMSFeedColumns
{
public:

  NewMSFeedColumns(NewMSFeed& msFeed);

  ~NewMSFeedColumns();

  // Access to columns
  ScalarColumn<Int>& antennaId() {return antennaId_p;}
  ScalarColumn<Int>& beamId() {return beamId_p;}
  ArrayColumn<Double>& beamOffset() {return beamOffset_p;}
  ScalarColumn<Int>& feedId() {return feedId_p;}
  ScalarColumn<Double>& interval() {return interval_p;}
  ScalarColumn<Int>& numReceptors() {return numReceptors_p;}
  ArrayColumn<Complex>& polResponse() {return polResponse_p;}
  ArrayColumn<String>& polarizationType() {return polarizationType_p;}
  ArrayColumn<Double>& position() {return position_p;}
  ArrayColumn<Double>& receptorAngle() {return receptorAngle_p;}
  ScalarColumn<Int>& spectralWindowId() {return spectralWindowId_p;}
  ScalarColumn<Double>& time() {return time_p;}
  ScalarColumn<Double>& focusLength() {return focusLength_p;}
  ScalarColumn<Int>& phasedFeedId() {return phasedFeedId_p;}

  // Access to Measure columns
  ArrayMeasColumn<MDirection>& beamOffsetMeas() 
    {return beamOffsetMeas_p;}
  ScalarMeasColumn<MPosition>& positionMeas() 
    { return positionMeas_p;}
  ScalarMeasColumn<MEpoch>& timeMeas() { return timeMeas_p;}

  // Access to Quantum columns
  ArrayQuantColumn<Double>& beamOffsetQuant() { return beamOffsetQuant_p;}
  ScalarQuantColumn<Double>& focusLengthQuant() { return focusLengthQuant_p;}
  ScalarQuantColumn<Double>& intervalQuant() { return intervalQuant_p;}
  ArrayQuantColumn<Double>& positionQuant() {return positionQuant_p;}
  ArrayQuantColumn<Double>& receptorAngleQuant() { return receptorAngleQuant_p;}
  ScalarQuantColumn<Double>& timeQuant() { return timeQuant_p;}

  // set the Direction reference for the beam offset column
  void setDirectionRef(Int ref);

  // Set the POSITION reference for the position column.
  // Give the reference code as e.g., MPosition::ITRF.
  void setPositionRef(Int ref);

private:

  ScalarColumn<Int> antennaId_p;
  ScalarColumn<Int> beamId_p;
  ArrayColumn<Double> beamOffset_p;
  ScalarColumn<Int> feedId_p;
  ScalarColumn<Double> interval_p;
  ScalarColumn<Int> numReceptors_p;
  ArrayColumn<Complex> polResponse_p;
  ArrayColumn<String> polarizationType_p;
  ArrayColumn<Double> position_p;
  ArrayColumn<Double> receptorAngle_p;
  ScalarColumn<Int> spectralWindowId_p;
  ScalarColumn<Double> time_p;
  ScalarColumn<Double> focusLength_p;
  ScalarColumn<Int> phasedFeedId_p;

  // Access to Measure columns
  ArrayMeasColumn<MDirection> beamOffsetMeas_p;
  ScalarMeasColumn<MPosition> positionMeas_p;
  ScalarMeasColumn<MEpoch> timeMeas_p;

  // Access to Quantum columns
  ArrayQuantColumn<Double> beamOffsetQuant_p;
  ScalarQuantColumn<Double> focusLengthQuant_p;
  ScalarQuantColumn<Double> intervalQuant_p;
  ArrayQuantColumn<Double> positionQuant_p;
  ArrayQuantColumn<Double> receptorAngleQuant_p;
  ScalarQuantColumn<Double> timeQuant_p;

};

// <summary>
// A convenience class to provide easy access to NewMSFeed columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> NewMSFeed
//   <li> ArrayColumn
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// RONewMSFeedColumns stands for Read-Only NewMeasurementSet Feed Table columns.
// </etymology>
//
// <synopsis>
// This class provides read-only access to the columns in the NewMSFeed Table.
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

class RONewMSFeedColumns
{
public:

  RONewMSFeedColumns(const NewMSFeed& msFeed);

  ~RONewMSFeedColumns();

  // Access to columns
  const ROScalarColumn<Int>& antennaId() const {return antennaId_p;}
  const ROScalarColumn<Int>& beamId() const {return beamId_p;}
  const ROArrayColumn<Double>& beamOffset() const {return beamOffset_p;}
  const ROScalarColumn<Int>& feedId() const {return feedId_p;}
  const ROScalarColumn<Double>& interval() const {return interval_p;}
  const ROScalarColumn<Int>& numReceptors() const {return numReceptors_p;}
  const ROArrayColumn<Complex>& polResponse() const {return polResponse_p;}
  const ROArrayColumn<String>& polarizationType() const {return polarizationType_p;}
  const ROArrayColumn<Double>& position() const {return position_p;}
  const ROArrayColumn<Double>& receptorAngle() const {return receptorAngle_p;}
  const ROScalarColumn<Int>& spectralWindowId() const {return spectralWindowId_p;}
  const ROScalarColumn<Double>& time() const {return time_p;}
  const ROScalarColumn<Double>& focusLength() const {return focusLength_p;}
  const ROScalarColumn<Int>& phasedFeedId() const {return phasedFeedId_p;}

  // Access to Measure columns
  const ROArrayMeasColumn<MDirection>& beamOffsetMeas() const 
    {return beamOffsetMeas_p;}
  const ROScalarMeasColumn<MPosition>& positionMeas() const 
    { return positionMeas_p;}
  const ROScalarMeasColumn<MEpoch>& timeMeas() const { return timeMeas_p;}

  // Access to Quantum columns
  const ROArrayQuantColumn<Double>& beamOffsetQuant() const { return beamOffsetQuant_p;}
  const ROScalarQuantColumn<Double>& focusLengthQuant() const { return focusLengthQuant_p;}
  const ROScalarQuantColumn<Double>& intervalQuant() const { return intervalQuant_p;}
  const ROArrayQuantColumn<Double>& positionQuant() const {return positionQuant_p;}
  const ROArrayQuantColumn<Double>& receptorAngleQuant() const { return receptorAngleQuant_p;}
  const ROScalarQuantColumn<Double>& timeQuant() const { return timeQuant_p;}

private:

  ROScalarColumn<Int> antennaId_p;
  ROScalarColumn<Int> beamId_p;
  ROArrayColumn<Double> beamOffset_p;
  ROScalarColumn<Int> feedId_p;
  ROScalarColumn<Double> interval_p;
  ROScalarColumn<Int> numReceptors_p;
  ROArrayColumn<Complex> polResponse_p;
  ROArrayColumn<String> polarizationType_p;
  ROArrayColumn<Double> position_p;
  ROArrayColumn<Double> receptorAngle_p;
  ROScalarColumn<Int> spectralWindowId_p;
  ROScalarColumn<Double> time_p;
  ROScalarColumn<Double> focusLength_p;
  ROScalarColumn<Int> phasedFeedId_p;

  // Access to Measure columns
  ROArrayMeasColumn<MDirection> beamOffsetMeas_p;
  ROScalarMeasColumn<MPosition> positionMeas_p;
  ROScalarMeasColumn<MEpoch> timeMeas_p;

  // Access to Quantum columns
  ROArrayQuantColumn<Double> beamOffsetQuant_p;
  ROScalarQuantColumn<Double> focusLengthQuant_p;
  ROScalarQuantColumn<Double> intervalQuant_p;
  ROArrayQuantColumn<Double> positionQuant_p;
  ROArrayQuantColumn<Double> receptorAngleQuant_p;
  ROScalarQuantColumn<Double> timeQuant_p;

};

#endif
