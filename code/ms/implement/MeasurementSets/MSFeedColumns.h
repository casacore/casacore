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

#if !defined(AIPS_NEWMSFEEDCOLUMNS_H)
#define AIPS_NEWMSFEEDCOLUMNS_H

#include <aips/aips.h>
#include <aips/Mathematics/Complex.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MEpoch.h>
#include <aips/Measures/MPosition.h>
#include <aips/TableMeasures/ArrayMeasColumn.h>
#include <aips/TableMeasures/ArrayQuantColumn.h>
#include <aips/TableMeasures/ScalarMeasColumn.h>
#include <aips/TableMeasures/ScalarQuantColumn.h>
#include <aips/Tables/ArrayColumn.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Utilities/String.h>

class NewMSFeed;

// <summary>
// A class to provide easy read-only access to NewMSFeed columns
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
  // Create a columns object that accesses the data in the specified Table
  RONewMSFeedColumns(const NewMSFeed& msFeed);

  // The destructor does nothing special
  ~RONewMSFeedColumns();

  // Access to required columns
  // <group>
  const ROScalarColumn<Int>& antennaId() const {return antennaId_p;}
  const ROScalarColumn<Int>& beamId() const {return beamId_p;}
  const ROArrayColumn<Double>& beamOffset() const {return beamOffset_p;}
  const ROArrayQuantColumn<Double>& beamOffsetQuant() const {
    return beamOffsetQuant_p;}
  const ROArrayMeasColumn<MDirection>& beamOffsetMeas() const 
    {return beamOffsetMeas_p;}
  const ROScalarColumn<Int>& feedId() const {return feedId_p;}
  const ROScalarColumn<Double>& interval() const {return interval_p;}
  const ROScalarQuantColumn<Double>& intervalQuant() const {
    return intervalQuant_p;}
  const ROScalarColumn<Int>& numReceptors() const {return numReceptors_p;}
  const ROArrayColumn<Complex>& polResponse() const {return polResponse_p;}
  const ROArrayColumn<String>& polarizationType() const {
    return polarizationType_p;}
  const ROArrayColumn<Double>& position() const {return position_p;}
  const ROArrayQuantColumn<Double>& positionQuant() const {
    return positionQuant_p;}
  const ROScalarMeasColumn<MPosition>& positionMeas() const 
    { return positionMeas_p;}
  const ROArrayColumn<Double>& receptorAngle() const {return receptorAngle_p;}
  const ROArrayQuantColumn<Double>& receptorAngleQuant() const {
    return receptorAngleQuant_p;}
  const ROScalarColumn<Int>& spectralWindowId() const {
    return spectralWindowId_p;}
  const ROScalarColumn<Double>& time() const {return time_p;}
  const ROScalarQuantColumn<Double>& timeQuant() const {
    return timeQuant_p;}
  const ROScalarMeasColumn<MEpoch>& timeMeas() const {return timeMeas_p;}
  // </group>

  // Access to optional columns
  // <group>
  const ROScalarColumn<Double>& focusLength() const {return focusLength_p;}
  const ROScalarQuantColumn<Double>& focusLengthQuant() const {
    return focusLengthQuant_p;}
  const ROScalarColumn<Int>& phasedFeedId() const {return phasedFeedId_p;}
  // </group>

  // Convenience function that returns the number of rows in any of the columns
  uInt nrow() const {return antennaId_p.nrow();}

protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  RONewMSFeedColumns();

  //# attach this object to the supplied table.
  void attach(const NewMSFeed& msFeed);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  RONewMSFeedColumns(const RONewMSFeedColumns&);
  RONewMSFeedColumns& operator=(const RONewMSFeedColumns&);

  //# Check if any optional columns exist and if so attach them.
  void attachOptionalCols(const NewMSFeed& msFeed);
  
  //# required columns
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
  //# optional columns
  ROScalarColumn<Double> focusLength_p;
  ROScalarColumn<Int> phasedFeedId_p;

  // Access to Measure columns
  ROArrayMeasColumn<MDirection> beamOffsetMeas_p;
  ROScalarMeasColumn<MPosition> positionMeas_p;
  ROScalarMeasColumn<MEpoch> timeMeas_p;

  // Access to Quantum columns
  ROArrayQuantColumn<Double> beamOffsetQuant_p;
  ROScalarQuantColumn<Double> intervalQuant_p;
  ROArrayQuantColumn<Double> positionQuant_p;
  ROArrayQuantColumn<Double> receptorAngleQuant_p;
  ROScalarQuantColumn<Double> timeQuant_p;
  //# optional Quantum columns
  ROScalarQuantColumn<Double> focusLengthQuant_p;
};

// <summary>
// A class to provide easy read-write access to NewMSFeed columns
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

class NewMSFeedColumns: public RONewMSFeedColumns
{
public:
  // Create a columns object that accesses the data in the specified Table
  NewMSFeedColumns(NewMSFeed& msFeed);

  // The desctructor does nothing special
  ~NewMSFeedColumns();

  // Read-write access to required columns
  // <group>
  ScalarColumn<Int>& antennaId() {return antennaId_p;}
  ScalarColumn<Int>& beamId() {return beamId_p;}
  ArrayColumn<Double>& beamOffset() {return beamOffset_p;}
  ArrayQuantColumn<Double>& beamOffsetQuant() { return beamOffsetQuant_p;}
  ArrayMeasColumn<MDirection>& beamOffsetMeas() 
    {return beamOffsetMeas_p;}
  ScalarColumn<Int>& feedId() {return feedId_p;}
  ScalarColumn<Double>& interval() {return interval_p;}
  ScalarQuantColumn<Double>& intervalQuant() { return intervalQuant_p;}
  ScalarColumn<Int>& numReceptors() {return numReceptors_p;}
  ArrayColumn<Complex>& polResponse() {return polResponse_p;}
  ArrayColumn<String>& polarizationType() {return polarizationType_p;}
  ArrayColumn<Double>& position() {return position_p;}
  ArrayQuantColumn<Double>& positionQuant() {return positionQuant_p;}
  ScalarMeasColumn<MPosition>& positionMeas() 
    { return positionMeas_p;}
  ArrayColumn<Double>& receptorAngle() {return receptorAngle_p;}
  ArrayQuantColumn<Double>& receptorAngleQuant() {
    return receptorAngleQuant_p;}
  ScalarColumn<Int>& spectralWindowId() {return spectralWindowId_p;}
  ScalarColumn<Double>& time() {return time_p;}
  ScalarQuantColumn<Double>& timeQuant() { return timeQuant_p;}
  ScalarMeasColumn<MEpoch>& timeMeas() { return timeMeas_p;}
  // </group>

  // Read-write access to optional columns
  // <group>
  ScalarColumn<Double>& focusLength() {return focusLength_p;}
  ScalarQuantColumn<Double>& focusLengthQuant() { return focusLengthQuant_p;}
  ScalarColumn<Int>& phasedFeedId() {return phasedFeedId_p;}
  // </group>

  // Read-only access to required columns
  // <group>
  const ROScalarColumn<Int>& antennaId() const {
    return RONewMSFeedColumns::antennaId();}
  const ROScalarColumn<Int>& beamId() const {
    return RONewMSFeedColumns::beamId();}
  const ROArrayColumn<Double>& beamOffset() const {
    return RONewMSFeedColumns::beamOffset();}
  const ROArrayQuantColumn<Double>& beamOffsetQuant() const {
    return RONewMSFeedColumns::beamOffsetQuant();}
  const ROArrayMeasColumn<MDirection>& beamOffsetMeas() const {
    return RONewMSFeedColumns::beamOffsetMeas();}
  const ROScalarColumn<Int>& feedId() const {
    return RONewMSFeedColumns::feedId();}
  const ROScalarColumn<Double>& interval() const {
    return RONewMSFeedColumns::interval();}
  const ROScalarQuantColumn<Double>& intervalQuant() const {
    return RONewMSFeedColumns::intervalQuant();}
  const ROScalarColumn<Int>& numReceptors() const {
    return RONewMSFeedColumns::numReceptors();}
  const ROArrayColumn<Complex>& polResponse() const {
    return RONewMSFeedColumns::polResponse();}
  const ROArrayColumn<String>& polarizationType() const {
    return RONewMSFeedColumns::polarizationType();}
  const ROArrayColumn<Double>& position() const {
    return RONewMSFeedColumns::position();}
  const ROArrayQuantColumn<Double>& positionQuant() const {
    return RONewMSFeedColumns::positionQuant();}
  const ROScalarMeasColumn<MPosition>& positionMeas() const {
    return RONewMSFeedColumns::positionMeas();}
  const ROArrayColumn<Double>& receptorAngle() const {
    return RONewMSFeedColumns::receptorAngle();}
  const ROArrayQuantColumn<Double>& receptorAngleQuant() const {
    return RONewMSFeedColumns::receptorAngleQuant();}
  const ROScalarColumn<Int>& spectralWindowId() const {
    return RONewMSFeedColumns::spectralWindowId();}
  const ROScalarColumn<Double>& time() const {
    return RONewMSFeedColumns::time();}
  const ROScalarQuantColumn<Double>& timeQuant() const {
    return RONewMSFeedColumns::timeQuant();}
  const ROScalarMeasColumn<MEpoch>& timeMeas() const {
    return RONewMSFeedColumns::timeMeas();}
  // </group>

  // Read-only access to optional columns
  // <group>
  const ROScalarColumn<Double>& focusLength() const {
    return RONewMSFeedColumns::focusLength();}
  const ROScalarQuantColumn<Double>& focusLengthQuant() const {
    return RONewMSFeedColumns::focusLengthQuant();}
  const ROScalarColumn<Int>& phasedFeedId() const {
    return RONewMSFeedColumns::phasedFeedId();}
  // </group>

  // set the epoch type for the TIME column. This can only be done when the
  // table has no rows. Trying to do so at other times will throw an exception.
  void setEpochRef(MEpoch::Types ref);

  // set the direction type for the BEAM_OFFSET column. This can only be done
  // when the table has no rows. Trying to do so at other times will throw an
  // exception.
  void setDirectionRef(MDirection::Types ref);

  // set the position type for the POSITION column. This can only be done when
  // the table has no rows. Trying to do so at other times will throw an
  // exception.
  void setPositionRef(MPosition::Types ref);

protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  NewMSFeedColumns();

  //# attach this object to the supplied table.
  void attach(NewMSFeed& msFeed);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  NewMSFeedColumns(const NewMSFeedColumns&);
  NewMSFeedColumns& operator=(const NewMSFeedColumns&);

  //# Check if any optional columns exist and if so attach them.
  void attachOptionalCols(NewMSFeed& msFeed);
  
  //# required columns
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
  //# optional columns
  ScalarColumn<Double> focusLength_p;
  ScalarColumn<Int> phasedFeedId_p;

  //# Access to Measure columns
  ArrayMeasColumn<MDirection> beamOffsetMeas_p;
  ScalarMeasColumn<MPosition> positionMeas_p;
  ScalarMeasColumn<MEpoch> timeMeas_p;

  //# Access to Quantum columns
  ArrayQuantColumn<Double> beamOffsetQuant_p;
  ScalarQuantColumn<Double> intervalQuant_p;
  ArrayQuantColumn<Double> positionQuant_p;
  ArrayQuantColumn<Double> receptorAngleQuant_p;
  ScalarQuantColumn<Double> timeQuant_p;
  //# optional Quantum columns
  ScalarQuantColumn<Double> focusLengthQuant_p;
};
#endif
