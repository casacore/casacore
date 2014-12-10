//# MSFeedColumns.h: provides easy access to MSFeed columns
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

#ifndef MS_MSFEEDCOLUMNS_H
#define MS_MSFEEDCOLUMNS_H

#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/measures/Measures/MDirection.h>
#include <casacore/measures/Measures/MEpoch.h>
#include <casacore/measures/Measures/MPosition.h>
#include <casacore/measures/Measures/MCDirection.h>
#include <casacore/measures/Measures/MCEpoch.h>
#include <casacore/measures/Measures/MCPosition.h>
#include <casacore/measures/TableMeasures/ArrayMeasColumn.h>
#include <casacore/measures/TableMeasures/ArrayQuantColumn.h>
#include <casacore/measures/TableMeasures/ScalarMeasColumn.h>
#include <casacore/measures/TableMeasures/ScalarQuantColumn.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class MSFeed;

// <summary>
// A class to provide easy read-only access to MSFeed columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> MSFeed
//   <li> ArrayColumn
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// ROMSFeedColumns stands for Read-Only MeasurementSet Feed Table columns.
// </etymology>
//
// <synopsis>
// This class provides read-only access to the columns in the MSFeed Table.
// It does the declaration of all the Scalar and ArrayColumns with the
// correct types, so the application programmer doesn't have to
// worry about getting those right. There is an access function
// for every predefined column. Access to non-predefined columns will still
// have to be done with explicit declarations.
// See <linkto class=ROMSColumns> ROMSColumns</linkto> for an example.
// </synopsis>
//
// <motivation>
// See <linkto class=MSColumns> MSColumns</linkto> for the motivation.
// </motivation>

class ROMSFeedColumns
{
public:
  // Create a columns object that accesses the data in the specified Table
  ROMSFeedColumns(const MSFeed& msFeed);

  // The destructor does nothing special
  ~ROMSFeedColumns();

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

  // Returns the last row that contains a feed with the specified values.
  // If no matching row can be found, but a match is possible if the validity
  // time interval is widened, return that row and the suggestion for the
  // new time information.
  // If no change to time is necessary, newTimeQ and newIntervalQ are zero.
  // Returns -1 if no match could be found.
  // Ignore the Feed table rows contained in vector ignoreRows. 
  // focusLengthQ is only compared if this optional column is present and
  // if the value of focusLengthQ is not dimensionless.
  Int matchFeed(Quantum<Double>& newTimeQ,
		Quantum<Double>& newIntervalQ,
		const Int& antId,
		const Int& fId, // feedId
		const Int& spwId, 
		const Quantum<Double>& timeQ,
		const Quantum<Double>& intervalQ,
		const Int& numRec,
		const Array<Quantum<Double> >& beamOffsetQ,
		const Array<String>& polType,
		const Array<Complex>& polResp,
		const Array<Quantum<Double> >& positionQ,
		const Array<Quantum<Double> >& receptorAngleQ,
		const Vector<uInt>& ignoreRows,
		const Quantum<Double>& focusLengthQ=Quantum<Double>() 
		);

protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  ROMSFeedColumns();

  //# attach this object to the supplied table.
  void attach(const MSFeed& msFeed);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  ROMSFeedColumns(const ROMSFeedColumns&);
  ROMSFeedColumns& operator=(const ROMSFeedColumns&);

  //# Check if any optional columns exist and if so attach them.
  void attachOptionalCols(const MSFeed& msFeed);
  
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
// A class to provide easy read-write access to MSFeed columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> MSFeed
//   <li> ArrayColumn
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// MSFeedColumns stands for MeasurementSet Feed Table columns.
// </etymology>
//
// <synopsis>
// This class provides access to the columns in the MSFeed Table,
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

class MSFeedColumns: public ROMSFeedColumns
{
public:
  // Create a columns object that accesses the data in the specified Table
  MSFeedColumns(MSFeed& msFeed);

  // The desctructor does nothing special
  ~MSFeedColumns();

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
    return ROMSFeedColumns::antennaId();}
  const ROScalarColumn<Int>& beamId() const {
    return ROMSFeedColumns::beamId();}
  const ROArrayColumn<Double>& beamOffset() const {
    return ROMSFeedColumns::beamOffset();}
  const ROArrayQuantColumn<Double>& beamOffsetQuant() const {
    return ROMSFeedColumns::beamOffsetQuant();}
  const ROArrayMeasColumn<MDirection>& beamOffsetMeas() const {
    return ROMSFeedColumns::beamOffsetMeas();}
  const ROScalarColumn<Int>& feedId() const {
    return ROMSFeedColumns::feedId();}
  const ROScalarColumn<Double>& interval() const {
    return ROMSFeedColumns::interval();}
  const ROScalarQuantColumn<Double>& intervalQuant() const {
    return ROMSFeedColumns::intervalQuant();}
  const ROScalarColumn<Int>& numReceptors() const {
    return ROMSFeedColumns::numReceptors();}
  const ROArrayColumn<Complex>& polResponse() const {
    return ROMSFeedColumns::polResponse();}
  const ROArrayColumn<String>& polarizationType() const {
    return ROMSFeedColumns::polarizationType();}
  const ROArrayColumn<Double>& position() const {
    return ROMSFeedColumns::position();}
  const ROArrayQuantColumn<Double>& positionQuant() const {
    return ROMSFeedColumns::positionQuant();}
  const ROScalarMeasColumn<MPosition>& positionMeas() const {
    return ROMSFeedColumns::positionMeas();}
  const ROArrayColumn<Double>& receptorAngle() const {
    return ROMSFeedColumns::receptorAngle();}
  const ROArrayQuantColumn<Double>& receptorAngleQuant() const {
    return ROMSFeedColumns::receptorAngleQuant();}
  const ROScalarColumn<Int>& spectralWindowId() const {
    return ROMSFeedColumns::spectralWindowId();}
  const ROScalarColumn<Double>& time() const {
    return ROMSFeedColumns::time();}
  const ROScalarQuantColumn<Double>& timeQuant() const {
    return ROMSFeedColumns::timeQuant();}
  const ROScalarMeasColumn<MEpoch>& timeMeas() const {
    return ROMSFeedColumns::timeMeas();}
  // </group>

  // Read-only access to optional columns
  // <group>
  const ROScalarColumn<Double>& focusLength() const {
    return ROMSFeedColumns::focusLength();}
  const ROScalarQuantColumn<Double>& focusLengthQuant() const {
    return ROMSFeedColumns::focusLengthQuant();}
  const ROScalarColumn<Int>& phasedFeedId() const {
    return ROMSFeedColumns::phasedFeedId();}
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
  MSFeedColumns();

  //# attach this object to the supplied table.
  void attach(MSFeed& msFeed);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  MSFeedColumns(const MSFeedColumns&);
  MSFeedColumns& operator=(const MSFeedColumns&);

  //# Check if any optional columns exist and if so attach them.
  void attachOptionalCols(MSFeed& msFeed);
  
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

} //# NAMESPACE CASACORE - END

#endif
