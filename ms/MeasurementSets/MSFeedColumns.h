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
// A class to provide easy access to MSFeed columns
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

class MSFeedColumns
{
public:
  // Create a columns object that accesses the data in the specified Table
  MSFeedColumns(const MSFeed& msFeed);

  // The desctructor does nothing special
  ~MSFeedColumns();

  // Access to required columns
  // <group>
  ScalarColumn<int32_t>& antennaId() {return antennaId_p;}
  ScalarColumn<int32_t>& beamId() {return beamId_p;}
  ArrayColumn<double>& beamOffset() {return beamOffset_p;}
  ArrayQuantColumn<double>& beamOffsetQuant() { return beamOffsetQuant_p;}
  ArrayMeasColumn<MDirection>& beamOffsetMeas() 
    {return beamOffsetMeas_p;}
  ScalarColumn<int32_t>& feedId() {return feedId_p;}
  ScalarColumn<double>& interval() {return interval_p;}
  ScalarQuantColumn<double>& intervalQuant() { return intervalQuant_p;}
  ScalarColumn<int32_t>& numReceptors() {return numReceptors_p;}
  ArrayColumn<Complex>& polResponse() {return polResponse_p;}
  ArrayColumn<String>& polarizationType() {return polarizationType_p;}
  ArrayColumn<double>& position() {return position_p;}
  ArrayQuantColumn<double>& positionQuant() {return positionQuant_p;}
  ScalarMeasColumn<MPosition>& positionMeas() 
    { return positionMeas_p;}
  ArrayColumn<double>& receptorAngle() {return receptorAngle_p;}
  ArrayQuantColumn<double>& receptorAngleQuant() {
    return receptorAngleQuant_p;}
  ScalarColumn<int32_t>& spectralWindowId() {return spectralWindowId_p;}
  ScalarColumn<double>& time() {return time_p;}
  ScalarQuantColumn<double>& timeQuant() { return timeQuant_p;}
  ScalarMeasColumn<MEpoch>& timeMeas() { return timeMeas_p;}
  // </group>

  // Const access to required columns
  // <group>
  const ScalarColumn<int32_t>& antennaId() const {return antennaId_p;}
  const ScalarColumn<int32_t>& beamId() const {return beamId_p;}
  const ArrayColumn<double>& beamOffset() const {return beamOffset_p;}
  const ArrayQuantColumn<double>& beamOffsetQuant() const { return beamOffsetQuant_p;}
  const ArrayMeasColumn<MDirection>& beamOffsetMeas() const 
    {return beamOffsetMeas_p;}
  const ScalarColumn<int32_t>& feedId() const {return feedId_p;}
  const ScalarColumn<double>& interval() const {return interval_p;}
  const ScalarQuantColumn<double>& intervalQuant() const { return intervalQuant_p;}
  const ScalarColumn<int32_t>& numReceptors() const {return numReceptors_p;}
  const ArrayColumn<Complex>& polResponse() const {return polResponse_p;}
  const ArrayColumn<String>& polarizationType() const {return polarizationType_p;}
  const ArrayColumn<double>& position() const {return position_p;}
  const ArrayQuantColumn<double>& positionQuant() const {return positionQuant_p;}
  const ScalarMeasColumn<MPosition>& positionMeas() const 
    { return positionMeas_p;}
  const ArrayColumn<double>& receptorAngle() const {return receptorAngle_p;}
  const ArrayQuantColumn<double>& receptorAngleQuant() const {
    return receptorAngleQuant_p;}
  const ScalarColumn<int32_t>& spectralWindowId() const {return spectralWindowId_p;}
  const ScalarColumn<double>& time() const {return time_p;}
  const ScalarQuantColumn<double>& timeQuant() const { return timeQuant_p;}
  const ScalarMeasColumn<MEpoch>& timeMeas() const { return timeMeas_p;}
  // </group>

  // Access to optional columns
  // <group>
  ScalarColumn<double>& focusLength() {return focusLength_p;}
  ScalarQuantColumn<double>& focusLengthQuant() { return focusLengthQuant_p;}
  ScalarColumn<int32_t>& phasedFeedId() {return phasedFeedId_p;}
  // </group>

  // Const access to optional columns
  // <group>
  const ScalarColumn<double>& focusLength() const {return focusLength_p;}
  const ScalarQuantColumn<double>& focusLengthQuant() const { return focusLengthQuant_p;}
  const ScalarColumn<int32_t>& phasedFeedId() const {return phasedFeedId_p;}
  // </group>

  // set the epoch type for the TIME column.
  // <note role=tip>
  // In principle this function can only be used if the table is empty,
  // otherwise already written values may thereafter have an incorrect
  // reference, offset, or unit.  However, it is possible that part of the
  // table gets written before these values are known.  In that case the
  // reference, offset, or units can be set by using a false
  // <src>tableMustBeEmpty</src> argument.
  // </note>
  void setEpochRef(MEpoch::Types ref, bool tableMustBeEmpty=true);

  // set the direction type for the BEAM_OFFSET column. This can only be done
  // when the table has no rows. Trying to do so at other times will throw an
  // exception.
  void setDirectionRef(MDirection::Types ref);

  // set the position type for the POSITION column. This can only be done when
  // the table has no rows. Trying to do so at other times will throw an
  // exception.
  void setPositionRef(MPosition::Types ref);

  // Convenience function that returns the number of rows in any of the columns
  rownr_t nrow() const {return antennaId_p.nrow();}

  // Returns the last row that contains a feed with the specified values.
  // If no matching row can be found, but a match is possible if the validity
  // time interval is widened, return that row and the suggestion for the
  // new time information.
  // If no change to time is necessary, newTimeQ and newIntervalQ are zero.
  // Returns -1 if no match could be found.
  // Ignore the Feed table rows contained in vector ignoreRows. 
  // focusLengthQ is only compared if this optional column is present and
  // if the value of focusLengthQ is not dimensionless.
  int64_t matchFeed(Quantum<double>& newTimeQ,
                  Quantum<double>& newIntervalQ,
                  int32_t antId,
                  int32_t feedId,
                  int32_t spwId, 
                  const Quantum<double>& timeQ,
                  const Quantum<double>& intervalQ,
                  int32_t numReceptor,
                  const Array<Quantum<double> >& beamOffsetQ,
                  const Array<String>& polType,
                  const Array<Complex>& polResp,
                  const Array<Quantum<double> >& positionQ,
                  const Array<Quantum<double> >& receptorAngleQ,
                  const RowNumbers& ignoreRows,
                  const Quantum<double>& focusLengthQ=Quantum<double>() 
                  );

protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  MSFeedColumns();

  //# attach this object to the supplied table.
  void attach(const MSFeed& msFeed);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  MSFeedColumns(const MSFeedColumns&);
  MSFeedColumns& operator=(const MSFeedColumns&);

  //# Check if any optional columns exist and if so attach them.
  void attachOptionalCols(const MSFeed& msFeed);
  
  //# required columns
  ScalarColumn<int32_t> antennaId_p;
  ScalarColumn<int32_t> beamId_p;
  ArrayColumn<double> beamOffset_p;
  ScalarColumn<int32_t> feedId_p;
  ScalarColumn<double> interval_p;
  ScalarColumn<int32_t> numReceptors_p;
  ArrayColumn<Complex> polResponse_p;
  ArrayColumn<String> polarizationType_p;
  ArrayColumn<double> position_p;
  ArrayColumn<double> receptorAngle_p;
  ScalarColumn<int32_t> spectralWindowId_p;
  ScalarColumn<double> time_p;
  //# optional columns
  ScalarColumn<double> focusLength_p;
  ScalarColumn<int32_t> phasedFeedId_p;

  //# Access to Measure columns
  ArrayMeasColumn<MDirection> beamOffsetMeas_p;
  ScalarMeasColumn<MPosition> positionMeas_p;
  ScalarMeasColumn<MEpoch> timeMeas_p;

  //# Access to Quantum columns
  ArrayQuantColumn<double> beamOffsetQuant_p;
  ScalarQuantColumn<double> intervalQuant_p;
  ArrayQuantColumn<double> positionQuant_p;
  ArrayQuantColumn<double> receptorAngleQuant_p;
  ScalarQuantColumn<double> timeQuant_p;
  //# optional Quantum columns
  ScalarQuantColumn<double> focusLengthQuant_p;
};

//# Define the RO version for backward compatibility.
typedef MSFeedColumns ROMSFeedColumns;

} //# NAMESPACE CASACORE - END

#endif
