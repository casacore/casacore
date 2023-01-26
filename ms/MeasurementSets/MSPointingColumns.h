//# MSPointingColumns.h: provides easy access to MSPointing columns
//# Copyright (C) 1999,2000
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

#ifndef MS_MSPOINTINGCOLUMNS_H
#define MS_MSPOINTINGCOLUMNS_H

#include <casacore/casa/aips.h>
#include <casacore/measures/Measures/MDirection.h>
#include <casacore/measures/Measures/MEpoch.h>
#include <casacore/measures/Measures/MCDirection.h>
#include <casacore/measures/Measures/MCEpoch.h>
#include <casacore/measures/TableMeasures/ArrayMeasColumn.h>
#include <casacore/measures/TableMeasures/ScalarMeasColumn.h>
#include <casacore/measures/TableMeasures/ScalarQuantColumn.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/Tables/ScalarColumn.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class MSPointing;

// <summary>
// A class to provide easy access to MSPointing columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> MSPointing
//   <li> ArrayColumn
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// MSPointingColumns stands for MeasurementSet Pointing Table columns.
// </etymology>
//
// <synopsis>
// This class provides access to the columns in the MSPointing Table,
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

class MSPointingColumns
{
public:
  // Construct from the supplied Table
  MSPointingColumns(const MSPointing& msPointing);

  // The destructor does nothing special
  ~MSPointingColumns();

  // Access to required columns
  //
  // Note that the direction measures with a stored polynomial have Col() added
  // to their name. They are better accessed via the functions that have the
  // same name, without the Col suffix, that will do the interpolation for
  // you.
  // <group>
  ScalarColumn<int32_t>& antennaId() {return antennaId_p;}
  ScalarColumn<double>& time() {return time_p;}
  ScalarMeasColumn<MEpoch>& timeMeas() {return timeMeas_p;}
  ScalarQuantColumn<double>& timeQuant() {return timeQuant_p;}
  ScalarColumn<double>& interval() {return interval_p;}
  ScalarQuantColumn<double>& intervalQuant() {return intervalQuant_p;}
  ScalarColumn<String>& name() {return name_p;}
  ScalarColumn<int32_t>& numPoly() {return numPoly_p;}
  ScalarColumn<double>& timeOrigin() {return timeOrigin_p;}
  ScalarQuantColumn<double>& timeOriginQuant() {return timeOriginQuant_p;}
  ScalarMeasColumn<MEpoch>& timeOriginMeas() {return timeOriginMeas_p;}
  ArrayColumn<double>& direction() {return direction_p;}
  ArrayMeasColumn<MDirection>& directionMeasCol() {return directionMeas_p;}
  ArrayColumn<double>& target() {return target_p;}
  ArrayMeasColumn<MDirection>& targetMeasCol() {return targetMeas_p;}
  ScalarColumn<bool>& tracking() {return tracking_p;}
  // </group>

  // Access to optional columns
  // 
  // Note that the direction measures with a stored polynomial have Col() added
  // to their name. They are better accessed via the functions that have the
  // same name, without the Col suffix, that will do the interpolation for
  // you.
  // <group>
  ArrayColumn<double>& pointingOffset() {return pointingOffset_p;}
  ArrayMeasColumn<MDirection>& pointingOffsetMeasCol() {
    return pointingOffsetMeas_p;}
  ArrayColumn<double>& sourceOffset() {return sourceOffset_p;}
  ArrayMeasColumn<MDirection>& sourceOffsetMeasCol() {
    return sourceOffsetMeas_p;}
  ArrayColumn<double>& encoder() {return encoder_p;}
  ScalarMeasColumn<MDirection>& encoderMeas() {return encoderMeas_p;}
  ScalarColumn<int32_t>& pointingModelId() {return pointingModelId_p;}
  ScalarColumn<bool>& onSource() {return onSource_p;}
  ScalarColumn<bool>& overTheTop() {return overTheTop_p;}
  // </group>

  // Const access to required columns
  // <group>
  const ScalarColumn<int32_t>& antennaId() const {return antennaId_p;}
  const ScalarColumn<double>& time() const {return time_p;}
  const ScalarQuantColumn<double>& timeQuant() const {return timeQuant_p;}
  const ScalarMeasColumn<MEpoch>& timeMeas() const {return timeMeas_p;}
  const ScalarColumn<double>& interval() const {return interval_p;}
  const ScalarQuantColumn<double>& intervalQuant() const {
    return intervalQuant_p;}
  const ScalarColumn<String>& name() const {return name_p;}
  const ScalarColumn<int32_t>& numPoly() const {return numPoly_p;}
  const ScalarColumn<double>& timeOrigin() const {return timeOrigin_p;}
  const ScalarQuantColumn<double>& timeOriginQuant() const {
   return timeOriginQuant_p;}
  const ScalarMeasColumn<MEpoch>& timeOriginMeas() const {
    return timeOriginMeas_p;}
  const ArrayColumn<double>& direction() const {return direction_p;}
  const ArrayMeasColumn<MDirection>& directionMeasCol() const {
    return directionMeas_p;}
  const ArrayColumn<double>& target() const {return target_p;}
  const ArrayMeasColumn<MDirection>& targetMeasCol()const {
    return targetMeas_p;}
  const ScalarColumn<bool>& tracking() const {return tracking_p;}
  // </group>

  // Access to optional columns
  // <group>
  const ArrayColumn<double>& pointingOffset() const {
    return pointingOffset_p;}
  const ArrayMeasColumn<MDirection>& pointingOffsetMeasCol() const {
    return pointingOffsetMeas_p;}
  const ArrayColumn<double>& sourceOffset() const {return sourceOffset_p;}
  const ArrayMeasColumn<MDirection>& sourceOffsetMeasCol() const {
    return sourceOffsetMeas_p;}
  const ArrayColumn<double>& encoder() const {return encoder_p;}
  const ScalarMeasColumn<MDirection>& encoderMeas() const {
    return encoderMeas_p;}
  const ScalarColumn<int32_t>& pointingModelId() const {
    return pointingModelId_p;}
  const ScalarColumn<bool>& onSource() const {return onSource_p;}
  const ScalarColumn<bool>& overTheTop() const {return overTheTop_p;}
  // </group>
  
  // Access to interpolated directions, the default time of zero will
  // return the 0th order element of the polynomial.
  // <group>
  MDirection directionMeas(rownr_t row, double time = 0) const;
  MDirection targetMeas(rownr_t row, double time = 0) const;
  MDirection pointingOffsetMeas(rownr_t row, double time = 0) const;
  MDirection sourceOffsetMeas(rownr_t row, double time = 0) const;
  // </group>

  // return the first matching row index for this time and antenna, 
  // returns -1 if no match was found
  // For long tables you may give a guess row...the last return
  // is usually a good one.
  int64_t pointingIndex(int32_t antenna, double time, int64_t guessRow=0) const;

  // Convenience function that returns the number of rows in any of the columns
  rownr_t nrow() const {return antennaId_p.nrow();}

  // set the epoch reference type for the TIME & TIME_ORIGIN column.
  // <note role=tip>
  // In principle this function can only be used if the table is empty,
  // otherwise already written values may thereafter have an incorrect
  // reference, offset, or unit.  However, it is possible that part of the
  // table gets written before these values are known.  In that case the
  // reference, offset, or units can be set by using a false
  // <src>tableMustBeEmpty</src> argument.
  // </note>
  void setEpochRef(MEpoch::Types ref, bool tableMustBeEmpty=true);

  // set the direction reference type for the DIRECTION, TARGET & and, if
  // defined, the SOURCE_OFFSET & POINTING_OFFSET columns. This can only be
  // done when the table has no rows. Trying to do so at other times will throw
  // an exception.  Note that the optional ENCODER column must be done
  // separately as the MSv2 definition requires this column to use the frame(s)
  // of the antenna mounts.
  void setDirectionRef(MDirection::Types ref);

  // set the direction reference type for the ENCODER column (if it is defined).
  // This can only be done when the table has no rows. Trying to do so at other
  // times will throw an exception.
  void setEncoderDirectionRef(MDirection::Types ref);

protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  MSPointingColumns();

  //# attach this object to the supplied table.
  void attach(const MSPointing& msPointing);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  MSPointingColumns(const MSPointingColumns&);
  MSPointingColumns& operator=(const MSPointingColumns&);

  //# Check if any optional columns exist and if so attach them.
  void attachOptionalCols(const MSPointing& msPointing);
  
  //# required columns
  ScalarColumn<int32_t> antennaId_p;
  ArrayColumn<double> direction_p;
  ScalarColumn<double> interval_p;
  ScalarColumn<String> name_p;
  ScalarColumn<int32_t> numPoly_p;
  ArrayColumn<double> target_p;
  ScalarColumn<double> time_p;
  ScalarColumn<double> timeOrigin_p;
  ScalarColumn<bool> tracking_p;
  //# optional columns
  ArrayColumn<double> encoder_p;
  ScalarColumn<bool> onSource_p;
  ScalarColumn<int32_t> pointingModelId_p;
  ArrayColumn<double> pointingOffset_p;
  ArrayColumn<double> sourceOffset_p;
  ScalarColumn<bool> overTheTop_p;

  //# Access to Measure columns
  ArrayMeasColumn<MDirection> directionMeas_p;
  ArrayMeasColumn<MDirection> targetMeas_p;
  ScalarMeasColumn<MEpoch> timeMeas_p;
  ScalarMeasColumn<MEpoch> timeOriginMeas_p;
  //# optional Measure columns
  ScalarMeasColumn<MDirection> encoderMeas_p;
  ArrayMeasColumn<MDirection> pointingOffsetMeas_p;
  ArrayMeasColumn<MDirection> sourceOffsetMeas_p;

  //# Access to Quantum columns
  ScalarQuantColumn<double> intervalQuant_p;
  ScalarQuantColumn<double> timeQuant_p;
  ScalarQuantColumn<double> timeOriginQuant_p;
};

//# Define the RO version for backward compatibility.
typedef MSPointingColumns ROMSPointingColumns;

} //# NAMESPACE CASACORE - END

#endif
