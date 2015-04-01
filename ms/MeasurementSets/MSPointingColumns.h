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
//#
//# $Id$

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
// A class to provide easy read-only access to MSPointing columns
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
// ROMSPointingColumns stands for Read-Only MeasurementSet Pointing Table columns.
// </etymology>
//
// <synopsis>
// This class provides read-only access to the columns in the MSPointing
// Table.  It does the declaration of all the Scalar and ArrayColumns with the
// correct types, so the application programmer doesn't have to worry about
// getting those right. There is an access function for every predefined
// column. Access to non-predefined columns will still have to be done with
// explicit declarations.  See <linkto class=ROMSColumns>
// ROMSColumns</linkto> for an example.
// </synopsis>
//
// <motivation>
// See <linkto class=MSColumns> MSColumns</linkto> for the motivation.
// </motivation>

class ROMSPointingColumns
{
public:
  // Create a columns object that accesses the data in the specified Table
  ROMSPointingColumns(const MSPointing& msPointing);

  // The destructor does nothing special
  ~ROMSPointingColumns();

  // Access to required columns
  // <group>
  const ROScalarColumn<Int>& antennaId() const {return antennaId_p;}
  const ROScalarColumn<Double>& time() const {return time_p;}
  const ROScalarQuantColumn<Double>& timeQuant() const {return timeQuant_p;}
  const ROScalarMeasColumn<MEpoch>& timeMeas() const {return timeMeas_p;}
  const ROScalarColumn<Double>& interval() const {return interval_p;}
  const ROScalarQuantColumn<Double>& intervalQuant() const {
    return intervalQuant_p;}
  const ROScalarColumn<String>& name() const {return name_p;}
  const ROScalarColumn<Int>& numPoly() const {return numPoly_p;}
  const ROScalarColumn<Double>& timeOrigin() const {return timeOrigin_p;}
  const ROScalarQuantColumn<Double>& timeOriginQuant() const {
   return timeOriginQuant_p;}
  const ROScalarMeasColumn<MEpoch>& timeOriginMeas() const {
    return timeOriginMeas_p;}
  const ROArrayColumn<Double>& direction() const {return direction_p;}
  const ROArrayMeasColumn<MDirection>& directionMeasCol() const {
    return directionMeas_p;}
  const ROArrayColumn<Double>& target() const {return target_p;}
  const ROArrayMeasColumn<MDirection>& targetMeasCol()const {
    return targetMeas_p;}
  const ROScalarColumn<Bool>& tracking() const {return tracking_p;}
  // </group>

  // Access to optional columns
  // <group>
  const ROArrayColumn<Double>& pointingOffset() const {
    return pointingOffset_p;}
  const ROArrayMeasColumn<MDirection>& pointingOffsetMeasCol() const {
    return pointingOffsetMeas_p;}
  const ROArrayColumn<Double>& sourceOffset() const {return sourceOffset_p;}
  const ROArrayMeasColumn<MDirection>& sourceOffsetMeasCol() const {
    return sourceOffsetMeas_p;}
  const ROArrayColumn<Double>& encoder() const {return encoder_p;}
  const ROScalarMeasColumn<MDirection>& encoderMeas() const {
    return encoderMeas_p;}
  const ROScalarColumn<Int>& pointingModelId() const {
    return pointingModelId_p;}
  const ROScalarColumn<Bool>& onSource() const {return onSource_p;}
  const ROScalarColumn<Bool>& overTheTop() const {return overTheTop_p;}
  // </group>
  
  // Access to interpolated directions, the default time of zero will
  // return the 0th order element of the polynomial.
  // <group>
  MDirection directionMeas(Int row, Double time = 0) const;
  MDirection targetMeas(Int row, Double time = 0) const;
  MDirection pointingOffsetMeas(Int row, Double time = 0) const;
  MDirection sourceOffsetMeas(Int row, Double time = 0) const;
  // </group>

  // return the first matching row index for this time and antenna, 
  // returns -1 if no match was found
  // For long tables you may give a guess row...the last return
  // is usually a good one.
  Int pointingIndex(Int antenna, Double time, Int guessRow=0) const;

  // Convenience function that returns the number of rows in any of the columns
  uInt nrow() const {return antennaId_p.nrow();}

protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  ROMSPointingColumns();

  //# attach this object to the supplied table.
  void attach(const MSPointing& msPointing);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  ROMSPointingColumns(const ROMSPointingColumns&);
  ROMSPointingColumns& operator=(const ROMSPointingColumns&);

  //# Check if any optional columns exist and if so attach them.
  void attachOptionalCols(const MSPointing& msPointing);
  
  //# required columns
  ROScalarColumn<Int> antennaId_p;
  ROArrayColumn<Double> direction_p;
  ROScalarColumn<Double> interval_p;
  ROScalarColumn<String> name_p;
  ROScalarColumn<Int> numPoly_p;
  ROArrayColumn<Double> target_p;
  ROScalarColumn<Double> time_p;
  ROScalarColumn<Double> timeOrigin_p;
  ROScalarColumn<Bool> tracking_p;
  //# optional columns
  ROArrayColumn<Double> encoder_p;
  ROScalarColumn<Bool> onSource_p;
  ROScalarColumn<Int> pointingModelId_p;
  ROArrayColumn<Double> pointingOffset_p;
  ROArrayColumn<Double> sourceOffset_p;
  ROScalarColumn<Bool> overTheTop_p;

  //# Access to Measure columns
  ROArrayMeasColumn<MDirection> directionMeas_p;
  ROArrayMeasColumn<MDirection> targetMeas_p;
  ROScalarMeasColumn<MEpoch> timeMeas_p;
  ROScalarMeasColumn<MEpoch> timeOriginMeas_p;
  //# optional Measure columns
  ROScalarMeasColumn<MDirection> encoderMeas_p;
  ROArrayMeasColumn<MDirection> pointingOffsetMeas_p;
  ROArrayMeasColumn<MDirection> sourceOffsetMeas_p;

  //# Access to Quantum columns
  ROScalarQuantColumn<Double> intervalQuant_p;
  ROScalarQuantColumn<Double> timeQuant_p;
  ROScalarQuantColumn<Double> timeOriginQuant_p;
};

// <summary>
// A class to provide easy read-write access to MSPointing columns
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

class MSPointingColumns: public ROMSPointingColumns
{
public:
  // Construct from the supplied Table
  MSPointingColumns(MSPointing& msPointing);

  // The destructor does nothing special
  ~MSPointingColumns();

  // Read-write access to required columns
  //
  // Note that the direction measures with a stored polynomial have Col() added
  // to their name. They are better accessed via the functions that have the
  // same name, without the Col suffix, that will do the interpolation for
  // you. These functions are in the ROMSPointingColumns class.
  // <group>
  ScalarColumn<Int>& antennaId() {return antennaId_p;}
  ScalarColumn<Double>& time() {return time_p;}
  ScalarMeasColumn<MEpoch>& timeMeas() {return timeMeas_p;}
  ScalarQuantColumn<Double>& timeQuant() {return timeQuant_p;}
  ScalarColumn<Double>& interval() {return interval_p;}
  ScalarQuantColumn<Double>& intervalQuant() {return intervalQuant_p;}
  ScalarColumn<String>& name() {return name_p;}
  ScalarColumn<Int>& numPoly() {return numPoly_p;}
  ScalarColumn<Double>& timeOrigin() {return timeOrigin_p;}
  ScalarQuantColumn<Double>& timeOriginQuant() {return timeOriginQuant_p;}
  ScalarMeasColumn<MEpoch>& timeOriginMeas() {return timeOriginMeas_p;}
  ArrayColumn<Double>& direction() {return direction_p;}
  ArrayMeasColumn<MDirection>& directionMeasCol() {return directionMeas_p;}
  ArrayColumn<Double>& target() {return target_p;}
  ArrayMeasColumn<MDirection>& targetMeasCol() {return targetMeas_p;}
  ScalarColumn<Bool>& tracking() {return tracking_p;}
  // </group>

  // Read-write access to optional columns
  // 
  // Note that the direction measures with a stored polynomial have Col() added
  // to their name. They are better accessed via the functions that have the
  // same name, without the Col suffix, that will do the interpolation for
  // you. These functions are in the ROMSPointingColumns class.
  // <group>
  ArrayColumn<Double>& pointingOffset() {return pointingOffset_p;}
  ArrayMeasColumn<MDirection>& pointingOffsetMeasCol() {
    return pointingOffsetMeas_p;}
  ArrayColumn<Double>& sourceOffset() {return sourceOffset_p;}
  ArrayMeasColumn<MDirection>& sourceOffsetMeasCol() {
    return sourceOffsetMeas_p;}
  ArrayColumn<Double>& encoder() {return encoder_p;}
  ScalarMeasColumn<MDirection>& encoderMeas() {return encoderMeas_p;}
  ScalarColumn<Int>& pointingModelId() {return pointingModelId_p;}
  ScalarColumn<Bool>& onSource() {return onSource_p;}
  ScalarColumn<Bool>& overTheTop() {return overTheTop_p;}
  // </group>

  // Read-only access to required columns
  // <group>
  const ROScalarColumn<Int>& antennaId() const {
    return ROMSPointingColumns::antennaId();}
  const ROScalarColumn<Double>& time() const {
    return ROMSPointingColumns::time();}
  const ROScalarQuantColumn<Double>& timeQuant() const {
    return ROMSPointingColumns::timeQuant();}
  const ROScalarMeasColumn<MEpoch>& timeMeas() const {
    return ROMSPointingColumns::timeMeas();}
  const ROScalarColumn<Double>& interval() const {
    return ROMSPointingColumns::interval();}
  const ROScalarQuantColumn<Double>& intervalQuant() const {
    return ROMSPointingColumns::intervalQuant();}
  const ROScalarColumn<String>& name() const {
    return ROMSPointingColumns::name();}
  const ROScalarColumn<Int>& numPoly() const {
    return ROMSPointingColumns::numPoly();}
  const ROScalarColumn<Double>& timeOrigin() const {
    return ROMSPointingColumns::timeOrigin();}
  const ROScalarQuantColumn<Double>& timeOriginQuant() const {
    return ROMSPointingColumns::timeOriginQuant();}
  const ROScalarMeasColumn<MEpoch>& timeOriginMeas() const {
    return ROMSPointingColumns::timeOriginMeas();}
  const ROArrayColumn<Double>& direction() const {
    return ROMSPointingColumns::direction();}
  const ROArrayMeasColumn<MDirection>& directionMeasCol() const {
    return ROMSPointingColumns::directionMeasCol();}
  const ROArrayColumn<Double>& target() const {
    return ROMSPointingColumns::target();}
  const ROArrayMeasColumn<MDirection>& targetMeasCol()const {
    return ROMSPointingColumns::targetMeasCol();}
  const ROScalarColumn<Bool>& tracking() const {
    return ROMSPointingColumns::tracking();}
  // </group>

  // Access to optional columns
  // <group>
  const ROArrayColumn<Double>& pointingOffset() const {
    return ROMSPointingColumns::pointingOffset();}
  const ROArrayMeasColumn<MDirection>& pointingOffsetMeasCol() const {
    return ROMSPointingColumns::pointingOffsetMeasCol();}
  const ROArrayColumn<Double>& sourceOffset() const {
    return ROMSPointingColumns::sourceOffset();}
  const ROArrayMeasColumn<MDirection>& sourceOffsetMeasCol() const {
    return ROMSPointingColumns::sourceOffsetMeasCol();}
  const ROArrayColumn<Double>& encoder() const {
    return ROMSPointingColumns::encoder();}
  const ROScalarMeasColumn<MDirection>& encoderMeas() const {
    return ROMSPointingColumns::encoderMeas();}
  const ROScalarColumn<Int>& pointingModelId() const {
    return ROMSPointingColumns::pointingModelId();}
  const ROScalarColumn<Bool>& onSource() const {
    return ROMSPointingColumns::onSource();}
  const ROScalarColumn<Bool>& overTheTop() const {
    return ROMSPointingColumns::overTheTop();}
  // </group>
  
  // set the epoch reference type for the TIME & TIME_ORIGIN column.
  // <note role=tip>
  // In principle this function can only be used if the table is empty,
  // otherwise already written values may thereafter have an incorrect
  // reference, offset, or unit.  However, it is possible that part of the
  // table gets written before these values are known.  In that case the
  // reference, offset, or units can be set by using a False
  // <src>tableMustBeEmpty</src> argument.
  // </note>
  void setEpochRef(MEpoch::Types ref, Bool tableMustBeEmpty=True);

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
  void attach(MSPointing& msPointing);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  MSPointingColumns(const MSPointingColumns&);
  MSPointingColumns& operator=(const MSPointingColumns&);

  //# Check if any optional columns exist and if so attach them.
  void attachOptionalCols(MSPointing& msPointing);
  
  //# required columns
  ScalarColumn<Int> antennaId_p;
  ArrayColumn<Double> direction_p;
  ScalarColumn<Double> interval_p;
  ScalarColumn<String> name_p;
  ScalarColumn<Int> numPoly_p;
  ArrayColumn<Double> target_p;
  ScalarColumn<Double> time_p;
  ScalarColumn<Double> timeOrigin_p;
  ScalarColumn<Bool> tracking_p;
  //# optional columns
  ArrayColumn<Double> encoder_p;
  ScalarColumn<Bool> onSource_p;
  ScalarColumn<Int> pointingModelId_p;
  ArrayColumn<Double> pointingOffset_p;
  ArrayColumn<Double> sourceOffset_p;
  ScalarColumn<Bool> overTheTop_p;

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
  ScalarQuantColumn<Double> intervalQuant_p;
  ScalarQuantColumn<Double> timeQuant_p;
  ScalarQuantColumn<Double> timeOriginQuant_p;
};

} //# NAMESPACE CASACORE - END

#endif
