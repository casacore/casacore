//# NewMSPointingColumns.h: provides easy access to NewMSPointing columns
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

#if !defined(AIPS_NEWMSPOINTINGCOLUMNS_H)
#define AIPS_NEWMSPOINTINGCOLUMNS_H

#include <aips/aips.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MEpoch.h>
#include <aips/TableMeasures/ArrayMeasColumn.h>
#include <aips/TableMeasures/ScalarMeasColumn.h>
#include <aips/TableMeasures/ScalarQuantColumn.h>
#include <aips/Tables/ArrayColumn.h>
#include <aips/Tables/ScalarColumn.h>

class NewMSPointing;

// <summary>
// A class to provide easy read-only access to NewMSPointing columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> NewMSPointing
//   <li> ArrayColumn
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// RONewMSPointingColumns stands for Read-Only NewMeasurementSet Pointing Table columns.
// </etymology>
//
// <synopsis>
// This class provides read-only access to the columns in the NewMSPointing
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

class RONewMSPointingColumns
{
public:
  // Create a columns object that accesses the data in the specified Table
  RONewMSPointingColumns(const NewMSPointing& msPointing);

  // The destructor does nothing special
  ~RONewMSPointingColumns();

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
  Int pointingIndex(Int antenna, Double time) const;

  // Convenience function that returns the number of rows in any of the columns
  uInt nrow() const {return antennaId_p.nrow();}

protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  RONewMSPointingColumns();

  //# attach this object to the supplied table.
  void attach(const NewMSPointing& msPointing);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  RONewMSPointingColumns(const RONewMSPointingColumns&);
  RONewMSPointingColumns& operator=(const RONewMSPointingColumns&);

  //# Check if any optional columns exist and if so attach them.
  void attachOptionalCols(const NewMSPointing& msPointing);
  
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
// A class to provide easy read-write access to NewMSPointing columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> NewMSPointing
//   <li> ArrayColumn
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// NewMSPointingColumns stands for NewMeasurementSet Pointing Table columns.
// </etymology>
//
// <synopsis>
// This class provides access to the columns in the NewMSPointing Table,
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

class NewMSPointingColumns: public RONewMSPointingColumns
{
public:
  // Construct from the supplied Table
  NewMSPointingColumns(NewMSPointing& msPointing);

  // The destructor does nothing special
  ~NewMSPointingColumns();

  // Read-write access to required columns
  //
  // Note that the direction measures with a stored polynomial have Col() added
  // to their name. They are better accessed via the functions that have the
  // same name, without the Col suffix, that will do the interpolation for
  // you. These functions are in the RONewMSPointingColumns class.
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
  // you. These functions are in the RONewMSPointingColumns class.
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
    return RONewMSPointingColumns::antennaId();}
  const ROScalarColumn<Double>& time() const {
    return RONewMSPointingColumns::time();}
  const ROScalarQuantColumn<Double>& timeQuant() const {
    return RONewMSPointingColumns::timeQuant();}
  const ROScalarMeasColumn<MEpoch>& timeMeas() const {
    return RONewMSPointingColumns::timeMeas();}
  const ROScalarColumn<Double>& interval() const {
    return RONewMSPointingColumns::interval();}
  const ROScalarQuantColumn<Double>& intervalQuant() const {
    return RONewMSPointingColumns::intervalQuant();}
  const ROScalarColumn<String>& name() const {
    return RONewMSPointingColumns::name();}
  const ROScalarColumn<Int>& numPoly() const {
    return RONewMSPointingColumns::numPoly();}
  const ROScalarColumn<Double>& timeOrigin() const {
    return RONewMSPointingColumns::timeOrigin();}
  const ROScalarQuantColumn<Double>& timeOriginQuant() const {
    return RONewMSPointingColumns::timeOriginQuant();}
  const ROScalarMeasColumn<MEpoch>& timeOriginMeas() const {
    return RONewMSPointingColumns::timeOriginMeas();}
  const ROArrayColumn<Double>& direction() const {
    return RONewMSPointingColumns::direction();}
  const ROArrayMeasColumn<MDirection>& directionMeasCol() const {
    return RONewMSPointingColumns::directionMeasCol();}
  const ROArrayColumn<Double>& target() const {
    return RONewMSPointingColumns::target();}
  const ROArrayMeasColumn<MDirection>& targetMeasCol()const {
    return RONewMSPointingColumns::targetMeasCol();}
  const ROScalarColumn<Bool>& tracking() const {
    return RONewMSPointingColumns::tracking();}
  // </group>

  // Access to optional columns
  // <group>
  const ROArrayColumn<Double>& pointingOffset() const {
    return RONewMSPointingColumns::pointingOffset();}
  const ROArrayMeasColumn<MDirection>& pointingOffsetMeasCol() const {
    return RONewMSPointingColumns::pointingOffsetMeasCol();}
  const ROArrayColumn<Double>& sourceOffset() const {
    return RONewMSPointingColumns::sourceOffset();}
  const ROArrayMeasColumn<MDirection>& sourceOffsetMeasCol() const {
    return RONewMSPointingColumns::sourceOffsetMeasCol();}
  const ROArrayColumn<Double>& encoder() const {
    return RONewMSPointingColumns::encoder();}
  const ROScalarMeasColumn<MDirection>& encoderMeas() const {
    return RONewMSPointingColumns::encoderMeas();}
  const ROScalarColumn<Int>& pointingModelId() const {
    return RONewMSPointingColumns::pointingModelId();}
  const ROScalarColumn<Bool>& onSource() const {
    return RONewMSPointingColumns::onSource();}
  const ROScalarColumn<Bool>& overTheTop() const {
    return RONewMSPointingColumns::overTheTop();}
  // </group>
  
  // set the epoch reference type for the TIME & TIME_ORIGIN column. This can
  // only be done when the table has no rows. Trying to do so at other times
  // will throw an exception.
  void setEpochRef(MEpoch::Types ref);

  // set the direction reference type for the DIRECTION, TARGET & and, if
  // defined, the SOURCE_OFFSET & POINTING_OFFSET columns. This can only be
  // done when the table has no rows. Trying to do so at other times will throw
  // an exception. Note the optional ENCODERE column must be done separately as
  // the MSv2 definition allows this column to have a different frame.
  void setDirectionRef(MDirection::Types ref);

  // set the irection reference type for the ENCODER column (if it is defined).
  // This can only be done when the table has no rows. Trying to do so at other
  // times will throw an exception.
  void setEncoderDirectionRef(MDirection::Types ref);

protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  NewMSPointingColumns();

  //# attach this object to the supplied table.
  void attach(NewMSPointing& msPointing);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  NewMSPointingColumns(const NewMSPointingColumns&);
  NewMSPointingColumns& operator=(const NewMSPointingColumns&);

  //# Check if any optional columns exist and if so attach them.
  void attachOptionalCols(NewMSPointing& msPointing);
  
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
#endif
