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

#if !defined(AIPS_NewMSPOINTINGCOLUMNS_H)
#define AIPS_NewMSPOINTINGCOLUMNS_H

#include <aips/MeasurementSets/NewMSPointing.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Tables/ArrayColumn.h>
#include <aips/Quanta/Unit.h>
#include <aips/Quanta/Quantum.h>
#include <aips/TableMeasures/ScalarMeasColumn.h>
#include <aips/TableMeasures/ArrayMeasColumn.h>
#include <aips/TableMeasures/ScalarQuantColumn.h>
#include <aips/TableMeasures/ArrayQuantColumn.h>

class MEpoch;
#include <aips/Measures/MDirection.h>

// <summary>
// A convenience class to provide easy access to NewMSPointing columns
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

class NewMSPointingColumns
{
public:

NewMSPointingColumns(NewMSPointing& msPointing);

~NewMSPointingColumns();

  // Access to columns
  ScalarColumn<Int>& antennaId() {return antennaId_p;}
  ArrayColumn<Double>& direction() {return direction_p;}
  ScalarColumn<Double>& interval() {return interval_p;}
  ScalarColumn<String>& name() {return name_p;}
  ScalarColumn<Int>& numPoly() {return numPoly_p;}
  ArrayColumn<Double>& target() {return target_p;}
  ScalarColumn<Double>& time() {return time_p;}
  ScalarColumn<Double>& timeOrigin() {return timeOrigin_p;}
  ScalarColumn<Bool>& tracking() {return tracking_p;}
  ArrayColumn<Double>& encoder() {return encoder_p;}
  ScalarColumn<Bool>& onSource() {return onSource_p;}
  ScalarColumn<Int>& pointingModelId() {return pointingModelId_p;}
  ArrayColumn<Double>& pointingOffset() {return pointingOffset_p;}
  ArrayColumn<Double>& sourceOffset() {return sourceOffset_p;}

  // Access to Measure columns
  // Note that the Directions with stored polynomial have Col() added to their
  // name, they are better accessed via the interpolated functions below.
  ArrayMeasColumn<MDirection>& directionMeasCol() 
    {return directionMeas_p;}
  ArrayMeasColumn<MDirection>& targetMeasCol() 
    {return targetMeas_p;}
  ScalarMeasColumn<MEpoch>& timeMeas() { return timeMeas_p;}
  ScalarMeasColumn<MEpoch>& timeOriginMeas() { return timeOriginMeas_p;}
  ScalarMeasColumn<MDirection>& encoderMeas() 
    {return encoderMeas_p;}
  ArrayMeasColumn<MDirection>& pointingOffsetMeasCol() 
    {return pointingOffsetMeas_p;}
  ArrayMeasColumn<MDirection>& sourceOffsetMeasCol() 
    {return sourceOffsetMeas_p;}

  // Access to Quantum columns
  //#  ArrayQuantColumn<Double>& directionQuant() { return directionQuant_p;}
  ScalarQuantColumn<Double>& intervalQuant() { return intervalQuant_p;}
  //#  ArrayQuantColumn<Double>& targetQuant() { return targetQuant_p;}
  ScalarQuantColumn<Double>& timeQuant() { return timeQuant_p;}
  ScalarQuantColumn<Double>& timeOriginQuant() { return timeOriginQuant_p;}
  //#  ArrayQuantColumn<Double>& encoderQuant() { return encoderQuant_p;}
  //#  ArrayQuantColumn<Double>& pointingOffsetQuant() { return pointingOffsetQuant_p;}
  //#  ArrayQuantColumn<Double>& sourceOffsetQuant() { return sourceOffsetQuant_p;}

  // Access to interpolated directions, the default time of zero will
  // return the 0th order element of the polynomial.
  MDirection directionMeas(Int row, Double time = 0);
  MDirection targetMeas(Int row, Double time = 0);
  MDirection pointingOffsetMeas(Int row, Double time = 0);
  MDirection sourceOffsetMeas(Int row, Double time = 0);

  // set the encoder Direction reference 
  void setEncoderDirectionRef(Int ref);

  // return the first matching row index for this time and antenna, 
  // returns -1 if no match was found
  Int pointingIndex(Int antenna, Double time);

private:
  ScalarColumn<Int> antennaId_p;
  ArrayColumn<Double> direction_p;
  ScalarColumn<Double> interval_p;
  ScalarColumn<String> name_p;
  ScalarColumn<Int> numPoly_p;
  ArrayColumn<Double> target_p;
  ScalarColumn<Double> time_p;
  ScalarColumn<Double> timeOrigin_p;
  ScalarColumn<Bool> tracking_p;
  ArrayColumn<Double> encoder_p;
  ScalarColumn<Bool> onSource_p;
  ScalarColumn<Int> pointingModelId_p;
  ArrayColumn<Double> pointingOffset_p;
  ArrayColumn<Double> sourceOffset_p;

  // Access to Measure columns
  ArrayMeasColumn<MDirection> directionMeas_p;
  ArrayMeasColumn<MDirection> targetMeas_p;
  ScalarMeasColumn<MEpoch> timeMeas_p;
  ScalarMeasColumn<MEpoch> timeOriginMeas_p;
  ScalarMeasColumn<MDirection> encoderMeas_p;
  ArrayMeasColumn<MDirection> pointingOffsetMeas_p;
  ArrayMeasColumn<MDirection> sourceOffsetMeas_p;

  // Access to Quantum columns
  //#  ArrayQuantColumn<Double> directionQuant_p;
  ScalarQuantColumn<Double> intervalQuant_p;
  //#  ArrayQuantColumn<Double> targetQuant_p;
  ScalarQuantColumn<Double> timeQuant_p;
  ScalarQuantColumn<Double> timeOriginQuant_p;
  //#  ArrayQuantColumn<Double> encoderQuant_p;
  //#  ArrayQuantColumn<Double> pointingOffsetQuant_p;
  //#  ArrayQuantColumn<Double> sourceOffsetQuant_p;
};

// <summary>
// A convenience class to provide easy access to NewMSPointing columns
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
// This class provides read-only access to the columns in the NewMSPointing Table.
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

class RONewMSPointingColumns
{
public:

RONewMSPointingColumns(const NewMSPointing& msPointing);

~RONewMSPointingColumns();

// Access to columns
  const ROScalarColumn<Int>& antennaId() const {return antennaId_p;}
  const ROArrayColumn<Double>& direction() const {return direction_p;}
  const ROScalarColumn<Double>& interval() const {return interval_p;}
  const ROScalarColumn<String>& name() const {return name_p;}
  const ROScalarColumn<Int>& numPoly() const {return numPoly_p;}
  const ROArrayColumn<Double>& target() const {return target_p;}
  const ROScalarColumn<Double>& time() const {return time_p;}
  const ROScalarColumn<Double>& timeOrigin() const {return timeOrigin_p;}
  const ROScalarColumn<Bool>& tracking() const {return tracking_p;}
  const ROArrayColumn<Double>& encoder() const {return encoder_p;}
  const ROScalarColumn<Bool>& onSource() const {return onSource_p;}
  const ROScalarColumn<Int>& pointingModelId() const {return pointingModelId_p;}
  const ROArrayColumn<Double>& pointingOffset() const {return pointingOffset_p;}
  const ROArrayColumn<Double>& sourceOffset() const {return sourceOffset_p;}

  // Access to Measure columns
  // Note that the Directions with stored polynomial have Col() added to their
  // name, they are better accessed via the interpolated functions below.
  const ROArrayMeasColumn<MDirection>& directionMeasCol() const 
    {return directionMeas_p;}
  const ROArrayMeasColumn<MDirection>& targetMeasCol()const 
    {return targetMeas_p;}
  const ROScalarMeasColumn<MEpoch>& timeMeas() const { return timeMeas_p;}
  const ROScalarMeasColumn<MEpoch>& timeOriginMeas() const { return timeOriginMeas_p;}
  const ROScalarMeasColumn<MDirection>& encoderMeas() const 
    {return encoderMeas_p;}
  const ROArrayMeasColumn<MDirection>& pointingOffsetMeasCol() const 
    {return pointingOffsetMeas_p;}
  const ROArrayMeasColumn<MDirection>& sourceOffsetMeasCol() const 
    {return sourceOffsetMeas_p;}

  // Access to Quantum columns
  //#  const ROArrayQuantColumn<Double>& directionQuant() const { return directionQuant_p;}
  const ROScalarQuantColumn<Double>& intervalQuant() const { return intervalQuant_p;}
  //#  const ROArrayQuantColumn<Double>& targetQuant() const { return targetQuant_p;}
  const ROScalarQuantColumn<Double>& timeQuant() const { return timeQuant_p;}
  const ROScalarQuantColumn<Double>& timeOriginQuant() const { return timeOriginQuant_p;}
  //#  const ROArrayQuantColumn<Double>& encoderQuant() const { return encoderQuant_p;}
  //#  const ROArrayQuantColumn<Double>& pointingOffsetQuant() const { return pointingOffsetQuant_p;}
  //#  const ROArrayQuantColumn<Double>& sourceOffsetQuant() const { return sourceOffsetQuant_p;}

  // Access to interpolated directions, the default time of zero will
  // return the 0th order element of the polynomial.
  MDirection directionMeas(Int row, Double time = 0);
  MDirection targetMeas(Int row, Double time = 0);
  MDirection pointingOffsetMeas(Int row, Double time = 0);
  MDirection sourceOffsetMeas(Int row, Double time = 0);

  // return the first matching row index for this time and antenna, 
  // returns -1 if no match was found
  Int pointingIndex(Int antenna, Double time);

private:
  ROScalarColumn<Int> antennaId_p;
  ROArrayColumn<Double> direction_p;
  ROScalarColumn<Double> interval_p;
  ROScalarColumn<String> name_p;
  ROScalarColumn<Int> numPoly_p;
  ROArrayColumn<Double> target_p;
  ROScalarColumn<Double> time_p;
  ROScalarColumn<Double> timeOrigin_p;
  ROScalarColumn<Bool> tracking_p;
  ROArrayColumn<Double> encoder_p;
  ROScalarColumn<Bool> onSource_p;
  ROScalarColumn<Int> pointingModelId_p;
  ROArrayColumn<Double> pointingOffset_p;
  ROArrayColumn<Double> sourceOffset_p;

  // Access to Measure columns
  ROArrayMeasColumn<MDirection> directionMeas_p;
  ROArrayMeasColumn<MDirection> targetMeas_p;
  ROScalarMeasColumn<MEpoch> timeMeas_p;
  ROScalarMeasColumn<MEpoch> timeOriginMeas_p;
  ROScalarMeasColumn<MDirection> encoderMeas_p;
  ROArrayMeasColumn<MDirection> pointingOffsetMeas_p;
  ROArrayMeasColumn<MDirection> sourceOffsetMeas_p;

  // Access to Quantum columns
  //#  ROArrayQuantColumn<Double> directionQuant_p;
  ROScalarQuantColumn<Double> intervalQuant_p;
  //#  ROArrayQuantColumn<Double> targetQuant_p;
  ROScalarQuantColumn<Double> timeQuant_p;
  ROScalarQuantColumn<Double> timeOriginQuant_p;
  //#  ROArrayQuantColumn<Double> encoderQuant_p;
  //#  ROArrayQuantColumn<Double> pointingOffsetQuant_p;
  //#  ROArrayQuantColumn<Double> sourceOffsetQuant_p;

};

#endif
