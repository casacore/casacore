//# NewMSSourceColumns.h: provides easy access to NewMSSource columns
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

#if !defined(AIPS_NEWMSSOURCECOLUMNS_H)
#define AIPS_NEWMSSOURCECOLUMNS_H

#include <aips/aips.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MEpoch.h>
#include <aips/Measures/MFrequency.h>
#include <aips/Measures/MPosition.h>
#include <aips/Measures/MRadialVelocity.h>
#include <aips/TableMeasures/ArrayMeasColumn.h>
#include <aips/TableMeasures/ArrayQuantColumn.h>
#include <aips/TableMeasures/ScalarMeasColumn.h>
#include <aips/TableMeasures/ScalarQuantColumn.h>
#include <aips/Tables/ArrayColumn.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Utilities/String.h>

class NewMSSource;

// <summary>
// A class to provide easy read-only access to NewMSSource columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> NewMSSource
//   <li> ArrayColumn
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// RONewMSSourceColumns stands for Read-Only NewMeasurementSet Source Table columns.
// </etymology>
//
// <synopsis>
// This class provides read-only access to the columns in the NewMSSource Table.
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

class RONewMSSourceColumns
{
public:
  // Construct from the supplied Table
  RONewMSSourceColumns(const NewMSSource& msSource);

  // The destructor does nothing special
  ~RONewMSSourceColumns();

  // Is this object defined? (NewMSSource table is optional)
  Bool isNull() const {return isNull_p;}

  // Access to required columns
  // <group>
  const ROScalarColumn<Int>& calibrationGroup() const {
    return calibrationGroup_p;}
  const ROScalarColumn<String>& code() const {return code_p;}
  const ROArrayColumn<Double>& direction() const {return direction_p;}
  const ROArrayQuantColumn<Double>& directionQuant() const {
    return directionQuant_p;}
  const ROScalarMeasColumn<MDirection>& directionMeas() const {
    return directionMeas_p;}
  const ROScalarColumn<Double>& interval() const {return interval_p;}
  const ROScalarQuantColumn<Double>& intervalQuant() const {
    return intervalQuant_p;}
  const ROScalarColumn<String>& name() const {return name_p;}
  const ROScalarColumn<Int>& numLines() const {return numLines_p;}
  const ROArrayColumn<Double>& properMotion() const {return properMotion_p;}
  const ROArrayQuantColumn<Double>& properMotionQuant() const {
    return properMotionQuant_p;}
  const ROScalarColumn<Int>& sourceId() const {return sourceId_p;}
  const ROScalarColumn<Int>& spectralWindowId() const {
    return spectralWindowId_p;}
  const ROScalarColumn<Double>& time() const {return time_p;}
  const ROScalarQuantColumn<Double>& timeQuant() const {return timeQuant_p;}
  const ROScalarMeasColumn<MEpoch>& timeMeas() const {return timeMeas_p;}
  // </group>

  // Access to optional columns
  // <group>
  const ROArrayColumn<Double>& position() const {return position_p;}
  const ROArrayQuantColumn<Double>& positionQuant() const {
    return positionQuant_p;}
  const ROScalarMeasColumn<MPosition>& positionMeas() const {
    return positionMeas_p;}
  const ROScalarColumn<Int>& pulsarId() const {return pulsarId_p;}
  const ROArrayColumn<Double>& restFrequency() const {return restFrequency_p;}
  const ROArrayQuantColumn<Double>& restFrequencyQuant() const {
    return restFrequencyQuant_p;}
  const ROArrayMeasColumn<MFrequency>& restFrequencyMeas() const {
    return restFrequencyMeas_p;}
  const ROScalarColumn<TableRecord>& sourceModel() const {
    return sourceModel_p;}
  const ROArrayColumn<Double>& sysvel() const {return sysvel_p;}
  const ROArrayQuantColumn<Double>& sysvelQuant() const {return sysvelQuant_p;}
  const ROArrayMeasColumn<MRadialVelocity>& sysvelMeas() const {
    return sysvelMeas_p;}
  const ROArrayColumn<String>& transition() const {return transition_p;}
  // </group>

  // Convenience function that returns the number of rows in any of the
  // columns. Returns zero if the object is null.
  uInt nrow() const {return isNull() ? 0 : calibrationGroup_p.nrow();}

protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  RONewMSSourceColumns();

  //# attach this object to the supplied table.
  void attach(const NewMSSource& msSource);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  RONewMSSourceColumns(const RONewMSSourceColumns&);
  RONewMSSourceColumns& operator=(const RONewMSSourceColumns&);

  //# Check if any optional columns exist and if so attach them.
  void attachOptionalCols(const NewMSSource& msSource);
  
  //# Is the object not attached to a Table.
  Bool isNull_p;

  //# required columns
  ROScalarColumn<Int> calibrationGroup_p;
  ROScalarColumn<String> code_p;
  ROArrayColumn<Double> direction_p;
  ROScalarColumn<Double> interval_p;
  ROScalarColumn<String> name_p;
  ROScalarColumn<Int> numLines_p;
  ROArrayColumn<Double> properMotion_p;
  ROScalarColumn<Int> sourceId_p;
  ROScalarColumn<Int> spectralWindowId_p;
  ROScalarColumn<Double> time_p;
  //# optional columns
  ROArrayColumn<Double> position_p;
  ROScalarColumn<Int> pulsarId_p;
  ROArrayColumn<Double> restFrequency_p;
  ROScalarColumn<TableRecord> sourceModel_p;
  ROArrayColumn<Double> sysvel_p;
  ROArrayColumn<String> transition_p;

  //# Access to Measure columns
  ROScalarMeasColumn<MDirection> directionMeas_p;
  ROScalarMeasColumn<MEpoch> timeMeas_p;
  //# Optional Measure columns
  ROScalarMeasColumn<MPosition> positionMeas_p;
  ROArrayMeasColumn<MFrequency> restFrequencyMeas_p;
  ROArrayMeasColumn<MRadialVelocity> sysvelMeas_p;

  //# Access to Quantum columns
  ROArrayQuantColumn<Double> directionQuant_p;
  ROScalarQuantColumn<Double> intervalQuant_p;
  ROArrayQuantColumn<Double> properMotionQuant_p;
  ROScalarQuantColumn<Double> timeQuant_p;
  //# Optional Quantum columns
  ROArrayQuantColumn<Double> positionQuant_p;
  ROArrayQuantColumn<Double> restFrequencyQuant_p;
  ROArrayQuantColumn<Double> sysvelQuant_p;
};

// <summary>
// A class to provide easy read-write access to NewMSSource columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> NewMSSource
//   <li> ArrayColumn
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// NewMSSourceColumns stands for NewMeasurementSet Source Table columns.
// </etymology>
//
// <synopsis>
// This class provides access to the columns in the NewMSSource Table,
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

class NewMSSourceColumns: public RONewMSSourceColumns
{
public:
  // Construct from the supplied Table
  NewMSSourceColumns(NewMSSource& msSource);

  // The destructor does nothing special
  ~NewMSSourceColumns();

  // Read-write access to required columns
  // <group>
  ScalarColumn<Int>& calibrationGroup() {return calibrationGroup_p;}
  ScalarColumn<String>& code() {return code_p;}
  ArrayColumn<Double>& direction() {return direction_p;}
  ArrayQuantColumn<Double>& directionQuant() {return directionQuant_p;}
  ScalarMeasColumn<MDirection>& directionMeas() {return directionMeas_p;}
  ScalarColumn<Double>& interval() {return interval_p;}
  ScalarQuantColumn<Double>& intervalQuant() {return intervalQuant_p;}
  ScalarColumn<String>& name() {return name_p;}
  ScalarColumn<Int>& numLines() {return numLines_p;}
  ArrayColumn<Double>& properMotion() {return properMotion_p;}
  ArrayQuantColumn<Double>& properMotionQuant() {return properMotionQuant_p;}
  ScalarColumn<Int>& sourceId() {return sourceId_p;}
  ScalarColumn<Int>& spectralWindowId() {return spectralWindowId_p;}
  ScalarColumn<Double>& time() {return time_p;}
  ScalarQuantColumn<Double>& timeQuant() {return timeQuant_p;}
  ScalarMeasColumn<MEpoch>& timeMeas() {return timeMeas_p;}
  // </group>

  // Read-write access to optional columns
  // <group>
  ArrayColumn<Double>& position() {return position_p;}
  ArrayQuantColumn<Double>& positionQuant() {return positionQuant_p;}
  ScalarMeasColumn<MPosition>& positionMeas() {return positionMeas_p;}
  ScalarColumn<Int>& pulsarId() {return pulsarId_p;}
  ArrayColumn<Double>& restFrequency() {return restFrequency_p;}
  ArrayQuantColumn<Double>& restFrequencyQuant() {return restFrequencyQuant_p;}
  ArrayMeasColumn<MFrequency>& restFrequencyMeas() {
    return restFrequencyMeas_p;}
  ScalarColumn<TableRecord>& sourceModel() {return sourceModel_p;}
  ArrayColumn<Double>& sysvel() {return sysvel_p;}
  ArrayQuantColumn<Double>& sysvelQuant() {return sysvelQuant_p;}
  ArrayMeasColumn<MRadialVelocity>& sysvelMeas() {return sysvelMeas_p;}
  ArrayColumn<String>& transition() {return transition_p;}
  // </group>

  // Read-only access to required columns
  // <group>
  const ROScalarColumn<Int>& calibrationGroup() const {
    return RONewMSSourceColumns::calibrationGroup();}
  const ROScalarColumn<String>& code() const {
    return RONewMSSourceColumns::code();}
  const ROArrayColumn<Double>& direction() const {
    return RONewMSSourceColumns::direction();}
  const ROArrayQuantColumn<Double>& directionQuant() const {
    return RONewMSSourceColumns::directionQuant();}
  const ROScalarMeasColumn<MDirection>& directionMeas() const {
    return RONewMSSourceColumns::directionMeas();}
  const ROScalarColumn<Double>& interval() const {
    return RONewMSSourceColumns::interval();}
  const ROScalarQuantColumn<Double>& intervalQuant() const {
    return RONewMSSourceColumns::intervalQuant();}
  const ROScalarColumn<String>& name() const {
    return RONewMSSourceColumns::name();}
  const ROScalarColumn<Int>& numLines() const {
    return RONewMSSourceColumns::numLines();}
  const ROArrayColumn<Double>& properMotion() const {
    return RONewMSSourceColumns::properMotion();}
  const ROArrayQuantColumn<Double>& properMotionQuant() const {
    return RONewMSSourceColumns::properMotionQuant();}
  const ROScalarColumn<Int>& sourceId() const {
    return RONewMSSourceColumns::sourceId();}
  const ROScalarColumn<Int>& spectralWindowId() const {
    return RONewMSSourceColumns::spectralWindowId();}
  const ROScalarColumn<Double>& time() const {
    return RONewMSSourceColumns::time();}
  const ROScalarQuantColumn<Double>& timeQuant() const {
    return RONewMSSourceColumns::timeQuant();}
  const ROScalarMeasColumn<MEpoch>& timeMeas() const {
    return RONewMSSourceColumns::timeMeas();}
  // </group>

  // Read-only access to optional columns
  // <group>
  const ROArrayColumn<Double>& position() const {
    return RONewMSSourceColumns::position();}
  const ROArrayQuantColumn<Double>& positionQuant() const {
    return RONewMSSourceColumns::positionQuant();}
  const ROScalarMeasColumn<MPosition>& positionMeas() const {
    return RONewMSSourceColumns::positionMeas();}
  const ROScalarColumn<Int>& pulsarId() const {
    return RONewMSSourceColumns::pulsarId();}
  const ROArrayColumn<Double>& restFrequency() const {
    return RONewMSSourceColumns::restFrequency();}
  const ROArrayQuantColumn<Double>& restFrequencyQuant() const {
    return RONewMSSourceColumns::restFrequencyQuant();}
  const ROArrayMeasColumn<MFrequency>& restFrequencyMeas() const {
    return RONewMSSourceColumns::restFrequencyMeas();}
  const ROScalarColumn<TableRecord>& sourceModel() const {
    return RONewMSSourceColumns::sourceModel();}
  const ROArrayColumn<Double>& sysvel() const {
    return RONewMSSourceColumns::sysvel();}
  const ROArrayQuantColumn<Double>& sysvelQuant() const {
    return RONewMSSourceColumns::sysvelQuant();}
  const ROArrayMeasColumn<MRadialVelocity>& sysvelMeas() const {
    return RONewMSSourceColumns::sysvelMeas();}
  const ROArrayColumn<String>& transition() const {
    return RONewMSSourceColumns::transition();}
  // </group>

  // set the epoch type for the TIME column. This can only be done when the
  // table has no rows. Trying to do so at other times will throw an exception.
  void setEpochRef(MEpoch::Types ref);

  // set the direction type for the DIRECTION column. This can only be done
  // when the table has no rows. Trying to do so at other times will throw an
  // exception.
  void setDirectionRef(MDirection::Types ref);

  // set the position type for the POSITION column. This can only be done when
  // the table has no rows. Trying to do so at other times will throw an
  // exception.
  void setPositionRef(MPosition::Types ref);

  // set the frequency type for the REST_FREQUENCY column. Does nothing if this
  // column is not defined. This can only be done when the table has no
  // rows. Trying to do so at other times will throw an exception.
  void setFrequencyRef(MFrequency::Types ref);

  // set the radial velocity type for the SYSVEL column. Does nothing if this
  // column is not defined. This can only be done when the table has no
  // rows. Trying to do so at other times will throw an exception.
  void setRadialVelocityRef(MRadialVelocity::Types ref);

protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  NewMSSourceColumns();

  //# attach this object to the supplied table.
  void attach(NewMSSource& msSource);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  NewMSSourceColumns(const NewMSSourceColumns&);
  NewMSSourceColumns& operator=(const NewMSSourceColumns&);

  //# Check if any optional columns exist and if so attach them.
  void attachOptionalCols(NewMSSource& msSource);
  
  //# required columns
  ScalarColumn<Int> calibrationGroup_p;
  ScalarColumn<String> code_p;
  ArrayColumn<Double> direction_p;
  ScalarColumn<Double> interval_p;
  ScalarColumn<String> name_p;
  ScalarColumn<Int> numLines_p;
  ArrayColumn<Double> properMotion_p;
  ScalarColumn<Int> sourceId_p;
  ScalarColumn<Int> spectralWindowId_p;
  ScalarColumn<Double> time_p;
  //# optional columns
  ArrayColumn<Double> position_p;
  ScalarColumn<Int> pulsarId_p;
  ArrayColumn<Double> restFrequency_p;
  ScalarColumn<TableRecord> sourceModel_p;
  ArrayColumn<Double> sysvel_p;
  ArrayColumn<String> transition_p;

  //# Access to Measure columns
  ScalarMeasColumn<MDirection> directionMeas_p;
  ScalarMeasColumn<MEpoch> timeMeas_p;
  //# Optional Measure columns
  ScalarMeasColumn<MPosition> positionMeas_p;
  ArrayMeasColumn<MFrequency> restFrequencyMeas_p;
  ArrayMeasColumn<MRadialVelocity> sysvelMeas_p;

  //# Access to Quantum columns
  ArrayQuantColumn<Double> directionQuant_p;
  ScalarQuantColumn<Double> intervalQuant_p;
  ArrayQuantColumn<Double> properMotionQuant_p;
  ScalarQuantColumn<Double> timeQuant_p;
  //# Optional Quantum columns
  ArrayQuantColumn<Double> positionQuant_p;
  ArrayQuantColumn<Double> restFrequencyQuant_p;
  ArrayQuantColumn<Double> sysvelQuant_p;
};
#endif
