//# MSSourceColumns.h: provides easy access to MSSource columns
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

#ifndef MS_MSSOURCECOLUMNS_H
#define MS_MSSOURCECOLUMNS_H

#include <casacore/casa/aips.h>
#include <casacore/measures/Measures/MDirection.h>
#include <casacore/measures/Measures/MEpoch.h>
#include <casacore/measures/Measures/MFrequency.h>
#include <casacore/measures/Measures/MPosition.h>
#include <casacore/measures/Measures/MRadialVelocity.h>
#include <casacore/measures/Measures/MCDirection.h>
#include <casacore/measures/Measures/MCEpoch.h>
#include <casacore/measures/Measures/MCFrequency.h>
#include <casacore/measures/Measures/MCPosition.h>
#include <casacore/measures/Measures/MCRadialVelocity.h>
#include <casacore/measures/TableMeasures/ArrayMeasColumn.h>
#include <casacore/measures/TableMeasures/ArrayQuantColumn.h>
#include <casacore/measures/TableMeasures/ScalarMeasColumn.h>
#include <casacore/measures/TableMeasures/ScalarQuantColumn.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class MSSource;

// <summary>
// A class to provide easy read-only access to MSSource columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> MSSource
//   <li> ArrayColumn
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// ROMSSourceColumns stands for Read-Only MeasurementSet Source Table columns.
// </etymology>
//
// <synopsis>
// This class provides read-only access to the columns in the MSSource Table.
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

class ROMSSourceColumns
{
public:
  // Construct from the supplied Table
  ROMSSourceColumns(const MSSource& msSource);

  // The destructor does nothing special
  ~ROMSSourceColumns();

  // Is this object defined? (MSSource table is optional)
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
  ROMSSourceColumns();

  //# attach this object to the supplied table.
  void attach(const MSSource& msSource);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  ROMSSourceColumns(const ROMSSourceColumns&);
  ROMSSourceColumns& operator=(const ROMSSourceColumns&);

  //# Check if any optional columns exist and if so attach them.
  void attachOptionalCols(const MSSource& msSource);
  
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
// A class to provide easy read-write access to MSSource columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> MSSource
//   <li> ArrayColumn
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// MSSourceColumns stands for MeasurementSet Source Table columns.
// </etymology>
//
// <synopsis>
// This class provides access to the columns in the MSSource Table,
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

class MSSourceColumns: public ROMSSourceColumns
{
public:
  // Construct from the supplied Table
  MSSourceColumns(MSSource& msSource);

  // The destructor does nothing special
  ~MSSourceColumns();

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
    return ROMSSourceColumns::calibrationGroup();}
  const ROScalarColumn<String>& code() const {
    return ROMSSourceColumns::code();}
  const ROArrayColumn<Double>& direction() const {
    return ROMSSourceColumns::direction();}
  const ROArrayQuantColumn<Double>& directionQuant() const {
    return ROMSSourceColumns::directionQuant();}
  const ROScalarMeasColumn<MDirection>& directionMeas() const {
    return ROMSSourceColumns::directionMeas();}
  const ROScalarColumn<Double>& interval() const {
    return ROMSSourceColumns::interval();}
  const ROScalarQuantColumn<Double>& intervalQuant() const {
    return ROMSSourceColumns::intervalQuant();}
  const ROScalarColumn<String>& name() const {
    return ROMSSourceColumns::name();}
  const ROScalarColumn<Int>& numLines() const {
    return ROMSSourceColumns::numLines();}
  const ROArrayColumn<Double>& properMotion() const {
    return ROMSSourceColumns::properMotion();}
  const ROArrayQuantColumn<Double>& properMotionQuant() const {
    return ROMSSourceColumns::properMotionQuant();}
  const ROScalarColumn<Int>& sourceId() const {
    return ROMSSourceColumns::sourceId();}
  const ROScalarColumn<Int>& spectralWindowId() const {
    return ROMSSourceColumns::spectralWindowId();}
  const ROScalarColumn<Double>& time() const {
    return ROMSSourceColumns::time();}
  const ROScalarQuantColumn<Double>& timeQuant() const {
    return ROMSSourceColumns::timeQuant();}
  const ROScalarMeasColumn<MEpoch>& timeMeas() const {
    return ROMSSourceColumns::timeMeas();}
  // </group>

  // Read-only access to optional columns
  // <group>
  const ROArrayColumn<Double>& position() const {
    return ROMSSourceColumns::position();}
  const ROArrayQuantColumn<Double>& positionQuant() const {
    return ROMSSourceColumns::positionQuant();}
  const ROScalarMeasColumn<MPosition>& positionMeas() const {
    return ROMSSourceColumns::positionMeas();}
  const ROScalarColumn<Int>& pulsarId() const {
    return ROMSSourceColumns::pulsarId();}
  const ROArrayColumn<Double>& restFrequency() const {
    return ROMSSourceColumns::restFrequency();}
  const ROArrayQuantColumn<Double>& restFrequencyQuant() const {
    return ROMSSourceColumns::restFrequencyQuant();}
  const ROArrayMeasColumn<MFrequency>& restFrequencyMeas() const {
    return ROMSSourceColumns::restFrequencyMeas();}
  const ROScalarColumn<TableRecord>& sourceModel() const {
    return ROMSSourceColumns::sourceModel();}
  const ROArrayColumn<Double>& sysvel() const {
    return ROMSSourceColumns::sysvel();}
  const ROArrayQuantColumn<Double>& sysvelQuant() const {
    return ROMSSourceColumns::sysvelQuant();}
  const ROArrayMeasColumn<MRadialVelocity>& sysvelMeas() const {
    return ROMSSourceColumns::sysvelMeas();}
  const ROArrayColumn<String>& transition() const {
    return ROMSSourceColumns::transition();}
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
  MSSourceColumns();

  //# attach this object to the supplied table.
  void attach(MSSource& msSource);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  MSSourceColumns(const MSSourceColumns&);
  MSSourceColumns& operator=(const MSSourceColumns&);

  //# Check if any optional columns exist and if so attach them.
  void attachOptionalCols(MSSource& msSource);
  
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

} //# NAMESPACE CASACORE - END

#endif
