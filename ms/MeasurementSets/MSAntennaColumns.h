//# MSAntennaColumns.h: provides easy access to MSAntenna columns
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

#ifndef MS_MSANTENNACOLUMNS_H
#define MS_MSANTENNACOLUMNS_H

#include <casacore/casa/aips.h>
#include <casacore/measures/Measures/MPosition.h>
#include <casacore/measures/Measures/MCPosition.h>
#include <casacore/measures/TableMeasures/ArrayQuantColumn.h>
#include <casacore/measures/TableMeasures/ScalarMeasColumn.h>
#include <casacore/measures/TableMeasures/ScalarQuantColumn.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class MSAntenna;

// <summary>
// A class to provide easy read-only access to MSAntenna columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> MSAntenna
//   <li> ArrayColumn
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// ROMSAntennaColumns stands for Read-Only MeasurementSet Antenna Table columns.
// </etymology>
//
// <synopsis>
// This class provides read-only access to the columns in the MSAntenna
// Table.  It does the declaration of all the Scalar and ArrayColumns with the
// correct types, so the application programmer doesn't have to worry about
// getting those right. There is an access function for every predefined
// column. Access to non-predefined columns will still have to be done with
// explicit declarations.
// See <linkto class=ROMSColumns> ROMSColumns</linkto> for an example.
// </synopsis>
//
// <motivation>
// See <linkto class=MSColumns> MSColumns</linkto> for the motivation.
// </motivation>

class ROMSAntennaColumns
{
public:

  // Create a columns object that accesses the data in the specified Table
  ROMSAntennaColumns(const MSAntenna& msAntenna);

  // The destructor does nothing special
  ~ROMSAntennaColumns();

  // Access to columns
  // <group>
  const ROScalarColumn<Double>& dishDiameter() const {return dishDiameter_p;}
  const ROScalarQuantColumn<Double>& dishDiameterQuant() const 
    {return dishDiameterQuant_p;}
  const ROScalarColumn<Bool>& flagRow() const {return flagRow_p;}
  const ROScalarColumn<String>& mount() const {return mount_p;}
  const ROScalarColumn<String>& name() const {return name_p;}
  const ROArrayColumn<Double>& offset() const {return offset_p;}
  const ROArrayQuantColumn<Double>& offsetQuant() const {return offsetQuant_p;}
  const ROScalarMeasColumn<MPosition>& offsetMeas() const {
    return offsetMeas_p;}
  const ROArrayColumn<Double>& position() const {return position_p;}
  const ROArrayQuantColumn<Double>& positionQuant() const {
    return positionQuant_p;}
  const ROScalarMeasColumn<MPosition>& positionMeas() const 
    { return positionMeas_p;}
  const ROScalarColumn<String>& station() const {return station_p;}
  const ROScalarColumn<String>& type() const {return type_p;}
  // </group>

  // Access to optional columns
  // <group>
  const ROArrayColumn<Double>& meanOrbit() const {return meanOrbit_p;}
  const ROScalarColumn<Int>& orbitId() const {return orbitId_p;}
  const ROScalarColumn<Int>& phasedArrayId() const {return phasedArrayId_p;}
  // </group>

  // Convenience function that returns the number of rows in any of the columns
  uInt nrow() const {return dishDiameter_p.nrow();}

  // returns the last row that contains an antenna at the specified position,
  // to within the specified tolerance. The reference frame of the supplied
  // position must be the same as the one for the POSITION columns. If not an
  // AipsError is thrown as such an argument will never match any row of the
  // Table. The tolerance is the maximum allowed distance between the two
  // positions and the supplied Quantum must have dimensions of length. This is
  // checked when compiled in debug mode and an AipsError exception is thrown
  // if the dimensions are wrong. Returns -1 if no match could be found. Flaged
  // rows can never match. If tryRow is non-negative, then that row is tested
  // to see if it matches before any others are tested. Setting tryRow to a
  // positive value greater than the table length will throw an exception
  // (AipsError), when compiled in debug mode.
  Int matchAntenna(const MPosition& antennaPos,
		   const Quantum<Double>& tolerance, Int tryRow=-1);

  // Same as the previous function except that the antenna name must also
  // match.
  Int matchAntenna(const String& antName, const MPosition& antennaPos,
		   const Quantum<Double>& tolerance, Int tryRow=-1);

  // Same as the previous function except that the station name must also
  // match.
  Int matchAntennaAndStation(const String& antName, 
			     const String& stationName, // ignored when empty 
			     const MPosition& antennaPos,
			     const Quantum<Double>& tolerance, Int tryRow=-1);
protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  ROMSAntennaColumns();

  //# attach this object to the supplied table.
  void attach(const MSAntenna& msAntenna);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  ROMSAntennaColumns(const ROMSAntennaColumns&);
  ROMSAntennaColumns& operator=(const ROMSAntennaColumns&);

  //# Check if any optional columns exist and if so attach them.
  void attachOptionalCols(const MSAntenna& msAntenna);
  
  //# Functions which check the supplied values against the relevant column and
  //# the specified row.
  Bool matchName(uInt row, const String& antName) const;
  Bool matchStation(uInt row, const String& stationName) const;
  Bool matchPosition(uInt row, const Vector<Double>& antPosInM,	
		     const Double tolInM) const;


  //# required columns
  ROScalarColumn<Double> dishDiameter_p;
  ROScalarColumn<Bool> flagRow_p;
  ROScalarColumn<String> mount_p;
  ROScalarColumn<String> name_p;
  ROArrayColumn<Double> offset_p;
  ROArrayColumn<Double> position_p;
  ROScalarColumn<String> station_p;
  ROScalarColumn<String> type_p;
  //# optional columns
  ROArrayColumn<Double> meanOrbit_p;
  ROScalarColumn<Int> orbitId_p;
  ROScalarColumn<Int> phasedArrayId_p;

  //# Access to Measure columns
  ROScalarMeasColumn<MPosition> offsetMeas_p;
  ROScalarMeasColumn<MPosition> positionMeas_p;

  //# Access to Quantum columns
  ROScalarQuantColumn<Double> dishDiameterQuant_p;
  ROArrayQuantColumn<Double> offsetQuant_p;
  ROArrayQuantColumn<Double> positionQuant_p;
};

// <summary>
// A class to provide easy read-write access to MSAntenna columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> MSAntenna
//   <li> ArrayColumn
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// MSAntennaColumns stands for MeasurementSet Antenna Table columns.
// </etymology>
//
// <synopsis>
// This class provides access to the columns in the MSAntenna Table,
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

class MSAntennaColumns: public ROMSAntennaColumns
{
public:

  // Create a columns object that accesses the data in the specified Table
  MSAntennaColumns(MSAntenna& msAntenna);

  // The destructor does nothing special
  ~MSAntennaColumns();

  // Read-write access to required columns
  // <group>
  ScalarColumn<Double>& dishDiameter() {return dishDiameter_p;}
  ScalarQuantColumn<Double>& dishDiameterQuant() {return dishDiameterQuant_p;}
  ScalarColumn<Bool>& flagRow() {return flagRow_p;}
  ScalarColumn<String>& mount() {return mount_p;}
  ScalarColumn<String>& name() {return name_p;}
  ArrayColumn<Double>& offset() {return offset_p;}
  ArrayQuantColumn<Double>& offsetQuant() {return offsetQuant_p;}
  ScalarMeasColumn<MPosition>& offsetMeas() { return offsetMeas_p;}
  ArrayColumn<Double>& position() {return position_p;}
  ArrayQuantColumn<Double>& positionQuant() {return positionQuant_p;}
  ScalarMeasColumn<MPosition>& positionMeas() { return positionMeas_p;}
  ScalarColumn<String>& station() {return station_p;}
  ScalarColumn<String>& type() {return type_p;}
  // </group>

  // Read-write access to optional columns
  // <group>
  ArrayColumn<Double>& meanOrbit() {return meanOrbit_p;}
  ScalarColumn<Int>& orbitId() {return orbitId_p;}
  ScalarColumn<Int>& phasedArrayId() {return phasedArrayId_p;}
  // </group>

  // Read-only access to required columns
  // <group>
  const ROScalarColumn<Double>& dishDiameter() const {
    return ROMSAntennaColumns::dishDiameter();}
  const ROScalarQuantColumn<Double>& dishDiameterQuant() const {
    return ROMSAntennaColumns::dishDiameterQuant();}
  const ROScalarColumn<Bool>& flagRow() const {
    return ROMSAntennaColumns::flagRow();}
  const ROScalarColumn<String>& mount() const {
    return ROMSAntennaColumns::mount();}
  const ROScalarColumn<String>& name() const {
    return ROMSAntennaColumns::name();}
  const ROArrayColumn<Double>& offset() const {
    return ROMSAntennaColumns::offset();}
  const ROArrayQuantColumn<Double>& offsetQuant() const {
    return ROMSAntennaColumns::offsetQuant();}
  const ROScalarMeasColumn<MPosition>& offsetMeas() const {
    return ROMSAntennaColumns::offsetMeas();}
  const ROArrayColumn<Double>& position() const {
    return ROMSAntennaColumns::position();}
  const ROArrayQuantColumn<Double>& positionQuant() const {
    return ROMSAntennaColumns::positionQuant();}
  const ROScalarMeasColumn<MPosition>& positionMeas() const {
    return ROMSAntennaColumns::positionMeas();}
  const ROScalarColumn<String>& station() const {
    return ROMSAntennaColumns::station();}
  const ROScalarColumn<String>& type() const {
    return ROMSAntennaColumns::type();}
  // </group>

  // Read-only access to optional columns
  // <group>
  const ROArrayColumn<Double>& meanOrbit() const {
    return ROMSAntennaColumns::meanOrbit();}
  const ROScalarColumn<Int>& orbitId() const {
    return ROMSAntennaColumns::orbitId();}
  const ROScalarColumn<Int>& phasedArrayId() const {
    return ROMSAntennaColumns::phasedArrayId();}
  // </group>

  // set the position type for the POSITION column. This can only be done when
  // the table has no rows. Trying to do so at other times will throw an
  // exception.
  void setPositionRef(MPosition::Types ref);

  // set the position type for the OFFSET column. This can only be done when
  // the table has no rows. Trying to do so at other times will throw an
  // exception.
  void setOffsetRef(MPosition::Types ref);

protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  MSAntennaColumns();

  //# attach this object to the supplied table.
  void attach(MSAntenna& msAntenna);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  MSAntennaColumns(const MSAntennaColumns&);
  MSAntennaColumns& operator=(const MSAntennaColumns&);

  //# Check if any optional columns exist and if so attach them.
  void attachOptionalCols(MSAntenna& msAntenna);
  
  //# required columns
  ScalarColumn<Double> dishDiameter_p;
  ScalarColumn<Bool> flagRow_p;
  ScalarColumn<String> mount_p;
  ScalarColumn<String> name_p;
  ArrayColumn<Double> offset_p;
  ArrayColumn<Double> position_p;
  ScalarColumn<String> station_p;
  ScalarColumn<String> type_p;
  //# optional columns
  ArrayColumn<Double> meanOrbit_p;
  ScalarColumn<Int> orbitId_p;
  ScalarColumn<Int> phasedArrayId_p;

  //# Access to Measure columns
  ScalarMeasColumn<MPosition> offsetMeas_p;
  ScalarMeasColumn<MPosition> positionMeas_p;

  //# Access to Quantum columns
  ScalarQuantColumn<Double> dishDiameterQuant_p;
  ArrayQuantColumn<Double> offsetQuant_p;
  ArrayQuantColumn<Double> positionQuant_p;

};

} //# NAMESPACE CASACORE - END

#endif
