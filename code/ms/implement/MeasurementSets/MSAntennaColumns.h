//# NewMSAntennaColumns.h: provides easy access to NewMSAntenna columns
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

#if !defined(AIPS_NEWMSANTENNACOLUMNS_H)
#define AIPS_NEWMSANTENNACOLUMNS_H

#include <aips/aips.h>
#include <aips/Measures/MPosition.h>
#include <aips/TableMeasures/ArrayQuantColumn.h>
#include <aips/TableMeasures/ScalarMeasColumn.h>
#include <aips/TableMeasures/ScalarQuantColumn.h>
#include <aips/Tables/ArrayColumn.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Utilities/String.h>

class NewMSAntenna;

// <summary>
// A class to provide easy read-only access to NewMSAntenna columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> NewMSAntenna
//   <li> ArrayColumn
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// RONewMSAntennaColumns stands for Read-Only NewMeasurementSet Antenna Table columns.
// </etymology>
//
// <synopsis>
// This class provides read-only access to the columns in the NewMSAntenna
// Table.  It does the declaration of all the Scalar and ArrayColumns with the
// correct types, so the application programmer doesn't have to worry about
// getting those right. There is an access function for every predefined
// column. Access to non-predefined columns will still have to be done with
// explicit declarations.
// See <linkto class=RONewMSColumns> RONewMSColumns</linkto> for an example.
// </synopsis>
//
// <motivation>
// See <linkto class=NewMSColumns> NewMSColumns</linkto> for the motivation.
// </motivation>

class RONewMSAntennaColumns
{
public:

  // Create a columns object that accesses the data in the specified Table
  RONewMSAntennaColumns(const NewMSAntenna& msAntenna);

  // The destructor does nothing special
  ~RONewMSAntennaColumns();

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
  // to within the specified tolerance. The tolerance is the maximum allowed
  // distance between the two positions and the supplied Quantum must have
  // dimensions of length. This is checked when compiled in debug mode and an
  // AIpsError exception is thrown if the dimensions are wrong. Returns -1 if
  // no match could be found.
  Int matchPosition(const MPosition& antennaPosition,
		    const Quantum<Double>& tolerance);
protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  RONewMSAntennaColumns();

  //# attach this object to the supplied table.
  void attach(const NewMSAntenna& msAntenna);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  RONewMSAntennaColumns(const RONewMSAntennaColumns&);
  RONewMSAntennaColumns& operator=(const RONewMSAntennaColumns&);

  //# Check if any optional columns exist and if so attach them.
  void attachOptionalCols(const NewMSAntenna& msAntenna);
  
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
// A class to provide easy read-write access to NewMSAntenna columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> NewMSAntenna
//   <li> ArrayColumn
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// NewMSAntennaColumns stands for NewMeasurementSet Antenna Table columns.
// </etymology>
//
// <synopsis>
// This class provides access to the columns in the NewMSAntenna Table,
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

class NewMSAntennaColumns: public RONewMSAntennaColumns
{
public:

  // Create a columns object that accesses the data in the specified Table
  NewMSAntennaColumns(NewMSAntenna& msAntenna);

  // The destructor does nothing special
  ~NewMSAntennaColumns();

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
    return RONewMSAntennaColumns::dishDiameter();}
  const ROScalarQuantColumn<Double>& dishDiameterQuant() const {
    return RONewMSAntennaColumns::dishDiameterQuant();}
  const ROScalarColumn<Bool>& flagRow() const {
    return RONewMSAntennaColumns::flagRow();}
  const ROScalarColumn<String>& mount() const {
    return RONewMSAntennaColumns::mount();}
  const ROScalarColumn<String>& name() const {
    return RONewMSAntennaColumns::name();}
  const ROArrayColumn<Double>& offset() const {
    return RONewMSAntennaColumns::offset();}
  const ROArrayQuantColumn<Double>& offsetQuant() const {
    return RONewMSAntennaColumns::offsetQuant();}
  const ROScalarMeasColumn<MPosition>& offsetMeas() const {
    return RONewMSAntennaColumns::offsetMeas();}
  const ROArrayColumn<Double>& position() const {
    return RONewMSAntennaColumns::position();}
  const ROArrayQuantColumn<Double>& positionQuant() const {
    return RONewMSAntennaColumns::positionQuant();}
  const ROScalarMeasColumn<MPosition>& positionMeas() const {
    return RONewMSAntennaColumns::positionMeas();}
  const ROScalarColumn<String>& station() const {
    return RONewMSAntennaColumns::station();}
  const ROScalarColumn<String>& type() const {
    return RONewMSAntennaColumns::type();}
  // </group>

  // Read-only access to optional columns
  // <group>
  const ROArrayColumn<Double>& meanOrbit() const {
    return RONewMSAntennaColumns::meanOrbit();}
  const ROScalarColumn<Int>& orbitId() const {
    return RONewMSAntennaColumns::orbitId();}
  const ROScalarColumn<Int>& phasedArrayId() const {
    return RONewMSAntennaColumns::phasedArrayId();}
  // </group>

  // Set the POSITION reference for the position column.
  void setPositionRef(MPosition::Types ref);

  // set the POSITION reference for the offset column
  void setOffsetRef(MPosition::Types ref);

protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  NewMSAntennaColumns();

  //# attach this object to the supplied table.
  void attach(NewMSAntenna& msAntenna);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  NewMSAntennaColumns(const NewMSAntennaColumns&);
  NewMSAntennaColumns& operator=(const NewMSAntennaColumns&);

  //# Check if any optional columns exist and if so attach them.
  void attachOptionalCols(NewMSAntenna& msAntenna);
  
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
#endif
