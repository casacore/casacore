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

#if !defined(AIPS_NewMSANTENNACOLUMNS_H)
#define AIPS_NewMSANTENNACOLUMNS_H

#include <aips/MeasurementSets/NewMSAntenna.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Tables/ArrayColumn.h>
#include <aips/Quanta/Unit.h>
#include <aips/Quanta/Quantum.h>
#include <aips/TableMeasures/ScalarMeasColumn.h>
#include <aips/TableMeasures/ScalarQuantColumn.h>
#include <aips/TableMeasures/ArrayQuantColumn.h>

class MPosition;

// <summary>
// A convenience class to provide easy access to NewMSAntenna columns
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

class NewMSAntennaColumns
{
public:

  NewMSAntennaColumns(NewMSAntenna& msAntenna);

  ~NewMSAntennaColumns();

  // Access to columns
  ScalarColumn<Double>& dishDiameter() {return dishDiameter_p;}
  ScalarColumn<Bool>& flagRow() {return flagRow_p;}
  ScalarColumn<String>& mount() {return mount_p;}
  ScalarColumn<String>& name() {return name_p;}
  ArrayColumn<Double>& offset() {return offset_p;}
  ArrayColumn<Double>& position() {return position_p;}
  ScalarColumn<String>& station() {return station_p;}
  ScalarColumn<String>& type() {return type_p;}
  ArrayColumn<Double>& meanOrbit() {return meanOrbit_p;}
  ScalarColumn<Int>& orbitId() {return orbitId_p;}
  ScalarColumn<Int>& phasedArrayId() {return phasedArrayId_p;}

  // Access to Measure columns
  ScalarMeasColumn<MPosition>& offsetMeas() { return offsetMeas_p;}
  ScalarMeasColumn<MPosition>& positionMeas() { return positionMeas_p;}

  // Access to Quantum columns
  ScalarQuantColumn<Double>& dishDiameterQuant() {return dishDiameterQuant_p;}
  ArrayQuantColumn<Double>& offsetQuant() {return offsetQuant_p;}
  ArrayQuantColumn<Double>& positionQuant() {return positionQuant_p;}

  // Set the POSITION reference for the position column.
  // Give the reference code as e.g., MPosition::ITRF.
  void setPositionRef(Int ref);

  // set the POSITION reference for the offset column
  void setOffsetRef(Int ref);


private:

  ScalarColumn<Double> dishDiameter_p;
  ScalarColumn<Bool> flagRow_p;
  ScalarColumn<String> mount_p;
  ScalarColumn<String> name_p;
  ArrayColumn<Double> offset_p;
  ArrayColumn<Double> position_p;
  ScalarColumn<String> station_p;
  ScalarColumn<String> type_p;
  ArrayColumn<Double> meanOrbit_p;
  ScalarColumn<Int> orbitId_p;
  ScalarColumn<Int> phasedArrayId_p;

  // Access to Measure columns
  ScalarMeasColumn<MPosition> offsetMeas_p;
  ScalarMeasColumn<MPosition> positionMeas_p;

  // Access to Quantum columns
  ScalarQuantColumn<Double> dishDiameterQuant_p;
  ArrayQuantColumn<Double> offsetQuant_p;
  ArrayQuantColumn<Double> positionQuant_p;

};

// <summary>
// A convenience class to provide easy access to NewMSAntenna columns
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
// This class provides read-only access to the columns in the NewMSAntenna Table.
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

class RONewMSAntennaColumns
{
public:

  RONewMSAntennaColumns(const NewMSAntenna& msAntenna);

  ~RONewMSAntennaColumns();

  // Access to columns
  const ROScalarColumn<Double>& dishDiameter() const {return dishDiameter_p;}
  const ROScalarColumn<Bool>& flagRow() const {return flagRow_p;}
  const ROScalarColumn<String>& mount() const {return mount_p;}
  const ROScalarColumn<String>& name() const {return name_p;}
  const ROArrayColumn<Double>& offset() const {return offset_p;}
  const ROArrayColumn<Double>& position() const {return position_p;}
  const ROScalarColumn<String>& station() const {return station_p;}
  const ROScalarColumn<String>& type() const {return type_p;}
  const ROArrayColumn<Double>& meanOrbit() const {return meanOrbit_p;}
  const ROScalarColumn<Int>& orbitId() const {return orbitId_p;}
  const ROScalarColumn<Int>& phasedArrayId() const {return phasedArrayId_p;}

  // Access to Measure columns
  const ROScalarMeasColumn<MPosition>& offsetMeas() const 
    { return offsetMeas_p;}
  const ROScalarMeasColumn<MPosition>& positionMeas() const 
    { return positionMeas_p;}

  // Access to Quantum columns
  const ROScalarQuantColumn<Double>& dishDiameterQuant() const 
    {return dishDiameterQuant_p;}
  const ROArrayQuantColumn<Double>& offsetQuant() const {return offsetQuant_p;}
  const ROArrayQuantColumn<Double>& positionQuant() const {return positionQuant_p;}

private:

  ROScalarColumn<Double> dishDiameter_p;
  ROScalarColumn<Bool> flagRow_p;
  ROScalarColumn<String> mount_p;
  ROScalarColumn<String> name_p;
  ROArrayColumn<Double> offset_p;
  ROArrayColumn<Double> position_p;
  ROScalarColumn<String> station_p;
  ROScalarColumn<String> type_p;
  ROArrayColumn<Double> meanOrbit_p;
  ROScalarColumn<Int> orbitId_p;
  ROScalarColumn<Int> phasedArrayId_p;

  // Access to Measure columns
  ROScalarMeasColumn<MPosition> offsetMeas_p;
  ROScalarMeasColumn<MPosition> positionMeas_p;

  // Access to Quantum columns
  ROScalarQuantColumn<Double> dishDiameterQuant_p;
  ROArrayQuantColumn<Double> offsetQuant_p;
  ROArrayQuantColumn<Double> positionQuant_p;
};

#endif

