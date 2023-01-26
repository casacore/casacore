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
// A class to provide easy access to MSAntenna columns
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

class MSAntennaColumns
{
public:

  // Create a columns object that accesses the data in the specified Table
  MSAntennaColumns(const MSAntenna& msAntenna);

  // The destructor does nothing special
  ~MSAntennaColumns();

  // Access to required columns
  // <group>
  ScalarColumn<double>& dishDiameter() {return dishDiameter_p;}
  ScalarQuantColumn<double>& dishDiameterQuant() {return dishDiameterQuant_p;}
  ScalarColumn<bool>& flagRow() {return flagRow_p;}
  ScalarColumn<String>& mount() {return mount_p;}
  ScalarColumn<String>& name() {return name_p;}
  ArrayColumn<double>& offset() {return offset_p;}
  ArrayQuantColumn<double>& offsetQuant() {return offsetQuant_p;}
  ScalarMeasColumn<MPosition>& offsetMeas() { return offsetMeas_p;}
  ArrayColumn<double>& position() {return position_p;}
  ArrayQuantColumn<double>& positionQuant() {return positionQuant_p;}
  ScalarMeasColumn<MPosition>& positionMeas() { return positionMeas_p;}
  ScalarColumn<String>& station() {return station_p;}
  ScalarColumn<String>& type() {return type_p;}
  // </group>

  // Const access to required columns
  // <group>
  const ScalarColumn<double>& dishDiameter() const {return dishDiameter_p;}
  const ScalarQuantColumn<double>& dishDiameterQuant() const {return dishDiameterQuant_p;}
  const ScalarColumn<bool>& flagRow() const {return flagRow_p;}
  const ScalarColumn<String>& mount() const {return mount_p;}
  const ScalarColumn<String>& name() const {return name_p;}
  const ArrayColumn<double>& offset() const {return offset_p;}
  const ArrayQuantColumn<double>& offsetQuant() const {return offsetQuant_p;}
  const ScalarMeasColumn<MPosition>& offsetMeas() const { return offsetMeas_p;}
  const ArrayColumn<double>& position() const {return position_p;}
  const ArrayQuantColumn<double>& positionQuant() const {return positionQuant_p;}
  const ScalarMeasColumn<MPosition>& positionMeas() const { return positionMeas_p;}
  const ScalarColumn<String>& station() const {return station_p;}
  const ScalarColumn<String>& type() const {return type_p;}
  // </group>

  // Access to optional columns
  // <group>
  ArrayColumn<double>& meanOrbit() {return meanOrbit_p;}
  ScalarColumn<int32_t>& orbitId() {return orbitId_p;}
  ScalarColumn<int32_t>& phasedArrayId() {return phasedArrayId_p;}
  // </group>
 
  // Const access to optional columns
  // <group>
  const ArrayColumn<double>& meanOrbit() const {return meanOrbit_p;}
  const ScalarColumn<int32_t>& orbitId() const {return orbitId_p;}
  const ScalarColumn<int32_t>& phasedArrayId() const {return phasedArrayId_p;}
  // </group>
 
  // set the position type for the POSITION column. This can only be done when
  // the table has no rows. Trying to do so at other times will throw an
  // exception.
  void setPositionRef(MPosition::Types ref);

  // set the position type for the OFFSET column. This can only be done when
  // the table has no rows. Trying to do so at other times will throw an
  // exception.
  void setOffsetRef(MPosition::Types ref);

  // Convenience function that returns the number of rows in any of the columns
  rownr_t nrow() const {return dishDiameter_p.nrow();}

  // returns the last row that contains an antenna at the specified position,
  // to within the specified tolerance. The reference frame of the supplied
  // position must be the same as the one for the POSITION columns. If not an
  // AipsError is thrown as such an argument will never match any row of the
  // Table. The tolerance is the maximum allowed distance between the two
  // positions and the supplied Quantum must have dimensions of length. This is
  // checked when compiled in debug mode and an AipsError exception is thrown
  // if the dimensions are wrong. Returns -1 if no match could be found. Flagged
  // rows can never match. If tryRow is non-negative, then that row is tested
  // to see if it matches before any others are tested. Setting tryRow to a
  // positive value greater than the table length will throw an exception
  // (AipsError), when compiled in debug mode.
  int64_t matchAntenna(const MPosition& antennaPos,
                     const Quantum<double>& tolerance, int64_t tryRow=-1);

  // Same as the previous function except that the antenna name must also
  // match.
  int64_t matchAntenna(const String& antName, const MPosition& antennaPos,
                     const Quantum<double>& tolerance, int64_t tryRow=-1);

  // Same as the previous function except that the station name must also
  // match.
  int64_t matchAntennaAndStation(const String& antName, 
                               const String& stationName, // ignored when empty 
                               const MPosition& antennaPos,
                               const Quantum<double>& tolerance, int64_t tryRow=-1);

protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  MSAntennaColumns();

  //# attach this object to the supplied table.
  void attach(const MSAntenna& msAntenna);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  MSAntennaColumns(const MSAntennaColumns&);
  MSAntennaColumns& operator=(const MSAntennaColumns&);

  //# Check if any optional columns exist and if so attach them.
  void attachOptionalCols(const MSAntenna& msAntenna);
  
  //# Functions which check the supplied values against the relevant column and
  //# the specified row.
  bool matchName(rownr_t row, const String& antName) const;
  bool matchStation(rownr_t row, const String& stationName) const;
  bool matchPosition(rownr_t row, const Vector<double>& antPosInM,	
		     const double tolInM) const;

  //# required columns
  ScalarColumn<double> dishDiameter_p;
  ScalarColumn<bool> flagRow_p;
  ScalarColumn<String> mount_p;
  ScalarColumn<String> name_p;
  ArrayColumn<double> offset_p;
  ArrayColumn<double> position_p;
  ScalarColumn<String> station_p;
  ScalarColumn<String> type_p;
  //# optional columns
  ArrayColumn<double> meanOrbit_p;
  ScalarColumn<int32_t> orbitId_p;
  ScalarColumn<int32_t> phasedArrayId_p;

  //# Access to Measure columns
  ScalarMeasColumn<MPosition> offsetMeas_p;
  ScalarMeasColumn<MPosition> positionMeas_p;

  //# Access to Quantum columns
  ScalarQuantColumn<double> dishDiameterQuant_p;
  ArrayQuantColumn<double> offsetQuant_p;
  ArrayQuantColumn<double> positionQuant_p;

};

//# Define the RO version for backward compatibility.
typedef MSAntennaColumns ROMSAntennaColumns;

} //# NAMESPACE CASACORE - END

#endif
