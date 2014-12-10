//# MSStateColumns.h: provides easy access to MSState columns
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

#ifndef MS_MSSTATECOLUMNS_H
#define MS_MSSTATECOLUMNS_H

#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/measures/TableMeasures/ScalarQuantColumn.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class MSState;

// <summary>
// A class to provide easy read-only access to MSState columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> MSState
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// ROMSStateColumns stands for Read-Only MeasurementSet State Table columns.
// </etymology>
//
// <synopsis>
// This class provides read-only access to the columns in the MSState Table.
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

class ROMSStateColumns
{
public:
  // Create a columns object that accesses the data in the specified Table
  ROMSStateColumns(const MSState& msState);

  // The destructor does nothing special
  ~ROMSStateColumns();

  // Access to required columns
  // <group>
  const ROScalarColumn<Double>& cal() const {return cal_p;}
  const ROScalarQuantColumn<Double>& calQuant() const { return calQuant_p;}
  const ROScalarColumn<Bool>& flagRow() const {return flagRow_p;}
  const ROScalarColumn<Double>& load() const {return load_p;}
  const ROScalarQuantColumn<Double>& loadQuant() const { return loadQuant_p;}
  const ROScalarColumn<String>& obsMode() const {return obsMode_p;}
  const ROScalarColumn<Bool>& ref() const {return ref_p;}
  const ROScalarColumn<Bool>& sig() const {return sig_p;}
  const ROScalarColumn<Int>& subScan() const {return subScan_p;}
  // </group>

  // Convenience function that returns the number of rows in any of the columns
  uInt nrow() const {return cal_p.nrow();}

  // Returns the last row that contains a state with the specified values.
  // For Cal and Load, the tolerance is applied in the match.
  // Returns -1 if no match could be found. Flagged rows can never match. 
  // If tryRow is non-negative, then that row is tested
  // to see if it matches before any others are tested. Setting tryRow to a
  // positive value greater than the table length will throw an exception
  // (AipsError), when compiled in debug mode.
  Int matchState(const Quantum<Double>& stateCalQ,
		 const Quantum<Double>& stateLoadQ,
		 const String& stateObsMode,
		 const Bool& stateRef,
		 const Bool& stateSig,
		 const Int& stateSubScan,
		 const Quantum<Double>& tolerance,
		 Int tryRow=-1);


protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  ROMSStateColumns();

  //# attach this object to the supplied table.
  void attach(const MSState& msState);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  ROMSStateColumns(const ROMSStateColumns&);
  ROMSStateColumns& operator=(const ROMSStateColumns&);

  //# required columns
  ROScalarColumn<Double> cal_p;
  ROScalarColumn<Bool> flagRow_p;
  ROScalarColumn<Double> load_p;
  ROScalarColumn<String> obsMode_p;
  ROScalarColumn<Bool> ref_p;
  ROScalarColumn<Bool> sig_p;
  ROScalarColumn<Int> subScan_p;

  //# Access to Quantum columns
  ROScalarQuantColumn<Double> calQuant_p;
  ROScalarQuantColumn<Double> loadQuant_p;
};

// <summary>
// A class to provide easy read-write access to MSState columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> MSState
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// MSStateColumns stands for MeasurementSet State Table columns.
// </etymology>
//
// <synopsis>
// This class provides access to the columns in the MSState Table,
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

class MSStateColumns: public ROMSStateColumns
{
public:
  // Create a columns object that accesses the data in the specified Table
  MSStateColumns(MSState& msState);

  // The destructor does nothing special
  ~MSStateColumns();

  // Read-write access to required columns
  // <group>
  ScalarColumn<Double>& cal() {return cal_p;}
  ScalarQuantColumn<Double>& calQuant() { return calQuant_p;}
  ScalarColumn<Bool>& flagRow() {return flagRow_p;}
  ScalarColumn<Double>& load() {return load_p;}
  ScalarQuantColumn<Double>& loadQuant() { return loadQuant_p;}
  ScalarColumn<String>& obsMode() {return obsMode_p;}
  ScalarColumn<Bool>& ref() {return ref_p;}
  ScalarColumn<Bool>& sig() {return sig_p;}
  ScalarColumn<Int>& subScan() {return subScan_p;}
  // </group>

  // Read-only access to required columns
  // <group>
  // </group>

protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  MSStateColumns();

  //# attach this object to the supplied table.
  void attach(MSState& msState);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  MSStateColumns(const MSStateColumns&);
  MSStateColumns& operator=(const MSStateColumns&);

  //# required columns
  ScalarColumn<Double> cal_p;
  ScalarColumn<Bool> flagRow_p;
  ScalarColumn<Double> load_p;
  ScalarColumn<String> obsMode_p;
  ScalarColumn<Bool> ref_p;
  ScalarColumn<Bool> sig_p;
  ScalarColumn<Int> subScan_p;

  // Access to Quantum columns
  ScalarQuantColumn<Double> calQuant_p;
  ScalarQuantColumn<Double> loadQuant_p;
};

} //# NAMESPACE CASACORE - END

#endif
