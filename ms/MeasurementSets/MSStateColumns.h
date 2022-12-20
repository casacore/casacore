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

#ifndef MS_MSSTATECOLUMNS_H
#define MS_MSSTATECOLUMNS_H

#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/measures/TableMeasures/ScalarQuantColumn.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class MSState;

// <summary>
// A class to provide easy access to MSState columns
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

class MSStateColumns
{
public:
  // Create a columns object that accesses the data in the specified Table
  MSStateColumns(const MSState& msState);

  // The destructor does nothing special
  ~MSStateColumns();

  // Access to required columns
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

  // Const access to required columns
  // <group>
  const ScalarColumn<Double>& cal() const {return cal_p;}
  const ScalarQuantColumn<Double>& calQuant() const { return calQuant_p;}
  const ScalarColumn<Bool>& flagRow() const {return flagRow_p;}
  const ScalarColumn<Double>& load() const {return load_p;}
  const ScalarQuantColumn<Double>& loadQuant() const { return loadQuant_p;}
  const ScalarColumn<String>& obsMode() const {return obsMode_p;}
  const ScalarColumn<Bool>& ref() const {return ref_p;}
  const ScalarColumn<Bool>& sig() const {return sig_p;}
  const ScalarColumn<Int>& subScan() const {return subScan_p;}
  // </group>

  // Convenience function that returns the number of rows in any of the columns
  rownr_t nrow() const {return cal_p.nrow();}

  // Returns the last row that contains a state with the specified values.
  // For Cal and Load, the tolerance is applied in the match.
  // Returns -1 if no match could be found. Flagged rows can never match. 
  // If tryRow is non-negative, then that row is tested
  // to see if it matches before any others are tested. Setting tryRow to a
  // positive value greater than the table length will throw an exception
  // (AipsError), when compiled in debug mode.
  Int64 matchState(const Quantum<Double>& stateCalQ,
                   const Quantum<Double>& stateLoadQ,
                   const String& stateObsMode,
                   const Bool& stateRef,
                   const Bool& stateSig,
                   const Int& stateSubScan,
                   const Quantum<Double>& tolerance,
                   Int64 tryRow=-1);

protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  MSStateColumns();

  //# attach this object to the supplied table.
  void attach(const MSState& msState);

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

//# Define the RO version for backward compatibility.
typedef MSStateColumns ROMSStateColumns;

} //# NAMESPACE CASACORE - END

#endif
