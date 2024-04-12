//# MSFlagCmdColumns.h: provides easy access to MSFlagCmd columns
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#ifndef MS_MSFLAGCMDCOLUMNS_H
#define MS_MSFLAGCMDCOLUMNS_H

#include <casacore/casa/aips.h>
#include <casacore/measures/Measures/MEpoch.h>
#include <casacore/measures/Measures/MCEpoch.h>
#include <casacore/measures/TableMeasures/ScalarMeasColumn.h>
#include <casacore/measures/TableMeasures/ScalarQuantColumn.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class MSFlagCmd;

// <summary>
// A class to provide easy access to MSFlagCmd columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> MSFlagCmd
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// MSFlagCmdColumns stands for MeasurementSet FlagCmd Table columns.
// </etymology>
//
// <synopsis>
// This class provides access to the columns in the MSFlagCmd Table, it does
// the declaration of all the ScalarColumns with the correct types, so the
// application programmer doesn't have to worry about getting those
// right. There is an access function for every predefined column. Access to
// non-predefined columns will still have to be done with explicit
// declarations.  See <linkto class=MSColumns> MSColumns</linkto> for an
// example.
// </synopsis>
//
// <motivation>
// See <linkto class=MSColumns> MSColumns</linkto> for the motivation.
// </motivation>

class MSFlagCmdColumns
{
public:
  // Create a columns object that accesses the data in the specified Table
  MSFlagCmdColumns(const MSFlagCmd& msFlagCmd);
  
  // The destructor does nothing special
  ~MSFlagCmdColumns();

  // Access to required columns
  // <group>
  ScalarColumn<Bool>& applied() {return applied_p;}
  ScalarColumn<String>& command() {return command_p;}
  ScalarColumn<Double>& interval() {return interval_p;}
  ScalarQuantColumn<Double>& intervalQuant() {return intervalQuant_p;}
  ScalarColumn<Int>& level() {return level_p;}
  ScalarColumn<String>& reason() {return reason_p;}
  ScalarColumn<Int>& severity() {return severity_p;}
  ScalarColumn<Double>& time() {return time_p;}
  ScalarQuantColumn<Double>& timeQuant() {return timeQuant_p;}
  ScalarMeasColumn<MEpoch>& timeMeas() {return timeMeas_p;}
  ScalarColumn<String>& type() {return type_p;}
  // </group>

  // Const access to required columns
  // <group>
  const ScalarColumn<Bool>& applied() const {return applied_p;}
  const ScalarColumn<String>& command() const {return command_p;}
  const ScalarColumn<Double>& interval() const {return interval_p;}
  const ScalarQuantColumn<Double>& intervalQuant() const {return intervalQuant_p;}
  const ScalarColumn<Int>& level() const {return level_p;}
  const ScalarColumn<String>& reason() const {return reason_p;}
  const ScalarColumn<Int>& severity() const {return severity_p;}
  const ScalarColumn<Double>& time() const {return time_p;}
  const ScalarQuantColumn<Double>& timeQuant() const {return timeQuant_p;}
  const ScalarMeasColumn<MEpoch>& timeMeas() const {return timeMeas_p;}
  const ScalarColumn<String>& type() const {return type_p;}
  // </group>

  // Convenience function that returns the number of rows in any of the columns
  rownr_t nrow() const {return applied_p.nrow();}

  // set the epoch type for the FLAG_CMD column.
  // <note role=tip>
  // In principle this function can only be used if the table is empty,
  // otherwise already written values may thereafter have an incorrect
  // reference, offset, or unit.  However, it is possible that part of the
  // table gets written before these values are known.  In that case the
  // reference, offset, or units can be set by using a False
  // <src>tableMustBeEmpty</src> argument.
  // </note>
  void setEpochRef(MEpoch::Types ref, Bool tableMustBeEmpty=True);

protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  MSFlagCmdColumns();

  //# attach this object to the supplied table.
  void attach(const MSFlagCmd& msFlagCmd);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  MSFlagCmdColumns(const MSFlagCmdColumns&);
  MSFlagCmdColumns& operator=(const MSFlagCmdColumns&);

  //# required columns
  ScalarColumn<Bool> applied_p;
  ScalarColumn<String> command_p;
  ScalarColumn<Double> interval_p;
  ScalarColumn<Int> level_p;
  ScalarColumn<String> reason_p;
  ScalarColumn<Int> severity_p;
  ScalarColumn<Double> time_p;
  ScalarColumn<String> type_p;

  //# Access to Measure columns
  ScalarMeasColumn<MEpoch> timeMeas_p;

  //# Access to Quantum columns
  ScalarQuantColumn<Double> intervalQuant_p;
  ScalarQuantColumn<Double> timeQuant_p;
};

//# Define the RO version for backward compatibility.
typedef MSFlagCmdColumns ROMSFlagCmdColumns;

} //# NAMESPACE CASACORE - END

#endif
