//# NewMSFlagCmdColumns.h: provides easy access to NewMSFlagCmd columns
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

#if !defined(AIPS_NewMSFLAGCMDCOLUMNS_H)
#define AIPS_NewMSFLAGCMDCOLUMNS_H

#include <aips/MeasurementSets/NewMSFlagCmd.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Tables/ArrayColumn.h>
#include <aips/Quanta/Unit.h>
#include <aips/Quanta/Quantum.h>
#include <aips/TableMeasures/ScalarMeasColumn.h>
#include <aips/TableMeasures/ScalarQuantColumn.h>

class MEpoch;

// <summary>
// A convenience class to provide easy access to NewMSFlagCmd columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> NewMSFlagCmd
//   <li> ArrayColumn
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// NewMSFlagCmdColumns stands for NewMeasurementSet FlagCmd Table columns.
// </etymology>
//
// <synopsis>
// This class provides access to the columns in the NewMSFlagCmd Table,
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

class NewMSFlagCmdColumns
{
public:

NewMSFlagCmdColumns(NewMSFlagCmd& msFlagCmd);

~NewMSFlagCmdColumns();

  // Access to columns
  ScalarColumn<Bool>& applied() {return applied_p;}
  ScalarColumn<String>& command() {return command_p;}
  ScalarColumn<Double>& interval() {return interval_p;}
  ScalarColumn<Int>& level() {return level_p;}
  ScalarColumn<String>& reason() {return reason_p;}
  ScalarColumn<Int>& severity() {return severity_p;}
  ScalarColumn<Double>& time() {return time_p;}
  ScalarColumn<String>& type() {return type_p;}
  
  // Access to Measure columns
  ScalarMeasColumn<MEpoch>& timeMeas() { return timeMeas_p;}

   // Access to Quantum columns
  ScalarQuantColumn<Double>& intervalQuant() { return intervalQuant_p;}
  ScalarQuantColumn<Double>& timeQuant() { return timeQuant_p;}

private:
  ScalarColumn<Bool> applied_p;
  ScalarColumn<String> command_p;
  ScalarColumn<Double> interval_p;
  ScalarColumn<Int> level_p;
  ScalarColumn<String> reason_p;
  ScalarColumn<Int> severity_p;
  ScalarColumn<Double> time_p;
  ScalarColumn<String> type_p;

  // Access to Measure columns
  ScalarMeasColumn<MEpoch> timeMeas_p;

   // Access to Quantum columns
  ScalarQuantColumn<Double> intervalQuant_p;
  ScalarQuantColumn<Double> timeQuant_p;


};

// <summary>
// A convenience class to provide easy access to NewMSFlagCmd columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> NewMSFlagCmd
//   <li> ArrayColumn
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// RONewMSFlagCmdColumns stands for Read-Only NewMeasurementSet FlagCmd Table columns.
// </etymology>
//
// <synopsis>
// This class provides read-only access to the columns in the NewMSFlagCmd Table.
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

class RONewMSFlagCmdColumns
{
public:

RONewMSFlagCmdColumns(const NewMSFlagCmd& msFlagCmd);

~RONewMSFlagCmdColumns();

// Access to columns
  const ROScalarColumn<Bool>& applied() const {return applied_p;}
  const ROScalarColumn<String>& command() const {return command_p;}
  const ROScalarColumn<Double>& interval() const {return interval_p;}
  const ROScalarColumn<Int>& level() const {return level_p;}
  const ROScalarColumn<String>& reason() const {return reason_p;}
  const ROScalarColumn<Int>& severity() const {return severity_p;}
  const ROScalarColumn<Double>& time() const {return time_p;}
  const ROScalarColumn<String>& type() const {return type_p;}
  
  // Access to Measure columns
  const ROScalarMeasColumn<MEpoch>& timeMeas() const { return timeMeas_p;}

   // Access to Quantum columns
  const ROScalarQuantColumn<Double>& intervalQuant() const { return intervalQuant_p;}
  const ROScalarQuantColumn<Double>& timeQuant() const { return timeQuant_p;}


private:
  ROScalarColumn<Bool> applied_p;
  ROScalarColumn<String> command_p;
  ROScalarColumn<Double> interval_p;
  ROScalarColumn<Int> level_p;
  ROScalarColumn<String> reason_p;
  ROScalarColumn<Int> severity_p;
  ROScalarColumn<Double> time_p;
  ROScalarColumn<String> type_p;

  // Access to Measure columns
  ROScalarMeasColumn<MEpoch> timeMeas_p;

   // Access to Quantum columns
  ROScalarQuantColumn<Double> intervalQuant_p;
  ROScalarQuantColumn<Double> timeQuant_p;

};

#endif
