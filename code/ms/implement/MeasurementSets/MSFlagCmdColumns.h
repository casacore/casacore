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

#if !defined(AIPS_NEWMSFLAGCMDCOLUMNS_H)
#define AIPS_NEWMSFLAGCMDCOLUMNS_H

#include <aips/aips.h>
#include <aips/Measures/MEpoch.h>
#include <aips/TableMeasures/ScalarMeasColumn.h>
#include <aips/TableMeasures/ScalarQuantColumn.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Utilities/String.h>

class NewMSFlagCmd;

// <summary>
// A class to provide easy read-only access to NewMSFlagCmd columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> NewMSFlagCmd
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// RONewMSFlagCmdColumns stands for Read-Only NewMeasurementSet FlagCmd Table columns.
// </etymology>
//
// <synopsis>
// This class provides read-only access to the columns in the NewMSFlagCmd
// Table.  It does the declaration of all the ScalarColumns with the correct
// types, so the application programmer doesn't have to worry about getting
// those right. There is an access function for every predefined column. Access
// to non-predefined columns will still have to be done with explicit
// declarations.  See <linkto class=RONewMSColumns> RONewMSColumns</linkto> for
// an example.
// </synopsis>
//
// <motivation>
// See <linkto class=NewMSColumns> NewMSColumns</linkto> for the motivation.
// </motivation>

class RONewMSFlagCmdColumns
{
public:
  // Create a columns object that accesses the data in the specified Table
  RONewMSFlagCmdColumns(const NewMSFlagCmd& msFlagCmd);

  // The destructor does nothing special
  ~RONewMSFlagCmdColumns();

  // Access to required columns
  // <group>
  const ROScalarColumn<Bool>& applied() const {return applied_p;}
  const ROScalarColumn<String>& command() const {return command_p;}
  const ROScalarQuantColumn<Double>& intervalQuant() const {
    return intervalQuant_p;}
  const ROScalarColumn<Double>& interval() const {return interval_p;}
  const ROScalarColumn<Int>& level() const {return level_p;}
  const ROScalarColumn<String>& reason() const {return reason_p;}
  const ROScalarColumn<Int>& severity() const {return severity_p;}
  const ROScalarColumn<Double>& time() const {return time_p;}
  const ROScalarQuantColumn<Double>& timeQuant() const { return timeQuant_p;}
  const ROScalarMeasColumn<MEpoch>& timeMeas() const { return timeMeas_p;}
  const ROScalarColumn<String>& type() const {return type_p;}
  // </group>

  // Convenience function that returns the number of rows in any of the columns
  uInt nrow() const {return applied_p.nrow();}

protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  RONewMSFlagCmdColumns();

  //# attach this object to the supplied table.
  void attach(const NewMSFlagCmd& msFlagCmd);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  RONewMSFlagCmdColumns(const RONewMSFlagCmdColumns&);
  RONewMSFlagCmdColumns& operator=(const RONewMSFlagCmdColumns&);

  //# required columns
  ROScalarColumn<Bool> applied_p;
  ROScalarColumn<String> command_p;
  ROScalarColumn<Double> interval_p;
  ROScalarColumn<Int> level_p;
  ROScalarColumn<String> reason_p;
  ROScalarColumn<Int> severity_p;
  ROScalarColumn<Double> time_p;
  ROScalarColumn<String> type_p;

  //# Access to Measure columns
  ROScalarMeasColumn<MEpoch> timeMeas_p;

  //# Access to Quantum columns
  ROScalarQuantColumn<Double> intervalQuant_p;
  ROScalarQuantColumn<Double> timeQuant_p;
};

// <summary>
// A class to provide easy read-write access to NewMSFlagCmd columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> NewMSFlagCmd
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// NewMSFlagCmdColumns stands for NewMeasurementSet FlagCmd Table columns.
// </etymology>
//
// <synopsis>
// This class provides access to the columns in the NewMSFlagCmd Table, it does
// the declaration of all the ScalarColumns with the correct types, so the
// application programmer doesn't have to worry about getting those
// right. There is an access function for every predefined column. Access to
// non-predefined columns will still have to be done with explicit
// declarations.  See <linkto class=NewMSColumns> NewMSColumns</linkto> for an
// example.
// </synopsis>
//
// <motivation>
// See <linkto class=NewMSColumns> NewMSColumns</linkto> for the motivation.
// </motivation>

class NewMSFlagCmdColumns: public RONewMSFlagCmdColumns
{
public:
  // Create a columns object that accesses the data in the specified Table
  NewMSFlagCmdColumns(NewMSFlagCmd& msFlagCmd);
  
  // The destructor does nothing special
  ~NewMSFlagCmdColumns();

  // Read-write access to required columns
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

  // Read-only access to required columns
  // <group>
  const ROScalarColumn<Bool>& applied() const {
    return RONewMSFlagCmdColumns::applied();}
  const ROScalarColumn<String>& command() const {
    return RONewMSFlagCmdColumns::command();}
  const ROScalarQuantColumn<Double>& intervalQuant() const {
    return RONewMSFlagCmdColumns::intervalQuant();}
  const ROScalarColumn<Double>& interval() const {
    return RONewMSFlagCmdColumns::interval();}
  const ROScalarColumn<Int>& level() const {
    return RONewMSFlagCmdColumns::level();}
  const ROScalarColumn<String>& reason() const {
    return RONewMSFlagCmdColumns::reason();}
  const ROScalarColumn<Int>& severity() const {
    return RONewMSFlagCmdColumns::severity();}
  const ROScalarColumn<Double>& time() const {
    return RONewMSFlagCmdColumns::time();}
  const ROScalarQuantColumn<Double>& timeQuant() const {
    return RONewMSFlagCmdColumns::timeQuant();}
  const ROScalarMeasColumn<MEpoch>& timeMeas() const {
    return RONewMSFlagCmdColumns::timeMeas();}
  const ROScalarColumn<String>& type() const {
    return RONewMSFlagCmdColumns::type();}
  // </group>

protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  NewMSFlagCmdColumns();

  //# attach this object to the supplied table.
  void attach(NewMSFlagCmd& msFlagCmd);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  NewMSFlagCmdColumns(const NewMSFlagCmdColumns&);
  NewMSFlagCmdColumns& operator=(const NewMSFlagCmdColumns&);

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
#endif
