//# NewMSHistoryColumns.h: provides easy access to NewMSHistory columns
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

#if !defined(AIPS_NEWMSHISTORYCOLUMNS_H)
#define AIPS_NEWMSHISTORYCOLUMNS_H

#include <aips/aips.h>
#include <aips/Measures/MEpoch.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Tables/ArrayColumn.h>
#include <aips/TableMeasures/ScalarMeasColumn.h>
#include <aips/TableMeasures/ScalarQuantColumn.h>
#include <aips/Utilities/String.h>

class NewMSHistory;
// <summary>
// A class to provide easy read-only access to NewMSHistory columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> NewMSHistory
//   <li> ArrayColumn
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// RONewMSHistoryColumns stands for Read-Only NewMeasurementSet History Table columns.
// </etymology>
//
// <synopsis>
// This class provides read-only access to the columns in the NewMSHistory
// Table.  It does the declaration of all the Scalar and ArrayColumns with the
// correct types, so the application programmer doesn't have to worry about
// getting those right. There is an access function for every predefined
// column. Access to non-predefined columns will still have to be done with
// explicit declarations.  See <linkto class=RONewMSColumns>
// RONewMSColumns</linkto> for an example.
// </synopsis>
//
// <motivation>
// See <linkto class=NewMSColumns> NewMSColumns</linkto> for the motivation.
// </motivation>

class RONewMSHistoryColumns
{
public:
  // Create a columns object that accesses the data in the specified Table
  RONewMSHistoryColumns(const NewMSHistory& msHistory);

  // The destructor does nothing special
  ~RONewMSHistoryColumns();

  // Access to required columns
  // <group>
  const ROScalarColumn<String>& application() const {return application_p;}
  const ROArrayColumn<String>& appParams() const {return appParams_p;}
  const ROArrayColumn<String>& cliCommand() const {return cliCommand_p;}
  const ROScalarColumn<String>& message() const {return message_p;}
  const ROScalarColumn<Int>& objectId() const {return objectId_p;}
  const ROScalarColumn<Int>& observationId() const {return observationId_p;}
  const ROScalarColumn<String>& origin() const {return origin_p;}
  const ROScalarColumn<String>& priority() const {return priority_p;}
  const ROScalarColumn<Double>& time() const {return time_p;}
  const ROScalarQuantColumn<Double>& timeQuant() const {return timeQuant_p;}
  const ROScalarMeasColumn<MEpoch>& timeMeas() const {return timeMeas_p;}
  // </group>

  // Convenience function that returns the number of rows in any of the columns
  uInt nrow() const {return application_p.nrow();}

protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  RONewMSHistoryColumns();

  //# attach this object to the supplied table.
  void attach(const NewMSHistory& msHistory);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  RONewMSHistoryColumns(const RONewMSHistoryColumns&);
  RONewMSHistoryColumns& operator=(const RONewMSHistoryColumns&);

  //# required columns
  ROScalarColumn<String> application_p;
  ROArrayColumn<String> appParams_p;
  ROArrayColumn<String> cliCommand_p;
  ROScalarColumn<String> message_p;
  ROScalarColumn<Int> objectId_p;
  ROScalarColumn<Int> observationId_p;
  ROScalarColumn<String> origin_p;
  ROScalarColumn<String> priority_p;
  ROScalarColumn<Double> time_p;

  //# Access to Measure columns
  ROScalarMeasColumn<MEpoch> timeMeas_p;

  //# Access to Quantum columns
  ROScalarQuantColumn<Double> timeQuant_p;
};

// <summary>
// A class to provide easy read-write access to NewMSHistory columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> NewMSHistory
//   <li> ArrayColumn
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// NewMSHistoryColumns stands for NewMeasurementSet History Table columns.
// </etymology>
//
// <synopsis>
// This class provides access to the columns in the NewMSHistory Table,
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

class NewMSHistoryColumns: public RONewMSHistoryColumns
{
public:
  // Create a columns object that accesses the data in the specified Table
  NewMSHistoryColumns(NewMSHistory& msHistory);

  // The destructor does nothing special
  ~NewMSHistoryColumns();

  // Read-write access to required columns
  // <group>
  ScalarColumn<String>& application() {return application_p;}
  ArrayColumn<String>& appParams() {return appParams_p;}
  ArrayColumn<String>& cliCommand() {return cliCommand_p;}
  ScalarColumn<String>& message() {return message_p;}
  ScalarColumn<Int>& objectId() {return objectId_p;}
  ScalarColumn<Int>& observationId() {return observationId_p;}
  ScalarColumn<String>& origin() {return origin_p;}
  ScalarColumn<String>& priority() {return priority_p;}
  ScalarColumn<Double>& time() {return time_p;}
  ScalarQuantColumn<Double>& timeQuant() {return timeQuant_p;}
  ScalarMeasColumn<MEpoch>& timeMeas() {return timeMeas_p;}
  // </group>

  // Read-only access to required columns
  // <group>
  const ROScalarColumn<String>& application() const {
    return RONewMSHistoryColumns::application();}
  const ROArrayColumn<String>& appParams() const {
    return RONewMSHistoryColumns::appParams();}
  const ROArrayColumn<String>& cliCommand() const {
    return RONewMSHistoryColumns::cliCommand();}
  const ROScalarColumn<String>& message() const {
    return RONewMSHistoryColumns::message();}
  const ROScalarColumn<Int>& objectId() const {
    return RONewMSHistoryColumns::objectId();}
  const ROScalarColumn<Int>& observationId() const {
    return RONewMSHistoryColumns::observationId();}
  const ROScalarColumn<String>& origin() const {
    return RONewMSHistoryColumns::origin();}
  const ROScalarColumn<String>& priority() const {
    return RONewMSHistoryColumns::priority();}
  const ROScalarColumn<Double>& time() const {
    return RONewMSHistoryColumns::time();}
  const ROScalarQuantColumn<Double>& timeQuant() const {
    return RONewMSHistoryColumns::timeQuant();}
  const ROScalarMeasColumn<MEpoch>& timeMeas() const {
    return RONewMSHistoryColumns::timeMeas();}
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

protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  NewMSHistoryColumns();

  //# attach this object to the supplied table.
  void attach(NewMSHistory& msHistory);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  NewMSHistoryColumns(const NewMSHistoryColumns&);
  NewMSHistoryColumns& operator=(const NewMSHistoryColumns&);

  //# required columns
  ScalarColumn<String> application_p;
  ArrayColumn<String> appParams_p;
  ArrayColumn<String> cliCommand_p;
  ScalarColumn<String> message_p;
  ScalarColumn<Int> objectId_p;
  ScalarColumn<Int> observationId_p;
  ScalarColumn<String> origin_p;
  ScalarColumn<String> priority_p;
  ScalarColumn<Double> time_p;

  //# Access to Measure columns
  ScalarMeasColumn<MEpoch> timeMeas_p;

  //# Access to Quantum columns
  ScalarQuantColumn<Double> timeQuant_p;
};
#endif
