//# NewMSFreqOffsetColumns.h: provides easy access to FREQ_OFFSET columns
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

#if !defined(AIPS_NEWMSFREQ_OFFSETCOLUMNS_H)
#define AIPS_NEWMSFREQ_OFFSETCOLUMNS_H

#include <aips/aips.h>
#include <aips/Measures/MEpoch.h>
#include <aips/TableMeasures/ScalarMeasColumn.h>
#include <aips/TableMeasures/ScalarQuantColumn.h>
#include <aips/Tables/ScalarColumn.h>

class NewMSFreqOffset;

// <summary>
// A class to provide easy read-only access to NewMSFreqOffset columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> NewMSFreqOffset
//   <li> ArrayColumn
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// RONewMSFreqOffsetColumns stands for Read-Only NewMeasurementSet FreqOffset
// Table columns.
// </etymology>
//
// <synopsis>
// This class provides read-only access to the columns in the NewMSFreqOffset
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

class RONewMSFreqOffsetColumns
{
public:
  // Create a columns object that accesses the data in the specified Table
  RONewMSFreqOffsetColumns(const NewMSFreqOffset& msFreqOffset);
  
  // The destructor does nothing special
  ~RONewMSFreqOffsetColumns();
  
  // Is this object defined? (NewMSFreqOffset table is optional)
  Bool isNull() const {return isNull_p;}
  
  // Access to columns
  // <group>
  const ROScalarColumn<Int>& antenna1() const {return antenna1_p;}
  const ROScalarColumn<Int>& antenna2() const {return antenna2_p;}
  const ROScalarColumn<Int>& feedId() const {return feedId_p;}
  const ROScalarColumn<Double>& interval() const {return interval_p;}
  const ROScalarQuantColumn<Double>& intervalQuant() const {
    return intervalQuant_p;}
  const ROScalarColumn<Double>& offset() const {return offset_p;}
  const ROScalarQuantColumn<Double>& offsetQuant() const {
    return offsetQuant_p;}
  const ROScalarColumn<Int>& spectralWindowId() const {
    return spectralWindowId_p;}
  const ROScalarColumn<Double>& time() const {return time_p;}
  const ROScalarQuantColumn<Double>& timeQuant() const {return timeQuant_p;}
  const ROScalarMeasColumn<MEpoch>& timeMeas() const {return timeMeas_p;}
  // </group>
  
  // Convenience function that returns the number of rows in any of the
  // columns. Returns zero if the object is null.
  uInt nrow() const {return isNull() ? 0 : antenna1_p.nrow();}

protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  RONewMSFreqOffsetColumns();

  //# attach this object to the supplied table.
  void attach(const NewMSFreqOffset& msFreqOffset);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  RONewMSFreqOffsetColumns(const RONewMSFreqOffsetColumns&);
  RONewMSFreqOffsetColumns& operator=(const RONewMSFreqOffsetColumns&);

  //# Is the object not attached to a Table.
  Bool isNull_p;

  //# required columns
  ROScalarColumn<Int> antenna1_p;
  ROScalarColumn<Int> antenna2_p;
  ROScalarColumn<Int> feedId_p;
  ROScalarColumn<Double> interval_p;
  ROScalarColumn<Double> offset_p;
  ROScalarColumn<Int> spectralWindowId_p;
  ROScalarColumn<Double> time_p;

  //# Access to Measure columns
  ROScalarMeasColumn<MEpoch> timeMeas_p;

  //# Access to Quantum columns
  ROScalarQuantColumn<Double> intervalQuant_p;
  ROScalarQuantColumn<Double> offsetQuant_p;
  ROScalarQuantColumn<Double> timeQuant_p;
};

// <summary>
// A class to provide easy read-write access to NewMSFreqOffset columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> NewMSFreqOffset
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// NewMSFreqOffsetColumns stands for NewMeasurementSet FreqOffset Table
// columns.
// </etymology>
//
// <synopsis>
// This class provides access to the columns in the NewMSFreqOffset Table,
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

class NewMSFreqOffsetColumns: public RONewMSFreqOffsetColumns
{
public:
  // Create a columns object that accesses the data in the specified Table
  NewMSFreqOffsetColumns(NewMSFreqOffset& msFreqOffset);

  // The destructor does nothing special
  ~NewMSFreqOffsetColumns();
  
  // Read-write access to required columns
  // <group>
  ScalarColumn<Int>& antenna1() {return antenna1_p;}
  ScalarColumn<Int>& antenna2() {return antenna2_p;}
  ScalarColumn<Int>& feedId() {return feedId_p;}
  ScalarColumn<Double>& interval() {return interval_p;}
  ScalarQuantColumn<Double>& intervalQuant() {return intervalQuant_p;}
  ScalarColumn<Double>& offset() {return offset_p;}
  ScalarQuantColumn<Double>& offsetQuant() {return offsetQuant_p;}
  ScalarColumn<Int>& spectralWindowId() {return spectralWindowId_p;}
  ScalarColumn<Double>& time() {return time_p;}
  ScalarQuantColumn<Double>& timeQuant() {return timeQuant_p;}
  ScalarMeasColumn<MEpoch>& timeMeas() {return timeMeas_p;}
  // </group>

  // Read-only access to required columns
  // <group>
  const ROScalarColumn<Int>& antenna1() const {
    return RONewMSFreqOffsetColumns::antenna1();}
  const ROScalarColumn<Int>& antenna2() const {
    return RONewMSFreqOffsetColumns::antenna2();}
  const ROScalarColumn<Int>& feedId() const {
    return RONewMSFreqOffsetColumns::feedId();}
  const ROScalarColumn<Double>& interval() const {
    return RONewMSFreqOffsetColumns::interval();}
  const ROScalarQuantColumn<Double>& intervalQuant() const {
    return RONewMSFreqOffsetColumns::intervalQuant();}
  const ROScalarColumn<Double>& offset() const {
    return RONewMSFreqOffsetColumns::offset();}
  const ROScalarQuantColumn<Double>& offsetQuant() const {
    return RONewMSFreqOffsetColumns::offsetQuant();}
  const ROScalarColumn<Int>& spectralWindowId() const {
    return RONewMSFreqOffsetColumns::spectralWindowId();}
  const ROScalarColumn<Double>& time() const {
    return RONewMSFreqOffsetColumns::time();}
  const ROScalarQuantColumn<Double>& timeQuant() const {
    return RONewMSFreqOffsetColumns::timeQuant();}
  const ROScalarMeasColumn<MEpoch>& timeMeas() const {
    return RONewMSFreqOffsetColumns::timeMeas();}
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
  NewMSFreqOffsetColumns();

  //# attach this object to the supplied table.
  void attach(NewMSFreqOffset& msFreqOffset);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  NewMSFreqOffsetColumns(const NewMSFreqOffsetColumns&);
  NewMSFreqOffsetColumns& operator=(const NewMSFreqOffsetColumns&);

  //# required columns
  ScalarColumn<Int> antenna1_p;
  ScalarColumn<Int> antenna2_p;
  ScalarColumn<Int> feedId_p;
  ScalarColumn<Double> interval_p;
  ScalarColumn<Double> offset_p;
  ScalarColumn<Int> spectralWindowId_p;
  ScalarColumn<Double> time_p;

  //# Access to Measure columns
  ScalarMeasColumn<MEpoch> timeMeas_p;

  //# Access to Quantum columns
  ScalarQuantColumn<Double> intervalQuant_p;
  ScalarQuantColumn<Double> offsetQuant_p;
  ScalarQuantColumn<Double> timeQuant_p;
};

#endif
