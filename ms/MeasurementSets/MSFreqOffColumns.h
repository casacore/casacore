//# MSFreqOffsetColumns.h: provides easy access to FREQ_OFFSET columns
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

#ifndef MS_MSFREQOFFCOLUMNS_H
#define MS_MSFREQOFFCOLUMNS_H

#include <casacore/casa/aips.h>
#include <casacore/measures/Measures/MEpoch.h>
#include <casacore/measures/Measures/MCEpoch.h>
#include <casacore/measures/TableMeasures/ScalarMeasColumn.h>
#include <casacore/measures/TableMeasures/ScalarQuantColumn.h>
#include <casacore/tables/Tables/ScalarColumn.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class MSFreqOffset;

// <summary>
// A class to provide easy access to MSFreqOffset columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> MSFreqOffset
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// MSFreqOffsetColumns stands for MeasurementSet FreqOffset Table
// columns.
// </etymology>
//
// <synopsis>
// This class provides access to the columns in the MSFreqOffset Table,
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

class MSFreqOffsetColumns
{
public:
  // Create a columns object that accesses the data in the specified Table
  MSFreqOffsetColumns(const MSFreqOffset& msFreqOffset);

  // The destructor does nothing special
  ~MSFreqOffsetColumns();
  
  // Is this object defined? (MSFreqOffset table is optional)
  Bool isNull() const {return isNull_p;}
  
  // Access to required columns
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

  // Const access to required columns
  // <group>
  const ScalarColumn<Int>& antenna1() const {return antenna1_p;}
  const ScalarColumn<Int>& antenna2() const {return antenna2_p;}
  const ScalarColumn<Int>& feedId() const {return feedId_p;}
  const ScalarColumn<Double>& interval() const {return interval_p;}
  const ScalarQuantColumn<Double>& intervalQuant() const {return intervalQuant_p;}
  const ScalarColumn<Double>& offset() const {return offset_p;}
  const ScalarQuantColumn<Double>& offsetQuant() const {return offsetQuant_p;}
  const ScalarColumn<Int>& spectralWindowId() const {return spectralWindowId_p;}
  const ScalarColumn<Double>& time() const {return time_p;}
  const ScalarQuantColumn<Double>& timeQuant() const {return timeQuant_p;}
  const ScalarMeasColumn<MEpoch>& timeMeas() const {return timeMeas_p;}
  // </group>

  // Convenience function that returns the number of rows in any of the
  // columns. Returns zero if the object is null.
  rownr_t nrow() const {return isNull() ? 0 : antenna1_p.nrow();}

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
  MSFreqOffsetColumns();

  //# attach this object to the supplied table.
  void attach(const MSFreqOffset& msFreqOffset);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  MSFreqOffsetColumns(const MSFreqOffsetColumns&);
  MSFreqOffsetColumns& operator=(const MSFreqOffsetColumns&);

  //# Is the object not attached to a Table.
  Bool isNull_p;

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

//# Define the RO version for backward compatibility.
typedef MSFreqOffsetColumns ROMSFreqOffsetColumns;

} //# NAMESPACE CASACORE - END

#endif
