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

#if !defined(AIPS_NewMSFREQ_OFFSETCOLUMNS_H)
#define AIPS_NewMSFREQ_OFFSETCOLUMNS_H

#include <aips/MeasurementSets/NewMSFreqOffset.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Quanta/Unit.h>
#include <aips/Quanta/Quantum.h>
#include <aips/TableMeasures/ScalarMeasColumn.h>
#include <aips/TableMeasures/ScalarQuantColumn.h>

class MEpoch;

// <summary>
// A convenience class to provide easy access to NewMSFreqOffset columns
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
// NewMSFreqOffsetColumns stands for NewMeasurementSet FreqOffset Table columns.
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

class NewMSFreqOffsetColumns
{
public:

NewMSFreqOffsetColumns(NewMSFreqOffset& msFreqOffset);

  ~NewMSFreqOffsetColumns();
  
  // Is this object defined? (NewMSFreqOffset table is optional)
  Bool isNull() {return isNull_p;}

  // Access to columns
  ScalarColumn<Int>& antenna1() {return antenna1_p;}
  ScalarColumn<Int>& antenna2() {return antenna2_p;}
  ScalarColumn<Int>& feedId() {return feedId_p;}
  ScalarColumn<Double>& interval() {return interval_p;}
  ScalarColumn<Double>& offset() {return offset_p;}
  ScalarColumn<Int>& spectralWindowId() {return spectralWindowId_p;}
  ScalarColumn<Double>& time() {return time_p;}

  // Access to Measure columns
  ScalarMeasColumn<MEpoch>& timeMeas() { return timeMeas_p;}

   // Access to Quantum columns
  ScalarQuantColumn<Double>& intervalQuant() { return intervalQuant_p;}
  ScalarQuantColumn<Double>& offsetQuant() { return offsetQuant_p;}
  ScalarQuantColumn<Double>& timeQuant() { return timeQuant_p;}
  
private:
  
  Bool isNull_p;
  ScalarColumn<Int> antenna1_p;
  ScalarColumn<Int> antenna2_p;
  ScalarColumn<Int> feedId_p;
  ScalarColumn<Double> interval_p;
  ScalarColumn<Double> offset_p;
  ScalarColumn<Int> spectralWindowId_p;
  ScalarColumn<Double> time_p;

  // Access to Measure columns
  ScalarMeasColumn<MEpoch> timeMeas_p;

   // Access to Quantum columns
  ScalarQuantColumn<Double> intervalQuant_p;
  ScalarQuantColumn<Double> offsetQuant_p;
  ScalarQuantColumn<Double> timeQuant_p;
  
};

// <summary>
// A convenience class to provide easy access to NewMSFreqOffset columns
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
// RONewMSFreqOffsetColumns stands for Read-Only NewMeasurementSet FreqOffset Table columns.
// </etymology>
//
// <synopsis>
// This class provides read-only access to the columns in the NewMSFreqOffset Table.
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

class RONewMSFreqOffsetColumns
{
public:

RONewMSFreqOffsetColumns(const NewMSFreqOffset& msFreqOffset);

~RONewMSFreqOffsetColumns();

  // Is this object defined? (NewMSFreqOffset table is optional)
  Bool isNull() {return isNull_p;}

// Access to columns
  const ROScalarColumn<Int>& antenna1() const {return antenna1_p;}
  const ROScalarColumn<Int>& antenna2() const {return antenna2_p;}
  const ROScalarColumn<Int>& feedId() const {return feedId_p;}
  const ROScalarColumn<Double>& interval() const {return interval_p;}
  const ROScalarColumn<Double>& offset() const {return offset_p;}
  const ROScalarColumn<Int>& spectralWindowId() const {return spectralWindowId_p;}
  const ROScalarColumn<Double>& time() const {return time_p;}

  // Access to Measure columns
  const ROScalarMeasColumn<MEpoch>& timeMeas() const { return timeMeas_p;}

   // Access to Quantum columns
  const ROScalarQuantColumn<Double>& intervalQuant() const { return intervalQuant_p;}
  const ROScalarQuantColumn<Double>& offsetQuant() const { return offsetQuant_p;}
  const ROScalarQuantColumn<Double>& timeQuant() const { return timeQuant_p;}
   
private:
  
  Bool isNull_p;
  ROScalarColumn<Int> antenna1_p;
  ROScalarColumn<Int> antenna2_p;
  ROScalarColumn<Int> feedId_p;
  ROScalarColumn<Double> interval_p;
  ROScalarColumn<Double> offset_p;
  ROScalarColumn<Int> spectralWindowId_p;
  ROScalarColumn<Double> time_p;

  // Access to Measure columns
  ROScalarMeasColumn<MEpoch> timeMeas_p;

   // Access to Quantum columns
  ROScalarQuantColumn<Double> intervalQuant_p;
  ROScalarQuantColumn<Double> offsetQuant_p;
  ROScalarQuantColumn<Double> timeQuant_p;

};

#endif


