//# SDSysCalFiller.h: fills the SYSCAL table for the SDFITS filler
//# Copyright (C) 2000
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
//#
//# $Id$

#ifndef MS_SDSYSCALHANDLER_H
#define MS_SDSYSCALHANDLER_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Containers/RecordField.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class MeasurementSet;
class MSSysCal;
class MSSysCalColumns;
class Record;

template <class T> class Vector;

// <summary>
// </summary>

// <use visibility=local>   or   <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> SomeClass
//   <li> SomeOtherClass
//   <li> some concept
// </prerequisite>
//
// <etymology>
// </etymology>
//
// <synopsis>
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// </motivation>
//
// <templating arg=T>
//    <li>
//    <li>
// </templating>
//
// <thrown>
//    <li>
//    <li>
// </thrown>
//
// <todo asof="yyyy/mm/dd">
//   <li> add this feature
//   <li> fix this bug
//   <li> start discussion of this possible extension
// </todo>

class SDSysCalHandler
{
public:
    // default ctor is not attached to a MS and hence is useless until attached
    SDSysCalHandler();

    // attach this to a MS - mark fields in row as handled
    SDSysCalHandler(MeasurementSet &ms, Vector<Bool> &handledCols, const Record &row);

    // copy ctor
    SDSysCalHandler(const SDSysCalHandler &other);

    ~SDSysCalHandler() {clearAll();}

    // assignment operator, uses copy semantics
    SDSysCalHandler &operator=(const SDSysCalHandler &other);

    // attach to a MS, mark fields in row as handled
    void attach(MeasurementSet &ms, Vector<Bool> &handledCols, const Record &row);

    // reset internals given indicated row, use the same MS
    void resetRow(const Record &row);
    
    // fill - a new row is added as necessary, there is no lookback to see if a row could be
    // reused.  Only the current row might be reused.
    void fill(const Record &row, Int antennaId, Int feedId, Int spectralWindowId,
	      Double time, Vector<Double> timeRange, uInt numReceptors);
private:
    MSSysCal *msSysCal_p;
    MSSysCalColumns *msSysCalCols_p;

    Int rownr_p;

    uInt nrecpt_p;

    Int tcalId_p, tsysId_p, trxId_p;

    Bool hasTsysCol_p, hasTcalCol_p, hasTrxCol_p;

    // fields which come from a previous incarnation as a MS
    RORecordFieldPtr<Double> intervalField_p, timeField_p;
    RORecordFieldPtr<Float> phaseDiffField_p;
    RORecordFieldPtr<Bool> tcalFlagField_p, trxFlagField_p, tsysFlagField_p, phaseDiffFlagField_p;
    RORecordFieldPtr<Array<Float> > tcalField_p, trxField_p, tsysField_p;

    // cleanup everything
    void clearAll();

    // cleanup row-related stuff
    void clearRow();

    // initialize everything
    void initAll(MeasurementSet &ms, Vector<Bool> &handledCols, const Record &row);

    // intialize the row related stuff
    void initRow(Vector<Bool> &handledCols, const Record &row);
};


} //# NAMESPACE CASACORE - END

#endif


