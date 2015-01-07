//# SDHistoryFiller.h: fills the HISTORY table for the SDFITS filler
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

#ifndef MS_SDHISTORYHANDLER_H
#define MS_SDHISTORYHANDLER_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Containers/RecordField.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class MeasurementSet;
class MSHistory;
class MSHistoryColumns;
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

class SDHistoryHandler
{
public:
    // default ctor is not attached to a MS and hence is useless until attached
    SDHistoryHandler();

    // attach this to a MS - no columns are explicitly handled here
    SDHistoryHandler(MeasurementSet &ms, const Vector<Bool> &handledCols, const Record &row);

    // copy ctor
    SDHistoryHandler(const SDHistoryHandler &other);

    ~SDHistoryHandler() {clearAll();}

    // assignment operator, uses copy semantics
    SDHistoryHandler &operator=(const SDHistoryHandler &other);

    // attach to a MS
    void attach(MeasurementSet &ms, Vector<Bool> &handledCols, const Record &row);

    // reset internals given indicated row, use the same MS
    void resetRow(const Record &row);

    // fill - a new row is added on each call, the message time stamp is the current time
    void fill(const Record& row, Int observationId,
	      const String &message, const String &priority);
private:
    MSHistory *msHis_p;
    MSHistoryColumns *msHisCols_p;

    // TIMESYS field pointer when available
    RORecordFieldPtr<String> timesys_p;

    // cleanup everything
    void clearAll();

    // clean up row-dependent stuff
    void clearRow();

    // initialize everything
    void initAll(MeasurementSet &ms, const Vector<Bool> &handledCols, const Record &row);

    // initialize stuff which depends on the row
    void initRow(const Vector<Bool> &handledCols, const Record &row);
};


} //# NAMESPACE CASACORE - END

#endif
