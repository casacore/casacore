//# SDFITSFiller.h: fills all otherwise unhandled columns for the SDFITS filler
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

#ifndef MS_SDFITSHANDLER_H
#define MS_SDFITSHANDLER_H

#include <casacore/casa/aips.h>
#include <casacore/measures/Measures/MEpoch.h>
#include <casacore/measures/TableMeasures/ScalarQuantColumn.h>
#include <casacore/measures/TableMeasures/ScalarMeasColumn.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class CopyRecordToTable;
class MeasurementSet;
class Record;
class Table;

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

class SDFITSHandler
{
public:
    // default ctor is not attached to a MS and hence is useless until attached
    SDFITSHandler();

    // attach this to a MS - any unhandled fields in row are handled here.
    // This handler must be attached last.
    SDFITSHandler(MeasurementSet &ms, Vector<Bool> &handledCols, const Record &row);

    // copy ctor
    SDFITSHandler(const SDFITSHandler &other);

    ~SDFITSHandler() {clearAll();}

    // assignment operator, uses copy semantics
    SDFITSHandler &operator=(const SDFITSHandler &other);

    // attach to a MS - any unhandled fields in row are handled here.
    // This handler must be attached last.
    void attach(MeasurementSet &ms, Vector<Bool> &handledCols, const Record &row);

    // fill - a new row is always added
    void fill(const Record &row, const MEpoch &time, const Double &interval);
private:
    // the output table
    Table *tab_p;

    // the TIME column
    ScalarMeasColumn<MEpoch> timeMeas_p;

    // the INTERVAL column
    ScalarQuantColumn<Double> intervalQuant_p;

    // this copies everything from the row to the table
    CopyRecordToTable *copier_p;

    // cleanup everything
    void clearAll();

    // cleanup the row related stuff
    void clearRow();

    // initialize everything
    void initAll(MeasurementSet &ms, Vector<Bool> &handledCols, const Record &row);

    // intialize the row related stuff
    void initRow(Vector<Bool> &handledCols, const Vector<String> &colNames, const Record &row);

    // get the required table desc given the unhandled columns and the row
    TableDesc requiredTableDesc(Vector<Bool> &handledCols, Vector<String> &colNames, const Record &row);
};


} //# NAMESPACE CASACORE - END

#endif


