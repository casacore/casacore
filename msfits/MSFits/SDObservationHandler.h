//# SDObservationFiller.h: fills the OBSERVATION table for the SDFITS filler
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

#ifndef MS_SDOBSERVATIONHANDLER_H
#define MS_SDOBSERVATIONHANDLER_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Containers/RecordField.h>
#include <casacore/tables/Tables/ScalarColumn.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class ColumnsIndex;
class MeasurementSet;
class MSObservation;
class MSObservationColumns;
class Record;
class String;

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

class SDObservationHandler
{
public:
    // default ctor is not attached to a MS and hence is useless until attached
    SDObservationHandler();

    // attach this to a MS, mark the appropriate columns as handled given
    // the indicated row
    SDObservationHandler(MeasurementSet &ms, Vector<Bool> &handledCols, const Record &row);

    // copy ctor
    SDObservationHandler(const SDObservationHandler &other);

    ~SDObservationHandler() {clearAll();}

    // assignment operator, uses copy semantics
    SDObservationHandler &operator=(const SDObservationHandler &other);

    // attach to a MS, mark the appropriate columns as handled given the row
    void attach(MeasurementSet &ms, Vector<Bool> &handledCols, const Record &row);

    // reset internals given indicated row, use the same MS 
    void resetRow(const Record &row);
    
    // fill - a new row is added only when necessary
    void fill(const Record &row, const String &telescopeName, const Vector<Double> &timeRange);

    // get the current observation ID
    Int observationId() {return rownr_p;}

    // update the time range
    void updateTimeRange(const Vector<Double> &timeRange);
private:
    ColumnsIndex *index_p;
    RecordFieldPtr<String> telescopeKey_p, observerKey_p, projectKey_p, ns_obsidKey_p;
    RecordFieldPtr<Double> releaseDateKey_p;
    RecordFieldPtr<Bool> flagRowKey_p;

    MSObservation *msObs_p;
    MSObservationColumns *msObsCols_p;

    Int rownr_p;

    ScalarColumn<String> nsObsIdCol_p;

    // pointers to fields in record, only used if attached
    RORecordFieldPtr<String> observer_p, projid_p, obsid_p;
    RORecordFieldPtr<Double> releaseDate_p;
    RORecordFieldPtr<Bool> flagRow_p;
    RORecordFieldPtr<Array<Double> > timeRange_p;

    // cleanup everything
    void clearAll();

    // cleanup things which depend on the row description being fixed
    void clearRow();

    // initialize everything
    void initAll(MeasurementSet &ms, Vector<Bool> &handledCols, const Record &row);

    // initialize the things which depend on the row
    void initRow(Vector<Bool> &handledCols, const Record &row);

    // initialize the index
    void makeIndex();
};


} //# NAMESPACE CASACORE - END

#endif


