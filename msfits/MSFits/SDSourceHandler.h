//# SDSourceFiller.h: fills the SOURCE table for the SDFITS filler
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

#ifndef MS_SDSOURCEHANDLER_H
#define MS_SDSOURCEHANDLER_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/ArrayFwd.h>
#include <casacore/casa/Containers/RecordField.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class ColumnsIndex;
class MeasurementSet;
class MSSource;
class MSSourceColumns;
class Record;

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

class SDSourceHandler
{
public:
    // default ctor is not attached to a MS and hence is useless until attached
    SDSourceHandler();

    // attach this to a MS, marking fields in row which are explicitly handled here
    SDSourceHandler(MeasurementSet &ms, Vector<bool> &handledCols, const Record &row);

    // copy ctor
    SDSourceHandler(const SDSourceHandler &other);

    ~SDSourceHandler() {clearAll();}

    // assignment operator, uses copy semantics
    SDSourceHandler &operator=(const SDSourceHandler &other);

    // attach to a MS, the handledCols and row arguments are ignored here
    void attach(MeasurementSet &ms, Vector<bool> &handledCols, const Record &row);

    // reset internals given indicated row, use the same MS; just resets the id pointer
    void resetRow(const Record &);
    
    // fill - a source is unique in source name and code
    void fill(const Record &row, int32_t spectralWindowId);

    // get the current source ID
    int32_t sourceId() {return sourceId_p;}
private:
    RecordFieldPtr<String> nameKey_p, codeKey_p;
    ColumnsIndex *index_p;
    MSSource *msSource_p;
    MSSourceColumns *msSourceCols_p;

    // the current source ID
    int32_t sourceId_p;

    // the next source ID to use
    int32_t nextSourceId_p;

    // fields possibly mined from the SDFITS row
    // floating point fields that we can't be certain of their type
    int32_t restfreq_p, vframe_p;
    // String fields
    RORecordFieldPtr<String> transiti_p, molecule_p, object_p, obsmode_p;

    // which optional colums exist
    bool hasTransition_p, hasRestFreq_p, hasSysVel_p, hasPosition_p;

    // fields which might come from a pre-existin MS
    RORecordFieldPtr<int32_t> calibrationGroupField_p, pulsarIdField_p;
    RORecordFieldPtr<double> timeField_p, intervalField_p;
    RORecordFieldPtr<Array<double> > directionField_p, positionField_p, properMotionField_p;

    // cleanup everything
    void clearAll();

    // clean up items related to the row
    void clearRow();

    // initialize everything
    void initAll(MeasurementSet &ms, Vector<bool> &handledCols, const Record &row);

    // initialize the stuff dependent on the row
    void initRow(Vector<bool> &handledCols, const Record &row);
};


} //# NAMESPACE CASACORE - END

#endif


