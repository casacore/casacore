//# SDFieldFiller.h: fills the FIELD table for the SDFITS filler
//# Copyright (C) 2000,2001
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

#ifndef MS_SDFIELDHANDLER_H
#define MS_SDFIELDHANDLER_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Containers/RecordField.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class MeasurementSet;
class MSField;
class MSFieldColumns;
class String;
class Record;
class ColumnsIndex;

template <class T> class Vector;
template <class T> class Matrix;

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

class SDFieldHandler
{
public:
    // default ctor is not attached to a MS and hence is useless until attached
    SDFieldHandler();

    // attach this to a MS - no columns are explicitly handled here
    SDFieldHandler(MeasurementSet &ms, Vector<Bool> &handledCols, const Record &row);

    // copy ctor
    SDFieldHandler(const SDFieldHandler &other);

    ~SDFieldHandler() {clearAll();}

    // assignment operator, uses copy semantics
    SDFieldHandler &operator=(const SDFieldHandler &other);

    // attach to a MS, the handledCols and row arguments are ignored here
    void attach(MeasurementSet &ms, Vector<Bool> &handledCols, const Record &row);

    // reset internals given indicated row, use the same MS; just resets the id pointer
    void resetRow(const Record &row);
    
    // fill - a new row is added at each call unless the data is from a previous MS fill
    // in which case an existing MAIN_FIELD_ID is used to see if that existing row might
    // be reused
    void fill(const Record &row, const String &name, Int directionRefType,
	      const Matrix<Double> &directionPoly, Double time, Int sourceId);

    // get the current field ID
    Int fieldId() {return rownr_p;}
private:
    MSField *msField_p;
    MSFieldColumns *msFieldCols_p;

    Int rownr_p;

    // fields which might be present if the data is originally from a MS
    RORecordFieldPtr<Int> fieldIdField_p;
    RORecordFieldPtr<String> codeField_p, nameField_p;
    RORecordFieldPtr<Double> timeField_p;
    RORecordFieldPtr<Array<Double> > delayDirField_p, delayDirRateField_p, 
	phaseDirField_p, phaseDirRateField_p, referenceDirField_p,
	referenceDirRateField_p;
    RORecordFieldPtr<Bool> flagRowField_p;

    ColumnsIndex *index_p;
    RecordFieldPtr<String> nameKey_p;
    RecordFieldPtr<Int> sourceIdKey_p;
    RecordFieldPtr<Double> timeKey_p;

    // cleanup everything
    void clearAll();

    void clearRow();

    // initialize everything
    void initAll(MeasurementSet &ms, Vector<Bool> &handledCols, const Record &row);

    // initialize things which depend on the row
    void initRow(Vector<Bool> &handledCols, const Record &row);
};


} //# NAMESPACE CASACORE - END

#endif


