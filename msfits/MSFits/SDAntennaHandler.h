//# SDAntennaFiller.h: fills the ANTENNA table for the SDFITS filler
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

#ifndef MS_SDANTENNAHANDLER_H
#define MS_SDANTENNAHANDLER_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Containers/RecordField.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/measures/Measures/MPosition.h>
#include <casacore/casa/Arrays/Array.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class ColumnsIndex;
class MeasurementSet;
class MSAntenna;
class MSAntennaColumns;
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

class SDAntennaHandler
{
public:
    // default ctor is not attached to a MS and hence is useless until attached
    SDAntennaHandler();

    // attach this to a MS, mark the appropriate columns as handled given
    // the indicated row
    SDAntennaHandler(MeasurementSet &ms, Vector<Bool> &handledCols, const Record &row);

    // copy ctor
    SDAntennaHandler(const SDAntennaHandler &other);

    ~SDAntennaHandler() {clearAll();}

    // assignment operator, uses copy semantics
    SDAntennaHandler &operator=(const SDAntennaHandler &other);

    // attach to a MS, mark the appropriate columns as handled given the
    void attach(MeasurementSet &ms, Vector<Bool> &handledCols, const Record &row);

    // reset internals given indicated row, use the same MS 
    void resetRow(const Record &row);
    
    // fill - a new row is added only when necessary
    void fill(const Record &row);

    // get the current antenna ID
    Int antennaId() {return rownr_p;}

    // get the telescope name
    String telescopeName() {return name_p;}

    // get the telescope position
    const MPosition &telescopePosition() {return position_p;}
private:
    ColumnsIndex *index_p;
    RecordFieldPtr<String> nameKey_p, stationKey_p, mountKey_p;
    RecordFieldPtr<Double> dishDiameterKey_p;
    RecordFieldPtr<Int> orbitIdKey_p, phasedIdKey_p;
    RecordFieldPtr<Bool> flagRowKey_p;
    MSAntenna *msAnt_p;
    MSAntennaColumns *msAntCols_p;

    Int rownr_p;

    // pointers to fields in record, only used if attached
    RORecordFieldPtr<String> telescopField_p;

    // telescope position might be a double or float,
    // just remember its location
    Int siteLongFldNum_p, siteLatFldNum_p, siteElevFldNum_p;

    String name_p;
    MPosition position_p;

    // fields which might exist if this SDFITS was converted from an MS using ms2sdfits
    RORecordFieldPtr<String> mountField_p, msNameField_p, stationField_p;
    RORecordFieldPtr<Int> orbitIdField_p, phasedArrayIdField_p;
    RORecordFieldPtr<Double> dishDiameterField_p;
    RORecordFieldPtr<Array<Double> > offsetField_p, positionField_p;
    RORecordFieldPtr<Bool> flagRowField_p;

    // I expect these will never be used, nevertheless, put them here just in case I'm wrong
    void addPhasedArrayIdColumn();
    void addOrbitIdColumn();

    // cleanup everything
    void clearAll();

    // cleanup things which depend on the row description being fixed
    void clearRow();

    // initialize everything
    void initAll(MeasurementSet &ms, Vector<Bool> &handledCols, const Record &row);

    // initialize the things which depend on the row
    void initRow(Vector<Bool> &handledCols, const Record &row);
};


} //# NAMESPACE CASACORE - END

#endif


