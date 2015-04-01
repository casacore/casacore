//# SDPointingFiller.h: fills the POINTING table for the SDFITS filler
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

#ifndef MS_SDPOINTINGHANDLER_H
#define MS_SDPOINTINGHANDLER_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Containers/RecordField.h>
#include <casacore/measures/Measures/MDirection.h>
#include <casacore/casa/Arrays/Matrix.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class MeasurementSet;
class MSPointing;
class MSPointingColumns;
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

class SDPointingHandler
{
public:
    // default ctor is not attached to a MS and hence is useless until attached
    SDPointingHandler();

    // attach this to a MS, mark fields row which are handled here
    SDPointingHandler(MeasurementSet &ms, Vector<Bool> &handledCols, const Record &row);

    // copy ctor
    SDPointingHandler(const SDPointingHandler &other);

    ~SDPointingHandler() {clearAll();}

    // assignment operator, uses copy semantics
    SDPointingHandler &operator=(const SDPointingHandler &other);

    // attach to a MS, mark fields in row which are handled here
    void attach(MeasurementSet &ms, Vector<Bool> &handledCols, const Record &row);

    // reset internals given indicated row, use the same MS
    void resetRow(const Record &);
    
    // fill - a new row is added when 
    //      a) the name changes
    //      b) the time changes such that it would be outside of the new interval when
    //         added to the old interval (i.e. intervals do not overlap)
    //      c) the direction changes
    //      d) the antennaId changes
    //  There is no look-back to see if a previous row could be re-used
    void fill(const Record &row, Int antennaId, Double time, const Vector<Double> &timeRange,
	      const MDirection &direction, const MeasFrame &frame);

    // convenience functions for use when filling the FIELD table, which is mostly
    // just a clone of this table for SD data
    Int nrow() {return rownr_p+1;}
    const String &name() {return name_p;}
    Int directionRefType() {return dirColRef_p.getType();}
    const Matrix<Double> &directionPoly() {return directionPoly_p;}
    Double time() {return time_p;}
private:
    MSPointing *msPointing_p;
    MSPointingColumns *msPointingCols_p;

    Double time_p;

    Int antId_p;
    MDirection direction_p;
    Matrix<Double> directionPoly_p;
    Vector<Double> directionRate_p;
    String name_p;

    Int rownr_p;

    MDirection::Ref dirColRef_p;

    RORecordFieldPtr<String> objectField_p;

    // these might come from an MS table
    // this can just come from an MS v1 table
    RORecordFieldPtr<Array<Double> > pointingDirRateField_p;
    RORecordFieldPtr<Double> intervalField_p, timeField_p;
    RORecordFieldPtr<String> nameField_p;
    RORecordFieldPtr<Bool> trackingField_p;

    // cleanup everything
    void clearAll();

    // cleanup things which depend on the row
    void clearRow();

    // initialize everything
    void initAll(MeasurementSet &ms, Vector<Bool> &handledCols, const Record &row);

    // initialize everythign which depends on row
    void initRow(Vector<Bool> &handledCols, const Record &row);
};


} //# NAMESPACE CASACORE - END

#endif


