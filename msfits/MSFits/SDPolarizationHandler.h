//# SDPolarizationFiller.h: fills the POLARIZATION table for the SDFITS filler
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

#ifndef MS_SDPOLARIZATIONHANDLER_H
#define MS_SDPOLARIZATIONHANDLER_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Containers/RecordField.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class ColumnsIndex;
class MeasurementSet;
class MSPolarization;
class MSPolarizationColumns;
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

class SDPolarizationHandler
{
public:
    // default ctor is not attached to a MS and hence is useless until attached
    SDPolarizationHandler();

    // attach this to a MS - no columns are explicitly handled here
    SDPolarizationHandler(MeasurementSet &ms, Vector<Bool> &handledCols, const Record &row);

    // copy ctor
    SDPolarizationHandler(const SDPolarizationHandler &other);

    ~SDPolarizationHandler() {clearAll();}

    // assignment operator, uses copy semantics
    SDPolarizationHandler &operator=(const SDPolarizationHandler &other);

    // attach to a MS, the handledCols and row arguments are ignored here
    void attach(MeasurementSet &ms, Vector<Bool> &handledCols, const Record &row);

    // reset internals given indicated row, use the same MS; just resets the id pointer
    void resetRow(const Record &row);
    
    // fill - a new row is added only when necessary
    void fill(const Record &row, const Vector<Int> &stokes);

    // get the current polarization ID
    Int polarizationId() {return rownr_p;}
private:
    RecordFieldPtr<Int> numCorrKey_p;
    ColumnsIndex *index_p;
    MSPolarization *msPol_p;
    MSPolarizationColumns *msPolCols_p;

    Int rownr_p;

    // from a pre-existing MS
    RORecordFieldPtr<Int> numCorrField_p;
    RORecordFieldPtr<Array<Int> > corrTypeField_p, corrProductField_p;
    RORecordFieldPtr<Bool> flagRowField_p;

    // decompose a stokes value into constituent parts for use
    // in making the CORR_PRODUCT matrix
    void stokesKeys(Int stokesValue, Int &key1, Int &key2);

    // cleanup everything
    void clearAll();

    void clearRow();

    // initialize everything
    void initAll(MeasurementSet &ms, Vector<Bool> &handledCols, const Record &row);

    void initRow(Vector<Bool> &handledCols, const Record &row);
};


} //# NAMESPACE CASACORE - END

#endif


