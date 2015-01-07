//# SDFeedFiller.h: fills the FEED table for the SDFITS filler
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

#ifndef MS_SDFEEDHANDLER_H
#define MS_SDFEEDHANDLER_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Containers/RecordField.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class ColumnsIndex;
class MeasurementSet;
class MSFeed;
class MSFeedColumns;
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

class SDFeedHandler
{
public:
    // default ctor is not attached to a MS and hence is useless until attached
    SDFeedHandler();

    // attach this to a MS - no columns are explicitly handled here
    SDFeedHandler(MeasurementSet &ms, Vector<Bool> &handledCols, const Record &row);

    // copy ctor
    SDFeedHandler(const SDFeedHandler &other);

    ~SDFeedHandler() {clearAll();}

    // assignment operator, uses copy semantics
    SDFeedHandler &operator=(const SDFeedHandler &other);

    // attach to a MS, the handledCols and row arguments are ignored here
    void attach(MeasurementSet &ms, Vector<Bool> &handledCols, const Record &row);

    // reset internals given indicated row, use the same MS
    void resetRow(const Record &row);
    
    // fill - a new row is added only when necessary
    void fill(const Record &row, Int antennaId, Int spwinId, const Vector<Int> &stokes);

    // get the current feed ID
    Int feedId() {return feedId_p;}

    // the current NUM_RECEPTORS value
    Int numReceptors() {return nrecpt_p;}
private:
    RecordFieldPtr<Int> numRecpKey_p;
    ColumnsIndex *index_p;
    MSFeed *msFeed_p;
    MSFeedColumns *msFeedCols_p;

    Int feedId_p, nextFeedId_p, nrecpt_p;

    // fields which might be the result of saving via ms2sdfits
    RORecordFieldPtr<Int> feed1Field_p, feed2Field_p, beamIdField_p, phasedFeedIdField_p, numReceptorsField_p;
    RORecordFieldPtr<Double> intervalField_p, timeField_p, scaReceptorAngleField_p; 
    RORecordFieldPtr<Array<Double> > beamOffsetField_p, positionField_p, receptorAngleField_p;
    RORecordFieldPtr<Array<Complex> > polResponseField_p;
    RORecordFieldPtr<String> polarizationTypeField_p;

    // get the polarization type from the stokes vector
    void stokesToPolType(const Vector<Int> &stokes, Vector<String> &polType);

    // cleanup everything
    void clearAll();

    void clearRow();

    // initialize everything
    void initAll(MeasurementSet &ms, Vector<Bool> &handledCols, const Record &row);

    // initialize things which depend on row
    void initRow(Vector<Bool> &handledCols, const Record &row);
};


} //# NAMESPACE CASACORE - END

#endif


