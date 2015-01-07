//# SDSpWindowFiller.h: fills the SPECTRAL_WINDOW table for the SDFITS filler
//# Copyright (C) 2000,2001,2002
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

#ifndef MS_SDSPWINHANDLER_H
#define MS_SDSPWINHANDLER_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Containers/RecordField.h>
#include <casacore/tables/Tables/TableColumn.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class ColumnsIndex;
class MeasurementSet;
class MSSpectralWindow;
class MSSpWindowColumns;
class Record;
class Table;

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

class SDSpWindowHandler
{
public:
    // default ctor is not attached to a MS and hence is useless until attached
    SDSpWindowHandler();

    // attach this to a MS, marking fields in row which are explicitly handled here
    SDSpWindowHandler(MeasurementSet &ms, Vector<Bool> &handledCols, const Record &row);

    // copy ctor
    SDSpWindowHandler(const SDSpWindowHandler &other);

    ~SDSpWindowHandler() {clearAll();}

    // assignment operator, uses copy semantics
    SDSpWindowHandler &operator=(const SDSpWindowHandler &other);

    // attach to a MS, the handledCols and row arguments are ignored here
    void attach(MeasurementSet &ms, Vector<Bool> &handledCols, const Record &row);

    // reset internals given indicated row, use the same MS; just resets the id pointer
    void resetRow(const Record &);
    
    // fill - a circular buffer of last 100 spectral windows is checked
    void fill(const Record &row, const Vector<Double> &frequency, Double refFrequency,
	      Double originalFreqDelt, Int freqRefType);

    // get the current spWindow ID
    Int spWindowId() {return rownr_p;}
private:
    RecordFieldPtr<Int> nchanKey_p, freqRefTypeKey_p, ifConvChainKey_p,
	freqGroupKey_p, netSidebandKey_p;
    Vector<Double> fNCache_p, f0Cache_p, bwCache_p;
    Double *fNCachePtr_p, *f0CachePtr_p, *bwCachePtr_p;
    Bool deleteItFN_p, deleteItF0_p, deleteItBw_p;
    RecordFieldPtr<Bool> flagRowKey_p;
    // the cache table is the one that is indexed
    ColumnsIndex *index_p;
    // temporary table to hold the fields we are indexing on, can't index on array column
    Table *theCache_p;
    MSSpectralWindow *msSpWin_p;
    MSSpWindowColumns *msSpWinCols_p;

    // the columns in the cache table
    TableColumn idCol_p, nchanCol_p, freqRefTypeCol_p, 
	freqresCol_p, ifConvChainCol_p,
	freqGroupCol_p, netSidebandCol_p, flagRowCol_p;

    // the next row number to use in the cached
    uInt nextCacheRow_p;

    // the maximum number of rows in the cache - currently this is 1000
    uInt cacheSize_p;

    // the current row number in the SPECTRAL_WINDOW table, i.e. the id
    Int rownr_p;

    // fields possibly mined from the SDFITS row
    // floating point fields that we can't be certain of their type
    Int bandwidField_p, freqresField_p;

    // fields from a previous life as a MS
    RORecordFieldPtr<Int> spWinIdField_p, ifConvChainField_p, freqGroupField_p,
	netSidebandField_p;
    RORecordFieldPtr<Bool> flagRowField_p;

    // cleanup everything
    void clearAll();

    // clean up items related to the row
    void clearRow();

    // initialize everything
    void initAll(MeasurementSet &ms, Vector<Bool> &handledCols, const Record &row);

    // initialize the stuff dependent on the row
    void initRow(Vector<Bool> &handledCols, const Record &row);
};


} //# NAMESPACE CASACORE - END

#endif


