//# ScaRecordColData.h: Access to a table column containing scalar records
//# Copyright (C) 1998
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
//# $Id$

#ifndef TABLES_SCARECORDCOLDATA_H
#define TABLES_SCARECORDCOLDATA_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/PlainColumn.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class ColumnSet;
class ScalarRecordColumnDesc;
class AipsIO;
template<class T> class Vector;


// <summary>
// Access to a table column containing scalar records.
// </summary>

// <use visibility=local>

// <reviewed reviewer="Wim Brouw" date="1998/12/09" tests="tRecordColumn.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=PlainColumn>PlainColumn</linkto>
//   <li> <linkto class=ScalarRecordColumnDesc>ScalarRecordColumnDesc</linkto>
//   <li> <linkto class=Table>Table</linkto>
// </prerequisite>

// <etymology>
// ScalarRecordColumnData represents a table column containing scalars.
// </etymology>

// <synopsis> 
// The class ScalarRecordColumnData is derived from PlainColumn.
// It implements the virtual functions accessing a table column
// containing scalars holding records.
// <br>
// It is possible to access an scalar in an individual cell (i.e. table row)
// or in the entire column.
// <p>
// The main task of this class is to communicate with the data manager
// column object. This consists of:
// <ul>
//  <li> Binding itself to a data manager.
//  <li> Letting the data manager create its column object.
//  <li> Closing the data manager column object (in putFileDerived).
//  <li> Reconstructing the data manager object for an existing table
//         (in getFileDerived).
//  <li> Transferring get/put calls to the data manager column object.
// </ul>
//
// The class is hidden from the user by the envelope class ScalarColumn.
// If used directly by other Table classes, it should be done with care.
// It assumes that the arrays in the various get and put functions have
// the correct length. ScalarColumn does that check.
// </synopsis>

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//  <li> Introduce a class ArrayRecordColumnData to support arrays of records.
// </todo>


class ScalarRecordColumnData : public PlainColumn
{
public:

    // Construct a scalar column object from the given description
    // in the given column set.
    // This constructor is used by ScalarRecordColumnDesc::makeColumn.
    ScalarRecordColumnData (const ScalarRecordColumnDesc*, ColumnSet*);

    ~ScalarRecordColumnData();

    // Ask if the data manager can handle a column.
    virtual Bool canAccessScalarColumn (Bool& reask) const;

    // Ask if the data manager can handle some cells in a column.
    virtual Bool canAccessScalarColumnCells (Bool& reask) const;

    // Initialize the rows from startRownr till endRownr (inclusive)
    // with the default value defined in the column description.
    virtual void initialize (uInt startRownr, uInt endRownr);

    // Test if the given cell contains a defined value.
    virtual Bool isDefined (uInt rownr) const;

    // Get the value from a particular cell.
    virtual void get (uInt rownr, void*) const;

    // Get the array of all values in the column.
    // The length of the buffer pointed to by dataPtr must match
    // the actual length. This is checked by ScalarColumn.
    virtual void getScalarColumn (void* dataPtr) const;

    // Get the array of some values in the column (on behalf of RefColumn).
    // The length of the buffer pointed to by dataPtr must match
    // the actual length. This is checked by ScalarColumn.
    virtual void getScalarColumnCells (const RefRows& rownrs,
				       void* dataPtr) const;

    // Put the value in a particular cell.
    // The length of the buffer pointed to by dataPtr must match
    // the actual length. This is checked by ScalarColumn.
    virtual void put (uInt rownr, const void* dataPtr);

    // Put the array of all values in the column.
    // The length of the buffer pointed to by dataPtr must match
    // the actual length. This is checked by ScalarColumn.
    virtual void putScalarColumn (const void* dataPtr);

    // Put the array of some values in the column (on behalf on RefColumn).
    // The length of the buffer pointed to by dataPtr must match
    // the actual length. This is checked by ScalarColumn.
    virtual void putScalarColumnCells (const RefRows& rownrs,
				       const void* dataPtr);

    // Add this column and its data to the Sort object.
    // Sorting on records is not supported, so an exception is thrown.
    // <group>
    virtual void makeSortKey (Sort&, CountedPtr<BaseCompare>& cmpObj,
                              Int order,
			      const void*& dataSave);
    // Do it only for the given row numbers.
    virtual void makeRefSortKey (Sort&, CountedPtr<BaseCompare>& cmpObj,
                                 Int order,
                                 const Vector<uInt>& rownrs,
                                 const void*& dataSave);
    // </group>

    // Free storage on the heap allocated by makeSortkey().
    // The pointer will be set to zero.
    virtual void freeSortKey (const void*& dataSave);

    // Allocate value buffers for the table iterator.
    // Iteration based on records is not supported, so an exception is thrown.
    virtual void allocIterBuf (void*& lastVal, void*& curVal,
			       CountedPtr<BaseCompare>& cmpObj);

    // Free the value buffers allocated by allocIterBuf.
    virtual void freeIterBuf (void*& lastVal, void*& curVal);

    // Create a data manager column object for this column.
    virtual void createDataManagerColumn();


private:
    // Pointer to column description.
    const ScalarRecordColumnDesc* scaDescPtr_p;
    

    // Copy constructor cannot be used.
    ScalarRecordColumnData (const ScalarRecordColumnData&);

    // Assignment cannot be used.
    ScalarRecordColumnData& operator= (const ScalarRecordColumnData&);

    // Write the column data.
    // The control information is written into the given AipsIO object,
    // while the data is written/flushed by the data manager.
    virtual void putFileDerived (AipsIO&);

    // Read the column data back.
    // The control information is read from the given AipsIO object.
    // This is used to bind the column to the appropriate data manager.
    // Thereafter the data manager gets opened.
    virtual void getFileDerived (AipsIO&, const ColumnSet&);

    // Handle getting and putting a record.
    // It is stored as a Vector of uChar.
    // <group>
    void getRecord (uInt rownr, TableRecord& rec) const;
    void putRecord (uInt rownr, const TableRecord& rec);
    // </group>
};



} //# NAMESPACE CASACORE - END

#endif
