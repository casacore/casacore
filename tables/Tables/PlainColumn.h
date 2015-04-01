//# PlainColumn.h: Base class for a column in a plain table
//# Copyright (C) 1994,1995,1996,1997,1998,1999,2000,2001
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

#ifndef TABLES_PLAINCOLUMN_H
#define TABLES_PLAINCOLUMN_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/BaseColumn.h>
#include <casacore/tables/Tables/ColumnSet.h>
#include <casacore/tables/Tables/TableRecord.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class TableAttr;
class BaseColumnDesc;
class DataManager;
class DataManagerColumn;
class AipsIO;
template<class T> class Array;
class IPosition;


// <summary>
// Base class for a column in a plain table
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> BaseColumn
//   <li> PlainTable
// </prerequisite>

// <etymology>
// PlainColumn represents any column in a plain table.
// A plain table is a regular table, i.e. not a table like a
// RefTable which is a view on a plain table.
// </etymology>

// <synopsis> 
// Abstract base class for all types of columns in a plain table.
// It implements the common functionality for all columns in a plain
// table. Furthermore it defines some virtual functions (on top of
// the virtual functions defined in BaseColumn) which are specific for
// plain columns.
// </synopsis> 

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
// </todo>


class PlainColumn : public BaseColumn
{
public:

    PlainColumn (const BaseColumnDesc*, ColumnSet*);

    virtual ~PlainColumn();

    // Test if the column is in principle writable.
    // This does not test if the table itself is writable.
    // That has to be done by the caller.
    virtual Bool isWritable() const;

    // Test if the column is stored (otherwise it is virtual).
    virtual Bool isStored() const;

    // Get access to the column keyword set.
    // <group>
    TableRecord& rwKeywordSet();
    TableRecord& keywordSet();
    // </group>

    // Get nr of rows in the column.
    uInt nrow() const;

    // Define the shape of all arrays in the column.
    virtual void setShapeColumn (const IPosition& shape);

    // Test if the column is bound to a storage manager or
    // virtual column engine.
    virtual Bool isBound() const;

    // Bind the column to a data manager.
    virtual void bind (DataManager*);

    // Create a data manager column for a filled column.
    virtual void createDataManagerColumn() = 0;

    // Get the pointer to the data manager.
    DataManager* dataManager() const;

    // Get the pointer to the data manager column.
    DataManagerColumn*& dataManagerColumn();

    // Get a pointer to the underlying column cache.
    virtual ColumnCache& columnCache();

    // Set the maximum cache size (in bytes) to be used by a storage manager.
    virtual void setMaximumCacheSize (uInt nbytes);

    // Write the column.
    void putFile (AipsIO&, const TableAttr&);

    // Read the column.
    void getFile (AipsIO&, const ColumnSet&, const TableAttr&);

protected:
    DataManager*        dataManPtr_p;    //# Pointer to data manager.
    DataManagerColumn*  dataColPtr_p;    //# Pointer to column in data manager.
    ColumnSet*          colSetPtr_p;
    String              originalName_p;  //# Column name before any rename
    Bool                rtraceColumn_p;  //# trace reads of the column?
    Bool                wtraceColumn_p;  //# trace writes of the column?

    // Get the trace-id of the table.
    int traceId() const
      { return colSetPtr_p->traceId(); }

    // Write the column.
    // The control information is written into the given AipsIO object,
    // while the data is written by the storage manager.
    virtual void putFileDerived (AipsIO&) = 0;

    // Read the column back.
    // The control information is read from the given AipsIO object.
    // This is used to bind the column to the appropriate data manager.
    virtual void getFileDerived (AipsIO&, const ColumnSet&) = 0;

    // Check the length of a value.
    // This a meant for String values for which a maximum length is defined.
    // The void* version is a no-op for other values.
    // <group>
    void checkValueLength (const void*) const;
    void checkValueLength (const String* value) const;
    void checkValueLength (const Array<String>* value) const;
    // </group>
    
    // Lock the table before reading or writing.
    // If manual or permanent locking is in effect, it checks if
    // the table is locked.
    // <group>
    void checkReadLock (Bool wait) const;
    void checkWriteLock (Bool wait) const;
    // </group>

    // Inspect the auto lock when the inspection interval has expired and
    // release it when another process needs the lock.
    void autoReleaseLock() const;
};


inline DataManager* PlainColumn::dataManager() const
    { return dataManPtr_p; }
inline DataManagerColumn*& PlainColumn::dataManagerColumn()
    { return dataColPtr_p; }

inline void PlainColumn::checkValueLength (const void*) const
    {}

inline void PlainColumn::checkReadLock (Bool wait) const
    { colSetPtr_p->checkReadLock (wait); }
inline void PlainColumn::checkWriteLock (Bool wait) const
    { colSetPtr_p->checkWriteLock (wait); }
inline void PlainColumn::autoReleaseLock() const
    { colSetPtr_p->autoReleaseLock(); }



} //# NAMESPACE CASACORE - END

#endif
