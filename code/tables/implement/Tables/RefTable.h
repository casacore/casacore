//# RefTable.h: Class for a table as a view of another table
//# Copyright (C) 1994,1995,1996,1997,1998
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

#if !defined(AIPS_REFTABLE_H)
#define AIPS_REFTABLE_H


//# Includes
#include <aips/aips.h>
#include <aips/Tables/BaseTable.h>
#include <aips/Utilities/String.h>
#include <aips/Arrays/Vector.h>
#include <aips/Containers/SimOrdMap.h>

//# Forward Declarations
class RefColumn;
class AipsIO;


// <summary>
// Class for a table as a view of another table
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> BaseTable
//   <li> RefColumn
// </prerequisite>

// <etymology>
// RefTable represents a table which is a view on another table,
// thus which references another table.
// </etymology>

// <synopsis> 
// RefTable is used to make a view on another table.
// Usually it is a view on a subset of the table, either in vertical
// or horizontal direction. Thus a subset of rows and/or columns.
// It will be the result of a select, sort, project or iterate function.
//
// It acts to the user as a normal table. All gets and puts are
// handled by RefColumn which directs them to the referenced column
// while (if needed) converting the given row number to the row number
// in the referenced table. For that purpose RefTable maintains a
// Vector of the row numbers in the referenced table.
//
// The RefTable constructor acts in a way that it will always reference
// the original table. This means that if a select is done on a RefTable,
// the resulting RefTable will also reference the original PlainTable.
// This is done to avoid long chains of RefTables.
// However, if ever some other kind of table views are introduced
// (like a join or a concatenation of similar tables), this cannot be
// used anymore. Most software already anticipates on that. The only
// exception is the code anding, oring tables (refAnd, etc.).
// </synopsis> 

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//   <li> Maybe not allocating the row number vector for a projection.
//          This saves space and time, but each rownr conversion will
//          take a bit more time because it has to test if there is a vector.
//   <li> Maybe maintain a Vector<String> telling on which columns
//          the table is ordered. This may speed up selection, but
//          it is hard to check if the order is changed by a put.
//   <li> Allow to remove a row or column from the RefTable
//   <li> Allow to rename a column in the RefTable
//   <li> Maybe implement doSort one time for a more efficient sort.
//          (now everything is handled by BaseTable).
// </todo>


class RefTable : public BaseTable
{
public:

    // Create a reference table object referencing the
    // given BaseTable object.
    // If the BaseTable is actually another RefTable, it will reference
    // its referenced table (thus the original table) and it will
    // take its vector of row numbers and projected column names
    // into account. Thus if a select is done on a projected table,
    // the resulting RefTable will have the same projection.
    // <group>
    // Construct a RefTable with an empty row number vector.
    // rowOrder=True indicates that the order of the rows will not
    // be disturbed (as will be the case for a sort).
    // A row number vector of the given size is initially allocated.
    // Later this RefTable will be filled in by the select, etc..
    RefTable (BaseTable*, Bool rowOrder, uInt initialNrrow);

    // A RefTable with the given row numbers is constructed.
    RefTable (BaseTable*, const Vector<uInt>& rowNumbers);

    // Create a reference table object out of a mask.
    // The row number vector will consist of the rows for which the
    // mask has a True value.
    // The length of the mask must be the number of rows in the BaseTable.
    RefTable (BaseTable*, const Vector<Bool>& rowMask);

    // Create a reference table object via projection (i.e. column selection).
    // The row number vector is a copy of the given table.
    RefTable (BaseTable*, const Vector<String>& columnNames);
    // </group>

    // Create a reference table out of a file (written by writeRefTable).
    // The referenced table will also be created (if not stored in the cache).
    RefTable (AipsIO&, const String& name, uInt nrrow, int option,
	      const TableLock& lockOptions);

    // The destructor flushes (i.e. writes) the table if it is opened
    // for output and not marked for delete.
    ~RefTable();

    // Return the layout of a table (i.e. description and #rows).
    // This function has the advantage that only the minimal amount of
    // information required is read from the table, thus it is much
    // faster than a normal table open.
    // <br> The number of rows is returned. The description of the table
    // is stored in desc (its contents will be overwritten).
    static void getLayout (TableDesc& desc, AipsIO& ios);

    // Try to reopen the table (the underlying one) for read/write access.
    // An exception is thrown if the table is not writable.
    // Nothing is done if the table is already open for read/write.
    virtual void reopenRW();

    // Is the table in use (i.e. open) in another process?
    virtual Bool isMultiUsed() const;

    // Get the locking info.
    virtual const TableLock& lockOptions() const;

    // Merge the given lock info with the existing one.
    virtual void mergeLock (const TableLock& lockOptions);

    // Has this process the read or write lock, thus can the table
    // be read or written safely?
    virtual Bool hasLock (FileLocker::LockType) const;

    // Try to lock the table for read or write access.
    virtual Bool lock (FileLocker::LockType, uInt nattempts);

    // Unlock the table. This will also synchronize the table data,
    // thus force the data to be written to disk.
    virtual void unlock();

    // Flush the table, i.e. write it to disk.
    // Nothing will be done if the table is not writable.
    // A flush can be executed at any time.
    // When a table is marked for delete, the destructor will remove
    // files written by intermediate flushes.
    // Note that if necessary the destructor will do an implicit flush,
    // unless it is executed due to an exception.
    virtual void flush (Bool sync);

    // Get the modify counter.
    virtual uInt getModifyCounter() const;

    // Test if the parent table is opended as writable.
    Bool isWritable() const;

    // Read a reference table from a file.
    // The referenced table will also be created (if not stored in the cache).
    void getRef (AipsIO&, int option, const TableLock& lockOptions);

    // Get readonly access to the table keyword set.
    TableRecord& keywordSet();

    // Get read/write access to the table keyword set.
    // This requires that the table is locked (or it gets locked
    // when using AutoLocking mode).
    TableRecord& rwKeywordSet();

    // Get a column object using its index.
    BaseColumn* getColumn (uInt columnIndex) const;

    // Get a column object using its name.
    BaseColumn* getColumn (const String& columnName) const;

    // Test if it is possible to remove a row from this table.
    Bool canRemoveRow() const;

    // Remove the given row.
    void removeRow (uInt rownr);

    // Test if a column can be removed (yes).
    Bool canRemoveColumn (const String& columnName) const;

    // Remove a column.
    void removeColumn (const String& columnName);

    // Test if a column can be renamed (yes).
    Bool canRenameColumn() const;

    // Rename a column.
    void renameColumn (const String& newName, const String& oldName);

    // Find the data manager with the given name.
    DataManager* findDataManager (const String& dataManagerName) const;

    // Get a vector of row numbers.
    Vector<uInt> rowNumbers() const;

    // Get parent of this table.
    BaseTable* root();

    // Get rownr in root table.
    // This converts the given row number to the row number in the root table.
    uInt rootRownr (uInt rownr) const;

    // Get vector of rownrs in root table.
    // This converts the given row numbers to row numbers in the root table.
    Vector<uInt> rootRownr (const Vector<uInt>& rownrs) const;

    // Tell if the table is in row order.
    Bool rowOrder() const;

    // Get row number vector.
    // This is used by the BaseTable logic and sort routines.
    Vector<uInt>* rowStorage();

    // Add a rownr to reference table.
    void addRownr (uInt rownr);

    // Set the exact number of rows in the table.
    // An exception is thrown if more than current nrrow.
    void setNrrow (uInt nrrow);

    // Adjust the row numbers to be the actual row numbers in the
    // root table. This is, for instance, used when a RefTable is sorted.
    // Optionally it also determines if the resulting rows are in row order.
    Bool adjustRownrs (uInt nrrow, Vector<uInt>& rownrs,
		       Bool determineOrder) const;

    // And, or, subtract or xor the row numbers of 2 tables.
    void refAnd (uInt nr1, const uInt* rows1, uInt nr2, const uInt* rows2);
    void refOr  (uInt nr1, const uInt* rows1, uInt nr2, const uInt* rows2);
    void refSub (uInt nr1, const uInt* rows1, uInt nr2, const uInt* rows2);
    void refXor (uInt nr1, const uInt* rows1, uInt nr2, const uInt* rows2);
    void refNot (uInt nr1, const uInt* rows1, uInt nrmain);

    // Get the internal pointer in a rowStorage vector.
    // It checks whether no copy is made of the data.
    static uInt* getStorage (Vector<uInt>& rownrs);

private:
    BaseTable*   baseTabPtr_p;                 //# pointer to parent table
    Bool         rowOrd_p;                     //# True = table is in row order
    Vector<uInt> rowStorage_p;                 //# row numbers in parent table
    uInt*        rows_p;                       //# Pointer to rowStorage_p
    SimpleOrderedMap<String,String> nameMap_p; //# map to column name in parent
    SimpleOrderedMap<String,RefColumn*> colMap_p; //# map name to column
    Bool         changed_p;                 //# True = changed since last write

    // Copy constructor is forbidden, because copying a table requires
    // some more knowledge (like table name of result).
    // Declaring it private, makes it unusable.
    RefTable (const RefTable&);

    // Assignment is forbidden, because copying a table requires
    // some more knowledge (like table name of result).
    // Declaring it private, makes it unusable.
    RefTable& operator= (const RefTable&);

    static void makeDesc (TableDesc& desc, const TableDesc& rootDesc,
			  SimpleOrderedMap<String,String>& nameMap);

    // Setup the main parts of the object.
    // <br>Create the initial name map from the table description.
    // This map maps a name to the name in the original table.
    // A rename might change the map.
    // <br>Create the RefColumn objects.
    // <br>Create the initial TableInfo as a copy of the original BaseTable.
    void setup (BaseTable* btp);

    // Create the RefColumn objects for all columns in the description.
    void makeRefCol();

    // Write a reference table.
    void writeRefTable (Bool sync);
};



inline uInt RefTable::rootRownr (uInt rnr) const
    { return rows_p[rnr]; }



#endif
