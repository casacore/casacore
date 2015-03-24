//# RefTable.h: Class for a table as a view of another table
//# Copyright (C) 1994,1995,1996,1997,1998,1999,2000,2001,2002,2003
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

#ifndef TABLES_REFTABLE_H
#define TABLES_REFTABLE_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/BaseTable.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Containers/SimOrdMap.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class TSMOption;
class RefColumn;
class AipsIO;


// <summary>
// Class for a table as a view of another table
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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
	      const TableLock& lockOptions, const TSMOption& tsmOption);

    // The destructor flushes (i.e. writes) the table if it is opened
    // for output and not marked for delete.
    virtual ~RefTable();

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

    // Is the table stored in big or little endian format?
    virtual Bool asBigEndian() const;

    // Get the storage option used for the table.
    virtual const StorageOption& storageOption() const;

    // Is the table in use (i.e. open) in another process?
    // It always returns False.
    virtual Bool isMultiUsed (Bool checkSubTable) const;

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
    virtual void flush (Bool fsync, Bool recursive);

    // Resync the Table object with the table file.
    virtual void resync();

    // Get the modify counter.
    virtual uInt getModifyCounter() const;

    // Test if the parent table is opened as writable.
    virtual Bool isWritable() const;

    // Read a reference table from a file.
    // The referenced table will also be created (if not stored in the cache).
    void getRef (AipsIO&, int option, const TableLock& lockOptions,
                 const TSMOption& tsmOption);

    // This is doing a shallow copy.
    // It gives an error if the RefTable has not been stored yet.
    virtual void copy (const String& newName, int tableOption) const;

    // Copy the table and all its subtables.
    // It copies the contents of each row to get a real copy.
    virtual void deepCopy (const String& newName,
			   const Record& dataManagerInfo,
			   int tableOption, Bool, int endianFormat,
			   Bool noRows) const;

    // It returns the type of the parent table.
    virtual int tableType() const;

    // Get the actual table description.
    virtual TableDesc actualTableDesc() const;

    // Get the data manager info.
    virtual Record dataManagerInfo() const;

    // Get readonly access to the table keyword set.
    virtual TableRecord& keywordSet();

    // Get read/write access to the table keyword set.
    // This requires that the table is locked (or it gets locked
    // when using AutoLocking mode).
    virtual TableRecord& rwKeywordSet();

    // Get a column object using its index.
    virtual BaseColumn* getColumn (uInt columnIndex) const;

    // Get a column object using its name.
    virtual BaseColumn* getColumn (const String& columnName) const;

    // Test if it is possible to remove a row from this table.
    virtual Bool canRemoveRow() const;

    // Remove the given row.
    virtual void removeRow (uInt rownr);

    // Add one or more columns to the table.
    // The column is added to the parent table if told so and if not existing.
    // <group>
    virtual void addColumn (const ColumnDesc& columnDesc,
                            Bool addToParent);
    virtual void addColumn (const ColumnDesc& columnDesc,
			    const String& dataManager, Bool byName,
                            Bool addToParent);
    virtual void addColumn (const ColumnDesc& columnDesc,
			    const DataManager& dataManager,
                            Bool addToParent);
    virtual void addColumn (const TableDesc& tableDesc,
			    const DataManager& dataManager,
                            Bool addToParent);
    // </group>

    // Test if columns can be removed (yes).
    virtual Bool canRemoveColumn (const Vector<String>& columnNames) const;

    // Remove columns.
    virtual void removeColumn (const Vector<String>& columnNames);

    // Test if a column can be renamed (yes).
    virtual Bool canRenameColumn (const String& columnName) const;

    // Rename a column.
    virtual void renameColumn (const String& newName, const String& oldName);

    // Rename a hypercolumn.
    virtual void renameHypercolumn (const String& newName,
				    const String& oldName);

    // Find the data manager with the given name or for the given column.
    virtual DataManager* findDataManager (const String& name,
                                          Bool byColumn) const;

    // Get a vector of row numbers.
    virtual Vector<uInt> rowNumbers() const;

    // Get parent of this table.
    virtual BaseTable* root();

    // Get rownr in root table.
    // This converts the given row number to the row number in the root table.
    uInt rootRownr (uInt rownr) const;

    // Get vector of rownrs in root table.
    // This converts the given row numbers to row numbers in the root table.
    Vector<uInt> rootRownr (const Vector<uInt>& rownrs) const;

    // Tell if the table is in row order.
    virtual Bool rowOrder() const;

    // Get row number vector.
    // This is used by the BaseTable logic and sort routines.
    virtual Vector<uInt>* rowStorage();

    // Add a rownr to reference table.
    void addRownr (uInt rownr);

    // Set the exact number of rows in the table.
    // An exception is thrown if more than current nrrow.
    void setNrrow (uInt nrrow);

    // Adjust the row numbers to be the actual row numbers in the
    // root table. This is, for instance, used when a RefTable is sorted.
    // Optionally it also determines if the resulting rows are in row order.
    virtual Bool adjustRownrs (uInt nrrow, Vector<uInt>& rownrs,
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

    // Get the names of the tables this table consists of.
    virtual void getPartNames (Block<String>& names, Bool recursive) const;

    // Show the extra table structure info (name of root table).
    void showStructureExtra (std::ostream&) const;

    // Make a table description for the given columns.
    static void makeDesc (TableDesc& desc, const TableDesc& rootDesc,
			  SimpleOrderedMap<String,String>& nameMap,
			  Vector<String>& names);

    // Setup the main parts of the object.
    // <br>First create the name map (mapping column name in RefTable to
    // the column in the original table).
    // If the BaseTable is a RefTable, use its name map.
    // Otherwise create the initial name map from the table description.
    // A rename might change the map.
    // <br>Create the RefColumn objects.
    // <br>Create the initial TableInfo as a copy of the original BaseTable.
    void setup (BaseTable* btp, const Vector<String>& columnNames);

    // Create the RefColumn objects for all columns in the description.
    void makeRefCol();

    // Write a reference table.
    void writeRefTable (Bool fsync);

    // Copy a RefTable that is not persistent. It requires some special logic.
    void copyRefTable (const String& newName, int tableOption);

    // Check if a column can be added. Return True if it can and must be
    // added to the parent table first.
    Bool checkAddColumn (const String& name, Bool addToParent);

    // Add a column.
    void addRefCol (const ColumnDesc& cd);
    // Add multiple columns.
    void addRefCol (const TableDesc& tdesc);
};



inline uInt RefTable::rootRownr (uInt rnr) const
    { return rows_p[rnr]; }




} //# NAMESPACE CASACORE - END

#endif
