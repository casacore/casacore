//# ConcatTable.h: Class to view a concatenation of tables as a single table
//# Copyright (C) 2008
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

#ifndef TABLES_CONCATTABLE_H
#define TABLES_CONCATTABLE_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/BaseTable.h>
#include <casacore/tables/Tables/ConcatRows.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Containers/SimOrdMap.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  //# Forward Declarations
  class TSMOption;
  class ConcatColumn;
  class AipsIO;


  // <summary>
  // Class to view a concatenation of tables as a single table.
  // </summary>

  // <use visibility=local>

  // <reviewed reviewer="UNKNOWN" date="" tests="">
  // </reviewed>

  // <prerequisite>
  //# Classes you should understand before using this one.
  //   <li> BaseTable
  //   <li> ConcatColumn
  // </prerequisite>

  // <etymology>
  // ConcatTable represents the concatenation of one or more tables.
  // </etymology>

  // <synopsis> 
  // ConcatTable is used to virtually concatenate one or more tables.
  // Those tables must have the same description.
  //
  // It acts to the user as a normal table. All gets and puts are
  // handled by ConcatColumn which directs them to the referenced columns
  // while (if needed) converting the given row number to the row number
  // in the referenced tables. For that purpose ConcatTable keeps the
  // number of rows in the referenced tables.
  // <note>Currently it cannot handle changes in the number of rows in the
  // underlying tables. </note>
  //
  // It is possible to specify the keyword names of the subtables that have
  // to be concatenated as well. The other subtables are assumed to be
  // identical for all tables, so only the subtable of the first table is used.
  //
  // The ConcatTable maintains its own keyword set, which is initially a copy
  // of the keyword set of the first table. It replaces the keywords of the
  // subtables to be concatenated.
  // The keyword set is not persistent. One can add or change keywords, but
  // these changes are not kept when the ConcatTable object is made persistent.
  // </synopsis> 

  // <motivation>
  // Sometimes a very large MeasurementSet is split into multiple smaller ones
  // using the time axis. Using ConcatTable they can still b viewed as a
  // single MS. The SYSCAL subtable is split in time as well, thus it has
  // to be possible to concatenate that one as well.
  // <note>An MS split in subband could be concatenated as well provided that
  // at least the first part contains the full SPECTRAL_WINDOW subtable and
  // that unique SPWids are used.
  // </note>
  // </motivation>

  // <todo asof="$DATE:$">
  //# A List of bugs, limitations, extensions or planned refinements.
  //   <li> Maybe not allocating the row number vector for a projection.
  //          This saves space and time, but each rownr conversion will
  //          take a bit more time because it has to test if there is a vector.
  //   <li> Maybe maintain a Vector<String> telling on which columns
  //          the table is ordered. This may speed up selection, but
  //          it is hard to check if the order is changed by a put.
  //   <li> Allow to remove a row or column from the ConcatTable
  //   <li> Allow to rename a column in the ConcatTable
  //   <li> Maybe implement doSort one time for a more efficient sort.
  //          (now everything is handled by BaseTable).
  // </todo>


  class ConcatTable : public BaseTable
  {
  public:

    // Create a virtual table as the concatenation of the given tables.
    // It checks if the table descriptions of the tables are the same.
    // Subtables with the given names will be concatenated as well.
    // It is assumed that the other subtables are the same for all tables,
    // so the ones of the first table are used.
    // <br>The option can be Table::Old or Table::Update.
    // <br>If a non-empty subdirectory name is given, the tables will
    // be moved to that subdirectory when the concatenated table is written
    // (by writeConcatTable).
    // <group>
    ConcatTable (const Block<BaseTable*>& tables,
		 const Block<String>& subTables,
                 const String& subDirName);
    ConcatTable (const Block<String>& tableNames,
		 const Block<String>& subTables,
                 const String& subDirName,
		 int option,
		 const TableLock& lockOptions,
                 const TSMOption& tsmOption);
    // </group>

    // Create a concat table out of a file (written by writeConcatTable).
    // The referenced tables will also be opened (if not stored in the cache).
    ConcatTable (AipsIO&, const String& name, uInt nrrow, int option,
		 const TableLock& lockOptions, const TSMOption& tsmOption);

    // The destructor flushes (i.e. writes) the table if it is opened
    // for output and not marked for delete.
    virtual ~ConcatTable();

    // Get the names of the tables this table consists of.
    virtual void getPartNames (Block<String>& names, Bool recursive) const;

    // Return the layout of a table (i.e. description and #rows).
    // This function has the advantage that only the minimal amount of
    // information required is read from the table, thus it is much
    // faster than a normal table open.
    // <br> The number of rows is returned. The description of the table
    // is stored in desc (its contents will be overwritten).
    static void getLayout (TableDesc& desc, AipsIO& ios);

    // Try to reopen the table (the underlying ones) for read/write access.
    // An exception is thrown if the table is not writable.
    // Nothing is done if the table is already open for read/write.
    virtual void reopenRW();

    // Is the table stored in big or little endian format?
    // It returns the endianness of the first underlying table.
    virtual Bool asBigEndian() const;

    // Get the storage option used for the table.
    // It returns the storage option of the first underlying table.
    virtual const StorageOption& storageOption() const;

    // Is the table in use (i.e. open) in another process?
    // It always returns False.
    virtual Bool isMultiUsed (Bool checkSubTable) const;

    // Get the locking info.
    // All underlying tables have the same lock option.
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

    // Resync the Table object with the table files.
    virtual void resync();

    // Get the modify counter.
    virtual uInt getModifyCounter() const;

    // Test if all underlying tables are opened as writable.
    virtual Bool isWritable() const;

    // Read a concat table from a file.
    // The underlying tables will be opened (if not stored in the cache).
    void getConcat (AipsIO&, int option, const TableLock& lockOptions,
                    const TSMOption& tsmOption);

    // This is doing a shallow copy.
    // It gives an error if the ConcatTable has not been stored yet.
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

    // Get the data manager info (of the first underlying table).
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

    // Test if it is possible to remove a row from this table (no).
    virtual Bool canRemoveRow() const;

    // Remove the given row.
    virtual void removeRow (uInt rownr);

    // Test if columns can be removed (no).
    virtual Bool canRemoveColumn (const Vector<String>& columnNames) const;

    // Add one or more columns to the table.
    // The column is added to the parent tables if told so and if not existing.
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

    // Remove a column.
    virtual void removeColumn (const Vector<String>& columnNames);

    // Test if a column can be renamed (no).
    virtual Bool canRenameColumn (const String& columnName) const;

    // Rename a column.
    virtual void renameColumn (const String& newName, const String& oldName);

    // Rename a hypercolumn.
    virtual void renameHypercolumn (const String& newName,
				    const String& oldName);

    // Find the data manager with the given name or for the given column.
    virtual DataManager* findDataManager (const String& name,
                                          Bool byColumn) const;

    // Get the rows object.
    const ConcatRows& rows() const
      { return rows_p; }

    // Get the column objects in the referenced tables.
    Block<BaseColumn*> getRefColumns (const String& columnName);

    // Create a (temporary) Table object from it.
    Table asTable()
      { return Table (this, False); }

  private:
    // Copy constructor is forbidden, because copying a table requires
    // some more knowledge (like table name of result).
    // Declaring it private, makes it unusable.
    ConcatTable (const ConcatTable&);

    // Assignment is forbidden, because copying a table requires
    // some more knowledge (like table name of result).
    // Declaring it private, makes it unusable.
    ConcatTable& operator= (const ConcatTable&);

    // Show the extra table structure info (names of used tables).
    void showStructureExtra (std::ostream&) const;

    // Open all tables in the required way.
    void openTables (const Block<String>& tableNames, int option,
		     const TableLock& lockOptions, const TSMOption& tsmOption);

    // Initialize.
    // It checks if the descriptions of all tables are equal.
    // It creates the keyword setfor which it concatenates subtables as needed.
    void initialize();

    // Setup the main parts of the object.
    // <br>First create the name map (mapping column name in ConcatTable to
    // the column in the original table).
    // If the BaseTable is a ConcatTable, use its name map.
    // Otherwise create the initial name map from the table description.
    // A rename might change the map.
    // <br>Create the ConcatColumn objects.
    // <br>Create the initial TableInfo as a copy of the original BaseTable.
    void setup (BaseTable* btp, const Vector<String>& columnNames);

    // Add lines containing the concatenated tables to the info.
    void addInfo();

    // Create the ConcatColumn objects for all columns in the description.
    void makeConcatCol();

    // Handle the subtales that have to be concatenated.
    void handleSubTables();

    // Write a reference table.
    void writeConcatTable (Bool fsync);

    // Check if the column can be added, thus does not exist yet.
    void checkAddColumn (const String& name, Bool addToParent);

    //# Data members
    Block<String>     subTableNames_p;
    String            subDirName_p;
    Block<BaseTable*> baseTabPtr_p;        //# pointers to parent tables
    SimpleOrderedMap<String,ConcatColumn*> colMap_p; //# map name to column
    TableRecord       keywordSet_p;
    Bool              changed_p;           //# True = changed since last write
    ConcatRows        rows_p;
  };


} //# NAMESPACE CASACORE - END

#endif
