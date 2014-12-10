//# SetupNewTab.h: Create a new table - define shapes, data managers, etc.
//# Copyright (C) 1994,1995,1996,1999,2001,2002,2003
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

#ifndef TABLES_SETUPNEWTAB_H
#define TABLES_SETUPNEWTAB_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/StorageOption.h>
#include <casacore/casa/Containers/SimOrdMap.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class TableDesc;
class ColumnSet;
class VirtualColumnEngine;
class DataManager;
class IPosition;


// <summary>
// Representation for handle class SetupNewTable
// </summary>

// <use visibility=local>

// <reviewed reviewer="bglenden" date="12AUG94" tests="None">
// </reviewed>

// <prerequisite>
//   <li> TableDesc and related classes like ArrayColumnDesc
//   <li> DataManager
//   <li> Table
// </prerequisite>

// <etymology>
// SetupNewTableRep is the representation of class SetupNewTable.
// </etymology>

// <synopsis> 
// SetupNewTableRep is the representation of class
// <linkto class="SetupNewTable:description">SetupNewTable</linkto>.
// Its functionality is described there.
// </synopsis>

// <motivation>
// Copying a SetupNewTable object as such is very difficult, if not
// impossible. However, being able to use a SetupNewTable copy constructor
// was required to be able to have (static) functions constructing a
// SetupNewTable object and return it by value (as done for example
// by <src>ForwardColumn::setupNewTable</src>).
// Therefore SetupNewTable is implemented using the handle idiom.
// SetupNewTable is the interface (i.e. the handle) for the user,
// while underneath SetupNewTableRep is doing all the work.
// The SetupNewTable copy constructor can simply copy yhe pointer
// to the underlying SetupNewTableRep object.
// </motivation>

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//   <li> full implementation of tiling
// </todo>


class SetupNewTableRep
{
public:
    // Create a new table using the table description with the given name.
    // The description will be read from a file.
    SetupNewTableRep (const String& tableName, const String& tableDescName,
		      Table::TableOption, const StorageOption&);

    // Create a new table using the given table description.
    SetupNewTableRep (const String& tableName, const TableDesc&,
		      Table::TableOption, const StorageOption&);

    ~SetupNewTableRep();

    // Get access to the reference count.
    uInt& count()
	{ return count_p; }

    // Get the name of the table.
    const String& name() const
	{ return tabName_p; }

    // Get the table create option.
    int option() const
	{ return option_p; }

    // Get the storage option.
    const StorageOption& storageOption() const
        { return storageOpt_p; }

    // Test if the table is marked for delete.
    Bool isMarkedForDelete() const
	{ return delete_p; }

    // Get the table description.
    const TableDesc& tableDesc() const
	{ return *tdescPtr_p; }

    // Bind a column to the given data manager.
    // If already bound, the binding will be overwritten.
    // It cannot be used anymore once the SetupNewTableRep object is used to
    // construct a Table object.
    void bindColumn (const String& columnName, const DataManager&);

    // Bind a column to the given data manager of the other column.
    // If the other column is not bound, nothing will be done.
    // If columnName is already bound, the binding will be overwritten.
    // It cannot be used anymore once the SetupNewTableRep object is used to
    // construct a Table object.
    void bindColumn (const String& columnName, const String& otherColumn);

    // Bind a group of columns to the given data manager.
    // The flag rebind tells if the binding of an already bound column
    // will be overwritten.
    // It cannot be used anymore once the SetupNewTableRep object is used to
    // construct a Table object.
    void bindGroup (const String& columnGroup, const DataManager&,
		    Bool rebind=False);

    // Bind all columns to the given data manager.
    // The flag rebind tells if the binding of an already bound column
    // will be overwritten.
    // It cannot be used anymore once the SetupNewTableRep object is used to
    // construct a Table object.
    void bindAll (const DataManager&, Bool rebind=False);

    // Create data managers and bind the columns using the specifications
    // in the given record (which is obtained using Table::dataManagerInfo()).
    void bindCreate (const Record& spec);

    // Define the shape of fixed shaped arrays in a column.
    // The shape of those arrays has to be known before the table
    // can be constructed. It has to be defined via this function,
    // if it was not already defined in the column description.
    // If only the dimensionality was defined in the column
    // description, the shape's dimensionality must match it.
    // Calling this function for an non-fixed shaped array results in
    // an exception.
    // It cannot be used anymore once the SetupNewTableRep object is used to
    // construct a Table object.
    void setShapeColumn (const String& columnName, const IPosition& shape);

    // Test if object is already in use.
    Bool isUsed() const
	{ return (colSetPtr_p == 0  ?  True : False); }

    // Get pointer to column set.
    // This function is used by PlainTable.
    ColumnSet* columnSetPtr()
	{ return colSetPtr_p; }

    // Get pointer to table description.
    // This function is used by PlainTable.
    TableDesc* tableDescPtr()
	{ return tdescPtr_p; }

    // Set object to in use by a (Plain)Table object.
    // This function is used by PlainTable.
    void setInUse()
	{ colSetPtr_p = 0; }

    // Make a data manager for all unbound columns.
    void handleUnbound();

private:
    // Reference count.
    uInt          count_p;
    // Table name.
    String        tabName_p;
    // Constructor options.
    int           option_p;
    StorageOption storageOpt_p;
    // Marked for delete?
    Bool          delete_p;
    TableDesc*    tdescPtr_p;
    ColumnSet*    colSetPtr_p;      //# 0 = object is already used by a Table
    SimpleOrderedMap<void*,void*> dataManMap_p;

    // Copy constructor is forbidden, because copying a table requires
    // some more knowledge (like table name of result).
    // Declaring it private, makes it unusable.
    SetupNewTableRep (const SetupNewTableRep&);

    // Assignment is forbidden, because copying a table requires
    // some more knowledge (like table name of result).
    // Declaring it private, makes it unusable.
    SetupNewTableRep& operator= (const SetupNewTableRep&);

    // Setup the new table.
    // This checks various things and creates the set of columns.
    void setup();

    // Get the internal data manager object for the given data manager.
    // If it does not exist yet, it will be cloned and stored internally.
    DataManager* getDataManager (const DataManager& dataMan);
};





// <summary>
// Create a new table - define shapes, data managers, etc.
// </summary>

// <use visibility=export>

// <reviewed reviewer="bglenden" date="12AUG94" tests="None">
// </reviewed>

// <prerequisite>
//   <li> TableDesc and related classes like ArrayColumnDesc
//   <li> DataManager
//   <li> Table
// </prerequisite>

// <etymology>
// SetupNewTable is a class to setup a new table.
// </etymology>

// <synopsis> 
// Constructing a new table is a two stage process.
// First a SetupNewTable object has to be created. Thereafter its columns
// have to be bound defining how they have to be stored or calculated.
// Columns have to be bound to a data manager (e.g. a storage manager
// or a virtual column engine)..
// Once the required columns are bound, the actual Table object can
// be created. At this stage, still unbound columns will be bound
// to the default data managers.
// The Table object can be used to write data, etc.
// 
// The construct options for SetupNewTable are defined in class Table.
// The possible options are:
// <ul>
//   <li> New
//      creates a new table file.
//      The Table destructor will write the table into the file.
//   <li> NewNoReplace
//      as option New, but an exception will be thrown if the table
//      file already exists.
//   <li> Scratch
//      creates a temporary table.
//      It will be lost when the Table object gets destructed.
// </ul>
// More information is provided in the Tables module documentation.
// </synopsis> 
//
// <example>
// <srcblock>
//      Table makeIt(const TableDesc &td) {                            // 1
//            SetupNewTable maker("test.table", td, Table::New);       // 2
//            maker.setShapeColumn("SomeArray", IPosition(2,10,10));   // 3
//            maker.setShapeColumn("AnotherArray", IPosition(1,100));  // 4
//            StManAipsIO sm1;                                         // 5
//            StManKarma  sm2;                                         // 6
//            maker.bindAll(sm1);                                      // 7
//            maker.bindColumn("SomeCol", sm2);                        // 8
//            maker.bindColumn("AnotherCol", sm2);                     // 9
//            return Table(maker, 1000); // 1000 row table             // 10
//      }                                                              // 11
// </srcblock>
// This code illustrates a simple function that creates a Table starting
// from a Table descriptor. I
// <ol>
//    <li> Declare the function makeIt which, given a TableDesc, returns
//           a table.
//    <li> Create the SetupNewTable object "maker". We want the new table
//           to be named "test.table", its rows columns and keywords come
//           from the TableDesc "td", and this table is to be created
//           unconditionally, that is, it will overwrite an existing table
//           of the same name. Alternative options are given in the synopsis.
//    <li>
//    <li> Give direct arrays declared in the table descriptor (but not
//           necessarily given a shape) a defined shape; 10x10 for the first
//           array, 100 long vector for the second. If all direct arrays 
//           do not have a shape, an error will occur when the table is
//           actually constructed.
//    <li>
//    <li> Declare two data (storage) managers. AipsIO keeps a whole column
//           in memory, Karma does I/O to keep a subsection in memory at once.
//           A powerful feature of Casacore tables is that different columns
//           may be bound to different data managers, which have different
//           properties.
//    <li> Define the default data manager. AipsIO in this case.
//           Note that this statement and statement 5 are actually not
//           needed. When the Table constructor finds some unbound columns,
//           it will construct the default data manager for them and
//           bind them. A default data manager can be defined in the
//           column description and defaults to AipsIO.
//    <li>
//    <li> Override the default for some particular columns.
//    <li> Create and return a 1000 row table. With the Karma storage manager
//           the table size must be defined at construction since new rows
//           can't be added or deleted. If AipsIO was the only storage manager,
//           the size wouldn't need to be defined since rows can be added with
//           AipsIO.
// </ol>
// </example>

// <motivation>
// In principle, SetupNewTab isn't necessary as what we are doing is logically
// just constructing a Table, so it could be done in the Table constructor.
// However such a process can be an involved one - binding multiple data
// managers and filling in the shapes of direct arrays - so separating
// the process makes it much clearer what is going on.
// </motivation>

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//   <li> full implementation of tiling
// </todo>


class SetupNewTable
{
friend class PlainTable;
friend class MemoryTable;

public:
    // Create a new table using the table description with the given name.
    // The description will be read from a file.
    SetupNewTable (const String& tableName, const String& tableDescName,
		   Table::TableOption, const StorageOption& = StorageOption());

    // Create a new table using the given table description.
    SetupNewTable (const String& tableName, const TableDesc&,
		   Table::TableOption, const StorageOption& = StorageOption());

    // Copy constructor (reference semantics).
    SetupNewTable (const SetupNewTable&);

    ~SetupNewTable();

    // Assignment (reference semantics).
    SetupNewTable& operator= (const SetupNewTable&);

    // Get the name of the table.
    const String& name() const
	{ return newTable_p->name(); }

    // Get the table create option.
    int option() const
	{ return newTable_p->option(); }

    // Get the storage option.
    const StorageOption& storageOption() const
        { return newTable_p->storageOption(); }

    // Test if the table is marked for delete.
    Bool isMarkedForDelete() const
	{ return newTable_p->isMarkedForDelete(); }

    // Get the table description.
    const TableDesc& tableDesc() const
	{ return newTable_p->tableDesc(); }

    // Adjust the hypercolumn definitions.
    // It renames and/or removes columns as necessary.
    void adjustHypercolumns (const SimpleOrderedMap<String, String>& old2new,
			     Bool keepUnknown)
      { newTable_p->tableDescPtr()->adjustHypercolumns(old2new,keepUnknown); }

    // Bind a column to the given data manager.
    // If already bound, the binding will be overwritten.
    // It cannot be used anymore once the SetupNewTable object is used to
    // construct a Table object.
    void bindColumn (const String& columnName, const DataManager& dm)
	{ newTable_p->bindColumn (columnName, dm); }

    // Bind a column to the given data manager of the other column.
    // If the other column is not bound, nothing will be done.
    // If columnName is already bound, the binding will be overwritten.
    // It cannot be used anymore once the SetupNewTableRep object is used to
    // construct a Table object.
    void bindColumn (const String& columnName, const String& otherColumn)
	{ newTable_p->bindColumn (columnName, otherColumn); }

    // Bind a group of columns to the given data manager.
    // The flag rebind tells if the binding of an already bound column
    // will be overwritten.
    // It cannot be used anymore once the SetupNewTable object is used to
    // construct a Table object.
    void bindGroup (const String& columnGroup, const DataManager& dm,
		    Bool rebind=False)
	{ newTable_p->bindGroup (columnGroup, dm, rebind); }

    // Bind all columns to the given data manager.
    // The flag rebind tells if the binding of an already bound column
    // will be overwritten.
    // It cannot be used anymore once the SetupNewTable object is used to
    // construct a Table object.
    void bindAll (const DataManager& dm, Bool rebind=False)
	{ newTable_p->bindAll (dm, rebind); }

    // Create data managers and bind the columns using the specifications
    // in the given record (which is obtained using Table::dataManagerInfo()).
    void bindCreate (const Record& spec)
        { newTable_p->bindCreate (spec); }

    // Define the shape of fixed shaped arrays in a column.
    // The shape of those arrays has to be known before the table
    // can be constructed. It has to be defined via this function,
    // if it was not already defined in the column description.
    // If only the dimensionality was defined in the column
    // description, the shape's dimensionality must match it.
    // Calling this function for an non-fixed shaped array results in
    // an exception.
    // It cannot be used anymore once the SetupNewTable object is used to
    // construct a Table object.
    void setShapeColumn (const String& columnName, const IPosition& shape)
	{ newTable_p->setShapeColumn (columnName, shape); }

    // Test if object is already in use.
    Bool isUsed() const
	{ return newTable_p->isUsed(); }

private:
    // Actual object.
    SetupNewTableRep* newTable_p;

    // Get pointer to column set.
    // This function is used by PlainTable.
    ColumnSet* columnSetPtr()
	{ return newTable_p->columnSetPtr(); }

    // Get pointer to table description.
    // This function is used by PlainTable.
    TableDesc* tableDescPtr()
	{ return newTable_p->tableDescPtr(); }

    // Set object to in use by a (Plain)Table object.
    // This function is used by PlainTable.
    void setInUse()
	{ newTable_p->setInUse(); }

    // Make a data manager for all unbound columns.
    void handleUnbound()
	{ newTable_p->handleUnbound(); }
};



} //# NAMESPACE CASACORE - END

#endif
