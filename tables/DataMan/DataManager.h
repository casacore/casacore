//# DataManager.h: Abstract base class for a data manager
//# Copyright (C) 1994,1995,1996,1997,1998,1999,2001,2002
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

#ifndef TABLES_DATAMANAGER_H
#define TABLES_DATAMANAGER_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/DataManagerColumn.h>
#include <casacore/tables/DataMan/TSMOption.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Containers/SimOrdMap.h>
#include <casacore/casa/IO/ByteIO.h>
#include <casacore/casa/OS/Mutex.h>
#include <iosfwd>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class DataManager;
class SetupNewTable;
class Table;
class MultiFileBase;
class Record;
class AipsIO;


// <summary>
// Define the type of the static construction function.
// </summary>

// <use visibility=local>

// <reviewed reviewer="Gareth Hunt" date="94Nov17" tests="">
// </reviewed>

// <synopsis>
// Class names of data managers and pointers to their associated constructor
// function are registered in a static map to be able to create the correct
// data manager object from a string giving the type name of the data manager.
// DataManagerCtor is the type of the constructor functions.
// </synopsis>
// <group name=DataManagerCtor>
typedef DataManager* (*DataManagerCtor) (const String& dataManagerType,
					 const Record& spec);
// </group>


// <summary>
// Abstract base class for a data manager
// </summary>

// <use visibility=local>

// <reviewed reviewer="Gareth Hunt" date="94Nov17" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
// </prerequisite>

// <synopsis> 
// DataManager is the abstract base class for all kind of table data managers.
// There are currently 2 classes of data managers:
// <ul>
//  <li> Storage managers handling the storage of data. These classes
//         have to be derived from DataManager.
//         StManAipsIO is an example of a storage manager.
//  <li> Virtual column engines handling the on-the-fly calculation
//         of data, which are not stored as such. The base class for
//         these is VirtualColumnEngine (which is derived from DataManager),
//         from which all virtual columns engine must be derived from.
// </ul>
// DataManager contains some common data and defines several virtual
// functions, which usually have to be implemented in the derived classes.
// It also contains some helper functions for the derived classes
// (like fileName().
//
// The actual handling of a column by the data manager is defined in
// the abstract base class
// <linkto class="DataManagerColumn:description">DataManagerColumn</linkto>.
// Each data manager must
// have an associated class (derived from DataManagerColumn) to
// handle the columns.
//
// There is a protocol defined how a data manager is created and
// initialized. For a new table it is:
// <ul>
//  <li>
//   The user creates data managers and binds them to columns. For example:
//   <srcblock>
//   SetupNewTable newtab("name.data", Table::New);  // set up new table
//   StManAipsIO stman;                       // define storage manager
//   newtab.bindColumn ("column1", stman);    // bind column to st.man.
//   newtab.bindColumn ("column2", stman);    // bind column to st.man.
//   Table tab(newtab);                       // actually create table
//   </srcblock>
//   When the given data manager object is used for the first time in a bind
//   function, a copy of the object is made using the clone function.
//   Thus in the above example column1 and column2 share the same data
//   manager; only at the first bind the stman object is cloned.
//   Columns not explicitly bound to a data manager get implicitly bound
//   to the default data manager (as defined in the column description)
//   by the Table constructor (as used in line 5).
//  <li>
//   After binding the unbound columns, the PlainTable constructor sets up
//   the data managers. For each column it asks the data manager to
//   construct a DataManagerColumn object (in fact, an object of a class
//   derived from DataManagerColumn). This is done by the functions
//   createScalarColumn, createIndArrColumn and createDirArrColumn.
//   For each data manager the create function is called. This allows
//   them to initialize themselves and/or to call an initialization
//   function in their column objects.
//   This is, for instance, used by the storage managers to create files.
//   Thereafter the prepare function is called to allow the data managers
//   to do further initialization possibly requiring information from
//   other columns.
//  <li>
//   When the table gets written (by the PlainTable destructor),
//   the flush function is called for each data manager. This allows
//   the data manager or their column objects to write or flush their data.
//   The table system takes care of storing the information required
//   to reconstruct the data managers. It uses the function dataManagerType
//   to store the (unique) type name of the data manager class.
//  <li>
//   Finally each data manager object gets deleted. Their destructors
//   must delete their column objects (if any and if needed).
// </ul>
// For an existing table the procedure is slightly different:
// <ul>
//  <li>
//   The statement
//   <br><src> Table tab("name.data"); </src>
//   will create a table object for an existing table. This has the effect
//   that the given table file will be read to reconstruct the Table object
//   and the data managers.
//  <li>
//   The stored data manager class names are used to reconstruct
//   the data managers. This uses the static registration map, which
//   maps the class name to a static class constructor function (usually
//   called makeObject). This requires that the type name and constructor
//   for each possible data manager are registered before the table
//   is opened. The DataManager function registerMainCtor (implemented
//   in DataManager.cc) is called before a table is opened, so registration
//   of data managers should, in principle, be done there.
//   <br>However, for unknown data managers it is tried to load a shared
//   library whose name is the lowercase version of the data manager without a
//   possible template argument (e.g. <src>bitflagsengine</src> for
//   data manager <src>BitFlagsEngine<Int></src>).
//   It can be preceeded by lib or libcasa_ and followed by .so or .dylib.
//   The shared library has to have a function with a name like
//   <src>register_bitflagsengine</src> that must register the data manager(s).
//   The function must be declared as <src>extern "C"</src>, otherwise its
//   name gets mangled.
//  <li>
//   Each table column is bound to the correct data manager. The sequence
//   number stored in the table file is used for that purpose.
//  <li>
//   The DataManager createXXXColumn functions are called for each table
//   column to let the data manager construct a data manager column object.
//  <li>
//   For each data manager the open function is called to allow it and
//   its column objects to read back the information stored in the
//   flush function.
//   Thereafter the prepare function is called for each data manager
//   to allow it to initialize some variables.
//   The reason that open and prepare are separated is that in order to
//   initialize variables it may be required to use other columns.
//   So it may be needed that all columns are read back before they
//   get initialized.
//  <li>
//   Similar to a new table the flush functions gets called when the
//   table gets written. Destruction is also the same as sketched
//   for new tables.
// </ul>
// </synopsis> 

// <motivation>
// An abstract base class is needed to support data managers and
// virtual column engines in the table system in a transparant way.
// </motivation>

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//  <li> Handle unregistered data managers in a better way.
//         Instead of throwing an exception a subprocess could be
//         started which represents the data manager.
// </todo>


class DataManager
{
friend class SetupNewTable;
friend class ColumnSet;

public:

    // Default constructor.
    DataManager();

    virtual ~DataManager();

    // Make a clone of the derived object.
    virtual DataManager* clone() const = 0;

    // Return the name of the data manager. This is the name of this
    // instantiation of the data manager, thus not its type name.
    // By default it returns an empty string.
    virtual String dataManagerName() const;

    // Return the type name of the data manager (in fact its class name).
    // It has to be a unique name, thus if the class is templated
    // the template parameter has to be part of the name.
    // This is used by the open/flush mechanism to be able to reconstruct
    // the correct data manager.
    virtual String dataManagerType() const = 0;

    // Add SEQNR and SPEC (the DataManagerSpec subrecord) to the info.
    void dataManagerInfo (Record& info) const;

    // Return a record containing data manager specifications.
    // The default implementation returns an empty record.
    virtual Record dataManagerSpec() const;

    // Get data manager properties that can be modified.
    // It is a subset of the data manager specification.
    // The default implementation returns an empty record.
    virtual Record getProperties() const;

    // Modify data manager properties given in record fields. Only the
    // properties as returned by getProperties are used, others are ignored.
    // The default implementation does nothing.
    virtual void setProperties (const Record& spec);

    // Is the data manager a storage manager?
    // The default is yes.
    virtual Bool isStorageManager() const;

    // Tell if the data manager wants to reallocate the data manager
    // column objects.
    // This is used by the tiling storage manager.
    // By default it returns False.
    virtual Bool canReallocateColumns() const;

    // Reallocate the column object if it is part of this data manager.
    // It returns a pointer to the new column object.
    // This function is used by the tiling storage manager.
    // By default it does nothing and returns the input pointer.
    virtual DataManagerColumn* reallocateColumn (DataManagerColumn* column);
    
    // Get the (unique) sequence nr of this data manager.
    uInt sequenceNr() const
	{ return seqnr_p; }

    // Get the nr of columns in this data manager (can be zero).
    uInt ncolumn() const
	{ return nrcol_p; }

    // Have the data to be stored in big or little endian canonical format?
    Bool asBigEndian() const
      { return asBigEndian_p; }

    // Get the TSM option.
    const TSMOption& tsmOption() const
      { return tsmOption_p; }

    // Get the MultiFile pointer (can be 0).
    MultiFileBase* multiFile()
      { return multiFile_p; }

    // Compose a keyword name from the given keyword appended with the
    // sequence number (e.g. key_0).
    // This makes the keyword name unique if multiple data managers
    // are used with the same type.
    String keywordName (const String& keyword) const;

    // Compose a unique filename from the table name and sequence number.
    String fileName() const;

    // Get the AipsIO option of the underlying file.
    ByteIO::OpenOption fileOption() const;

    // Is this a regular storage manager?
    // It is regular if it allows addition of rows and writing data in them.
    // <br>The default implementation returns True.
    virtual Bool isRegular() const;

    // Get the table this object is associated with.
    Table& table() const
	{ return *table_p; }

    // Reopen the data manager for read/write access.
    // By default it is assumed that a reopen for read/write does
    // not have to do anything.
    virtual void reopenRW();

    // Does the data manager allow to add rows? (default no)
    virtual Bool canAddRow() const;

    // Does the data manager allow to delete rows? (default no)
    virtual Bool canRemoveRow() const;

    // Does the data manager allow to add columns? (default no)
    virtual Bool canAddColumn() const;

    // Does the data manager allow to delete columns? (default no)
    virtual Bool canRemoveColumn() const;

    // Does the data manager allow to rename columns? (default yes)
    virtual Bool canRenameColumn() const;

    // Set the maximum cache size (in bytes) to be used by a storage manager.
    // The default implementation does nothing.
    virtual void setMaximumCacheSize (uInt nbytes);

    // Show the data manager's IO statistics. By default it does nothing.
    virtual void showCacheStatistics (std::ostream&) const;

    // Create a column in the data manager on behalf of a table column.
    // It calls makeXColumn and checks the data type.
    // <group>
    // Create a scalar column.
    // The <src>dataTypeId</src> argument is gives the id (i.e. name)
    // of the data type of the column. It is only used for virtual
    // columns of a non-standard data type to be able to check if
    // the correctness of the column data type.
    // <br>Storage managers only handle standard data types and
    // can readily ignore this argument.
    DataManagerColumn* createScalarColumn (const String& columnName,
					   int dataType,
					   const String& dataTypeId);
    // Create a direct array column.
    DataManagerColumn* createDirArrColumn (const String& columnName,
					   int dataType,
					   const String& dataTypeId);
    // Create an indirect array column.
    DataManagerColumn* createIndArrColumn (const String& columnName,
					   int dataType,
					   const String& dataTypeId);
    // </group>

    // The data manager will be deleted (because all its columns are
    // requested to be deleted).
    // So clean up the things needed (e.g. delete files).
    virtual void deleteManager() = 0;


protected:
    // Decrement number of columns (in case a column is deleted).
    void decrementNcolumn()
	{ nrcol_p--; }

    // Tell the data manager if big or little endian format is needed.
    void setEndian (Bool bigEndian)
      { asBigEndian_p = bigEndian; }

    // Tell the data manager which TSM option to use.
    void setTsmOption (const TSMOption& tsmOption);

    // Tell the data manager that MultiFile can be used.
    // Because MultiFile cannot be used with mmapped files, it sets
    // the TSMOption accordingly.
    void setMultiFile (MultiFileBase* mfile);

    // Does the data manager support use of MultiFile?
    // A derived class has to return True if it can use the MultiFile.
    // The default implementation returns False.
    virtual Bool hasMultiFileSupport() const;

    // Throw an exception in case data type is TpOther, because the
    // storage managers (and maybe other data managers) do not support
    // such columns.
    void throwDataTypeOther (const String& columnName, int dataType) const;


private:
    uInt         nrcol_p;            //# #columns in this st.man.
    uInt         seqnr_p;            //# Unique nr of this st.man. in a Table
    Bool         asBigEndian_p;      //# store data in big or little endian
    TSMOption    tsmOption_p;
    MultiFileBase* multiFile_p;      //# MultiFile to use; 0=no MultiFile
    Table*       table_p;            //# Table this data manager belongs to
    mutable DataManager* clone_p;    //# Pointer to clone (used by SetupNewTab)


    // The copy constructor cannot be used for this base class.
    // The clone function should be used instead.
    // The private declaration of this constructor makes it unusable.
    DataManager (const DataManager&);

    // Assignment cannot be used for this base class.
    // The private declaration of this operator makes it unusable.
    DataManager& operator= (const DataManager&);

    // Create a column in the data manager on behalf of a table column.
    //# Should be private, but has to be public because friend
    //# declaration gave internal CFront error.
    // <group>
    // Create a scalar column.
    virtual DataManagerColumn* makeScalarColumn (const String& columnName,
						 int dataType,
						 const String& dataTypeId) = 0;
    // Create a direct array column.
    virtual DataManagerColumn* makeDirArrColumn (const String& columnName,
						 int dataType,
						 const String& dataTypeId) = 0;
    // Create an indirect array column.
    virtual DataManagerColumn* makeIndArrColumn (const String& columnName,
						 int dataType,
						 const String& dataTypeId) = 0;
    // </group>

    // Check if the data type of the created data manager column is correct.
    void checkDataType (const DataManagerColumn* colPtr,
			const String& columnName,
			int dataType, const String& dataTypeId) const;

    // Add rows to all columns.
    // The default implementation throws a "not possible" exception.
    virtual void addRow (rownr_t nrrow);

    // Delete a row from all columns.
    // The default implementation throws a "not possible" exception.
    virtual void removeRow (rownr_t rownr);

    // Add a column.
    // The default implementation throws a "not possible" exception.
    virtual void addColumn (DataManagerColumn*);

    // Delete a column.
    // The default implementation throws a "not possible" exception.
    virtual void removeColumn (DataManagerColumn*);

    // Set the sequence number of this data manager.
    void setSeqnr (uInt nr)
	{ seqnr_p = nr; }

    // Link the data manager to the Table object.
    void linkToTable (Table& tab);

    // Flush and optionally fsync the data.
    // The AipsIO stream represents the main table file and can be
    // used by virtual column engines to store SMALL amounts of data.
    // It returns a True status if it had to flush (i.e. if data have changed).
    virtual Bool flush (AipsIO& ios, Bool fsync) = 0;

    // Let the data manager initialize itself for a new table.
    virtual void create (rownr_t nrrow) = 0;

    // Let the data manager initialize itself for an existing table.
    // The AipsIO stream represents the main table file and must be
    // used by virtual column engines to retrieve the data stored
    // in the flush function.
    virtual void open (rownr_t nrrow, AipsIO& ios) = 0;

    // Open as above.
    // The data manager can return the number of rows it thinks there are.
    // This is particularly useful for data managers like LofarStMan whose
    // data are written outside the table system, thus for which no rows
    // have been added.
    // <br>By default it calls open and returns <src>nrrow</src>.
    virtual rownr_t open1 (rownr_t nrrow, AipsIO& ios);

    // Resync the data by rereading cached data from the file.
    // This is called when a lock is acquired on the file and it appears 
    // that data in this data manager has been changed by another process.
    virtual void resync (rownr_t nrrow) = 0;

    // Resync as above.
    // The data manager can return the number of rows it thinks there are.
    // This is particularly useful for data managers like LofarStMan whose
    // data are written outside the table system, thus for which no rows
    // have been added.
    // <br>By default it calls resync and returns <src>nrrow</src>.
    virtual rownr_t resync1 (rownr_t nrrow);

    // Let the data manager initialize itself further.
    // Prepare is called after create/open has been called for all
    // columns. In this way one can be sure that referenced columns
    // are read back and partly initialized.
    // The default implementation does nothing.
    virtual void prepare();

    // Declare the mapping of the data manager type name to a static
    // "makeObject" function.
    static SimpleOrderedMap<String,DataManagerCtor> theirRegisterMap;
    static MutexedInit theirMutexedInit;

public:
    // Has the object already been cloned?
    DataManager* getClone() const
        { return clone_p; }

    // Set the pointer to the clone.
    void setClone (DataManager* clone) const
        { clone_p = clone; }

    // Register a mapping of a data manager type to its static construction
    // function. It is fully thread-safe.
    static void registerCtor (const String& type, DataManagerCtor func);

    // Get the "constructor" of a data manager (thread-safe).
    static DataManagerCtor getCtor (const String& dataManagerType);

    // Test if a data manager is registered (thread-safe).
    static Bool isRegistered (const String& dataManagerType);

    // Register the main data managers (if not done yet).
    // It is fully thread-safe.
    static void registerMainCtor()
      { theirMutexedInit.exec(); }

    // Serve as default function for theirRegisterMap, which catches all
    // unknown data manager types.
    // <thrown>
    //   <li> TableUnknownDataManager
    // </thrown>
    static DataManager* unknownDataManager (const String& dataManagerType,
					    const Record& spec);

private:
    // Register a data manager constructor.
    static void unlockedRegisterCtor (const String& type,
                                      DataManagerCtor func)
      { theirRegisterMap.define (type, func); }

    // Do the actual (thread-safe) registration of the main data managers.
    static void doRegisterMainCtor (void*);
};


} //# NAMESPACE CASACORE - END

#endif
