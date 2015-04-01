//# DataManager.h: Abstract base classes for a data manager
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
#include <casacore/tables/Tables/ColumnCache.h>
#include <casacore/tables/DataMan/TSMOption.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/Containers/SimOrdMap.h>
#include <casacore/casa/IO/ByteIO.h>
#include <casacore/casa/OS/Mutex.h>
#include<iosfwd>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class DataManager;
class DataManagerColumn;
class SetupNewTable;
class Table;
class MultiFileBase;
class Record;
class IPosition;
class Slicer;
class RefRows;
template<class T> class Array;
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
    virtual void addRow (uInt nrrow);

    // Delete a row from all columns.
    // The default implementation throws a "not possible" exception.
    virtual void removeRow (uInt rownr);

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
    virtual void create (uInt nrrow) = 0;

    // Let the data manager initialize itself for an existing table.
    // The AipsIO stream represents the main table file and must be
    // used by virtual column engines to retrieve the data stored
    // in the flush function.
    virtual void open (uInt nrrow, AipsIO& ios) = 0;

    // Open as above.
    // The data manager can return the number of rows it thinks there are.
    // This is particularly useful for data managers like LofarStMan whose
    // data are written outside the table system, thus for which no rows
    // have been added.
    // <br>By default it calls open and returns <src>nrrow</src>.
    virtual uInt open1 (uInt nrrow, AipsIO& ios);

    // Resync the data by rereading cached data from the file.
    // This is called when a lock is acquired on the file and it appears 
    // that data in this data manager has been changed by another process.
    virtual void resync (uInt nrrow) = 0;

    // Resync as above.
    // The data manager can return the number of rows it thinks there are.
    // This is particularly useful for data managers like LofarStMan whose
    // data are written outside the table system, thus for which no rows
    // have been added.
    // <br>By default it calls resync and returns <src>nrrow</src>.
    virtual uInt resync1 (uInt nrrow);

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




// <summary>
// Abstract base class for a column in a data manager
// </summary>

// <use visibility=local>

// <reviewed reviewer="Gareth Hunt" date="94Nov17" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> DataManager
// </prerequisite>

// <etymology>
// DataManagerColumn handles a column for a data manager.
// </etymology>

// <synopsis> 
// DataManagerColumn is the abstract base class to handle a column in
// a data manager. Each data manager class must have one or more associated
// classes derived from DataManagerColumn to handle the columns.
// For example, storage manager StManAipsIO has columns classes
// StManColumnAipsIO, StManColumnArrayAipsIO and StManColumnIndArrayAipsIO
// to handle scalars, direct arrays and indirect arrays, resp..
// However, using multiple inheritance it is possible that the derived
// DataManager and DataManagerColumn classes are the same. This is used
// in class ScaledArrayEngine<S,T> which represents both the data manager
// and its column class. It can do that, because the virtual column engine
// <linkto class="ScaledArrayEngine:description">ScaledArrayEngine</linkto>
// can handle only one column.
//
// In the synopsis of class DataManager it is described how the (derived)
// DataManagerColumn objects gets created and deleted.
// 
// DataManagerColumn defines various virtual functions to get or put (slices)
// of data in a column. These functions are called by the table column
// classes ScalarColumnData and ArrayColumnData.
// It does not define functions create, open, flush and prepare like
// those defined in DataManager. It is left to the derived classes to
// define those as needed and to interact properly with their
// data manager object.
// </synopsis> 

// <motivation>
// An abstract base class is needed to support multiple data
// managers in the table system
// </motivation>

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
// </todo>


class DataManagerColumn
{
public:

    // Create a column.
    DataManagerColumn()
	: isFixedShape_p(False) {;}

    // Frees up the storage.
    virtual ~DataManagerColumn();

    // Set the isFixedShape flag.
    void setIsFixedShape (Bool isFixedShape)
        { isFixedShape_p = isFixedShape; }

    // Is this a fixed shape column?
    Bool isFixedShape() const
        { return isFixedShape_p; }

    // Get the data type of the column as defined in DataType.h.
    virtual int dataType() const = 0;

    // Get the data type id of the column for dataType==TpOther.
    // The default implementation returns an emptry string.
    // This function is required for virtual column engines handling
    // non-standard data types. It is used to check the data type.
    virtual String dataTypeId() const;

    // Test if data can be put into this column.
    // This does not test if the data file is writable, only if
    // it is in principle allowed to store data into the column.
    // (It may not be allowed for virtual columns).
    // The default is True.
    virtual Bool isWritable() const;

    // Set the maximum length of the value (can be used for strings).
    // By default the maximum length is ignored.
    virtual void setMaxLength (uInt maxLength);

    // Set the shape of all (fixed-shaped) arrays in the column.
    // Effectively it is the same as setShapeColumn, but it also sets
    // the isFixedShape_p flag.
    void setFixedShapeColumn (const IPosition& shape)
        { setShapeColumn (shape); isFixedShape_p = True; }

    // Set the shape of an (variable-shaped) array in the given row.
    // By default it throws a "not possible" exception.
    virtual void setShape (uInt rownr, const IPosition& shape);

    // Set the shape and tile shape of an (variable-shaped) array
    // in the given row.
    // By default it ignores the tile shape (thus only sets the shape).
    virtual void setShapeTiled (uInt rownr, const IPosition& shape,
				const IPosition& tileShape);

    // Is the value shape defined in the given row?
    // By default it returns True.
    virtual Bool isShapeDefined (uInt rownr);

    // Get the dimensionality of the item in the given row.
    // By default it returns shape(rownr).nelements().
    virtual uInt ndim (uInt rownr);

    // Get the shape of the item in the given row.
    // By default it returns a zero-length IPosition (for a scalar value).
    virtual IPosition shape (uInt rownr);

    // Can the data manager handle chaging the shape of an existing array?
    // Default is no.
    virtual Bool canChangeShape() const;

    // Can the column data manager handle access to a scalar column?
    // If not, the caller should access the column by looping through
    // all cells in the column.
    // Default is no.
    // <br>
    // The returned reask switch determines if the information is
    // permanent. False indicates it is permanent; True indicates it
    // will be reasked for the next get/putColumn.
    // By default reask is set to False.
    virtual Bool canAccessScalarColumn (Bool& reask) const;

    // Can the column data manager handle access to a clooection of cells
    // in a scalar column?
    // If not, the caller should access the column cells by looping through
    // the cells in the column.
    // Default is no.
    // <br>
    // The returned reask switch determines if the information is
    // permanent. False indicates it is permanent; True indicates it
    // will be reasked for the next get/putColumn.
    // By default reask is set to False.
    virtual Bool canAccessScalarColumnCells (Bool& reask) const;

    // Can the column data manager handle access to a scalar column?
    // If not, the caller should access the column by looping through
    // all cells in the column.
    // Default is no.
    // <br>
    // The returned reask switch determines if the information is
    // permanent. False indicates it is permanent; True indicates it
    // will be reasked for the next get/putColumn.
    // By default reask is set to False.
    virtual Bool canAccessArrayColumn (Bool& reask) const;

    // Can the column data manager handle access to a collection of cells
    // in an array column?
    // If not, the caller should access the column cells by looping through
    // the cells in the column.
    // Default is no.
    // <br>
    // The returned reask switch determines if the information is
    // permanent. False indicates it is permanent; True indicates it
    // will be reasked for the next get/putColumn.
    // By default reask is set to False.
    virtual Bool canAccessArrayColumnCells (Bool& reask) const;

    // Can the column data manager handle access to a cell slice?
    // If not, the caller should do slicing itself (by accessing the
    // entire array and slicing it).
    // Default is no.
    // <br>
    // The returned reask switch determines if the information is
    // permanent. False indicates it is permanent; True indicates it
    // will be reasked for the next get/putColumn.
    // By default reask is set to False.
    virtual Bool canAccessSlice (Bool& reask) const;

    // Can the column data manager handle access to a column slice?
    // If not, the caller should access the column slice by looping through
    // all cell slices in the column.
    // Default is no.
    // <br>
    // The returned reask switch determines if the information is
    // permanent. False indicates it is permanent; True indicates it
    // will be reasked for the next get/putColumn.
    // By default reask is set to False.
    virtual Bool canAccessColumnSlice (Bool& reask) const;

    // Get access to the ColumnCache object.
    // <group>
    ColumnCache& columnCache()
        { return colCache_p; }
    const ColumnCache* columnCachePtr() const
        { return &colCache_p; }
    // </group>

    // Get the scalar value in the given row.
    // These functions are non-virtual and are converted to their
    // virtual getV equivalent to achieve that a derived templated class
    //(like VirtualScalarColumn) does not have to declare and implement
    // all these functions.
    // The compiler complains about hiding virtual functions if you do not
    // declare all virtual functions with the same name in a derived class.
    // <group>
    void get (uInt rownr, Bool* dataPtr)
	{ getBoolV (rownr, dataPtr); }
    void get (uInt rownr, uChar* dataPtr)
	{ getuCharV (rownr, dataPtr); }
    void get (uInt rownr, Short* dataPtr)
	{ getShortV (rownr, dataPtr); }
    void get (uInt rownr, uShort* dataPtr)
	{ getuShortV (rownr, dataPtr); }
    void get (uInt rownr, Int* dataPtr)
	{ getIntV (rownr, dataPtr); }
    void get (uInt rownr, uInt* dataPtr)
	{ getuIntV (rownr, dataPtr); }
    void get (uInt rownr, float* dataPtr)
	{ getfloatV (rownr, dataPtr); } 
   void get (uInt rownr, double* dataPtr)
	{ getdoubleV (rownr, dataPtr); }
    void get (uInt rownr, Complex* dataPtr)
	{ getComplexV (rownr, dataPtr); }
    void get (uInt rownr, DComplex* dataPtr)
	{ getDComplexV (rownr, dataPtr); }
    void get (uInt rownr, String* dataPtr)
	{ getStringV (rownr, dataPtr); }
    // This function is the get for all non-standard data types.
    void get (uInt rownr, void* dataPtr)
	{ getOtherV (rownr, dataPtr); }
    // </group>

    // Put the scalar value into the given row.
    // These functions are non-virtual and are converted to their
    // virtual putV equivalent to achieve that a derived templated class
    //(like VirtualScalarColumn) does not have to declare and implement
    // all these functions.
    // The compiler complains about hiding virtual functions if you do not
    // declare all virtual functions with the same name in a derived class.
    // <group>
    void put (uInt rownr, const Bool* dataPtr)
	{ putBoolV (rownr, dataPtr); }
    void put (uInt rownr, const uChar* dataPtr)
	{ putuCharV (rownr, dataPtr); }
    void put (uInt rownr, const Short* dataPtr)
	{ putShortV (rownr, dataPtr); }
    void put (uInt rownr, const uShort* dataPtr)
	{ putuShortV (rownr, dataPtr); }
    void put (uInt rownr, const Int* dataPtr)
	{ putIntV (rownr, dataPtr); }
    void put (uInt rownr, const uInt* dataPtr)
	{ putuIntV (rownr, dataPtr); }
    void put (uInt rownr, const float* dataPtr)
	{ putfloatV (rownr, dataPtr); }
    void put (uInt rownr, const double* dataPtr)
	{ putdoubleV (rownr, dataPtr); }
    void put (uInt rownr, const Complex* dataPtr)
	{ putComplexV (rownr, dataPtr); }
    void put (uInt rownr, const DComplex* dataPtr)
	{ putDComplexV (rownr, dataPtr); }
    void put (uInt rownr, const String* dataPtr)
	{ putStringV (rownr, dataPtr); }
    // This function is the put for all non-standard data types.
    void put (uInt rownr, const void* dataPtr)
	{ putOtherV (rownr, dataPtr); }
    // </group>

    // Get all scalar values in the column.
    // The argument dataPtr is in fact a Vector<T>*, but a void*
    // is needed to be generic.
    // The vector pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ScalarColumn getColumn function).
    // The default implementation throws an "invalid operation" exception.
    virtual void getScalarColumnV (void* dataPtr);

    // Put all scalar values in the column.
    // The argument dataPtr is in fact a const Vector<T>*, but a const void*
    // is needed to be generic.
    // The vector pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ScalarColumn putColumn function).
    // The default implementation throws an "invalid operation" exception.
    virtual void putScalarColumnV (const void* dataPtr);

    // Get some scalar values in the column.
    // The argument dataPtr is in fact a Vector<T>*, but a void*
    // is needed to be generic.
    // The vector pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ScalarColumn getColumn function).
    // The default implementation throws an "invalid operation" exception.
    virtual void getScalarColumnCellsV (const RefRows& rownrs,
					void* dataPtr);

    // Put some scalar values in the column.
    // The argument dataPtr is in fact a const Vector<T>*, but a const void*
    // is needed to be generic.
    // The vector pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ScalarColumn getColumn function).
    // The default implementation throws an "invalid operation" exception.
    virtual void putScalarColumnCellsV (const RefRows& rownrs,
					const void* dataPtr);

    // Get scalars from the given row on with a maximum of nrmax values.
    // It returns the actual number of values got.
    // This can be used to get an entire column of scalars or to get
    // a part of a column (for a cache for example).
    // The argument dataPtr is in fact a T*, but a void*
    // is needed to be generic.
    // The default implementation throws an "invalid operation" exception.
    virtual uInt getBlockV (uInt rownr, uInt nrmax, void* dataPtr);

    // Put nrmax scalars from the given row on.
    // It returns the actual number of values put.
    // This can be used to put an entire column of scalars or to put
    // a part of a column (for a cache for example).
    // The argument dataPtr is in fact a const T*, but a const void*
    // is needed to be generic.
    // The default implementation throws an "invalid operation" exception.
    virtual void putBlockV (uInt rownr, uInt nrmax, const void* dataPtr);

    // Get the array value in the given row.
    // The argument dataPtr is in fact an Array<T>*, but a void*
    // is needed to be generic.
    // The array pointed to by dataPtr has to have the correct shape
    // (which is guaranteed by the ArrayColumn get function).
    // The default implementation throws an "invalid operation" exception.
    virtual void getArrayV (uInt rownr, void* dataPtr);

    // Put the array value into the given row.
    // The argument dataPtr is in fact a const Array<T>*, but a const void*
    // is needed to be generic.
    // The array pointed to by dataPtr has to have the correct shape
    // (which is guaranteed by the ArrayColumn put function).
    // The default implementation throws an "invalid operation" exception.
    virtual void putArrayV (uInt rownr, const void* dataPtr);

    // Get all array values in the column.
    // The argument dataPtr is in fact an Array<T>*, but a void*
    // is needed to be generic.
    // The vector pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ArrayColumn getColumn function).
    // The default implementation throws an "invalid operation" exception.
    virtual void getArrayColumnV (void* dataPtr);

    // Put all array values in the column.
    // The argument dataPtr is in fact a const Array<T>*, but a const void*
    // is needed to be generic.
    // The vector pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ArrayColumn putColumn function).
    // The default implementation throws an "invalid operation" exception.
    virtual void putArrayColumnV (const void* dataPtr);

    // Get some array values in the column.
    // The argument dataPtr is in fact an Array<T>*, but a void*
    // is needed to be generic.
    // The vector pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ArrayColumn getColumn function).
    // The default implementation throws an "invalid operation" exception.
    virtual void getArrayColumnCellsV (const RefRows& rownrs,
				       void* dataPtr);

    // Put some array values in the column.
    // The argument dataPtr is in fact an const Array<T>*, but a const void*
    // is needed to be generic.
    // The vector pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ArrayColumn getColumn function).
    // The default implementation throws an "invalid operation" exception.
    virtual void putArrayColumnCellsV (const RefRows& rownrs,
				       const void* dataPtr);

    // Get a section of the array in the given row.
    // The argument dataPtr is in fact an Array<T>*, but a void*
    // is needed to be generic.
    // The array pointed to by dataPtr has to have the correct shape
    // (which is guaranteed by the ArrayColumn getSlice function).
    // The default implementation throws an "invalid operation" exception.
    virtual void getSliceV (uInt rownr, const Slicer& slicer, void* dataPtr);

    // Put into a section of the array in the given row.
    // The argument dataPtr is in fact a const Array<T>*, but a const void*
    // is needed to be generic.
    // The array pointed to by dataPtr has to have the correct shape
    // (which is guaranteed by the ArrayColumn putSlice function).
    // The default implementation throws an "invalid operation" exception.
    virtual void putSliceV (uInt rownr, const Slicer& slicer,
			    const void* dataPtr);

    // Get a section of all arrays in the column.
    // The argument dataPtr is in fact an Array<T>*, but a void*
    // is needed to be generic.
    // The array pointed to by dataPtr has to have the correct shape
    // (which is guaranteed by the ArrayColumn getColumn function).
    // The default implementation throws an "invalid operation" exception.
    virtual void getColumnSliceV (const Slicer& slicer, void* dataPtr);

    // Put into a section of all arrays in the column.
    // The argument dataPtr is in fact a const Array<T>*, but a const void*
    // is needed to be generic.
    // The array pointed to by dataPtr has to have the correct shape
    // (which is guaranteed by the ArrayColumn putColumn function).
    // The default implementation throws an "invalid operation" exception.
    virtual void putColumnSliceV (const Slicer& slicer, const void* dataPtr);

    // Get a section of some arrays in the column.
    // The argument dataPtr is in fact an Array<T>*, but a void*
    // is needed to be generic.
    // The array pointed to by dataPtr has to have the correct shape
    // (which is guaranteed by the ArrayColumn getColumn function).
    // The default implementation throws an "invalid operation" exception.
    virtual void getColumnSliceCellsV (const RefRows& rownrs,
				       const Slicer& slicer, void* dataPtr);

    // Put into a section of some arrays in the column.
    // The argument dataPtr is in fact a const Array<T>*, but a const void*
    // is needed to be generic.
    // The array pointed to by dataPtr has to have the correct shape
    // (which is guaranteed by the ArrayColumn putColumn function).
    // The default implementation throws an "invalid operation" exception.
    virtual void putColumnSliceCellsV (const RefRows& rownrs,
				       const Slicer& slicer,
				       const void* dataPtr);

    // Throw an "invalid operation" exception for the default
    // implementation of get.
    void throwGet() const;

    // Throw an "invalid operation" exception for the default
    // implementation of put.
    void throwPut() const;

    // Set the column name.
    void setColumnName (const String& colName)
      { colName_p = colName; }

    // Get rhe column name.
    const String& columnName() const
      { return colName_p; }

protected:
    // Get the scalar value in the given row.
    // The default implementation throws an "invalid operation" exception.
    // <group>
    virtual void getBoolV     (uInt rownr, Bool* dataPtr);
    virtual void getuCharV    (uInt rownr, uChar* dataPtr);
    virtual void getShortV    (uInt rownr, Short* dataPtr);
    virtual void getuShortV   (uInt rownr, uShort* dataPtr);
    virtual void getIntV      (uInt rownr, Int* dataPtr);
    virtual void getuIntV     (uInt rownr, uInt* dataPtr);
    virtual void getfloatV    (uInt rownr, float* dataPtr);
    virtual void getdoubleV   (uInt rownr, double* dataPtr);
    virtual void getComplexV  (uInt rownr, Complex* dataPtr);
    virtual void getDComplexV (uInt rownr, DComplex* dataPtr);
    virtual void getStringV   (uInt rownr, String* dataPtr);
    // This function is the get for all non-standard data types.
    virtual void getOtherV    (uInt rownr, void* dataPtr);
    // </group>

    // Put the scalar value into the given row.
    // The default implementation throws an "invalid operation" exception.
    // <group>
    virtual void putBoolV     (uInt rownr, const Bool* dataPtr);
    virtual void putuCharV    (uInt rownr, const uChar* dataPtr);
    virtual void putShortV    (uInt rownr, const Short* dataPtr);
    virtual void putuShortV   (uInt rownr, const uShort* dataPtr);
    virtual void putIntV      (uInt rownr, const Int* dataPtr);
    virtual void putuIntV     (uInt rownr, const uInt* dataPtr);
    virtual void putfloatV    (uInt rownr, const float* dataPtr);
    virtual void putdoubleV   (uInt rownr, const double* dataPtr);
    virtual void putComplexV  (uInt rownr, const Complex* dataPtr);
    virtual void putDComplexV (uInt rownr, const DComplex* dataPtr);
    virtual void putStringV   (uInt rownr, const String* dataPtr);
    // This function is the put for all non-standard data types.
    virtual void putOtherV    (uInt rownr, const void* dataPtr);
    // </group>

private:
    Bool        isFixedShape_p;
    String      colName_p;
    ColumnCache colCache_p;

    // Set the shape of all (fixed-shaped) arrays in the column.
    // By default it throws a "not possible" exception.
    virtual void setShapeColumn (const IPosition& shape);

    // The copy constructor cannot be used for this base class.
    // The private declaration of this constructor makes it unusable.
    DataManagerColumn (const DataManagerColumn&);

    // Assignment cannot be used for this base class.
    // The private declaration of this operator makes it unusable.
    DataManagerColumn& operator= (const DataManagerColumn&);
};



} //# NAMESPACE CASACORE - END

#endif
