//# ForwardCol.h: Virtual Column Engine to forward to other columns
//# Copyright (C) 1995,1996,1997,2001
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

#ifndef TABLES_FORWARDCOL_H
#define TABLES_FORWARDCOL_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/VirtColEng.h>
#include <casacore/tables/DataMan/DataManager.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableColumn.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class ForwardColumnEngine;
class BaseColumn;


// <summary>
// Virtual column forwarding to another column
// </summary>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <use visibility=local>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> ForwardColumnEngine
//   <li> DataManagerColumn
// </prerequisite>

// <synopsis>
// ForwardColumn represents a virtual column which forwards the
// gets and puts to a column with the same name in another table.
// It is, in fact, a reference to the other column.
// The name of the other table is stored as a keyword in the
// forwarding column. When there is a forwarding chain (i.e.
// forwarding to a forwarding column), the name of the last
// table in the chain is stored in the keyword. In this way, the
// length of the chain is kept to a minimum. Otherwise a very long
// chain could occur, which would slow things down.
//
// Addition and deletion of rows is allowed, but the functions addRow and
// removeRow do not do anything at all. They are implemented to override
// the default "throw exception" implementation. Because the engine
// allows this, it can be used in a table supporting addition and removal
// of rows.
//
// An object of this class is created (and deleted) by
// <linkto class="ForwardColumnEngine:description">ForwardColumnEngine</linkto>
// which creates a ForwardColumn object for each column being forwarded.
// </synopsis> 

// <motivation>
// This class will be used by the calibration software.
// Most columns in a measurement table will be forwarded, while
// a few (i.e. the data themselves) will be calculated by a dedicated
// calibration engine.
// </motivation>

class ForwardColumn : public DataManagerColumn
{
public:

    // Construct it for the given column.
    ForwardColumn (ForwardColumnEngine* enginePtr,
		   const String& columnName,
		   int dataType,
		   const String& dataTypeId,
		   const Table& referencedTable);

    // Destructor is mandatory.
    virtual ~ForwardColumn();

    // Define the special keyword containing the name of the original table.
    // If the column in the referenced table contains that special keyword,
    // it is in its turn a forwarding column. In that case the special
    // keyword value will be copied over to shortcut the forwarding chain.
    // The suffix is appended to the keyword name when defining it.
    // This makes this function usable for derived classes.
    void fillTableName (const Table& thisTable, const Table& referencedTable);

    // Initialize the object.
    // This means binding the column to the column with the same name
    // in the original table.
    // It checks if the description of both columns is the same.
    // It also determines if the column is writable.
    virtual void prepare (const Table& thisTable);

    // Set the column to writable if its underlying table is writable.
    void setRW();

protected:
    // Do the preparation of the base class column object.
    void basePrepare (const Table& thisTable, Bool writable);

    BaseColumn* colPtr() const
	{ return colPtr_p; }

private:
    // Copy constructor is not needed and therefore forbidden
    // (so make it private).
    ForwardColumn (const ForwardColumn&);

    // Assignment is not needed and therefore forbidden (so make it private).
    ForwardColumn& operator= (const ForwardColumn&);

    // Create a SetupNewTable object with the given name and option
    // and with the description from the given table.
    // The SetupNewTable object will use a single ForwardColumn
    // engine which forwards all columns to the given table.
    // Later the SetupNewTable::bind functions can be used to bind one
    // or more columns to another data manager.
    static SetupNewTable setupNewTable (const Table& table,
					const String& tableName,
					Table::TableOption option);

    // This data manager may be able to handle changing array shapes.
    Bool canChangeShape() const;

    // This data manager may be able to do get/putScalarColumn.
    Bool canAccessScalarColumn (Bool& reask) const;

    // This data manager may be able to do get/putArrayColumn.
    Bool canAccessArrayColumn (Bool& reask) const;

    // This data manager may be able to do get/putSlice.
    Bool canAccessSlice (Bool& reask) const;

    // This data manager may be able to do get/putColumnSlice.
    Bool canAccessColumnSlice (Bool& reask) const;

    // Get the data type of the column as defined in DataType.h.
    int dataType() const;

    // Get the data type id of the column for dataType==TpOther.
    // This function is required for virtual column engines handling
    // non-standard data types. It is used to check the data type.
    String dataTypeId() const;

    // Test if data can be put into this column.
    Bool isWritable() const;

    // Set the shape of an direct array.
    // This only checks if the shape matches the referenced column.
    void setShapeColumn (const IPosition& shape);

    // Set the shape of an (indirect) array in the given row.
    void setShape (uInt rownr, const IPosition& shape);

    // Is the value shape defined in the given row?
    Bool isShapeDefined (uInt rownr);

    // Get the dimensionality of the item in the given row.
    uInt ndim (uInt rownr);

    // Get the shape of the item in the given row.
    IPosition shape (uInt rownr);

    // Get the scalar value with a standard data type in the given row.
    // <group>
    void getBoolV     (uInt rownr, Bool* dataPtr);
    void getuCharV    (uInt rownr, uChar* dataPtr);
    void getShortV    (uInt rownr, Short* dataPtr);
    void getuShortV   (uInt rownr, uShort* dataPtr);
    void getIntV      (uInt rownr, Int* dataPtr);
    void getuIntV     (uInt rownr, uInt* dataPtr);
    void getfloatV    (uInt rownr, float* dataPtr);
    void getdoubleV   (uInt rownr, double* dataPtr);
    void getComplexV  (uInt rownr, Complex* dataPtr);
    void getDComplexV (uInt rownr, DComplex* dataPtr);
    void getStringV   (uInt rownr, String* dataPtr);
    // </group>

    // Get the scalar value with a non-standard data type in the given row.
    void getOtherV    (uInt rownr, void* dataPtr);

    // Put the scalar value with a standard data type into the given row.
    // <group>
    void putBoolV     (uInt rownr, const Bool* dataPtr);
    void putuCharV    (uInt rownr, const uChar* dataPtr);
    void putShortV    (uInt rownr, const Short* dataPtr);
    void putuShortV   (uInt rownr, const uShort* dataPtr);
    void putIntV      (uInt rownr, const Int* dataPtr);
    void putuIntV     (uInt rownr, const uInt* dataPtr);
    void putfloatV    (uInt rownr, const float* dataPtr);
    void putdoubleV   (uInt rownr, const double* dataPtr);
    void putComplexV  (uInt rownr, const Complex* dataPtr);
    void putDComplexV (uInt rownr, const DComplex* dataPtr);
    void putStringV   (uInt rownr, const String* dataPtr);
    // </group>

    // Put the scalar value with a non-standard data type into the given row.
    void putOtherV    (uInt rownr, const void* dataPtr);

    // Get all scalar values in the column.
    // The argument dataPtr is in fact a Vector<T>*, but a void*
    // is needed to be generic.
    // The vector pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ScalarColumn getColumn function).
    void getScalarColumnV (void* dataPtr);

    // Put all scalar values in the column.
    // The argument dataPtr is in fact a const Vector<T>*, but a const void*
    // is needed to be generic.
    // The vector pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ScalarColumn putColumn function).
    void putScalarColumnV (const void* dataPtr);

    // Get some scalar values in the column.
    // The argument dataPtr is in fact a Vector<T>*, but a void*
    // is needed to be generic.
    // The vector pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ScalarColumn getColumn function).
    virtual void getScalarColumnCellsV (const RefRows& rownrs,
					void* dataPtr);

    // Put some scalar values in the column.
    // The argument dataPtr is in fact a const Vector<T>*, but a const void*
    // is needed to be generic.
    // The vector pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ScalarColumn getColumn function).
    virtual void putScalarColumnCellsV (const RefRows& rownrs,
					const void* dataPtr);

    // Get the array value in the given row.
    // The argument dataPtr is in fact a Array<T>*, but a void*
    // is needed to be generic.
    // The array pointed to by dataPtr has to have the correct shape
    // (which is guaranteed by the ArrayColumn get function).
    void getArrayV (uInt rownr, void* dataPtr);

    // Put the array value into the given row.
    // The argument dataPtr is in fact a const Array<T>*, but a const void*
    // is needed to be generic.
    // The array pointed to by dataPtr has to have the correct shape
    // (which is guaranteed by the ArrayColumn put function).
    void putArrayV (uInt rownr, const void* dataPtr);

    // Get a section of the array in the given row.
    // The argument dataPtr is in fact a Array<T>*, but a void*
    // is needed to be generic.
    // The array pointed to by dataPtr has to have the correct shape
    // (which is guaranteed by the ArrayColumn getSlice function).
    void getSliceV (uInt rownr, const Slicer& slicer, void* dataPtr);

    // Put into a section of the array in the given row.
    // The argument dataPtr is in fact a const Array<T>*, but a const void*
    // is needed to be generic.
    // The array pointed to by dataPtr has to have the correct shape
    // (which is guaranteed by the ArrayColumn putSlice function).
    void putSliceV (uInt rownr, const Slicer& slicer, const void* dataPtr);

    // Get all scalar values in the column.
    // The argument dataPtr is in fact a Vector<T>*, but a void*
    // is needed to be generic.
    // The vector pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ScalarColumn getColumn function).
    void getArrayColumnV (void* dataPtr);

    // Put all scalar values in the column.
    // The argument dataPtr is in fact a const Vector<T>*, but a const void*
    // is needed to be generic.
    // The vector pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ScalarColumn putColumn function).
    void putArrayColumnV (const void* dataPtr);

    // Get some array values in the column.
    // The argument dataPtr is in fact an Array<T>*, but a void*
    // is needed to be generic.
    // The vector pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ArrayColumn getColumn function).
    virtual void getArrayColumnCellsV (const RefRows& rownrs,
				       void* dataPtr);

    // Put some array values in the column.
    // The argument dataPtr is in fact an const Array<T>*, but a const void*
    // is needed to be generic.
    // The vector pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ArrayColumn getColumn function).
    virtual void putArrayColumnCellsV (const RefRows& rownrs,
				       const void* dataPtr);

    // Get a section of all arrays in the column.
    // The argument dataPtr is in fact a Array<T>*, but a void*
    // is needed to be generic.
    // The array pointed to by dataPtr has to have the correct shape
    // (which is guaranteed by the ArrayColumn getColumn function).
    void getColumnSliceV (const Slicer& slicer, void* dataPtr);

    // Put a section into all arrays in the column.
    // The argument dataPtr is in fact a const Array<T>*, but a const void*
    // is needed to be generic.
    // The array pointed to by dataPtr has to have the correct shape
    // (which is guaranteed by the ArrayColumn putColumn function).
    void putColumnSliceV (const Slicer& slicer, const void* dataPtr);

    // Get a section of some arrays in the column.
    // The argument dataPtr is in fact an Array<T>*, but a void*
    // is needed to be generic.
    // The array pointed to by dataPtr has to have the correct shape
    // (which is guaranteed by the ArrayColumn getColumn function).
    virtual void getColumnSliceCellsV (const RefRows& rownrs,
				       const Slicer& slicer, void* dataPtr);

    // Put into a section of some arrays in the column.
    // The argument dataPtr is in fact a const Array<T>*, but a const void*
    // is needed to be generic.
    // The array pointed to by dataPtr has to have the correct shape
    // (which is guaranteed by the ArrayColumn putColumn function).
    virtual void putColumnSliceCellsV (const RefRows& rownrs,
				       const Slicer& slicer,
				       const void* dataPtr);


    //# Now define the data members.
    ForwardColumnEngine* enginePtr_p;  //# pointer to parent engine
    String        colName_p;           //# The name of the column
    int           dataType_p;          //# data type of the column
    String        dataTypeId_p;        //# data type Id of the column
    TableColumn   refCol_p;            //# Column in referenced table
    //#                                    This is only filled in when
    //#                                    a new table is created.
    Bool          writable_p;          //# True = column is writable
    Table         origTable_p;         //# The original table for this column
    BaseColumn*   colPtr_p;            //# pointer to column in original table
};




// <summary>
// Virtual column engine forwarding to other columns
// </summary>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <use visibility=export>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> VirtualColumnEngine
// </prerequisite>

// <synopsis>
// ForwardColumnEngine is a data manager which forwards
// the gets and puts of columns to columns with the same names in
// another table.
// It is, in fact, a reference to the other table columns.
// The engine consists of a set of
// <linkto class="ForwardColumn:description">ForwardColumn</linkto>
// objects, which handle the actual gets and puts.
// </synopsis> 

// <motivation>
// This class will be used by the calibration software.
// Most columns in a measurement table will be forwarded
// (thus bound to a ForwardColumnEngine object), while
// a few (i.e. the data themselves) will be calculated by a dedicated
// calibration engine.
// </motivation>

// <example>
// <srcblock>
//    // The original table.
//    Table tab("someTable");
//    // Create another table with the same description.
//    SetupNewTable newtab("tForwardCol1.data", tab.tableDesc(), Table::New);
//    // Create an engine which forwards to the original table.
//    // Bind all columns in the new table to the forwarding engine.
//    ForwardColumnEngine fce(tab);
//    newtab.bindAll (fce);
//    // Create the new table.
//    // Every get and put on this table is forwarded to the original table.
//    // NB. Puts cannot be done here, because the original table was
//    //     opened as readonly.
//    // Of course, some columns could have been bound to another
//    // data manager (storage manager, calibration engine, ...).
//    Table forwTab(newtab);
// </srcblock>
// </example>

class ForwardColumnEngine : public VirtualColumnEngine
{
public:

    // The default constructor is required for reconstruction of the
    // engine when a table is read back.
    ForwardColumnEngine (const String& dataManagerName, const Record& spec);

    // Create the engine.
    // The columns using this engine will reference the given table.
    // The data manager gets the given name.
    ForwardColumnEngine (const Table& referencedTable,
			 const String& dataManagerName);

    // Create the engine.
    // The columns using this engine will reference the given table.
    // The data manager has no name.
    ForwardColumnEngine (const Table& referencedTable);

    // Destructor is mandatory.
    ~ForwardColumnEngine();

    // Clone the engine object.
    DataManager* clone() const;

    // Return the name of the data manager. This is the name of this
    // instantiation of the data manager, thus not its type name.
    String dataManagerName() const;

    // Return the type of the engine (i.e. its class name ForwardColumnEngine).
    String dataManagerType() const;

    // Record a record containing data manager specifications.
    virtual Record dataManagerSpec() const;

    // Get the suffix to be used for names.
    const String& suffix() const;

    // Return the name of the class.
    static String className();

    // Register the class name and the static makeObject "constructor".
    // This will make the engine known to the table system.
    static void registerClass();

protected:
    // Set the suffix.
    void setSuffix (const String& suffix);

    // Add a ForwardColumn object to the block.
    void addForwardColumn (ForwardColumn* colp);

    // Get access to the refTable_p data member.
    const Table& refTable() const
	{ return refTable_p; }

    // Do the creation (i.e. initialization) of the engine.
    void baseCreate();

    // Do the preparation of the engine by preparing all columns.
    void basePrepare();

private:
    // The copy constructor is forbidden (so it is private).
    ForwardColumnEngine (const ForwardColumnEngine&);

    // Assignment is forbidden (so it is private).
    ForwardColumnEngine& operator= (const ForwardColumnEngine&);

    // This data manager allows to add rows.
    Bool canAddRow() const;

    // This data manager allows to delete rows.
    Bool canRemoveRow() const;

    // Add rows to all columns.
    // This is not doing anything (but needed to override the default).
    void addRow (uInt nrrow);

    // Delete a row from all columns.
    // This is not doing anything (but needed to override the default).
    void removeRow (uInt rownr);

    // This data manager allows to add columns.
    Bool canAddColumn() const;

    // This data manager allows to delete columns.
    Bool canRemoveColumn() const;

    // Add a column.
    void addColumn (DataManagerColumn*);

    // Delete a column.
    void removeColumn (DataManagerColumn*);

    // Create the column object for the scalar column in this engine.
    DataManagerColumn* makeScalarColumn (const String& columnName,
					 int dataType,
					 const String& dataTypeId);

    // Create the column object for the indirect array column in this engine.
    DataManagerColumn* makeIndArrColumn (const String& columnName,
					 int dataType,
					 const String& dataTypeId);

    // Initialize the object for a new table.
    // It defines the column keywords containing the name of the
    // original table, which can be the parent of the referenced table.
    void create (uInt initialNrrow);

    // Initialize the engine.
    // It gets the name of the original table(s) from the column keywords,
    // opens those tables and attaches the ForwardColumn objects to the
    // columns in those tables.
    void prepare();

    // Reopen the engine for read/write access.
    // It makes all its columns writable if their underlying table is writable.
    void reopenRW();


    // Define the various engine column objects.
    PtrBlock<ForwardColumn*> refColumns_p;
    // The referenced table.
    // For newly created tables this is filled in by the constructor.
    // For existing tables this is filled in by the first ForwardColumn
    // object being constructed.
    Table refTable_p;
    // The name of the data manager.
    String dataManName_p;
    // The suffix to be used in names.
    String suffix_p;


public:
    // Set RefTable_p if not set yet.
    // This is done by ForwardColumn to cover the case for existing
    // tables where the default constructor of ForwardColumnEngine
    // is used and refTable_p is not filled in.
    void setRefTable (const Table& refTable);

    // Define the "constructor" to construct this engine when a
    // table is read back.
    // This "constructor" has to be registered by the user of the engine.
    // If the engine is commonly used, its registration can be added
    // into the registerAllCtor function in DataManReg.cc. 
    // This function gets automatically invoked by the table system.
    static DataManager* makeObject (const String& dataManagerType,
				    const Record& spec);
};



inline const String& ForwardColumnEngine::suffix() const
    { return suffix_p; }

inline void ForwardColumnEngine::setSuffix (const String& suffix)
    { suffix_p = suffix; }



} //# NAMESPACE CASACORE - END

#endif
