//# ForwardColRow.h: Virtual Column Engine to forward to other rows/columns
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

#ifndef TABLES_FORWARDCOLROW_H
#define TABLES_FORWARDCOLROW_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/ForwardCol.h>
#include <casacore/tables/Tables/ScalarColumn.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class ForwardColumnIndexedRowEngine;


// <summary>
// Virtual column forwarding to another row/column
// </summary>

// <reviewed reviewer="Paul Shannon" date="1995/05/22" tests="tForwardColRow.cc">
// </reviewed>

// <use visibility=local>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> ForwardColumnIndexedRowEngine
//   <li> ForwardColumn
// </prerequisite>

// <etymology>
// ForwardColumnIndexedRow handles the forwarding of the gets and puts
// for an individual row/column on behalf of the virtual column engine
// ForwardColumnIndexedRowEngine. It forwards them to a row/column in
// another table. The row forwarding is done using a special column
// containing row numbers indexing the referenced table. 
// </etymology>

// <synopsis>
// ForwardColumnIndexedRow represents a virtual column which forwards the
// gets and puts to a column with the same name in another table.
// It is, in fact, a reference to the other column.
// The row numbers in the column are mapped to row numbers in the referenced
// column using a special column containing the mapping.
// The name of the other table is stored as a keyword in the
// forwarding column. When the referenced column is in its turn a
// ForwardColumn (note: not a ForwardColumnIndexedRow), the table
// mentioned in there will be used. In this way, the length of the
// forwarding chain is kept to a minimum.
//
// An object of this class is created (and deleted) by the virtual column
// engine
// <linkto class="ForwardColumnIndexedRowEngine:description">
// ForwardColumnIndexedRowEngine</linkto>
// which creates a ForwardColumnIndexedRow object for each column being
// forwarded.
// </synopsis> 


class ForwardColumnIndexedRow : public ForwardColumn
{
public:

    // Construct it for the given column.
    ForwardColumnIndexedRow (ForwardColumnIndexedRowEngine* enginePtr,
			     const String& columnName,
			     int dataType,
			     const String& dataTypeId,
			     const Table& referencedTable);

    // Destructor is mandatory.
    ~ForwardColumnIndexedRow();

    // Initialize the object.
    // This means binding the column to the column with the same name
    // in the original table.
    // It checks if the description of both columns is the same.
    void prepare (const Table& thisTable);

private:
    // Copy constructor is not needed and therefore forbidden
    // (so make it private).
    ForwardColumnIndexedRow (const ForwardColumnIndexedRow&);

    // Assignment is not needed and therefore forbidden (so make it private).
    ForwardColumnIndexedRow& operator= (const ForwardColumnIndexedRow&);

    // This data manager cannot handle changing array shapes.
    Bool canChangeShape() const;

    // This data manager cannot do get/putColumn.
    Bool canAccessScalarColumn (Bool& reask) const;

    // This data manager cannot do get/putColumn.
    Bool canAccessArrayColumn (Bool& reask) const;

    // This data manager cannot do get/putColumn.
    Bool canAccessColumnSlice (Bool& reask) const;

    // Set the shape of an (indirect) array in the given row.
    // This throws an exception, because putting is not supported.
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
    // This throws an exception, because putting is not supported.
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
    // This throws an exception, because putting is not supported.
    void putOtherV    (uInt rownr, const void* dataPtr);

    // Get the array value in the given row.
    // The argument dataPtr is in fact a Array<T>*, but a void*
    // is needed to be generic.
    // The array pointed to by dataPtr has to have the correct shape
    // (which is guaranteed by the ArrayColumn get function).
    void getArrayV (uInt rownr, void* dataPtr);

    // Put the array value into the given row.
    // This throws an exception, because putting is not supported.
    void putArrayV (uInt rownr, const void* dataPtr);

    // Get a section of the array in the given row.
    // The argument dataPtr is in fact a Array<T>*, but a void*
    // is needed to be generic.
    // The array pointed to by dataPtr has to have the correct shape
    // (which is guaranteed by the ArrayColumn getSlice function).
    void getSliceV (uInt rownr, const Slicer& slicer, void* dataPtr);

    // Put into a section of the array in the given row.
    // This throws an exception, because putting is not supported.
    void putSliceV (uInt rownr, const Slicer& slicer, const void* dataPtr);

    // Convert the rownr to the rownr in the underlying table.
    uInt convertRownr (uInt rownr);

    //# Now define the data members.
    ForwardColumnIndexedRowEngine* enginePtr_p;  //# pointer to parent engine
};




// <summary>
// Virtual column engine forwarding to other columns/rows.
// </summary>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <use visibility=export>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> VirtualColumnEngine
// </prerequisite>

// <etymology>
// ForwardColumnIndexedRowEngine is a virtual column engine which
// forwards the gets and puts of columns to corresponding columns
// in another table. Furthermore it maps the row number by indexing
// the row number in the referenced table.
// </etymology>

// <synopsis>
// ForwardColumnIndexedRowEngine is a data manager which forwards
// the gets and puts of columns to columns with the same names in
// another table. In that sense it is the same as the virtual column engine
// <linkto class="ForwardColumnEngine:description">
// ForwardColumnEngine</linkto>.
// However, it also forwards the row number. That is, it uses a column
// containing row numbers to index the correct row in the referenced table.
// The name of this column and the name of the referenced table have to
// be given when constructing the engine.
//
// For example:<br>
// Table TABA contains columns A, B and C and consists of N rows.
// Table TABF uses ForwardColumnIndexedRowEngine to forward its columns
// A, B and C to the corresponding columns in TABA. Furthermore it
// contains a column ROW containing row numbers in TABA. This column is
// the mapping of row numbers in TABF to rows in TABA. E.g. if ROW has
// the value 25 in row 10, row 10 of TABF is forwarded to row 25 in TABA.
//
// Actually, puts are not possible. When multiple rows map to the same row
// in the referenced table, putting a value in one row would also change
// the value in another row referencing the same underlying row. This
// could result in unexpected behaviour.
//
// The engine consists of a set of
// <linkto class="ForwardColumnIndexedRow:description">
// ForwardColumnIndexedRow</linkto>
// objects, which handle the actual gets.
// </synopsis> 

// <motivation>
// In some ways it overlaps the functionality of the storage manager
// StManMirAIO. They both allow to have the same value used by multiple
// rows. However, StManMirAIO only allows that for consecutive rows,
// while this engine allows it for any row. On the other side,
// StManMirAIO is faster.
// </motivation>

// <example>
// <srcblock>
//    // The original table.
//    Table tab("someTable");
//    // Create another table with the same description.
//    SetupNewTable newtab("tForwardColRow.data", tab.tableDesc(), Table::New);
//    // Create an engine which forwards to the original table and uses
//    // column rowColumn to get the row number in the referenced table.
//    // Bind all columns in the new table to the forwarding engine.
//    ForwardColumnIndexedRowEngine fce(tab, "rowColumn");
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

class ForwardColumnIndexedRowEngine : public ForwardColumnEngine
{
public:

    // The default constructor is required for reconstruction of the
    // engine when a table is read back.
    ForwardColumnIndexedRowEngine (const String& dataManagerName,
				   const Record& spec);

    // Create the engine.
    // The columns using this engine will reference the given table.
    // The column with the given name contains the row number mapping,
    // i.e. a row number in a get or put is converted to a row number
    // in the referenced table using the value in this column.
    // The data manager gets the given name.
    ForwardColumnIndexedRowEngine (const Table& referencedTable,
				   const String& rowColumnName,
				   const String& dataManagerName);

    // Create the engine.
    // The columns using this engine will reference the given table.
    // The column with the given name contains the row number mapping,
    // i.e. a row number in a get or put is converted to a row number
    // in the referenced table using the value in this column.
    // The data manager has no name.
    ForwardColumnIndexedRowEngine (const Table& referencedTable,
				   const String& rowColumnName);

    // Destructor is mandatory.
    ~ForwardColumnIndexedRowEngine();

    // Clone the engine object.
    DataManager* clone() const;

    // Return the type name of the engine
    // (i.e. its class name ForwardColumnIndexedRowEngine).
    String dataManagerType() const;

    // Record a record containing data manager specifications.
    virtual Record dataManagerSpec() const;

    // Return the name of the class.
    static String className();

    // Register the class name and the static makeObject "constructor".
    // This will make the engine known to the table system.
    static void registerClass();

private:
    // The copy constructor is forbidden (so it is private).
    ForwardColumnIndexedRowEngine (const ForwardColumnIndexedRowEngine&);

    // Assignment is forbidden (so it is private).
    ForwardColumnIndexedRowEngine& operator=
	                          (const ForwardColumnIndexedRowEngine&);

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
    // It also defines a keyword containing the row column name.
    void create (uInt initialNrrow);

    // Initialize the engine.
    // It gets the name of the original table(s) from the column keywords,
    // opens those tables and attaches the ForwardColumnIndexedRow objects
    // to the columns in those tables.
    void prepare();

    // Reopen the engine for read/write access.
    // This cannot be done, so all columns remain readonly.
    // The function is needed to override the behaviour of its base class.
    void reopenRW();


    // Define the column with the row numbers (must have data type uInt).
    String                          rowColumnName_p;
    ScalarColumn<uInt>              rowColumn_p;
    // Define the various engine column objects.
    PtrBlock<ForwardColumnIndexedRow*> refColumns_p;
    // Cache of last row used to get row number.
    Int lastRow_p;
    uInt rowNumber_p;


public:
    // Define the "constructor" to construct this engine when a
    // table is read back.
    // This "constructor" has to be registered by the user of the engine.
    // If the engine is commonly used, its registration can be added
    // into the registerAllCtor function in DataManReg.cc. 
    // This function gets automatically invoked by the table system.
    static DataManager* makeObject (const String& dataManagerName,
				    const Record& spec);

    // Convert the rownr to the rownr in the underlying table.
    uInt convertRownr (uInt rownr);
};


inline uInt ForwardColumnIndexedRowEngine::convertRownr (uInt rownr)
{
    if (Int(rownr) != lastRow_p) {
	rowNumber_p = rowColumn_p(rownr);
	lastRow_p   = rownr;
    }
    return rowNumber_p;
}

inline uInt ForwardColumnIndexedRow::convertRownr (uInt rownr)
    { return enginePtr_p->convertRownr (rownr); }



} //# NAMESPACE CASACORE - END

#endif
