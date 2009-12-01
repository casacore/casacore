//# ConcatColumn.h: A column in a concatenated table
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

#ifndef TABLES_CONCATCOLUMN_H
#define TABLES_CONCATCOLUMN_H


//# Includes
#include <casa/aips.h>
#include <tables/Tables/BaseColumn.h>
#include <tables/Tables/ColumnCache.h>
#include <tables/Tables/TableRecord.h>

namespace casa { //# NAMESPACE CASA - BEGIN

  //# Forward Declarations
  class ConcatTable;
  class BaseColumnDesc;
  class TableRecord;
  class Slicer;
  class IPosition;
  template<class T> class Vector;


  // <summary>
  // A column in a concatenated table
  // </summary>

  // <use visibility=local>

  // <reviewed reviewer="UNKNOWN" date="" tests="">
  // </reviewed>

  // <prerequisite>
  //# Classes you should understand before using this one.
  //   <li> ConcatTable
  //   <li> BaseColumn
  // </prerequisite>

  // <etymology>
  // ConcatTable represents a column in a ConcatTable. A ConcatTable is a table
  // referencing another table, usually as the result of a select, etc..
  // </etymology>

  // <synopsis> 
  // ConcatColumn handles the access of a column in a ConcatTable.
  // It calls the corresponding function in the referenced column
  // while converting the given row number to the row number in the
  // referenced table.
  // </synopsis> 

  // <motivation>
  // This class is untyped, i.e. not templated.
  // Every call is sent to the underlying referenced BaseColumn which
  // is typed by the virtual function mechanism.
  // A ConcatColumn can never be used directly. A user always has to
  // construct a typed ArrayColumn or ScalarColumn object to access a column.
  // This means everyting is fully type safe.
  // </motivation>

  // <todo asof="$DATE:$">
  //# A List of bugs, limitations, extensions or planned refinements.
  //   <li> Act upon removal of rows or the underlying column
  // </todo>


  class ConcatColumn : public BaseColumn
  {
  public:
    // Construct the ConcatColumn. It will point to the given column
    // description, ConcatTable and referenced column.
    // The ConcatTable will be used to convert the rownr to the rownr
    // in the referenced column.
    ConcatColumn (const BaseColumnDesc*, ConcatTable*);

    ~ConcatColumn();

    // Test if the column is writable in the parent table.
    virtual Bool isWritable() const;

    // Test if the column is stored (otherwise it is virtual).
    virtual Bool isStored() const;

    // Get access to the column keyword set.
    // The initial keyword set is a copy of the keyword set of the first table.
    // <group>
    virtual TableRecord& rwKeywordSet();
    virtual TableRecord& keywordSet();
    // </group>

    // Get nr of rows in the column.
    virtual uInt nrow() const;

    // Test if a value in a particular cell has been defined.
    virtual Bool isDefined (uInt rownr) const;

    // Set the shape of the array in the given row.
    virtual void setShape (uInt rownr, const IPosition& shape);

    // Set the shape and tile shape of the array in the given row.
    virtual void setShape (uInt rownr, const IPosition& shape,
			   const IPosition& tileShape);

    // Get the global #dimensions of an array (i.e. for all rows).
    virtual uInt ndimColumn() const;

    // Get the global shape of an array (i.e. for all rows).
    virtual IPosition shapeColumn() const;

    // Get the #dimensions of an array in a particular cell.
    virtual uInt ndim (uInt rownr) const;

    // Get the shape of an array in a particular cell.
    virtual IPosition shape (uInt rownr) const;

    // It can change shape if the underlying column can.
    virtual Bool canChangeShape() const;

    // It can handle a scalar column if the underlying column
    // can handle cells in a scalar column.
    virtual Bool canAccessScalarColumn (Bool& reask) const;

    // It can handle an array column if the underlying column
    // can handle cells in an array column.
    virtual Bool canAccessArrayColumn (Bool& reask) const;

    // It can handle a cell slice if the underlying column can do it.
    virtual Bool canAccessSlice (Bool& reask) const;

    // It can handle a column slice if the underlying column
    // can handle a collection of cells in a column and a column slice.
    virtual Bool canAccessColumnSlice (Bool& reask) const;

    // It can handle cells in a scalar column if the underlying column
    // can do it.
    virtual Bool canAccessScalarColumnCells (Bool& reask) const;

    // It can handle cells in an array column if the underlying column
    // can do it.
    virtual Bool canAccessArrayColumnCells (Bool& reask) const;

    // Initialize the rows from startRownr till endRownr (inclusive)
    // with the default value defined in the column description (if defined).
    void initialize (uInt startRownr, uInt endRownr);

    // Get the value from a particular cell.
    // This can be a scalar or an array.
    virtual void get (uInt rownr, void* dataPtr) const;

    // Get a slice of an N-dimensional array in a particular cell.
    virtual void getSlice (uInt rownr, const Slicer&, void* dataPtr) const;

    // Put the value in a particular cell.
    // This can be a scalar or an array.
    virtual void put (uInt rownr, const void* dataPtr);

    // Put a slice of an N-dimensional array in a particular cell.
    virtual void putSlice (uInt rownr, const Slicer&, const void* dataPtr);

    // Get the array of all array values in a column.
    // If the column contains n-dim arrays, the resulting array is (n+1)-dim.
    // The arrays in the column have to have the same shape in all cells.
    virtual void getArrayColumn (void* dataPtr) const;

    // Get subsections from all arrays in the column.
    // If the column contains n-dim arrays, the resulting array is (n+1)-dim.
    // The arrays in the column have to have the same shape in all cells.
    virtual void getColumnSlice (const Slicer&, void* dataPtr) const;

    // Get the array of some array values in a column.
    // If the column contains n-dim arrays, the resulting array is (n+1)-dim.
    // The arrays in the column have to have the same shape in all cells.
    virtual void getArrayColumnCells (const RefRows& rownrs,
				      void* dataPtr) const;

    // Get subsections from some arrays in the column.
    // If the column contains n-dim arrays, the resulting array is (n+1)-dim.
    // The arrays in the column have to have the same shape in all cells.
    virtual void getColumnSliceCells (const RefRows& rownrs,
				      const Slicer&, void* dataPtr) const;

    // Put the array of all array values in the column.
    // If the column contains n-dim arrays, the source array is (n+1)-dim.
    // The arrays in the column have to have the same shape in all cells.
    virtual void putArrayColumn (const void* dataPtr);

    // Put into subsections of all table arrays in the column.
    // If the column contains n-dim arrays, the source array is (n+1)-dim.
    // The arrays in the column have to have the same shape in all cells.
    virtual void putColumnSlice (const Slicer&, const void* dataPtr);

    // Get the array of some array values in a column.
    // If the column contains n-dim arrays, the resulting array is (n+1)-dim.
    // The arrays in the column have to have the same shape in all cells.
    virtual void putArrayColumnCells (const RefRows& rownrs,
				      const void* dataPtr);

    // Put subsections of some arrays in the column.
    // If the column contains n-dim arrays, the source array is (n+1)-dim.
    // The arrays in the column have to have the same shape in all cells.
    virtual void putColumnSliceCells (const RefRows& rownrs,
				      const Slicer&, const void* dataPtr);

    // Get the underlying column cache.
    virtual ColumnCache& columnCache();

    // Set the maximum cache size (in bytes) to be used by a storage manager.
    virtual void setMaximumCacheSize (uInt nbytes);

    // Allocate value buffers for the table iterator.
    // Also get a comparison function if undefined.
    // The function freeIterBuf must be called to free the buffers.
    virtual void allocIterBuf (void*& lastVal, void*& curVal,
			       ObjCompareFunc*& cmpFunc);

    // Free the value buffers allocated by allocIterBuf.
    virtual void freeIterBuf (void*& lastVal, void*& curVal);

  private:
    // Define the function to handle access to an entire column.
    typedef void AccessColumnFunc (BaseColumn* col,
				   const Slicer*, ArrayBase* array);

    // Define the function to handle access to a number of rows.
    typedef void AccessRowsFunc (BaseColumn* col, const RefRows& rows,
				  const Slicer*, ArrayBase* array);

    // Access the data for an entire column.
    void accessColumn (const Slicer* ns,
		       void* dataPtr,
		       AccessColumnFunc*) const;

    // Access the data with multiple rows combined.
    void accessRows (const RefRows& rownrs,
		     const Slicer* ns,
		     void* dataPtr,
		     AccessRowsFunc*) const;

    // Define the access functions.
    static void getColumnPart (BaseColumn* col,
			       const Slicer*, ArrayBase* arr);
    static void putColumnPart (BaseColumn* col,
			       const Slicer*, ArrayBase* arr);
    static void getColumnSlicePart (BaseColumn* col,
				    const Slicer* ns, ArrayBase* arr);
    static void putColumnSlicePart (BaseColumn* col,
				    const Slicer* ns, ArrayBase* arr);
    static void getRowsPart (BaseColumn* col, const RefRows& rows,
			     const Slicer*, ArrayBase* array);
    static void putRowsPart (BaseColumn* col, const RefRows& rows,
			     const Slicer*, ArrayBase* array);
    static void getRowsSlicePart (BaseColumn* col, const RefRows& rows,
				  const Slicer*, ArrayBase* array);
    static void putRowsSlicePart (BaseColumn* col, const RefRows& rows,
				  const Slicer*, ArrayBase* array);
    // </group>

  protected:
    // Set the column cache to the cache of the given table.
    // The row numbers will be adjusted as needed.
    void setColumnCache (uInt tableNr, const ColumnCache&) const;

    //# Data members
    ConcatTable*        refTabPtr_p;
    Block<BaseColumn*>  refColPtr_p;
    mutable ColumnCache colCache_p;
    TableRecord         keywordSet_p;
  };

} //# NAMESPACE CASA - END

#endif
