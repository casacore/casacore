//# MSMColumn.h: A column in the MemoryStMan
//# Copyright (C) 2003
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

#ifndef TABLES_MSMCOLUMN_H
#define TABLES_MSMCOLUMN_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/StManColumnBase.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations
class MSMBase;


// <summary>
// Column in the Memory table storage manager class
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=StManColumn>StManColumn</linkto>
//   <li> <linkto class=MemoryStMan>MemoryStMan</linkto>
// </prerequisite>

// <etymology>
// MSMColumn handles a column for the memory-based storage manager.
// </etymology>

// <synopsis> 
// MSMColumn is used by MemoryStMan to handle the access to
// the data in a table column.
// It is an storage manager based in memory. Thus the data is lost
// when the table is closed.
// On reopen it will be initialized to the default column value.
// It fully supports addition and removal of rows.
//
// MSMColumn serves 2 purposes:
// <ol>
// <li> It handles a column containing scalar values.
// <li> It serves as a base class for MSMDirColumn and MSMIndColumn
//        These classes handle arrays and use MSMColumn to hold a pointer
//        to the array in each row.
// </ol>
//
// MSMColumn does not hold a column as a consecutive array,
// because extending the column (i.e. adding rows) proved be too
// expensive due to the repeated copying involved when creating a table
// (this method was used by the first version of the table system).
// Instead it has a number of data blocks (extensions) indexed to by a
// super block. Accessing a row means finding the appropriate extension
// via a binary search. Because there is only 1 extension when a table is
// read back, the overhead in finding a row is small.
// </synopsis> 

// <motivation>
// MSMColumn handles the standard data types. The class
// is not templated, but a switch statement is used instead.
// Templates would cause too many instantiations.
// </motivation>

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
// <li> StManAipsIO should use this class
// </todo>


class MSMColumn: public StManColumnBase
{
public:
  // Create a column of the given type.
  // It will maintain a pointer to its parent storage manager.
  MSMColumn (MSMBase* smptr, int dataType, Bool byPtr);

  // Frees up the storage.
  virtual ~MSMColumn();

  // Get a scalar value in the given row.
  // The buffer pointed to by dataPtr has to have the correct length
  // (which is guaranteed by the Scalar/ArrayColumn get function).
  // <group>
  virtual void getBool     (rownr_t rownr, Bool* dataPtr);
  virtual void getuChar    (rownr_t rownr, uChar* dataPtr);
  virtual void getShort    (rownr_t rownr, Short* dataPtr);
  virtual void getuShort   (rownr_t rownr, uShort* dataPtr);
  virtual void getInt      (rownr_t rownr, Int* dataPtr);
  virtual void getuInt     (rownr_t rownr, uInt* dataPtr);
  virtual void getInt64    (rownr_t rownr, Int64* dataPtr);
  virtual void getfloat    (rownr_t rownr, float* dataPtr);
  virtual void getdouble   (rownr_t rownr, double* dataPtr);
  virtual void getComplex  (rownr_t rownr, Complex* dataPtr);
  virtual void getDComplex (rownr_t rownr, DComplex* dataPtr);
  virtual void getString   (rownr_t rownr, String* dataPtr);
  // </group>

  // Put a scalar value into the given row.
  // The buffer pointed to by dataPtr has to have the correct length
  // (which is guaranteed by the Scalar/ArrayColumn put function).
  // <group>
  virtual void putBool     (rownr_t rownr, const Bool* dataPtr);
  virtual void putuChar    (rownr_t rownr, const uChar* dataPtr);
  virtual void putShort    (rownr_t rownr, const Short* dataPtr);
  virtual void putuShort   (rownr_t rownr, const uShort* dataPtr);
  virtual void putInt      (rownr_t rownr, const Int* dataPtr);
  virtual void putuInt     (rownr_t rownr, const uInt* dataPtr);
  virtual void putInt64    (rownr_t rownr, const Int64* dataPtr);
  virtual void putfloat    (rownr_t rownr, const float* dataPtr);
  virtual void putdouble   (rownr_t rownr, const double* dataPtr);
  virtual void putComplex  (rownr_t rownr, const Complex* dataPtr);
  virtual void putDComplex (rownr_t rownr, const DComplex* dataPtr);
  virtual void putString   (rownr_t rownr, const String* dataPtr);
  // </group>

  // Get all scalar values in the column.
  // The vector given in <src>data</src> has to have the correct length
  // (which is guaranteed by the ScalarColumn getColumn function).
  virtual void getScalarColumnV (ArrayBase& data);

  // Put all scalar values in the column.
  // The vector given in <src>data</src> has to have the correct length
  // (which is guaranteed by the ScalarColumn putColumn function).
  virtual void putScalarColumnV (const ArrayBase& data);

  // Add (newNrrow-oldNrrow) rows to the column.
  virtual void addRow (rownr_t newNrrow, rownr_t oldNrrow);

  // Resize the data blocks.
  // This adds an extension when needed.
  void resize (rownr_t nrval);

  // Remove the given row.
  // If no rows remain in the extension, the extension is also removed.
  virtual void remove (rownr_t rownr);

  // Create the number of rows in a new table.
  // This is used when a table gets created or opened.
  virtual void doCreate (rownr_t nrrow);

  // Make it possible to write the column data.
  // It is only used by derived classes.
  virtual void putFile (rownr_t nrval, AipsIO&);

  // Make it possible to read the column data.
  // It is only used by derived classes.
  virtual void getFile (rownr_t nrval, AipsIO&);

  // Reopen the storage manager files for read/write.
  virtual void reopenRW();

  // Check if the class invariants still hold.
  virtual Bool ok() const;

protected:
  MSMBase* stmanPtr_p;
  // The data is indirectly accessed via a pointer (for the derived classes).
  Bool     byPtr_p;
  // The number of allocated rows in the column.
  rownr_t  nralloc_p;
  // The nr of extensions in use.
  uInt     nrext_p;
  // The assembly of all extensions (actually Block<T*>).
  Block<void*> data_p;
  // The cumulative nr of rows in all extensions.
  Block<rownr_t> ncum_p;

  // Find the extension in which the row number is.
  // If the flag is true, it also sets the columnCache object.
  uInt findExt (rownr_t rownr, Bool setCache);

  // Allocate an extension with the data type of the column.
  void* allocData (rownr_t nrval, Bool byPtr);

  // Delete all extensions.
  // Possible underlying data (as used by StManArrayColumnMemory)
  // will not be deleted and should have been deleted beforehand.
  void deleteAll();

  // Delete an extension.
  void deleteData (void* datap, Bool byPtr);

  // Remove an entry (i.e. a row) from an extension at the given index.
  // It will do this by shifting the rest (nrvalAfter elements)
  // one position to the left.
  void removeData (void* datap, rownr_t inx, rownr_t nrvalAfter);

  // Initialize the data (after an open).
  virtual void initData (void* datap, rownr_t nrval);

  // Get the pointer for the given row.
  // This is for the derived classes like StManArrayColumnMemory.
  void* getArrayPtr (rownr_t rownr);

  // Put the pointer for the given row.
  // This is for the derived classes like StManArrayColumnMemory.
  void putArrayPtr (rownr_t rownr, void* dataPtr);

private:
  // Forbid copy constructor.
  MSMColumn (const MSMColumn&);

  // Forbid assignment.
  MSMColumn& operator= (const MSMColumn&);
};



} //# NAMESPACE CASACORE - END

#endif
