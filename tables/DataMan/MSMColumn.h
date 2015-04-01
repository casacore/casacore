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
//#
//# $Id$

#ifndef TABLES_MSMCOLUMN_H
#define TABLES_MSMCOLUMN_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/DataManager.h>
#include <casacore/tables/DataMan/StManColumn.h>
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


class MSMColumn: public StManColumn
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

  // Put a scalar value into the given row.
  // The buffer pointed to by dataPtr has to have the correct length
  // (which is guaranteed by the Scalar/ArrayColumn put function).
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

  // Get scalars from the given row on with a maximum of nrmax values.
  // This can be used to get an entire column of scalars or to get
  // a part of a column (for a cache for example).
  // The buffer pointed to by dataPtr has to have the correct length
  // (which is guaranteed by the ScalarColumn get function).
  // <group>
  uInt getBlockBoolV     (uInt rownr, uInt nrmax, Bool* dataPtr);
  uInt getBlockuCharV    (uInt rownr, uInt nrmax, uChar* dataPtr);
  uInt getBlockShortV    (uInt rownr, uInt nrmax, Short* dataPtr);
  uInt getBlockuShortV   (uInt rownr, uInt nrmax, uShort* dataPtr);
  uInt getBlockIntV      (uInt rownr, uInt nrmax, Int* dataPtr);
  uInt getBlockuIntV     (uInt rownr, uInt nrmax, uInt* dataPtr);
  uInt getBlockfloatV    (uInt rownr, uInt nrmax, float* dataPtr);
  uInt getBlockdoubleV   (uInt rownr, uInt nrmax, double* dataPtr);
  uInt getBlockComplexV  (uInt rownr, uInt nrmax, Complex* dataPtr);
  uInt getBlockDComplexV (uInt rownr, uInt nrmax, DComplex* dataPtr);
  uInt getBlockStringV   (uInt rownr, uInt nrmax, String* dataPtr);
  // </group>

  // Put nrmax scalars from the given row on.
  // This can be used to put an entire column of scalars or to put
  // a part of a column (for a cache for example).
  // The buffer pointed to by dataPtr has to have the correct length
  // (which is guaranteed by the ScalarColumn put function).
  // <group>
  void putBlockBoolV     (uInt rownr, uInt nrmax, const Bool* dataPtr);
  void putBlockuCharV    (uInt rownr, uInt nrmax, const uChar* dataPtr);
  void putBlockShortV    (uInt rownr, uInt nrmax, const Short* dataPtr);
  void putBlockuShortV   (uInt rownr, uInt nrmax, const uShort* dataPtr);
  void putBlockIntV      (uInt rownr, uInt nrmax, const Int* dataPtr);
  void putBlockuIntV     (uInt rownr, uInt nrmax, const uInt* dataPtr);
  void putBlockfloatV    (uInt rownr, uInt nrmax, const float* dataPtr);
  void putBlockdoubleV   (uInt rownr, uInt nrmax, const double* dataPtr);
  void putBlockComplexV  (uInt rownr, uInt nrmax, const Complex* dataPtr);
  void putBlockDComplexV (uInt rownr, uInt nrmax, const DComplex* dataPtr);
  void putBlockStringV   (uInt rownr, uInt nrmax, const String* dataPtr);
  // </group>

  // Get the scalar values in some cells of the column.
  // The buffer pointed to by dataPtr has to have the correct length.
  // (which is guaranteed by the ScalarColumn getColumnCells function).
  // The default implementation loops through all rows.
  // <group>
  virtual void getScalarColumnCellsBoolV     (const RefRows& rownrs,
					      Vector<Bool>* dataPtr);
  virtual void getScalarColumnCellsuCharV    (const RefRows& rownrs,
					      Vector<uChar>* dataPtr);
  virtual void getScalarColumnCellsShortV    (const RefRows& rownrs,
					      Vector<Short>* dataPtr);
  virtual void getScalarColumnCellsuShortV   (const RefRows& rownrs,
					      Vector<uShort>* dataPtr);
  virtual void getScalarColumnCellsIntV      (const RefRows& rownrs,
					      Vector<Int>* dataPtr);
  virtual void getScalarColumnCellsuIntV     (const RefRows& rownrs,
					      Vector<uInt>* dataPtr);
  virtual void getScalarColumnCellsfloatV    (const RefRows& rownrs,
					      Vector<float>* dataPtr);
  virtual void getScalarColumnCellsdoubleV   (const RefRows& rownrs,
					      Vector<double>* dataPtr);
  virtual void getScalarColumnCellsComplexV  (const RefRows& rownrs,
					      Vector<Complex>* dataPtr);
  virtual void getScalarColumnCellsDComplexV (const RefRows& rownrs,
					      Vector<DComplex>* dataPtr);
  virtual void getScalarColumnCellsStringV   (const RefRows& rownrs,
					      Vector<String>* dataPtr);
  // </group>

  // Add (newNrrow-oldNrrow) rows to the column.
  virtual void addRow (uInt newNrrow, uInt oldNrrow);

  // Resize the data blocks.
  // This adds an extension when needed.
  void resize (uInt nrval);

  // Remove the given row.
  // If no rows remain in the extension, the extension is also removed.
  virtual void remove (uInt rownr);

  // Create the number of rows in a new table.
  // This is used when a table gets created or opened.
  virtual void doCreate (uInt nrrow);

  // Check if the class invariants still hold.
  virtual Bool ok() const;

protected:
  MSMBase* stmanPtr_p;
  // The data type (for caching purposes).
  int dtype_p;
  // The data is indirectly accessed via a pointer (for the derived classes).
  Bool  byPtr_p;
  // The number of allocated rows in the column.
  uInt  nralloc_p;
  // The nr of extensions in use.
  uInt  nrext_p;
  // The assembly of all extensions (actually Block<T*>).
  Block<void*> data_p;
  // The cumulative nr of rows in all extensions.
  Block<uInt>  ncum_p;

  // Find the extension in which the row number is.
  // If the flag is true, it also sets the columnCache object.
  uInt findExt (uInt rownr, Bool setCache);

  // Get the next extension.
  // For the first iteration extnr should be zero.
  // It returns the number of values in it until the maximum is reached.
  // Zero means no more extensions.
  uInt nextExt (void*& ext, uInt& extnr, uInt nrmax) const;

  // Allocate an extension with the data type of the column.
  void* allocData (uInt nrval, Bool byPtr);

  // Delete all extensions.
  // Possible underlying data (as used by StManArrayColumnMemory)
  // will not be deleted and should have been deleted beforehand.
  void deleteAll();

  // Delete an extension.
  void deleteData (void* datap, Bool byPtr);

  // Remove an entry (i.e. a row) from an extension at the given index.
  // It will do this by shifting the rest (nrvalAfter elements)
  // one position to the left.
  void removeData (void* datap, uInt inx, uInt nrvalAfter);

  // Initialize the data (after an open).
  void initData (void* datap, uInt nrval);

  // Get the pointer for the given row.
  // This is for the derived classes like StManArrayColumnMemory.
  void* getArrayPtr (uInt rownr);

  // Put the pointer for the given row.
  // This is for the derived classes like StManArrayColumnMemory.
  void putArrayPtr (uInt rownr, void* dataPtr);

private:
  // Forbid copy constructor.
  MSMColumn (const MSMColumn&);

  // Forbid assignment.
  MSMColumn& operator= (const MSMColumn&);
};



} //# NAMESPACE CASACORE - END

#endif
