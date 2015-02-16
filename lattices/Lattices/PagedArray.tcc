//# PagedArray.cc: this defines the PagedArray class
//# Copyright (C) 1994,1995,1996,1997,1998,1999,2000,2001,2003
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This library is free software; you can redistribute it and/or modify it
//# under the terms of the GNU Library General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or(at your
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

#ifndef LATTICES_PAGEDARRAY_TCC
#define LATTICES_PAGEDARRAY_TCC


#include <casacore/lattices/Lattices/PagedArray.h>
#include <casacore/lattices/Lattices/PagedArrIter.h>
#include <casacore/lattices/Lattices/LatticeNavigator.h>
#include <casacore/lattices/Lattices/TiledShape.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/OS/File.h>
#include <casacore/casa/OS/Path.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/DataMan/TiledCellStMan.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/ColumnDesc.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/TableInfo.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T>
PagedArray<T>::PagedArray()
: itsIsClosed   (True),
  itsMarkDelete (False),
  itsWritable   (False)
{
  // Initializes all private data using their default consructor
}

template<class T>
PagedArray<T>::PagedArray (const TiledShape& shape, const String& filename) 
: itsColumnName (defaultColumn()),
  itsRowNumber  (defaultRow()),
  itsIsClosed   (True)
{
  makeTable(filename, Table::New);
  makeArray (shape);
  setTableType();
  DebugAssert (ok(), AipsError);
}

template<class T>
PagedArray<T>::PagedArray (const TiledShape& shape)
: itsColumnName (defaultColumn()),
  itsRowNumber  (defaultRow()),
  itsIsClosed   (True)
{
  Path filename=File::newUniqueName(String("./"), String("pagedArray"));
  makeTable (filename.absoluteName(), Table::Scratch);
  makeArray (shape);
  setTableType();
  DebugAssert (ok(), AipsError);
}

template<class T>
PagedArray<T>::PagedArray (const TiledShape& shape, Table& file)
: itsTable      (file),
  itsColumnName (defaultColumn()),
  itsRowNumber  (defaultRow()),
  itsIsClosed   (False),
  itsMarkDelete (False),
  itsWritable   (file.isWritable())
{
  makeArray (shape);
  setTableType();
  DebugAssert (ok(), AipsError);
}

template<class T>
PagedArray<T>::PagedArray (const TiledShape& shape, Table& file,
			   const String& columnName, uInt rowNumber)
: itsTable      (file),
  itsColumnName (columnName),
  itsRowNumber  (rowNumber),
  itsIsClosed   (False),
  itsMarkDelete (False),
  itsWritable   (file.isWritable())
{
  makeArray (shape);
  setTableType();
  DebugAssert (ok(), AipsError);
}

template<class T>
PagedArray<T>::PagedArray (const String& filename)
: itsTable      (filename),
  itsColumnName (defaultColumn()), 
  itsRowNumber  (defaultRow()),
  itsIsClosed   (False),
  itsMarkDelete (False),
  itsWritable   (False),
  itsArray      (itsTable, itsColumnName),
  itsAccessor   (itsTable, itsColumnName)
{
  DebugAssert (ok(), AipsError);
}

template<class T> PagedArray<T>::PagedArray (Table& file)
: itsTable      (file),
  itsColumnName (defaultColumn()), 
  itsRowNumber  (defaultRow()),
  itsIsClosed   (False),
  itsMarkDelete (False),
  itsWritable   (False),
  itsArray      (itsTable, itsColumnName),
  itsAccessor   (itsTable, itsColumnName)
{
  DebugAssert (ok(), AipsError);
}

template<class T>
PagedArray<T>::PagedArray (Table& file, const String& columnName,
			   uInt rowNumber)
: itsTable      (file),
  itsColumnName (columnName), 
  itsRowNumber  (rowNumber),
  itsIsClosed   (False),
  itsMarkDelete (False),
  itsWritable   (False),
  itsArray      (itsTable, itsColumnName),
  itsAccessor   (itsTable, itsColumnName)
{
  DebugAssert (ok(), AipsError);
}

template<class T>
PagedArray<T>::PagedArray (const PagedArray<T>& other)
: Lattice<T>(),
  itsTable      (other.itsTable),
  itsColumnName (other.itsColumnName), 
  itsRowNumber  (other.itsRowNumber),
  itsIsClosed   (other.itsIsClosed),
  itsMarkDelete (other.itsMarkDelete),
  itsTableName  (other.itsTableName),
  itsWritable   (other.itsWritable),
  itsLockOpt    (other.itsLockOpt),
  itsArray      (other.itsArray),
  itsAccessor   (other.itsAccessor)
{
  DebugAssert (ok(), AipsError);
}

template<class T>
PagedArray<T>::~PagedArray()
{
  // Reopen if marked for delete to force that the table files get removed.
  if (itsMarkDelete) {
    tempReopen();
  }
  // Only need to do something if really constructed.
  if (! itsTable.isNull()) {
    // Table may not be written if ref count > 1 - here we force a write.
    // (but only if it is not a scratch table).
    if (! itsTable.isMarkedForDelete()) {
      DebugAssert (ok(), AipsError);
      itsTable.flush();
    }
  }
}

template<class T>
PagedArray<T>& PagedArray<T>::operator= (const PagedArray<T>& other)
{
  if (this != &other) {
    itsTable      = other.itsTable;
    itsColumnName = other.itsColumnName;
    itsRowNumber  = other.itsRowNumber;
    itsIsClosed   = other.itsIsClosed;
    itsMarkDelete = other.itsMarkDelete;
    itsTableName  = other.itsTableName;
    itsWritable   = other.itsWritable;
    itsLockOpt    = other.itsLockOpt;
    itsArray.reference(other.itsArray);
    itsAccessor   = other.itsAccessor;
  }
  DebugAssert (ok(), AipsError);
  return *this;
}

template<class T>
Lattice<T>* PagedArray<T>::clone() const
{
  return new PagedArray<T> (*this);
}

template<class T>
Bool PagedArray<T>::isPersistent() const
{
  return True;
}

template<class T>
Bool PagedArray<T>::isPaged() const
{
  return True;
}

template<class T>
Bool PagedArray<T>::isWritable() const
{
  // PagedArray is writable if underlying table is already open for write
  // or if the underlying table is in principle writable.
  if (itsIsClosed) {
    return  (itsWritable  ||
		   Table::isWritable (itsTableName));
  }
  return  (itsTable.isWritable()  ||
		 Table::isWritable (itsTable.tableName()));
}

template<class T>
const String& PagedArray<T>::tableName() const
{
  // Make sure the table is open, so it knows about a possible rename
  // (e.g. if an LCPagedMask (which uses Lattice<Bool>) gets renamed).
  return table().tableName();
}

template <class T> 
String PagedArray<T>::name (Bool stripPath) const 
{
   Path path(tableName());
   if (!stripPath) {
      return path.absoluteName();
   } 
   return path.baseName();
}

template<class T>
IPosition PagedArray<T>::shape() const
{
  DebugAssert (ok(), AipsError);
  doReopen();
  return itsArray.shape (itsRowNumber);
}

template<class T>
void PagedArray<T>::resize (const TiledShape& newShape)
{
  IPosition tileShape = newShape.tileShape();
  getRWArray().setShape (itsRowNumber, newShape.shape(), tileShape);
}

template<class T>
Bool PagedArray<T>::doGetSlice (Array<T>& buffer, const Slicer& section)
{
  doReopen();
  itsArray.getSlice (itsRowNumber, section, buffer, True);
  return False;
}

template<class T>
void PagedArray<T>::doPutSlice (const Array<T>& sourceArray, 
				const IPosition& where,
				const IPosition& stride)
{
  // Create a writable column object in case not existing yet.
  getRWArray();
  const uInt arrDim = sourceArray.ndim();
  const uInt latDim = ndim();
  AlwaysAssert(arrDim <= latDim, AipsError);
  if (arrDim == latDim) {
    Slicer section(where, sourceArray.shape(), stride, Slicer::endIsLength); 
    itsArray.putSlice (itsRowNumber, section, sourceArray);
  } else {
    Array<T> degenerateArr(sourceArray.addDegenerate(latDim-arrDim));
    Slicer section(where, degenerateArr.shape(), stride, Slicer::endIsLength); 
    itsArray.putSlice (itsRowNumber, section, degenerateArr);
  } 
}

template<class T>
IPosition PagedArray<T>::tileShape() const
{
  doReopen();
  return itsAccessor.tileShape (itsRowNumber);
}

template<class T>
uInt PagedArray<T>::advisedMaxPixels() const
{
  return tileShape().product();
}

template<class T>
IPosition PagedArray<T>::doNiceCursorShape (uInt maxPixels) const
{
  IPosition retval = tileShape();
  if (retval.product() > Int(maxPixels)) {
    retval = Lattice<T>::doNiceCursorShape(maxPixels);
  }
  return retval;
}

template<class T>
void PagedArray<T>::setMaximumCacheSize (uInt howManyPixels)
{
  doReopen();
  const uInt sizeInBytes = howManyPixels * sizeof(T);
  itsAccessor.setMaximumCacheSize (sizeInBytes);
}

template<class T>
uInt PagedArray<T>::maximumCacheSize() const
{
  doReopen();
  return itsAccessor.maximumCacheSize() / sizeof(T);
}

template<class T>
void PagedArray<T>::setCacheSizeInTiles (uInt howManyTiles)
{
  doReopen();
  itsAccessor.setCacheSize (itsRowNumber, howManyTiles);
}

template<class T>
void PagedArray<T>::setCacheSizeFromPath (const IPosition& sliceShape,
					  const IPosition& windowStart,
					  const IPosition& windowLength,
					  const IPosition& axisPath)
{
  doReopen();
  itsAccessor.setCacheSize (itsRowNumber, sliceShape, windowStart,
			    windowLength, axisPath, True);
}

template<class T>
void PagedArray<T>::clearCache()
{
  doReopen();
  itsAccessor.clearCaches();
}

template<class T>
void PagedArray<T>::showCacheStatistics (ostream& os) const
{
  doReopen();
  itsAccessor.showCacheStatistics (os);
}

template<class T>
T PagedArray<T>::getAt(const IPosition& where) const
{
  doReopen();
  // Use a temporary 1-element array with the correct dimensionality.
  const IPosition shape(where.nelements(),1);
  T value;
  Array<T> buffer (shape, &value, SHARE);
  itsArray.getSlice (itsRowNumber, Slicer(where,shape), buffer);
  return value;
}

template<class T>
void PagedArray<T>::putAt (const T& value, const IPosition& where)
{
  // Use a temporary 1-element array with the correct dimensionality.
  const IPosition shape(where.nelements(),1);
  Array<T> buffer (shape, &value);
  getRWArray().putSlice (itsRowNumber, Slicer(where,shape), buffer);
}

template<class T>
Bool PagedArray<T>::ok() const
{
  if (itsIsClosed) {
    if (itsTable.isNull() == False) {
      throw AipsError ("PagedArray::ok - "
	               "Table associated with closed PagedArray");
      return False;
    }
  } else {
    if (itsTable.isNull() == True) {
      throw AipsError ("PagedArray::ok - "
             "No Table associated with the PagedArray");
      return False;
    }
    if (itsArray.isNull() == True) {
      throw AipsError ("PagedArray::ok - "
		       "No Array associated with the PagedArray");
      return False;
    }
    if (itsRowNumber > itsTable.nrow()) {
      throw AipsError ("PagedArray::ok - "
		       "Row number is too big for the current Table");
      return False;
    }
  }
  if (itsColumnName.length() == 0) {
      throw AipsError ("PagedArray::ok - "
		       "Column name cannot by empty");
    return False;
  }
  return True;
}


template<class T>
LatticeIterInterface<T>* PagedArray<T>::makeIter (const LatticeNavigator& nav,
						  Bool useRef) const
{
  return new PagedArrIter<T>(*this, nav, useRef);
}


template <class T>
void PagedArray<T>::makeArray (const TiledShape& shape)
{
  doReopen();
  // Make sure the table is writable.
  itsTable.reopenRW();
  // Get the lattice shape and tile shape.
  IPosition latShape  = shape.shape();
  IPosition tileShape = shape.tileShape();
  // Create a new column if it does not already exist.
  const uInt ndim = latShape.nelements();
  Bool newColumn = False;
  if (!itsTable.tableDesc().isColumn(itsColumnName)) {
    newColumn = True;
    // To build the column a table description must be created
    TableDesc description;
    description.addColumn(ArrayColumnDesc<T>(itsColumnName,
					     defaultComment(), 
					     ndim));
    description.defineHypercolumn(itsColumnName, ndim, 
				  stringToVector(itsColumnName));
    TiledCellStMan stman(itsColumnName, tileShape);
    itsTable.addColumn(description, stman);
  }

  // Attach the default constructed ArrayColumn to the Table
  itsArray.attach (itsTable, itsColumnName);

  // if table doesn't have enough rows to match our row number
  // then add rows and fill them with empty arrays
  const IPosition emptyShape(ndim, 1);
  const uInt rows = itsTable.nrow();
  if (rows <= itsRowNumber) {
    itsTable.addRow (itsRowNumber-rows+1);
    for (uInt r = rows; r < itsRowNumber; r++) {
      itsArray.setShape(r, emptyShape);
    }
  }
  if (newColumn) {
    for (uInt r = 0; r < rows; r++) {
      if (r != itsRowNumber) {
	itsArray.setShape(r, emptyShape);
      }
    }
  }
  // set a shape of the PagedArray
  itsArray.setShape(itsRowNumber, latShape);
  // create the accessor object
  itsAccessor = ROTiledStManAccessor (itsTable, itsColumnName);
}

template<class T>
void PagedArray<T>::setTableType()
{
  AlwaysAssert (!itsTable.isNull(), AipsError);
  TableInfo& info(itsTable.tableInfo());
  {
    const String reqdType = info.type (TableInfo::PAGEDARRAY);
    if (info.type() != reqdType) {
      info.setType (reqdType);
    }
  }
  {
    const String reqdSubType = info.subType (TableInfo::PAGEDARRAY);
    if (info.subType() != reqdSubType) {
      info.setSubType (reqdSubType);
    }
  }
}

template<class T>
void PagedArray<T>::makeTable (const String& filename,
			       Table::TableOption option)
{
  SetupNewTable setupTable(filename, TableDesc(), option);
  itsTable = Table(setupTable);
  itsIsClosed   = False;
  itsMarkDelete = False;
  itsWritable   = True;
}

template<class T>
String PagedArray<T>::defaultComment()
{
  return String("version 4.0");
}

template<class T>
Bool PagedArray<T>::lock (FileLocker::LockType type, uInt nattempts)
{
  doReopen();
  return itsTable.lock (type, nattempts);
}
template<class T>
void PagedArray<T>::unlock()
{
  if (!itsIsClosed) {
    itsTable.unlock();
  }
}
template<class T>
Bool PagedArray<T>::hasLock (FileLocker::LockType type) const
{
  return (itsIsClosed  ?  False : itsTable.hasLock (type));
}
template<class T>
void PagedArray<T>::resync()
{
  if (!itsIsClosed) {
    itsTable.resync();
  }
}
template<class T>
void PagedArray<T>::flush()
{
  if (!itsIsClosed) {
    itsTable.flush();
  }
}
template<class T>
void PagedArray<T>::tempClose()
{
  if (!itsIsClosed) {
    itsTable.flush();
    itsTableName = itsTable.tableName();
    itsWritable  = itsTable.isWritable();
    itsLockOpt   = itsTable.lockOptions();
    // Take care that table does not get deleted on temporary close.
    if (itsTable.isMarkedForDelete()) {
      itsMarkDelete = True;
      itsTable.unmarkForDelete();
    }
    itsTable = Table();
    itsArray.reference (ArrayColumn<T>());
    itsIsClosed = True;
  }
}

template<class T>
void PagedArray<T>::reopen()
{
  doReopen();
}

template<class T>
void PagedArray<T>::tempReopen() const
{
  if (itsIsClosed) {
    if (itsWritable) {
      itsTable = Table (itsTableName, itsLockOpt, Table::Update);
    } else {
      itsTable = Table (itsTableName, itsLockOpt);
    }
    itsArray.attach (itsTable, itsColumnName);
    itsAccessor = ROTiledStManAccessor (itsTable, itsColumnName);
    itsIsClosed = False;
    // Mark the table for delete if needed.
    if (itsMarkDelete) {
      itsTable.markForDelete();
      itsMarkDelete = False;
    }
  }
}

} //# NAMESPACE CASACORE - END

#endif
