//# PagedArray.cc: this defines the PagedArray class
//# Copyright (C) 1994,1995,1996,1997,1998,1999,2000
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


#include <aips/Lattices/PagedArray.h>
#include <aips/Lattices/PagedArrIter.h>
#include <aips/Lattices/LatticeNavigator.h>
#include <aips/Lattices/TiledShape.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/ArrayUtil.h>
#include <aips/Exceptions/Error.h>
#include <aips/Arrays/Slicer.h>
#include <aips/Arrays/IPosition.h>
#include <aips/Logging/LogOrigin.h>
#include <aips/OS/File.h>
#include <aips/OS/Path.h>
#include <aips/Tables/SetupNewTab.h>
#include <aips/Tables/TiledCellStMan.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/ColumnDesc.h>
#include <aips/Tables/ArrColDesc.h>
#include <aips/Tables/TableInfo.h>
#include <aips/Utilities/Assert.h>
#include <iostream.h>


template<class T>
PagedArray<T>::PagedArray()
: itsIsClosed (True),
  itsWritable (False)
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
  AlwaysAssert(ok() == True, AipsError);
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
  AlwaysAssert(ok() == True, AipsError);
}

template<class T>
PagedArray<T>::PagedArray (const TiledShape& shape, Table& file)
: itsTable      (file),
  itsColumnName (defaultColumn()),
  itsRowNumber  (defaultRow()),
  itsIsClosed   (False),
  itsWritable   (file.isWritable())
{
  makeArray (shape);
  setTableType();
  AlwaysAssert(ok() == True, AipsError);
}

template<class T>
PagedArray<T>::PagedArray (const TiledShape& shape, Table& file,
			   const String& columnName, uInt rowNumber)
: itsTable      (file),
  itsColumnName (columnName),
  itsRowNumber  (rowNumber),
  itsIsClosed   (False)
{
  makeArray (shape);
  setTableType();
  AlwaysAssert(ok() == True, AipsError);
}

template<class T>
PagedArray<T>::PagedArray (const String& filename)
: itsTable      (filename),
  itsColumnName (defaultColumn()), 
  itsRowNumber  (defaultRow()),
  itsIsClosed   (False),
  itsROArray    (itsTable, itsColumnName),
  itsAccessor   (itsTable, itsColumnName)
{
  AlwaysAssert(ok() == True, AipsError);
}

template<class T> PagedArray<T>::PagedArray (Table& file)
: itsTable      (file),
  itsColumnName (defaultColumn()), 
  itsRowNumber  (defaultRow()),
  itsIsClosed   (False),
  itsROArray    (itsTable, itsColumnName),
  itsAccessor   (itsTable, itsColumnName)
{
  AlwaysAssert(ok() == True, AipsError);
}

template<class T>
PagedArray<T>::PagedArray (Table& file, const String& columnName,
			   uInt rowNumber)
: itsTable      (file),
  itsColumnName (columnName), 
  itsRowNumber  (rowNumber),
  itsIsClosed   (False),
  itsROArray    (itsTable, itsColumnName),
  itsAccessor   (itsTable, itsColumnName)
{
  AlwaysAssert(ok() == True, AipsError);
}

template<class T>
PagedArray<T>::PagedArray (const PagedArray<T>& other)
: itsTable      (other.itsTable),
  itsColumnName (other.itsColumnName), 
  itsRowNumber  (other.itsRowNumber),
  itsIsClosed   (other.itsIsClosed),
  itsTableName  (other.itsTableName),
  itsWritable   (other.itsWritable),
  itsLockOpt    (other.itsLockOpt),
  itsRWArray    (other.itsRWArray),
  itsROArray    (other.itsROArray),
  itsAccessor   (other.itsAccessor)
{
  DebugAssert(ok() == True, AipsError);
}

template<class T>
PagedArray<T>::~PagedArray()
{
  // Only need to do something if really constructed.
  if (! itsTable.isNull()) {
    // Table may not be written if ref count > 1 - here we force a write.
    DebugAssert(ok() == True, AipsError);
    itsTable.flush();
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
    itsTableName  = other.itsTableName;
    itsWritable   = other.itsWritable;
    itsLockOpt    = other.itsLockOpt;
    itsROArray.reference(other.itsROArray);
    itsRWArray.reference(other.itsRWArray);
    itsAccessor   = other.itsAccessor;
  }
  DebugAssert(ok() == True, AipsError);
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
    return ToBool (itsWritable  ||
		   Table::isWritable (itsTableName));
  }
  return ToBool (itsTable.isWritable()  ||
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
  DebugAssert(ok() == True, AipsError);
  doReopen();
  return itsROArray.shape (itsRowNumber);
}

template<class T>
void PagedArray<T>::resize (const TiledShape& newShape)
{
  itsLog << LogOrigin("PagedArray<T>", 
		      "resize(const TiledShape& shape)");
  IPosition tileShape = newShape.tileShape();
  getRWArray().setShape (itsRowNumber, newShape.shape(), tileShape);
  itsLog << LogIO::DEBUGGING
	 << "Resizing the PagedArray to shape " << newShape.shape()
	 << " with a tile shape of " << tileShape << endl
	 << " in row " << itsRowNumber << " and column '" << itsColumnName
	 << "' of the Table '" << tableName() << "'" << LogIO::POST;
}

template<class T>
Bool PagedArray<T>::doGetSlice (Array<T>& buffer, const Slicer& section)
{
  doReopen();
  itsROArray.getSlice (itsRowNumber, section, buffer, True);
  return False;
}

template<class T>
void PagedArray<T>::doPutSlice (const Array<T>& sourceArray, 
				const IPosition& where,
				const IPosition& stride)
{
  // Create a writable column object when not existing yet.
  getRWArray();
  const uInt arrDim = sourceArray.ndim();
  const uInt latDim = ndim();
  AlwaysAssert(arrDim <= latDim, AipsError);
  if (arrDim == latDim) {
    Slicer section(where, sourceArray.shape(), stride, Slicer::endIsLength); 
    itsRWArray.putSlice (itsRowNumber, section, sourceArray);
  } else {
    Array<T> degenerateArr(sourceArray.addDegenerate(latDim-arrDim));
    Slicer section(where, degenerateArr.shape(), stride, Slicer::endIsLength); 
    itsRWArray.putSlice (itsRowNumber, section, degenerateArr);
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
  itsROArray.getSlice (itsRowNumber, Slicer(where,shape), buffer);
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
      LogIO logErr(LogOrigin("PagedArray<T>", "ok()"));
      logErr << LogIO::SEVERE << "Table associated with closed PagedArray"
	     << LogIO::POST;
      return False;
    }
  } else {
    if (itsTable.isNull() == True) {
      LogIO logErr(LogOrigin("PagedArray<T>", "ok()"));
      logErr << LogIO::SEVERE << "No Table associated with the PagedArray"
	     << LogIO::POST;
      return False;
    }
    if (itsROArray.isNull() == True) {
      LogIO logErr(LogOrigin("PagedArray<T>", "ok()"));
      logErr  << LogIO::SEVERE << "No Array associated with the PagedArray"
	      << LogIO::POST;
      return False;
    }
    if (itsRowNumber > itsTable.nrow()) {
      LogIO logErr(LogOrigin("PagedArray<T>", "ok()"));
      logErr << LogIO::SEVERE << "Row number is too big for the current Table"
	     << LogIO::POST;
      return False;
    }
  }
  if (itsColumnName.length() == 0) {
    LogIO logErr(LogOrigin("PagedArray<T>", "ok()"));
    logErr << LogIO::SEVERE << "Column name cannot by empty" << LogIO::POST;
    return False;
  }
  return True;
}


template<class T>
LatticeIterInterface<T>* PagedArray<T>::makeIter
                                (const LatticeNavigator& navigator) const
{
  return new PagedArrIter<T>(*this, navigator);
}


template <class T>
void PagedArray<T>::makeArray (const TiledShape& shape)
{
  doReopen();
  // Make sure the table is writable.
  itsTable.reopenRW();
  itsLog << LogOrigin("PagedArray<T>", 
		      "makeArray(const TiledShape& shape)");
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
    itsTable.addColumn(description, TiledCellStMan(itsColumnName, tileShape));
  }

  // Attach the default constructed ArrayColumn to the Table
  itsROArray.attach (itsTable, itsColumnName);
  itsRWArray.attach (itsTable, itsColumnName);

  // if table doesn't have enough rows to match our row number
  // then add rows and fill them with empty arrays
  const IPosition emptyShape(ndim, 1);
  const uInt rows = itsTable.nrow();
  if (rows <= itsRowNumber) {
    itsTable.addRow (itsRowNumber-rows+1);
    for (uInt r = rows; r < itsRowNumber; r++) {
      itsRWArray.setShape(r, emptyShape);
    }
  }
  if (newColumn) {
    for (uInt r = 0; r < rows; r++) {
      if (r != itsRowNumber) {
	itsRWArray.setShape(r, emptyShape);
      }
    }
  }
  // set a shape of the PagedArray
  itsRWArray.setShape(itsRowNumber, latShape);
  // create the accessor object
  itsAccessor = ROTiledStManAccessor (itsTable, itsColumnName);
  itsLog << LogIO::DEBUGGING
         << "Created PagedArray of shape " << latShape
         << " with a tile shape of " << tileShape << endl
         << " in row " << itsRowNumber << " and column '" << itsColumnName
         << "' of the Table '" << tableName() << "'" << LogIO::POST;
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
  itsIsClosed = False;
  itsWritable = True;
}

template<class T>
String PagedArray<T>::defaultComment()
{
  return String("version 4.0");
}

template<class T>
void PagedArray<T>::makeRWArray()
{
  doReopen();
  itsTable.reopenRW();
  itsRWArray.attach (itsTable, itsColumnName);
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
    itsTable     = Table();
    itsROArray.reference (ROArrayColumn<T>());
    itsRWArray.reference (ArrayColumn<T>());
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
      itsRWArray.attach (itsTable, itsColumnName);
    } else {
      itsTable = Table (itsTableName, itsLockOpt);
    }
    itsROArray.attach (itsTable, itsColumnName);
    itsAccessor = ROTiledStManAccessor (itsTable, itsColumnName);
    itsIsClosed = False;
  }
}
