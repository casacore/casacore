//# PagedArray.cc: this defines the PagedArray class
//# Copyright (C) 1994,1995,1996,1997,1998
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

#include <aips/aips.h>

#include <trial/Lattices/PagedArray.h>
#include <trial/Lattices/PagedArrIter.h>
#include <trial/Lattices/LatticeNavigator.h>
#include <trial/Lattices/TiledShape.h>

#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/ArrayUtil.h>
#include <aips/Exceptions/Error.h>
#include <aips/Lattices/Slicer.h>
#include <aips/Lattices/IPosition.h>
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
{
  // Initializes all private data using their default consructor
}

template<class T>
PagedArray<T>::PagedArray (const TiledShape& shape, const String& filename) 
: itsColumnName (defaultColumn()),
  itsRowNumber  (defaultRow()) 
{
  makeTable(filename, Table::New);
  itsTable.makePermanent();      // avoid double deletion by Cleanup
  makeArray (shape);
  setTableType();
  AlwaysAssert(ok() == True, AipsError);
}

template<class T>
PagedArray<T>::PagedArray (const TiledShape& shape)
: itsColumnName (defaultColumn()),
  itsRowNumber  (defaultRow())
{
  Path filename=File::newUniqueName(String("./"), String("pagedArray"));
  makeTable (filename.absoluteName(), Table::Scratch);
  itsTable.makePermanent();      // avoid double deletion by Cleanup
  makeArray (shape);
  setTableType();
  AlwaysAssert(ok() == True, AipsError);
}

template<class T>
PagedArray<T>::PagedArray (const TiledShape& shape, Table& file)
: itsTable      (file),
  itsColumnName (defaultColumn()),
  itsRowNumber  (defaultRow()) 
{
  itsTable.makePermanent();      // avoid double deletion by Cleanup
  makeArray (shape);
  setTableType();
  AlwaysAssert(ok() == True, AipsError);
}

template<class T>
PagedArray<T>::PagedArray (const TiledShape& shape, Table& file,
			   const String& columnName, uInt rowNumber)
: itsTable      (file),
  itsColumnName (columnName),
  itsRowNumber  (rowNumber)
{
  itsTable.makePermanent();      // avoid double deletion by Cleanup
  makeArray (shape);
  setTableType();
  AlwaysAssert(ok() == True, AipsError);
}

template<class T>
PagedArray<T>::PagedArray (const String& filename)
: itsTable      (filename),
  itsColumnName (defaultColumn()), 
  itsRowNumber  (defaultRow()),
  itsROArray    (itsTable, itsColumnName),
  itsAccessor   (itsTable, itsColumnName)
{
  itsTable.makePermanent();      // avoid double deletion by Cleanup
  AlwaysAssert(ok() == True, AipsError);
}

template<class T> PagedArray<T>::PagedArray (Table& file)
: itsTable      (file),
  itsColumnName (defaultColumn()), 
  itsRowNumber  (defaultRow()),
  itsROArray    (itsTable, itsColumnName),
  itsAccessor   (itsTable, itsColumnName)
{
  itsTable.makePermanent();      // avoid double deletion by Cleanup
  AlwaysAssert(ok() == True, AipsError);
}

template<class T>
PagedArray<T>::PagedArray (Table& file, const String& columnName,
			   uInt rowNumber)
: itsTable      (file),
  itsColumnName (columnName), 
  itsRowNumber  (rowNumber),
  itsROArray    (itsTable, itsColumnName),
  itsAccessor   (itsTable, itsColumnName)
{
  itsTable.makePermanent();      // avoid double deletion by Cleanup
  AlwaysAssert(ok() == True, AipsError);
}

template<class T>
PagedArray<T>::PagedArray (const PagedArray<T>& other)
: itsTable      (other.itsTable),
  itsColumnName (other.itsColumnName), 
  itsRowNumber  (other.itsRowNumber),
  itsRWArray    (other.itsRWArray),
  itsROArray    (other.itsROArray),
  itsAccessor   (other.itsAccessor)
{
  itsTable.makePermanent();      // avoid double deletion by Cleanup
  DebugAssert(ok() == True, AipsError);
}

template<class T>
PagedArray<T>::~PagedArray()
{
  // Table may not be written if ref count > 1 - here we force a write.
  DebugAssert(ok() == True, AipsError);
  itsTable.flush();
}

template<class T>
PagedArray<T>& PagedArray<T>::operator= (const PagedArray<T>& other)
{
  if (this != &other) {
    itsTable      = other.itsTable;
    itsTable.makePermanent();      // avoid double deletion by Cleanup
    itsColumnName = other.itsColumnName;
    itsRowNumber  = other.itsRowNumber;
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
Bool PagedArray<T>::isPaged() const
{
  return True;
}

template<class T>
Bool PagedArray<T>::isWritable() const
{
  // PagedArray is writable if underlying table is already open for write
  // or if the underlying table is in principle writable.
  return ToBool (itsTable.isWritable()  ||
		 Table::isWritable (itsTable.tableName()));
}

template<class T>
IPosition PagedArray<T>::shape() const
{
  DebugAssert(ok() == True, AipsError);
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
const String& PagedArray<T>::tableName() const
{
  return itsTable.tableName();
}

template<class T>
const String& PagedArray<T>::columnName() const
{
  return itsColumnName;
}

template<class T>
String PagedArray<T>::defaultColumn()
{
  return "PagedArray";
}

template<class T>
const ROTiledStManAccessor& PagedArray<T>::accessor() const
{
  return itsAccessor;
}

template<class T>
uInt PagedArray<T>::rowNumber() const
{
  return itsRowNumber;
}

template<class T>
uInt PagedArray<T>::defaultRow()
{
  return 0;
}

template<class T>
IPosition PagedArray<T>::tileShape() const
{
  return itsAccessor.tileShape (itsRowNumber);
}

template<class T>
uInt PagedArray<T>::maxPixels() const
{
  return tileShape().product();
}

template<class T>
IPosition PagedArray<T>::doNiceCursorShape (uInt maxPixels) const
{
  IPosition retval = tileShape();
  if (retval.product() > Int(maxPixels)) {
    retval = Lattice<T>::niceCursorShape(maxPixels);
  }
  return retval;
}

template<class T>
void PagedArray<T>::setMaximumCacheSize (uInt howManyPixels)
{
  const uInt sizeInBytes = howManyPixels * sizeof(T);
  itsAccessor.setMaximumCacheSize (sizeInBytes);
}

template<class T>
uInt PagedArray<T>::maximumCacheSize() const
{
  return itsAccessor.maximumCacheSize() / sizeof(T);
}

template<class T>
void PagedArray<T>::setCacheSizeInTiles (uInt howManyTiles)
{
  itsAccessor.setCacheSize (itsRowNumber, howManyTiles);
}

template<class T>
void PagedArray<T>::setCacheSizeFromPath (const IPosition& sliceShape,
					  const IPosition& windowStart,
					  const IPosition& windowLength,
					  const IPosition& axisPath)
{
  itsAccessor.setCacheSize (itsRowNumber, sliceShape, windowStart,
			    windowLength, axisPath, True);
}

template<class T>
void PagedArray<T>::clearCache()
{
  itsAccessor.clearCaches();
}

template<class T>
void PagedArray<T>::showCacheStatistics (ostream& os) const
{
  itsAccessor.showCacheStatistics (os);
}

template<class T>
T PagedArray<T>::getAt(const IPosition& where) const
{
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
  if (itsTable.isNull() == True) {
    LogIO logErr(LogOrigin("PagedArray<T>", "ok()"));
    logErr << LogIO::SEVERE << "No Table associated with the Paged Array"
	   << LogIO::POST;
     return False;
  }
  if (itsROArray.isNull() == True) {
    LogIO logErr(LogOrigin("PagedArray<T>", "ok()"));
    logErr  << LogIO::SEVERE << "No Array associated with the Paged Array"
	    << LogIO::POST;
     return False;
  }
  if (itsRowNumber > itsTable.nrow()) {
    LogIO logErr(LogOrigin("PagedArray<T>", "ok()"));
    logErr << LogIO::SEVERE << "Row number is too big for the current Table"
 	   << LogIO::POST;
    return False;
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

// IT IS IMPOSSIBLE TO CREATE A COLUMN WHICH CONTAINS ARRAYS OF
// DIFFERENT DIMENSIONALITY
template <class T>
void PagedArray<T>::makeArray (const TiledShape& shape)
{
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
}

template<class T>
String PagedArray<T>::defaultComment()
{
  return String("version 4.0");
}

template<class T>
void PagedArray<T>::makeRWArray()
{
  itsTable.reopenRW();
  itsRWArray.reference (ArrayColumn<T> (itsTable, itsColumnName));
}
