//# PagedArray.cc: this defines the PagedArray class
//# Copyright (C) 1994,1995,1996,1997
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
#include <aips/Utilities/COWPtr.h>

#include <iostream.h>


template<class T>
PagedArray<T>::PagedArray() 
{
  // Initializes all private data using their default consructor
}

template<class T>
PagedArray<T>::PagedArray (const TiledShape& shape, const String& filename) 
: theColumnName (defaultColumn()),
  theRowNumber  (defaultRow()) 
{
  makeTable(filename, Table::New);
  makeArray (shape);
  setTableType();
  AlwaysAssert(ok() == True, AipsError);
}

template<class T>
PagedArray<T>::PagedArray (const TiledShape& shape)
: theColumnName (defaultColumn()),
  theRowNumber  (defaultRow())
{
  Path filename=File::newUniqueName(String("./"), String("pagedArray"));
  makeTable (filename.absoluteName(), Table::Scratch);
  makeArray (shape);
  setTableType();
  AlwaysAssert(ok() == True, AipsError);
}

template<class T>
PagedArray<T>::PagedArray (const TiledShape& shape, Table& file)
: theTable      (file),
  theColumnName (defaultColumn()),
  theRowNumber  (defaultRow()) 
{
  makeArray (shape);
  setTableType();
  AlwaysAssert(ok() == True, AipsError);
}

template<class T>
PagedArray<T>::PagedArray (const TiledShape& shape, Table& file,
			   const String& columnName, uInt rowNumber)
: theTable      (file),
  theColumnName (columnName),
  theRowNumber  (rowNumber)
{
  makeArray (shape);
  setTableType();
  AlwaysAssert(ok() == True, AipsError);
}

template<class T>
PagedArray<T>::PagedArray (const String& filename)
: theTable      (filename),
  theColumnName (defaultColumn()), 
  theRowNumber  (defaultRow()),
  theROArray    (theTable, theColumnName),
  theAccessor   (theTable, theColumnName)
{
  AlwaysAssert(ok() == True, AipsError);
}

template<class T> PagedArray<T>::PagedArray (Table& file)
: theTable      (file),
  theColumnName (defaultColumn()), 
  theRowNumber  (defaultRow()),
  theROArray    (theTable, theColumnName),
  theAccessor   (theTable, theColumnName)
{
  AlwaysAssert(ok() == True, AipsError);
}

template<class T>
PagedArray<T>::PagedArray (Table& file, const String& columnName,
			   uInt rowNumber)
: theTable      (file),
  theColumnName (columnName), 
  theRowNumber  (rowNumber),
  theROArray    (theTable, theColumnName),
  theAccessor   (theTable, theColumnName)
{
  AlwaysAssert(ok() == True, AipsError);
}

template<class T>
PagedArray<T>::PagedArray (const PagedArray<T> &other)
: theTable      (other.theTable),
  theColumnName (other.theColumnName), 
  theRowNumber  (other.theRowNumber),
  theROArray    (other.theROArray),
  theRWArray    (other.theRWArray),
  theAccessor   (other.theAccessor)
{
  DebugAssert(ok() == True, AipsError);
}

template<class T>
PagedArray<T>::~PagedArray()
{
  // Table may not be written if ref count > 1 - here we force a write.
  DebugAssert(ok() == True, AipsError);
  theTable.flush();
}

template<class T>
PagedArray<T>& PagedArray<T>::operator= (const PagedArray<T>& other)
{
  if (this != &other) {
    theTable      = other.theTable;
    theColumnName = other.theColumnName;
    theRowNumber  = other.theRowNumber;
    theROArray.reference(other.theROArray);
    theRWArray.reference(other.theRWArray);
    theAccessor   = other.theAccessor;
  }
  DebugAssert(ok() == True, AipsError);
  return *this;
}

template<class T>
IPosition PagedArray<T>::shape() const
{
  DebugAssert(ok() == True, AipsError);
  return theROArray.shape (theRowNumber);
}

template<class T>
void PagedArray<T>::resize (const TiledShape& newShape)
{
  theLog << LogOrigin("PagedArray<T>", 
		      "resize(const TiledShape& shape)");
  IPosition tileShape = newShape.tileShape();
  getRWArray().setShape (theRowNumber, newShape.shape(), tileShape);
  theLog << LogIO::DEBUGGING
	 << "Resizing the PagedArray to shape " << newShape.shape()
	 << " with a tile shape of " << tileShape << endl
	 << " in row " << theRowNumber << " and column '" << theColumnName
	 << "' of the Table '" << tableName() << "'" << LogIO::POST;
}

template <class T>
Bool PagedArray<T>::getSlice (COWPtr<Array<T> >& buffer,
			      const IPosition& start,
			      const IPosition& shape,
			      const IPosition& stride, 
			      Bool removeDegenerateAxes) const
{
  return getSlice (buffer, Slicer(start, shape, stride), removeDegenerateAxes);
}

template<class T>
Bool PagedArray<T>::getSlice (COWPtr<Array<T> >& bufPtr,
			      const Slicer& section, 
			      Bool removeDegenerateAxes) const
{
  if (bufPtr.isNull()) {
    if (removeDegenerateAxes == False) {
      bufPtr.set(new Array<T>(section.length()));
    } else {
      bufPtr.set(new Array<T>(section.length().nonDegenerate()));
    }
  }
  // I can remove the constness because the buffer is never returned by
  // reference.
  PagedArray<T>* This = (PagedArray<T>*) this;
  return This->getSlice (bufPtr.rwRef(), section, removeDegenerateAxes);
}

template <class T>
Bool PagedArray<T>::getSlice (Array<T>& buffer,
			      const IPosition& start,
			      const IPosition& shape, 
			      const IPosition& stride,
			      Bool removeDegenerateAxes)
{
  return getSlice (buffer, Slicer(start, shape, stride), removeDegenerateAxes);
}

template<class T>
Bool PagedArray<T>::getSlice (Array<T>& buffer, const Slicer& section, 
			      Bool removeDegenerateAxes)
{
  if (buffer.nelements() == 0) {
    theROArray.getSlice (theRowNumber, section, buffer, True);
    if (removeDegenerateAxes == True) {
      const IPosition shape = buffer.shape();
      if (!shape.nonDegenerate().isEqual(shape)) {
	Array<T> noDegen(buffer.nonDegenerate());
	buffer.reference(noDegen);
      }
    }
  } else {
    const IPosition bshape = buffer.shape();
    const IPosition slength = section.length();
    if (bshape.nelements() == slength.nelements()) {
      removeDegenerateAxes = False;
    }
    if (removeDegenerateAxes == False) {
      AlwaysAssert(bshape.isEqual(slength), AipsError);
      theROArray.getSlice (theRowNumber, section, buffer);
    } else {
      AlwaysAssert(bshape.nonDegenerate().isEqual
                               (slength.nonDegenerate()), AipsError);
      // There is a nasty problem here.
      // RO_ArrayColumn::getSlice expects an Array that includes all the
      // axes, including the degenerate ones. How can I add the degenerate
      // axes to the supplied buffer? 
      // 1. Use the Array.reform() function. This does not work if the
      // supplied buffer is a sub-array of some larger Array.
      // 2. Use the Array.addDegenerate() function. This only works if the
      // degenerate axes are at the end.
      // 3. Create a new temporary buffer with all the degenerate axes and
      // then copy the data between the user supplied and temporary buffer.
      // This is wasteful as it involves extra memory for the temporary
      // buffer and an Array copy.
      // The Array.contigiousStorage function could be used to determine
      // when a copy needs to be made, but it is protected and hence cannot
      // be used. I'll circumvent this by calling getStorage, and letting it
      // decide whether to make a copy.
      Bool isAcopy;
      T* bufferPtr = buffer.getStorage (isAcopy);
      Array<T> tempBuffer(slength, bufferPtr, SHARE);
      theROArray.getSlice (theRowNumber, section, tempBuffer);
      buffer.putStorage (bufferPtr, isAcopy);
    }
  }
  return False;
}

template<class T>
void PagedArray<T>::putSlice (const Array<T>& sourceArray, 
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
    theRWArray.putSlice (theRowNumber, section, sourceArray);
  } else {
    Array<T> degenerateArr(sourceArray.addDegenerate(latDim-arrDim));
    Slicer section(where, degenerateArr.shape(), stride, Slicer::endIsLength); 
    theRWArray.putSlice (theRowNumber, section, degenerateArr);
  } 
}

template<class T>
void PagedArray<T>::putSlice (const Array <T>& sourceBuffer,
			      const IPosition& where)
{
  Lattice<T>::putSlice (sourceBuffer, where);
}

template<class T>
const String& PagedArray<T>::tableName() const
{
  return theTable.tableName();
}

template<class T>
const String& PagedArray<T>::columnName() const
{
  return theColumnName;
}

template<class T>
String PagedArray<T>::defaultColumn()
{
  return "PagedArray";
}

template<class T>
const ROTiledStManAccessor& PagedArray<T>::accessor() const
{
  return theAccessor;
}

template<class T>
uInt PagedArray<T>::rowNumber() const
{
  return theRowNumber;
}

template<class T>
uInt PagedArray<T>::defaultRow()
{
  return 0;
}

template<class T>
IPosition PagedArray<T>::tileShape() const
{
  return theAccessor.tileShape (theRowNumber);
}

template<class T>
uInt PagedArray<T>::maxPixels() const
{
  return tileShape().product();
}

template<class T>
IPosition PagedArray<T>::niceCursorShape (uInt maxPixels) const
{
  IPosition retval = tileShape();
  if (retval.product() > maxPixels) {
    retval = Lattice<T>::niceCursorShape(maxPixels);
  }
  return retval;
}

template<class T>
void PagedArray<T>::setMaximumCacheSize (uInt howManyPixels)
{
  const uInt sizeInBytes = howManyPixels * sizeof(T);
  theAccessor.setMaximumCacheSize (sizeInBytes);
}

template<class T>
uInt PagedArray<T>::maximumCacheSize() const
{
  return theAccessor.maximumCacheSize() / sizeof(T);
}

template<class T>
void PagedArray<T>::setCacheSizeInTiles (uInt howManyTiles)
{
  theAccessor.setCacheSize (theRowNumber, howManyTiles);
}

template<class T>
void PagedArray<T>::setCacheSizeFromPath (const IPosition& sliceShape,
					  const IPosition& windowStart,
					  const IPosition& windowLength,
					  const IPosition& axisPath)
{
  theAccessor.setCacheSize (theRowNumber, sliceShape, windowStart,
			    windowLength, axisPath, True);
}

template<class T>
void PagedArray<T>::clearCache()
{
  theAccessor.clearCaches();
}

template<class T>
void PagedArray<T>::showCacheStatistics (ostream &os) const
{
  theAccessor.showCacheStatistics (os);
}

template<class T>
T PagedArray<T>::getAt(const IPosition& where) const
{
  const uInt dim = ndim();
  AlwaysAssert(dim == where.nelements(), AipsError);
  const IPosition one(dim, 1);
  COWPtr<Array<T> > bufPtr(new Array<T>(one));
  getSlice(bufPtr, Slicer(where, one, one, Slicer::endIsLength));
  return bufPtr->operator()(IPosition(dim,0));
}

template<class T>
void PagedArray<T>::putAt (const T &value, const IPosition& where)
{
  const IPosition shape(ndim(),1);
  Array<T> buffer(shape);
  buffer = value;
  getRWArray().putSlice(theRowNumber, Slicer(where,shape), buffer);
}

template<class T>
Bool PagedArray<T>::ok() const
{
  if (theTable.isNull() == True) {
    LogIO logErr(LogOrigin("PagedArray<T>", "ok()"));
    logErr << LogIO::SEVERE << "No Table associated with the Paged Array"
	   << LogIO::POST;
     return False;
  }
  if (theROArray.isNull() == True) {
    LogIO logErr(LogOrigin("PagedArray<T>", "ok()"));
    logErr  << LogIO::SEVERE << "No Array associated with the Paged Array"
	    << LogIO::POST;
     return False;
  }
  if (theRowNumber > theTable.nrow()) {
    LogIO logErr(LogOrigin("PagedArray<T>", "ok()"));
    logErr << LogIO::SEVERE << "Row number is too big for the current Table"
 	   << LogIO::POST;
    return False;
  }
  if (theColumnName.length() == 0) {
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
  theTable.reopenRW();
  theLog << LogOrigin("PagedArray<T>", 
		      "makeArray(const TiledShape& shape)");
  // Get the lattice shape and tile shape.
  IPosition latShape  = shape.shape();
  IPosition tileShape = shape.tileShape();
  // Create a new column if it does not already exist.
  const uInt ndim = latShape.nelements();
  Bool newColumn = False;
  if (!theTable.tableDesc().isColumn(theColumnName)) {
    newColumn = True;
    // To build the column a table description must be created
    TableDesc description;
    description.addColumn(ArrayColumnDesc<T>(theColumnName,
					     defaultComment(), 
					     ndim));
    description.defineHypercolumn(theColumnName, ndim, 
				  stringToVector(theColumnName));
    theTable.addColumn(description, TiledCellStMan(theColumnName, tileShape));
  }

  // Attach the default constructed ArrayColumn to the Table
  theROArray.attach (theTable, theColumnName);
  theRWArray.attach (theTable, theColumnName);

  // if table doesn't have enough rows to match our row number
  // then add rows and fill them with empty arrays
  const IPosition emptyShape(ndim, 1);
  const Int rows = theTable.nrow() - 1;
  if (rows < theRowNumber) {
    theTable.addRow(theRowNumber-rows);
    for (Int r = rows+1; r < theRowNumber; r++) {
      theRWArray.setShape(r, emptyShape);
    }
  }
  if (newColumn) {
    for (Int r = 0; r <= rows; r++) {
      if (r != theRowNumber) {
	theRWArray.setShape(r, emptyShape);
      }
    }
  }
  // set a shape of the PagedArray
  theRWArray.setShape(theRowNumber, latShape);
  // create the accessor object
  theAccessor = ROTiledStManAccessor (theTable, theColumnName);
  theLog << LogIO::DEBUGGING
         << "Created PagedArray of shape " << latShape
         << " with a tile shape of " << tileShape << endl
         << " in row " << theRowNumber << " and column '" << theColumnName
         << "' of the Table '" << tableName() << "'" << LogIO::POST;
}

template<class T>
void PagedArray<T>::setTableType()
{
  TableInfo& info(theTable.tableInfo());
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
  theTable = Table(setupTable);
}

template<class T>
String PagedArray<T>::defaultComment()
{
  return String("version 4.0");
}

template<class T>
void PagedArray<T>::makeRWArray()
{
  theTable.reopenRW();
  theRWArray.reference (ArrayColumn<T> (theTable, theColumnName));
}
