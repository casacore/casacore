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
#include <trial/Lattices/LatticeIterInterface.h>
#include <trial/Lattices/LatticeNavigator.h>

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
#include <aips/Tables/TiledStManAccessor.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/ColumnDesc.h>
#include <aips/Tables/ArrColDesc.h>
#include <aips/Tables/TableInfo.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/COWPtr.h>

#include <iostream.h>

template<class T> PagedArray<T>::
PagedArray() 
{
  // Initialises all private data using their default consructor
};

template<class T> PagedArray<T>::
PagedArray(const IPosition & shape, const String & filename) 
  :theColumnName(defaultColumn()),
   theRowNumber(defaultRow()) 
{
  makeTable(filename, Table::New);
  makeArray(shape, defaultTileShape(shape));
  setTableType();
  AlwaysAssert(ok() == True, AipsError);
};

template<class T> PagedArray<T>::
PagedArray(const IPosition & shape)
  :theColumnName(defaultColumn()),
   theRowNumber(defaultRow())
{
  Path filename=File::newUniqueName(String("./"), String("pagedArray"));
  makeTable(filename.absoluteName(), Table::Scratch);
  makeArray(shape, defaultTileShape(shape));
  setTableType();
  AlwaysAssert(ok() == True, AipsError);
};

template<class T> PagedArray<T>::
PagedArray(const IPosition & shape, Table & file)
  :theTable(file),
   theColumnName(defaultColumn()),
   theRowNumber(defaultRow()) 
{
  makeArray(shape, defaultTileShape(shape));
  setTableType();
  AlwaysAssert(ok() == True, AipsError);
};

template<class T> PagedArray<T>::
PagedArray(const IPosition & shape, Table & file, const IPosition tileShape)
  :theTable(file),
   theColumnName(defaultColumn()),
   theRowNumber(defaultRow())
{
  checkTileShape(shape, tileShape);
  makeArray(shape, tileShape);
  setTableType();
  AlwaysAssert(ok() == True, AipsError);
};

template<class T> PagedArray<T>::
PagedArray(const IPosition & shape, Table & file,
	   const String & columnName, uInt rowNumber)
  :theTable(file),
   theColumnName(columnName),
   theRowNumber(rowNumber)
{
  makeArray(shape, defaultTileShape(shape));
  setTableType();
  AlwaysAssert(ok() == True, AipsError);
};

template<class T> PagedArray<T>::
PagedArray(const IPosition & shape, Table & file,
	   const String & columnName, uInt rowNumber, const IPosition tileShape)
  :theTable(file),
   theColumnName(columnName),
   theRowNumber(rowNumber)
{
  checkTileShape(shape, tileShape);
  makeArray(shape, tileShape);
  setTableType();
  AlwaysAssert(ok() == True, AipsError);
};

template<class T> PagedArray<T>::
PagedArray(const String & filename)
  :theTable(filename, Table::Update),
   theColumnName(defaultColumn()), 
   theRowNumber(defaultRow()),
   theArray(theTable, theColumnName)
{
  AlwaysAssert(ok() == True, AipsError);
};

template<class T> PagedArray<T>::
PagedArray(Table & file)
  :theTable(file),
   theColumnName(defaultColumn()), 
   theRowNumber(defaultRow()),
   theArray(theTable, theColumnName)
{
  AlwaysAssert(ok() == True, AipsError);
};

template<class T> PagedArray<T>::
PagedArray(Table & file, const String & columnName, uInt rowNumber)
  :theTable(file),
   theColumnName(columnName), 
   theRowNumber(rowNumber),
   theArray(theTable, theColumnName)
{
  AlwaysAssert(ok() == True, AipsError);
};

template<class T> PagedArray<T>::PagedArray(const PagedArray<T> &other)
  :theTable(other.theTable),
   theColumnName(other.theColumnName), 
   theRowNumber(other.theRowNumber),
   theArray(other.theArray)
{
  DebugAssert(ok() == True, AipsError);
};

template<class T> PagedArray<T>::
~PagedArray() {
  // Table may not be written if ref count > 1 - here we force a write.
  DebugAssert(ok() == True, AipsError);
  theTable.flush();
};

template<class T> PagedArray<T> & PagedArray<T>::
operator=(const PagedArray<T> & other) {
  if (this != &other) {
    theTable = other.theTable;
    theColumnName = other.theColumnName;
    theRowNumber = other.theRowNumber;
    theArray.reference(other.theArray);
  }
  DebugAssert(ok() == True, AipsError);
  return *this;
};

template<class T> IPosition PagedArray<T>::
shape() const {
  DebugAssert(ok() == True, AipsError);
  return theArray.shape(theRowNumber);
};

template<class T> void PagedArray<T>::
resize(const IPosition & newShape) {
  resize(newShape, defaultTileShape(newShape));
};

// GER, I FOUND THE Table::re{move,name}Column FUNCTION'S ARE NOT
// IMPLEMENTED YET.  IN THIS CODE I HAVE WORKED AROUND THIS. BUT IT IS
// PROBABLY QUITE INEFFECIENT
template<class T> void PagedArray<T>::
resize(const IPosition & newShape, const IPosition & tileShape) {
  theLog << LogOrigin("PagedArray<T>", 
		      "resize(const IPosition & shape,"
		      " const IPosition & tileShape)");
  checkTileShape(newShape, tileShape);
  const uInt nRows = theTable.nrow();
  if (nRows == 1){
    // only one cell to the column, quicker to delete and start over.
    if (theTable.canRemoveColumn(theColumnName))
      theTable.removeColumn(theColumnName);
    else
      theColumnName += "_";
    // build a new column: 
    makeArray(newShape, tileShape);
  }
  else {
    const String tempName(theColumnName + "_");
    const uInt ndim = theArray.ndim(theRowNumber);
    // This assertion is required as every array in a column must have the
    // same number of dimensions. This may be relaxed eventually.
    AlwaysAssert(ndim == newShape.nelements(), AipsError);
    // recreate the column to hold the data
    TableDesc description;
    description.addColumn(ArrayColumnDesc<T>(tempName, defaultComment(), 
					     ndim));
    description.defineHypercolumn(tempName, ndim, stringToVector(tempName));
    theTable.addColumn(description, TiledCellStMan(tempName, tileShape));
    // Copy the data from the old column to the new
    theArray.attach(theTable, tempName);
    ROArrayColumn<T> oldColumn(theTable, theColumnName);
    for (uInt r = 0; r < nRows; r++)
      if (r == theRowNumber)
    	theArray.setShape(r, newShape);
      else 
    	theArray.put(r, oldColumn, r);

    if (theTable.canRemoveColumn(theColumnName))
      theTable.removeColumn(theColumnName);
    if (theTable.canRenameColumn())
      theTable.renameColumn(theColumnName, tempName);
    else
      theColumnName = tempName;
    theArray.attach(theTable, theColumnName);
  }
  theLog 
    << LogIO::DEBUGGING
    << "Resizing the PagedArray to shape " << newShape
    << " with a tile shape of " << tileShape << endl
    << " in row " << theRowNumber << " and column '" << theColumnName
    << "' of the Table '" << tableName() << "'" << LogIO::POST;
};

template <class T> Bool PagedArray<T>::
getSlice(COWPtr<Array<T> > & buffer, const IPosition & start,
	 const IPosition & shape, const IPosition & stride, 
	 Bool removeDegenerateAxes) const {
  return Lattice<T>::getSlice(buffer, start, shape, stride, removeDegenerateAxes);
};
 
template<class T> Bool PagedArray<T>::
getSlice(COWPtr<Array<T> > & bufPtr, const Slicer & section, 
	 Bool removeDegenerateAxes) const {
  if (bufPtr.isNull())
    if (removeDegenerateAxes == False)
      bufPtr.set(new Array<T>(section.length()));
    else
      bufPtr.set(new Array<T>(section.length().nonDegenerate()));
  // I can remove the constness because the buffer is never returned by
  // reference.
  PagedArray<T> * This = (PagedArray<T> *) this;
  return This->getSlice(bufPtr.rwRef(), section, removeDegenerateAxes);
};

template <class T> Bool PagedArray<T>::
getSlice(Array<T> & buffer, const IPosition & start, const IPosition & shape, 
	 const IPosition & stride, Bool removeDegenerateAxes) {
  return Lattice<T>::getSlice(buffer, start, shape, stride, removeDegenerateAxes);
};

template<class T> Bool PagedArray<T>::
getSlice(Array<T> & buffer, const Slicer & section, 
	 Bool removeDegenerateAxes) {
  if (buffer.nelements() == 0) {
    theArray.getSlice(theRowNumber, section, buffer, True);
    if (removeDegenerateAxes == True) {
      const IPosition shape = buffer.shape();
      if (!shape.nonDegenerate().isEqual(shape)){
	Array<T> noDegen(buffer.nonDegenerate());
	buffer.reference(noDegen);
      }
    }
  }
  else {
    const IPosition bshape = buffer.shape();
    const IPosition slength = section.length();
    if (bshape.nelements() == slength.nelements())
      removeDegenerateAxes = False;
    if (removeDegenerateAxes == False) {
      AlwaysAssert(bshape.isEqual(slength), AipsError);
      theArray.getSlice(theRowNumber, section, buffer);
    }
    else {
      AlwaysAssert(bshape.nonDegenerate().isEqual(
                   slength.nonDegenerate()), AipsError);
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
      T * bufferPtr = buffer.getStorage(isAcopy);
      Array<T> tempBuffer(slength, bufferPtr, SHARE);
      theArray.getSlice(theRowNumber, section, tempBuffer);
      buffer.putStorage(bufferPtr, isAcopy);
    }
  }
  return False;
};

template<class T> void PagedArray<T>::putSlice(const Array<T> & sourceArray, 
					       const IPosition & where,
					       const IPosition & stride) {
  const uInt arrDim = sourceArray.ndim();
  const uInt latDim = ndim();
  AlwaysAssert(arrDim <= latDim, AipsError);
  
  if (arrDim == latDim) {
    Slicer section(where, sourceArray.shape(), stride, Slicer::endIsLength); 
    theArray.putSlice(theRowNumber, section, sourceArray);
  }
  else {
    Array<T> degenerateArr(sourceArray.addDegenerate(latDim-arrDim));
    Slicer section(where, degenerateArr.shape(), stride, Slicer::endIsLength); 
    theArray.putSlice(theRowNumber, section, degenerateArr);
  } 
};

template<class T> void PagedArray<T>::
putSlice(const Array <T> & sourceBuffer, const IPosition & where){
  Lattice<T>::putSlice(sourceBuffer, where);
};

template<class T> const String & PagedArray<T>::
tableName() const {
  return theTable.tableName();
};

template<class T> const String & PagedArray<T>::
columnName() const {
  return theColumnName;
};

template<class T> String PagedArray<T>::
defaultColumn() {
  return String("PagedArray");
};


template<class T> uInt PagedArray<T>::
rowNumber() const {
  return theRowNumber;
};

template<class T> uInt PagedArray<T>::
defaultRow() {
  return 0;
};

template<class T> IPosition PagedArray<T>::
tileShape() const {
  ROTiledStManAccessor accessor(theTable, theColumnName);
  return accessor.tileShape(theRowNumber);
};

template<class T> uInt PagedArray<T>::
maxPixels() const {
  return tileShape().product();
};

template<class T> IPosition PagedArray<T>::
niceCursorShape(uInt maxPixels) const {
  IPosition retval = tileShape();
  if (retval.product() > maxPixels)
    retval = Lattice<T>::niceCursorShape(maxPixels);
  return retval;
};

template<class T> void PagedArray<T>::
setMaximumCacheSize(uInt howManyPixels) {
  const uInt sizeInBytes = howManyPixels*sizeof(T);
  ROTiledStManAccessor accessor(theTable, theColumnName);
  accessor.setMaximumCacheSize(sizeInBytes);
};

template<class T> uInt PagedArray<T>::
maximumCacheSize() const {
  ROTiledStManAccessor accessor(theTable, theColumnName);
  return accessor.maximumCacheSize()/sizeof(T);
};

// GER, IT WOULD BE NICE TO HAVE A FUNCTION WHICH RETURNS THE CURRENT CACHE
// SIZE FOR THE ROW/COLUMN. ie. uInt cacheSize() const
// THIS WOULD INVOLVE ANOTHER FUNCTION IN THE ROTiledStManAccessor
template<class T> void PagedArray<T>::
setCacheSize(uInt howManyPixels) {
  const uInt sizeInBytes = howManyPixels*sizeof(T);
  ROTiledStManAccessor accessor(theTable, theColumnName);
  accessor.setCacheSize(theRowNumber, sizeInBytes);
};

template<class T> void PagedArray<T>::
setCacheSizeFromPath(const IPosition& sliceShape,
		     const IPosition& windowStart,
		     const IPosition& windowLength,
		     const IPosition& axisPath) {
  ROTiledStManAccessor accessor(theTable, theColumnName);
  accessor.setCacheSize(theRowNumber, sliceShape, windowStart,
			windowLength, axisPath, False);
};

template<class T> void PagedArray<T>::
clearCache() const {
  ROTiledStManAccessor accessor(theTable, theColumnName);
  accessor.clearCaches();
};

template<class T> void PagedArray<T>::
showCacheStatistics(ostream &os) {
  ROTiledStManAccessor accessor(theTable, theColumnName);
  accessor.showCacheStatistics(os);
};

template<class T> T PagedArray<T>::
getAt(const IPosition & where) const {
  const uInt dim = ndim();
  AlwaysAssert(dim == where.nelements(), AipsError);
  const IPosition one(dim, 1);
  COWPtr<Array<T> > bufPtr(new Array<T>(one));
  getSlice(bufPtr, Slicer(where, one, one, Slicer::endIsLength));
  return bufPtr->operator()(IPosition(dim,0));
};

template<class T> void PagedArray<T>::
putAt(const T &value, const IPosition & where) {
  const IPosition shape(ndim(),1);
  Array<T> buffer(shape);
  buffer = value;
  theArray.putSlice(theRowNumber, Slicer(where,shape), buffer);
};

template<class T> Bool PagedArray<T>::
ok() const {
  if (theTable.isNull() == True) {
    LogIO logErr(LogOrigin("PagedArray<T>", "ok()"));
    logErr << LogIO::SEVERE << "No Table associated with the Paged Array"
	   << LogIO::POST;
     return False;
  }
  if (theArray.isNull() == True) {
    LogIO logErr(LogOrigin("PagedArray<T>", "ok()"));
    logErr  << LogIO::SEVERE << "No Array associated with the Paged Array"
	    << LogIO::POST;
     return False;
  }
  if (theRowNumber > theArray.nrow()) {
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
};

template<class T> RO_LatticeIterInterface<T> * PagedArray<T>::
makeIter(const LatticeNavigator & navigator) const {
  return new RO_PagedArrIter<T>(*this, navigator);
};

template<class T> LatticeIterInterface<T> * PagedArray<T>::
makeIter(const LatticeNavigator & navigator) {
  return new PagedArrIter<T>(*this, navigator);
};

// IT IS IMPOSSIBLE TO CREATE A COLUMN WHICH CONTAINS ARRAYS OF
// DIFFERENT DIMENSIONS
template <class T> void PagedArray<T>::
makeArray(const IPosition & shape, const IPosition & tileShape) {
  theLog << LogOrigin("PagedArray<T>", 
		      "makeArray(const IPosition & shape,"
		      " const IPosition & tileShape)");
  // Create a new column if it does not already exist.
  const uInt ndim = shape.nelements();
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
  theArray.attach(theTable, theColumnName);

  // if table doesn't have enough rows to match our row number
  // then add rows and fill them with empty arrays
  const IPosition emptyShape(ndim, 1);
  const Int rows = theTable.nrow() - 1;
  if (rows < theRowNumber) {
    theTable.addRow(theRowNumber-rows);
    for (Int r = rows+1; r < theRowNumber; r++)
      theArray.setShape(r, emptyShape);
  }
  if (newColumn) 
    for (Int r = 0; r <= rows; r++)
      if (r != theRowNumber)
	theArray.setShape(r, emptyShape);
  // set a shape of the PagedArray
  theArray.setShape(theRowNumber, shape);
  theLog << LogIO::DEBUGGING
         << "Created PagedArray of shape " << shape
         << " with a tile shape of " << tileShape << endl
         << " in row " << theRowNumber << " and column '" << theColumnName
         << "' of the Table '" << tableName() << "'" << LogIO::POST;
};

template<class T> void PagedArray<T>::
setTableType() {
  TableInfo & info(theTable.tableInfo());
  {
    const String reqdType = info.type(TableInfo::PAGEDARRAY);
    if (info.type() != reqdType)
      info.setType(reqdType);
  }
  {
    const String reqdSubType = info.subType(TableInfo::PAGEDARRAY);
    if (info.subType() != reqdSubType)
      info.setSubType(reqdSubType);
  }
};

template<class T> void PagedArray<T>::
makeTable(const String & filename, Table::TableOption option) {
  SetupNewTable setupTable(filename, TableDesc(), option);
  theTable = Table(setupTable);
};

template<class T> void PagedArray<T>::
checkTileShape(const IPosition & shape, const IPosition & tileShape) {
  const uInt ndim = shape.nelements();
  AlwaysAssert(tileShape.nelements() == ndim, AipsError);
  for (uInt i = 0; i < ndim; i++)
    AlwaysAssert(shape(i) >= tileShape(i), AipsError);
};

template<class T> String PagedArray<T>::
defaultComment() {
  return String("version 3.0");
};

template<class T> IPosition PagedArray<T>::
defaultTileShape(const IPosition & latticeShape) {
  // Get the shape recommended by the Tiles Storage manager
  const IPosition stmantile = TiledStMan::makeTileShape(latticeShape);

  // If the lattice is small  enough then use the whole lattice
  if (latticeShape.product() <= stmantile.product()){
    IPosition temp(latticeShape);
    return latticeShape;
  }
  
  // If the storage manager tile fits evenly assume it's best
  const uInt ndim = latticeShape.nelements();
  Bool stmantilefits = True;
  for (uInt j=0; j < ndim; j++) {
    if (latticeShape(j) % stmantile(j) != 0) {
      stmantilefits = False;
      break;
    }
  }
  if (stmantilefits)
    return stmantile;
  // Alas, the storage manager tile does NOT fit evenly. Find
  // one that does. Algorithm:
  // 1. Start with a tile of length one on all axes
  // 2. While (tile.volume() <= stman volume && tile can grow)
  // 3.    adjust next axis to the next larger size that fits
  
  // This algorithm should always provide at least a row-cursor
  // that fits. The worst case is if the first axis is degenerate
  // and the other axes are all large primes.
  IPosition tile(ndim, 1);
  IPosition lastTile(ndim, 1);
  const Int idealVolume = min(stmantile.product(), latticeShape.product());
  Int currentAxis = 0;
  // This condition means that the tile hasn't filled up
  // the entire shape.
  while (tile.product() < idealVolume){
    // Cache the current tile shape in case the next guess is too big
    lastTile = tile;
    // Make the current axis the next size larger that fits
    // Advance until we hit the end or it fits evenly
    Bool itFits = False;
    while(!itFits && (tile(currentAxis) < latticeShape(currentAxis))) {
      tile(currentAxis)++;
      if ((latticeShape(currentAxis) % tile(currentAxis)) == 0)
	itFits = True;
    }
    // Advance the axis counter so that it wraps (0,1,2,0,1,2,...)
    currentAxis = (currentAxis + 1) % ndim;
  }
  // OK, we have a tile that FITS. We prefer larger tiles to
  // smaller ones unless they are TOO large. Arbitrarily assume
  // 10*idealVolume is too large, then we use the last volume.
  if (tile.product() > 10*idealVolume)
    return lastTile;
  else
    return tile;
};
