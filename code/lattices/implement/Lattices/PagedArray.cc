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
#include <aips/Tables/SetupNewTab.h>
#include <aips/Tables/TiledCellStMan.h>
#include <aips/Tables/TiledStManAccessor.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/ArrColDesc.h>
#include <aips/Tables/TableInfo.h>
#include <aips/Utilities/COWPtr.h>

#include <aips/Logging/LogSink.h>

#include <iostream.h>
#include <strstream.h>

// a default PagedArray is empty
template<class T> PagedArray<T>::PagedArray()
: columnname_p(COLUMNNAME), rownumber_p(0)
{
  // does nothing
};

// IPosition and file name constructor
template<class T> PagedArray<T>::PagedArray(const IPosition &arrayShape, 
					    Table &table)
: table_p(table), columnname_p(COLUMNNAME), rownumber_p(0)
{
  T overloadhelper;
  // build the column: 
  const Int maxPixels=256*256;
  buildColumn(overloadhelper, table_p, columnname_p, COLUMNCOMMENT, 
	      rownumber_p, indirectArray_p, arrayShape, maxPixels);
  setTableType();
};

// IPosition, file name and maxPixels constructor
template<class T> PagedArray<T>::PagedArray(const IPosition &arrayShape, 
					    Table &table, const Int maxPixels)
: table_p(table), columnname_p(COLUMNNAME), rownumber_p(0)
{
  T overloadhelper;
  // build the column: 
  buildColumn(overloadhelper, table_p, columnname_p, COLUMNCOMMENT, 
	      rownumber_p, indirectArray_p, arrayShape, maxPixels);
  setTableType();
};

// IPosition, file name, column name and row number constructor
template<class T> PagedArray<T>::PagedArray(const IPosition &arrayShape, 
					    Table &table,
					    const String &colName, 
					    Int rowNum)
: table_p(table), columnname_p(colName), rownumber_p(rowNum)
{
  T overloadhelper;
  // build the column: 
  const Int maxPixels=32768;
  buildColumn(overloadhelper, table_p, columnname_p, COLUMNCOMMENT, 
	      rownumber_p, indirectArray_p, arrayShape, maxPixels);
  setTableType();
};

// pre-existing Table constructor
template<class T> PagedArray<T>::PagedArray(Table &table)
: table_p(table), columnname_p(COLUMNNAME), 
  indirectArray_p(table_p, columnname_p), rownumber_p(0)
{
  setTableType();
  // does nothing
};

// pre-existing table, column name and row number constructor
template<class T> PagedArray<T>::PagedArray(Table &table,
					    const String &colName, 
					    Int rowNum)
: table_p(table), columnname_p(colName), 
  indirectArray_p(table_p, columnname_p), rownumber_p(rowNum)
{
  setTableType();
  // does nothing
};

// copy ctor (reference semantics)
template<class T> PagedArray<T>::PagedArray(const PagedArray<T> &other)
: table_p(other.table_p), columnname_p(other.columnname_p), 
  indirectArray_p(other.indirectArray_p), rownumber_p(other.rownumber_p)
{
  // does nothing
};

// destructor
template<class T> PagedArray<T>::~PagedArray()
{
  // Table may not be written if ref count > 1 - here we force a write.
  if (!table_p.isNull()) table_p.flush();
};

// assignment operator - reference semantics
template<class T> 
PagedArray<T> &PagedArray<T>::operator=(const PagedArray<T> &other)
{
  if (this != &other) {
    table_p = other.table_p;
    indirectArray_p.reference(other.indirectArray_p);
    columnname_p = other.columnname_p;
    rownumber_p = other.rownumber_p;
  }
  return *this;
};

template<class T> void PagedArray<T>::setTable(Table &table)
{
  table_p = table;
  if(table_p.tableDesc().isColumn(columnname_p)) 
    indirectArray_p.attach(table_p, columnname_p);
};

template<class T> Table &PagedArray<T>::table()
{
  if (table_p.isNull())
    throw(AipsError("PagedArray<T>::table: no table exists."));
  return table_p;
}

// a default constructed PagedArray is automatically given a temporary
// TableColumn name.  You may change it here.
//template<class T> 
//void PagedArray<T>::setColumnName(const String &newColumnName)
//{
//  table_p.renameColumn(newColumnName, columnname_p);
//  columnname_p = newColumnName;
//};

// get the current TableColumn name 
template<class T> const String &PagedArray<T>::columnName() const
{
  return columnname_p;
};

// a default constructed PagedArray is automatically given a temporary
// TableColumn row number.  You may change it here.
template<class T> void PagedArray<T>::setRowNumber(uInt rowNum)
{
  if (!indirectArray_p.isNull()) {
    uInt numOfRows = table_p.nrow();
    if(numOfRows < rowNum) {
      table_p.addRow(rowNum-numOfRows+1);
    }
    if (indirectArray_p.isDefined(rownumber_p))
      indirectArray_p.put(rowNum, indirectArray_p(rownumber_p));
  }
  rownumber_p = rowNum;
};

// get the current TableColumn row number 
template<class T> uInt PagedArray<T>::rowNumber() const
{
  return rownumber_p;
};

// get the shape of the pagedArray 
template<class T> IPosition PagedArray<T>::shape() const
{
  IPosition val;
  if (!indirectArray_p.isNull() && indirectArray_p.isDefined(rownumber_p))
    val = indirectArray_p.shape(rownumber_p);
  return val;
};

template<class T> void PagedArray<T>::resize(const IPosition &newShape)
{
  Int numrows = table_p.nrow();
  if (!indirectArray_p.isNull()){
    if (table_p.canRemoveColumn(columnname_p)){
      if (numrows > 1 ){
	// create a column to temporarily hold the data
	TableDesc description;
	uInt ndim = newShape.nelements();
	description.addColumn(ArrayColumnDesc<T>("temp",COLUMNCOMMENT,ndim));
	description.defineHypercolumn("temp", ndim, stringToVector("temp"));
	IPosition tile;
	switch (ndim) {
	case 1:
	case 2:
	  tile = newShape;
	  break;
	default:
	  tile = IPosition(2, newShape(0), newShape(1));
	  break;
	};
	table_p.addColumn(description, TiledCellStMan("temp", tile));
	ArrayColumn<T> hold(table_p, "temp");
	hold.putColumn(indirectArray_p);
	table_p.removeColumn(columnname_p);
	T overloadhelper;
	// build the column: 
	buildColumn(overloadhelper, table_p, columnname_p, COLUMNCOMMENT, 
		    rownumber_p, indirectArray_p, newShape, maxPixels());
	for(int i =0;i<numrows;i++) {
	  if (i != rownumber_p) indirectArray_p.put(i,hold(i));
	  else indirectArray_p.setShape(i, newShape);
	}
	table_p.removeColumn("temp");
      } else { 
	// only one cell to the column, quicker to delete and start over.
	table_p.removeColumn(columnname_p);
	T overloadhelper;
	// build the column: 
	buildColumn(overloadhelper, table_p, columnname_p, COLUMNCOMMENT, 
		    rownumber_p, indirectArray_p, newShape, maxPixels());
      }
    } else {
      // the column is not deletable, throw an error
      throw(AipsError("PagedArray<T>::resize - storage manager prevents "
		      "resizing."));
    }
  } else {
    // the indirect array is null, we must build the whole shebang
    T overloadhelper;
    buildColumn(overloadhelper, table_p, columnname_p, COLUMNCOMMENT, 
		rownumber_p, indirectArray_p, newShape, maxPixels());
  }
};

template<class T> 
Bool PagedArray<T>::getSlice(COWPtr<Array<T> > &buffer, 
			     const IPosition &start, 
			     const IPosition &shape, const IPosition &stride,
			     Bool removeDegenerateAxes) const
{
  Slicer theSlice(start, shape, stride, Slicer::endIsLength);
  return getSlice(buffer, theSlice, removeDegenerateAxes);
};

template<class T> 
Bool PagedArray<T>::getSlice(COWPtr<Array<T> > &buffer, 
			     const Slicer &theSlice, 
			     Bool removeDegenerateAxes) const
{
  if (indirectArray_p.isNull())
    throw(AipsError("PagedArray<T>::getSlice: no data in Table."));

  Array<T> temporarySubArray(theSlice.length());
  indirectArray_p.getSlice(rownumber_p, theSlice, temporarySubArray);

  if(removeDegenerateAxes)
     buffer.set(new Array<T>(temporarySubArray.nonDegenerate()), True, False);
  else
     buffer.set(new Array<T>(temporarySubArray), True, False);

  return False;
};

// return a slice of the inside of the PagedArray, returned Boolean of True
// if the buffer is a reference to the data, else False.
template<class T>
Bool PagedArray<T>::getSlice(Array<T> &buffer, const IPosition &start, 
			     const IPosition &shape, const IPosition &stride,
			     Bool removeDegenerateAxes)
{
  Slicer theSlice(start, shape, stride, Slicer::endIsLength);
  return getSlice(buffer, theSlice, removeDegenerateAxes);
}; 

template<class T> 
Bool PagedArray<T>::getSlice(Array<T> &buffer, const Slicer &theSlice, 
			     Bool removeDegenerateAxes)
{
  if (indirectArray_p.isNull())
    throw(AipsError("PagedArray<T>::getSlice: no data in Table."));
  if (!removeDegenerateAxes && buffer.ndim() != theSlice.length().nelements()
      && buffer.nelements() != 0)
    throw(AipsError("PagedArray<T>::getSlice: buffer and slice are different"
		    " shapes"));

  if(removeDegenerateAxes) {
    Array<T> temporarySubArray(theSlice.length());
    indirectArray_p.getSlice(rownumber_p, theSlice, temporarySubArray);
    // resize does nothing if they are the same already
    buffer.resize(theSlice.length().nonDegenerate());
    buffer = temporarySubArray.nonDegenerate();
  } else 
    buffer = indirectArray_p.getSlice(rownumber_p, theSlice);
  
  return False;
};

// function which places an Array of values within the lattice
template<class T> void PagedArray<T>::putSlice(const Array<T> &sourceArray, 
					       const IPosition &where,
					       const IPosition &stride)
{
  if (indirectArray_p.isNull()) {
    T overloadhelper;
    buildColumn(overloadhelper, table_p, columnname_p, COLUMNCOMMENT, 
		rownumber_p, indirectArray_p, sourceArray.shape(), maxPixels());
  }
  uInt arrDim = sourceArray.ndim();
  uInt latDim = ndim();
  if (arrDim > latDim) {
    throw(AipsError("PagedArray<T>::putSlice: "
		    "where.nelements()<sourceArray.ndim()"));
  } else if (arrDim < latDim) {
    Array<T> degenerateArr(sourceArray.addDegenerate(latDim-arrDim));
    Slicer theSlice(where, degenerateArr.shape(), stride, Slicer::endIsLength); 
    indirectArray_p.putSlice(rownumber_p, theSlice, degenerateArr);
  } else {
    Slicer theSlice(where, sourceArray.shape(), stride, Slicer::endIsLength); 
    indirectArray_p.putSlice(rownumber_p, theSlice, sourceArray);
  }
};


template<class T> Bool PagedArray<T>::ok() const
{
  if(!Lattice<T>::ok())
    throw(AipsError("PagedArray::ok(): underlying lattice has problem"));
  if(table_p.isNull()) 
    throw(AipsError("PagedArray::ok(): table data member has problem"));
  if(indirectArray_p.isNull())
    throw(AipsError("PagedArray::ok(): indirectArray has problem"));
  if(indirectArray_p.nrow()< rownumber_p)
    throw(AipsError("PagedArray::ok(): row count has problem"));
  if(columnname_p.length()<= 0)
    throw(AipsError("PagedArray::ok(): Column name String has problem"));
  return True;
};

// const paran operator r-value implementation 
template<class T> T PagedArray<T>::getAt(const IPosition &where) const
{
  uInt numberOfAxes = this->ndim();
  if (numberOfAxes != where.nelements())
    throw(AipsError("PagedArray<T>::operator() - IPosition argument is not"
		    " congruent with the PagedArray."));

  COWPtr<Array<T> > buffer;
  getSlice(buffer,Slicer(where,IPosition(numberOfAxes, 1),
			 IPosition(numberOfAxes, 1), 
			 Slicer::endIsLength), True);

  return (buffer->operator()(IPosition(1,0)));
};

// non const paran operator l-value implementation
template<class T> void PagedArray<T>::putAt(const T &value, 
					    const IPosition &where)
{
  // expensive operation.  better to use lattice iterators for multiple calls
  IPosition shape(ndim(),1);
  Array<T> buffer(shape);
  buffer = value;
  indirectArray_p.putSlice(rownumber_p,Slicer(where,shape),buffer);
};

//
template<class T>
RO_LatticeIterInterface<T> *PagedArray<T>::makeIter(const LatticeNavigator &
						    navigator) const
{
  return new RO_PagedArrIter<T>(*this, navigator);
};

//
template<class T>
LatticeIterInterface<T> *PagedArray<T>::makeIter(const LatticeNavigator &
						 navigator)
{
  return new PagedArrIter<T>(*this, navigator);
};

//
template<class T>
void PagedArray<T>::showCacheStatistics(ostream &os)
{
    ROTiledStManAccessor accessor(table_p, columnname_p);
    accessor.showCacheStatistics(os);
}

template<class T>
uInt PagedArray<T>::maxPixels() const
{
    ROTiledStManAccessor accessor(table_p, columnname_p);
    return accessor.tileShape(rownumber_p).product();
}

template<class T>
IPosition PagedArray<T>::niceCursorShape(uInt maxPixels) const
{
    ROTiledStManAccessor accessor(table_p, columnname_p);
    IPosition retval = accessor.tileShape(rownumber_p);
    if (retval.product() > maxPixels) {
      retval = Lattice<T>::niceCursorShape(maxPixels);
    }
    return retval;
}


template<class T> uInt PagedArray<T>::cacheSize() const
{
  ROTiledStManAccessor accessor(table_p, columnname_p);
  return accessor.maximumCacheSize()/sizeof(T);
}


template<class T> void PagedArray<T>::setCacheSize(uInt howManyPixels)
{
  uInt sizeInBytes = howManyPixels*sizeof(T);
  ROTiledStManAccessor accessor(table_p, columnname_p);

  accessor.setCacheSize(rownumber_p, sizeInBytes);
}

template<class T>
void PagedArray<T>::setCacheSizeFromPath(const IPosition& sliceShape,
					 const IPosition& windowStart,
					 const IPosition& windowLength,
					 const IPosition& axisPath)
{
  ROTiledStManAccessor accessor(table_p, columnname_p);
  accessor.setCacheSize(rownumber_p, sliceShape, windowStart,
			windowLength, axisPath, False);
}

template<class T> void PagedArray<T>::clearCache() const
{
  ROTiledStManAccessor accessor(table_p, columnname_p);
  accessor.clearCaches();
}

template <class T> 
void buildStandardColumn(Table &table, const String &columnName,
			 const String &columnComment, Int rowNumber, 
			 ArrayColumn<T> &indirectArray, 
			 const IPosition &shape,
			 Int maxPixels)
{
    LogOrigin where("buildStandardColumn(Table &table, const String &columnName,"
		    "const String &columnComment, Int rowNumber, "
		    "ArrayColumn<T> &indirectArray, "
		    "const IPosition &shape)", WHERE);
    LogMessage message(where);

  // if the column doesn't exist, build it
  if (!table.tableDesc().isColumn(columnName)){
    // create a Table Description
    TableDesc description;
    uInt ndim = shape.nelements();
    description.addColumn(ArrayColumnDesc<T>(columnName,columnComment,ndim));
    description.defineHypercolumn(columnName,ndim,stringToVector(columnName));

    // Try to find a good tile shape heuristically
    IPosition tile(shape.nelements());
    if (shape.product() <= maxPixels) {
        tile = shape;
    } else {
	IPosition stmantile = TiledStMan::makeTileShape(shape);
	// If the storage manager tile fits evenly assume it's best
	Bool stmantilefits = True;
	for (uInt j=0; j<shape.nelements(); j++) {
	    if (shape(j) % stmantile(j) != 0) {
		stmantilefits = False;
		break;
	    }
	}
	if (stmantilefits) {
	    tile = stmantile;
	} else {
	    // Alas, the storage manager tile does NOT fit evenly. Find
	    // one that does. Algorithm:
	    // 1. Start with a tile of length one on all axes
	    // 2. While (tile.volume() <= stman volume && tile can grow)
	    // 3.    adjust next axis to the next larger size that fits

	    // This algorithm should always provide at least a row-cursor
	    // that fits. The worst case is if the first axis is degenerate
	    // and the other axes are all large primes.

	    tile = 1;
	    IPosition lastTile(tile.nelements());
	    lastTile = -1;
	    const Int idealVolume = stmantile.product();
	    const Int maxVolume = shape.product();
	    Int currentAxis = 0;
	    while ((tile.product() < idealVolume) && 
		   // This condition means that the tile hasn't filled up
		   // the entire shape.
		   (tile.product() < maxVolume)) {
		// Make the current axis the next size larger that fits
		lastTile = tile;
		// Advance until we hit the end or it fits evenly
		Bool itFits = False;
		while(!itFits && (tile(currentAxis)+1 <= shape(currentAxis))) {
		    tile(currentAxis)++;
		    if ((shape(currentAxis) % tile(currentAxis)) == 0) {
			itFits = True;
		    }
		}
		// Advance the axis counter so that it wraps (0,1,2,0,1,2,...)
		currentAxis = (currentAxis + 1) % tile.nelements();
	    }
	    // OK, we have a tile that FITS. We prefer larger tiles to
	    // smaller ones unless they are TOO large. Arbitrarily assume
	    // 10*idealVolume is too large, then we use the last volume.
	    if (tile.product() > 10*idealVolume) {
		tile = lastTile;
	    }
	}
    }

    ostrstream buffer;
    buffer << "Creating Lattice cell with tile of shape " << tile <<
      " (" << tile.product() << " pixels)";
    message.message(buffer).line(__LINE__).priority(LogMessage::DEBUGGING);
    LogSink::postGlobally(message);
    
    table.addColumn(description, TiledCellStMan(columnName, tile));
  } 
  // now attach the indirect array: the Table code throws an exception if 
  // the columnName doesn't exist
  if (indirectArray.isNull())
    indirectArray.attach(table, columnName);
  // if table doesn't have enough rows to match our row number
  // then add rows and fill them with empty arrays (TiledStMan bug?)
  int rows = table.nrow();
  if (rows <= rowNumber) {
    table.addRow(rowNumber+1-rows);
  }
  indirectArray.setShape(rowNumber, shape);
};

template<class T> void PagedArray<T>::setTableType()
{
  TableInfo & info(table_p.tableInfo());
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
