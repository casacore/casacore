//# PagedArray.h: templated Lattice, paged from disk to memory on demand
//# Copyright (C) 1994,1995,1996,1997
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

#if !defined(AIPS_PAGEDARRAY_H)
#define AIPS_PAGEDARRAY_H

#if defined(_AIX)
#pragma implementation ("PagedArray.cc")
#endif 

#include <aips/aips.h>

#include <trial/Lattices/Lattice.h>

#include <aips/Tables/ArrayColumn.h>
#include <aips/Tables/Table.h>
#include <aips/Utilities/String.h>

//# predeclarations
class IPosition;
class LatticeNavigator;
class Slicer;
template <class T> class Array;
template <class T> class COWPtr;
template <class T> class RO_LatticeIterInterface;
template <class T> class LatticeIterInterface;
template <class T> class RO_PagedArrIter;
template <class T> class PagedArrIter;
class ostream;

extern String COLUMNNAME;
extern String COLUMNCOMMENT;

// <summary> 
// Templated Lattice, paged from disk to memory on demand 
// </summary>
//
// <use visibility=export>
// 
// <reviewed reviewer="" date="" tests="tPagedArray.cc" demos="dPagedArray.cc">
// </reviewed>
//
// <prerequisite>
//   <li> Lattice
//   <li> Array 
//   <li> LatticeIterator
//   <li> LatticeStepper
// </prerequisite>
//
// <etymology>
// "Demand paging" is a technique used to implement virtual memory
// in computer operating systems.  In this scheme, code or data are read 
// from disk to memory only as needed by a process, and are read in fixed-sized
// chunks called "pages".  PagedArrays are somewhat the same -- though without
// the automatic features found in virtual memeory demand paging.
// <note> If PagedArrays were a full imitation of demand paging, then a 
// reference to any element of a PagedArray would cause that  element and
// its neighbors to be read in automatically.  PagedArray's don't (yet) do 
// this.  However, everytime the cursor is moved in a LatticeIterator for
// a PagedArray, the cursor's contents are written.  This is true for constant
// PagedArray's, as well. </note>
// </etymology>
//
// <synopsis> 
// At the time of this writing, typical scientific computers
// provide sufficient memory for storing and manipulating 2-dimensional
// astronomical images, which have average size of around 8 MB.  Since radio 
// astronomy often uses 3-dimensional images, which can be one or two orders
// of magnitude larger, we need a convenient means to get selected portions
// of the full image into memory.  PagedArrays do this.
// 
// When a PagedArray gives you your specified slice, what it actually
// returns is a traditional aips++ array, which you can operate on
// in all of the standard ways.  
//
// PagedArrays live in secondary storage -- typically large capacity
// magnetic disks -- in aips++ Tables.  To construct a PagedArray, 
// therefore, is to implicitly constuct a Table.   Whole-array
// operations on PagedArrays are slow, due to disk i/o.
// </synopsis> 
//
// <example>
// Example 1: create a 12x12x3 PagedArray of Floats:
// <srcblock>
// IPosition arrayShape(3,12,12,3);
// // construct the table which will hold our PagedArray
// SetupNewTable newSetup("PagedArrTable", TableDesc("", TableDesc::Scratch), 
//			   Table::New);
// Table myTable(newSetup);
// // construct a PagedArray by supplying the array and a Table
// PagedArray<Float> pagedArray(arrayShape, myTable);
// // assign all of the elements in the array to a single value:
// pagedArray.set(1.0);  
// </srcblock>
//
// The destructor for PagedArray is called when the PagedArray  goes
// out of scope.  If you have attached a filename to the PagedArray --
// by using the proper constructor, or by call the setName member function --
// then the destructor will write the PagedArray to disk.  If there
// is no filename explicitly attached to the PagedArray, then the 
// destructor deletes the (temporary) disk storage that had been used
// by the PagedArray, and all traces of it are gone.
//
// Example 2:  read the PagedArray, modify it (in a very simple-minded way)
// and save the results.
// <srcblock>
// // recreate a pagedArray from data in a table
// PagedArray<Float> pagedArray(myTable);
// cout << "example has shape: " << pagedArray.shape () << endl;
// // change every element's value.  (this invokes an internal iterator
// // to traverse the array one plane at a time.)
// pagedArray.set(2.0);
// </srcblock>
//
// <note role=tip> 
// We discourage direct calls to getSlice and putSlice and
// recommend that you use LatticeIterators whenever you can, since
// they are designed to make traversal quite easy.
// So it's with some reluctance that we present the following example.
// It's included for two reasons: first, some PagedArray manipulations
// simply are not traversals, so a LatticeIterator is not always the
// answer.  Second, it is helpful to understand how getSlice and putSlice
// are called, because that sheds light on the operation of PagedArrays.
// </note>
// <note role=caution>
// getSlice and putSlice use arrays (or array subclasses) and these
// must be axis-congruent with the PagedArray you are working with.
// So, if you want to call getSlice on a 3-d PagedArray (as in the example 
// just below) you must supply a 3-d array where the slice will be go.
// If you only want a 2-d slice, you still must provide a 3-d array --
// though it will have a degenerate 3rd axis (its length will be 1).
// </note>
//
// Example 3: read the PagedArray, get the first plane, find the sum of
// the elements in that plane
// <srcblock>
//  PagedArray<Float> pagedArray(myTable);
//  uInt width = pagedArray.shape()(0);
//  uInt height = pagedArray.shape()(1);
//  uInt depth = pagedArray.shape()(2);
//  // make some helpful IPositions for the slice
//  IPosition start(3,0);    
//  IPosition shape(3, width, height, 1);
//  IPosition stride(3,1);   
//  // declare a cube to act as our buffer
//  Cube<Float> degenerateCube;  
//  // get a slice into the buffer
//  pagedArray.getSlice(degenerateCube, start, shape, stride);
//  // use the buffer for something...
//  Float runningSum = 0.0;
//  for (uInt row = 0; row < height; row++)
//    for (uInt column = 0; column < width; column++)
//      runningSum += degenerateCube(IPosition(3,row,column,0));
//  // print the results      
//  cout << "sum of elements in plane 0: " << runningSum  << endl;
// </srcblock>
//
// Example 4: read the PagedArray, assign new values to each element in
// every plane, then write them back with putSlice
// Note the use of degenerate third axis in "degenerateCube"
// <srcblock>
//  PagedArray<Float> pagedArray(myTable);
//  if(pagedArray.shape().nelements() != 3)
//    throw (AipsError ("demonstration PagedArray is not a cube."))
//  // make a buffer for slicing      
//  Cube<Float> degenerateCube;
//  // declare some loop boundaries
//  uInt width = pagedArray.shape()(0);
//  uInt height = pagedArray.shape()(1);
//  uInt depth = pagedArray.shape()(2);
//  // declare some slice parameters
//  IPosition stride(3,1);
//  IPosition shape(3, width, height, 1);
//  // loop through ...
//  for(uInt plane=0; plane < depth; plane++) {
//    IPosition start(3,0,0,plane);
//    pagedArray.getSlice(degenerateCube, start, shape, stride);
//    for(uInt row = 0; row < height; row++)
//      for(uInt column = 0; column < width;  column++)
//        degenerateCube(IPosition(3, row,column,0)) =
//          (plane * 100) + (row * 10) + column;
//    // put the value back...  
//    pagedArray.putSlice(degenerateCube, start, stride);
//  }
// </srcblock>
//  
// Example 5: demonstrating the virtues of the LatticeIterator class
// for traversing a PagedArray.  This example function finds the average
// value of the elements in each plane.
// <srcblock> 
//  void demonstrateLatticeIterator(const String &inputFilename)
//  {
//    SetupNewTable newSetup(inputFilename, TableDesc("", TableDesc::Scratch), 
//			     Table::Update);
//    Table standard(newSetup);   
//    PagedArray<Float> pagedArray(standard);
//
//    if(pagedArray.shape().nelements() != 3)
//       throw(AipsError("demonstration PagedArray is not a cube."));
// 
//    uInt width = pagedArray.shape()(0);
//    uInt height = pagedArray.shape()(1);
//
//    IPosition windowShape(2, width, height);
//    IPosition stride(2,1,1);
//
//    // create an iterator with 
//    // a Matrix<Float> cursor (a window into the data), whose shape
//    // is defined by the 'windowShape' parameter
//
//    RO_LatticeIterator<Float> iterator(pagedArray, windowShape);
//    const Matrix<Float> &myCursor = iterator.matrixCursor();
//
//    uInt planeNumber = 0;
//    for (iterator.reset(); !iterator.atEnd(); iterator++) {
//      Float sum = 0.0;
//      for (uInt column = 0; column < width; column++) {
//        for (uInt row = 0; row < height; row++) 
//          sum += myCursor(row, column);
//      }
//      cout << "Plane " << planeNumber++ << " has average value "
//           << sum / (windowShape (0) * windowShape (1)) << endl;
//    } 
//  }
// </srcblock>
// </example>
//
// <motivation>
// Arrays of data are sometimes much too large to hold in random access memory.
// PagedArrays -- especially in combination with LatticeIterators -- 
// provide convenient access to such large data sets.
// </motivation>
//
// <templating arg=T>
//  <li> Due to storage in Tables, the templated type must be able to be 
// stored in an AIPS++ Table.  See RetypedArrayEngine for help.
// </templating>
//
// <todo asof="1995/04/14">
//   <li> TableColumn::rename() went away.  This broke 
//        PagedArray::setColumnName(...) and an alternative method has 
//        not been resolved.
//   <li> Table::removeColumn() has yet to be implemented.  This prevents
//        PagedArray::resize() from working for non-default constructed
//        instances.
// </todo>

template <class T> class PagedArray : public Lattice<T>
{
public: 
  // default ctor, useless (though legal) until assigned to
  PagedArray();

  // construct a new PagedArray, with the data assuming 'arrayShape', in
  // the given Table.
  PagedArray(const IPosition &arrayShape, Table &theTable);

  // construct a new PagedArray, with the data assuming 'arrayShape', in
  // the given Table. maxPixels defines the maximum number of pixels
  // in the tile
  PagedArray(const IPosition &arrayShape, Table &theTable, const Int maxPixels);

  // construct a new PagedArray, with data of 'arrayShape', stored
  // in 'theTable', with a TableColumn name, and a row number (defaults 
  // to row zero.)
  PagedArray(const IPosition &arrayShape, Table &theTable, 
	     const String &columnName, Int rowNum = 0);

  // reconstruct from a pre-existing PagedArray in the Table, 
  PagedArray(Table &theTable);

  // reconstruct from a pre-existing PagedArray in the Table, with 
  // TableColumn name, and row number (defaults to row zero)
  PagedArray(Table &theTable, const String &columnName, Int rowNum = 0);

  // the copy constructor (reference semantics):  passing by value
  // doesn't make sense, because it would require the creation of a
  // temporary (but possibly huge) file on disk
  PagedArray(const PagedArray<T> &other);
  
  // destructor
  ~PagedArray();
  
  // the assignment operator with reference semantics.
  PagedArray<T> &operator=(const PagedArray<T> &other);
  
  // a default-constructed PagedArray has no data.  To store data you will
  // have to set the table it will exist in.
  void setTable(Table &Table);

  // return the Table this instance is stored in.
  Table &table();

  // a default constructed PagedArray is automatically given a temporary
  // TableColumn name.  You may change it here.
  //  void setColumnName(const String &newColumnName);

  // returns the current TableColumn name 
  const String &columnName() const;

  // a default constructed PagedArray is automatically given a temporary
  // TableColumn row number.  You may change it here.
  void setRowNumber(uInt rowNum);

  // returns the current TableColumn row number 
  uInt rowNumber() const;

  // returns the shape of the PagedArray.
  IPosition shape() const;

  // function to set the shape of this instance.
  void resize(const IPosition &newShape);

  // function which extracts an Array of values from a Lattice - a read-only 
  // operation. 
  // getSlice parameters:
  // <ul>
  // <li> buffer: an Array<T> with a shape that is unimportant.  The 
  //      sub-class implementation should always call Array::resize(uInt) 
  //      in order to match the Array<T> shape to the "shape" argument.
  // <li> start: an IPosition which must have the same number of axes
  //      as the underlying Lattice, otherwise, throw an exception.
  // <li> shape: an IPosition which must have equal or fewer axes than the 
  //      true shape od the Lattice, otherwise, throw an exception
  // <li> stride: an IPosition which must have the same number of axes
  //      as the underlying Lattice, otherwise, throw an exception.
  // <li> removeDegenerateAxes: a Bool which dictates whether to remove 
  //      "empty" axis created in buffer. (e.g. extracting an n-dimensional 
  //      from an (n+1)-dimensional will fill 'buffer' with an array that 
  //      has a degenerate axis (i.e. one axis will have a length = 1.))
  // </ul>
  // 
  // The sub-class implementation of these functions return
  // 'True' if Array <T> buffer contains a reference when this function
  // returns, and 'False' if it contains a copy.
  // <note role=tip> 
  // In most cases, it will be more efficient in execution, if you
  // use a LatticeIterator class to move through the Lattice. 
  // LatticeIterators are optimized for that purpose.  If you are doing 
  // unsystematic traversal, or random gets and puts, then getSlice and 
  // putSlice or operator() may be the right tools to use.
  // </note>
  // <group>   
  Bool getSlice(COWPtr<Array<T> > &buffer, const IPosition &start, 
		const IPosition &shape, const IPosition &stride, 
		Bool removeDegenerateAxes=False) const;
  
  Bool getSlice(COWPtr<Array<T> > &buffer, const Slicer &theSlice, 
		Bool removeDegenerateAxes=False) const;
  
  Bool getSlice(Array<T> &buffer, const IPosition &start, 
		const IPosition &shape, const IPosition &stride,
		Bool removeDegenerateAxes=False);

  Bool getSlice(Array<T> &buffer, const Slicer &theSlice, 
		Bool removeDegenerateAxes=False);
  // </group>

  // function which places an Array of values within the lattice
  void putSlice(const Array <T> &sourceBuffer, const IPosition &where, 
		const IPosition &stride);

  // These are the true implementations of the paran operator.
  // <note> Not for public use </note>
  // <group>
  T getAt(const IPosition &where) const;
  void putAt(const T &value, const IPosition &where);
  // </group>
  
  // These are the PagedArray letter implementations for the LatticeIterator
  // envelope class <note> Not for public use </note>
  // <group>
  virtual RO_LatticeIterInterface<T> * makeIter(const LatticeNavigator & 
						navigator) const;

  virtual LatticeIterInterface<T> * makeIter(const LatticeNavigator & 
					     navigator);
  // </group>

  // Help the user pick a cursor for most efficient access if he only wants
  // pixel values and doesn't care about the order. Usually just use
  // <src>IPosition shape = pa.niceCursorShape(pa.maxPixels());</src>
  virtual uInt maxPixels() const;
  virtual IPosition niceCursorShape(uInt maxPixels) const;

  // Maximum size - not necessarily all used. In pixels.
  uInt cacheSize() const;
  // Set the maximum (allowd) cache size as indicated.
  void setCacheSize(uInt howManyPixels);
  // Set the cache size as to "fit" the indicated path.
  void setCacheSizeFromPath(const IPosition& sliceShape,
			    const IPosition& windowStart,
			    const IPosition& windowLength,
			    const IPosition& axisPath);
  // Clears and frees up the caches, but the maximum allowd cache size is 
  // unchanged from when setCacheSize was called
  void clearCache() const;

  // Report on cache success
  void showCacheStatistics(ostream &os);

  friend class RO_PagedArrIter<T>;
  friend class PagedArrIter<T>;
protected:

  // a handy place to check for internal consistency
  Bool ok() const;

  // an "indirect array" in an aips++ Table is current implementation
  // of the PagedArray.  "indirect" means that the array is not
  // physically present in the table, but is stored separately in
  // a manner decided by the current storage manager.  (we use
  // the tiled cell storage manager)  This indirectness is the reason 
  // why multiples files, with related names, are associated with 
  // each PagedArray.
  Table table_p;
  String columnname_p;
  ArrayColumn<T> indirectArray_p;
  Int rownumber_p;

private:
  void setTableType();
};

// PagedArray helper function - not for public use.
//<group>
template <class T> 
void buildStandardColumn(Table &table, const String &columnName,
			 const String &columnComment, Int rowNumber,
			 ArrayColumn<T> &indirectArray_p, 
			 const IPosition &shape,
			 Int maxPixels);

void buildColumn(Bool tmp, Table &table, const String &columnName,
		 const String &columnComment, Int rowNumber,
		 ArrayColumn<Bool> &indirectArray_p, 
		 const IPosition &shape,
		 Int maxPixels);

void buildColumn(Int tmp, Table &table, const String &columnName,
		 const String &columnComment, Int rowNumber,
		 ArrayColumn<Int> &indirectArray_p, 
		 const IPosition &shape,
		 Int maxPixels);

void buildColumn(Float tmp, Table &table, const String &columnName,
		 const String &columnComment, Int rowNumber,
		 ArrayColumn<Float> &indirectArray_p, 
		 const IPosition &shape,
		 Int maxPixels);

void buildColumn(Double tmp, Table &table, const String &columnName,
		 const String &columnComment, Int rowNumber,
		 ArrayColumn<Double> &indirectArray_p, 
		 const IPosition &shape,
		 Int maxPixels);

void buildColumn(Complex tmp, Table &table, const String &columnName,
		 const String &columnComment, Int rowNumber,
		 ArrayColumn<Complex> &indirectArray_p, 
		 const IPosition &shape,
		 Int maxPixels);

void buildColumn(DComplex tmp, Table &table, const String &columnName,
		 const String &columnComment, Int rowNumber,
		 ArrayColumn<DComplex> &indirectArray_p, 
		 const IPosition &shape,
		 Int maxPixels);

//</group>

#endif
