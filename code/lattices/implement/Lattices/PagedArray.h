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
#include <aips/Logging/LogIO.h>

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

// <summary>An Array that is read/written from/to disk on demand</summary>
//
// <use visibility=export>
// 
// <reviewed reviewer="" date="" tests="tPagedArray.cc" demos="dPagedArray.cc">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class="Lattice">Lattice</linkto>
//   <li> <linkto class="Array">Array</linkto>
// </prerequisite>
//
// <etymology>

// "Demand paging" is a technique used to implement virtual memory in
// computer operating systems.  In this scheme, code or data are read from
// disk to memory only as needed by a process, and are read in fixed-sized
// chunks called "pages".  PagedArrays are somewhat the same -- though
// without the automatic features found in virtual memory demand paging.
// However PagedArrays do allow the user to access chunks of the disk in a
// flexible way, that can match the requirements of many algorithms.
// </etymology>
//
// <synopsis> 

// At the time of writing, typical scientific computers provide sufficient
// memory for storing and manipulating 2-dimensional astronomical images,
// which have average size of around 8MBytes.  Astronomy is increasingly
// using three or higher dimensional arrays, which can be larger by one or
// two orders of magnitude. PagedArrays provide a convenient way of
// accessing these large arrays without requiring all the data to be read
// into real or virtual memory.

// When you construct a PagedArray you do not read any data into
// memory. Instead a disk file (ie. a Table) is created, in a place you
// specify, to hold the data. This means you need to have enough disk space
// to hold the array. Constructing a PagedArray is equivalent to opening a
// file. 

// Because the data is stored on disk it can be saved after the program,
// function or task that created the PagedArray has finished. This saved
// array can then be read again at a later stage. 

// So there are two reasons for using a PagedArray:
// <ol>
// <li> To provide arrays that are too large for the computers memory.
// <li> To provide a way of saving arrays to disk for later access. 
// </ol>

// To access the data in a pagedArray you can either:
// <ol>
// <li> Use a <linkto class=LatticeIterator>LatticeIterator</linkto>
// <li> Use the getSlice and putSlice member functions
// <li> Use the parenthesis operators.
// </ol>
// These access methods are given in order of preference.  Some examples of
// these access methods are in the documentation for the 
// <linkto class=Lattice>Lattice</linkto> class as well as below. 

// In nearly all cases you access the PagedArray by reading a "slice" of the
// PagedArray into an aips++ <linkto class=Array>Array</linkto>. Because the
// slice is stored in memory it is important that the slice you read is not
// too big compared to the physical memory on your computer. Otherwise your
// computer will page excessively and performance will be poor.

// To overcome this you may be tempted to access the PagedArray a pixel at a
// time. This will use little memory but the overhead of accessing a large
// data set by separately reading each pixel from disk will also lead to poor
// performance.

// In general the best way to access the data in PagedArrays is to use a
// LatticeIterator with a cursor size that "fits" nicely into memory. Not
// only do the LaticeIterator classes provide a relatively simple way to
// read/write all the data but they optimally set up the cache that is
// associated with each PagedArray. 

// If the LatticeIterator classes do not access the data the way you want
// you can use the getSlice and putSlice member functions. These functions
// do not set up the cache for you and improved performance may be obtained
// by tweaking the cache using the setCacheSizeFromPath member frunction.

// <A NAME="PagedArray:advanced">
// <h3><More Details></h3></A>
// In order to utilise PagedArrays fully and understand many of the member
// functions and data access methods in this class you need to be familiar
// with some of the concepts involved in the implementation of PagedArrays.

// Each PagedArray is stored in one cell of a Table as an indirect Array
// (see the documentation for <linkto module=Table>Tables</linkto> module
// for more information). This means that multiple PagedArrays can be stored
// in one Table. To specify which PagedArray you are referring to in a given
// Table you need to specify the cell using its column name and row number
// during construction. If a cell is not specified the default column name
// (as given by the defaultColumnName function) and row number (as given by
// the defaultRowNumber function) are used. This ability to store multiple
// PagedArrays's is used in the PagedImage class where the image is stored
// in one cell and a mask is optionally stored in a another column in the
// same row.

// There are currently a number of limitations when storing multiple
// PagedArrays in the same Table. 
// <ul>
// <li> All the PagedArrays in the same column MUST have the same number of
// dimensions. The dimension used for any particular column is set when the
// first PagedArray in that column is constructed. If you want to put a
// say two-dimensional PagedArray  into another row of a column that
// already contains a four-dimensional PagedArray you need to add two
// degenerate axes. In principle you could use the resize function, but see
// below for why this is not recommended. It is better to just ensure that
// all the PagedArrays have the same number of dimensions.
// <li> All the cells in a column that contains PagedArrays must have their
// shape defined. This becomes important if you are creating a PagedArray in
// say row five of a Table that currently only has one row. The PagedArray
// constructor will add another four rows to the Table, and put your
// PagedArray (with the shape you specify) in row five. For the other three
// rows, for which no shape was specified, the constructor will construct
// PagedArrays with only one element (and of an appropriate
// dimensionality). As you cannot resize these single element PagedArrays
// without difficulty (see below), it is recommended that you add
// PagedArrays to rows in your Table sequentially. It is necessary to have
// the constructor define the shape of all cells in the Table as it is an
// error to write a Table to disk with undefined cell shapes.
// </ul>

// Each PagedArray is stored on disk using the tiled cell storage manager
// (<linkto class=TiledCellStMan>TiledCellStMan</linkto>). This stores the
// data in the Array in tiles which are regular subsections of the
// PagedArray. For example a PagedArray of shape [512,512,4,32] may have a
// tile shape of [32,16,4,16]. The data in each tile is stored as a unit on
// the disk. This means that there is no prefered axes when accessing
// multi-dimensional arrays.

// The tile shape can be specified when constructing a new PagedArray but
// not when reading an old one as it is intrinsic to the way the data is
// stored on disk. It is NOT recommended that you specify the tile shape
// unless you can control the lifetime of the PagedArray (this includes the
// time it spends on disk), or can guarentee the access pattern. For example
// if you know that a PagedArray of shape [512,512,4,32] will always be
// sliced plane by plane you may prefer to specify a tile shape of
// [512,64,1,1] rather than the default of [32,16,4,16]. 

// Tiles can be cached by the tile storage manager so that it does not need
// to read the data from disk every time you are accessing the a pixel in a
// different tile. In order to cache the correct tiles you should tell the
// storage manager what section of the PagedArray you will be
// accessing. This is done using the setCacheSizeFromPath member
// function. 

// By default there is no limit on how much memory the tile cache can
// consume. This can be changed using the setMaximumCacheSize member
// function. Because the tiled storage manager always tries to cache enough
// pixels to ensure that each tile is read from disk only once, setting the
// maximum cache size will trade off memory usage for disk I/O. This is best
// illustrated in example XXX below. 

// The showCacheStatistics member function is provided to allow you to
// evaluate the performance of the tile cache.

// It is not recommended that you use the resize member function. It
// currently has a number of limitations. These occur because the Table
// system does not allow you to change the shape of a cell that is written
// using the tiled cell storage manager. To work around this the resize
// function will create a new column in the Table with the same name as the
// original column but with a "_" appended. The resize function then copies
// all the PagedArrays from the old column to the new column, except the one
// that needs to be resized. This PagedArray is created from scratch so its
// previous data is lost.

// Currently the Table system cannot delete columns. This means that the old
// column cannot be removed and continues to use disk space. The table
// system cannot currently rename columns either. So when you use the resize
// function you need to be aware that the column name of your PagedArray
// will change (as described above). This can lead to additional problems
// when reading a PagedArray from disk.

// </synopsis> 

// <example>
// Example 1: create a 512x512x32 PagedArray of Floats in a file called "myData.array"
// <srcblock>
// IPosition arrayShape(3,512,512,32);
// String filename("myData.array");
// // construct a PagedArray by supplying the array shape and filename
// PagedArray<Float> diskArray(arrayShape, filename);
// // assign all of the elements in the array to a single value:
// diskArray.set(0.0f);  // This is an efficient way to initialise the PagedArray
// </srcblock>
// The destructor for PagedArray is called when the PagedArray goes
// out of scope. In this example you will be left with a directory called
// "myData.array" on disk that contains files that exceed 512*512*32*4
// (=32MBytes) in size.
//
// Example 2:  read the PagedArray, modify it (in a very simple-minded way)
// The results get stored when the PagedArray is destructed. 
// <srcblock>
// // recreate a pagedArray from data in a table
// PagedArray<Float> diskArray("myData.array");
// cout << "example has shape: " << pagedArray.shape() << endl;
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
// Example 3: demonstrating the virtues of using a LatticeIterator to
// traverse a PagedArray.  This example function finds the average value of
// the elements in each plane.
// <srcblock> 
//  void demonstrateLatticeIterator(const String & inputFilename)
//  {
//    PagedArray<Float> pa(inputFilename);
//    const shape = pa.shape()
//    if (shape.nelements() != 3)
//       throw(AipsError("demonstration PagedArray is not a cube."));
// 
//    uInt width = shape(0);
//    uInt height = shape(1);
//
//    IPosition windowShape(2, width, height);
//    IPosition stride(2,1,1);
//
//    // create an iterator with 
//    // a Matrix<Float> cursor (a window into the data), whose shape
//    // is defined by the 'windowShape' parameter
//
//    RO_LatticeIterator<Float> iterator(pagedArray, windowShape);
//    const Matrix<Float> & myCursor = iterator.matrixCursor();
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
// <todo asof="1997/04/14">
//   <li> A better way of resizing PagedArrays
//   <li> The ability to construct a PagedArray from a Table that is opened
//   readonly. (ie a readonly PagedArray). 
// </todo>

template <class T> class PagedArray : public Lattice<T>
{
public: 
  // The default constructor creates a PagedArray that is useless for just
  // about everything, except that it can be assigned to with the assignment
  // operator.
  PagedArray();

  // Construct a new PagedArray with the specified shape. A new Table with
  // the specified filename is constructed to hold the array.
  PagedArray(const IPosition & shape, const String & filename);

  // Construct a new PagedArray with the specified shape. A scratch Table is
  // created in the current working directory to hold the array.
  PagedArray(const IPosition & shape);

  // construct a new PagedArray, with the specified shape, in the default
  // row and column of the supplied Table.
  PagedArray(const IPosition & shape, Table & file);

  // construct a new PagedArray, with the specified shape, in the default
  // row and column of the supplied Table. The tile shape for the PagedArray
  // can also be specified.
  PagedArray(const IPosition & shape, Table & file, const IPosition tileShape);

  // construct a new PagedArray, with the specified shape, in the specified
  // row and column of the supplied Table. Use the default tile shape.
  PagedArray(const IPosition & shape, Table & file,
	     const String & columnName, uInt rowNum);

  // construct a new PagedArray, with the specified shape, in the specified
  // row and column of the supplied Table. The tile shape for the PagedArray
  // also needs to be specified.
  PagedArray(const IPosition & shape, Table & file,
	     const String & columnName, uInt rowNum, 
	     const IPosition tileShape);

  // reconstruct from a pre-existing PagedArray in the default row and
  // column of the supplied Table with the supplied filename.
  PagedArray(const String & filename);

  // reconstruct from a pre-existing PagedArray in the default row and
  // column of the supplied Table.
  PagedArray(Table & file);

  // reconstruct from a pre-existing PagedArray in the specified row and
  // column of the supplied Table.
  PagedArray(Table & file, const String & columnName, uInt rowNum);

  // the copy constructor which uses reference semantics. Passing by value
  // doesn't make sense, because it would require the creation of a
  // temporary (but possibly huge) file on disk.
  PagedArray(const PagedArray<T> & other);
  
  // destructor.
  ~PagedArray();
  
  // the assignment operator with reference semantics. As with the copy
  // constructor assigning by value does not make sense.
  PagedArray<T> & operator=(const PagedArray<T> & other);
  
  // returns the shape of the PagedArray.
  virtual IPosition shape() const;

  // functions to resize the PagedArray. The old contents are lost.
  // <group>
  void resize(const IPosition & newShape);
  void resize(const IPosition & newShape, const IPosition & tileShape);
  // </group>

  // function which extracts an Array of values from a Lattice - a read-only 
  // operation. 
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
  virtual Bool getSlice(COWPtr<Array<T> > & buffer, const IPosition & start, 
			const IPosition & shape, const IPosition & stride, 
			Bool removeDegenerateAxes=False) const;
  virtual Bool getSlice(COWPtr<Array<T> > & buffer, const Slicer & section,
			Bool removeDegenerateAxes=False) const;
  virtual Bool getSlice(Array<T> & buffer, const IPosition & start, 
			const IPosition & shape, const IPosition & stride,
			Bool removeDegenerateAxes=False);
  virtual Bool getSlice(Array<T> & buffer, const Slicer & section, 
			Bool removeDegenerateAxes=False);
  // </group>

  // function which places an Array of values within the PagedArray
  // <group>
  virtual void putSlice(const Array <T> & sourceBuffer, 
			const IPosition & where, const IPosition & stride);
  virtual void putSlice(const Array <T> & sourceBuffer, 
			const IPosition & where);
  // </group>

  // returns the current table name (ie. filename) of this PagedArray
  const String & tableName() const;

  // returns the current Table column name of this PagedArray
  const String & columnName() const;

  // returns the default TableColumn name for PagedArray's
  static String defaultColumn();

  // returns the current row number of this PagedArray.
  uInt rowNumber() const;

  // returns the default row number for PagedArray's
  static uInt defaultRow();

  // returns the current tile shape for this PagedArray.
  IPosition tileShape() const;

  // returns the default tileShape for a specified Lattice shape
  static IPosition defaultTileShape(const IPosition & latticeShape);

  // Returns the maximum recommended number of pixels for a cursor. This is
  // the number of pixels in a tile. 
  virtual uInt maxPixels() const;

  // Help the user pick a cursor for most efficient access if they only want
  // pixel values and doent care about the order or dimension of the
  // cursor. Usually the tile shape is the best cursor shape, and this can
  // be obtained using:<br>
  // <src>IPosition shape = pa.niceCursorShape(pa.maxPixels())</src> where
  // <src>pa</src> is a PagedArray object.
  virtual IPosition niceCursorShape(uInt maxPixels) const;

  // Set the maximum allowed cache size for all Arrays in this column of the
  // Table.  The actual value used may be smaller. A value of zero means
  // that there is no maximum.
  void setMaximumCacheSize(uInt howManyPixels);

  // Return the maximum allowed cache size (in pixels) for all Arrays in
  // this column of the Table. The actual cache size may be smaller. A
  // value of zero means that no maximum is currently defined.
  uInt maximumCacheSize() const;

  // Set the actual cache size for this Array to "fit" the indicated
  // path. This cache is not shared with PagedArrays in other rows and is
  // always less than the maximum value. 
  void setCacheSize(uInt howManyPixels);

  // Set the actual cache size for this Array to "fit" the indicated
  // path. This cache is not shared with PagedArrays in other rows and is
  // always less than the maximum value. The sliceShape is the cursor or
  // slice that you will be requiring (with each call to
  // {get,put}Slice). The windowStart and windowLength delimit the range of
  // pixels that will ultimatly be accessed. The AxisPath is described in
  // the documentation for the LatticeStepper class.
  void setCacheSizeFromPath(const IPosition & sliceShape,
			    const IPosition & windowStart,
			    const IPosition & windowLength,
			    const IPosition & axisPath);

  // Clears and frees up the tile cache. The maximum allowed cache size is
  // unchanged from when <src>setMaximumCacheSize</src> was last called.
  void clearCache() const;

  // Generate a report on cache how the cache is doing. This is reset every
  // time <src>clearCache</src> is called.
  void showCacheStatistics(ostream & os);

  // These are the true implementations of the parenthesis operator. It will
  // probably be more convienient to use the actual parenthesis operator
  // defined in the Lattice base class.
  // <group>
  virtual T getAt(const IPosition & where) const;
  virtual void putAt(const T & value, const IPosition & where);
  // </group>
  
  // a function which checks for internal consistency. Returns False if
  // something nasty has happened to the PagedArray.
  Bool ok() const;

  // These functions are used by the LatticeIterator class to generate an
  // iterator of the correct type for a specified Lattice. Not recommended
  // for general use. 
  // <group>
  virtual RO_LatticeIterInterface<T> * makeIter(
                                   const LatticeNavigator & navigator) const;

  virtual LatticeIterInterface<T> * makeIter(
                                   const LatticeNavigator & navigator);
  // </group>

private:
  // Set the data in the TableInfo file
  void setTableType();
  // make the ArrayColumn
  void makeArray(const IPosition & shape, const IPosition & tileShape);
  // Make a Table for this object
  void makeTable(const String & filename, Table::TableOption option);
  // Check the user supplied tileshape is valid
  void checkTileShape(const IPosition & shape, const IPosition & tileShape);
  // Ther default comment for PagedArray Colums
  static String defaultComment();

  // an "indirect array" in an aips++ Table is the implementation of a
  // PagedArray.  "indirect" means that the array is not physically present
  // in the table, but is stored separately in a manner decided by the tiled
  // cell storage manager.
  Table theTable;
  String theColumnName;
  Int theRowNumber;
  ArrayColumn<T> theArray;
  LogIO theLog;
};
#endif
