//# PagedArray.h: templated Lattice, paged from disk to memory on demand
//# Copyright (C) 1994,1995,1996,1997,1998
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

//# Includes
#include <aips/aips.h>
#include <trial/Lattices/Lattice.h>
#include <trial/Lattices/TiledShape.h>
#include <aips/Tables/ArrayColumn.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/TiledStManAccessor.h>
#include <aips/Utilities/String.h>
#include <aips/Logging/LogIO.h>

//# Forward Declarations
class LatticeNavigator;
class IPosition;
class Slicer;
template <class T> class Array;
template <class T> class RO_LatticeIterInterface;
template <class T> class LatticeIterInterface;
template <class T> class RO_PagedArrIter;
template <class T> class PagedArrIter;
#if defined(AIPS_STDLIB)
#include <iosfwd.h>
#else
class ostream;
#endif


// <summary>
// A Lattice that is read from or written to disk.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tPagedArray.cc" demos="dPagedArray.cc">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="Lattice">Lattice</linkto>
//   <li> <linkto class="TiledShape">TiledShape</linkto>
// </prerequisite>

// <etymology>
// "Demand paging" is a technique used to implement virtual memory in
// computer operating systems.  In this scheme, code or data are read from
// disk to memory only as needed by a process, and are read in fixed-sized
// chunks called "pages".  PagedArrays are somewhat the same -- though
// without the automatic features found in virtual memory demand paging.
// However PagedArrays do allow the user to access chunks of the disk in a
// flexible way, that can match the requirements of many algorithms.
// </etymology>

// <synopsis> 
// At the time of writing, typical scientific computers provide sufficient
// memory for storing and manipulating 2-dimensional astronomical images,
// which have average size of around 8 MBytes.  Astronomy is increasingly
// using three or higher dimensional arrays, which can be larger by one or
// two orders of magnitude. PagedArrays provide a convenient way of
// accessing these large arrays without requiring all the data to be read
// into real or virtual memory.
// <p>
// When you construct a PagedArray you do not read any data into
// memory. Instead a disk file (ie. a Table) is created, in a place you
// specify, to hold the data. This means you need to have enough disk space
// to hold the array. Constructing a PagedArray is equivalent to opening a
// file. 
// <p>
// Because the data is stored on disk it can be saved after the program,
// function, or task that created the PagedArray has finished. This saved
// array can then be read again at a later stage. 
// <p>
// So there are two reasons for using a PagedArray:
// <ol>
// <li> To provide arrays that are too large for the computers memory.
// <li> To provide a way of saving arrays to disk for later access. 
// </ol>
//
// To access the data in a PagedArray you can either:
// <ol>
// <li> Use a <linkto class=LatticeIterator>LatticeIterator</linkto>
// <li> Use the getSlice and putSlice member functions
// <li> Use the parenthesis operator or getAt and putAt functions
// </ol>
// These access methods are given in order of preference.  Some examples of
// these access methods are in the documentation for the 
// <linkto class=Lattice>Lattice</linkto> class as well as below. 
// <p>
// In nearly all cases you access the PagedArray by reading a "slice" of the
// PagedArray into an aips++ <linkto class=Array>Array</linkto>. Because the
// slice is stored in memory it is important that the slice you read is not
// too big compared to the physical memory on your computer. Otherwise your
// computer will page excessively and performance will be poor.
// <p>
// To overcome this you may be tempted to access the PagedArray a pixel at a
// time. This will use little memory but the overhead of accessing a large
// data set by separately reading each pixel from disk will also lead to poor
// performance.
// <p>
// In general the best way to access the data in PagedArrays is to use a
// LatticeIterator with a cursor size that "fits" nicely into memory. Not
// only do the LaticeIterator classes provide a relatively simple way to
// read/write all the data but they optimally set up the cache that is
// associated with each PagedArray. 
// <p>
// If the LatticeIterator classes do not access the data the way you want
// you can use the getSlice and putSlice member functions. These functions
// do not set up the cache for you and improved performance may be obtained
// by tweaking the cache using the setCacheSizeFromPath member frunction.
//
// <A NAME="PagedArray:advanced"><h3>More Details</h3></A>
// In order to utilise PagedArrays fully and understand many of the member
// functions and data access methods in this class you need to be familiar
// with some of the concepts involved in the implementation of PagedArrays.
// <p>
// Each PagedArray is stored in one cell of a Table as an indirect Array
// (see the documentation for the <linkto module="Tables">Tables</linkto>
// module for more information). This means that multiple PagedArrays can be
// stored in one Table. To specify which PagedArray you are referring to in
// a given Table you need to specify the cell using its column name and row
// number during construction. If a cell is not specified the default column
// name (as given by the defaultColumnName function) and row number (as
// given by the defaultRowNumber function) are used. This ability to store
// multiple PagedArrays's is used in the PagedImage class where the image is
// stored in one cell and a mask is optionally stored in a another column in
// the same row.
// <p>
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
// PagedArray (with the shape you specify) in row five. For the three
// rows for which no shape was specified, the constructor will construct
// PagedArrays with only one element (and of an appropriate
// dimensionality). As you cannot resize these single element PagedArrays
// without difficulty (see below), it is recommended that you add
// PagedArrays to rows in your Table sequentially. It is necessary to have
// the constructor define the shape of all cells in the Table as it is an
// error to write a Table to disk with undefined cell shapes.
// </ul>
//
// Each PagedArray is stored on disk using the tiled cell storage manager
// (<linkto class=TiledCellStMan>TiledCellStMan</linkto>). This stores the
// data in tiles which are regular subsections of the PagedArray. For
// example a PagedArray of shape [512,512,4,32] may have a tile shape of
// [32,16,4,16]. The data in each tile is stored as a unit on the disk. This
// means that there is no preferred axis when accessing multi-dimensional
// data.
// <br>
// The tile shape can be specified when constructing a new PagedArray but
// not when reading an old one as it is intrinsic to the way the data is
// stored on disk. It is NOT recommended that you specify the tile shape
// unless you can control the lifetime of the PagedArray (this includes the
// time it spends on disk), or can guarantee the access pattern. For example
// if you know that a PagedArray of shape [512,512,4,32] will always be
// sliced plane by plane you may prefer to specify a tile shape of
// [512,64,1,1] rather than the default of [32,16,4,16]. 
// <br>
// Tiles can be cached by the tile storage manager so that it does not need
// to read the data from disk every time you are accessing the a pixel in a
// different tile. In order to cache the correct tiles you should tell the
// storage manager what section of the PagedArray you will be
// accessing. This is done using the setCacheSizeFromPath member
// function. Alternatively you can set the size of the cache using the
// setCacheSizeInTiles member function.
// <br>
// By default there is no limit on how much memory the tile cache can
// consume. This can be changed using the setMaximumCacheSize member
// function. The tiled storage manager always tries to cache enough tiles to
// ensure that each tile is read from disk only once, so setting the maximum
// cache size will trade off memory usage for disk I/O. Seeting the cache
// size is illustrated in example 5 below.
// <br>
// The showCacheStatistics member function is provided to allow you to
// evaluate the performance of the tile cache.
// </synopsis> 

// <example>
// All the examples in this section are available in dPagedArray.cc
//
// <h4>Example 1:</h4>
// Create a PagedArray of Floats of shape [512,512,4,32] in a file
// called "myData_tmp.array" and initialize it to zero. This will create a
// directory on disk called "myData_tmp.array" that contains files that
// exceed 512*512*4*32*4 (=128MBytes) in size.
// <srcblock>
// const IPosition arrayShape(4,512,512,4,32);
// const String filename("myData_tmp.array");
// PagedArray<Float> diskArray(arrayShape, filename);
// cout << "Created a PagedArray of shape " << diskArray.shape() 
//   << " (" << diskArray.shape().product()/1024/1024*sizeof(Float) 
//   << " MBytes)" << endl
//   << "in the table called " << diskArray.tableName() << endl;
// diskArray.set(0.0f);
// // Using the set function is an efficient way to initialize the PagedArray
// // as it uses a PagedArrIter internally. Note that the set function is
// // defined in the Lattice class that PagedArray is derived from. 
// </srcblock>
//
// <h4>Example 2:</h4>
// Read the PagedArray produced in Example 1 and put a Gaussian profile into
// each spectral channel.
// <srcblock>
// PagedArray<Float> diskArray("myData_tmp.array");
// IPosition shape = diskArray.shape();
// // Construct a Gaussian Profile to be 10 channels wide and centred on
// // channel 16. Its height is 1.0.
// Gaussian1D<Float> g(1.0f, 16.0f, 10.0f);
// // Create a vector to cache a sampled version of this profile.
// Vector<Float> profile(shape(3));
// indgen(profile.ac());
// profile.ac().apply(g);
// // Now put this profile into every spectral channel in the paged array. This
// // is best done using an iterator.
// LatticeIterator<Float> iter(diskArray, 
//                          TiledLineStepper(shape, diskArray.tileShape(), 3));
// for (iter.reset(); !iter.atEnd(); iter++) {
//    iter.woCursor() = profile;
// }
// </srcblock>
//
// <h4>Example 3:</h4>
// Now multiply the I-polarization data by 10.0 in this PagedArray. The
// I-polarization data occupies 32MBytes of RAM which is too big to read
// into the memory of most computers. So an iterator is used to get suitable
// sized chunks.
// <srcblock>
// Table t("myData_tmp.array", Table::Update);
// PagedArray<Float> da(t);
// const IPosition latticeShape = da.shape();
// const nx = latticeShape(0);
// const ny = latticeShape(1);
// const npol = latticeShape(2);
// const nchan = latticeShape(3);
// IPosition cursorShape = da.niceCursorShape();
// cursorShape(2) = 1;
// LatticeStepper step(latticeShape, cursorShape);
// step.subSection(IPosition(4,0), IPosition(4,nx-1,ny-1,0,nchan-1));
// LatticeIterator<Float> iter(da, step);
// for (iter.reset(); !iter.atEnd(); iter++) {
//    iter.rwCursor() *= 10.0f;
// }
// </srcblock>
// 
// <h4>Example 4:</h4>
// Use a direct call to getSlice to access a small central region of the
// V-polarization in spectral channel 0 only. The region is small enough
// to not warrent constructing iterators and setting up
// LatticeNavigators. In this example the call to the getSlice function
// is unnecessary but is done for illustration purposes anyway.
// <srcblock>
// SetupNewTable maskSetup("mask_tmp.array", TableDesc(), Table::New);
// Table maskTable(maskSetup);
// PagedArray<Bool> maskArray(IPosition(4, 512, 512, 4, 32), maskTable);
// maskArray.set(False);
// COWPtr<Array<Bool> > maskPtr;
// maskArray.getSlice(maskPtr, IPosition(4,240,240,3,0),
// 		      IPosition(4,32,32,1,1), IPosition(4,1));
// maskPtr.rwRef() = True;
// maskArray.putSlice(*maskPtr, IPosition(4,240,240,3,1));
// </srcblock>
// 
// <h4>Example 5:</h4>
// In this example the data in the PagedArray will be accessed a row at
// a time while setting the cache size to different values. The comments
// illustrate the results when running on an Ultra 1/140 with 64MBytes
// of memory.
// <srcblock>
// PagedArray<Float> pa(IPosition(4,128,128,4,32));
// const IPosition latticeShape = pa.shape();
// cout << "The tile shape is:" << pa.tileShape() << endl;
// // The tile shape is:[32, 16, 4, 16]
//   
// // Setup to access the PagedArray a row at a time
// const IPosition sliceShape(4,latticeShape(0), 1, 1, 1);
// const IPosition stride(4,1);
// Array<Float> row(sliceShape);
// IPosition start(4, 0);
//   
// // Set the cache size to enough pixels for one tile only. This uses
// // 128kBytes of cache memory and takes 125 secs
// pa.setCacheSizeInTiles (1);
// Timer clock;
// for (start(3) = 0; start(3) < latticeShape(3); start(3)++) {
//   for (start(2) = 0; start(2) < latticeShape(2); start(2)++) {
//     for (start(1) = 0; start(1) < latticeShape(1); start(1)++) {
//       pa.getSlice(row,  start, sliceShape, stride);
//     }
//   }
// }
// clock.show();
// pa.showCacheStatistics(cout);
// pa.clearCache();
//   
// // Set the cache size to enough pixels for one row of tiles (ie. 4)
// // This uses 512 kBytes of cache memory and takes 10 secs
// pa.setCacheSizeInTiles (4);
// clock.mark();
// for (start(3) = 0; start(3) < latticeShape(3); start(3)++) {
//   for (start(2) = 0; start(2) < latticeShape(2); start(2)++) {
//     for (start(1) = 0; start(1) < latticeShape(1); start(1)++) {
//       pa.getSlice(row,  start, sliceShape, stride);
//     }
//   }
// }
// clock.show();
// pa.showCacheStatistics(cout);
// pa.clearCache();
//   
// // Set the cache size to enough pixels for one plane of tiles
// // (ie. 4*8) This uses 4 MBytes of cache memory and takes 2 secs
// pa.setCacheSizeInTiles (4*8);
// clock.mark();
// for (start(3) = 0; start(3) < latticeShape(3); start(3)++) {
//   for (start(2) = 0; start(2) < latticeShape(2); start(2)++) {
//     for (start(1) = 0; start(1) < latticeShape(1); start(1)++) {
//       pa.getSlice(row,  start, sliceShape, stride);
//     }
//   }
// }
// clock.show();
// pa.showCacheStatistics(cout);
// pa.clearCache();
// </srcblock>
// </example>

// <motivation>
// Arrays of data are sometimes much too large to hold in random access memory.
// PagedArrays, especially in combination with LatticeIterator, 
// provide convenient access to such large data sets.
// </motivation>

// <templating arg=T>
//  <li> Due to storage in Tables, the templated type must be able to be 
// stored in an AIPS++ Table.  This restricts the template argument to all
// the common types Bool, Float, Double, Complex, String etc.) More details
// can be found in the RetypedArrayEngine class.
// </templating>

// <todo asof="1997/04/14">
//   <li> A better way of resizing PagedArrays
// </todo>

// <linkfrom anchor="PagedArray" classes="Lattice ArrayLattice">
//  <here>PagedArray</here> - a disk based Lattice.
// </linkfrom>


template <class T> class PagedArray : public Lattice<T>
{
public: 
  // The default constructor creates a PagedArray that is useless for just
  // about everything, except that it can be assigned to with the assignment
  // operator.
  PagedArray();

  // Construct a new PagedArray with the specified shape. A new Table with
  // the specified filename is constructed to hold the array. The Table will
  // remain on disk after the PagedArray goes out of scope or is deleted.
  PagedArray (const TiledShape& shape, const String& filename);

  // Construct a new PagedArray with the specified shape. A scratch Table is
  // created in the current working directory to hold the array. This Table
  // will be deleted automatically when the PagedArray goes out of scope or
  // is deleted.
  PagedArray (const TiledShape& shape);

  // construct a new PagedArray, with the specified shape, in the default
  // row and column of the supplied Table.
  PagedArray (const TiledShape& shape, Table& file);

  // construct a new PagedArray, with the specified shape, in the specified
  // row and column of the supplied Table. Use the default tile shape.
  PagedArray (const TiledShape& shape, Table& file,
	      const String& columnName, uInt rowNum);

  // reconstruct from a pre-existing PagedArray in the default row and
  // column of the supplied Table with the supplied filename.
  PagedArray (const String& filename);

  // reconstruct from a pre-existing PagedArray in the default row and
  // column of the supplied Table.
  PagedArray (Table& file);

  // reconstruct from a pre-existing PagedArray in the specified row and
  // column of the supplied Table.
  PagedArray (Table& file, const String& columnName, uInt rowNum);

  // the copy constructor which uses reference semantics. Passing by value
  // doesn't make sense, because it would require the creation of a
  // temporary (but possibly huge) file on disk.
  PagedArray (const PagedArray<T>& other);
  
  // the destructor flushes the PagedArrays contents to disk. 
  ~PagedArray();
  
  // the assignment operator with reference semantics. As with the copy
  // constructor assigning by value does not make sense.
  PagedArray<T>& operator= (const PagedArray<T>& other);
  
  // Make a copy of the object (reference semantics).
  virtual Lattice<T>* clone() const;

  // A PagedArray is always paged to disk.
  virtual Bool isPaged() const;

  // Is the PagedArray writable?
  virtual Bool isWritable() const;

  // returns the shape of the PagedArray.
  virtual IPosition shape() const;

  // functions to resize the PagedArray. The old contents are lost. Usage of
  // this function is NOT currently recommended (see the <linkto
  // class="PagedArray:Advanced">More Details</linkto> section above)
  void resize (const TiledShape& newShape);

  // returns the current table name (ie. filename) of this PagedArray
  const String& tableName() const;

  // returns the current Table column name of this PagedArray
  const String& columnName() const;

  // returns the default TableColumn name for PagedArray's
  static String defaultColumn();

  // returns an accessor to the tiled storage manager
  const ROTiledStManAccessor& accessor() const;

  // returns the current row number of this PagedArray.
  uInt rowNumber() const;

  // returns the default row number for PagedArray's
  static uInt defaultRow();

  // returns the current tile shape for this PagedArray.
  IPosition tileShape() const;

  // Returns the maximum recommended number of pixels for a cursor. This is
  // the number of pixels in a tile. 
  virtual uInt maxPixels() const;

  // Set the maximum allowed cache size for all Arrays in this column of the
  // Table.  The actual value used may be smaller. A value of zero means
  // that there is no maximum.
  void setMaximumCacheSize (uInt howManyPixels);

  // Return the maximum allowed cache size (in pixels) for all Arrays in
  // this column of the Table. The actual cache size may be smaller. A
  // value of zero means that no maximum is currently defined.
  uInt maximumCacheSize() const;

  // Set the actual cache size for this Array to be be big enough for the
  // indicated number of tiles. This cache is not shared with PagedArrays
  // in other rows and is always clipped to be less than the maximum value
  // set using the setMaximumCacheSize member function.
  // tiles. Tiles are cached using a first in first out algorithm.
  void setCacheSizeInTiles (uInt howManyTiles);

  // Set the actual cache size for this Array to "fit" the indicated
  // path. This cache is not shared with PagedArrays in other rows and is
  // always less than the maximum value. The sliceShape is the cursor or
  // slice that you will be requiring (with each call to
  // {get,put}Slice). The windowStart and windowLength delimit the range of
  // pixels that will ultimatly be accessed. The AxisPath is described in
  // the documentation for the LatticeStepper class.
  void setCacheSizeFromPath (const IPosition& sliceShape,
				     const IPosition& windowStart,
				     const IPosition& windowLength,
				     const IPosition& axisPath);

  // Clears and frees up the tile cache. The maximum allowed cache size is
  // unchanged from when <src>setMaximumCacheSize</src> was last called.
  void clearCache();

  // Generate a report on how the cache is doing. This is reset every
  // time <src>clearCache</src> is called.
  void showCacheStatistics (ostream& os) const;

  // Return the value of the single element located at the argument
  // IPosition.
  // Note that <src>Lattice::operator()</src> can also be used.
  virtual T getAt (const IPosition& where) const;
  
  // Put the value of a single element.
  virtual void putAt (const T& value, const IPosition& where);

  // A function which checks for internal consistency. Returns False if
  // something nasty has happened to the PagedArray.
  virtual Bool ok() const;

  // This function is used by the LatticeIterator class to generate an
  // iterator of the correct type for a specified Lattice. Not recommended
  // for general use. 
  virtual LatticeIterInterface<T>* makeIter(
                                   const LatticeNavigator& navigator) const;

  // Do the actual getting of an array of values.
  virtual Bool doGetSlice (Array<T>& buffer, const Slicer& section);

  // Do the actual getting of an array of values.
  virtual void doPutSlice (const Array<T>& sourceBuffer,
			   const IPosition& where,
			   const IPosition& stride);
  
  // Get the best cursor shape.
  virtual IPosition doNiceCursorShape (uInt maxPixels) const;

private:
  // Set the data in the TableInfo file
  void setTableType();
  // make the ArrayColumn
  void makeArray (const TiledShape& shape);
  // Make a Table to hold this PagedArray
  void makeTable (const String& filename, Table::TableOption option);
  // The default comment for PagedArray Colums
  static String defaultComment();
  // Get the writable ArrayColumn object. It is created when needed.
  ArrayColumn<T>& getRWArray();
  // Create the writable ArrayColumn object
  // It reopens the table for write when needed.
  void makeRWArray();

  Table  itsTable;
  String itsColumnName;
  uInt   itsRowNumber;
  ArrayColumn<T>       itsRWArray;
  ROArrayColumn<T>     itsROArray;
  ROTiledStManAccessor itsAccessor;
  LogIO  itsLog;
};


template<class T>
inline ArrayColumn<T>& PagedArray<T>::getRWArray()
{
  if (itsRWArray.isNull()) {
    makeRWArray();
  }
  return itsRWArray;
}


#endif
