//# PagedArray.h: templated Lattice, paged from disk to memory on demand
//# Copyright (C) 1994,1995,1996,1997,1998,1999,2000,2001,2002,2003
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

#ifndef LATTICES_PAGEDARRAY_H
#define LATTICES_PAGEDARRAY_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/lattices/Lattices/Lattice.h>
#include <casacore/lattices/Lattices/TiledShape.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/DataMan/TiledStManAccessor.h>
#include <casacore/casa/BasicSL/String.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// A Lattice that is read from or written to disk.
// </summary>

// <use visibility=export>

// <reviewed reviewer="Peter Barnes" date="1999/10/30" tests="tPagedArray.cc" demos="dPagedArray.cc">
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
// <li> To provide for arrays that are too large for the computer's memory.
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
// PagedArray into a Casacore <linkto class=Array>Array</linkto>. Because the
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
// <ANCHOR NAME="PagedArray:Advanced"><h3>More Details</h3></ANCHOR>
// In order to utilise PagedArrays fully and understand many of the member
// functions and data access methods in this class, you need to be familiar
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
// example a PagedArray of shape [1024,1024,4,128] may have a tile shape of
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
// cache size will trade off memory usage for disk I/O. Setting the cache
// size is illustrated in example 5 below.
// <br>
// The showCacheStatistics member function is provided to allow you to
// evaluate the performance of the tile cache.
// </synopsis> 

// <example>
// All the examples in this section are available in dPagedArray.cc
//
// <h4>Example 1:</h4>
// Create a PagedArray of Floats of shape [1024,1024,4,256] in a file
// called "myData_tmp.array" and initialize it to zero. This will create a
// directory on disk called "myData_tmp.array" that contains files that
// exceed 1024*1024*4*256*4 (= 4 GBytes) in size.
// <srcblock>
// const IPosition arrayShape(4,1024,1024,4,256);
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
// indgen(profile);
// profile.apply(g);
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
// I-polarization data occupies 1 GByte of RAM which is too big to read
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
// to not warrant constructing iterators and setting up
// LatticeNavigators. In this example the call to the getSlice function
// is unnecessary but is done for illustration purposes anyway.
// <srcblock>
// SetupNewTable maskSetup("mask_tmp.array", TableDesc(), Table::New);
// Table maskTable(maskSetup);
// PagedArray<Bool> maskArray(IPosition(4,1024,1024,4,256), maskTable);
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
// // 128kBytes of cache memory and takes 125 secs.
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
// // Set the cache size to enough pixels for one row of tiles (ie. 4).
// // This uses 512 kBytes of cache memory and takes 10 secs.
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
// // (ie. 4*8). This uses 4 MBytes of cache memory and takes 2 secs.
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
// stored in a Casacore Table.  This restricts the template argument to all
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
  //# Make members of parent class known.
public:
  using Lattice<T>::ndim;

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
  explicit PagedArray (const TiledShape& shape);

  // Construct a new PagedArray, with the specified shape, in the default
  // row and column of the supplied Table.
  PagedArray (const TiledShape& shape, Table& file);

  // Construct a new PagedArray, with the specified shape, in the specified
  // row and column of the supplied Table.
  PagedArray (const TiledShape& shape, Table& file,
	      const String& columnName, uInt rowNum);

  // Reconstruct from a pre-existing PagedArray in the default row and
  // column of the supplied Table with the supplied filename.
  explicit PagedArray (const String& filename);

  // Reconstruct from a pre-existing PagedArray in the default row and
  // column of the supplied Table.
  explicit PagedArray (Table& file);

  // Reconstruct from a pre-existing PagedArray in the specified row and
  // column of the supplied Table.
  PagedArray (Table& file, const String& columnName, uInt rowNum);

  // The copy constructor which uses reference semantics. Copying by value
  // doesn't make sense, because it would require the creation of a
  // temporary (but possibly huge) file on disk.
  PagedArray (const PagedArray<T>& other);
  
  // The destructor flushes the PagedArrays contents to disk. 
  ~PagedArray();
  
  // The assignment operator with reference semantics. As with the copy
  // constructor assigning by value does not make sense.
  PagedArray<T>& operator= (const PagedArray<T>& other);
  
  // Make a copy of the object (reference semantics).
  virtual Lattice<T>* clone() const;

  // A PagedArray is always persistent.
  virtual Bool isPersistent() const;

  // A PagedArray is always paged to disk.
  virtual Bool isPaged() const;

  // Is the PagedArray writable?
  virtual Bool isWritable() const;

  // Returns the shape of the PagedArray.
  virtual IPosition shape() const;

  // Return the current Table name. By default this includes the full path. 
  // The path preceeding the file name can be stripped off on request.
  virtual String name (Bool stripPath=False) const;

  // Functions to resize the PagedArray. The old contents are lost. Usage of
  // this function is NOT currently recommended (see the <linkto
  // class="PagedArray:Advanced">More Details</linkto> section above).
  void resize (const TiledShape& newShape);

  // Returns the current table name (ie. filename) of this PagedArray.
  const String& tableName() const;

  // Return the current table object.
  // <group>
  Table& table();
  const Table& table() const;
  // </group>

  // Returns the current Table column name of this PagedArray.
  const String& columnName() const;

  // Returns the default TableColumn name for a PagedArray.
  static String defaultColumn();

  // Returns an accessor to the tiled storage manager.
  const ROTiledStManAccessor& accessor() const;

  // Returns the current row number of this PagedArray.
  uInt rowNumber() const;

  // Returns the default row number for a PagedArray.
  static uInt defaultRow();

  // Returns the current tile shape for this PagedArray.
  IPosition tileShape() const;

  // Returns the maximum recommended number of pixels for a cursor. This is
  // the number of pixels in a tile.
  virtual uInt advisedMaxPixels() const;

  // Set the maximum allowed cache size for all Arrays in this column of the
  // Table.  The actual value used may be smaller. A value of zero means
  // that there is no maximum.
  virtual void setMaximumCacheSize (uInt howManyPixels);

  // Return the maximum allowed cache size (in pixels) for all Arrays in
  // this column of the Table. The actual cache size may be smaller. A
  // value of zero means that no maximum is currently defined.
  virtual uInt maximumCacheSize() const;

  // Set the actual cache size for this Array to be big enough for the
  // indicated number of tiles. This cache is not shared with PagedArrays
  // in other rows and is always clipped to be less than the maximum value
  // set using the setMaximumCacheSize member function.
  // Tiles are cached using a first in first out algorithm.
  virtual void setCacheSizeInTiles (uInt howManyTiles);

  // Set the actual cache size for this Array to "fit" the indicated
  // path. This cache is not shared with PagedArrays in other rows and is
  // always less than the maximum value. The sliceShape is the cursor or
  // slice that you will be requiring (with each call to
  // {get,put}Slice). The windowStart and windowLength delimit the range of
  // pixels that will ultimatly be accessed. The AxisPath is described in
  // the documentation for the LatticeStepper class.
  virtual void setCacheSizeFromPath (const IPosition& sliceShape,
				     const IPosition& windowStart,
				     const IPosition& windowLength,
				     const IPosition& axisPath);

  // Clears and frees up the tile cache. The maximum allowed cache size is
  // unchanged from when <src>setMaximumCacheSize</src> was last called.
  virtual void clearCache();

  // Generate a report on how the cache is doing. This is reset every
  // time <src>clearCache</src> is called.
  virtual void showCacheStatistics (ostream& os) const;

  // Return the value of the single element located at the argument
  // IPosition.
  // Note that <src>Lattice::operator()</src> can also be used.
  virtual T getAt (const IPosition& where) const;
  
  // Put the value of a single element.
  virtual void putAt (const T& value, const IPosition& where);

  // A function which checks for internal consistency. Returns False if
  // something nasty has happened to the PagedArray. In that case
  // it also throws an exception.
  virtual Bool ok() const;

  // This function is used by the LatticeIterator class to generate an
  // iterator of the correct type for a specified Lattice. Not recommended
  // for general use. 
  virtual LatticeIterInterface<T>* makeIter (const LatticeNavigator& navigator,
					     Bool useRef) const;

  // Do the actual getting of an array of values.
  virtual Bool doGetSlice (Array<T>& buffer, const Slicer& section);

  // Do the actual getting of an array of values.
  virtual void doPutSlice (const Array<T>& sourceBuffer,
			   const IPosition& where,
			   const IPosition& stride);
  
  // Get the best cursor shape.
  virtual IPosition doNiceCursorShape (uInt maxPixels) const;

  // Handle the (un)locking.
  // <group>
  virtual Bool lock (FileLocker::LockType, uInt nattempts);
  virtual void unlock();
  virtual Bool hasLock (FileLocker::LockType) const;
  // </group>

  // Resynchronize the PagedArray object with the lattice file.
  // This function is only useful if no read-locking is used, ie.
  // if the table lock option is UserNoReadLocking or AutoNoReadLocking.
  // In that cases the table system does not acquire a read-lock, thus
  // does not synchronize itself automatically.
  virtual void resync();

  // Flush the data (but do not unlock).
  virtual void flush();

  // Temporarily close the lattice.
  // It will be reopened automatically on the next access.
  virtual void tempClose();

  // Explicitly reopen the temporarily closed lattice.
  virtual void reopen();

private:
  // Set the data in the TableInfo file
  void setTableType();
  // make the ArrayColumn
  void makeArray (const TiledShape& shape);
  // Make a Table to hold this PagedArray
  void makeTable (const String& filename, Table::TableOption option);
  // The default comment for PagedArray Colums
  static String defaultComment();
  // Get the writable ArrayColumn object.
  // It reopens the table for write if needed.
  ArrayColumn<T>& getRWArray();
  // Do the reopen of the table (if not open already).
  // <group>
  void doReopen() const;
  void tempReopen() const;
  // </group>

  mutable Table     itsTable;
          String    itsColumnName;
          uInt      itsRowNumber;
  mutable Bool      itsIsClosed;
  mutable Bool      itsMarkDelete;
          String    itsTableName;
          Bool      itsWritable;
          TableLock itsLockOpt;
  mutable ArrayColumn<T>       itsArray;
  mutable ROTiledStManAccessor itsAccessor;
};


template<class T>
inline ArrayColumn<T>& PagedArray<T>::getRWArray()
{
  if (itsIsClosed) {
    doReopen();
  }
  if (!itsWritable) {
    itsTable.reopenRW();
    itsWritable = True;
  }
  return itsArray;
}

template<class T>
inline Table& PagedArray<T>::table()
{
  doReopen();
  return itsTable;
}
template<class T>
inline const Table& PagedArray<T>::table() const
{
  doReopen();
  return itsTable;
}

template<class T>
inline const String& PagedArray<T>::columnName() const
{
  return itsColumnName;
}

template<class T>
inline String PagedArray<T>::defaultColumn()
{
  return "PagedArray";
}

template<class T>
inline const ROTiledStManAccessor& PagedArray<T>::accessor() const
{
  return itsAccessor;
}

template<class T>
inline uInt PagedArray<T>::rowNumber() const
{
  return itsRowNumber;
}

template<class T>
inline uInt PagedArray<T>::defaultRow()
{
  return 0;
}

template<class T>
void PagedArray<T>::doReopen() const
{
  if (itsIsClosed) {
    tempReopen();
  }
}



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/lattices/Lattices/PagedArray.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
