//# TempLattice.h:
//# Copyright (C) 1997
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
//#
//# $Id$

#if !defined(AIPS_TEMPLATTICE_H)
#define AIPS_TEMPLATTICE_H


//# Includes
#include <aips/aips.h>
#include <aips/Utilities/CountedPtr.h>
#include <trial/Lattices/Lattice.h>

//# Forward Declarations
template <class T> class Array;
template <class T> class COWPtr;
template <class Domain, class Range> class Functional;
template <class T> class LatticeIterInterface;
class IPosition;
class Slicer;
class LatticeNavigator;


// <summary>
// A Lattice that can be used for temporary storage
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="Lattice">Lattice</linkto>
//   <li> <linkto class="ArrayLattice">ArrayLattice</linkto>
//   <li> <linkto class="PagedArray">PagedArray</linkto>
// </prerequisite>

// <etymology>
// A TempLattice disappears from both memory and disk when it goes out of
// scope. Hence it is only useful for temporary storage of data. Unlike Arrays
// however TempLattices can get huge and not place excessive demands on the
// computers paging system. This is because 
// </etymology>

// <synopsis>
// Lattice classes are designed to allow the memory efficient handling of large
// amounts of data. But they can also used with much smaller arrays. With
// large amounts of data the <linkto class="PagedArray">PagedArray</linkto>
// class should be used as this will store the data on disk and effeciently
// accesses specified portions of the data on request. With small amounts of
// data the <linkto class="ArrayLattice">ArrayLattice</linkto> class should be
// used as all the data is always in memory avoiding the I/O associated with
// PagedArrays.
//
// However applications often cannot predict until run time whether they will
// be dealing with a large or small amount of data. So that the use of a
// PagedArray or an ArrayLattice cannot be made until the size of the arrays
// are known. TempLattices make this decision given the size of the Arrays. To
// help in making a good choice the TempLattice class also examines how much
// memory the operating system has and compares it with the size of the
// requested Arrays.
//
// The algorithm currently used is to create an ArrayLattice if the size of the
// array is less than a quarter of the total system memory otherwise a
// PagedArray is created. The PagedArray is currently stored in the current
// working directory and given a unique name that contains the string
// "pagedArray". This pagedArray will be deleted once the TempArray goes out of
// scope. SO unlike PagedArrays which can be made to exist longer than the time
// they are used by a process, the PagedArrays created by the TempArray class
// are always scratch arrays.
// </synopsis>

// <example>
// <srcblock>
// </srcblock>
// </example>

// <motivation>
// I needed a temporaryt Lattice when converting the Convolver class to using
// Lattices. This was to store the Transfre function.
// </motivation>

// <templating arg=T>
// Any type that can be used by the Tables System can also be used by
// this class.
// </templating>

// <todo asof="yyyy/mm/dd">
//   <li> add this feature
//   <li> fix this bug
//   <li> start discussion of this possible extension
// </todo>

template <class T> class TempLattice : public Lattice<T>
{
public:
  // The default constructor creates a TempLattice that is useless for just
  // about everything, except that it can be assigned to with the assignment
  // operator.
  TempLattice();

  // Create a TempLattice of the specified Shape. You can specify how much
  // memory the Lattice can consume before it becomes disk based by giving a
  // positive value to the maxMemoryinMB argument. Otherwise it will assume
  // it can use up to 25% of the memory on your machine (this algorithm may
  // change).
  TempLattice (const IPosition& shape, Int maxMemoryInMB=-1);
  
  // The copy constructor uses reference semantics. ie modifying data in the
  // coipied TempLattice also modifies the data in the original TempLattice.
  // Passing by value doesn't make sense, because it may require the creation
  // of a temporary (but possibly huge) file on disk.
  TempLattice (const TempLattice<T>& other) ;
    
  // The destructor removes the Lattice from memory and if necessary disk.
  virtual ~TempLattice();

  // the assignment operator with reference semantics. As with the copy
  // constructor assigning by value does not make sense.
  TempLattice<T>& operator= (const TempLattice<T>& other);

  // Make a copy of the object (reference semantics).
  virtual Lattice<T>* clone() const;

  // Is the TempLattice writable? It should be.
  virtual Bool isWritable() const;

  // returns the shape of the Lattice including all degenerate axes
  // (ie. axes with a length of one)
  virtual IPosition shape() const;
  
  // returns the number of axes in this Lattice. This includes all
  // degenerate axes
  virtual uInt ndim() const;
  
  // returns the total number of elements in this Lattice.
  virtual uInt nelements() const;
  
  // returns a value of "True" if this instance of Lattice and 'other' have 
  // the same shape, otherwise returns a value of "False".
  virtual Bool conform (const Lattice<T>& other) const;
  
  // Functions which extract an Array of values from a Lattice. All the
  // IPosition arguments must have the same number of axes as the underlying
  // Lattice, otherwise, an exception is thrown. <br>
  // The parameters are:
  // <ul>
  // <li> buffer: a <src>COWPtr<Array<T>></src> or an <src>Array<T></src>. See
  //      the <linkto class="Lattice">Lattice</linkto> class for an example of
  //      how to use a COWPtr.
  // <li> start: The starting position (or Bottom Left Corner), within 
  //      the Lattice, of the data to be extracted.
  // <li> shape: The shape of the data to be extracted.  This is not a
  //      position within the Lattice but the actual shape the buffer will 
  //      have after this function is called.  This argument added
  //      to the "start" argument minus one should be the "Top Right Corner".
  // <li> stride: The increment for each axis.  A stride of
  //      one will return every data element, a stride of two will return
  //      every other element.  The IPosition elements may be different for
  //      each respective axis.  Thus, a stride of IPosition(3,1,2,3) says:
  //      fill the buffer with every element whose position has a first 
  //      index between start(0) and start(0)+shape(0), a second index
  //      which is every other element between start(1) and 
  //      (start(1)+shape(1))*2, and a third index of every third element 
  //      between start(2) and (start(2)+shape(2))*3.
  // <li> section: An alternate way of specifying the start, shape and stride
  // <li> removeDegenerateAxes: a Bool which dictates whether to remove 
  //      "empty" axis created in buffer. (e.g. extracting an n-dimensional 
  //      from an (n+1)-dimensional will fill 'buffer' with an array that 
  //      has a degenerate axis (i.e. one axis will have a length = 1.) 
  //      Setting removeDegenerateAxes = True will return a buffer with 
  //      a shape that doesn't reflect these superfluous axes.)
  // </ul>
  // 
  // These functions return 'True' if "buffer" is a reference to Lattice data
  // and 'False' if it is a copy.
  // <group>   
  virtual Bool getSlice (COWPtr<Array<T> >& buffer, const IPosition& start,
			 const IPosition& shape, const IPosition& stride,
			 Bool removeDegenerateAxes=False) const;
  virtual Bool getSlice (COWPtr<Array<T> >& buffer, const Slicer& section,
			 Bool removeDegenerateAxes=False) const;
  virtual Bool getSlice (Array<T>& buffer, const IPosition& start, 
			 const IPosition& shape, const IPosition& stride, 
			 Bool removeDegenerateAxes=False);
  virtual Bool getSlice (Array<T>& buffer, const Slicer& section, 
			 Bool removeDegenerateAxes=False);
  // </group>   
  
  // A function which places an Array of values within this instance of the
  // Lattice at the location specified by the IPosition "where", incrementing 
  // by "stride".  All of the IPosition arguments must be of the same
  // dimensionality as the Lattice.  The sourceBuffer array may (and probably
  // will) have less axes than the Lattice. The stride defaults to one if
  // not specified. 
  // <group>   
  virtual void putSlice (const Array<T>& sourceBuffer, const IPosition& where,
			 const IPosition& stride);
  virtual void putSlice (const Array<T>& sourceBuffer, const IPosition& where);
  // </group>   
  
  // function which sets all of the elements in the Lattice to a value.
  virtual void set (const T& value);

  // replace every element, x, of the Lattice with the result of f(x).  You
  // must pass in the address of the function -- so the function must be
  // declared and defined in the scope of your program.  All versions of
  // apply require a function that accepts a single argument of type T (the
  // Lattice template type) and return a result of the same type.  The first
  // apply expects a function with an argument passed by value; the second
  // expects the argument to be passed by const reference; the third
  // requires an instance of the class <src>Functional<T,T></src>.  The
  // first form ought to run faster for the built-in types, which may be an
  // issue for large Lattices stored in memory, where disk access is not an
  // issue.
  // <group>
  virtual void apply (T (*function)(T));
  virtual void apply (T (*function)(const T&));
  virtual void apply (const Functional<T,T>& function);
  // </group>

  // This function returns the recommended maximum number of pixels to
  // include in the cursor of an iterator.
  virtual uInt maxPixels() const;

  // Returns a recommended cursor shape for iterating through all the pixels in
  // the Lattice.  Usually, this function will be called with
  // <src>maxPixels()</src> as its argument.
  virtual IPosition niceCursorShape (uInt maxPixels) const;

  // These are the true implementations of the parentheses operator.
  // <group>
  virtual T getAt (const IPosition& where) const;
  virtual void putAt (const T& value, const IPosition& where);
  // </group>
  
  // Check class internals - used for debugging. Should always return True
  virtual Bool ok() const;

  // This function is used by the LatticeIterator class to generate an
  // iterator of the correct type for this Lattice. Not recommended
  // for general use. 
  virtual LatticeIterInterface<T>*
                      makeIter (const LatticeNavigator& navigator) const;

private:
  CountedPtr<Lattice<T> > itsLatticePtr;
};


#endif
