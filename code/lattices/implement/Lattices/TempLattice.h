//# TempLattice.h: A Lattice that can be used for temporary storage
//# Copyright (C) 1997,1998
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
#include <trial/Lattices/Lattice.h>
#include <trial/Lattices/TiledShape.h>
#include <aips/Utilities/CountedPtr.h>


// <summary>
// A Lattice that can be used for temporary storage
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tTempLattice.cc" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="Lattice">Lattice</linkto>
//   <li> <linkto class="ArrayLattice">ArrayLattice</linkto>
//   <li> <linkto class="PagedArray">PagedArray</linkto>
// </prerequisite>

// <etymology>
// A TempLattice disappears from both memory and disk when it goes out of
// scope. Hence it is only useful for temporary storage of data.
// </etymology>

// <synopsis>
// Lattice classes are designed to allow the memory efficient handling of large
// amounts of data. But they can also used with much smaller arrays. With
// large amounts of data the <linkto class="PagedArray">PagedArray</linkto>
// class should be used as this will store the data on disk and efficiently
// accesses specified portions of the data on request. With small amounts of
// data the <linkto class="ArrayLattice">ArrayLattice</linkto> class should be
// used as all the data is always in memory avoiding the I/O associated with
// PagedArrays.
// <p>
// Applications often cannot predict until run time whether they will
// be dealing with a large or small amount of data. So that the use of a
// PagedArray or an ArrayLattice cannot be made until the size of the arrays
// are known. TempLattice makes this decision given the size of the Array. To
// help in making a good choice the TempLattice class also examines how much
// memory the operating system has (using an aipsrc variable) and compares
// it with the size of the requested Array.
// <p>
// The algorithm currently used is to create an ArrayLattice if the size of the
// array is less than a quarter of the total system memory otherwise a
// PagedArray is created. The PagedArray is currently stored in the current
// working directory and given a unique name that contains the string
// "pagedArray". This pagedArray will be deleted once the TempArray goes out of
// scope. So unlike PagedArrays which can be made to exist longer than the time
// they are used by a process, the PagedArrays created by the TempArray class
// are always scratch arrays.
// </synopsis>

// <example>
// <srcblock>
// </srcblock>
// </example>

// <motivation>
// I needed a temporary Lattice when converting the Convolver class to using
// Lattices. This was to store the Transfer function.
// </motivation>

// <templating arg=T>
// Any type that can be used by the Lattices can also be used by
// this class.
// </templating>

//# <todo asof="yyyy/mm/dd">
//#   <li> add this feature
//#   <li> fix this bug
//#   <li> start discussion of this possible extension
//# </todo>


template<class T> class TempLattice : public Lattice<T>
{
public:
  // The default constructor creates a TempLattice containing a
  // default ArrayLattice object.
  TempLattice();

  // Create a TempLattice of the specified Shape. You can specify how much
  // memory the Lattice can consume before it becomes disk based by giving a
  // positive value to the maxMemoryinMB argument. Otherwise it will assume
  // it can use up to 25% of the memory on your machine as defined in aipsrc
  // (this algorithm may change).
  TempLattice (const TiledShape& shape, Int maxMemoryInMB=-1);
  
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

  // Is the TempLattice paged to disk?
  virtual Bool isPaged() const;

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

  // Get or put a single element in the lattice.
  // Note that Lattice::operator() can also be used to get a single element.
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

  // Do the actual getting of an array of values.
  virtual Bool doGetSlice (Array<T>& buffer, const Slicer& section);

  // Do the actual getting of an array of values.
  virtual void doPutSlice (const Array<T>& sourceBuffer,
			   const IPosition& where,
			   const IPosition& stride);
  
  // Get the best cursor shape.
  virtual IPosition doNiceCursorShape (uInt maxPixels) const;

private:
  CountedPtr<Lattice<T> > itsLatticePtr;
};


#endif
