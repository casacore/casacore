//# ArrayLattice: Object which converts an Array to a Lattice.
//# Copyright (C) 1994,1995,1996,1997,1998,1999,2000,2003
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

#ifndef LATTICES_ARRAYLATTICE_H
#define LATTICES_ARRAYLATTICE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/lattices/Lattices/Lattice.h>
#include <casacore/casa/Arrays/Array.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// A memory resident Lattice
// </summary>

// <use visibility=export>

// <reviewed reviewer="Peter Barnes" date="1999/10/30" tests="tArrayLattice" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Lattice>Lattice</linkto>
//   <li> <linkto class=Array>Array</linkto>
// </prerequisite>

// <etymology>
// The ArrayLattice name reflects its role as a Lattice interface to an Array
// object.
// </etymology>

// <synopsis> 
// An ArrayLattice is a concrete Lattice class where the data is stored in
// memory as opposed to the <linkto class=PagedArray>PagedArray</linkto> class
// where the data is stored on disk. As a result this class is much more
// suitable to problems which require small Lattices that can fit into the
// memory of a computer. 
//
// ArrayLattice imposes another layer of function calls on top of a an
// Array. As a result they should not be used for generic Array
// manipulation. They are useful if you have an Array that needs to use
// Lattice functions or needs to be used with PagedArrays or other Lattice
// derivatives (like <linkto class=LatticeExpr>LatticeExpr</linkto> or
// <linkto class=SubLattice>SubLattice</linkto>).
// For example the LatticeIterator class can iterate through an Array in
// more ways than any of the ArrayIterator classes can. The examples below
// illustrate some uses for ArrayLattices. 
// </synopsis> 

// <example>
// All the examples in this section are available in
// <src>dArrayLattice.cc</src>
//
// <h4>Example 1:</h4>
// In this example an Array of data is converted into an ArrayLattice so that
// the copyData function can be used to write the data to a PagedArray which
// will be stored on disk.
// <srcblock>
// // make an Array and fill it with data.
// Array<Float> myArray(IPosition(3, 64, 64, 2));
// indgen(myArray); // fills the Array with 0,1,2,....,64*64*2-1
// // construct the ArrayLattice
// ArrayLattice<Float> myLattice(myArray);
// // make a PagedArray to store the data on disk
// PagedArray<Float> myPagedArray(myLattice.shape(), "myTestData.array");
// // now copy the data onto disk
// myPagedArray.copyData (myLattice);
// </srcblock>
// Note that it could be done in a somewhat simpler way as:
// <srcblock>
// // make an Array and fill it with data.
// Array<Float> myArray(IPosition(3, 64, 64, 2));
// indgen(myArray); // fills the Array with 0,1,2,....,64*64*2-1
// // make a PagedArray to store the data on disk
// PagedArray<Float> myPagedArray(myLattice.shape(), "myTestData.array");
// // now put the data onto disk
// myPagedArray.put (myArray);
// </srcblock>
//
// <h4>Example 2:</h4>
// The <linkto class=ArrayIterator>ArrayIterator</linkto> class (or its
// derivatives the <linkto class=VectorIterator>VectorIterator</linkto> and the
// <linkto class=MatrixIterator>MatrixIterator</linkto> classes) do not allow
// the user to specify a cursor shape. In this example a Cube class will be
// converted into an ArrayLattice so that an ArrLatticeIter can be used to
// access the data spectrum by spectrum (assuming the z-axis is frequency).
//
// <srcblock>
// Cube<Float> arr(64,64,128);
// // assume that the data gets put into the cube somehow
// // now construct an ArrayLattice from this cube.
// ArrayLattice<Float> lat(arr);
// // Construct an iterator that returns the 128-element spectra one at a time
// ArrLatticeIter<Float> iter(lat, IPosition(3,1,1,128));
// // construct a Matrix to hold the results
// Matrix<Float> channelSum(64,64);
// // and do the summation one spectrum at a time
// for (iter.reset(); !iter.atEnd(); iter++)
//    channelSum(iter.position().getFirst(2)) = sum(iter.cursor());
// </srcblock>
//
//  There are more examples in the <linkto class=Lattice>Lattice</linkto> class
//  and many of the examples in the 
// <linkto class=PagedArray>PagedArray</linkto> class will also be instructive.
// </example>

// <motivation>
// We needed a way of creating Lattices but with Casacore Array characteristics.
// </motivation>

//# <todo asof="1997/05/31">
//# </todo>

// <linkfrom anchor="ArrayLattice" classes="Lattice PagedArray">
//  <here>ArrayLattice</here> - a memory based Lattice.
// </linkfrom>


template <class T> class ArrayLattice : public Lattice<T>
{
  //# Make members of parent class known.
public:
  using Lattice<T>::ndim;

public: 
  // The default constructor creates a ArrayLattice that is useless for just
  // about everything, except that it can be assigned to with the assignment
  // operator.
  ArrayLattice();

  // Construct an ArrayLattice with the specified shape.
  // It results in a writable lattice.
  explicit ArrayLattice (const IPosition& shape);

  // Construct an ArrayLattice that references the given Array.
  // By default it results in a writable lattice.
  ArrayLattice (Array<T>& array, Bool isWritable = True);

  // Construct an ArrayLattice that references the given Array.
  // It results in a non-writable lattice.
  ArrayLattice (const Array<T>& array);

  // The copy constructor uses reference semantics.
  ArrayLattice (const ArrayLattice<T>& other);

  virtual ~ArrayLattice();

  // The assignment operator uses copy semantics.
  ArrayLattice<T>& operator= (const ArrayLattice<T>& other);

  // Make a copy of the object (reference semantics).
  virtual Lattice<T>* clone() const;

  // The lattice data can be referenced as an array section.
  virtual Bool canReferenceArray() const;

  // Is the lattice writable?
  virtual Bool isWritable() const;

  // returns the shape of the ArrayLattice.
  virtual IPosition shape() const; 
  
  // Set all of the elements in the Lattice to a value.
  virtual void set (const T& value);

  // Return the Array of the data within this Lattice.
  // <group>
  Array<T>& asArray();
  const Array<T>& asArray() const;
  // </group>

  // Return the value of the single element located at the argument
  // IPosition.  
  // Note that operator() (defined in the base class) can also be used.
  virtual T getAt (const IPosition& where) const;
  
  // Put the value of a single element.
  virtual void putAt (const T& value, const IPosition& where);

  // Check for internal consistency. Returns False if
  // something nasty has happened to the ArrayLattice.
  virtual Bool ok() const;
  
  // Returns the maximum recommended number of pixels for a cursor.
  // For this class this is equal to the number of pixels in the lattice.
  virtual uInt advisedMaxPixels() const;

  // Get a slice in an optimized way (specifically for ArrLatticeIter).
  // It returns in <src>buffer</src> a reference to the lattice array.
  void getIterSlice (Array<T>& buffer, const IPosition& start,
		     const IPosition& end, const IPosition& incr);

protected:
  // Do the actual getting of an array of values.
  virtual Bool doGetSlice (Array<T>& buffer, const Slicer& section);

  // Do the actual putting of an array of values.
  virtual void doPutSlice (const Array<T>& sourceBuffer,
			   const IPosition& where,
			   const IPosition& stride);
  
private:
  Array<T> itsData;
  Bool     itsWritable;
};



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/lattices/Lattices/ArrayLattice.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
