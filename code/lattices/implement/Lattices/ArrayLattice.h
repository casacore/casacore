//# ArrayLattice: Object which converts an Array to a Lattice.
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
//#
//# $Id$

#if !defined(AIPS_ARRAYLATTICE_H)
#define AIPS_ARRAYLATTICE_H

#include <aips/aips.h>
#include <trial/Lattices/Lattice.h>
#include <aips/Arrays/Array.h>

class IPosition;
class LatticeNavigator;
class Slicer;
template <class T> class COWPtr;
template <class T> class RO_LatticeIterInterface;
template <class T> class LatticeIterInterface;

// <summary>
// A memory resident Lattice
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tArrayLattice" demos="">
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
// An ArrayLattice is a concrete Lattice class were the data is stored in
// memory as opposed to the <linkto class=PagedArray>PagedArray</linkto> class
// where the data is stored on disk. As a result this class is much more
// suitable to problems which require small Lattices that can fit into the
// memory of a computer. 
//
// ArrayLattices impose another layer of function calls on top of a an
// Array. As a result they should not be used for generic Array
// manipulation. They are useful if you have an Array that needs to use
// Lattice functions or exchange data with PagedArrays or other Lattice
// derivatives (if they ever get written). For example the LatticeIterator
// class can iterate through an Array in more ways than any of the
// ArrayIterator classes can. The examples below illustrate some uses for
// ArrayLattices. 
// </synopsis> 

// <example>
// All the examples in this section are available in
// <src>dArrayLattice.cc</src>
//
// <h4>Example 1:</h4>
// In this example an Array of data is converted into an ArrayLattice so that
// the CopyLattice function can be used to write the data to a PagedArray which
// will be stored on disk. This may be conceptually simpler than having to use
// the putSlice member function in the PagedArray class and is only slightly
// less efficient.
// <srcblock>
// // make an Array and fill it with data.
// Array<Float> myArray(IPosition(3, 64, 64, 2));
// indgen(myArray); // fills the Array with 0,1,2,....,64*64*2-1
// // construct the ArrayLattice
// ArrayLattice<Float> myLattice(myArray);
// // make a PagedArray to store the data on disk
// PagedArray<Float> myPagedArray(myLattice.shape(), "myTestData.array");
// // now copy the data onto disk
// CopyLattice(myPagedArray, myLattice);
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
// We needed a way of creating Lattices but with AIPS++ Array characteristics.
// </motivation>

//# <todo asof="1997/05/31">
//# </todo>

// <linkfrom anchor="ArrayLattice" classes="Lattice PagedArray">
//  <here>ArrayLattice</here> - a memory based Lattice.
// </linkfrom>


template<class T>
class ArrayLattice : public Lattice<T>
{
public: 
  // The default constructor creates a ArrayLattice that is useless for just
  // about everything, except that it can be assigned to with the assignment
  // operator.
  ArrayLattice();

  // Construct an ArrayLattice with the specified shape.
  // This results in a writable lattice.
  ArrayLattice (const IPosition& shape);

  // Construct an ArrayLattice that references the given Array.
  // This results in a writable lattice.
  ArrayLattice (Array<T>& array);

  // Construct an ArrayLattice that references the given Array.
  // This results in a non-writable lattice.
  ArrayLattice (const Array<T>& array);

  // the copy constructor which uses reference semantics.
  ArrayLattice (const ArrayLattice& other);

  // the destructor does very little
  ~ArrayLattice();

  // the assignment operator which uses copy semantics.
  ArrayLattice& operator= (const ArrayLattice& other);

  // Make a copy of the object (reference semantics).
  virtual Lattice<T>* clone() const;

  // Is the lattice writable?
  virtual Bool isWritable() const;

  // returns the shape of the ArrayLattice.
  virtual IPosition shape() const; 
  
  // Functions which extract an Array of values from the ArrayLattice. The
  // arguments to these functions are:
  // <ul>
  // <li> buffer: an <src>Array<T></src> or a
  // <src>COWPtr<Array<T>></src>. The buffer shape must be big enough to
  // exactly contain the specified slice. Otherwise an exception will be
  // thrown. Alternatively the buffer can be empty
  // (buffer.nelements()==0). Then the buffer will be resized to fit the
  // requested slice. Unlike the other arguments the buffer can have fewer
  // axes than the underlying PagedArray, but then you must set the
  // removeDegenerateAxes flag to True.
  // <li> start: The starting position (or Bottom Left Corner), within the
  // PagedArray, of the slice to be extracted. Must have the same number of
  // axes as the PagedArray otherwise an exception will be thrown.
  // <li> shape: The shape of the slice to be extracted. This is not a
  // position within the PagedArray but the actual shape the buffer will
  // have after this function is called althoug it always includes all the
  // degenerate axes even if the buffer does not. This argument added to the
  // "start" argument will be the "Top Right Corner", assuming the stride is
  // one on all axes.
  // <li> stride: The increment for each axis. A stride of one will return
  // every data element, a stride of two will return every other
  // element. The IPosition elements may be different for each respective
  // axis. Thus, a stride of IPosition(2,1,2) says: fill the buffer with
  // every element whose position has a first index between start(0) and
  // start(0)+shape(0) and a second index which is every other element
  // between start(1) and (start(1)+shape(1)*2). Must have the same number
  // of axes as the PagedArray otherwise an exception will be thrown.
  // <li> section: The preferred way of specifying the start, shape and
  // stride.
  // <li> removeDegenerateAxes: a Bool which indicates whether to remove axes
  // of length one in the extracted slice. You must set this to True if the
  // supplied buffer has fewer axes than the specified slice, otherwise an
  // exception is thrown. If the supplied buffer is empty all degenerate
  // axes are stripped from the returned buffer.
  // </ul>
  // These functions return False if the returned buffer is a copy of the
  // actual data and True if the returned buffer is a reference to the data in
  // the ArrayLattice. You do not need to call putSlice to put the buffer back
  // into the ArrayLattice if the buffer is a reference to the data. Currently
  // the const versions of getSlice return False and the non-const versions
  // (ie. the ones with <src>Array<T></src> and not <src>COWPtr<Array<T>></src>
  // arguments) return True.
  // <group>  
  virtual Bool getSlice (COWPtr<Array<T> >& bufPtr, const IPosition& start, 
			 const IPosition& shape, const IPosition& stride,
			 Bool removeDegenerateAxes=False) const;
  virtual Bool getSlice (COWPtr<Array<T> >& bufPtr, const Slicer& section, 
			 Bool removeDegenerateAxes=False) const;
  virtual Bool getSlice (Array<T>& buffer, const IPosition& start, 
			 const IPosition& shape, const IPosition& stride,
			 Bool removeDegenerateAxes=False);
  virtual Bool getSlice (Array<T>& buffer, const Slicer& section, 
			 Bool removeDegenerateAxes=False);
  // </group>

  // A function which copies an Array of values into the ArrayLattice at the
  // location specified by the IPosition "where", incrementing by
  // "stride". All of the IPosition arguments must have the same number of
  // dimensions as the ArrayLattice. The sourceBuffer array may have fewer
  // axes than the ArrayLattice. The stride defaults to one if not specified.
  // <group>
  virtual void putSlice (const Array <T>& sourceBuffer, 
			 const IPosition& where, const IPosition& stride);
  virtual void putSlice (const Array <T>& sourceBuffer, 
			 const IPosition& where);
  // </group>
  
  // functions which sets all of the elements in the Lattice to a value.
  virtual void set (const T& value);

  // functions which returns an Array of the data within this Lattice.
  // <group>
  Array<T>& asArray();
  const Array<T>& asArray() const;
  // </group>

  // These are the true implementations of the parenthesis operator. It will
  // probably be more convienient to use the actual parenthesis operator
  // defined in the Lattice base class.
  // <group>
  virtual T getAt (const IPosition& where) const;
  virtual void putAt (const T& value, const IPosition& where);
  // </group>
  
  // a function which checks for internal consistency. Returns False if
  // something nasty has happened to the ArrayLattice.
  virtual Bool ok() const;
  
  // This function is used by the LatticeIterator class to generate an
  // iterator of the correct type for a specified Lattice. Not recommended
  // for general use. 
  virtual LatticeIterInterface<T>* makeIter(
				   const LatticeNavigator& navigator) const;

  // Get a slice in an optimized way (specifically for ArrLatticeIter).
  // It returns in <src>buffer</src> a reference to the lattice array.
  void getIterSlice (Array<T>& buffer, const IPosition& start,
		     const IPosition& end, const IPosition& incr);

private:
  Array<T> itsData;
  Bool     itsWritable;
};


#endif
