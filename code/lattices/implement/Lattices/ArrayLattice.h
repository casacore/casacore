//# ArrayLattice: Object which converts an Array to a Lattice.
//# Copyright (C) 1994,1995,1996
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

//# predeclarations
class IPosition;
class LatticeNavigator;
class Slicer;
template <class T> class COWPtr;
template <class T> class RO_LatticeIterInterface;
template <class T> class LatticeIterInterface;

// <summary> 
// Class which converts an Array into a Lattice.
// </summary>
//
// <reviewed reviewer="" date="" tests="tArrayLattice" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> Lattice
//   <li> Array
// </prerequisite>
//
// <etymology>
// The ArrayLattice name reflects its role as the converter from an AIPS++ 
// Array to an AIPS++ Lattice object. 
// </etymology>
//
// <synopsis> 
// Given an instance of an AIPS++ Array (or one its derivatives, Vector, 
// Matrix, or Cube) and a need for an AIPS++ Lattice object, the programmer
// instantiates an ArrayLattice by passing the Array as an argument to the 
// ArrayLattice constructor.  The ArrayLattice acts as a Lattice "Front-End"
// for the data contained in the Array.  Any Lattice operation may then be 
// performed on the ArrayLattice instance and have the results affect the
// Array instance underneath.
// </synopsis> 
//
// <example>
// <srcblock>
// // build an Array
// Cube<Float> myArray(IPosition(3, 32, 64, 128));
// for (int i = 0, i<32, i++)
//   for (int j = 0, j<64, j++)
//     for (int k = 0, i<128, k++) myArray(IPosition(3,i,j,k)) = i + j + k;
// // build our Lattice object
// ArrayLattice<Float> myLattice(myArray);
// // do fancy Lattice things...
// // ...like apply a Functional (here it's a polynomial) to every element.
// Polynomial<Float> poly(3);
// poly.setCoefficient(1, 0.5);
// poly.setCoefficient(2, 0.75);
// poly.setCoefficient(3, 1.0);
// myLattice.apply(poly);
// //...or get a "slice" of data out.
// Vector<Float> theData;
// IPosition start(3,0);
// IPosition shape(3,3,1,1);
// IPosition stride(3,1)
// myLattice.getSlice(theData, start, shape, stride);
// cout << "The vector = " << theData << endl; 
// // ...or use an Iterator to make changes ourselves.
// LatticeIterator iter(myLattice, shape);
// theData = iter.vectorCursor();
// Vector<Float> result;
// for (iter.reset(); !iter.atEnd(); iter++) {
//   result = innerproduct(theData, theData);
//   cout << "The result" << result << endl;
// }
// </srcblock>
// </example>
//
// <motivation>
// We needed a way of creating Lattices but with AIPS++ Array characteristics.
// The thought is that ArrayLattices will eventually be replaced by making
// Arrays inhereted from Lattice.
// </motivation>
//
// <todo asof="1996/04/02">
//   <li> replace all this code by having Array inherit from Lattice.
// </todo>

template<class T> class ArrayLattice : public Lattice<T>
{
public: 
  // default ctor
  ArrayLattice();

  // shape constructor
  ArrayLattice(const IPosition &shape);

  // array constructor (reference semantics)
  ArrayLattice(const Array<T> &array);

  // copy constructor (reference semantics)
  ArrayLattice(const ArrayLattice &other);

  // destructor
  ~ArrayLattice();

  // assignemnt operator (reference semantics)
  ArrayLattice &operator=(const ArrayLattice &other);

  // function to return the shape of the Lattice.
  IPosition shape() const; 
  
  // function which extracts an Array of values from a Lattice - a read-only 
  // operation. 
  // getSlice parameters:
  // <ul>
  // <li> buffer: a COWPtr<Array<T> > or an Array<T>. 
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
  // These functions return 'True' if "buffer" points to a reference and 
  // 'False' if it points to a copy.
  // 
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

  // function which places an Array of values within this instance of the
  // Lattice at the location specified by the IPosition "where", incrementing 
  // by "stride".
  void putSlice(const Array<T> &sourceBuffer, const IPosition &where,
		const IPosition &stride);
  
  // function which sets all of the elements in the Lattice to value.
  void set(const T &value);

  // function which returns an Array of the data within this Lattice.
  // <group>
  Array<T> &asArray();
  const Array<T> &asArray() const;
  // </group>

  // These are the true implementations of the paran operator.
  // <note> Not for public use </note>
  // <group>
  T getAt(const IPosition &where) const;
  void putAt(const T &value, const IPosition &where);
  // </group>
  
  // These are the true implementations of the Lattice Iterators.
  // <note> Not for public use. </note>
  // <group>
  RO_LatticeIterInterface<T> *makeIter(
				    const LatticeNavigator &navigator) const;
  RO_LatticeIterInterface<T> *makeIter(const IPosition &cursorShape) const;
  LatticeIterInterface<T> *makeIter(const LatticeNavigator &navigator);
  LatticeIterInterface<T> *makeIter(const IPosition &cursorShape);
  // </group>

protected:

  // Check class invariants. 
  Bool ok() const;
  
  // the data itself
  Array<T> array_p;

};

#endif
