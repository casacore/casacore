//# LatticeIterator.h: iterators for Lattices: read-only or read&write
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

#if !defined(AIPS_LATTICEITERATOR_H)
#define AIPS_LATTICEITERATOR_H

#if defined(_AIX)
#pragma implementation ("LatticeIterator.cc")
#endif 

//# Includes
#include <aips/aips.h>
#include <trial/Lattices/Lattice.h>
#include <trial/Lattices/LatticeIterInterface.h>
#include <aips/Utilities/CountedPtr.h>

//# Forward Declarations
class IPosition;
class LatticeNavigator;
template <class T> class Array;
template <class T> class Cube;
template <class T> class Matrix;
template <class T> class Vector;


// <summary>
// A read-only iterator for Lattices
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tPagedArrIter.cc">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="Lattice">Lattice</linkto>
//   <li> <linkto class="LatticeNavigator">LatticeNavigator</linkto>
//   <li> <linkto class="Array">Array</linkto>
// </prerequisite>

// <etymology>
// The leading "RO" is shorthand for "read-only", which indicates that 
// RO_LatticeIterator's are used for traversing Lattices, examining and 
// possibly extracting their contents, but not for modifying them. 
// </etymology>

// <synopsis> 
// This class provides a convenient way to traverse any class derived from
// Lattice. You can iterate through the Lattice's data from "start" to "end"
// by calling <src>operator++</src>, and reverse direction by calling
// <src>operator--</src>.  You can return immediately to the beginning by
// calling the <src>reset</src> function.  The RO_LatticeIterator gives the
// user the opportunity to methodically walk through the data, in an
// efficient way.
//
// Generally iterators should not be long-lived objects - create new ones
// when needed rather than keeping one around for a long time to be
// reused. This is because the cache memory used by the cursor will be
// released when the iterator is destroyed.
//
// The purpose of this class is to hide the possibly complicated
// implementation and structure of the Lattice classes, and allow you to
// iterate through a Lattice with the same ease as one iterates through a
// Fortran or C vector.  For example, and assuming that initialization has
// been done properly, here's a typical 'for' loop:
// <srcblock>
//  // code omitted  which associates Lattice object and the iterator
// for (iterator.reset(); !iterator.atEnd(); iterator++)
//    meanValue = mean(iterator.cursor()); 
// </srcblock>
// The iterator's <src>cursor()</src> member function returns a reference to
// that part of the Lattice data which is presently "seen" by the
// LatticeIterator.
// 
// Before explaining the initialization of an iterator, the LatticeNavigator
// class must be introduced.  This is an abstract base class, from which
// concrete navigators are derived.  After one of these is created, you
// attach it to the LatticeIterator, and it provides a specific technique
// for navigating through the Lattice.  Different navigators deliver
// different traversal schemes.  The most basic is LatticeStepper, which
// moves a specified shape sequentially through the Lattice -- for example,
// by moving one plane at a time, front to back, through a cube.  Another
// navigator might be designed to move a small, 2-dimensional plane through
// a cube, centering each iteration on the brightest pixel of the cube's
// plane, and ignoring the darker regions of the cube.
//
// Many of the LatticeIterator member functions are forwarded directly
// to its LatticeNavigator, for example: increment and decrement
// operators, atStart and atEnd.
// 
// Here's a typical declaration:
// <srcblock>
// RO_LatticeIterator<Float> iterator(pagedArray, stepper);
// </srcblock>
// 
// The template identifier <src>Float</src> defines the data type of 
// Array object that will be the iterator's cursor. 
//
// The <src>pagedArray</src> constructor argument names a PagedArray object,
// which is what the iterator will traverse.  The <src>stepper</src>
// argument is a LatticeStepper which defines the method of iteration.

// <example>
// When passed the name of a previously-created PagedArray stored on disk,
// this function will traverse the whole array, and report the average value
// of all of the elements.  Imagine that the filename contains a PagedArray
// with dimension 64 x 64 x 8 (though this function only requires that
// the PagedArray have dimension 2 or higher).
// <srcblock>
// void demonstrateIterator (const String &filename)
// {
//   PagedArray<Float> pagedArray(filename);
//   IPosition latticeShape = pagedArray.shape();
//   cout << "paged array has shape: " << latticeShape << endl;
//   // insist upon at least 2 axes, allowing iteration by a matrix
// 
//   uInt lengthOfAxis0 = latticeShape(0);
//   uInt lengthOfAxis1 = latticeShape(1);
//   IPosition cursorShape(2, lengthOfAxis0, lengthOfAxis1);
// 
//   // construct a stepper, which needs to know the shape of the PagedArray
//   // and the shape of the iterator's cursor.  It's no coincidence that
//   // the cursorShape is congruent with the PagedArray shape  -- this 
//   // ensures that the stepper sees all of the PagedArray as it steps 
//   // through it.
//   LatticeStepper stepper(latticeShape, cursorShape);
//  
//   // construct the iterator.  since we only want to read the PagedArray,
//   // use the read-only class, which disallows writing back to the cursor.
//   RO_LatticeIterator<Float> iterator(pagedArray, stepper);
// 
//   Float runningSum = 0.0;
//   for (iterator.reset(); !iterator.atEnd(); iterator++) {
//     for (uInt column = 0; column < cursorShape(0); column++)
//       for (uInt row = 0; row < cursorShape(1); row++) 
//         runningSum += iterator.matrixCursor()(row, column);
//   } // for iterator
//   cout << "average value, from demostrateIterator: " 
//       << runningSum / latticeShape.product() << endl;
// }
// </srcblock>
// </example>

// <motivation>
// Iterator classes are a standard feature in C++ libraries -- they
// provide convenience and allow the implementation of the "iteratee"
// to be kept hidden
// </motivation>

// <todo asof="1995/09/12">
//  <li> IPositions are returned by value.  This a reflection of the 
//       LatticeIterInterface base class' inability to predict the
//       availibility of data members for references.
// </todo>


template <class T> class RO_LatticeIterator
{
public:

  // Construct the Iterator with the supplied data, and iteration strategy
  RO_LatticeIterator (const Lattice<T>& data, const LatticeNavigator& method);

  // Iterate through the data with a LatticeStepper that has uses the
  // supplied cursorShape.
  RO_LatticeIterator (const Lattice<T>& data, const IPosition& cursorShape);  

  // The copy constructor uses reference semantics (ie. NO real copy is made).
  // The function <src>copy</src> can be used to make a true copy..
  RO_LatticeIterator (const RO_LatticeIterator<T>& other);
 
  // destructor (cleans up dangling references and releases memory)
  ~RO_LatticeIterator();

  // Assignment uses reference semantics (ie. NO real copy is made).
  // The function <src>copy</src> can be used to make a true copy.
  RO_LatticeIterator<T> &operator= (const RO_LatticeIterator<T>& other);

  // Make a copy of the iterator object.
  // This means that an independent navigator object is created to
  // be able to iterate independently through the same Lattice.
  // The position in the copied navigator is the same as the original.
  // The reset function has to be used to start at the beginning.
  // <br>Note that if the Lattice uses a cache (e.g. PagedArray), the
  // cache is shared by the iterators.
  RO_LatticeIterator copy() const;
    
  // Increment operator - increment the cursor to the next position.  These
  // functions are forwarded to the current LatticeNavigator and both
  // postfix and prefix versions will usually do the same thing.
  // <group>
  Bool operator++();
  Bool operator++(Int);
  // </group>

  // Decrement operator - decrement the cursor to the previous
  // position. These functions are forwarded to the current LatticeNavigator
  // and both postfix and prefix versions will usually do the same thing.
  // <group>
  Bool operator--();
  Bool operator--(Int);
  // </group>
  
  // Function which resets the cursor to the beginning of the Lattice and
  // resets the number of steps taken to zero. Forwarded to the current
  // LatticeNavigator.
  void reset();

  // Function which returns a value of "True" if the cursor is at the
  // beginning of the Lattice, otherwise, returns "False". Forwarded to the
  // current LatticeNavigator.
  Bool atStart() const;

  // Function which returns a value of "True" if and attempt has been made
  // to move the cursor beyond the end of the Lattice. Forwarded to the
  // current LatticeNavigator.
  Bool atEnd() const;
  
  // Function to return the number of steps (increments or decrements) taken
  // since construction (or since last reset).  This is a running count of
  // all cursor movement since doing N increments followed by N decrements
  // does not necessarily put the cursor back at the origin of the Lattice.
  // Forwarded to the current LatticeNavigator.
  uInt nsteps() const;
  
  // Function which returns the current position of the beginning of the
  // cursor within the Lattice. The returned IPosition will have the same
  // number of axes as the underlying Lattice. Forwarded to the current
  // LatticeNavigator.
  IPosition position() const;

  // Function which returns the current position of the end of the
  // cursor. The returned IPosition will have the same number of axes as the
  // underlying Lattice. Forwarded to the current LatticeNavigator.
  IPosition endPosition() const;

  // Function which returns the shape of the Lattice being iterated through.
  // The returned IPosition will always have the same number of axes as the
  // underlying Lattice. Forwarded to the current LatticeNavigator.
  IPosition latticeShape() const;

  // Function which returns the shape of the cursor which is iterating
  // through the Lattice.  The returned IPosition will have the same number
  // of axes as the underlying Lattice. Forwarded to the current
  // LatticeNavigator.
  IPosition cursorShape() const;

  // Functions which returns a window to the data in the Lattice. These are
  // used to read the data within the Lattice.  Use the function that is
  // appropriate to the current cursor dimension, AFTER REMOVING DEGENERATE
  // AXES, or use the <src>cursor</src> function which works with any number
  // of dimensions in the cursor. A call of the function whose return value
  // is inappropriate with respect to the current cursor dimension will
  // throw an exception (AipsError).
  // <group>
  const Vector<T>& vectorCursor() const;
  const Matrix<T>& matrixCursor() const; 
  const Cube<T>& cubeCursor() const; 
  const Array<T>& cursor() const; 
  // </group>
  
  // Function which checks the internals of the class for consistency.
  // Returns True if everything is fine otherwise returns False.
  Bool ok() const;

protected:
  // The pointer to the Iterator
  CountedPtr<LatticeIterInterface<T> > itsIterPtr;

private:
  // The default constructor is disallowed by making it private.
  RO_LatticeIterator();
};



// <summary>
// A read-write lattice iterator
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tPagedArrIter.cc">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="RO_LatticeIterator">RO_LatticeIterator</linkto>
//   <li> <linkto class="Lattice">Lattice</linkto>
//   <li> <linkto class="LatticeNavigator">LatticeNavigator</linkto>
//   <li> <linkto class="Array">Array</linkto>
// </prerequisite>

// <synopsis>
// LatticeIterator differs from the RO_LatticeIterator class in that 
// the window into the Lattice data which moves with each iterative step may
// be used to alter the Lattice data itself.  The moving "cursor" gives the
// user the door to reach in and change the basic Lattice before moving to
// another section of the Lattice.
// </synopsis>

// <example>
// Here's an iterator that runs through a cube, assigning every element
// of each plane of the cube a value equal to the number of the plane.
// <srcblock>
//      PagedArray<Float> pa("someName");
//      IPosition windowShape(2,pa.shape(0), pa.shape(1));
//      LatticeStepper stepper(pa.shape(), windowShape);
//      LatticeIterator<Float> iterator(pa, stepper);
//      Int planeNumber = 0;
//      for (iterator.reset(); !iterator.atEnd(); iterator++) {
//        iterator.cursor() = planeNumber;
//        iterator.writeCursor();
//      }
// </srcblock>
// <example>

template <class T>
class LatticeIterator : public RO_LatticeIterator<T>
{
public:
  
  // Construct the Iterator with the supplied data, and iteration strategy
  LatticeIterator (Lattice<T>& data, const LatticeNavigator& method);
  
  // Iterate through the data with a LatticeStepper that has uses the
  // supplied cursorShape.
  LatticeIterator (Lattice<T>& data, const IPosition& cursorShape);
  
  // The copy constructor uses reference semantics (ie. NO real copy is made).
  // The function <src>copy</src> can be used to make a true copy..
  LatticeIterator (const LatticeIterator<T>& other);
  
  // destructor (cleans up dangling references and releases memory)
  ~LatticeIterator();
  
  // Assignment uses reference semantics (ie. NO real copy is made).
  // The function <src>copy</src> can be used to make a true copy.
  LatticeIterator<T>& operator= (const LatticeIterator<T>& other);  
  
  // Make a copy of the iterator object.
  // This means that an independent navigator object is created to
  // be able to iterate independently through the same Lattice.
  // The position in the copied navigator is the same as the original.
  // The reset function has to be used to start at the beginning.
  // <br>Note that if the Lattice uses a cache (e.g. PagedArray), the
  // cache is shared by the iterators.
  LatticeIterator<T> copy() const;
    
  // Functions which returns a window to the data in the Lattice. These are
  // used to read AND write the data within the Lattice.  Use the function
  // that is appropriate to the current cursor dimension, AFTER REMOVING
  // DEGENERATE AXES, or use the <src>cursor</src> function which works with
  // any number of dimensions in the cursor. A call of the function whose
  // return value is inappropriate with respect to the current cursor
  // dimension will throw an exception (AipsError).
  // <group>
  Vector<T>& vectorCursor();
  Matrix<T>& matrixCursor();
  Cube<T>& cubeCursor();
  Array<T>& cursor();
  //</group>
  
  // Write the data in the cursor.
  void writeCursor();

  // Write the given data array at the current cursor position.
  // The array shape has to match the cursor shape (as given by cursorShape()).
  void writeArray (const Array<T>& data);

  // Function which checks the internals of the class for consistency.
  // Returns True if everything is fine otherwise returns False.
  Bool ok() const;

private:
  // The default constructor is disallowed by making it private.
  LatticeIterator();

};



#endif
