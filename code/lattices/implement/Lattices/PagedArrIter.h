//# PagedArrIter.h: a concrete iterator for use with PagedArray's.
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

#if !defined(AIPS_PAGEDARRITER_H)
#define AIPS_PAGEDARRITER_H

#if defined(_AIX)
#pragma implementation ("PagedArrIter.cc")
#endif 

#include <aips/aips.h>
#include <trial/Lattices/LatticeIterInterface.h>
#include <trial/Lattices/PagedArray.h>
#include <aips/Arrays/Array.h>
#include <aips/Utilities/CountedPtr.h>
#include <trial/Lattices/LatticeNavigator.h>

class IPosition;
template <class T> class Cube;
template <class T> class Matrix;
template <class T> class Vector;

// <summary>A readonly Lattice iterator for PagedArray's</summary>
// <use visibility=export>
//
// <reviewed reviewer="" date="yyyy/mm/dd" tests="tPagedArray.cc,tPagedArrIter.cc" demos="dPagedArray.cc">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class="PagedArray">PagedArray</linkto>
//   <li> <linkto class="Lattice">Lattice</linkto>
//   <li> <linkto class="RO_LatticeIterator">RO_LatticeIterator</linkto>
//   <li> <linkto class="RO_LatticeIterInterface">RO_LatticeIterInterface
//        </linkto>
//   <li> letter/envelope schemes, eg. Coplien, "Advanced C++", ch 5.5
// </prerequisite>
//
// <etymology>
// The RO_PagedArrIter class name is a contraction of Read-Only Paged Array
// Iterator and reflects its role as the methods for iterating through
// Lattices that are resident on disk.
// </etymology>
//
// <synopsis>
// This class can be used by a user if it is known that the user will always
// be iterating through a <linkto class="PagedArray">PagedArray</linkto>
// object. Usually it will be more convienient to use the more general
// <linkto class="RO_LatticeIterator">RO_LatticeIterator</linkto> class to
// iterate through a <src>PagedArray</src> as this class can also iterate
// through any other object derived from the
// <linkto class="Lattice">Lattice</linkto> base class
// (like an <linkto class="ArrayLattice">ArrayLattice</linkto>).
//
// The usage of this class is identical to the 
// <linkto class="RO_LatticeIterator">RO_LatticeIterator</linkto> class, and
// reference should be made to documentation of that class. This
// documentation will only describe details specific to the
// <src>RO_PagedArrIter</src> class.
// </synopsis>
//
// <example>
// In the next block of code, please note that the actual instantiation
// of a PagedArrIter is never seen - LatticeIterator takes care of every
// user's interface needs.
// <srcblock> 
//  // make a Table to hold our PagedArray
//  SetupNewTable setup("STANDARD", TableDesc(), Table::New);
//  Table standard(setup);  
//  // make an array filled with a gradient of values
//  Array<Float> array(IPosition(2,256,128));
//  for(int i=0;i<256;i++) 
//    for(int j=0;j<128;j++) array(IPosition(2,i,j)) = i+j;
//  // construct a new PagedArray, with 'array' as contents, in
//  // the given Table.
//  PagedArray<Float> pa(array, standard);
//  // now let's do some iterating
//  // the cursor is a three vector
//  IPosition cursorShape(1,3);
//  // we don't want to alter anything so use an Read Only Iter.
//  RO_LatticeIterator<Float> constIter(pa, cursorShape);
//  // make an external holder of the cursor contents
//  const Vector<Float> &myCursor = constIter.vectorCursor();
//  // now iterate (note: the call to "reset()" was unnecessary but good code)
//  for(constIter.reset();!constIter.atEnd();constIter++){
//    cout << "The Mean: " << mean(myCursor) << " At " << constIter.position()
//	<< endl;
//  }
// </srcblock>
// </example>
//
// <motivation>
// We are hoping to allow each derivation of Lattice to make as efficient an
// iterator as possible.  The letter/envelope scheme allowed us to hide the 
// juicy bits in classes like the one you see here.
// </motivation>
//
// <templating arg=T>
//    <li> Restricted to the type of the PagedArray argument in the 
//         constructors
// </templating>
//
// <todo asof="1997/01/31">
//   <li> Ensure that the cursor is using reference semantics wherever possible
//   <li> Fixup the code when the cursor is overhanging the Lattice
//   <li> try and be more efficient about when the cursor needs to be updated.
// </todo>

template <class T> class RO_PagedArrIter: public RO_LatticeIterInterface<T> {
public:

  // Construct the Iterator with the supplied data, and iteration strategy
  RO_PagedArrIter(const PagedArray<T> & data, const LatticeNavigator & method);

  // Lattice and cursor shape constructor - LatticeNavigator method is
  // automatically defined to be a LatticeStepper.		  
  RO_PagedArrIter(const PagedArray<T> & data, const IPosition & cursorShape);

  // The copy constructor uses reference sematics for the PagedArray and
  // copy semantics for the cursor and Navigator. This way the newly
  // constructed RO_PagedArrIter can independently iterate through the same
  // data set. (with the same cursor shape etc.)
  RO_PagedArrIter(const RO_PagedArrIter<T> & other);

  // destructor (cleans up dangling references and releases cursor memory)
  virtual ~RO_PagedArrIter();
  
  // The assignment operator uses reference sematics for the PagedArray and
  // copy semantics for the cursor and Navigator. This way the
  // RO_PagedArrIter's share the same data set but independently iterate
  // with cursors of the same size.
  RO_PagedArrIter<T> & operator=(const RO_PagedArrIter<T> & other);

  // Increment operator - increment the cursor to the next position. These
  // functions are forwarded to the current LatticeNavigator and both
  // postfix and prefix versions will usually do the same thing.
  // <group>
  virtual Bool operator++();
  virtual Bool operator++(Int);
  // </group>  
  
  // Decrement operator - decrement the cursor to the previous
  // position. These functions are forwarded to the current LatticeNavigator
  // and both postfix and prefix versions will usually do the same thing.
  // <group>
  virtual Bool operator--();
  virtual Bool operator--(Int);
  // </group>
  
  // Function which resets the cursor to the beginning of the Lattice and
  // resets the number of steps taken to zero. Forwarded to the current
  // LatticeNavigator. 
  virtual void reset();
  
  // Function which returns a value of "True" if the cursor is at the
  // beginning of the Lattice, otherwise, returns "False". Forwarded to the
  // current LatticeNavigator.
  virtual Bool atStart() const;
  
  // Function which returns a value of "True" if and attempt has been made
  // to move the cursor beyond the end of the Lattice. Forwarded to the
  // current LatticeNavigator.
  virtual Bool atEnd() const;
  
  // Function to return the number of steps (increments or decrements) taken
  // since construction (or since last reset).  This is a running count of
  // all cursor movement since doing N increments followed by N decrements
  // does not necessarily put the cursor back at the origin of the Lattice.
  // Forwarded to the current LatticeNavigator.
  virtual uInt nsteps() const;
  
  // Function which returns the current position of the beginning of the
  // cursor within the Lattice. The returned IPosition will have the same
  // number of axes as the underlying Lattice. Forwarded to the current
  // LatticeNavigator.
  virtual IPosition position() const;
  
  // Function which returns the current position of the end of the
  // cursor. The returned IPosition will have the same number of axes as the
  // underlying Lattice. Forwarded to the current LatticeNavigator.
  virtual IPosition endPosition() const;
  
  // Function which returns the shape of the Lattice being iterated through.
  // The returned IPosition will always have the same number of axes as the
  // underlying Lattice. Forwarded to the current LatticeNavigator.
  virtual IPosition latticeShape() const;
  
  // Function which returns the shape of the cursor which is iterating
  // through the Lattice.  The returned IPosition will have the same number
  // of axes as the underlying Lattice. Forwarded to the current
  // LatticeNavigator.
  virtual IPosition cursorShape() const;
  
  // Functions which returns a window to the data in the Lattice. These are
  // used to read the data within the Lattice.  Use the function that is
  // appropriate to the current cursor dimension, AFTER REMOVING DEGENERATE
  // AXES (or use the <src>cursor</src> function which always works with any
  // number of dimensions in the cursor). A call of the function whose
  // return value is inappropriate with respect to the current cursor
  // dimension will throw an exception (AipsError).
  //<group>
  virtual const Vector<T> & vectorCursor() const;
  virtual const Matrix<T> & matrixCursor() const;
  virtual const Cube<T> & cubeCursor() const;
  virtual const Array<T> & cursor() const;
  //</group> 
  
  // Function which checks the internal data of this class for correct
  // dimensionality and consistant values. 
  // Returns True if everything is fine otherwise returns False
  virtual Bool ok() const;
  
private:
  // "read" the contents of the PagedArray into the cursor.
  void cursorUpdate();
  // Allocate the internal cursor to be a pointer to the correct type
  // Returns False if the memory could not be allocated.
  Bool allocateCursor();
  // setup the cache in the tiled storage manager
  void setup_tile_cache();

  // reference to the Lattice
  PagedArray<T> theData;
  // Polymorphic pointer to a subpart of the Lattice
  Array<T> * theCurPtr;
  // pointer to the method of Lattice transversal
  CountedPtr<LatticeNavigator> theNavPtr;
};

// <summary>A read/write Lattice iterator for PagedArray's</summary>
// <use visibility=export>
//
// <reviewed reviewer="" date="yyyy/mm/dd" tests="tPagedArray.cc,tPagedArrIter.cc" demos="dPagedArray.cc">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class="PagedArray">PagedArray</linkto>
//   <li> <linkto class="Lattice">Lattice</linkto>
//   <li> <linkto class="LatticeIterator">LatticeIterator</linkto>
//   <li> <linkto class="LatticeIterInterface">LatticeIterInterface
//        </linkto>
//   <li> letter/envelope schemes, eg. Coplien, "Advanced C++", ch 5.5
// </prerequisite>
//
// <etymology>
// The PagedArrIter class name is a contraction of Paged Array Iterator 
// and reflects its role as the methods for iterating through Lattices which 
// are resident on disk.
// </etymology>
//
// <synopsis>

// This class can be used by a user if it is known that the user will always
// be iterating through an <linkto class="PagedArray">PagedArray</linkto>
// object. Usually it will be more convienient to use the more general
// <linkto class="LatticeIterator">LatticeIterator</linkto> class to iterate
// through a <src>PagedArray</src> as this class can also iterate through
// any other object derived from the
// <linkto class="Lattice">Lattice</linkto> base class
// (like an <linkto class="ArrayLattice">ArrayLattice</linkto>).
//
// The usage of this class is identical to the 
// <linkto class="LatticeIterator">LatticeIterator</linkto> class, and
// reference should be made to documentation of that class. This
// documentation will only describe details specific to the
// <src>PagedArrIter</src> class.
// </synopsis>
//
// <example>
// In the next block of code, please note that the actual instantiation
// of a PagedArrIter is never seen - LatticeIterator takes care of every
// user's interface needs.
// <srcblock> 
// // make a Table to hold our PagedArray
//  Table standard(SetupNewTable("STANDARD", 
//                               TableDesc("", TableDesc::Scratch), 
//                               Table::New));
//  // make an array filled with a gradient of values
//  Array<Float> array(IPosition(2,256,128));
//  for(int i=0;i<256;i++) 
//    for(int j=0;j<128;j++) array(IPosition(2,i,j)) = i+j;
//  // construct a new PagedArray, with 'array' as contents, in
//  // the given Table.
//  PagedArray<Float> pa(array, standard);
//  // now let's do some iterating
//  // the cursor is a three vector
//  IPosition cursorShape(1,3);
//  // we don't want to alter anything so use an Read Only Iter.
//  RO_LatticeIterator<Float> constIter(pa, cursorShape);
//  // make an external holder of the cursor contents
//  const Vector<Float> &myCursor = constIter.vectorCursor();
//  // now iterate (note: the call to "reset()" was unnecessary but good code)
//  for(constIter.reset();!constIter.atEnd();constIter++){
//    cout << "The Mean: " << mean(myCursor) << " At " << constIter.position()
//	<< endl;
//  }
// </srcblock>
// </example>
//
// <motivation>
// We are hoping to allow each derivation of Lattice to make as efficient an
// iterator as possible.  The letter/envelope scheme allowed us to hide the 
// juicy bits in classes like the one you see here.
// </motivation>
//
// <templating arg=T>
//    <li> Restricted to the type of the PagedArray argument in the 
//constructors
// </templating>
//
// <todo asof="yyyy/mm/dd">
//   <li> add this feature
//   <li> fix this bug
//   <li> start discussion of this possible extension
// </todo>

template <class T> class PagedArrIter: public LatticeIterInterface<T> {
public:

  // Construct the Iterator with the supplied data, and iteration strategy
  PagedArrIter(PagedArray<T> & data, const LatticeNavigator & method);

  // Lattice and cursor shape constructor - LatticeNavigator method is
  // automatically defined to be a LatticeStepper.		  
  PagedArrIter(PagedArray<T> & data, const IPosition & cursorShape);

  // The copy constructor uses reference sematics for the PagedArray and
  // copy semantics for the cursor and Navigator. This way the newly
  // constructed RO_PagedArrIter can independently iterate through the same
  // data set. (with the same cursor shape etc.)
  PagedArrIter(const PagedArrIter<T> & other);

  // destructor (cleans up dangling references and releases cursor memory)
  virtual ~PagedArrIter();
  
  // The assignment operator uses reference sematics for the PagedArray and
  // copy semantics for the cursor and Navigator. This way the
  // RO_PagedArrIter's share the same data set but independently iterate
  // with cursors of the same size.
  PagedArrIter<T> & operator=(const PagedArrIter<T> & other);

  // Increment operator - increment the cursor to the next position. These
  // functions are forwarded to the current LatticeNavigator and both
  // postfix and prefix versions will usually do the same thing.
  // <group>
  virtual Bool operator++();
  virtual Bool operator++(Int);
  // </group>  
  
  // Decrement operator - decrement the cursor to the previous
  // position. These functions are forwarded to the current LatticeNavigator
  // and both postfix and prefix versions will usually do the same thing.
  // <group>
  virtual Bool operator--();
  virtual Bool operator--(Int);
  // </group>
  
  // Function which resets the cursor to the beginning of the Lattice and
  // resets the number of steps taken to zero. Forwarded to the current
  // LatticeNavigator. 
  virtual void reset();
  
  // Function which returns a value of "True" if the cursor is at the
  // beginning of the Lattice, otherwise, returns "False". Forwarded to the
  // current LatticeNavigator.
  virtual Bool atStart() const;
  
  // Function which returns a value of "True" if and attempt has been made
  // to move the cursor beyond the end of the Lattice. Forwarded to the
  // current LatticeNavigator.
  virtual Bool atEnd() const;
  
  // Function to return the number of steps (increments or decrements) taken
  // since construction (or since last reset).  This is a running count of
  // all cursor movement since doing N increments followed by N decrements
  // does not necessarily put the cursor back at the origin of the Lattice.
  // Forwarded to the current LatticeNavigator.
  virtual uInt nsteps() const;
  
  // Function which returns the current position of the beginning of the
  // cursor within the Lattice. The returned IPosition will have the same
  // number of axes as the underlying Lattice. Forwarded to the current
  // LatticeNavigator.
  virtual IPosition position() const;
  
  // Function which returns the current position of the end of the
  // cursor. The returned IPosition will have the same number of axes as the
  // underlying Lattice. Forwarded to the current LatticeNavigator.
  virtual IPosition endPosition() const;
  
  // Function which returns the shape of the Lattice being iterated through.
  // The returned IPosition will always have the same number of axes as the
  // underlying Lattice. Forwarded to the current LatticeNavigator.
  virtual IPosition latticeShape() const;
  
  // Function which returns the shape of the cursor which is iterating
  // through the Lattice.  The returned IPosition will have the same number
  // of axes as the underlying Lattice. Forwarded to the current
  // LatticeNavigator.
  virtual IPosition cursorShape() const;

  // Functions which returns a window to the data in the Lattice. These are
  // used to read AND write the data within the Lattice.  Use the function
  // that is appropriate to the current cursor dimension, AFTER REMOVING
  // DEGENERATE AXES, or use the <src>cursor</src> function which works with
  // any number of dimensions in the cursor. A call of the function whose
  // return value is inappropriate with respect to the current cursor
  // dimension will throw an exception (AipsError).
  //<group>
  virtual Vector<T> & vectorCursor();
  virtual Matrix<T> & matrixCursor();
  virtual Cube<T> & cubeCursor();
  virtual Array<T> & cursor();
  //</group>
  
  // Function which checks the internal data of this class for correct
  // dimensionality and consistant values. 
  // Returns True if everything is fine otherwise returns False
  virtual Bool ok() const;

private:
  // "read" the contents of the PagedArray into the cursor.
  void cursorUpdate();
  // "write" the cursor contents into the PagedArray
  void cursorWrite();
  // Allocate the internal cursor to be a pointer to the correct type
  // Returns False if the memory could not be allocated.
  Bool allocateCursor();
  // setup the cache in the tiled storage manager
  void setup_tile_cache();

  // reference to the Lattice
  PagedArray<T> theData;
  // Polymorphic pointer to a subpart of the Lattice
  Array<T> * theCurPtr;
  // pointer to the method of Lattice transversal
  CountedPtr<LatticeNavigator> theNavPtr;
};

#endif
