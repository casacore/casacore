//# LatticeIterInterface.h: A base class for concrete Lattice iterators
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

#if !defined(AIPS_LATTICEITERINTERFACE_H)
#define AIPS_LATTICEITERINTERFACE_H

#if defined(_AIX)
#pragma implementation ("LatticeIterInterface.cc")
#endif 

//# Includes
#include <aips/aips.h>
#include <trial/Lattices/LatticeNavigator.h>
#include <aips/Lattices/IPosition.h>

//# Forward Declarations
template <class T> class Array;
template <class T> class Vector;
template <class T> class Matrix;
template <class T> class Cube;
template <class T> class Lattice;


// <summary>
// An abstract base class for Lattice iterators
// </summary>

// <use visibility=local> 

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> letter/envelope schemes - see Coplien, "Advanced C++", ch 5.5
//   <li> <linkto class="Lattice">Lattice</linkto>
//   <li> <linkto class="LatticeIterator">LatticeIterator</linkto>
// </prerequisite>

// <etymology>
// The LatticeIterInterface class name reflects its role as the abstract
// base class for concrete read-write LatticeIterators
// </etymology>

// <synopsis>
// This class is only for authors of Lattice letters for the LatticeIterator
// envelope.  General users should see LatticeIterator.
// 
// The LatticeIterInterface class defines an abstract base for the standard
// methods of iteration required by Lattices.  Declaring an Iterator that is
// derived from this class forces it to meet the virtual requirements.
//
// The author of a Lattice derived class should consider the following:
// <ul>
// <li> The LatticeStepper class has strong effects on how the cursor is 
// filled.  A non-integral shape of the cursor may allow a step of 
// iteration to only be partially "touching" the Lattice.  We have dubbed
// this "hangover."
// <li> If the cursor has "hangover" it should be filled with a value that 
// indicates the cursor is in undefined space.
// <li> The cursor cannot be a reference to a part of the Lattice since 
// hangover would imply a reference to undefined memory.  To enclose the 
// Lattice with a zero valued hangover buffer would be inefficient.  The 
// method thus forced upon the programmer is to "update" the cursor with 
// Lattice values after each move or iteration and to "write" the possibly 
// changed cursor values back into the Lattice before each iteration. An
// algorithm which does the cursor update/write actions (and is independent 
// of Lattice dimensionality) may be copied from ArrLatticeIter::cursorUpdate()
// and ArrLatticeIter::cursorWrite(), respectively.
// <li> The majority of the code in a new letter for LatticeIterator may be
// cut and pasted from other implementations of letters.  See ArrLatticeIter
// or PagedArrIter.
// </ul>
// </synopsis>

// <example>
// The class is pure virtual and isn't, itself, instantiated.  For an example 
// derivation from LatticeIterInterface, see ArrLatticeIter.
// </example>

// <motivation>
// The is class provides a tidy base for letter envelope techniques of 
// iteration. 
// </motivation>

// <todo asof="1997/01/12">
//  <li> IPositions are returned by value.  This a reflection of the 
//       LatticeNavigator base class' inability to predict the
//       availibility of data members for references.
// </todo>


template <class T> class LatticeIterInterface 
{
public:
  // Construct with the given navigator.
  LatticeIterInterface (const LatticeNavigator& navigator);

  // Copy constructor (copy semantics).
  LatticeIterInterface (const LatticeIterInterface<T>& other);
   
  // Assignment (copy semantics).
  LatticeIterInterface& operator= (const LatticeIterInterface<T>& other);
   
  // A virtual destructor. A virtual is needed to ensure that derived
  // classes declared as pointers to a LatticeIterInterface will scope their
  // destructor to the derived class destructor.
  virtual ~LatticeIterInterface();

  // Clone the object.
  virtual LatticeIterInterface<T>* clone() const = 0;

  // Increment operator - increment the cursor to the next position. The
  // implementation of the prefix operator calls the postfix one.
  // <group>
  Bool operator++();
  virtual Bool operator++ (Int) = 0;
  // </group>

  // Decrement operator - decrement the cursor to the previous position. The
  // implementation of the prefix operator calls the postfix one.
  // <group>
  Bool operator--();
  virtual Bool operator-- (Int) = 0;
  // </group>

  // Function which resets the cursor to the beginning of the Lattice and
  // resets the number of steps taken to zero.
  virtual void reset() = 0;

  // Function which returns a value of "True" if the cursor is at the
  // beginning of the Lattice, otherwise, returns "False"
  Bool atStart() const;

  // Function which returns "True" if the cursor has been incremented to
  // the end of the lattice, otherwise, returns "False"
  Bool atEnd() const;

  // Function to return the number of steps (increments or decrements) taken
  // since construction (or since last reset).  This is a running count of
  // all cursor movement since doing N increments followed by N decrements
  // does not necessarily put the cursor back at the origin of the Lattice.
  uInt nsteps() const;
  
  // Function which returns the current position of the beginning of the
  // cursor within the Lattice. The returned IPosition will have the same
  // number of axes as the underlying Lattice.
  IPosition position() const;
  
  // Function which returns the current position of the end of the
  // cursor. The returned IPosition will have the same number of axes as the
  // underlying Lattice.
  IPosition endPosition() const;

  // Function which returns the shape of the Lattice being iterated through.
  // The returned IPosition will always have the same number of axes as the
  // underlying Lattice.
  IPosition latticeShape() const;

  // Function which returns the shape of the cursor which is iterating
  // through the Lattice. The cursor will always have as many dimensions as
  // the Lattice.
  IPosition cursorShape() const;

  // Functions which returns a window to the data in the Lattice. These are
  // used to read the data within the Lattice.  Use the function
  // that is appropriate to the current cursor dimension, AFTER REMOVING
  // DEGENERATE AXES, or use the <src>cursor</src> function which works with
  // any number of dimensions in the cursor. A call of the function whose
  // return value is inappropriate with respect to the current cursor
  // dimension will throw an exception (AipsError).
  // <group>
  virtual Vector<T>& vectorCursor() = 0;
  virtual Matrix<T>& matrixCursor() = 0;
  virtual Cube<T>& cubeCursor() = 0;
  virtual Array<T>& cursor() = 0;
  //</group>

  // Write the data in the cursor.
  virtual void writeCursor() = 0;

  // Write the given data array at the current cursor position.
  // The array shape has to match the cursor shape (as given by cursorShape()).
  virtual void writeArray (const Array<T>& data) = 0;

  // Function which checks the internals of the class for consistency.
  // Returns True if everything is fine otherwise returns False. The default
  // implementation of this function always returns True.
  virtual Bool ok() const;

protected:
  // pointer to the method of Lattice transversal
  LatticeNavigator* itsNavPtr;
};



template <class T>
inline Bool LatticeIterInterface<T>::operator++() {
  return operator++ (0);
}

template <class T>
inline Bool LatticeIterInterface<T>::operator--() {
  return operator-- (0);
}

template<class T>
inline Bool LatticeIterInterface<T>::atStart() const
{
  return itsNavPtr->atStart();
}

template<class T>
inline Bool LatticeIterInterface<T>::atEnd() const
{
  return itsNavPtr->atEnd();
}

template<class T>
inline uInt LatticeIterInterface<T>::nsteps() const
{
  return itsNavPtr->nsteps();
}

template<class T>
inline IPosition LatticeIterInterface<T>::position() const
{
  return itsNavPtr->position();
}

template<class T>
inline IPosition LatticeIterInterface<T>::endPosition() const
{
  return itsNavPtr->endPosition();
}

template<class T>
inline IPosition LatticeIterInterface<T>::latticeShape() const
{
  return itsNavPtr->latticeShape();
}

template<class T>
inline IPosition LatticeIterInterface<T>::cursorShape() const
{
  return itsNavPtr->cursorShape();
}


#endif
