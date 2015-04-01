//# LatticeIterInterface.cc: A base class for concrete Lattice iterators
//# Copyright (C) 1995,1997,1998,1999,2000,2003
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

#ifndef LATTICES_LATTICEITERINTERFACE_TCC
#define LATTICES_LATTICEITERINTERFACE_TCC

#include <casacore/lattices/Lattices/LatticeIterInterface.h>
#include <casacore/lattices/Lattices/Lattice.h>
#include <casacore/lattices/Lattices/LatticeNavigator.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Cube.h>
#include <casacore/casa/Utilities/DefaultValue.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

template <class T>
LatticeIterInterface<T>::LatticeIterInterface()
: itsNavPtr   (0),
  itsLattPtr  (0),
  itsCurPtr   (0),
  itsUseRef   (False),
  itsIsRef    (False),
  itsHaveRead (False),
  itsRewrite  (False)
{}

template <class T>
LatticeIterInterface<T>::LatticeIterInterface (const Lattice<T>& lattice,
					       const LatticeNavigator& nav,
					       Bool useRef)
: itsNavPtr     (nav.clone()),
  itsLattPtr    (lattice.clone()),
  itsUseRef     (useRef && lattice.canReferenceArray()),
  itsIsRef      (False),
  itsHaveRead   (False),
  itsRewrite    (False),
  itsCursorAxes (nav.cursorAxes())
{
  allocateCurPtr();
  if (!itsUseRef) {
    allocateBuffer();
  }
  DebugAssert(ok() == True, AipsError);
}

template <class T>
LatticeIterInterface<T>::LatticeIterInterface
                                        (const LatticeIterInterface<T>& other)
: itsCurPtr (0)
{
  copyBase (other);
  DebugAssert(ok() == True, AipsError);
}
   
template <class T>
LatticeIterInterface<T>::~LatticeIterInterface()
{
  rewriteData();
  delete itsCurPtr;
  delete itsNavPtr;
  delete itsLattPtr;
}

template <class T>
LatticeIterInterface<T>& LatticeIterInterface<T>::operator=
                                        (const LatticeIterInterface<T>& other)
{
  if (this != &other) {
    rewriteData();
    copyBase (other);
  }
  DebugAssert(ok() == True, AipsError);
  return *this;
}

template <class T>
void LatticeIterInterface<T>::copyBase (const LatticeIterInterface<T>& other)
{
  delete itsCurPtr;
  itsCurPtr = 0;
  itsBuffer.resize();
  itsCursorAxes.resize(0);
  itsNavPtr     = other.itsNavPtr->clone();
  itsLattPtr    = other.itsLattPtr->clone();
  itsUseRef     = other.itsUseRef;
  itsIsRef      = other.itsIsRef;
  itsHaveRead   = other.itsHaveRead;
  itsRewrite    = False;
  itsCursorAxes = other.itsCursorAxes;
  allocateCurPtr();
  if (!itsIsRef) {
    allocateBuffer();
    if (itsHaveRead) {
      itsBuffer = other.itsBuffer;
    }
  } else {
    Array<T> tmp(other.itsCursor);
    itsCursor.reference (tmp);
    setCurPtr2Cursor();
  }
}

template<class T>
LatticeIterInterface<T>* LatticeIterInterface<T>::clone() const
{
    return new LatticeIterInterface<T> (*this);
}

template<class T>
Bool LatticeIterInterface<T>::operator++(int)
{
  if (itsRewrite) {
    rewriteData();
  }
  Bool moved = itsNavPtr->operator++();
  if (moved) {
    cursorUpdate();
  }
  DebugAssert(ok() == True, AipsError);
  return moved;
}

template<class T>
Bool LatticeIterInterface<T>::operator--(int)
{
  if (itsRewrite) {
    rewriteData();
  }
  Bool moved = itsNavPtr->operator--();
  if (moved) {
    cursorUpdate();
  }
  DebugAssert(ok() == True, AipsError);
  return moved;
}

template<class T>
void LatticeIterInterface<T>::reset()
{
  rewriteData();
  itsNavPtr->reset();
  cursorUpdate();
  DebugAssert(ok() == True, AipsError);
}

template<class T>
Vector<T>& LatticeIterInterface<T>::vectorCursor (Bool doRead,
						  Bool autoRewrite)
{
  DebugAssert(ok() == True, AipsError);
  if (itsCurPtr->ndim() != 1) {
    throw(AipsError("LatticeIterInterface<T>::vectorCursor"
		    " - check the cursor has only one non-degenerate axis"));
  }
  if (!itsHaveRead) {
    readData (doRead);
  }
  if (autoRewrite) {
    itsRewrite = True;
  }
  return *(Vector<T>*)itsCurPtr;
}

template<class T>
Matrix<T>& LatticeIterInterface<T>::matrixCursor (Bool doRead,
						  Bool autoRewrite)
{
  DebugAssert(ok() == True, AipsError);
  if (itsCurPtr->ndim() != 2) {
    throw(AipsError("LatticeIterInterface<T>::matrixCursor"
		    " - check the cursor has only two non-degenerate axes"));
  }
  if (!itsHaveRead) {
    readData (doRead);
  }
  if (autoRewrite) {
    itsRewrite = True;
  }
  return *(Matrix<T>*)itsCurPtr;
}

template<class T>
Cube<T>& LatticeIterInterface<T>::cubeCursor (Bool doRead, Bool autoRewrite)
{
  DebugAssert(ok() == True, AipsError);
  if (itsCurPtr->ndim() != 3) {
    throw(AipsError("LatticeIterInterface<T>::cubeCursor"
		    " - check the cursor has only three non-degenerate axes"));
  }
  if (!itsHaveRead) {
    readData (doRead);
  }
  if (autoRewrite) {
    itsRewrite = True;
  }
  return *(Cube<T>*)itsCurPtr;
}

template<class T>
Array<T>& LatticeIterInterface<T>::cursor (Bool doRead, Bool autoRewrite)
{
  DebugAssert(ok() == True, AipsError);
  if (!itsHaveRead) {
    readData (doRead);
  }
  if (autoRewrite) {
    itsRewrite = True;
  }
  return itsCursor;
}


template<class T>
void LatticeIterInterface<T>::readData (Bool doRead)
{
  if (doRead  ||  itsUseRef) {
    const IPosition shape = itsNavPtr->cursorShape();
    const IPosition start = itsNavPtr->position();
    const IPosition incr  = itsNavPtr->increment();
    IPosition extractShape;
    Bool hangOver = itsNavPtr->hangOver();
    if (hangOver) {
      extractShape = 1 + (itsNavPtr->endPosition() - start) / incr;
      if (extractShape == shape) {
	hangOver = False;
      }
    }
    if (!hangOver) {
      // No hangover, so get entire slice.
      if (itsUseRef) {
	// Set the cursor as a reference to the original array.
	itsIsRef = itsLattPtr->getSlice (itsCursor, start, shape, incr);
	DebugAssert (itsIsRef, AipsError);
	setCurPtr2Cursor();
      } else {
	itsIsRef = False;
	if (doRead) {
	  // Use a temporary array pointing to the same storage as itsCursor. 
	  // When getSlice returns a reference, tmp is pointing to that
	  // referenced storage, so we have to copy the data.
	  Array<T> tmp (itsCursor);
	  Bool isARef = itsLattPtr->getSlice (tmp, start, shape, incr);
	  if (isARef) {
	    itsCursor = tmp;
	  }
	}
      }
    } else {
      itsIsRef = False;
      if (itsUseRef) {
	allocateBuffer();
      }
      T overHangVal;
      defaultValue(overHangVal); 
      itsBuffer = overHangVal;
      // Fill in the appropriate region with the bit that does not overhang.
      // Use the same method as above to deal with possible references.
      const uInt nrdim = extractShape.nelements();
      Array<T> subArr(itsCursor(IPosition(nrdim, 0), extractShape-1));
      Bool isARef = itsLattPtr->getSlice (subArr, start, extractShape, incr);
      if (isARef) {
	itsCursor(IPosition(nrdim, 0), extractShape-1) = subArr;
      }
    }
  }
  itsHaveRead = True;
}


template<class T>
void LatticeIterInterface<T>::rewriteData()
{
  if (itsRewrite) {
    DebugAssert (ok(), AipsError);
    // Check that both cursors point to the same data.
    if (itsCursor.data() != itsCurPtr->data()) {
      throw (AipsError ("LatticeIterInterface::rewriteData - "
			"the data pointer inside the cursor has been changed "
			"(probably by an Array::reference)"));
    }
    // Writing is only needed if the data was not referenced.
    if (!itsIsRef) {
      const IPosition start = itsNavPtr->position();
      const IPosition incr = itsNavPtr->increment();
      if (itsNavPtr->hangOver() == False) {
	itsLattPtr->putSlice (itsCursor, start, incr);
      } else {
	// Write the appropriate region.
	IPosition extractShape = 1 + (itsNavPtr->endPosition() - start) / incr;
	const uInt nrdim = extractShape.nelements();
	Array<T> subArr(itsCursor(IPosition(nrdim, 0), extractShape-1));
	itsLattPtr->putSlice (subArr, start, incr); 
      }
    }
    itsRewrite = False;
  }
}

template<class T>
void LatticeIterInterface<T>::cursorUpdate()
{
  // Set to data not read.
  itsHaveRead = False;
  itsIsRef = False;
  // Reshape the cursor array if needed.
  if (!itsUseRef  &&  itsCursor.shape() != itsNavPtr->cursorShape()) {
    allocateBuffer();
  }
}

template<class T>
void LatticeIterInterface<T>::allocateCurPtr()
{
  const IPosition cursorShape(itsNavPtr->cursorShape());
  const IPosition realShape(cursorShape.nonDegenerate(itsCursorAxes));
  const uInt ndim = realShape.nelements();
  AlwaysAssert(ndim > 0, AipsError);
  switch (ndim) {
  case 1:
    itsCurPtr = new Vector<T>();
    break;
  case 2:
    itsCurPtr = new Matrix<T>();
    break;
  case 3:
    itsCurPtr = new Cube<T>();
    break;
  default:
    itsCurPtr = new Array<T>();
    break;
  }
}

template<class T>
void LatticeIterInterface<T>::setCurPtr2Cursor()
{
  if (itsCursor.data() != 0) {
    if (itsCurPtr->ndim() == itsCursor.ndim()) {
      itsCurPtr->reference (itsCursor);
    } else {
      Array<T> tmp (itsCursor.nonDegenerate (itsCursorAxes));
      itsCurPtr->reference (tmp);
    }
  } else {
    itsCurPtr->resize();
  }
}

template<class T>
void LatticeIterInterface<T>::allocateBuffer()
{
  // Do not reallocate the buffer if not really needed.
  // If the cursor gets smaller, the existing buffer can still be used.
  if (itsBuffer.nelements() == 0) {
    itsBuffer.resize (itsNavPtr->cursorShape());
  }
  Bool isACopy;
  T* data = itsBuffer.getStorage(isACopy);
  DebugAssert(isACopy == False, AipsError);
  itsCursor.takeStorage (itsNavPtr->cursorShape(), data, SHARE);
  DebugAssert (itsBuffer.nelements() >= itsCursor.nelements(), AipsError);
  setCurPtr2Cursor();
}

template<class T>
Bool LatticeIterInterface<T>::ok() const
{
  String message;
  Bool flag = True;
  // Check that we have a pointer to a cursor and not a NULL pointer.
  if (itsCurPtr == 0) {
    message += "Cursor pointer is uninitialized\n";
    flag = False;
  }
  // Check the cursor is OK (by calling its "ok" function).
  if (itsCurPtr->ok() == False) {
    message += "Cursor internals are inconsistent\n"; 
    flag = False;
  }
  // Do the same for the Array cursor
  if (itsCursor.ok() == False) {
    message += "Array Cursor internals are inconsistent\n";
    flag = False;
  }
  // Check that both cursors have the same number of elements
  if (itsCursor.nelements() != itsCurPtr->nelements()) {
    message += "Cursors have inconsistent lengths\n"; 
    flag = False;
  }
  // Check that both cursors point to the same data.
  if (itsCursor.data() != itsCurPtr->data()) {
    message += "Cursors contain different data\n"; 
    flag = False;
  }
  // Check that we have a pointer to a navigator and not a NULL pointer.
  if (itsNavPtr == 0) {
    message += "Navigator pointer is uninitialized\n";
    flag = False;
  }
  // Check the navigator is OK (by calling its "ok" function).
  if (itsNavPtr->ok() == False) {
    message += "Navigator internals are inconsistent\n"; 
    flag = False;
  }
  // Check the Navigator and Lattice are the same shape
  if (!(itsNavPtr->latticeShape().isEqual(itsLattPtr->shape()))) {
    message += "Navigator Lattice and Data Lattice have different shapes\n";
    flag = False;
  }
  // We do not check if the Navigator cursor and itsCursor are the same shape,
  // because ArrLatticeIter resizes the cursor only when it is being used.
  if (!flag) {
    throw AipsError ("LatticeIterInterface::ok - " + message);
  }
  return flag;
}

} //# NAMESPACE CASACORE - END


#endif
