//# LatticeIterInterface.cc: A base class for concrete Lattice iterators
//# Copyright (C) 1995,1997,1998
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

#include <trial/Lattices/LatticeIterInterface.h>
#include <aips/Utilities/DefaultValue.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>


template <class T>
LatticeIterInterface<T>::LatticeIterInterface()
: itsNavPtr   (0),
  itsLattPtr  (0),
  itsCurPtr   (0),
  itsHaveRead (False),
  itsRewrite  (False)
{}

template <class T>
LatticeIterInterface<T>::LatticeIterInterface (const Lattice<T>& lattice,
					      const LatticeNavigator& navigator)
: itsNavPtr   (navigator.clone()),
  itsLattPtr  (lattice.clone()),
  itsHaveRead (False),
  itsRewrite  (False)
{
  AlwaysAssert(allocateCursor() == True, AipsError);
  cursorUpdate();
  AlwaysAssert(ok() == True, AipsError);
}

template <class T>
LatticeIterInterface<T>::LatticeIterInterface
                                        (const LatticeIterInterface<T>& other)
{
  copyBase (other);
  if (itsHaveRead) {
    itsCursor = other.itsCursor;
  }
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
    delete itsCurPtr;
    copyBase (other);
    if (itsHaveRead) {
      itsCursor = other.itsCursor;
    }
  }
  DebugAssert(ok() == True, AipsError);
  return *this;
}

template <class T>
void LatticeIterInterface<T>::copyBase (const LatticeIterInterface<T>& other)
{
  itsCurPtr   = 0;
  itsNavPtr   = other.itsNavPtr->clone();
  itsLattPtr  = other.itsLattPtr->clone();
  itsHaveRead = other.itsHaveRead;
  itsRewrite  = False;
  AlwaysAssert(allocateCursor() == True, AipsError);
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
Vector<T>& LatticeIterInterface<T>::vectorCursor (Bool doRead, Bool autoRewrite)
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
Matrix<T>& LatticeIterInterface<T>::matrixCursor (Bool doRead, Bool autoRewrite)
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
  if (doRead) {
    const IPosition shape = itsNavPtr->cursorShape();
    const IPosition start = itsNavPtr->position();
    const IPosition incr  = itsNavPtr->increment();
    if (itsNavPtr->hangOver() == False) {
      // No hangover, so get entire slice.
      // Use a temporary array pointing to the same storage as itsCursor. 
      // When getSlice returns a reference, tmp is pointing to that
      // referenced storage, so we have to copy the data.
      Array<T> tmp (itsCursor);
      Bool isARef = itsLattPtr->getSlice (tmp, start, shape, incr);
      if (isARef) {
	itsCursor = tmp;
      }
    } else {
      IPosition extractShape = 1 + (itsNavPtr->endPosition() - start) / incr;
      // If needed the entire cursor is initialized first.
      if (extractShape != shape) {
	T overHangVal;
	defaultValue(overHangVal); 
	*itsCurPtr = overHangVal;
      }
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
    itsRewrite = False;
  }
}

template<class T>
void LatticeIterInterface<T>::cursorUpdate()
{
  // Set to data not read.
  itsHaveRead = False;
  // Check if the cursor shape has changed.
  if (itsCursor.shape() != itsNavPtr->cursorShape()) {
    itsCurPtr->resize (itsNavPtr->cursorShape().nonDegenerate(itsCursorAxes));
    relinkArray();
  }
}

template<class T>
Bool LatticeIterInterface<T>::allocateCursor()
{
  const IPosition cursorAxes(itsNavPtr->cursorAxes());
  itsCursorAxes.resize (cursorAxes.nelements());
  itsCursorAxes = cursorAxes;
  const IPosition cursorShape(itsNavPtr->cursorShape());
  const IPosition realShape(cursorShape.nonDegenerate(itsCursorAxes));
  const uInt ndim = realShape.nelements();
  AlwaysAssert(ndim > 0, AipsError)
  switch (ndim) {
  case 1:
    itsCurPtr = new Vector<T>(realShape);
    break;
  case 2:
    itsCurPtr = new Matrix<T>(realShape);
    break;
  case 3:
    itsCurPtr = new Cube<T>(realShape);
    break;
  default:
    itsCurPtr = new Array<T>(realShape);
    break;
  }
  if (itsCurPtr == 0) {
    return False;
  }
  relinkArray();
  return True;
}

template<class T>
void LatticeIterInterface<T>::relinkArray()
{
  Bool isACopy;
  itsCursor.takeStorage (itsNavPtr->cursorShape(), 
			 itsCurPtr->getStorage(isACopy), SHARE);
  DebugAssert(isACopy == False, AipsError);
}

template<class T>
Bool LatticeIterInterface<T>::ok() const
{
  // For performance reasons the LogIO class is only constructed if an
  // error is detected. Both function static and file static variables
  // where considered and rejected for this purpose.

  String message;
  Bool flag = True;
  // Check that we have a pointer to a cursor and not a NULL pointer.
  if (itsCurPtr == 0) {
    message += "Cursor pointer is uninitialised\n";
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
    message += "Cursors are inconsistent lengths\n"; 
    flag = False;
  }
  // Check that both cursors point to the same data.
  const T* p1 = &(itsCursor(IPosition (itsCursor.ndim(), 0)));
  const T* p2 = &((*itsCurPtr)(IPosition (itsCurPtr->ndim(), 0)));
  if (p1 != p2) {
    message += "Cursors contain different data\n"; 
    flag = False;
  }
  // Check that we have a pointer to a navigator and not a NULL pointer.
  if (itsNavPtr == 0) {
    message += "Navigator pointer is uninitialised\n";
    flag = False;
  }
  // Check the navigator is OK (by calling its "ok" function).
  if (itsNavPtr->ok() == False) {
    message += "Navigator internals are inconsistent\n"; 
    flag = False;
  }
  // Check the Navigator and Lattice are the same shape
  if (!(itsNavPtr->latticeShape().isEqual(itsLattPtr->shape()))) {
    message += "Navigator Lattice and Data Lattice are different shapes\n";
    flag = False;
  }
  // Check the Navigator cursor and cached Array are the same shape
  if (!(itsNavPtr->cursorShape().isEqual(itsCursor.shape()))) {
    message += "Navigator cursor and Data cursor are different shapes\n"; 
    flag = False;
  }
  if (!flag) {
    LogIO ROlogErr(LogOrigin("LatticeIterInterface<T>", "ok()"));
    ROlogErr << LogIO::SEVERE << message << LogIO::POST;
  }
  return flag;
}
