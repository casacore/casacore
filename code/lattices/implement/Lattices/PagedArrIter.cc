//# PagedArrIter.cc: a concrete iterator for use with PagedArray's.
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

#include <trial/Lattices/PagedArrIter.h>
#include <trial/Lattices/LatticeStepper.h>
#include <trial/Lattices/TiledStepper.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/Cube.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/MatrixMath.h>
#include <aips/Exceptions/Error.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Logging/LogIO.h>
#include <aips/Logging/LogOrigin.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/DefaultValue.h>
#include <aips/Utilities/String.h>

// +++++++++++++++++++++++ RO_PagedArrIter ++++++++++++++++++++++++++++++++

template<class T> RO_PagedArrIter<T>::
RO_PagedArrIter(const PagedArray<T> & data, const LatticeNavigator & nav)
  :theData(data),
   theNavPtr(nav.clone()),
   theReadFlag(False)
{
  AlwaysAssert(allocateCursor() == True, AipsError);
  setup_tile_cache();
  AlwaysAssert(ok() == True, AipsError);
};

template<class T> RO_PagedArrIter<T>::
RO_PagedArrIter(const PagedArray<T> & data, const IPosition & curShape)
  :theData(data),
   theNavPtr(new LatticeStepper(data.shape(), curShape)),
   theReadFlag(False)
{
  AlwaysAssert(allocateCursor() == True, AipsError);
  setup_tile_cache();
  AlwaysAssert(ok() == True, AipsError);
};

template<class T> RO_PagedArrIter<T>::
RO_PagedArrIter(const RO_PagedArrIter<T> & other)
  :theData(other.theData),
   theNavPtr(other.theNavPtr->clone()),
   theReadFlag(False)
{
  AlwaysAssert(allocateCursor() == True, AipsError);
  DebugAssert(ok() == True, AipsError);
};

template<class T> RO_PagedArrIter<T>::
~RO_PagedArrIter() {
  theData.clearCache();
  delete theCurPtr;
};

template<class T> RO_PagedArrIter<T> & RO_PagedArrIter<T>::
operator=(const RO_PagedArrIter<T> & other) {
  if (this != &other) {
    delete theCurPtr;
    theCurPtr = 0;
    theData = other.theData;
    theNavPtr = other.theNavPtr->clone();
    AlwaysAssert(allocateCursor() == True, AipsError);
    theReadFlag = False;
  }
  DebugAssert(ok() == True, AipsError);
  return *this;
};

template<class T> Bool RO_PagedArrIter<T>::
operator++() {
  return this->operator++(0);
};

template<class T> Bool RO_PagedArrIter<T>::
operator++(Int) {
  Bool moved = theNavPtr->operator++();
  if (moved)
    cursorUpdate();
  DebugAssert(ok() == True, AipsError);
  return moved;
};

template<class T> Bool RO_PagedArrIter<T>::
operator--() {
  return this->operator--(0);
};

template<class T> Bool RO_PagedArrIter<T>::
operator--(Int) {
  Bool moved = theNavPtr->operator--();
  if (moved)
    cursorUpdate();
  DebugAssert(ok() == True, AipsError);
  return moved;
};

template<class T> void RO_PagedArrIter<T>::
reset() {
  theNavPtr->reset();
  cursorUpdate();
  DebugAssert(ok() == True, AipsError);
};

template<class T> Bool RO_PagedArrIter<T>::
atStart() const {
  return theNavPtr->atStart();
};

template<class T> Bool RO_PagedArrIter<T>::
atEnd() const {
  return theNavPtr->atEnd();
};

template<class T> uInt RO_PagedArrIter<T>::
nsteps() const {
  return theNavPtr->nsteps();
};

template<class T> IPosition RO_PagedArrIter<T>::
position() const {
  return theNavPtr->position();
};

template<class T> IPosition RO_PagedArrIter<T>::
endPosition() const {
  return theNavPtr->endPosition();
};

template<class T> IPosition RO_PagedArrIter<T>::
latticeShape() const {
  return theNavPtr->latticeShape();
};

template<class T> IPosition RO_PagedArrIter<T>::
cursorShape() const {
  return theNavPtr->cursorShape();
};

template<class T> const Vector<T> & RO_PagedArrIter<T>::
vectorCursor() const {
  DebugAssert(ok() == True, AipsError);
  if (theCurPtr->ndim() != 1)
    throw(AipsError("RO_PagedArrIter<T>::vectorCursor()"
		    " - check the cursor has only one non-degenerate axis"));
  if (!theReadFlag) {
    getData();
  }
  return *(const Vector<T> *) theCurPtr;
};

template<class T> const Matrix<T> & RO_PagedArrIter<T>::
matrixCursor() const {
  DebugAssert(ok() == True, AipsError);
  if (theCurPtr->ndim() != 2)
    throw(AipsError("RO_PagedArrIter<T>::matrixCursor()"
		    " - check the cursor has only two non-degenerate axes"));
  if (!theReadFlag) {
    getData();
  }
  return *(const Matrix<T> *) theCurPtr;
};

template<class T> const Cube<T> & RO_PagedArrIter<T>::
cubeCursor() const {
  DebugAssert(ok() == True, AipsError);
  if (theCurPtr->ndim() != 3)
    throw(AipsError("RO_PagedArrIter<T>::cubeCursor()"
		    " - check the cursor has only three non-degenerate axes"));
  if (!theReadFlag) {
    getData();
  }
  return *(const Cube<T> *) theCurPtr;
};

template<class T> const Array<T> & RO_PagedArrIter<T>::
cursor() const {
  DebugAssert(ok() == True, AipsError);
  if (!theReadFlag) {
    getData();
  }
  return theCursor;
};

template<class T> void RO_PagedArrIter<T>::
getData()
{
  const IPosition shape = theNavPtr->cursorShape();
  const IPosition start = theNavPtr->position();
  const IPosition incr = theNavPtr->increment();
  if (theNavPtr->hangOver() == False) {
    Bool isARef = theData.getSlice(theCursor, start, shape, incr);
    DebugAssert(isARef == False, AipsError);
  } else {
    IPosition extractShape = 1 + (theNavPtr->endPosition() - start) / incr;
    // If needed the entire cursor is initialized first.
    if (extractShape != shape) {
      T overHangVal;
      defaultValue(overHangVal); 
      *theCurPtr = overHangVal;
    }
    // Then fill in the appropriate region with the bit that does not overhang.
    const uInt nrdim = extractShape.nelements();
    Array<T> subArr(theCursor(IPosition(nrdim, 0), extractShape-1));
    Bool isARef = theData.getSlice(subArr, start, extractShape, incr);
    DebugAssert(isARef == False, AipsError);
  }
  theReadFlag = True;
};

template<class T> Bool RO_PagedArrIter<T>::
ok() const {
  // The LogIO class is only constructed if an Error is detected for
  // performance reasons. Both function static and file static variables
  // where considered and rejected for this purpose.

  String message;
  Bool flag = True;
  // Check that we have a pointer to a cursor and not a NULL pointer.
  if (theCurPtr == 0) {
    message += "Cursor pointer is uninitialised\n";
    flag = False;
  }
  // Check the cursor is OK (by calling its "ok" function).
  if (theCurPtr->ok() == False) {
    message += "Cursor internals are inconsistent\n"; 
    flag = False;
  }
  // Do the same for the Array cursor
  if (theCursor.ok() == False) {
    message += "Array Cursor internals are inconsistent\n";
    flag = False;
  }
  // Check that both cursors have the same number of elements
  if (theCursor.nelements() != theCurPtr->nelements()) {
    message += "Cursors are inconsistent lengths\n"; 
    flag = False;
  }
  // Check that both cursors point to the same data.
  const T* p1 = &(theCursor(IPosition (theCursor.ndim(), 0)));
  const T* p2 = &((*theCurPtr)(IPosition (theCurPtr->ndim(), 0)));
  if (p1 != p2) {
    message += "Cursors contain different data\n"; 
    flag = False;
  }
  // Check that we have a pointer to a navigator and not a NULL pointer.
  if (theNavPtr.null() == True) {
    message += "Navigator pointer is uninitialised\n";
    flag = False;
  }
  // Check the navigator is OK (by calling its "ok" function).
  if (theNavPtr->ok() == False) {
    message += "Navigator internals are inconsistent\n"; 
    flag = False;
  }
  // Check the Navigator and Lattice are the same shape
  if (!(theNavPtr->latticeShape().isEqual(theData.shape()))) {
    message += "Navigator Lattice and Data Lattice are different shapes\n";
    flag = False;
  }
  // Check the Navigator cursor and cached Array are the same shape
  if (!(theNavPtr->cursorShape().isEqual(theCursor.shape()))) {
    message += "Navigator cursor and Data cursor are different shapes\n"; 
    flag = False;
  }
  if (!flag) {
    LogIO ROlogErr(LogOrigin("RO_PagedArrIter<T>", "ok()"));
    ROlogErr << LogIO::SEVERE << message << LogIO::POST;
  }
  return flag;
};

template<class T> void RO_PagedArrIter<T>::
cursorUpdate()
{
  // Set to data not read.
  theReadFlag = False;
  // Check if the cursor shape has changed.
  const IPosition oldShape(theCurPtr->shape());
  const IPosition newShape(theNavPtr->cursorShape().
			                      nonDegenerate(theCursorAxes));
  if (oldShape != newShape) {
    theCurPtr->resize (newShape);
    relinkArray();
  }
};

template<class T> Bool RO_PagedArrIter<T>::
allocateCursor() {
  const IPosition cursorAxes(theNavPtr->cursorAxes());
  theCursorAxes.resize (cursorAxes.nelements());
  theCursorAxes = cursorAxes;
  const IPosition cursorShape(theNavPtr->cursorShape());
  const IPosition realShape(cursorShape.nonDegenerate (theCursorAxes));
  const uInt ndim = realShape.nelements();
  AlwaysAssert(ndim > 0, AipsError)
  switch (ndim) {
  case 1:
    theCurPtr = new Vector<T>(realShape);
    break;
  case 2:
    theCurPtr = new Matrix<T>(realShape);
    break;
  case 3:
    theCurPtr = new Cube<T>(realShape);
    break;
  default:
    theCurPtr = new Array<T>(realShape);
    break;
  };
  if (theCurPtr == 0) 
    return False;
  relinkArray();
  return True;
};

template<class T> void RO_PagedArrIter<T>::
setup_tile_cache() {
  IPosition axisPath (theNavPtr->axisPath());
  LatticeStepper * stepper = theNavPtr->castToStepper();

  if (stepper != 0) {
    theData.setCacheSizeFromPath(theNavPtr->cursorShape(), theNavPtr->blc(), 
                                 theNavPtr->trc()-theNavPtr->blc()+1, axisPath);
    return;
  }
  TiledStepper * tilerPtr = theNavPtr->castToTiler();
  if (tilerPtr != 0){
    // The main axis is the last axis in the path.
    Int axis = axisPath (axisPath.nelements() - 1);
    // Tile per tile is accessed, but the main axis needs the entire window.
    // So calculate the start and end tile for the window.
    IPosition tileShape = tilerPtr->tileShape();
    Int tilesz = tileShape(axis);
    Int stTile = theNavPtr->blc()(axis) / tilesz;
    Int endTile = theNavPtr->trc()(axis) / tilesz;
    theData.setCacheSize ((1 + endTile - stTile) * tileShape.product());
    return;
  }
  // Because the current stepper is not a LatticeStepper or TiledStepper
  // assume that the longest axis is the fastest moving one.
  IPosition shape = theData.shape();
  Int whichLongest = 0;
  for (uInt i=1; i<shape.nelements(); i++)
    if (shape(i) > shape(whichLongest))
      whichLongest = i;
  axisPath = IPosition(1,whichLongest);
  theData.setCacheSizeFromPath(theNavPtr->cursorShape(), theNavPtr->blc(), 
                               theNavPtr->trc()-theNavPtr->blc()+1, axisPath);
};

template<class T> void RO_PagedArrIter<T>::
relinkArray() {
  Bool isACopy;
  theCursor.takeStorage(theNavPtr->cursorShape(), 
                        theCurPtr->getStorage(isACopy), SHARE);
  DebugAssert(isACopy == False, AipsError);
};

// +++++++++++++++++++++++ PagedArrIter ++++++++++++++++++++++++++++++++

template<class T> PagedArrIter<T>::
PagedArrIter(PagedArray<T> & data, const LatticeNavigator & nav)
  :theData(data),
   theNavPtr(nav.clone()),
   theReadFlag(False)
{
  AlwaysAssert(allocateCursor() == True, AipsError);
  setup_tile_cache();
  cursorUpdate();
  AlwaysAssert(ok() == True, AipsError);
};

template<class T> PagedArrIter<T>::
PagedArrIter(PagedArray<T> & data, const IPosition & curShape)
  :theData(data),
   theNavPtr(new LatticeStepper(data.shape(), curShape)),
   theReadFlag(False)
{
  AlwaysAssert(allocateCursor() == True, AipsError);
  setup_tile_cache();
  cursorUpdate();
  AlwaysAssert(ok() == True, AipsError);
};

template<class T> PagedArrIter<T>::
PagedArrIter(const PagedArrIter<T> & other)
  :theData(other.theData),
   theNavPtr(other.theNavPtr->clone()),
   theReadFlag(False)
{
  AlwaysAssert(allocateCursor() == True, AipsError);
  *theCurPtr = *(other.theCurPtr);
  DebugAssert(ok() == True, AipsError);
};

template<class T> PagedArrIter<T>::
~PagedArrIter() {
  cursorWrite();
  theData.clearCache();
  delete theCurPtr;
};

template<class T> PagedArrIter<T> & PagedArrIter<T>::
operator=(const PagedArrIter<T> & other) {
  if (this != &other) {
    cursorWrite();
    theData.clearCache();
    delete theCurPtr;
    theCurPtr = 0;
    theData = other.theData;
    theNavPtr = other.theNavPtr->clone();
    AlwaysAssert(allocateCursor() == True, AipsError);
    theReadFlag = False;
    *theCurPtr = *(other.theCurPtr);
  }
  DebugAssert(ok() == True, AipsError);
  return *this;
};

template<class T> Bool PagedArrIter<T>::
operator++() {
  return this->operator++(0);
};

template<class T> Bool PagedArrIter<T>::
operator++(Int) {
  cursorWrite();
  Bool moved = theNavPtr->operator++();
  if (moved)
    cursorUpdate();
  DebugAssert(ok() == True, AipsError);
  return moved;
};

template<class T> Bool PagedArrIter<T>::
operator--() {
  return this->operator--(0);
};

template<class T> Bool PagedArrIter<T>::
operator--(Int) {
  cursorWrite();
  Bool moved = theNavPtr->operator--();
  if (moved)
    cursorUpdate();
  DebugAssert(ok() == True, AipsError);
  return moved;
};

template<class T> void PagedArrIter<T>::
reset() {
  cursorWrite();
  theNavPtr->reset();
  cursorUpdate();
  DebugAssert(ok() == True, AipsError);
};

template<class T> Bool PagedArrIter<T>::
atStart() const {
  return theNavPtr->atStart();
};

template<class T> Bool PagedArrIter<T>::
atEnd() const {
  return theNavPtr->atEnd();
};

template<class T> uInt PagedArrIter<T>::
nsteps() const {
  return theNavPtr->nsteps();
};

template<class T> IPosition PagedArrIter<T>::
position() const {
  return theNavPtr->position();
};

template<class T> IPosition PagedArrIter<T>::
endPosition() const {
  return theNavPtr->endPosition();
};

template<class T> IPosition PagedArrIter<T>::
latticeShape() const {
  return theNavPtr->latticeShape();
};

template<class T> IPosition PagedArrIter<T>::
cursorShape() const {
  return theNavPtr->cursorShape();
};

template<class T> Vector<T> & PagedArrIter<T>::
vectorCursor() {
  DebugAssert(ok() == True, AipsError);
  if (theCurPtr->ndim() != 1)
    throw(AipsError("PagedArrIter<T>::vectorCursor()"
		    " - check the cursor has only one non-degenerate axis"));
  return *(Vector<T> *) theCurPtr;
};

template<class T> Matrix<T> & PagedArrIter<T>::
matrixCursor() {
  DebugAssert(ok() == True, AipsError);
  if (theCurPtr->ndim() != 2)
    throw(AipsError("PagedArrIter<T>::matrixCursor()"
		    " - check the cursor has only two non-degenerate axes"));
  return *(Matrix<T> *) theCurPtr;
};

template<class T> Cube<T> & PagedArrIter<T>::
cubeCursor() {
  DebugAssert(ok() == True, AipsError);
  if (theCurPtr->ndim() != 3)
    throw(AipsError("PagedArrIter<T>::cubeCursor()"
		    " - check the cursor has only three non-degenerate axes"));
  return *(Cube<T> *) theCurPtr;
};

template<class T> Array<T> & PagedArrIter<T>::
cursor() {
  DebugAssert(ok() == True, AipsError);
  return theCursor;
};

template<class T> Bool PagedArrIter<T>::
ok() const {
  // The LogIO class is only constructed if an Error is detected for
  // performance reasons. Both function static and file static variables
  // where considered and rejected for this purpose.

  // Check that we have a pointer to a cursor and not a NULL pointer.
  if (theCurPtr == 0) {
    LogIO logErr(LogOrigin("PagedArrIter<T>", "ok()"));
    logErr << LogIO::SEVERE << "Cursor pointer is uninitialised"
           << LogIO::POST;
     return False;
  }
  // Check the cursor is OK (by calling its "ok" function).
  if (theCurPtr->ok() == False) {
    LogIO logErr(LogOrigin("PagedArrIter<T>", "ok()"));
    logErr << LogIO::SEVERE << "Cursor internals are inconsistent" 
           << LogIO::POST;
     return False;
  }
  // Do the same for the Array cursor
  if (theCursor.ok() == False) {
    LogIO logErr(LogOrigin("PagedArrIter<T>", "ok()"));
    logErr << LogIO::SEVERE << "Array Cursor internals are inconsistent" 
           << LogIO::POST;
     return False;
  }
  // Check that both cursors have the same number of elements
  if (theCursor.nelements() != theCurPtr->nelements()) {
    LogIO logErr(LogOrigin("PagedArrIter<T>", "ok()"));
    logErr << LogIO::SEVERE << "Cursors are inconsistent lengths" 
           << LogIO::POST;
     return False;
  }
  // Check that both cursors point to the same data.
  const T* p1 = &(theCursor(IPosition (theCursor.ndim(), 0)));
  const T* p2 = &((*theCurPtr)(IPosition (theCurPtr->ndim(), 0)));
  if (p1 != p2) {
    LogIO logErr(LogOrigin("PagedArrIter<T>", "ok()"));
    logErr << LogIO::SEVERE << "Cursors contain different data" 
	   << LogIO::POST;
    return False;
  }
  // Check that we have a pointer to a navigator and not a NULL pointer.
  if (theNavPtr.null() == True) {
    LogIO logErr(LogOrigin("PagedArrIter<T>", "ok()"));
    logErr << LogIO::SEVERE << "Navigator pointer is uninitialised"
           << LogIO::POST;
    return False;
  }
  // Check the navigator is OK (by calling its "ok" function).
  if (theNavPtr->ok() == False) {
    LogIO logErr(LogOrigin("PagedArrIter<T>", "ok()"));
    logErr << LogIO::SEVERE << "Navigator internals are inconsistent" 
           << LogIO::POST;
    return False;
  }
  // Check the Navigator and Lattice are the same shape
  if (!(theNavPtr->latticeShape().isEqual(theData.shape()))) {
    LogIO logErr(LogOrigin("PagedArrIter<T>", "ok()"));
    logErr << LogIO::SEVERE 
           << "Navigator Lattice and Data Lattice are different shapes"
           << LogIO::POST;
     return False;
  }
  // Check the Navigator cursor and cached Array are the same shape
  if (!(theNavPtr->cursorShape().isEqual(theCursor.shape()))) {
    LogIO logErr(LogOrigin("PagedArrIter<T>", "ok()"));
    logErr << LogIO::SEVERE 
	   << "Navigator cursor and Data cursor are different shapes" 
	   << LogIO::POST;
    return False;
  }
  return True;
};

template<class T> void PagedArrIter<T>::
cursorWrite() {
  const IPosition start = theNavPtr->position();
  const IPosition incr = theNavPtr->increment();
  if (theNavPtr->hangOver() == False)
    theData.putSlice(theCursor, start, incr);
  else {
    // Write the appropriate region.
    IPosition extractShape = 1 + (theNavPtr->endPosition() - start) / incr;
    const uInt nrdim = extractShape.nelements();
    Array<T> subArr(theCursor(IPosition(nrdim, 0), extractShape-1));
    theData.putSlice(subArr, start, incr); 
  }
};

template<class T> void PagedArrIter<T>::
cursorUpdate() {
  // Set to data not read.
  theReadFlag = False;
  const IPosition shape = theNavPtr->cursorShape();
  const IPosition start = theNavPtr->position();
  const IPosition incr = theNavPtr->increment();
  // Check if the cursor shape has changed.
  {
    const IPosition oldShape(theCurPtr->shape());
    const IPosition newShape(shape.nonDegenerate(theCursorAxes));
    if (oldShape != newShape) {
      theCurPtr->resize(newShape);
      relinkArray();
    }
  }
  if (theNavPtr->hangOver() == False) {
    Bool isARef = theData.getSlice(theCursor, start, shape, incr);
    DebugAssert(isARef == False, AipsError);
  }
  else {
    IPosition extractShape = 1 + (theNavPtr->endPosition() - start) / incr;
    // If needed the entire cursor is initialized first.
    if (extractShape != shape) {
      T overHangVal;
      defaultValue(overHangVal); 
      *theCurPtr = overHangVal;
    }
    // Then fill in the appropriate region with the bit that does not overhang.
    const uInt nrdim = extractShape.nelements();
    Array<T> subArr(theCursor(IPosition(nrdim, 0), extractShape-1));
    Bool isARef = theData.getSlice(subArr, start, extractShape, incr);
    DebugAssert(isARef == False, AipsError);
  }
};

template<class T> Bool PagedArrIter<T>::
allocateCursor() {
  const IPosition cursorAxes(theNavPtr->cursorAxes());
  theCursorAxes.resize (cursorAxes.nelements());
  theCursorAxes = cursorAxes;
  const IPosition cursorShape(theNavPtr->cursorShape());
  const IPosition realShape(cursorShape.nonDegenerate(theCursorAxes));
  const uInt ndim = realShape.nelements();
  AlwaysAssert(ndim > 0, AipsError)
  switch (ndim) {
  case 1:
    theCurPtr = new Vector<T>(realShape);
    break;
  case 2:
    theCurPtr = new Matrix<T>(realShape);
    break;
  case 3:
    theCurPtr = new Cube<T>(realShape);
    break;
  default:
    theCurPtr = new Array<T>(realShape);
    break;
  };
  if (theCurPtr == 0) 
    return False;
  relinkArray();
  return True;
};

template<class T> void PagedArrIter<T>::
setup_tile_cache() {
  IPosition axisPath (theNavPtr->axisPath());
  LatticeStepper * stepper = theNavPtr->castToStepper();

  if (stepper != 0) {
    theData.setCacheSizeFromPath(theNavPtr->cursorShape(), theNavPtr->blc(), 
                                 theNavPtr->trc()-theNavPtr->blc()+1, axisPath);
    return;
  }
  TiledStepper * tilerPtr = theNavPtr->castToTiler();
  if (tilerPtr != 0){
    // The main axis is the last axis in the path.
    Int axis = axisPath (axisPath.nelements() - 1);
    // Tile per tile is accessed, but the main axis needs the entire window.
    // So calculate the start and end tile for the window.
    IPosition tileShape = tilerPtr->tileShape();
    Int tilesz = tileShape(axis);
    Int stTile = theNavPtr->blc()(axis) / tilesz;
    Int endTile = theNavPtr->trc()(axis) / tilesz;
    theData.setCacheSize ((endTile - stTile + 1) * tileShape.product());
//    IPosition cursorShape = tilerPtr->cursorShape();
//    cursorShape(axis) = theNavPtr->trc()(axis) - theNavPtr->blc()(axis) + 1;
//    theData.setCacheSizeFromPath(cursorShape, IPosition(),
//                                 IPosition(), IPosition(1,axis));
    return;
  }
  // Because the current stepper is not a LatticeStepper or TiledStepper
  // assume that the longest axis is the fastest moving one.
  IPosition shape = theData.shape();
  Int whichLongest = 0;
  for (uInt i=1; i<shape.nelements(); i++)
    if (shape(i) > shape(whichLongest))
      whichLongest = i;
  axisPath = IPosition(1,whichLongest);
  theData.setCacheSizeFromPath(theNavPtr->cursorShape(), theNavPtr->blc(), 
                               theNavPtr->trc()-theNavPtr->blc()+1, axisPath);
};

template<class T> void PagedArrIter<T>::
relinkArray() {
  Bool isACopy;
  theCursor.takeStorage(theNavPtr->cursorShape(), 
                        theCurPtr->getStorage(isACopy), SHARE);
  DebugAssert(isACopy == False, AipsError);
};
