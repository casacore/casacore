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
   theNavPtr(nav.clone())
{
  AlwaysAssert(allocateCursor() == True, AipsError);
  setup_tile_cache();
  theData.getSlice(*theCurPtr, theNavPtr->position(),
		   theNavPtr->cursorShape(), theNavPtr->increment(), True);
  AlwaysAssert(ok() == True, AipsError);
};

template<class T> RO_PagedArrIter<T>::
RO_PagedArrIter(const PagedArray<T> & data, const IPosition & curShape)
  :theData(data),
   theNavPtr(new LatticeStepper(data.shape(), curShape))
{
  AlwaysAssert(allocateCursor() == True, AipsError);
  setup_tile_cache();
  theData.getSlice(*theCurPtr, theNavPtr->position(),
		   theNavPtr->cursorShape(), theNavPtr->increment(), True);
  AlwaysAssert(ok() == True, AipsError);
};

template<class T> RO_PagedArrIter<T>::
RO_PagedArrIter(const RO_PagedArrIter<T> & other)
  :theData(other.theData),
   theNavPtr(other.theNavPtr->clone())
{
  AlwaysAssert(allocateCursor() == True, AipsError);
  *theCurPtr = *(other.theCurPtr);
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
    theData = other.theData;
    theNavPtr = other.theNavPtr->clone();
    if (theCurPtr != 0) 
      delete theCurPtr;
    AlwaysAssert(allocateCursor() == True, AipsError);
    *theCurPtr = *(other.theCurPtr);
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
  Bool retval = theNavPtr->operator++(0);
  cursorUpdate();
  DebugAssert(ok() == True, AipsError);
  return retval;
};

template<class T> Bool RO_PagedArrIter<T>::
operator--() {
  return this->operator--(0);
};

template<class T> Bool RO_PagedArrIter<T>::
operator--(Int) {
  Bool retval = theNavPtr->operator--(0);
  cursorUpdate();
  DebugAssert(ok() == True, AipsError);
  return retval;
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
		    " - check the cursor shape is 1-dimensional"));
  return *(const Vector<T> *) theCurPtr;
};

template<class T> const Matrix<T> & RO_PagedArrIter<T>::
matrixCursor() const {
  DebugAssert(ok() == True, AipsError);
  if (theCurPtr->ndim() != 2)
    throw(AipsError("RO_PagedArrIter<T>::matrixCursor()"
		    " - check the cursor shape is 2-dimensional"));
  return *(const Matrix<T> *) theCurPtr;
};

template<class T> const Cube<T> & RO_PagedArrIter<T>::
cubeCursor() const {
  DebugAssert(ok() == True, AipsError);
  if (theCurPtr->ndim() != 3)
    throw(AipsError("RO_PagedArrIter<T>::cubeCursor()"
		    " - check the cursor shape is 3-dimensional"));
  return *(const Cube<T> *) theCurPtr;
};

template<class T> const Array<T> & RO_PagedArrIter<T>::
cursor() const {
  DebugAssert(ok() == True, AipsError);
  return *(const Array<T> *) theCurPtr;
};

template<class T> Bool RO_PagedArrIter<T>::
ok() const {
  // The LogIO class is only constructed if an Error is detected for
  // performance reasons. Both function static and file static variables
  // where considered and rejected for this purpose.

  // Check that we have a pointer to a cursor and not a NULL pointer.
  if (theCurPtr == 0) {
    LogIO ROlogErr(LogOrigin("RO_PagedArrIter<T>", "ok()"));
    ROlogErr << LogIO::SEVERE << "Cursor pointer is uninitialised"
	   << LogIO::POST;
     return False;
  }
  // Check the cursor is OK (by calling its "ok" function).
  if (theCurPtr->ok() == False) {
    LogIO ROlogErr(LogOrigin("RO_PagedArrIter<T>", "ok()"));
    ROlogErr << LogIO::SEVERE << "Cursor internals are inconsistant" 
	   << LogIO::POST;
     return False;
  }
  // Check that we have a pointer to a navigator and not a NULL pointer.
  if (theNavPtr.null() == True) {
    LogIO ROlogErr(LogOrigin("RO_PagedArrIter<T>", "ok()"));
    ROlogErr << LogIO::SEVERE << "Navigator pointer is uninitialised"
	   << LogIO::POST;
    return False;
  }
  // Check the navigator is OK (by calling its "ok" function).
  if (theNavPtr->ok() == False) {
    LogIO ROlogErr(LogOrigin("RO_PagedArrIter<T>", "ok()"));
    ROlogErr << LogIO::SEVERE << "Navigator internals are inconsistant" 
	   << LogIO::POST;
    return False;
  }
  // Check the Navigator and Lattice are the same shape
  if (!(theNavPtr->latticeShape()).isEqual(theData.shape())) {
    LogIO ROlogErr(LogOrigin("RO_PagedArrIter<T>", "ok()"));
    ROlogErr << LogIO::SEVERE 
	     << "Navigator Lattice and Data Lattice are different shapes"
	     << LogIO::POST;
     return False;
  }
  // Check the Navigator cursor and cached Array are the same shape
  // There is a special case if the cursor has only one element
  if ((theCurPtr->ndim() == 1) && 
      (theCurPtr->shape()(0) == 1)) {
    if (theNavPtr->cursorShape().product() != 1) {
      LogIO ROlogErr(LogOrigin("RO_PagedArrIter<T>", "ok()"));
      ROlogErr << LogIO::SEVERE 
	       << "Navigator cursor and Data cursor are not both unit shapes" 
	       << LogIO::POST;
      return False;
    }
  }
  else
    if (!(theNavPtr->cursorShape().nonDegenerate())
	.isEqual(theCurPtr->shape())) {
      LogIO ROlogErr(LogOrigin("RO_PagedArrIter<T>", "ok()"));
      ROlogErr << LogIO::SEVERE 
	       << "Navigator cursor and Data cursor are different shapes" 
	       << LogIO::POST;
      return False;
    }
  return True;
};

template<class T> void RO_PagedArrIter<T>::
cursorUpdate() {
  DebugAssert(ok() == True, AipsError);
  // Check if the cursor shape has changed.
  {
    IPosition newShape(theCurPtr->shape());
    IPosition oldShape(theNavPtr->cursorShape().nonDegenerate());
    if (newShape.nelements() != oldShape.nelements()){
      delete theCurPtr;
      allocateCursor();
    }
    else if (oldShape != newShape)
      theCurPtr->resize(newShape);
  }
  if (theNavPtr->hangOver() == False)
    theData.getSlice(*theCurPtr, theNavPtr->position(),
		     theNavPtr->cursorShape(), theNavPtr->increment(), True); 
  else {
    // Here I set the entire cursor to zero.
    // (the alternative is to set the cursor to an undefined value 
    //  using ValType.h))
    T overHangVal;
    defaultValue(overHangVal); 
    *theCurPtr = overHangVal;
    // and then fill in the appropriate region with the bit that does not
    // overhang.
    const IPosition latticeTrc(theNavPtr->latticeShape() - 1);
    const IPosition fullShape(theNavPtr->cursorShape());
    const uInt latDim = fullShape.nelements();
    IPosition cursorBlc(latDim, 0);
    IPosition cursorTrc(fullShape - 1);
    IPosition blc(theNavPtr->position());
    IPosition trc(theNavPtr->endPosition());
    const IPosition inc(theNavPtr->increment());
    IPosition extractShape(latDim,0);

    Int trim;
    for (uInt d = 0; d < latDim; d++) {
      if (blc(d) < 0) {
	trim = ((inc(d) - 1 - blc(d))/inc(d));
	cursorBlc(d) = trim;
	blc(d) += trim*inc(d);
      }
      if (trc(d) > latticeTrc(d)) {
	trim = ((inc(d) - 1 + trc(d) - latticeTrc(d))/inc(d));
	cursorTrc(d) -= trim;
	trc(d) -= trim*inc(d);
      }
    }
    extractShape = (trc - blc)/inc + 1;
    Array<T> allDims(theCurPtr->reform(fullShape, IPosition(latDim,0)));
    Array<T> subArr(allDims(cursorBlc, cursorTrc));
    theData.getSlice(subArr, blc, extractShape, inc, False); 
  }
};

template<class T> Bool RO_PagedArrIter<T>::
allocateCursor() {
  IPosition realShape(theNavPtr->cursorShape().nonDegenerate());
  switch (realShape.nelements()) {
  case 0:
    // This seems to work but I bet there is a problem lurking here.
    theCurPtr = new Array<T>(theNavPtr->cursorShape());
    break;
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
  else
    return True;
};

template<class T> void RO_PagedArrIter<T>::
setup_tile_cache() {
  IPosition axisPath;
  LatticeStepper *stepper = theNavPtr->castToStepper();

  if (stepper != 0) {
    axisPath = stepper->axisPath();
    theData.setCacheSizeFromPath(theNavPtr->cursorShape(), theNavPtr->blc(), 
				 theNavPtr->trc()-theNavPtr->blc()+1, axisPath);
  }
  else {
    TiledStepper * tilerPtr = theNavPtr->castToTiler();
    if (tilerPtr != 0){
      IPosition cursorShape = tilerPtr->cursorShape();
      Int whichLongest = 0;
      for (uInt i=1; i < cursorShape.nelements(); i++)
	if (cursorShape(i) > cursorShape(whichLongest))
	  whichLongest = i;
      axisPath = IPosition(1,whichLongest);
      theData.setCacheSizeFromPath(cursorShape, tilerPtr->blc(), 
				   tilerPtr->blc() + tilerPtr->tileShape(),
				   axisPath);
    }
    else {
      // Because the current stepper is not a LatticeStepper or TiledStepper
      // assume that the
      // longest axis is the fastest moving one.
      IPosition shape = theData.shape();
      Int whichLongest = 0;
      for (uInt i=1; i<shape.nelements(); i++)
	if (shape(i) > shape(whichLongest))
	  whichLongest = i;
      axisPath = IPosition(1,whichLongest);
      theData.setCacheSizeFromPath(theNavPtr->cursorShape(), theNavPtr->blc(), 
				   theNavPtr->trc()-theNavPtr->blc()+1, axisPath);
    }
  }
};

// +++++++++++++++++++++++ PagedArrIter ++++++++++++++++++++++++++++++++

template<class T> PagedArrIter<T>::
PagedArrIter(PagedArray<T> & data, const LatticeNavigator & nav)
  :theData(data),
   theNavPtr(nav.clone())
{
  AlwaysAssert(allocateCursor() == True, AipsError);
  setup_tile_cache();
  theData.getSlice(*theCurPtr, theNavPtr->position(),
		   theNavPtr->cursorShape(), theNavPtr->increment(), True);
  AlwaysAssert(ok() == True, AipsError);
};

template<class T> PagedArrIter<T>::
PagedArrIter(PagedArray<T> & data, const IPosition & curShape)
  :theData(data),
   theNavPtr(new LatticeStepper(data.shape(), curShape))
{
  AlwaysAssert(allocateCursor() == True, AipsError);
  setup_tile_cache();
  theData.getSlice(*theCurPtr, theNavPtr->position(),
		   theNavPtr->cursorShape(), theNavPtr->increment(), True);
  AlwaysAssert(ok() == True, AipsError);
};

template<class T> PagedArrIter<T>::
PagedArrIter(const PagedArrIter<T> & other)
  :theData(other.theData),
   theNavPtr(other.theNavPtr->clone())
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
    if (theCurPtr != 0) 
      delete theCurPtr;
    theData = other.theData;
    theNavPtr = other.theNavPtr->clone();
    AlwaysAssert(allocateCursor() == True, AipsError);
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
  Bool retval = theNavPtr->operator++();
  cursorUpdate();
  return retval;
};

template<class T> Bool PagedArrIter<T>::
operator--() {
  return this->operator--(0);
};

template<class T> Bool PagedArrIter<T>::
operator--(Int) {
  cursorWrite();
  Bool retval = theNavPtr->operator--();
  cursorUpdate();
  DebugAssert(ok() == True, AipsError);
  return retval;
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
  if(theCurPtr->ndim() != 1)
    throw(AipsError("PagedArrIter<T>::vectorCursor()"
		    " - check the cursor shape is 1-dimensional"));
  return *(Vector<T> *) theCurPtr;
};

template<class T> Matrix<T> & PagedArrIter<T>::
matrixCursor() {
  DebugAssert(ok() == True, AipsError);
  if(theCurPtr->ndim() != 2)
    throw(AipsError("PagedArrIter<T>::matrixCursor()"
		    " check the cursor shape is 2-dimensional"));
  return *(Matrix<T> *) theCurPtr;
};

template<class T> Cube<T> & PagedArrIter<T>::
cubeCursor() {
  DebugAssert(ok() == True, AipsError);
  if(theCurPtr->ndim() != 3)
    throw(AipsError("PagedArrIter<T>::cubeCursor()"
		    " check the cursor shape is 3-dimensional"));
  return *(Cube<T> *) theCurPtr;
};

template<class T> Array<T> & PagedArrIter<T>::
cursor() {
  DebugAssert(ok() == True, AipsError);
  return *theCurPtr;
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
    logErr << LogIO::SEVERE << "Cursor internals are inconsistant" 
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
    logErr << LogIO::SEVERE << "Navigator internals are inconsistant" 
	   << LogIO::POST;
    return False;
  }
  // Check the Navigator and Lattice are the same shape
  if (!(theNavPtr->latticeShape()).isEqual(theData.shape())) {
    LogIO logErr(LogOrigin("PagedArrIter<T>", "ok()"));
    logErr << LogIO::SEVERE 
	   << "Navigator Lattice and Data Lattice are different shapes"
	   << LogIO::POST;
     return False;
  }
  // Check the Navigator cursor and cached Array are the same shape
  // There is a special case if the cursor has only one element
  if ((theCurPtr->ndim() == 1) && 
      (theCurPtr->shape()(0) == 1)) {
    if (theNavPtr->cursorShape().product() != 1) {
      LogIO logErr(LogOrigin("PagedArrIter<T>", "ok()"));
      logErr << LogIO::SEVERE 
	     << "Navigator cursor and Data cursor are not both unit shapes" 
	     << LogIO::POST;
      return False;
    }
  }
  else
    if (!(theNavPtr->cursorShape().nonDegenerate())
	.isEqual(theCurPtr->shape())) {
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
  IPosition fullShape(theNavPtr->cursorShape());
  uInt fullDim(fullShape.nelements());

  if (theNavPtr->hangOver() == False) {
    // putSlice requires a same dimensional sourceBuffer as the underlying
    // Lattice so check if the cursor shape has had degenerate axes
    // stripped.
    if (theCurPtr->ndim() == fullDim)
      theData.putSlice(*theCurPtr, theNavPtr->blc(), theNavPtr->increment());
    else {
      // make a temporary Array with degenerate axes and 
      // have it reference the data in the actual cursor.
      theData.putSlice(theCurPtr->reform(fullShape, IPosition(fullDim,0)), 
		       theNavPtr->position(), theNavPtr->increment());
    }
  }
  else {
    // Find the appropriate region and just put that bit into the Lattice
    const IPosition latticeTrc(theNavPtr->latticeShape() - 1);
    const IPosition fullShape(theNavPtr->cursorShape());
    const uInt latDim = fullShape.nelements();
    IPosition cursorBlc(latDim, 0);
    IPosition cursorTrc(fullShape - 1);
    IPosition blc(theNavPtr->position());
    IPosition trc(theNavPtr->endPosition());
    const IPosition inc(theNavPtr->increment());

    Int trim;
    for (uInt d = 0; d < latDim; d++) {
      if (blc(d) < 0) {
	trim = ((inc(d) - 1 - blc(d))/inc(d));
	cursorBlc(d) = trim;
	blc(d) += trim*inc(d);
      }
      if (trc(d) > latticeTrc(d)) {
	trim = ((inc(d) - 1 + trc(d) - latticeTrc(d))/inc(d));
	cursorTrc(d) -= trim;
      }
    }
    Array<T> allDims(theCurPtr->reform(fullShape, IPosition(latDim,0)));
    Array<T> subArr(allDims(cursorBlc, cursorTrc));
    theData.putSlice(subArr, blc, inc); 
  }
};

template<class T> Bool PagedArrIter<T>::
allocateCursor() {
  IPosition realShape(theNavPtr->cursorShape().nonDegenerate());
  switch (realShape.nelements()) {
  case 0:
    // This seems to work but I bet there is a problem lurking here.
    theCurPtr = new Array<T>(theNavPtr->cursorShape());
    break;
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
  else
    return True;
};

template<class T> void PagedArrIter<T>::
cursorUpdate() {
  DebugAssert(ok() == True, AipsError);
  // Check if the cursor shape has changed.
  {
    IPosition newShape(theCurPtr->shape());
    IPosition oldShape(theNavPtr->cursorShape().nonDegenerate());
    if (newShape.nelements() != oldShape.nelements()){
      delete theCurPtr;
      allocateCursor();
    }
    else if (oldShape != newShape)
      theCurPtr->resize(newShape);
  }
  if (theNavPtr->hangOver() == False)
    theData.getSlice(*theCurPtr, theNavPtr->position(),
		     theNavPtr->cursorShape(), theNavPtr->increment(), True); 
  else {
    // Here I set the entire cursor to zero.
    // (the alternative is to set the cursor to an undefined value 
    //  using ValType.h))
    T overHangVal;
    defaultValue(overHangVal); 
    *theCurPtr = overHangVal;
    // and then fill in the appropriate region with the bit that does not
    // overhang.
    const IPosition latticeTrc(theNavPtr->latticeShape() - 1);
    const IPosition fullShape(theNavPtr->cursorShape());
    const uInt latDim = fullShape.nelements();
    IPosition cursorBlc(latDim, 0);
    IPosition cursorTrc(fullShape - 1);
    IPosition blc(theNavPtr->position());
    IPosition trc(theNavPtr->endPosition());
    const IPosition inc(theNavPtr->increment());
    IPosition extractShape(latDim,0);

    Int trim;
    for (uInt d = 0; d < latDim; d++) {
      if (blc(d) < 0) {
	trim = ((inc(d) - 1 - blc(d))/inc(d));
	cursorBlc(d) = trim;
	blc(d) += trim*inc(d);
      }
      if (trc(d) > latticeTrc(d)) {
	trim = ((inc(d) - 1 + trc(d) - latticeTrc(d))/inc(d));
	cursorTrc(d) -= trim;
	trc(d) -= trim*inc(d);
      }
    }
    extractShape = (trc - blc)/inc + 1;
    Array<T> allDims(theCurPtr->reform(fullShape, IPosition(latDim,0)));
    Array<T> subArr(allDims(cursorBlc, cursorTrc));
    theData.getSlice(subArr, blc, extractShape, inc, False); 
  }
};

template<class T> void PagedArrIter<T>::
setup_tile_cache() {
  IPosition axisPath;
  LatticeStepper *stepper = theNavPtr->castToStepper();

  if (stepper != 0) {
    axisPath = stepper->axisPath();
    theData.setCacheSizeFromPath(theNavPtr->cursorShape(), theNavPtr->blc(), 
				 theNavPtr->trc()-theNavPtr->blc()+1, axisPath);
  }
  else {
    TiledStepper * tilerPtr = theNavPtr->castToTiler();
    if (tilerPtr != 0){
      IPosition cursorShape = tilerPtr->cursorShape();
      Int whichLongest = 0;
      for (uInt i=1; i < cursorShape.nelements(); i++)
	if (cursorShape(i) > cursorShape(whichLongest))
	  whichLongest = i;
      axisPath = IPosition(1,whichLongest);
      theData.setCacheSizeFromPath(cursorShape, tilerPtr->blc(), 
				   tilerPtr->blc() + tilerPtr->tileShape(),
				   axisPath);
    }
    else {
      // Because the current stepper is not a LatticeStepper or TiledStepper
      // assume that the
      // longest axis is the fastest moving one.
      IPosition shape = theData.shape();
      Int whichLongest = 0;
      for (uInt i=1; i<shape.nelements(); i++)
	if (shape(i) > shape(whichLongest))
	  whichLongest = i;
      axisPath = IPosition(1,whichLongest);
      theData.setCacheSizeFromPath(theNavPtr->cursorShape(), theNavPtr->blc(), 
				   theNavPtr->trc()-theNavPtr->blc()+1, axisPath);
    }
  }
};
