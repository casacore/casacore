//# TiledStepper.cc: defines TiledStepper class
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

#include <trial/Lattices/TiledStepper.h>
#include <aips/Exceptions/Error.h>
#include <aips/Logging/LogIO.h>
#include <aips/Logging/LogOrigin.h>
#include <aips/Utilities/Assert.h>

TiledStepper::TiledStepper(const IPosition & latticeShape, 
			   const IPosition & tileShape,
			   const uInt axis)
  :theIndexer(latticeShape),
   theTiler(latticeShape),
   theCursorPos(latticeShape.nelements(),0),
   theCursorShape(latticeShape.nelements(),1),
   theAxisPath(latticeShape.nelements(),0),
   theNsteps(0),
   theEnd(False),
   theStart(True)
{
  const uInt latDim = latticeShape.nelements();
  AlwaysAssert(latDim > 0, AipsError);
  AlwaysAssert(tileShape.nelements() == latDim, AipsError);
  AlwaysAssert(axis < latDim, AipsError);
  const uInt cursorLength = latticeShape(axis);
  IPosition tiledStep(tileShape);
  tiledStep(axis) = cursorLength;
  theCursorShape(axis) = cursorLength;
  const IPosition latOrigin(latDim,0);
  theTiler.subSection(latOrigin, latticeShape - 1, tiledStep);
  theIndexer.subSection(latOrigin, tiledStep - 1, IPosition(latDim, 1));
  for (Int i = 0; i < axis; i++)
    theAxisPath(i) = i;
  for (Int j = axis+1; j < latDim; j++)
    theAxisPath(j-1) = j;
  theAxisPath(latDim-1) = axis;
  DebugAssert(ok() == True, AipsError);
};

// the copy constructor which uses copy semantics.
TiledStepper::TiledStepper(const TiledStepper & other)
  :theIndexer(other.theIndexer),
   theTiler(other.theTiler),
   theCursorPos(other.theCursorPos),
   theCursorShape(other.theCursorShape),
   theAxisPath(other.theAxisPath),
   theNsteps(other.theNsteps),
   theEnd(other.theEnd),
   theStart(other.theStart)
{
  DebugAssert(ok() == True, AipsError);
};

TiledStepper::~TiledStepper() {
  // does nothing
};

TiledStepper & TiledStepper::operator=(const TiledStepper & other) {
  if (this != &other) { 
    theIndexer = other.theIndexer;
    theTiler = other.theTiler;
    theCursorPos = other.theCursorPos;
    theCursorShape = other.theCursorShape;
    theAxisPath = other.theAxisPath;
    theNsteps = other.theNsteps;
    theEnd = other.theEnd;
    theStart = other.theStart;
  }
  DebugAssert(ok() == True, AipsError);
  return *this;
};

Bool TiledStepper::operator++(Int) {
  DebugAssert(ok() == True, AipsError);
  if (theEnd) return False;
  IPosition currentPos(theCursorPos);
  Bool successful = theIndexer.tiledCursorMove(True, theCursorPos, 
					       theCursorShape, theAxisPath);
  while (successful && 
	 !(theIndexer.absolutePosition(theCursorPos)<theIndexer.fullShape())){
    successful = theIndexer.tiledCursorMove(True, theCursorPos, 
					    theCursorShape, theAxisPath);
  }
  if (!successful) { // Move to the next set of tiles
    IPosition tileOrigin(theIndexer.offset()/theTiler.increment());
    successful = theTiler.tiledCursorMove(True, tileOrigin, 
					  theCursorShape, theAxisPath);
    if (successful) {
      tileOrigin =  theTiler.absolutePosition(tileOrigin);
      theIndexer.fullSize();
      theIndexer.subSection(tileOrigin, tileOrigin+theTiler.increment() - 1,
			    IPosition(theTiler.ndim(), 1));
      theCursorPos = 0;
    }
  }
  if (successful && 
      theIndexer.absolutePosition(theCursorPos) < theIndexer.fullShape()) {
    theStart = False;// by definition when incrementing
    theNsteps++;     // increment the counter since we have moved
  }
  else {
    theEnd = True;
    theCursorPos = currentPos;
  }
  DebugAssert(ok() == True, AipsError);
  return successful;
};

Bool TiledStepper::operator++() {
  return operator++(0);
};

Bool TiledStepper::operator--(Int) {
  DebugAssert(ok() == True, AipsError);
  if (theStart) return False;
  Bool successful = theIndexer.tiledCursorMove(False, theCursorPos, 
					       theCursorShape, theAxisPath);
  if (!successful) { // Move to the next set of tiles
    IPosition tileStep(theTiler.increment());
    IPosition tileOrigin(theIndexer.offset()/tileStep);
    successful = theTiler.tiledCursorMove(False, tileOrigin, 
					  theCursorShape, theAxisPath);
    if (successful) {
      tileOrigin =  theTiler.absolutePosition(tileOrigin);
      theIndexer.fullSize();
      theIndexer.subSection(tileOrigin, tileOrigin+tileStep-1,
			    IPosition(theTiler.ndim(), 1));
      theCursorPos = tileStep-theCursorShape;
    }
  }
  if (successful) {
    theEnd = False;// by definition when decrementing
    theNsteps++;     // increment the counter since we have moved
    if (theCursorPos.isEqual(theIndexer.offset()) &&
	theCursorPos.isEqual(IPosition(theIndexer.ndim(), 0)))
      theStart = True;
  }
  else
    theStart = True;
  DebugAssert(ok() == True, AipsError);
  return successful;
};

Bool TiledStepper::operator--() {
  return operator--(0);
};

void TiledStepper::reset() {
  theCursorPos = 0;
  theNsteps = 0;
  theEnd = False;
  theStart = True;
  theIndexer.fullSize();
  theIndexer.subSection(blc(), tileShape()-1, increment());
  DebugAssert(ok() == True, AipsError);
};

Bool TiledStepper::atStart() const {
  DebugAssert(ok() == True, AipsError);
  return theStart;
};

Bool TiledStepper::atEnd() const {
  DebugAssert(ok() == True, AipsError);
  return theEnd;
};

uInt TiledStepper::nsteps() const {
  DebugAssert(ok() == True, AipsError);
  return theNsteps;
};

IPosition TiledStepper::position() const {
  DebugAssert(ok() == True, AipsError);
  return theIndexer.absolutePosition(theCursorPos);
};

IPosition TiledStepper::endPosition() const {
  DebugAssert(ok() == True, AipsError);
  return theIndexer.absolutePosition(theCursorPos + theCursorShape - 1);
};

IPosition TiledStepper::latticeShape() const {
  DebugAssert(ok() == True, AipsError);
  return theIndexer.fullShape();
};

IPosition TiledStepper::cursorShape() const {
  DebugAssert(ok() == True, AipsError);
  return theCursorShape;
};

IPosition TiledStepper::tileShape() const {
  DebugAssert(ok() == True, AipsError);
  return theTiler.increment();
};

Bool TiledStepper::hangOver() const {
  return False;
};

IPosition TiledStepper::blc() const {
  DebugAssert(ok() == True, AipsError);
  return theTiler.offset();
};

IPosition TiledStepper::trc() const {
  DebugAssert(ok() == True, AipsError);
  return theTiler.fullShape() - 1;
};

IPosition TiledStepper::increment() const {
  DebugAssert(ok() == True, AipsError);
  return theIndexer.increment();
};

LatticeNavigator * TiledStepper::clone() const {
  return new TiledStepper(*this);
};

static LogIO logErr(LogOrigin("TiledStepper", "ok()"));

Bool TiledStepper::ok() const {
//   const uInt latticeDim = theIndexer.ndim();
//   // Check the cursor shape is OK
//   if (theCursorShape.nelements() != latticeDim) {
//     logErr << LogIO::SEVERE << "cursor shape"
// 	   << " (=" << theCursorShape << ")"
// 	   << " has wrong number of dimensions"
// 	   << " (ie. not" << latticeDim << ")" << endl;
//      return False;
//   }
//   for (uInt i=0; i < latticeDim; i++) 
//     // the cursor shape must be <= the corresponding lattice axes AND
//     // a cursor shape with an axis of length zero makes no sense
//     if (theCursorShape(i) > theIndexer.shape(i) || theCursorShape(i) <= 0) {
//       logErr << LogIO::SEVERE << "cursor shape"
// 	     << " (=" << theCursorShape << ")"
// 	     << " is too big or small for lattice shape"
// 	     << " (=" << theIndexer.shape() << ")" << LogIO::POST;
//       return False;
//     }
//   // Check the cursor position is OK
//   if (theCursorPos.nelements() != latticeDim) {
//     logErr << LogIO::SEVERE << "cursor position"
// 	   << " (=" << theCursorPos << ")"
// 	   << " has wrong number of dimensions"
// 	   << " (ie. not" << latticeDim << ")" << LogIO::POST;
//      return False;
//   }
  
//   // cursor position or its "far corner" must be inside the (sub)-Lattice
//   if (!(theIndexer.isInside(theCursorPos) ||
// 	theIndexer.isInside(theCursorPos+theCursorShape-1))){
//     logErr << LogIO::SEVERE << "cursor beginning"
// 	   << " (=" << theCursorPos << ")"
// 	   << " or end"
// 	   << " (=" << theCursorPos + theCursorShape - 1 << ")"
// 	   << " is entirely outside the lattice shape"
// 	   << " (=" << theIndexer.shape() << ")" << LogIO::POST;
//     return False;
//   }

//   // check the Axis Path is OK
//   if(theAxisPath.nelements() != latticeDim){
//     logErr << LogIO::SEVERE << "axis path"
// 	   << " (=" << theAxisPath << ")"
// 	   << " has wrong number of dimensions"
// 	   << " (ie. not " << latticeDim << ")" << LogIO::POST;
//     return False;
//   }
//   // each theAxisPath value must be a lattice axis number, 0..n-1
//   for (uInt n=0; n < latticeDim; n++)
//     if (theAxisPath(n) >= latticeDim){
//       logErr << LogIO::SEVERE << "axis path"
// 	     << " (=" << theAxisPath << ")"
// 	     << " has elements bigger than the lattice dim -1 "
// 	     << " (ie. " << latticeDim - 1 << ")" << LogIO::POST;
//       return False;
//     }

//   // each theAxisPath value must be unique
//   for (uInt k=0; k < (latticeDim - 1); k++)
//     for (uInt j=k+1; j < latticeDim; j++)
//       if (theAxisPath(k) == theAxisPath(j)) {
// 	logErr << LogIO::SEVERE << "axis path"
// 	       << " (=" << theAxisPath << ")"
// 	       << " does not have unique elements " << LogIO::POST;
// 	return False;
//       }
//   // Check the LatticeIndexer is OK
//   if (theIndexer.ok() == False) {
//     logErr << LogIO::SEVERE << "LatticeIndexer"
//  	   << " thinks things are bad" << LogIO::POST;
//     return False;
//   }
  
//   // Check if theStart flag is correct
//   if ((theCursorPos.isEqual(IPosition(latticeDim,0)) && (theStart == False)) ||
//       (!theCursorPos.isEqual(IPosition(latticeDim,0)) && (theStart == True))){
//     logErr << LogIO::SEVERE << "cursor position"
// 	   << " (=" << theCursorPos << ")"
// 	   << " is inconsistant with theStart flag"
// 	   << " (theStart = ";
//     if (theStart == True)
//       logErr << "True";
//     else
//       logErr << "False";
//     cout << ")" << LogIO::POST;
//     return False;
//   }
  
//   // Otherwise it has passed all the tests
  return True;
};

TiledStepper * TiledStepper::castToTiler() {
  return this;
};

const TiledStepper * TiledStepper::castToConstTiler() const {
  return this;
};

// Local Variables: 
// compile-command: "gmake OPTLIB=1 TiledStepper"
// End: 
