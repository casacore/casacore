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

TiledStepper::TiledStepper(const IPosition& latticeShape, 
			   const IPosition& tileShape,
			   const uInt axis)
  :theIndexer(latticeShape),
   theTiler(latticeShape),
   theSubSection(latticeShape),
   theBlc(latticeShape.nelements(), 0),
   theTrc(latticeShape - 1),
   theInc(latticeShape.nelements(), 1),
   theTileShape(tileShape),
   theTilerCursorPos(latticeShape.nelements(), 0),
   theIndexerCursorPos(latticeShape.nelements(), 0),
   theCursorShape(latticeShape.nelements(), 1),
   theAxisPath(latticeShape.nelements(), 0),
   theAxis(axis),
   theNsteps(0),
   theEnd(False),
   theStart(True)
{
  const uInt nrdim = latticeShape.nelements();
  AlwaysAssert(nrdim > 0, AipsError);
  AlwaysAssert(tileShape.nelements() == nrdim, AipsError);
  AlwaysAssert(axis < nrdim, AipsError);
  uInt i;
  for (i=0; i<theAxis; i++) {
    theAxisPath(i) = i;
  }
  for (i=theAxis; i<nrdim-1; i++) {
    theAxisPath(i) = i+1;
  }
  theAxisPath(nrdim-1) = theAxis;
  reset();
  DebugAssert(ok() == True, AipsError);
};

// the copy constructor which uses copy semantics.
TiledStepper::TiledStepper(const TiledStepper & other)
  :theIndexer(other.theIndexer),
   theTiler(other.theTiler),
   theSubSection(other.theSubSection),
   theBlc(other.theBlc),
   theTrc(other.theTrc),
   theInc(other.theInc),
   theTileShape(other.theTileShape),
   theTilerCursorPos(other.theTilerCursorPos),
   theIndexerCursorPos(other.theIndexerCursorPos),
   theCursorShape(other.theCursorShape),
   theAxisPath(other.theAxisPath),
   theAxis(other.theAxis),
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
    theSubSection = other.theSubSection;
    theBlc = other.theBlc;
    theTrc = other.theTrc;
    theInc = other.theInc;
    theTileShape = other.theTileShape;
    theTilerCursorPos = other.theTilerCursorPos;
    theIndexerCursorPos = other.theIndexerCursorPos;
    theCursorShape = other.theCursorShape;
    theAxisPath = other.theAxisPath;
    theAxis = other.theAxis;
    theNsteps = other.theNsteps;
    theEnd = other.theEnd;
    theStart = other.theStart;
  }
  DebugAssert(ok() == True, AipsError);
  return *this;
};

Bool TiledStepper::operator++ (Int)
{
  DebugAssert(ok() == True, AipsError);
  if (theEnd) {
    return False;
  }
  theStart = False;
  theNsteps++;
  IPosition currentPos = theIndexerCursorPos;
  //# Move to the next position in the tile.
  //# If at the end of the tile, move to the next tile.
  if (theIndexer.tiledCursorMove (True, theIndexerCursorPos, 
				  theCursorShape, theAxisPath)) {
    return True;
  }
  //# Move to the next tile.
  //# Set end-status if no more tiles.
  IPosition tilerPos = theTilerCursorPos;
  while (!theEnd) {
    if (! theTiler.tiledCursorMove (True, theTilerCursorPos,
				    theTileShape, theAxisPath)) {
      theEnd = True;
      theIndexerCursorPos = currentPos;
      theTilerCursorPos   = tilerPos;
      return False;
    }
    //# Calculate the boundaries of the tile.
    IPosition tileblc = theTiler.absolutePosition (theTilerCursorPos);
    IPosition tiletrc = tileblc + theTileShape - 1;
//    cout << tileblc << tiletrc << "   ";
    tileblc(theAxis) = theBlc(theAxis);
    tiletrc(theAxis) = theTrc(theAxis);
    Bool empty = False;
    //# Calculate the first and last pixel in the tile taking the
    //# increment into account.
    Int nrdim = tileblc.nelements();
    for (int i=0; i<nrdim; i++) {
      if (i != theAxis) {
	if (tiletrc(i) > theTrc(i)) {
	  tiletrc(i) = theTrc(i);
	}
	if (tileblc(i) <= theBlc(i)) {
	  tileblc(i) = theBlc(i);
	}else{
	  tileblc(i) = (tileblc(i) - theBlc(i) + theInc(i) - 1) / theInc(i)
	               * theInc(i) + theBlc(i);
	}
	tiletrc(i) = (tiletrc(i) - theBlc(i)) / theInc(i)
	             * theInc(i) + theBlc(i);
	//# It is possible that the tile does not have any pixel at all
	//# (e.g. when increment > tileshape).
//	cout << tileblc << tiletrc << endl;
	if (tileblc(i) > tiletrc(i)) {
	  empty = True;
	  break;
	}
      }
    }
    //# When pixels in this tile, set to the first pixel.
    if (!empty) {
      theIndexer.fullSize();
      theIndexer.subSection (tileblc, tiletrc, theInc);
      theIndexerCursorPos = 0;
      return True;
    }
  }
  DebugAssert(ok() == True, AipsError);
  return False;
};

Bool TiledStepper::operator++() {
  return operator++(0);
};

Bool TiledStepper::operator--(Int) {
  DebugAssert(ok() == True, AipsError);
  if (theStart) {
    return False;
  }
  theEnd = False;
  theNsteps++;
  IPosition currentPos = theIndexerCursorPos;
  //# Move to the previous position in the tile.
  //# If at the beginning of the tile, move to the previous tile.
  if (theIndexer.tiledCursorMove (False, theIndexerCursorPos, 
				  theCursorShape, theAxisPath)) {
    return True;
  }
  //# Move to the previous tile.
  //# Set start-status if no more tiles.
  IPosition tilerPos = theTilerCursorPos;
  while (!theStart) {
    if (! theTiler.tiledCursorMove (False, theTilerCursorPos,
				    theTileShape, theAxisPath)) {
      theStart = True;
      theIndexerCursorPos = currentPos;
      theTilerCursorPos   = tilerPos;
      return False;
    }
    //# Calculate the boundaries of the tile.
    IPosition tileblc = theTiler.absolutePosition (theTilerCursorPos);
    IPosition tiletrc = tileblc + theTileShape - 1;
//    cout << tileblc << tiletrc << "   ";
    tileblc(theAxis) = theBlc(theAxis);
    tiletrc(theAxis) = theTrc(theAxis);
    Bool empty = False;
    //# Calculate the first and last pixel in the tile taking the
    //# increment into account.
    Int nrdim = tileblc.nelements();
    for (int i=0; i<nrdim; i++) {
      if (i != theAxis) {
	if (tiletrc(i) > theTrc(i)) {
	  tiletrc(i) = theTrc(i);
	}
	if (tileblc(i) <= theBlc(i)) {
	  tileblc(i) = theBlc(i);
	}else{
	  tileblc(i) = (tileblc(i) - theBlc(i) + theInc(i) - 1) / theInc(i)
	               * theInc(i) + theBlc(i);
	}
	tiletrc(i) = (tiletrc(i) - theBlc(i)) / theInc(i)
	             * theInc(i) + theBlc(i);
	//# It is possible that the tile does not have any pixel at all
	//# (e.g. when increment > tileshape).
//	cout << tileblc << tiletrc << endl;
	if (tileblc(i) > tiletrc(i)) {
	  empty = True;
	  break;
	}
      }
    }
    //# When pixels in this tile, set to the first pixel.
    if (!empty) {
      theIndexer.fullSize();
      theIndexer.subSection (tileblc, tiletrc, theInc);
      theIndexerCursorPos = (tiletrc - tileblc) / theInc;
      theIndexerCursorPos(theAxis) = 0;
      return True;
    }
  }
  DebugAssert(ok() == True, AipsError);
  return False;

/*
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
*/
};

Bool TiledStepper::operator--() {
  return operator--(0);
};

void TiledStepper::reset()
{
  //# Make sure the tiler starts on a tile boundary.
  //# Set theTiler subsection (its increment is always one).
  //# For theTiler we are not interested in the length of theAxis axis,
  //# so make it the tile shape for convenience.
  IPosition tilerBlc = theBlc / theTileShape * theTileShape;
  IPosition tilerTrc = theTrc;
  tilerTrc(theAxis) = tilerBlc(theAxis) + theTileShape(theAxis) - 1;
  theTiler.fullSize();
  theTiler.subSection (tilerBlc, tilerTrc);
  theTilerCursorPos = 0;
  theCursorShape(theAxis) = 1 + (theTrc(theAxis) - theBlc(theAxis))
                            / theInc(theAxis);
  //# Calculate the boundaries of the tile.
  IPosition tileblc = theTiler.absolutePosition (theTilerCursorPos);
  IPosition tiletrc = tileblc + theTileShape - 1;
//  cout << tileblc << tiletrc << "   ";
  tileblc(theAxis) = theBlc(theAxis);
  tiletrc(theAxis) = theTrc(theAxis);
  Bool empty = False;
  //# Calculate the first and last pixel in the tile taking the
  //# increment into account.
  Int nrdim = tileblc.nelements();
  for (int i=0; i<nrdim; i++) {
    if (i != theAxis) {
      if (tiletrc(i) > theTrc(i)) {
	tiletrc(i) = theTrc(i);
      }
      if (tileblc(i) <= theBlc(i)) {
	tileblc(i) = theBlc(i);
      }else{
        tileblc(i) = (tileblc(i) - theBlc(i) + theInc(i) - 1) / theInc(i)
	             * theInc(i) + theBlc(i);
      }
      tiletrc(i) = (tiletrc(i) - theBlc(i)) / theInc(i)
	           * theInc(i) + theBlc(i);
//      cout << tileblc << tiletrc << endl;
    }
  }
  theIndexer.fullSize();
  theIndexer.subSection (tileblc, tiletrc, theInc);
  theIndexerCursorPos = 0;
  theNsteps = 0;
  theEnd = False;
  theStart = True;
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
//  cout << "position = " << theIndexer.absolutePosition(theIndexerCursorPos)
//       << endl;
  return theIndexer.absolutePosition(theIndexerCursorPos);
};

IPosition TiledStepper::endPosition() const {
  DebugAssert(ok() == True, AipsError);
  IPosition last = theIndexerCursorPos;
  last(theAxis) += (theCursorShape(theAxis) - 1) * theInc(theAxis);
  return theIndexer.absolutePosition(last);
};

IPosition TiledStepper::latticeShape() const {
  DebugAssert(ok() == True, AipsError);
  return theSubSection.fullShape();
};

IPosition TiledStepper::subLatticeShape() const {
  DebugAssert(ok() == True, AipsError);
  return theSubSection.shape();
};

IPosition TiledStepper::cursorShape() const {
  DebugAssert(ok() == True, AipsError);
  return theCursorShape;
};

IPosition TiledStepper::cursorAxes() const
{
  DebugAssert(ok() == True, AipsError);
  return IPosition(1, theAxis);
};

IPosition TiledStepper::tileShape() const {
  DebugAssert(ok() == True, AipsError);
  return theTileShape;
};

Bool TiledStepper::hangOver() const {
  return False;
};

// Function to specify a "section" of the Lattice to Navigate over. A
// section is defined in terms of the Bottom Left Corner (blc), Top Right
// Corner (trc), and step size (inc), on ALL of its axes, including
// degenerate axes.
void TiledStepper::subSection (const IPosition& blc, const IPosition& trc, 
			       const IPosition& inc)
{
  theSubSection.subSection (blc, trc, inc);
  theBlc = theSubSection.offset();
  theInc = theSubSection.increment();
  theTrc = theBlc + (theSubSection.shape() - 1) * theInc;
  reset();
};

// Function to specify a "section" of the Lattice to Navigate over. The step
// increment is assumed to be one. 
void TiledStepper::subSection(const IPosition & blc, const IPosition & trc)
{
  subSection(blc, trc, IPosition(theIndexer.ndim(), 1));
};

// Return the bottom left hand corner of the current sub-Lattice. If no
// sub-Lattice has been defined return blc=0
IPosition TiledStepper::blc() const{
  DebugAssert(ok() == True, AipsError);
  return theBlc;
};

// Return the top right hand corner of the current sub-Lattice. If no
// sub-Lattice has been defined return trc=latticeShape-1
IPosition TiledStepper::trc() const{
  DebugAssert(ok() == True, AipsError);
  return theTrc;
};

// Return the step increment between the current sub-Lattice and the main
// Lattice. If no sub-Lattice has been defined return inc=1
IPosition TiledStepper::increment() const {
  DebugAssert(ok() == True, AipsError);
  return theInc;
};

const IPosition & TiledStepper::axisPath() const
{
  DebugAssert(ok() == True, AipsError);
  return theAxisPath;
}

LatticeNavigator * TiledStepper::clone() const
{
  DebugAssert(ok() == True, AipsError);
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
