//# LatticeStepper.cc: defines LatticeStepper class
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

#include <trial/Lattices/LatticeStepper.h>
#include <aips/Exceptions/Error.h>
#include <aips/Logging/LogIO.h>
#include <aips/Logging/LogOrigin.h>
#include <aips/Utilities/Assert.h>

// The first argument is the shape of the Lattice to be iterated and the
// second argument is the shape of the cursor. The cursor will increment
// initially along first axis, then the second and then the third
// (ie. axisPath = IPosition(ndim,0,1,2,...))
LatticeStepper::LatticeStepper(const IPosition & latticeShape,
			       const IPosition & cursorShape)
  :theIndexer(latticeShape),
   theCursorPos(latticeShape.nelements(),0),
   theCursorShape(cursorShape),
   theAxisPath(latticeShape.nelements()),
   theNsteps(0),
   theEnd(False),
   theStart(True),
   theNiceFit(False),
   theHangover(False)
{
  uInt ndim = theIndexer.ndim();
  for (uInt i=0; i < ndim; i++)
    theAxisPath(i) = i;
  padCursor();
  theNiceFit = niceFit();
  DebugAssert(ok() == True, AipsError);
};

// Same as the above constructor except that the axis path is explicitly
// specified. The axis path is described in the synopsis. 
LatticeStepper::LatticeStepper(const IPosition & latticeShape,
			       const IPosition & cursorShape,
			       const IPosition & axisPath)
  :theIndexer(latticeShape),
   theCursorPos(latticeShape.nelements(), 0),
   theCursorShape(cursorShape),
   theAxisPath(axisPath),
   theNsteps(0),
   theEnd(False),
   theStart(True),
   theNiceFit(False),
   theHangover(False)
{
   padCursor();
   theNiceFit = niceFit();
   DebugAssert(ok() == True, AipsError);
};

// the copy constructor which uses copy semantics.
LatticeStepper::LatticeStepper(const LatticeStepper & other)
  :theIndexer(other.theIndexer),
   theCursorPos(other.theCursorPos),
   theCursorShape(other.theCursorShape),
   theAxisPath(other.theAxisPath),
   theNsteps(other.theNsteps),
   theEnd(other.theEnd),
   theStart(other.theStart),
   theNiceFit(other.theNiceFit),
   theHangover(other.theHangover)
{
  DebugAssert(ok() == True, AipsError);
};

// destructor (does nothing)
LatticeStepper::~LatticeStepper()
{
  // does nothing
};

// The assignment operator which uses copy semantics.
LatticeStepper & LatticeStepper::operator=(const LatticeStepper & other)
{
  if (this != &other) { 
    theIndexer = other.theIndexer;
    theCursorPos = other.theCursorPos;
    theCursorShape = other.theCursorShape;
    theAxisPath = other.theAxisPath;
    theNsteps = other.theNsteps;
    theEnd = other.theEnd;
    theStart = other.theStart;
    theNiceFit = other.theNiceFit;
    theHangover = other.theHangover;
  }
  DebugAssert(ok() == True, AipsError);
  return *this;
};

// Increment operator (postfix) - move the cursor forward one
// step. Returns True if the cursor was moved.
Bool LatticeStepper::operator++(Int)
{
  DebugAssert(ok() == True, AipsError);
  if (theEnd) return False;
  Bool successful = theIndexer.tiledCursorMove(True, theCursorPos, 
					       theCursorShape, theAxisPath);
  if (successful) {
    // theStart = false by definition when incrementing
    theStart = False;
    // increment the counter since we have moved
    theNsteps++;
    // test for hang over since cursor has moved.
    if (theNiceFit == False){
      const IPosition curPos(theCursorPos);
      const IPosition curEndPos(theCursorPos+theCursorShape-1);
      const IPosition latShape(theIndexer.shape());
      const uInt ndim = theIndexer.ndim();
      uInt i = 0;
      while (i < ndim && curEndPos(i) < latShape(i) && curPos(i) >= 0)
	i++;
      if (i == ndim)
	theHangover = False;
      else
	theHangover = True;
    }
  }
  else
    theEnd = True;
  DebugAssert(ok() == True, AipsError);
  return successful;
};

// Increment operator (prefix) - move the cursor forward one
// step. Identical to the postfix operator above.
Bool LatticeStepper::operator++()
{
  return operator++(0);
};

// Decrement operator (postfix) - move the cursor backwards one
// step. Returns True if the cursor was moved.
Bool LatticeStepper::operator--(Int)
{
  DebugAssert(ok() == True, AipsError);
  if (theStart) return False;
  Bool successful = theIndexer.tiledCursorMove(False, theCursorPos, 
					       theCursorShape, theAxisPath);
  if (successful) {
    // theEnd = false by definition when decrementing
    theEnd = False; 
    // increment the counter since we have moved
    theNsteps++;
    // test to see if we are at the beginning
    const IPosition curPos(theCursorPos);
    const uInt ndim = theIndexer.ndim();
    uInt i = 0;
    while (i < ndim && curPos(i) == 0)
      i++;
    if (i == ndim)
      theStart = True;
    else
      theStart = False;
    // test for hang over since cursor has moved
    if (theNiceFit == False){
      const IPosition curEndPos(theCursorPos+theCursorShape);
      const IPosition latShape(theIndexer.shape());
      i = 0;
      while (i < ndim && curPos(i) >= 0 && curEndPos(i) < latShape(i))
	i++;
      if (i == ndim)
	theHangover = False;
      else
	theHangover = True;
    }
  }
  else
    theStart = True;
  DebugAssert(ok() == True, AipsError);
  return successful;
};

// Decrement operator (prefix) - move the cursor backwards one
// step. Identical to the postfix operator above.
Bool LatticeStepper::operator--()
{
  return operator--(0);
};

// Function to move the cursor to the beginning of the (sub)-Lattice. Also
// resets the number of steps (<src>nsteps</src> function) to zero. 
void LatticeStepper::reset()
{
  theCursorPos = 0;
  theNsteps = 0;
  theEnd = False;
  theStart = True;
  theHangover = False;
  DebugAssert(ok() == True, AipsError);
};

// Function which returns "True" if the cursor is at the beginning of the
// lattice, otherwise, returns "False"
Bool LatticeStepper::atStart() const
{
  DebugAssert(ok() == True, AipsError);
  return theStart;
};

// Function which returns "True" if an attempt has been made to move the
// cursor beyond the end of the Lattice.
Bool LatticeStepper::atEnd() const
{
  DebugAssert(ok() == True, AipsError);
  return theEnd;
};

// Function to return the number of steps (increments & decrements) taken
// since construction (or since last reset).  This is a running count of
// all cursor movement (operator++ or operator--)
uInt LatticeStepper::nsteps() const
{
  DebugAssert(ok() == True, AipsError);
  return theNsteps;
};

// Function which returns the current position of the beginning of the 
// cursor relative to the main Lattice
IPosition LatticeStepper::position() const
{
  DebugAssert(ok() == True, AipsError);
  return theIndexer.absolutePosition(theCursorPos);
};

// Function which returns the current position of the beginning of the 
// cursor relative to the sub-Lattice
IPosition LatticeStepper::relativePosition() const
{
  DebugAssert(ok() == True, AipsError);
  return theCursorPos;
};

// Function which returns the current position of the end of the cursor
// relative to the main Lattice.
IPosition LatticeStepper::endPosition() const
{
  DebugAssert(ok() == True, AipsError);
  return theIndexer.absolutePosition(theCursorPos + theCursorShape - 1);
};

// Function which returns the current position of the end of the cursor
// relative to the sub Lattice.
IPosition LatticeStepper::relativeEndPosition() const
{
  DebugAssert(ok() == True, AipsError);
  return theCursorPos + theCursorShape - 1;
};

// Function which returns the shape of the main Lattice being iterated through.
IPosition LatticeStepper::latticeShape() const
{
  DebugAssert(ok() == True, AipsError);
  return theIndexer.fullShape();
};

// Function which returns the shape of the sub-Lattice being iterated through.
IPosition LatticeStepper::subLatticeShape() const
{
  DebugAssert(ok() == True, AipsError);
  return theIndexer.shape();
};

// Function to change the cursor shape to a new one. This always resets
// the cursor to the beginning of the Lattice (and resets the number of
// steps to zero)
void LatticeStepper::setCursorShape(const IPosition & cursorShape){
  if (cursorShape.nelements() != theIndexer.ndim()){
    theCursorShape.resize(cursorShape.nelements());
    theCursorShape = cursorShape;
    padCursor();
  }
  else
    theCursorShape = cursorShape;
  DebugAssert(ok() == True, AipsError);
}

// Function which returns the shape of the cursor. This always includes
// all axes (ie. it includes degenerates axes)
IPosition LatticeStepper::cursorShape() const 
{
  DebugAssert(ok() == True, AipsError);
  return theCursorShape;
};

// Function which returns "True" if the increment/decrement operators have
// moved the cursor position such that the cursor beginning or end is
// hanging over the edge of the Lattice.
Bool LatticeStepper::hangOver() const
{
  DebugAssert(ok() == True, AipsError);
  return theHangover;
};

// Function to specify a "section" of the Lattice to Navigate over. A
// section is defined in terms of the Bottom Left Corner (blc), Top Right
// Corner (trc), and step size (inc), on ALL of its axes, including
// degenerate axes.
void LatticeStepper::subSection(const IPosition & blc, const IPosition & trc, 
				const IPosition & inc)
{
  theIndexer.fullSize();
  theIndexer.subSection(blc, trc, inc);
  reset();
  theNiceFit = niceFit();
  DebugAssert(ok() == True, AipsError);
};

// Function to specify a "section" of the Lattice to Navigate over. The step
// increment is assumed to be one. 
void LatticeStepper::subSection(const IPosition & blc, const IPosition & trc) {
  subSection(blc, trc, IPosition(theIndexer.ndim(), 1));
};

// Return the bottom left hand corner of the current sub-Lattice. If no
// sub-Lattice has been defined return blc=0
IPosition LatticeStepper::blc() const{
  DebugAssert(ok() == True, AipsError);
  return theIndexer.offset();
};

// Return the top right hand corner of the current sub-Lattice. If no
// sub-Lattice has been defined return trc=latticeShape-1
IPosition LatticeStepper::trc() const{
  DebugAssert(ok() == True, AipsError);
  return theIndexer.absolutePosition(theIndexer.shape()-1);
};

// Return the step increment between the current sub-Lattice and the main
// Lattice. If no sub-Lattice has been defined return inc=1
IPosition LatticeStepper::increment() const {
  DebugAssert(ok() == True, AipsError);
  return theIndexer.increment();
};

// Return the axis path.
const IPosition & LatticeStepper::axisPath() const
{
  DebugAssert(ok() == True, AipsError);
  return theAxisPath;
}

// Function which returns a pointer to dynamic memory of an exact copy 
// of this instance.
LatticeNavigator * LatticeStepper::clone() const
{
  return new LatticeStepper(*this);
};

static   LogIO logErr(LogOrigin("LatticeStepper", "ok()"));


// Function which checks the internal data of this class for correct
// dimensionality and consistant values. 
// Returns True if everything is fine otherwise returns False
Bool LatticeStepper::ok() const
{
  const uInt latticeDim = theIndexer.ndim();
  // Check the cursor shape is OK
  if (theCursorShape.nelements() != latticeDim) {
    logErr << LogIO::SEVERE << "cursor shape"
	   << " (=" << theCursorShape << ")"
	   << " has wrong number of dimensions"
	   << " (ie. not" << latticeDim << ")" << endl;
     return False;
  }
  for (uInt i=0; i < latticeDim; i++) 
    // the cursor shape must be <= the corresponding lattice axes AND
    // a cursor shape with an axis of length zero makes no sense
    if (theCursorShape(i) > theIndexer.shape(i) || theCursorShape(i) <= 0) {
      logErr << LogIO::SEVERE << "cursor shape"
	     << " (=" << theCursorShape << ")"
	     << " is too big or small for lattice shape"
	     << " (=" << theIndexer.shape() << ")" << LogIO::POST;
      return False;
    }
  // Check the cursor position is OK
  if (theCursorPos.nelements() != latticeDim) {
    logErr << LogIO::SEVERE << "cursor position"
	   << " (=" << theCursorPos << ")"
	   << " has wrong number of dimensions"
	   << " (ie. not" << latticeDim << ")" << LogIO::POST;
     return False;
  }
  
  // cursor position or its "far corner" must be inside the (sub)-Lattice
  if (!(theIndexer.isInside(theCursorPos) ||
	theIndexer.isInside(theCursorPos+theCursorShape-1))){
    logErr << LogIO::SEVERE << "cursor beginning"
	   << " (=" << theCursorPos << ")"
	   << " or end"
	   << " (=" << theCursorPos + theCursorShape - 1 << ")"
	   << " is entirely outside the lattice shape"
	   << " (=" << theIndexer.shape() << ")" << LogIO::POST;
    return False;
  }

  // check the Axis Path is OK
  if(theAxisPath.nelements() != latticeDim){
    logErr << LogIO::SEVERE << "axis path"
	   << " (=" << theAxisPath << ")"
	   << " has wrong number of dimensions"
	   << " (ie. not " << latticeDim << ")" << LogIO::POST;
    return False;
  }
  // each theAxisPath value must be a lattice axis number, 0..n-1
  for (uInt n=0; n < latticeDim; n++)
    if (theAxisPath(n) >= latticeDim){
      logErr << LogIO::SEVERE << "axis path"
	     << " (=" << theAxisPath << ")"
	     << " has elements bigger than the lattice dim -1 "
	     << " (ie. " << latticeDim - 1 << ")" << LogIO::POST;
      return False;
    }

  // each theAxisPath value must be unique
  for (uInt k=0; k < (latticeDim - 1); k++)
    for (uInt j=k+1; j < latticeDim; j++)
      if (theAxisPath(k) == theAxisPath(j)) {
	logErr << LogIO::SEVERE << "axis path"
	       << " (=" << theAxisPath << ")"
	       << " does not have unique elements " << LogIO::POST;
	return False;
      }
  // Check the LatticeIndexer is OK
  if (theIndexer.ok() == False) {
    logErr << LogIO::SEVERE << "LatticeIndexer"
 	   << " thinks things are bad" << LogIO::POST;
    return False;
  }
  
  // Check if theStart flag is correct
  if ((theCursorPos.isEqual(IPosition(latticeDim,0)) && (theStart == False)) ||
      (!theCursorPos.isEqual(IPosition(latticeDim,0)) && (theStart == True))){
    logErr << LogIO::SEVERE << "cursor position"
	   << " (=" << theCursorPos << ")"
	   << " is inconsistant with theStart flag"
	   << " (theStart = ";
    if (theStart == True)
      logErr << "True";
    else
      logErr << "False";
    cout << ")" << LogIO::POST;
    return False;
  }
  
  // Otherwise it has passed all the tests
  return True;
};

// pad the cursor to the right number of dimensions
void LatticeStepper::padCursor()
{
  const uInt latticeDim = theIndexer.ndim();
  // the stepper theCursorShape must not have more axes than the lattice
  const uInt cursorDim = theCursorShape.nelements();
  if (cursorDim != latticeDim)
    if (cursorDim < latticeDim){
      IPosition tempCursor(theCursorShape);
      theCursorShape.resize(latticeDim);
      theCursorShape = 1;
      for (uInt k = 0; k < cursorDim; k++)
	theCursorShape(k) = tempCursor(k);
    }
    else
      throw(AipsError("LatticeStepper::padCursor()"
		      " - Cursor shape has more axes than the lattice."));
};

// check if the cursor shape is an sub-multiple of the Lattice shape
Bool LatticeStepper::niceFit()
{
  const uInt cursorDim = theCursorShape.nelements();
  // Check the case when theCursorShape == 0 as this will cause a floating
  // point error below.
  uInt i = 0;
  for ( ;i < cursorDim; i++)
    if (theCursorShape(i) <= 0)
      throw(AipsError("LatticeStepper::niceFit()"
		      " - Cursor shape is is zero or negative"));

  // Determine if the Lattice shape is a multiple of the cursor shape.
  i = 0;
  while (i < cursorDim && 
	 theIndexer.shape(i)%theCursorShape(i) == 0)
    i++;

  if (i == cursorDim)
    return True;
  else
    return False;
};

LatticeStepper * LatticeStepper::castToStepper()
{
  return this;
}

const LatticeStepper * LatticeStepper::castToConstStepper() const
{
  return this;
}
