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

LatticeStepper::LatticeStepper(const IPosition & latticeShape,
			       const IPosition & cursorShape,
			       const uInt hangOverPolicy)
  :theIndexer(latticeShape),
   theCursorShape(latticeShape.nelements()),
   theAxisPath(latticeShape.nelements()),
   theCursorPos(latticeShape.nelements(),0),
   theNsteps(0),
   theEnd(False),
   theStart(True),
   theNiceFit(False),
   theHangover(False),
   thePolicy(hangOverPolicy)
{
  uInt ndim = theIndexer.ndim();
  for (uInt i=0; i < ndim; i++) {
    theAxisPath(i) = i;
  }
  setCursorShape (cursorShape);
  DebugAssert(ok() == True, AipsError);
};

LatticeStepper::LatticeStepper(const IPosition & latticeShape,
			       const IPosition & cursorShape,
			       const IPosition & axisPath,
			       const uInt hangOverPolicy)
  :theIndexer(latticeShape),
   theCursorShape(latticeShape.nelements()),
   theAxisPath(axisPath),
   theCursorPos(latticeShape.nelements(), 0),
   theNsteps(0),
   theEnd(False),
   theStart(True),
   theNiceFit(False),
   theHangover(False),
   thePolicy(hangOverPolicy)
{
   setCursorShape (cursorShape);
   DebugAssert(ok() == True, AipsError);
};

LatticeStepper::LatticeStepper(const IPosition & latticeShape,
			       const IPosition & cursorShape,
			       const IPosition & cursorAxes,
			       const IPosition & axisPath,
			       const uInt hangOverPolicy)
  :theIndexer(latticeShape),
   theCursorShape(latticeShape.nelements()),
   theAxisPath(axisPath),
   theCursorPos(latticeShape.nelements(), 0),
   theNsteps(0),
   theEnd(False),
   theStart(True),
   theNiceFit(False),
   theHangover(False),
   thePolicy(hangOverPolicy)
{
   setCursorShape (cursorShape, cursorAxes);
   DebugAssert(ok() == True, AipsError);
};

LatticeStepper::LatticeStepper(const LatticeStepper & other)
  :theIndexer(other.theIndexer),
   theCursorAxes (other.theCursorAxes),
   theCursorShape(other.theCursorShape),
   theAxisPath(other.theAxisPath),
   theCursorPos(other.theCursorPos),
   theNsteps(other.theNsteps),
   theEnd(other.theEnd),
   theStart(other.theStart),
   theNiceFit(other.theNiceFit),
   theHangover(other.theHangover),
   thePolicy(other.thePolicy)
{
  DebugAssert(ok() == True, AipsError);
};

LatticeStepper::~LatticeStepper() {
  // does nothing
};

LatticeStepper & LatticeStepper::operator=(const LatticeStepper & other) {
  if (this != &other) { 
    theIndexer = other.theIndexer;
    theCursorAxes  = other.theCursorAxes;
    theCursorShape = other.theCursorShape;
    theAxisPath = other.theAxisPath;
    theCursorPos = other.theCursorPos;
    theNsteps = other.theNsteps;
    theEnd = other.theEnd;
    theStart = other.theStart;
    theNiceFit = other.theNiceFit;
    theHangover = other.theHangover;
    thePolicy = other.thePolicy;
  }
  DebugAssert(ok() == True, AipsError);
  return *this;
};

Bool LatticeStepper::operator++(Int) {
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

Bool LatticeStepper::operator++() {
  return operator++(0);
};

Bool LatticeStepper::operator--(Int) {
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

Bool LatticeStepper::operator--() {
  return operator--(0);
};

void LatticeStepper::reset() {
  theCursorPos = 0;
  theNsteps = 0;
  theEnd = False;
  theStart = True;
  theHangover = False;
  DebugAssert(ok() == True, AipsError);
};

Bool LatticeStepper::atStart() const {
  DebugAssert(ok() == True, AipsError);
  return theStart;
};

Bool LatticeStepper::atEnd() const {
  DebugAssert(ok() == True, AipsError);
  return theEnd;
};

uInt LatticeStepper::nsteps() const {
  DebugAssert(ok() == True, AipsError);
  return theNsteps;
};

IPosition LatticeStepper::position() const {
  DebugAssert(ok() == True, AipsError);
  return theIndexer.absolutePosition(theCursorPos);
};

IPosition LatticeStepper::relativePosition() const {
  DebugAssert(ok() == True, AipsError);
  return theCursorPos;
};

// Function which returns the current position of the end of the cursor
// relative to the main Lattice.
IPosition LatticeStepper::endPosition() const {
  DebugAssert(ok() == True, AipsError);
  return theIndexer.absolutePosition(relativeEndPosition());
};

// Function which returns the current position of the end of the cursor
// relative to the sub Lattice.
IPosition LatticeStepper::relativeEndPosition() const {
  DebugAssert(ok() == True, AipsError);
  IPosition trc(theCursorPos + theCursorShape - 1);
  if (thePolicy == RESIZE) {
    const IPosition latticeShape(subLatticeShape());
    const uInt nDim = trc.nelements();
    for (uInt n = 0; n < nDim; n++) {
      if (trc(n) >= latticeShape(n)) {
        trc(n) = latticeShape(n) - 1;
      }
    }
  }
  return trc;
};

IPosition LatticeStepper::latticeShape() const {
  DebugAssert(ok() == True, AipsError);
  return theIndexer.fullShape();
};

IPosition LatticeStepper::subLatticeShape() const {
  DebugAssert(ok() == True, AipsError);
  return theIndexer.shape();
};

void LatticeStepper::setCursorShape(const IPosition & cursorShape)
{
  setCursorShape (cursorShape, IPosition());
};

void LatticeStepper::setCursorShape(const IPosition & cursorShape,
				    const IPosition & cursorAxes)
{
  const IPosition& latticeShape = theIndexer.fullShape();
  uInt latticeDim = theIndexer.ndim();
  uInt ndimCS = cursorShape.nelements();
  uInt ndimCA = cursorAxes.nelements();
  if (ndimCS == 0  ||  ndimCS > latticeDim) {
    throw (AipsError ("LatticeStepper::setCursorShape: cursorShape"
		      " has no axes or more axes than lattice"));
  }
  if (ndimCA > latticeDim) {
    throw (AipsError ("LatticeStepper::setCursorShape: cursorAxes"
		      " has more axes than lattice"));
  }
  if (!(ndimCA==0 || ndimCA==ndimCS || ndimCS==latticeDim)) {
    throw (AipsError ("LatticeStepper::setCursorShape: cursorAxes"
		      " has invalid number of axes; it should be 0,"
		      " equal to cursorShape, or cursorShape should"
		      " contain all axes"));
  }
  uInt i;
  // Check the cursor shape.
  // Count the cursor shape axes with length > 1.
  uInt count = 0;
  for (i=0; i<ndimCS; i++) {
    if (cursorShape(i) <= 0  ||  cursorShape(i) > latticeShape(i)) {
      throw (AipsError ("LatticeStepper::setCursorShape: "
			"cursorShape <=0 or > latticeShape"));
    }
    if (cursorShape(i) > 1) {
      count++;
    }
  }
  // Check if the cursor axes are given correctly and in ascending order.
  // Check if the cursor shape for non-given axes is 1.
  for (i=0; i<ndimCA; i++) {
    if (cursorAxes(i) < 0   ||  cursorAxes(i) >= latticeDim) {
      throw (AipsError ("LatticeStepper::setCursorShape: "
			"cursorAxes value <0 or >latticeDim"));
    }
    if (i > 0) {
      if (cursorAxes(i) <= cursorAxes(i-1)) {
	throw (AipsError ("LatticeStepper::setCursorShape: "
			  "cursorAxes values not in ascending order"));
      }
    }
  }
  // If cursorAxes is given and cursorShape is given for all axes,
  // check if the cursor shape for non-cursorAxes is 1.
  if (ndimCA > 0  &&  ndimCA != ndimCS) {
    for (i=0; i<ndimCS; i++) {
      for (uInt j=0; j<ndimCA; j++) {
	if (i == cursorAxes(j)) {
	  break;
	}
      }
      if (j == ndimCA) {
	if (cursorShape(i) != 1) {
	  throw (AipsError ("LatticeStepper::setCursorShape: "
			    "a non-cursorAxes axis in the cursorShape"
			    " should have length 1"));
	}
      }
    }
  }
  // Pad the cursor shape with 1's if not given completely.
  // When ndimCA==ndimCS, cursorAxes gives the axes of the cursor shape.
  theCursorShape = 1;
  for (i=0; i<ndimCS; i++) {
    if (ndimCA == ndimCS) {
      theCursorShape(cursorAxes(i)) = cursorShape(cursorAxes(i));
    }else{
      theCursorShape(i) = cursorShape(i);
    }
  }
  // When cursorAxes is not given, the axes with length>1 form the cursorAxes.
  if (ndimCA == 0) {
    theCursorAxes.resize (count);
    count = 0;
    for (i=0; i<ndimCS; i++) {
      if (theCursorShape(i) > 1) {
	theCursorAxes(count++) = i;
      }
    }
  }else{
    theCursorAxes.resize (ndimCA);
    theCursorAxes = cursorAxes;
  }
  theNiceFit = niceFit();
  AlwaysAssert(ok() == True, AipsError);
};

IPosition LatticeStepper::cursorAxes() const
{
  DebugAssert(ok() == True, AipsError);
  return theCursorAxes;
};

IPosition LatticeStepper::cursorShape() const {
  DebugAssert(ok() == True, AipsError);
  if (hangOver() && thePolicy == RESIZE)
    return relativeEndPosition() - relativePosition() + 1;
  else
    return theCursorShape;
};

Bool LatticeStepper::hangOver() const {
  DebugAssert(ok() == True, AipsError);
  return theHangover;
};

void LatticeStepper::subSection(const IPosition & blc, const IPosition & trc, 
				const IPosition & inc) {
  theIndexer.fullSize();
  theIndexer.subSection(blc, trc, inc);
  reset();
  theNiceFit = niceFit();
  DebugAssert(ok() == True, AipsError);
};

void LatticeStepper::subSection(const IPosition & blc, const IPosition & trc) {
  subSection(blc, trc, IPosition(theIndexer.ndim(), 1));
};

IPosition LatticeStepper::blc() const {
  DebugAssert(ok() == True, AipsError);
  return theIndexer.offset();
};

IPosition LatticeStepper::trc() const {
  DebugAssert(ok() == True, AipsError);
  return theIndexer.absolutePosition(theIndexer.shape()-1);
};

IPosition LatticeStepper::increment() const {
  DebugAssert(ok() == True, AipsError);
  return theIndexer.increment();
};

const IPosition & LatticeStepper::axisPath() const {
  DebugAssert(ok() == True, AipsError);
  return theAxisPath;
};

LatticeNavigator * LatticeStepper::clone() const {
  return new LatticeStepper(*this);
};

Bool LatticeStepper::ok() const
{
  const uInt latticeDim = theIndexer.ndim();
  // Check the cursor shape is OK
  if (theCursorShape.nelements() != latticeDim) {
    LogIO logErr(LogOrigin("LatticeStepper", "ok()"));
    logErr << LogIO::SEVERE << "cursor shape"
	   << " (=" << theCursorShape << ")"
	   << " has wrong number of dimensions"
	   << " (ie. not " << latticeDim << ")" << endl;
     return False;
  }
  for (uInt i=0; i < latticeDim; i++) 
    // the cursor shape must be <= the corresponding lattice axes AND
    // a cursor shape with an axis of length zero makes no sense
    if (theCursorShape(i) > theIndexer.shape(i) || theCursorShape(i) <= 0) {
    LogIO logErr(LogOrigin("LatticeStepper", "ok()"));
      logErr << LogIO::SEVERE << "cursor shape"
	     << " (=" << theCursorShape << ")"
	     << " is too big or small for lattice shape"
	     << " (=" << theIndexer.shape() << ")" << LogIO::POST;
      return False;
    }
  // Check the cursor position is OK
  if (theCursorPos.nelements() != latticeDim) {
    LogIO logErr(LogOrigin("LatticeStepper", "ok()"));
    logErr << LogIO::SEVERE << "cursor position"
	   << " (=" << theCursorPos << ")"
	   << " has wrong number of dimensions"
	   << " (ie. not" << latticeDim << ")" << LogIO::POST;
     return False;
  }
  
  // cursor position or its "far corner" must be inside the (sub)-Lattice
  if (!(theIndexer.isInside(theCursorPos) ||
	theIndexer.isInside(theCursorPos+theCursorShape-1))){
    LogIO logErr(LogOrigin("LatticeStepper", "ok()"));
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
    LogIO logErr(LogOrigin("LatticeStepper", "ok()"));
    logErr << LogIO::SEVERE << "axis path"
	   << " (=" << theAxisPath << ")"
	   << " has wrong number of dimensions"
	   << " (ie. not " << latticeDim << ")" << LogIO::POST;
    return False;
  }
  // each theAxisPath value must be a lattice axis number, 0..n-1
  for (uInt n=0; n < latticeDim; n++)
    if (theAxisPath(n) >= latticeDim){
      LogIO logErr(LogOrigin("LatticeStepper", "ok()"));
      logErr << LogIO::SEVERE << "axis path"
	     << " (=" << theAxisPath << ")"
	     << " has elements >= the lattice dim "
	     << latticeDim - 1 << LogIO::POST;
      return False;
    }

  // each theAxisPath value must be unique
  for (uInt k=0; k < (latticeDim - 1); k++)
    for (uInt j=k+1; j < latticeDim; j++)
      if (theAxisPath(k) == theAxisPath(j)) {
	LogIO logErr(LogOrigin("LatticeStepper", "ok()"));
	logErr << LogIO::SEVERE << "axis path"
	       << " (=" << theAxisPath << ")"
	       << " does not have unique elements " << LogIO::POST;
	return False;
      }
  // Check the LatticeIndexer is OK
  if (theIndexer.ok() == False) {
    LogIO logErr(LogOrigin("LatticeStepper", "ok()"));
    logErr << LogIO::SEVERE << "LatticeIndexer"
 	   << " thinks things are bad" << LogIO::POST;
    return False;
  }
  
  // Check if theStart flag is correct
  if ((theCursorPos.isEqual(IPosition(latticeDim,0)) && (theStart == False)) ||
      (!theCursorPos.isEqual(IPosition(latticeDim,0)) && (theStart == True))){
    LogIO logErr(LogOrigin("LatticeStepper", "ok()"));
    logErr << LogIO::SEVERE << "cursor position"
	   << " (=" << theCursorPos << ")"
	   << " is inconsistent with theStart flag"
	   << " (theStart = ";
    if (theStart == True)
      logErr << "True";
    else
      logErr << "False";
    logErr << ")" << LogIO::POST;
    return False;
  }

  // Check if theNiceFit is correct.
  if (theNiceFit != niceFit()) {
    LogIO logErr(LogOrigin("LatticeStepper", "ok()"));
    logErr << LogIO::SEVERE << "theNiceFit"
	   << " (=" << theNiceFit
	   << " is inconsistent with niceFit()";
    logErr << LogIO::POST;
    return False;
  }
  
  // Otherwise it has passed all the tests
  return True;
};

// check if the cursor shape is an sub-multiple of the Lattice shape
Bool LatticeStepper::niceFit() const
{
  const uInt cursorDim = theCursorShape.nelements();
  // Determine if the Lattice shape is a multiple of the cursor shape.
  uInt i = 0;
  while (i < cursorDim && 
	 theIndexer.shape(i)%theCursorShape(i) == 0)
    i++;

  if (i == cursorDim)
    return True;
  else
    return False;
};

LatticeStepper * LatticeStepper::castToStepper() {
  return this;
};

const LatticeStepper * LatticeStepper::castToConstStepper() const {
  return this;
};
