//# LatticeStepper.cc: defines LatticeStepper class
//# Copyright (C) 1994,1995,1996,1997,1998
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
#include <aips/Tables/TiledStManAccessor.h>
#include <aips/Logging/LogIO.h>
#include <aips/Logging/LogOrigin.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>


LatticeStepper::LatticeStepper (const IPosition& latticeShape,
				const IPosition& cursorShape,
				const uInt hangOverPolicy)
: itsIndexer     (latticeShape),
  itsCursorShape (latticeShape.nelements()),
  itsCursorPos   (latticeShape.nelements(),0),
  itsAxisPath    (IPosition::makeAxisPath(latticeShape.nelements())),
  itsNsteps      (0),
  itsEnd         (False),
  itsStart       (True),
  itsNiceFit     (False),
  itsHangover    (False),
  itsPolicy      (hangOverPolicy)
{
  setCursorShape (cursorShape);
  DebugAssert (ok() == True, AipsError);
}

LatticeStepper::LatticeStepper (const IPosition& latticeShape,
				const IPosition& cursorShape,
				const IPosition& axisPath,
				const uInt hangOverPolicy)
: itsIndexer     (latticeShape),
  itsCursorShape (latticeShape.nelements()),
  itsCursorPos   (latticeShape.nelements(), 0),
  itsAxisPath    (IPosition::makeAxisPath(latticeShape.nelements(), axisPath)),
  itsNsteps      (0),
  itsEnd         (False),
  itsStart       (True),
  itsNiceFit     (False),
  itsHangover    (False),
  itsPolicy      (hangOverPolicy)
{
   setCursorShape (cursorShape);
   DebugAssert (ok() == True, AipsError);
}

LatticeStepper::LatticeStepper (const IPosition& latticeShape,
				const IPosition& cursorShape,
				const IPosition& cursorAxes,
				const IPosition& axisPath,
				const uInt hangOverPolicy)
: itsIndexer     (latticeShape),
  itsCursorShape (latticeShape.nelements()),
  itsCursorPos   (latticeShape.nelements(), 0),
  itsAxisPath    (IPosition::makeAxisPath(latticeShape.nelements(), axisPath)),
  itsNsteps      (0),
  itsEnd         (False),
  itsStart       (True),
  itsNiceFit     (False),
  itsHangover    (False),
  itsPolicy      (hangOverPolicy)
{
   setCursorShape (cursorShape, cursorAxes);
   DebugAssert (ok() == True, AipsError);
}

LatticeStepper::LatticeStepper (const LatticeStepper& other)
: itsIndexer     (other.itsIndexer),
  itsCursorAxes  (other.itsCursorAxes),
  itsCursorShape (other.itsCursorShape),
  itsCursorPos   (other.itsCursorPos),
  itsAxisPath    (other.itsAxisPath),
  itsNsteps      (other.itsNsteps),
  itsEnd         (other.itsEnd),
  itsStart       (other.itsStart),
  itsNiceFit     (other.itsNiceFit),
  itsHangover    (other.itsHangover),
  itsPolicy      (other.itsPolicy)
{
  DebugAssert(ok() == True, AipsError);
}

LatticeStepper::~LatticeStepper()
{
  // does nothing
}

LatticeStepper& LatticeStepper::operator=(const LatticeStepper& other)
{
  if (this != &other) { 
    itsIndexer     = other.itsIndexer;
    itsCursorAxes  = other.itsCursorAxes;
    itsCursorShape = other.itsCursorShape;
    itsCursorPos   = other.itsCursorPos;
    itsAxisPath    = other.itsAxisPath;
    itsNsteps      = other.itsNsteps;
    itsEnd         = other.itsEnd;
    itsStart       = other.itsStart;
    itsNiceFit     = other.itsNiceFit;
    itsHangover    = other.itsHangover;
    itsPolicy      = other.itsPolicy;
  }
  DebugAssert (ok() == True, AipsError);
  return *this;
}

Bool LatticeStepper::operator++(int)
{
  DebugAssert (ok() == True, AipsError);
  if (itsEnd) {
    return False;
  }
  // Increment the counter.
  itsNsteps++;
  // itsStart = false by definition when incrementing
  itsStart = False;
  Bool successful = itsIndexer.tiledCursorMove (True, itsCursorPos, 
						itsCursorShape, itsAxisPath);
  if (successful) {
    // test for hang over since cursor has moved.
    if (itsNiceFit == False) {
      const IPosition curPos(itsCursorPos);
      const IPosition curEndPos(itsCursorPos+itsCursorShape-1);
      const IPosition latShape(itsIndexer.shape());
      const uInt ndim = itsIndexer.ndim();
      uInt i = 0;
      while (i < ndim  &&  curEndPos(i) < latShape(i)  &&  curPos(i) >= 0) {
	i++;
      }
      itsHangover = ToBool (i != ndim);
    }
  } else {
    itsEnd = True;
  }
  DebugAssert (ok() == True, AipsError);
  return successful;
}

Bool LatticeStepper::operator--(int)
{
  DebugAssert (ok() == True, AipsError);
  if (itsStart) {
    return False;
  }
  // Increment the counter.
  itsNsteps++;
  // itsEnd = false by definition when decrementing
  itsEnd = False; 
  Bool successful = itsIndexer.tiledCursorMove (False, itsCursorPos, 
						itsCursorShape, itsAxisPath);
  if (successful) {
    // test for hang over since cursor has moved
    const IPosition curPos(itsCursorPos);
    const uInt ndim = itsIndexer.ndim();
    if (itsNiceFit == False) {
      const IPosition curEndPos(itsCursorPos+itsCursorShape);
      const IPosition latShape(itsIndexer.shape());
      uInt i = 0;
      while (i < ndim  &&  curPos(i) >= 0  &&  curEndPos(i) < latShape(i)) {
	i++;
      }
      itsHangover = ToBool (i != ndim);
    }
  } else {
    itsStart = True;
  }
  DebugAssert (ok() == True, AipsError);
  return successful;
}

void LatticeStepper::reset()
{
  itsCursorPos = 0;
  itsNsteps = 0;
  itsEnd = False;
  itsStart = True;
  itsHangover = False;
  if (!itsNiceFit) {
    const uInt ndim = itsIndexer.ndim();
    const IPosition latShape(itsIndexer.shape());
    for (uInt i=0; i<ndim; i++) {
      if (itsCursorShape(i) > latShape(i)) {
	itsHangover = True;
      }
    }
  }
  DebugAssert (ok() == True, AipsError);
}

Bool LatticeStepper::atStart() const
{
  DebugAssert (ok() == True, AipsError);
  return itsStart;
}

Bool LatticeStepper::atEnd() const
{
  DebugAssert (ok() == True, AipsError);
  return itsEnd;
}

uInt LatticeStepper::nsteps() const
{
  DebugAssert (ok() == True, AipsError);
  return itsNsteps;
}

IPosition LatticeStepper::position() const
{
  DebugAssert (ok() == True, AipsError);
  return itsIndexer.absolutePosition (itsCursorPos);
}

IPosition LatticeStepper::relativePosition() const
{
  DebugAssert (ok() == True, AipsError);
  return itsCursorPos;
}

// Function which returns the current position of the end of the cursor
// relative to the main Lattice.
IPosition LatticeStepper::endPosition() const
{
  DebugAssert (ok() == True, AipsError);
  return itsIndexer.absolutePosition (relativeEndPosition());
}

// Function which returns the current position of the end of the cursor
// relative to the sub Lattice.
IPosition LatticeStepper::relativeEndPosition() const
{
  DebugAssert (ok() == True, AipsError);
  IPosition trc(itsCursorPos + itsCursorShape - 1);
  if (itsHangover) {
    const IPosition latticeShape(subLatticeShape());
    const uInt nDim = trc.nelements();
    for (uInt n = 0; n < nDim; n++) {
      if (trc(n) >= latticeShape(n)) {
        trc(n) = latticeShape(n) - 1;
      }
    }
  }
  return trc;
}

IPosition LatticeStepper::latticeShape() const
{
  DebugAssert (ok() == True, AipsError);
  return itsIndexer.fullShape();
}

IPosition LatticeStepper::subLatticeShape() const {
  DebugAssert(ok() == True, AipsError);
  return itsIndexer.shape();
}

void LatticeStepper::setCursorShape (const IPosition& cursorShape)
{
  setCursorShape (cursorShape, IPosition());
}

void LatticeStepper::setCursorShape (const IPosition& cursorShape,
				     const IPosition& cursorAxes)
{
  const IPosition& latticeShape = itsIndexer.fullShape();
  uInt latticeDim = itsIndexer.ndim();
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
  if (!(ndimCA==0  ||  ndimCA==ndimCS  ||  ndimCS==latticeDim)) {
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
    if (cursorAxes(i) < 0   ||  cursorAxes(i) >= Int(latticeDim)) {
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
	if (Int(i) == cursorAxes(j)) {
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
  itsCursorShape = 1;
  for (i=0; i<ndimCS; i++) {
    if (ndimCA == ndimCS) {
      itsCursorShape(cursorAxes(i)) = cursorShape(cursorAxes(i));
    } else {
      itsCursorShape(i) = cursorShape(i);
    }
  }
  // When cursorAxes is not given, the axes with length>1 form the cursorAxes.
  if (ndimCA == 0) {
    itsCursorAxes.resize (count);
    count = 0;
    for (i=0; i<ndimCS; i++) {
      if (itsCursorShape(i) > 1) {
	itsCursorAxes(count++) = i;
      }
    }
  }else{
    itsCursorAxes.resize (ndimCA);
    itsCursorAxes = cursorAxes;
  }
  itsNiceFit = niceFit();
  reset();
  AlwaysAssert (ok() == True, AipsError);
}

IPosition LatticeStepper::cursorAxes() const
{
  DebugAssert (ok() == True, AipsError);
  return itsCursorAxes;
}

IPosition LatticeStepper::cursorShape() const
{
  DebugAssert (ok() == True, AipsError);
  if (hangOver()  &&  itsPolicy == RESIZE) {
    return relativeEndPosition() - relativePosition() + 1;
  }
  return itsCursorShape;
}

Bool LatticeStepper::hangOver() const
{
  DebugAssert (ok() == True, AipsError);
  return itsHangover;
}

void LatticeStepper::subSection(const IPosition& blc, const IPosition& trc, 
				const IPosition& inc)
{
  itsIndexer.fullSize();
  itsIndexer.subSection (blc, trc, inc);
  itsNiceFit = niceFit();
  reset();
  DebugAssert (ok() == True, AipsError);
}

void LatticeStepper::subSection(const IPosition& blc, const IPosition& trc)
{
  subSection (blc, trc, IPosition(itsIndexer.ndim(), 1));
}

IPosition LatticeStepper::blc() const
{
  DebugAssert (ok() == True, AipsError);
  return itsIndexer.offset();
}

IPosition LatticeStepper::trc() const
{
  DebugAssert (ok() == True, AipsError);
  return itsIndexer.absolutePosition (itsIndexer.shape() - 1);
}

IPosition LatticeStepper::increment() const
{
  DebugAssert (ok() == True, AipsError);
  return itsIndexer.increment();
}

const IPosition& LatticeStepper::axisPath() const
{
  DebugAssert (ok() == True, AipsError);
  return itsAxisPath;
}

// check if the cursor shape is an sub-multiple of the Lattice shape
Bool LatticeStepper::niceFit() const
{
  const uInt cursorDim = itsCursorShape.nelements();
  // Determine if the Lattice shape is a multiple of the cursor shape.
  uInt i = 0;
  while (i < cursorDim  &&  itsIndexer.shape(i)%itsCursorShape(i) == 0) {
    i++;
  }
  return ToBool (i == cursorDim);
}

LatticeNavigator* LatticeStepper::clone() const
{
  return new LatticeStepper(*this);
}

uInt LatticeStepper::calcCacheSize (const ROTiledStManAccessor& accessor,
				    uInt rowNumber) const
{
  return accessor.calcCacheSize (rowNumber, itsCursorShape,
				 blc(), trc() - blc() + 1,
				 itsAxisPath);
}

Bool LatticeStepper::ok() const
{
  const uInt latticeDim = itsIndexer.ndim();
  // Check the cursor shape is OK
  if (itsCursorShape.nelements() != latticeDim) {
    LogIO logErr(LogOrigin("LatticeStepper", "ok()"));
    logErr << LogIO::SEVERE << "cursor shape"
	   << " (=" << itsCursorShape << ")"
	   << " has wrong number of dimensions"
	   << " (ie. not " << latticeDim << ")" << LogIO::POST;
    return False;
  }
  for (uInt i=0; i < latticeDim; i++) {
    // the cursor shape must be <= the corresponding lattice axes AND
    // a cursor shape with an axis of length zero makes no sense
    if (itsCursorShape(i) > Int(itsIndexer.fullShape(i))
    ||  itsCursorShape(i) <= 0) {
      LogIO logErr(LogOrigin("LatticeStepper", "ok()"));
      logErr << LogIO::SEVERE << "cursor shape"
	     << " (=" << itsCursorShape << ")"
	     << " is too big or small for full lattice shape"
	     << " (=" << itsIndexer.fullShape() << ")" << LogIO::POST;
      return False;
    }
  }
  // Check the cursor position is OK
  if (itsCursorPos.nelements() != latticeDim) {
    LogIO logErr(LogOrigin("LatticeStepper", "ok()"));
    logErr << LogIO::SEVERE << "cursor position"
	   << " (=" << itsCursorPos << ")"
	   << " has wrong number of dimensions"
	   << " (ie. not" << latticeDim << ")" << LogIO::POST;
     return False;
  }
  
  // cursor position or its "far corner" must be inside the (sub)-Lattice
  if (!(itsIndexer.isInside(itsCursorPos)  ||
	itsIndexer.isInside(itsCursorPos+itsCursorShape-1))){
    LogIO logErr(LogOrigin("LatticeStepper", "ok()"));
    logErr << LogIO::SEVERE << "cursor beginning"
	   << " (=" << itsCursorPos << ")"
	   << " or end"
	   << " (=" << itsCursorPos + itsCursorShape - 1 << ")"
	   << " is entirely outside the lattice shape"
	   << " (=" << itsIndexer.shape() << ")" << LogIO::POST;
    return False;
  }

  // check the Axis Path is OK
  if(itsAxisPath.nelements() != latticeDim) {
    LogIO logErr(LogOrigin("LatticeStepper", "ok()"));
    logErr << LogIO::SEVERE << "axis path"
	   << " (=" << itsAxisPath << ")"
	   << " has wrong number of dimensions"
	   << " (ie. not " << latticeDim << ")" << LogIO::POST;
    return False;
  }
  // each itsAxisPath value must be a lattice axis number, 0..n-1
  for (uInt n=0; n < latticeDim; n++) {
    if (itsAxisPath(n) >= Int(latticeDim)){
      LogIO logErr(LogOrigin("LatticeStepper", "ok()"));
      logErr << LogIO::SEVERE << "axis path"
	     << " (=" << itsAxisPath << ")"
	     << " has elements >= the lattice dim "
	     << latticeDim - 1 << LogIO::POST;
      return False;
    }
  }
  // each itsAxisPath value must be unique
  for (uInt k=0; k < (latticeDim - 1); k++) {
    for (uInt j=k+1; j < latticeDim; j++) {
      if (itsAxisPath(k) == itsAxisPath(j)) {
	LogIO logErr(LogOrigin("LatticeStepper", "ok()"));
	logErr << LogIO::SEVERE << "axis path"
	       << " (=" << itsAxisPath << ")"
	       << " does not have unique elements " << LogIO::POST;
	return False;
      }
    }
  }
  // Check the LatticeIndexer is OK
  if (itsIndexer.ok() == False) {
    LogIO logErr(LogOrigin("LatticeStepper", "ok()"));
    logErr << LogIO::SEVERE << "LatticeIndexer"
 	   << " thinks things are bad" << LogIO::POST;
    return False;
  }
  // Check if itsNiceFit is correct.
  if (itsNiceFit != niceFit()) {
    LogIO logErr(LogOrigin("LatticeStepper", "ok()"));
    logErr << LogIO::SEVERE << "itsNiceFit"
	   << " (=" << itsNiceFit
	   << " is inconsistent with niceFit()";
    logErr << LogIO::POST;
    return False;
  }
  // Otherwise it has passed all the tests
  return True;
}
