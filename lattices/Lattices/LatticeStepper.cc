//# LatticeStepper.cc: defines LatticeStepper class
//# Copyright (C) 1994,1995,1996,1997,1998,2000,2001,2003
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

#include <casacore/lattices/Lattices/LatticeStepper.h>
#include <casacore/tables/DataMan/TSMCube.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/sstream.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

LatticeStepper::LatticeStepper (const IPosition& latticeShape,
				const IPosition& cursorShape,
				const uint32_t hangOverPolicy)
: itsIndexer     (latticeShape),
  itsCursorShape (latticeShape.nelements()),
  itsCursorPos   (latticeShape.nelements(),0),
  itsAxisPath    (IPosition::makeAxisPath(latticeShape.nelements())),
  itsNsteps      (0),
  itsEnd         (false),
  itsStart       (true),
  itsNiceFit     (false),
  itsHangover    (false),
  itsPolicy      (hangOverPolicy)
{
  setCursorShape (cursorShape);
  DebugAssert (ok() == true, AipsError);
}

LatticeStepper::LatticeStepper (const IPosition& latticeShape,
				const IPosition& cursorShape,
				const IPosition& axisPath,
				const uint32_t hangOverPolicy)
: itsIndexer     (latticeShape),
  itsCursorShape (latticeShape.nelements()),
  itsCursorPos   (latticeShape.nelements(), 0),
  itsAxisPath    (IPosition::makeAxisPath(latticeShape.nelements(), axisPath)),
  itsNsteps      (0),
  itsEnd         (false),
  itsStart       (true),
  itsNiceFit     (false),
  itsHangover    (false),
  itsPolicy      (hangOverPolicy)
{
   setCursorShape (cursorShape);
   DebugAssert (ok() == true, AipsError);
}

LatticeStepper::LatticeStepper (const IPosition& latticeShape,
				const IPosition& cursorShape,
				const IPosition& cursorAxes,
				const IPosition& axisPath,
				const uint32_t hangOverPolicy)
: itsIndexer     (latticeShape),
  itsCursorShape (latticeShape.nelements()),
  itsCursorPos   (latticeShape.nelements(), 0),
  itsAxisPath    (IPosition::makeAxisPath(latticeShape.nelements(), axisPath)),
  itsNsteps      (0),
  itsEnd         (false),
  itsStart       (true),
  itsNiceFit     (false),
  itsHangover    (false),
  itsPolicy      (hangOverPolicy)
{
   setCursorShape (cursorShape, cursorAxes);
   DebugAssert (ok() == true, AipsError);
}

LatticeStepper::LatticeStepper (const LatticeStepper& other)
: LatticeNavigator(),
  itsIndexer     (other.itsIndexer),
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
  DebugAssert(ok() == true, AipsError);
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
  DebugAssert (ok() == true, AipsError);
  return *this;
}

bool LatticeStepper::operator++(int)
{
  DebugAssert (ok() == true, AipsError);
  if (itsEnd) {
    return false;
  }
  // Increment the counter.
  itsNsteps++;
  // itsStart = false by definition when incrementing
  itsStart = false;
  bool successful = itsIndexer.tiledCursorMove (true, itsCursorPos, 
						itsCursorShape, itsAxisPath);
  if (successful) {
    // test for hang over since cursor has moved.
    if (itsNiceFit == false) {
      const IPosition curPos(itsCursorPos);
      const IPosition curEndPos(itsCursorPos+itsCursorShape-1);
      const IPosition latShape(itsIndexer.shape());
      const uint32_t ndim = itsIndexer.ndim();
      uint32_t i = 0;
      while (i < ndim  &&  curEndPos(i) < latShape(i)  &&  curPos(i) >= 0) {
	i++;
      }
      itsHangover =  (i != ndim);
    }
  } else {
    itsEnd = true;
  }
  DebugAssert (ok() == true, AipsError);
  return successful;
}

bool LatticeStepper::operator--(int)
{
  DebugAssert (ok() == true, AipsError);
  if (itsStart) {
    return false;
  }
  // Increment the counter.
  itsNsteps++;
  // itsEnd = false by definition when decrementing
  itsEnd = false; 
  bool successful = itsIndexer.tiledCursorMove (false, itsCursorPos, 
						itsCursorShape, itsAxisPath);
  if (successful) {
    // test for hang over since cursor has moved
    const IPosition curPos(itsCursorPos);
    const uint32_t ndim = itsIndexer.ndim();
    if (itsNiceFit == false) {
      const IPosition curEndPos(itsCursorPos+itsCursorShape);
      const IPosition latShape(itsIndexer.shape());
      uint32_t i = 0;
      while (i < ndim  &&  curPos(i) >= 0  &&  curEndPos(i) < latShape(i)) {
	i++;
      }
      itsHangover =  (i != ndim);
    }
  } else {
    itsStart = true;
  }
  DebugAssert (ok() == true, AipsError);
  return successful;
}

void LatticeStepper::reset()
{
  itsCursorPos = 0;
  itsNsteps = 0;
  itsEnd = false;
  itsStart = true;
  itsHangover = false;
  if (!itsNiceFit) {
    const uint32_t ndim = itsIndexer.ndim();
    const IPosition latShape(itsIndexer.shape());
    for (uint32_t i=0; i<ndim; i++) {
      if (itsCursorShape(i) > latShape(i)) {
	itsHangover = true;
      }
    }
  }
  DebugAssert (ok() == true, AipsError);
}

bool LatticeStepper::atStart() const
{
  DebugAssert (ok() == true, AipsError);
  return itsStart;
}

bool LatticeStepper::atEnd() const
{
  DebugAssert (ok() == true, AipsError);
  return itsEnd;
}

uint32_t LatticeStepper::nsteps() const
{
  DebugAssert (ok() == true, AipsError);
  return itsNsteps;
}

IPosition LatticeStepper::position() const
{
  DebugAssert (ok() == true, AipsError);
  return itsIndexer.absolutePosition (itsCursorPos);
}

IPosition LatticeStepper::relativePosition() const
{
  DebugAssert (ok() == true, AipsError);
  return itsCursorPos;
}

// Function which returns the current position of the end of the cursor
// relative to the main Lattice.
IPosition LatticeStepper::endPosition() const
{
  DebugAssert (ok() == true, AipsError);
  return itsIndexer.absolutePosition (relativeEndPosition());
}

// Function which returns the current position of the end of the cursor
// relative to the sub Lattice.
IPosition LatticeStepper::relativeEndPosition() const
{
  DebugAssert (ok() == true, AipsError);
  IPosition trc(itsCursorPos + itsCursorShape - 1);
  if (itsHangover) {
    const IPosition latticeShape(subLatticeShape());
    const uint32_t nDim = trc.nelements();
    for (uint32_t n = 0; n < nDim; n++) {
      if (trc(n) >= latticeShape(n)) {
        trc(n) = latticeShape(n) - 1;
      }
    }
  }
  return trc;
}

IPosition LatticeStepper::latticeShape() const
{
  DebugAssert (ok() == true, AipsError);
  return itsIndexer.fullShape();
}

IPosition LatticeStepper::subLatticeShape() const {
  DebugAssert(ok() == true, AipsError);
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
  uint32_t latticeDim = itsIndexer.ndim();
  uint32_t ndimCS = cursorShape.nelements();
  uint32_t ndimCA = cursorAxes.nelements();
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
  uint32_t i;
  // Check if the cursor axes are given correctly and in ascending order.
  for (i=0; i<ndimCA; i++) {
    if (cursorAxes(i) < 0   ||  cursorAxes(i) >= int32_t(latticeDim)) {
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
  // Count the cursor shape axes with length > 1.
  uint32_t count = 0;
  for (i=0; i<ndimCS; i++) {
    if (cursorShape(i) > 1) {
      count++;
    }
  }
  // If cursorAxes is given and cursorShape is given for all axes,
  // check if the cursor shape for non-cursorAxes is 1.
  if (ndimCA > 0  &&  ndimCA != ndimCS) {
    for (i=0; i<ndimCS; i++) {
      uint32_t j;
      for (j=0; j<ndimCA; j++) {
	if (int32_t(i) == cursorAxes(j)) {
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
  // Check the cursor shape.
  // Pad the cursor shape with 1's if not given completely.
  // When ndimCA==ndimCS, cursorAxes gives the axes of the cursor shape.
  itsCursorShape = 1;
  for (i=0; i<ndimCS; i++) {
    if (ndimCA == ndimCS) {
      itsCursorShape(cursorAxes(i)) = cursorShape(i);
    } else {
      itsCursorShape(i) = cursorShape(i);
    }
    if (itsCursorShape(i) <= 0  ||  itsCursorShape(i) > latticeShape(i)) {
      throw (AipsError ("LatticeStepper::setCursorShape: "
			"cursorShape <=0 or > latticeShape"));
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
  AlwaysAssert (ok() == true, AipsError);
}

IPosition LatticeStepper::cursorAxes() const
{
  DebugAssert (ok() == true, AipsError);
  return itsCursorAxes;
}

IPosition LatticeStepper::cursorShape() const
{
  DebugAssert (ok() == true, AipsError);
  if (hangOver()  &&  itsPolicy == RESIZE) {
    return relativeEndPosition() - relativePosition() + 1;
  }
  return itsCursorShape;
}

bool LatticeStepper::hangOver() const
{
  DebugAssert (ok() == true, AipsError);
  return itsHangover;
}

void LatticeStepper::subSection(const IPosition& blc, const IPosition& trc, 
				const IPosition& inc)
{
  itsIndexer.fullSize();
  itsIndexer.subSection (blc, trc, inc);
  itsNiceFit = niceFit();
  reset();
  DebugAssert (ok() == true, AipsError);
}

void LatticeStepper::subSection(const IPosition& blc, const IPosition& trc)
{
  subSection (blc, trc, IPosition(itsIndexer.ndim(), 1));
}

IPosition LatticeStepper::blc() const
{
  DebugAssert (ok() == true, AipsError);
  return itsIndexer.offset();
}

IPosition LatticeStepper::trc() const
{
  DebugAssert (ok() == true, AipsError);
  return itsIndexer.absolutePosition (itsIndexer.shape() - 1);
}

IPosition LatticeStepper::increment() const
{
  DebugAssert (ok() == true, AipsError);
  return itsIndexer.increment();
}

const IPosition& LatticeStepper::axisPath() const
{
  DebugAssert (ok() == true, AipsError);
  return itsAxisPath;
}

// check if the cursor shape is an sub-multiple of the Lattice shape
bool LatticeStepper::niceFit() const
{
  const uint32_t cursorDim = itsCursorShape.nelements();
  // Determine if the Lattice shape is a multiple of the cursor shape.
  uint32_t i = 0;
  while (i < cursorDim  &&  itsIndexer.shape(i)%itsCursorShape(i) == 0) {
    i++;
  }
  return  (i == cursorDim);
}

LatticeNavigator* LatticeStepper::clone() const
{
  return new LatticeStepper(*this);
}

uint32_t LatticeStepper::calcCacheSize (const IPosition& cubeShape,
                                    const IPosition& tileShape,
                                    uint32_t maxCacheSize, uint32_t bucketSize) const
{
  return (bucketSize == 0  ?  0 :
          TSMCube::calcCacheSize (cubeShape, tileShape, false,
                                  itsCursorShape,
                                  blc(), trc() - blc() + 1,
                                  itsAxisPath,
                                  maxCacheSize, bucketSize));
}

bool LatticeStepper::ok() const
{
  ostringstream str;
  str << "LatticeStepper::ok - ";
  const uint32_t latticeDim = itsIndexer.ndim();
  // Check the cursor shape is OK
  if (itsCursorShape.nelements() != latticeDim) {
    str << "cursor shape " << itsCursorShape
	<< " has wrong number of dimensions (ie. not "
	<< latticeDim << ')';
    throw AipsError (String(str.str()));
    return false;
  }
  for (uint32_t i=0; i < latticeDim; i++) {
    // the cursor shape must be <= the corresponding lattice axes AND
    // a cursor shape with an axis of length zero makes no sense
    if (itsCursorShape(i) > int32_t(itsIndexer.fullShape(i))
    ||  itsCursorShape(i) <= 0) {
      str << "cursor shape " << itsCursorShape
	  << " is too big or small for full lattice shape "
	  << itsIndexer.fullShape();
      throw AipsError (String(str.str()));
      return false;
    }
  }
  // Check the cursor position is OK
  if (itsCursorPos.nelements() != latticeDim) {
    str << "cursor position " << itsCursorPos
	<< " has wrong number of dimensions (ie. not "
	<< latticeDim << ')';
    throw AipsError (String(str.str()));
     return false;
  }
  
  // cursor position or its "far corner" must be inside the (sub)-Lattice
  if (!(itsIndexer.isInside(itsCursorPos)  ||
	itsIndexer.isInside(itsCursorPos+itsCursorShape-1))){
    str << "cursor beginning " << itsCursorPos
	<< " or end " << itsCursorPos + itsCursorShape - 1
	<< " is entirely outside the lattice shape "
	<< itsIndexer.shape();
    throw AipsError (String(str.str()));
    return false;
  }

  // check the Axis Path is OK
  if(itsAxisPath.nelements() != latticeDim) {
    str << "axis path " << itsAxisPath
	<< " has wrong number of dimensions (ie. not "
	<< latticeDim << ')';
    throw AipsError (String(str.str()));
    return false;
  }
  // each itsAxisPath value must be a lattice axis number, 0..n-1
  for (uint32_t n=0; n < latticeDim; n++) {
    if (itsAxisPath(n) >= int32_t(latticeDim)){
      str << "axis path " << itsAxisPath
	  << " has elements >= the lattice dim "
	  << latticeDim - 1;
      throw AipsError (String(str.str()));
      return false;
    }
  }
  // each itsAxisPath value must be unique
  for (uint32_t k=0; k < (latticeDim - 1); k++) {
    for (uint32_t j=k+1; j < latticeDim; j++) {
      if (itsAxisPath(k) == itsAxisPath(j)) {
	str << "axis path " << itsAxisPath
	    << " does not have unique elements";
	throw AipsError (String(str.str()));
	return false;
      }
    }
  }
  // Check the LatticeIndexer is OK
  if (itsIndexer.ok() == false) {
    str << "LatticeIndexer thinks things are bad";
    throw AipsError (String(str.str()));
    return false;
  }
  // Check if itsNiceFit is correct.
  if (itsNiceFit != niceFit()) {
    str << "itsNiceFit " << itsNiceFit
	<< " is inconsistent with niceFit()";
    throw AipsError (String(str.str()));
    return false;
  }
  // Otherwise it has passed all the tests
  return true;
}

} //# NAMESPACE CASACORE - END

