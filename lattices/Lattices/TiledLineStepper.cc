//# TiledLineStepper.cc: defines TiledLineStepper class
//# Copyright (C) 1994,1995,1996,1997,1999,2000,2001,2003
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

#include <casacore/lattices/Lattices/TiledLineStepper.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/sstream.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

TiledLineStepper::TiledLineStepper (const IPosition& latticeShape, 
				    const IPosition& tileShape,
				    const uInt axis)
: itsBlc(latticeShape.nelements(), 0),
  itsTrc(latticeShape - 1),
  itsInc(latticeShape.nelements(), 1),
  itsSubSection(latticeShape),
  itsIndexer(latticeShape),
  itsTiler(latticeShape),
  itsIndexerCursorPos(latticeShape.nelements(), 0),
  itsTilerCursorPos(latticeShape.nelements(), 0),
  itsCursorShape(latticeShape.nelements(), 1),
  itsTileShape(tileShape),
  itsAxisPath(latticeShape.nelements(), 0),
  itsNsteps(0),
  itsAxis(axis),
  itsEnd(False),
  itsStart(True)
{
  const uInt nrdim = latticeShape.nelements();
  AlwaysAssert(nrdim > 0, AipsError);
  AlwaysAssert(tileShape.nelements() == nrdim, AipsError);
  AlwaysAssert(axis < nrdim, AipsError);
  uInt i;
  for (i=0; i<itsAxis; i++) {
    itsAxisPath(i) = i;
  }
  for (i=itsAxis; i<nrdim-1; i++) {
    itsAxisPath(i) = i+1;
  }
  itsAxisPath(nrdim-1) = itsAxis;
  reset();
  DebugAssert(ok() == True, AipsError);
}

// the copy constructor which uses copy semantics.
TiledLineStepper::TiledLineStepper (const TiledLineStepper& other)
: LatticeNavigator(),
  itsBlc(other.itsBlc),
  itsTrc(other.itsTrc),
  itsInc(other.itsInc),
  itsSubSection(other.itsSubSection),
  itsIndexer(other.itsIndexer),
  itsTiler(other.itsTiler),
  itsIndexerCursorPos(other.itsIndexerCursorPos),
  itsTilerCursorPos(other.itsTilerCursorPos),
  itsCursorShape(other.itsCursorShape),
  itsTileShape(other.itsTileShape),
  itsAxisPath(other.itsAxisPath),
  itsNsteps(other.itsNsteps),
  itsAxis(other.itsAxis),
  itsEnd(other.itsEnd),
  itsStart(other.itsStart)
{
  DebugAssert(ok() == True, AipsError);
}

TiledLineStepper::~TiledLineStepper()
{
  // does nothing
}

TiledLineStepper& TiledLineStepper::operator= (const TiledLineStepper& other)
{
  if (this != &other) { 
    itsBlc = other.itsBlc;
    itsTrc = other.itsTrc;
    itsInc = other.itsInc;
    itsSubSection = other.itsSubSection;
    itsIndexer = other.itsIndexer;
    itsTiler = other.itsTiler;
    itsIndexerCursorPos = other.itsIndexerCursorPos;
    itsTilerCursorPos = other.itsTilerCursorPos;
    itsCursorShape = other.itsCursorShape;
    itsTileShape = other.itsTileShape;
    itsAxisPath = other.itsAxisPath;
    itsNsteps = other.itsNsteps;
    itsAxis = other.itsAxis;
    itsEnd = other.itsEnd;
    itsStart = other.itsStart;
  }
  DebugAssert(ok() == True, AipsError);
  return *this;
}

Bool TiledLineStepper::operator++(int)
{
  DebugAssert(ok() == True, AipsError);
  if (itsEnd) {
    return False;
  }
  itsStart = False;
  itsNsteps++;
  IPosition currentPos = itsIndexerCursorPos;
  //# Move to the next position in the tile.
  //# If at the end of the tile, move to the next tile.
  if (itsIndexer.tiledCursorMove (True, itsIndexerCursorPos, 
				  itsCursorShape, itsAxisPath)) {
    return True;
  }
  //# Move to the next tile.
  //# Set end-status if no more tiles.
  IPosition tilerPos = itsTilerCursorPos;
  while (!itsEnd) {
    if (! itsTiler.tiledCursorMove (True, itsTilerCursorPos,
				    itsTileShape, itsAxisPath)) {
      itsEnd = True;
      itsIndexerCursorPos = currentPos;
      itsTilerCursorPos   = tilerPos;
      return False;
    }
    //# Calculate the boundaries of the tile.
    IPosition tileblc = itsTiler.absolutePosition (itsTilerCursorPos);
    IPosition tiletrc = tileblc + itsTileShape - 1;
    tileblc(itsAxis) = itsBlc(itsAxis);
    tiletrc(itsAxis) = itsTrc(itsAxis);
    Bool empty = False;
    //# Calculate the first and last pixel in the tile taking the
    //# increment into account.
    uInt nrdim = tileblc.nelements();
    for (uInt i=0; i<nrdim; i++) {
      if (i != itsAxis) {
	if (tiletrc(i) > itsTrc(i)) {
	  tiletrc(i) = itsTrc(i);
	}
	if (tileblc(i) <= itsBlc(i)) {
	  tileblc(i) = itsBlc(i);
	}else{
	  tileblc(i) = (tileblc(i) - itsBlc(i) + itsInc(i) - 1) / itsInc(i)
	               * itsInc(i) + itsBlc(i);
	}
	tiletrc(i) = (tiletrc(i) - itsBlc(i)) / itsInc(i)
	             * itsInc(i) + itsBlc(i);
	//# It is possible that the tile does not have any pixel at all
	//# (e.g. when increment > tileshape).
	if (tileblc(i) > tiletrc(i)) {
	  empty = True;
	  break;
	}
      }
    }
    //# When pixels in this tile, set to the first pixel.
    if (!empty) {
      itsIndexer.fullSize();
      itsIndexer.subSection (tileblc, tiletrc, itsInc);
      itsIndexerCursorPos = 0;
      return True;
    }
  }
  DebugAssert(ok() == True, AipsError);
  return False;
}

Bool TiledLineStepper::operator--(int)
{
  DebugAssert(ok() == True, AipsError);
  if (itsStart) {
    return False;
  }
  itsEnd = False;
  itsNsteps++;
  IPosition currentPos = itsIndexerCursorPos;
  //# Move to the previous position in the tile.
  //# If at the beginning of the tile, move to the previous tile.
  if (itsIndexer.tiledCursorMove (False, itsIndexerCursorPos, 
				  itsCursorShape, itsAxisPath)) {
    return True;
  }
  //# Move to the previous tile.
  //# Set start-status if no more tiles.
  IPosition tilerPos = itsTilerCursorPos;
  while (!itsStart) {
    if (! itsTiler.tiledCursorMove (False, itsTilerCursorPos,
				    itsTileShape, itsAxisPath)) {
      itsStart = True;
      itsIndexerCursorPos = currentPos;
      itsTilerCursorPos   = tilerPos;
      return False;
    }
    //# Calculate the boundaries of the tile.
    IPosition tileblc = itsTiler.absolutePosition (itsTilerCursorPos);
    IPosition tiletrc = tileblc + itsTileShape - 1;
    tileblc(itsAxis) = itsBlc(itsAxis);
    tiletrc(itsAxis) = itsTrc(itsAxis);
    Bool empty = False;
    //# Calculate the first and last pixel in the tile taking the
    //# increment into account.
    uInt nrdim = tileblc.nelements();
    for (uInt i=0; i<nrdim; i++) {
      if (i != itsAxis) {
	if (tiletrc(i) > itsTrc(i)) {
	  tiletrc(i) = itsTrc(i);
	}
	if (tileblc(i) <= itsBlc(i)) {
	  tileblc(i) = itsBlc(i);
	}else{
	  tileblc(i) = (tileblc(i) - itsBlc(i) + itsInc(i) - 1) / itsInc(i)
	               * itsInc(i) + itsBlc(i);
	}
	tiletrc(i) = (tiletrc(i) - itsBlc(i)) / itsInc(i)
	             * itsInc(i) + itsBlc(i);
	//# It is possible that the tile does not have any pixel at all
	//# (e.g. when increment > tileshape).
	if (tileblc(i) > tiletrc(i)) {
	  empty = True;
	  break;
	}
      }
    }
    //# When pixels in this tile, set to the first pixel.
    if (!empty) {
      itsIndexer.fullSize();
      itsIndexer.subSection (tileblc, tiletrc, itsInc);
      itsIndexerCursorPos = (tiletrc - tileblc) / itsInc;
      itsIndexerCursorPos(itsAxis) = 0;
      return True;
    }
  }
  DebugAssert(ok() == True, AipsError);
  return False;
}

void TiledLineStepper::reset()
{
  //# Make sure the tiler starts on a tile boundary.
  //# Set itsTiler subsection (its increment is always one).
  //# For itsTiler we are not interested in the length of itsAxis axis,
  //# so make it the tile shape for convenience (but not exceeding lattice).
  IPosition tilerBlc = itsBlc / itsTileShape * itsTileShape;
  IPosition tilerTrc = itsTrc;
  tilerTrc(itsAxis) = std::min(latticeShape()(itsAxis) - 1,
                               tilerBlc(itsAxis) + itsTileShape(itsAxis) - 1);
  itsTiler.fullSize();
  itsTiler.subSection (tilerBlc, tilerTrc);
  itsTilerCursorPos = 0;
  itsCursorShape(itsAxis) = 1 + (itsTrc(itsAxis) - itsBlc(itsAxis))
                            / itsInc(itsAxis);
  //# Calculate the boundaries of the tile.
  IPosition tileblc = itsTiler.absolutePosition (itsTilerCursorPos);
  IPosition tiletrc = tileblc + itsTileShape - 1;
  tileblc(itsAxis) = itsBlc(itsAxis);
  tiletrc(itsAxis) = itsTrc(itsAxis);
  //# Calculate the first and last pixel in the tile taking the
  //# increment into account.
  uInt nrdim = tileblc.nelements();
  for (uInt i=0; i<nrdim; i++) {
    if (i != itsAxis) {
      if (tiletrc(i) > itsTrc(i)) {
	tiletrc(i) = itsTrc(i);
      }
      if (tileblc(i) <= itsBlc(i)) {
	tileblc(i) = itsBlc(i);
      }else{
        tileblc(i) = (tileblc(i) - itsBlc(i) + itsInc(i) - 1) / itsInc(i)
	             * itsInc(i) + itsBlc(i);
      }
      tiletrc(i) = (tiletrc(i) - itsBlc(i)) / itsInc(i)
	           * itsInc(i) + itsBlc(i);
    }
  }
  itsIndexer.fullSize();
  itsIndexer.subSection (tileblc, tiletrc, itsInc);
  itsIndexerCursorPos = 0;
  itsNsteps = 0;
  itsEnd = False;
  itsStart = True;
  DebugAssert(ok() == True, AipsError);
}

Bool TiledLineStepper::atStart() const
{
  DebugAssert(ok() == True, AipsError);
  return itsStart;
}

Bool TiledLineStepper::atEnd() const
{
  DebugAssert(ok() == True, AipsError);
  return itsEnd;
}

uInt TiledLineStepper::nsteps() const
{
  DebugAssert(ok() == True, AipsError);
  return itsNsteps;
}

IPosition TiledLineStepper::position() const
{
  DebugAssert(ok() == True, AipsError);
  return itsIndexer.absolutePosition (itsIndexerCursorPos);
}

IPosition TiledLineStepper::endPosition() const
{
  DebugAssert(ok() == True, AipsError);
  IPosition last = itsIndexerCursorPos;
  last(itsAxis) += (itsCursorShape(itsAxis) - 1) * itsInc(itsAxis);
  return itsIndexer.absolutePosition (last);
}

IPosition TiledLineStepper::latticeShape() const
{
  DebugAssert(ok() == True, AipsError);
  return itsSubSection.fullShape();
}

IPosition TiledLineStepper::subLatticeShape() const
{
  DebugAssert(ok() == True, AipsError);
  return itsSubSection.shape();
}

IPosition TiledLineStepper::cursorShape() const
{
  DebugAssert(ok() == True, AipsError);
  return itsCursorShape;
}

IPosition TiledLineStepper::cursorAxes() const
{
  DebugAssert(ok() == True, AipsError);
  return IPosition(1, itsAxis);
}

IPosition TiledLineStepper::tileShape() const
{
  DebugAssert(ok() == True, AipsError);
  return itsTileShape;
}

Bool TiledLineStepper::hangOver() const
{
  return False;
}

// Function to specify a "section" of the Lattice to Navigate over. A
// section is defined in terms of the Bottom Left Corner (blc), Top Right
// Corner (trc), and step size (inc), on ALL of its axes, including
// degenerate axes.
void TiledLineStepper::subSection (const IPosition& blc,
				   const IPosition& trc, 
				   const IPosition& inc)
{
  itsSubSection.subSection (blc, trc, inc);
  itsBlc = itsSubSection.offset();
  itsInc = itsSubSection.increment();
  itsTrc = itsBlc + (itsSubSection.shape() - 1) * itsInc;
  reset();
}

// Function to specify a "section" of the Lattice to Navigate over. The step
// increment is assumed to be one. 
void TiledLineStepper::subSection (const IPosition& blc,
				   const IPosition& trc)
{
  subSection(blc, trc, IPosition(itsIndexer.ndim(), 1));
}

// Return the bottom left hand corner of the current sub-Lattice. If no
// sub-Lattice has been defined return blc=0
IPosition TiledLineStepper::blc() const
{
  DebugAssert(ok() == True, AipsError);
  return itsBlc;
}

// Return the top right hand corner of the current sub-Lattice. If no
// sub-Lattice has been defined return trc=latticeShape-1
IPosition TiledLineStepper::trc() const
{
  DebugAssert(ok() == True, AipsError);
  return itsTrc;
}

// Return the step increment between the current sub-Lattice and the main
// Lattice. If no sub-Lattice has been defined return inc=1
IPosition TiledLineStepper::increment() const
{
  DebugAssert(ok() == True, AipsError);
  return itsInc;
}

const IPosition& TiledLineStepper::axisPath() const
{
  DebugAssert(ok() == True, AipsError);
  return itsAxisPath;
}

uInt TiledLineStepper::calcCacheSize (const IPosition&,
                                      const IPosition& tileShape,
                                      uInt, uInt bucketSize) const
{
  if (bucketSize == 0) {
    return 0;
  }
  // Tile by tile is accessed, but the main axis needs the entire window.
  // So calculate the start and end tile for the window.
  Int tilesz = tileShape(itsAxis);
  Int stTile = itsBlc(itsAxis) / tilesz;
  Int endTile = itsTrc(itsAxis) / tilesz;
  return (endTile - stTile + 1);
}

LatticeNavigator* TiledLineStepper::clone() const
{
  DebugAssert(ok() == True, AipsError);
  return new TiledLineStepper(*this);
}


Bool TiledLineStepper::ok() const
{
  ostringstream str;
  str << "TiledLineStepper::ok - ";
  const uInt tilerDim = itsTiler.ndim();
  for (uInt i=0; i < tilerDim; i++) {
    // the cursor shape must be <= the corresponding lattice axes AND
    // a cursor shape with an axis of length zero makes no sense
    if (itsTileShape(i) > Int(itsTiler.shape(i)) || itsTileShape(i) <= 0) {
      str << "tiler cursor shape " << itsTileShape
	  << " is too big or small for lattice shape "
	  << itsTiler.shape();
      throw AipsError (String(str.str()));
      return False;
    }
  }
  // Check the cursor position is OK
  if (itsTilerCursorPos.nelements() != tilerDim) {
    str << "tiler cursor position " << itsTilerCursorPos
	<< " has wrong number of dimensions (ie. not "
	<< tilerDim << ')' ;
    throw AipsError (String(str.str()));
    return False;
  }

  // cursor position or its "far corner" must be inside the (sub)-Lattice
  if (!(itsTiler.isInside(itsTilerCursorPos) ||
 	itsTiler.isInside(itsTilerCursorPos+itsTileShape-1))) {
    str << "tiler cursor beginning " << itsTilerCursorPos
	<< " or end " << itsTilerCursorPos + itsTileShape - 1
	<< " is entirely outside the lattice shape "
	<< itsTiler.shape();
    throw AipsError (String(str.str()));
    return False;
  }

  const uInt latticeDim = itsIndexer.ndim();
  // Check the cursor shape is OK
  if (itsCursorShape.nelements() != latticeDim) {
    str << "cursor shape " << itsCursorShape
	<< " has wrong number of dimensions (ie. not "
	<< latticeDim << ')';
    throw AipsError (String(str.str()));
    return False;
  }
  for (uInt i=0; i < latticeDim; i++) {
    // the cursor shape must be <= the corresponding lattice axes AND
    // a cursor shape with an axis of length zero makes no sense
    if (itsCursorShape(i) > Int(itsIndexer.shape(i))
        || itsCursorShape(i) <= 0) {
      str << "cursor shape " << itsCursorShape
	  << " is too big or small for lattice shape "
	  << itsIndexer.shape();
      throw AipsError (String(str.str()));
      return False;
    }
  }
  // Check the cursor position is OK
  if (itsIndexerCursorPos.nelements() != latticeDim) {
    str << "cursor position " << itsIndexerCursorPos
	<< " has wrong number of dimensions (ie. not "
	<< latticeDim << ')';
    throw AipsError (String(str.str()));
    return False;
  }
  
  // cursor position or its "far corner" must be inside the (sub)-Lattice
  if (!(itsIndexer.isInside(itsIndexerCursorPos) ||
	itsIndexer.isInside(itsIndexerCursorPos+itsCursorShape-1))) {
    str << "cursor beginning " << itsIndexerCursorPos
	<< " or end " << itsIndexerCursorPos + itsCursorShape - 1
	<< " is entirely outside the lattice shape "
	<< itsIndexer.shape();
    throw AipsError (String(str.str()));
    return False;
  }

  // check the Axis Path is OK
  if (itsAxisPath.nelements() != latticeDim) {
    str << "axis path " << itsAxisPath
	<< " has wrong number of dimensions (ie. not "
	<< latticeDim << ')';
    throw AipsError (String(str.str()));
    return False;
  }
  // each itsAxisPath value must be a lattice axis number, 0..n-1
  for (uInt n=0; n < latticeDim; n++) {
    if (itsAxisPath(n) >= Int(latticeDim)) {
      str << "axis path " << itsAxisPath
	  << " has elements bigger than the lattice dim -1 (ie. "
	  << latticeDim - 1 << ')';
      throw AipsError (String(str.str()));
      return False;
    }
  }

  // each itsAxisPath value must be unique
  for (uInt k=0; k < (latticeDim - 1); k++) {
    for (uInt j=k+1; j < latticeDim; j++) {
      if (itsAxisPath(k) == itsAxisPath(j)) {
	str << "axis path " << itsAxisPath
	    << " does not have unique elements";
	throw AipsError (String(str.str()));
	return False;
      }
    }
  }
  // Check the LatticeIndexers are OK
  if (itsIndexer.ok() == False) {
    str << "LatticeIndexer thinks things are bad";
    throw AipsError (String(str.str()));
    return False;
  }
  if (itsTiler.ok() == False) {
    str<< "itsTiler thinks things are bad";
    throw AipsError (String(str.str()));
    return False;
  }
  // Check the LatticeIndexer is OK
  if (itsIndexer.ok() == False) {
    str << "itsIndexer thinks things are bad";
    throw AipsError (String(str.str()));
    return False;
  }
  // Otherwise it has passed all the tests
  return True;
}

} //# NAMESPACE CASACORE - END

