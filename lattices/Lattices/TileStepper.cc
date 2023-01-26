//# TileStepper.cc: defines TileStepper class
//# Copyright (C) 1997,1999,2000,2001,2003
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

#include <casacore/lattices/Lattices/TileStepper.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/sstream.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

TileStepper::TileStepper(const IPosition& latticeShape, 
			 const IPosition& tileShape)
: itsBlc(latticeShape.nelements(), 0),
  itsTrc(latticeShape - 1),
  itsInc(latticeShape.nelements(), 1),
  itsSubSection(latticeShape),
  itsTiler(latticeShape),
  itsTilerCursorPos(latticeShape.nelements(), 0),
  itsTileShape(tileShape),
  itsAxisPath(latticeShape.nelements(), 0),
  itsCurBlc(latticeShape.nelements()),
  itsCurTrc(latticeShape.nelements()),
  itsNsteps(0),
  itsEnd(false),
  itsStart(true)
{
  const uint32_t nrdim = latticeShape.nelements();
  AlwaysAssert(nrdim > 0, AipsError);
  AlwaysAssert(tileShape.nelements() == nrdim, AipsError);
  for (uint32_t i=0; i<nrdim; i++) {
    itsAxisPath(i) = i;
  }
  reset();
  DebugAssert(ok() == true, AipsError);
}

TileStepper::TileStepper(const IPosition& latticeShape, 
			 const IPosition& tileShape,
			 const IPosition& axisPath)
: itsBlc(latticeShape.nelements(), 0),
  itsTrc(latticeShape - 1),
  itsInc(latticeShape.nelements(), 1),
  itsSubSection(latticeShape),
  itsTiler(latticeShape),
  itsTilerCursorPos(latticeShape.nelements(), 0),
  itsTileShape(tileShape),
  itsAxisPath(IPosition::makeAxisPath(latticeShape.nelements(), axisPath)),
  itsCurBlc(latticeShape.nelements()),
  itsCurTrc(latticeShape.nelements()),
  itsNsteps(0),
  itsEnd(false),
  itsStart(true)
{
  const uint32_t nrdim = latticeShape.nelements();
  AlwaysAssert(nrdim > 0, AipsError);
  AlwaysAssert(tileShape.nelements() == nrdim, AipsError);
  reset();
  DebugAssert(ok() == true, AipsError);
}

// the copy constructor which uses copy semantics.
TileStepper::TileStepper(const TileStepper& other)
: LatticeNavigator(),
  itsBlc(other.itsBlc),
  itsTrc(other.itsTrc),
  itsInc(other.itsInc),
  itsSubSection(other.itsSubSection),
  itsTiler(other.itsTiler),
  itsTilerCursorPos(other.itsTilerCursorPos),
  itsTileShape(other.itsTileShape),
  itsAxisPath(other.itsAxisPath),
  itsCurBlc(other.itsCurBlc),
  itsCurTrc(other.itsCurTrc),
  itsNsteps(other.itsNsteps),
  itsEnd(other.itsEnd),
  itsStart(other.itsStart)
{
  DebugAssert(ok() == true, AipsError);
}

TileStepper::~TileStepper()
{
  // does nothing
}

TileStepper& TileStepper::operator=(const TileStepper& other)
{
  if (this != &other) { 
    itsBlc = other.itsBlc;
    itsTrc = other.itsTrc;
    itsInc = other.itsInc;
    itsSubSection = other.itsSubSection;
    itsTiler = other.itsTiler;
    itsTilerCursorPos = other.itsTilerCursorPos;
    itsTileShape = other.itsTileShape;
    itsAxisPath = other.itsAxisPath;
    itsCurBlc = other.itsCurBlc;
    itsCurTrc = other.itsCurTrc;
    itsNsteps = other.itsNsteps;
    itsEnd = other.itsEnd;
    itsStart = other.itsStart;
  }
  DebugAssert(ok() == true, AipsError);
  return *this;
}

bool TileStepper::operator++(int)
{
  DebugAssert(ok() == true, AipsError);
  if (itsEnd) {
    return false;
  }
  itsStart = false;
  itsNsteps++;
  IPosition currentPos = itsTilerCursorPos;
  //# Move to the next tile.
  //# Set end-status if no more tiles.
  bool empty = true;
  while (empty) {
    if (! itsTiler.tiledCursorMove (true, itsTilerCursorPos,
				    itsTileShape, itsAxisPath)) {
      itsEnd = true;
      itsTilerCursorPos = currentPos;
      return false;
    }
    //# Calculate the boundaries of the tile.
    itsCurBlc = itsTiler.absolutePosition (itsTilerCursorPos);
    itsCurTrc = itsCurBlc + itsTileShape - 1;
//    cout << itsCurBlc << itsCurTrc << "   ";
    empty = false;
    //# Calculate the first and last pixel in the tile taking the
    //# increment into account.
    int32_t nrdim = itsCurBlc.nelements();
    for (int i=0; i<nrdim; i++) {
      if (itsCurTrc(i) > itsTrc(i)) {
	itsCurTrc(i) = itsTrc(i);
      }
      if (itsCurBlc(i) <= itsBlc(i)) {
	itsCurBlc(i) = itsBlc(i);
      }else{
	itsCurBlc(i) = (itsCurBlc(i) - itsBlc(i) + itsInc(i) - 1) / itsInc(i)
	               * itsInc(i) + itsBlc(i);
      }
      itsCurTrc(i) = (itsCurTrc(i) - itsBlc(i)) / itsInc(i)
	             * itsInc(i) + itsBlc(i);
      //# It is possible that the tile does not have any pixel at all
      //# (e.g. when increment > tileshape).
//	cout << itsCurBlc << itsCurTrc << endl;
      if (itsCurBlc(i) > itsCurTrc(i)) {
	empty = true;
	break;
      }
    }
  }
  DebugAssert(ok() == true, AipsError);
  return true;
}

bool TileStepper::operator--(int)
{
  DebugAssert(ok() == true, AipsError);
  if (itsStart) {
    return false;
  }
  itsEnd = false;
  itsNsteps++;
  IPosition currentPos = itsTilerCursorPos;
  //# Move to the previous tile.
  //# Set start-status if no more tiles.
  bool empty = true;
  while (empty) {
    if (! itsTiler.tiledCursorMove (false, itsTilerCursorPos,
				    itsTileShape, itsAxisPath)) {
      itsStart = true;
      itsTilerCursorPos = currentPos;
      return false;
    }
    //# Calculate the boundaries of the tile.
    itsCurBlc = itsTiler.absolutePosition (itsTilerCursorPos);
    itsCurTrc = itsCurBlc + itsTileShape - 1;
//    cout << itsCurBlc << itsCurTrc << "   ";
    empty = false;
    //# Calculate the first and last pixel in the tile taking the
    //# increment into account.
    int32_t nrdim = itsCurBlc.nelements();
    for (int i=0; i<nrdim; i++) {
      if (itsCurTrc(i) > itsTrc(i)) {
	itsCurTrc(i) = itsTrc(i);
      }
      if (itsCurBlc(i) <= itsBlc(i)) {
	itsCurBlc(i) = itsBlc(i);
      }else{
	itsCurBlc(i) = (itsCurBlc(i) - itsBlc(i) + itsInc(i) - 1) / itsInc(i)
	               * itsInc(i) + itsBlc(i);
      }
      itsCurTrc(i) = (itsCurTrc(i) - itsBlc(i)) / itsInc(i)
	             * itsInc(i) + itsBlc(i);
      //# It is possible that the tile does not have any pixel at all
      //# (e.g. when increment > tileshape).
//	cout << itsCurBlc << itsCurTrc << endl;
      if (itsCurBlc(i) > itsCurTrc(i)) {
	empty = true;
	break;
      }
    }
  }
  DebugAssert(ok() == true, AipsError);
  return true;
}

void TileStepper::reset()
{
  //# Make sure the tiler starts on a tile boundary.
  //# Set itsTiler subsection (its increment is always one).
  IPosition tilerBlc = itsBlc / itsTileShape * itsTileShape;
  IPosition tilerTrc = itsTrc;
  itsTiler.fullSize();
  itsTiler.subSection (tilerBlc, tilerTrc);
  itsTilerCursorPos = 0;
  //# Calculate the boundaries of the tile.
  itsCurBlc = itsTiler.absolutePosition (itsTilerCursorPos);
  itsCurTrc = itsCurBlc + itsTileShape - 1;
//  cout << itsCurBlc << itsCurTrc << "   ";
  //# Calculate the first and last pixel in the tile taking the
  //# increment into account.
  int32_t nrdim = itsCurBlc.nelements();
  for (int i=0; i<nrdim; i++) {
    if (itsCurTrc(i) > itsTrc(i)) {
      itsCurTrc(i) = itsTrc(i);
    }
    if (itsCurBlc(i) <= itsBlc(i)) {
      itsCurBlc(i) = itsBlc(i);
    }else{
      itsCurBlc(i) = (itsCurBlc(i) - itsBlc(i) + itsInc(i) - 1) / itsInc(i)
	             * itsInc(i) + itsBlc(i);
    }
    itsCurTrc(i) = (itsCurTrc(i) - itsBlc(i)) / itsInc(i)
	           * itsInc(i) + itsBlc(i);
//      cout << itsCurBlc << itsCurTrc << endl;
  }
  itsNsteps = 0;
  itsEnd = false;
  itsStart = true;
  DebugAssert(ok() == true, AipsError);
}

bool TileStepper::atStart() const
{
  DebugAssert(ok() == true, AipsError);
  return itsStart;
}

bool TileStepper::atEnd() const
{
  DebugAssert(ok() == true, AipsError);
  return itsEnd;
}

uint32_t TileStepper::nsteps() const
{
  DebugAssert(ok() == true, AipsError);
  return itsNsteps;
}

IPosition TileStepper::position() const
{
  DebugAssert(ok() == true, AipsError)
//  cout << "position = " << itsTiler.absolutePosition(itsTilerCursorPos)
//       << endl;
  return itsCurBlc;
}

IPosition TileStepper::endPosition() const
{
  DebugAssert(ok() == true, AipsError);
  return itsCurTrc;
}

IPosition TileStepper::latticeShape() const
{
  DebugAssert(ok() == true, AipsError);
  return itsSubSection.fullShape();
}

IPosition TileStepper::subLatticeShape() const
{
  DebugAssert(ok() == true, AipsError);
  return itsSubSection.shape();
}

IPosition TileStepper::cursorShape() const
{
  DebugAssert(ok() == true, AipsError);
  return (itsCurTrc - itsCurBlc) / itsInc + 1;
}

IPosition TileStepper::cursorAxes() const
{
  DebugAssert(ok() == true, AipsError);
  return itsAxisPath;
}

IPosition TileStepper::tileShape() const
{
  DebugAssert(ok() == true, AipsError);
  return itsTileShape;
}

bool TileStepper::hangOver() const
{
  return false;
}

// Function to specify a "section" of the Lattice to Navigate over. A
// section is defined in terms of the Bottom Left Corner (blc), Top Right
// Corner (trc), and step size (inc), on ALL of its axes, including
// degenerate axes.
void TileStepper::subSection (const IPosition& blc, const IPosition& trc, 
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
void TileStepper::subSection(const IPosition& blc, const IPosition& trc)
{
  subSection(blc, trc, IPosition(itsTiler.ndim(), 1));
}

// Return the bottom left hand corner of the current sub-Lattice. If no
// sub-Lattice has been defined return blc=0
IPosition TileStepper::blc() const
{
  DebugAssert(ok() == true, AipsError);
  return itsBlc;
}

// Return the top right hand corner of the current sub-Lattice. If no
// sub-Lattice has been defined return trc=latticeShape-1
IPosition TileStepper::trc() const
{
  DebugAssert(ok() == true, AipsError);
  return itsTrc;
}

// Return the step increment between the current sub-Lattice and the main
// Lattice. If no sub-Lattice has been defined return inc=1
IPosition TileStepper::increment() const
{
  DebugAssert(ok() == true, AipsError);
  return itsInc;
}

const IPosition& TileStepper::axisPath() const
{
  DebugAssert(ok() == true, AipsError);
  return itsAxisPath;
}

uint32_t TileStepper::calcCacheSize (const IPosition&,
                                 const IPosition&,
                                 uint32_t, uint32_t) const
{
  // Cache needs to be 1 tile only.
  return 1;
}

LatticeNavigator* TileStepper::clone() const
{
  DebugAssert(ok() == true, AipsError);
  return new TileStepper(*this);
}


bool TileStepper::ok() const
{
  ostringstream str;
  str << "TileStepper::ok - ";
  const uint32_t latticeDim = itsTiler.ndim();
  // Check the cursor shape is OK
  if (itsTileShape.nelements() != latticeDim) {
    str << "cursor shape " << itsTileShape
	<< " has wrong number of dimensions (ie. not "
	<< latticeDim << ')';
    throw AipsError (String(str.str()));
    return false;
  }
  for (uint32_t i=0; i < latticeDim; i++) {
    // the cursor shape must be <= the corresponding lattice axes AND
    // a cursor shape with an axis of length zero makes no sense
    if (itsTileShape(i) > int32_t(itsTiler.shape(i)) || itsTileShape(i) <= 0) {
      str << "cursor shape " << itsTileShape
	  << " is too big or small for lattice shape "
	  << itsTiler.shape();
      throw AipsError (String(str.str()));
      return false;
    }
  }
  // Check the cursor position is OK
  if (itsTilerCursorPos.nelements() != latticeDim) {
    str << "cursor position " << itsTilerCursorPos
	<< " has wrong number of dimensions (ie. not "
	<< latticeDim << ')';
    throw AipsError (String(str.str()));
    return false;
  }

  // cursor position or its "far corner" must be inside the (sub)-Lattice
  if (!(itsTiler.isInside(itsTilerCursorPos) ||
 	itsTiler.isInside(itsTilerCursorPos+itsTileShape-1))) {
    str << "cursor beginning " << itsTilerCursorPos
 	   << " or end " << itsTilerCursorPos + itsTileShape - 1
 	   << " is entirely outside the lattice shape "
 	   << itsTiler.shape();
    throw AipsError (String(str.str()));
    return false;
  }

  // check the Axis Path is OK
  if (itsAxisPath.nelements() != latticeDim) {
    str << "axis path " << itsAxisPath
	<< " has wrong number of dimensions (ie. not "
	<< latticeDim << ')';
    throw AipsError (String(str.str()));
    return false;
  }
  // each itsAxisPath value must be a lattice axis number, 0..n-1
  for (uint32_t n=0; n < latticeDim; n++) {
    if (itsAxisPath(n) >= int32_t(latticeDim)) {
      str << "axis path " << itsAxisPath
	  << " has elements bigger than the lattice dim -1 (ie. "
	  << latticeDim - 1 << ')';
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
  if (itsTiler.ok() == false) {
    str << "LatticeIndexer thinks things are bad";
    throw AipsError (String(str.str()));
    return false;
  }
  // Otherwise it has passed all the tests
  return true;
}

} //# NAMESPACE CASACORE - END

