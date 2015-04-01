//# LatticeIndexer.cc: A class for stepping through (sub-)Lattices
//# Copyright (C) 1994,1996,1997,2000,2001,2003
//# Associated Universities, Inc. Washington DC, USA.,1995
//# 
//# This program is free software; you can redistribute it and/or modify
//# it under the terms of the GNU General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or
//# (at your option) any later version.
//# 
//# This program is distributed in the hope that it will be useful,
//# but WITHOUT ANY WARRANTY; without even the implied warranty of
//# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//# GNU General Public License for more details.
//# 
//# You should have received a copy of the GNU General Public License
//# along with this program; if not, write to the Free Software
//# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
//# 
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$
//

#include <casacore/lattices/Lattices/LatticeIndexer.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/sstream.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

LatticeIndexer::LatticeIndexer()
: itsFullShape (IPosition(1,1)), 
  itsNdim      (1),
  itsShape     (IPosition(1,1)),
  itsAxisInc   (IPosition(1,1)),
  itsOffset    (IPosition(1,0))
{
  DebugAssert(ok() == True, AipsError);
}

// Specify the size of the Lattice. Assume a full size sub-Lattice. 
LatticeIndexer::LatticeIndexer (const IPosition& shape)
: itsFullShape (shape),
  itsNdim      (shape.nelements()), 
  itsShape     (shape),
  itsAxisInc   (shape.nelements(), 1),
  itsOffset    (shape.nelements(), 0)
{
  DebugAssert(ok() == True, AipsError);
}

// Specify a Lattice and define a sub-Lattice within it.
LatticeIndexer::LatticeIndexer (const IPosition& shape, const IPosition& blc, 
				const IPosition& trc, const IPosition& inc)
: itsFullShape (shape),
  itsNdim      (shape.nelements()), 
  itsShape     (shape),
  itsAxisInc   (shape.nelements(), 1),
  itsOffset    (shape.nelements(), 0)
{
  DebugAssert (ok() == True, AipsError);
  AlwaysAssert (blc.nelements() == itsNdim, AipsError);
  AlwaysAssert (trc.nelements() == itsNdim, AipsError);
  AlwaysAssert (inc.nelements() == itsNdim, AipsError);
  for (uInt i=0; i<itsNdim; i++) {
    AlwaysAssert (blc(i) >= 0  &&  blc(i) < itsFullShape(i), AipsError);
    AlwaysAssert (trc(i) < itsFullShape(i)  &&  trc(i) >= blc(i), AipsError);
    AlwaysAssert (inc(i) > 0  &&  inc(i) <= itsFullShape(i), AipsError);
  }
  itsOffset  = blc;
  itsAxisInc = inc;
  itsShape   = (trc - blc + inc) / inc; 
  DebugAssert (ok() == True, AipsError);
}

// Copy constructor. This uses copy semantics.
LatticeIndexer::LatticeIndexer (const LatticeIndexer& other)
: itsFullShape (other.itsFullShape),
  itsNdim      (other.itsNdim),
  itsShape     (other.itsShape), 
  itsAxisInc   (other.itsAxisInc), 
  itsOffset    (other.itsOffset)
{
  DebugAssert(ok() == True, AipsError);
}

// the destructor does nothing
LatticeIndexer::~LatticeIndexer()
{
  // does nothing
}

// Assignment operator. Uses copy semantics.
LatticeIndexer& LatticeIndexer::operator= (const LatticeIndexer& other)
{
  if (this != &other) {
    if (itsNdim != other.itsNdim) {
      itsNdim = other.itsNdim;
      itsFullShape.resize (itsNdim);
      itsShape.resize (itsNdim);
      itsAxisInc.resize (itsNdim);
      itsOffset.resize (itsNdim);
    }
    itsFullShape = other.itsFullShape;
    itsShape     = other.itsShape;
    itsAxisInc   = other.itsAxisInc;
    itsOffset    = other.itsOffset;
  }
  DebugAssert(ok() == True, AipsError);
  return *this;
}

// function to change the shape of the Lattice. Resets the sub-Lattice to
// fullsize.
void LatticeIndexer::resize (const IPosition& newShape)
{
  if (newShape.nelements() != itsNdim) {
    itsNdim = newShape.nelements();
    itsFullShape.resize (itsNdim);
    itsShape.resize (itsNdim);
    itsAxisInc.resize (itsNdim);
    itsOffset.resize (itsNdim);
  }
  itsFullShape = newShape;
  itsShape     = itsFullShape;
  itsAxisInc   = 1;
  itsOffset    = 0;
  DebugAssert(ok() == True, AipsError);
}

// Returns the length of the requested axis in the parent Lattice
uInt LatticeIndexer::fullShape (uInt axis) const
{
  DebugAssert (ok() == True, AipsError);
  AlwaysAssert (axis < itsNdim, AipsError);
  return itsFullShape(axis);
}

// Returns the length of the requested axis in the sub-Lattice
uInt LatticeIndexer::shape (uInt axis) const
{
  DebugAssert (ok() == True, AipsError);
  AlwaysAssert (axis < itsNdim, AipsError);
  return itsShape(axis);
}

// function to return the increments along the requested axis of the
// Lattice.
uInt LatticeIndexer::increment (uInt axis) const
{
  DebugAssert (ok() == True, AipsError);
  AlwaysAssert (axis < itsNdim, AipsError);
  return itsAxisInc(axis);
}

// function to return the offset on the specified axes between the
// sub-Lattice and the parent one.
uInt LatticeIndexer::offset (uInt axis) const
{
  DebugAssert (ok() == True, AipsError);
  AlwaysAssert (axis < itsNdim, AipsError);
  return itsOffset(axis);
}

// Revert from a sub-Lattice description back to the main Lattice. This is
// the only way to "increase" the the size of the sub-Lattice used by the
// LatticeIndexer.
void LatticeIndexer::fullSize()
{
  itsShape   = itsFullShape;
  itsAxisInc = 1;
  itsOffset  = 0;
  DebugAssert (ok() == True, AipsError);
}

// function which increments (incr=True) or decrements (incr=False) the
// cursor position (the first IPosition argument) by a cursor shape (the
// second IPosition argument), tiling to the next/previous axis if
// necessary.  The path of movement is based upon the third IPosition
// argument (a cursor heading) that is zero-based e.g. IPosition(3,0,2,1)
// implies starting movement along the x-axis, then the z-axis, and then
// the y-axis.  Returns a value of False if the beginning/end of the
// sub-Lattice is reached. The cursorPosition is relative to the origin of
// the sub-Lattice. To get its location relative to the main Lattice use
// the absolutePosition() function. 
Bool LatticeIndexer::tiledCursorMove (Bool incr, IPosition& cursorPos, 
				      const IPosition& cursorShape,
				      const IPosition& cursorHeading) const
{
  // this function performs IPosition addition/subtraction
  // ie. cursorPos += cursorShape but it makes sure that some pixels are
  // within the this Layout's shape (hereafter known as "boundary").  It
  // wraps (or "carries" to use the grade school arithmetic term) the result
  // onto the next axis when necessary, and returns true or false depending
  // on whether or not the addition worked.  Failure occurs when the
  // addition/subtraction would move the cursor so that no pixels in it are
  // within the boundary, and axis-wrapping has been exhaused.
  
  // some key local variables:
  // activeAxis:
  //    in incrementing [0,0] by [2,2] to produce [2,0], the zeroth
  //    axis is "active".  if the resulting cursor overflows the underlying
  //    lattice, then 1st axis becomes the activeAxis.  (this can go on
  //    until the cursor fits, or until all the axes are exhausted.)
  // candidateCursorPos:
  //    preliminary value for cursorPos += congruentCursorShape.
  //    this is the "base" or "bottomLeftCorner" of the new cursor.

  DebugAssert (ok() == True, AipsError);
  AlwaysAssert (cursorPos.nelements() == itsNdim, AipsError);
  AlwaysAssert (cursorShape.nelements() == itsNdim, AipsError);
  AlwaysAssert (cursorHeading.nelements() == itsNdim,AipsError);
  for (uInt i=0; i<itsNdim; i++) {
    AlwaysAssert (cursorShape(i) > 0, AipsError);
  }
  uInt activeAxis;
  uInt indexToActiveAxis = 0;
  IPosition candidateCursorPos(cursorPos);

  while (indexToActiveAxis < itsNdim) {
    activeAxis = cursorHeading(indexToActiveAxis);
    if (incr) {
      candidateCursorPos(activeAxis) += cursorShape(activeAxis);
    } else {
      candidateCursorPos(activeAxis) -= cursorShape(activeAxis);
    }
    if ((candidateCursorPos(activeAxis) < itsShape(activeAxis))  &&
	(candidateCursorPos(activeAxis) + cursorShape(activeAxis) > 0)) {
      cursorPos = candidateCursorPos;
      return True;
    }
    if (incr) {
      candidateCursorPos(activeAxis) -= 
	((candidateCursorPos(activeAxis) + cursorShape(activeAxis) - 1)
	 / cursorShape(activeAxis))
	* cursorShape(activeAxis);
    } else {
      candidateCursorPos(activeAxis) +=
	((itsShape(activeAxis) - candidateCursorPos(activeAxis) - 1)
	 / cursorShape(activeAxis))
	* cursorShape(activeAxis);
    }
    indexToActiveAxis++;
  } // while 
  return False;
}

// function which returns a value of True if the IPosition argument
// is within the sub-Lattice.  Returns False if the IPosition argument is 
// outside the sub-Lattice or if the argument doesn't conform to the 
// data members.
Bool LatticeIndexer::isInside (const IPosition& index) const
{
  DebugAssert (ok() == True, AipsError);
  AlwaysAssert (index.nelements () == itsNdim, AipsError);
  for (uInt i=0; i<itsNdim; i++) {
    if ((index(i) < 0) || (index(i) >= itsShape(i))) {
      return False;
    }
  }
  return True;
}

// function which subsections a LatticeIndexer.  The argument IPositions
// specify "bottom left" and "upper right" corners and axis increments
// (which default to one).  The origins are cumulative. i.e. specifying a
// blc of (2,2), and then (1,1) results in the sub-Lattice having an
// origin at pixel (3,3) in the parent Lattice. Similarly the increment is
// cumulative, i.e. an increment of 2 on top of an increment of 3 results
// in a total increment of 6. This function can only decrease the size of
// the sub-Lattice (i.e. blc >= 0, and trc <= shape(), and inc >= 1). The
// fullSize() function should be used to revert back to the maximum
// possible Lattice size.  Also note that the trc might not be used if an
// integral number of increments does not end on the trc (in which case
// the last position below the trc will be used).
void LatticeIndexer::subSection (const IPosition& blc, const IPosition& trc,
				 const IPosition& inc)
{
  DebugAssert (ok() == True, AipsError);
  AlwaysAssert (blc.nelements() == itsNdim, AipsError);
  AlwaysAssert (trc.nelements() == itsNdim, AipsError);
  AlwaysAssert (inc.nelements() == itsNdim, AipsError);
  for (uInt i=0; i<itsNdim; i++) {
    AlwaysAssert (blc(i) >= 0, AipsError);
    AlwaysAssert (trc(i) < itsShape(i), AipsError);
    AlwaysAssert (blc(i) <= trc(i), AipsError);
    AlwaysAssert (inc(i) > 0  &&  inc(i) <= itsShape(i), AipsError);
  }
  itsShape   = (trc-blc+inc) / inc;
  itsOffset  = itsOffset + blc*itsAxisInc;
  itsAxisInc = itsAxisInc*inc;
  DebugAssert (ok() == True, AipsError);
}

void LatticeIndexer::subSection (const IPosition& blc, const IPosition& trc)
{
  DebugAssert (ok() == True, AipsError);
  subSection (blc, trc, IPosition(itsNdim, 1));
}

// function which returns an IPosition in the parent Lattice given an
// IPostion in the sub-Lattice.  Accounting is taken of any offsets and
// increments caused by subSectioning. No checks are made to ensure the
// supplied IPosition or the returned one are within the bounds of the
// Lattice(s).
IPosition LatticeIndexer::absolutePosition (const IPosition& position) const
{
  DebugAssert (ok() == True, AipsError);
  AlwaysAssert (position.nelements () == itsNdim, AipsError);
  return itsOffset + position*itsAxisInc;
}


// function which returns True if all the elements in this 
// sub-Lattice, are arranged contiguously, 
// i.e. without any gaps caused by increments or subSectioning.
// THIS FUNCTION IS NOT FINISHED YET.
// Bool LatticeIndexer::isContiguous() const
// {
//   DebugAssert(ok() == True, AipsError);
//   Bool checkDegenerate = False;
//   for (uInt i=0; i < itsNdim; i++) {
//     if (itsAxisInc(i) > 1) 
//       checkDegenerate = True;
//     if (itsOffset(i) != 0  &&  i != 0)
//       return False;
//     if (checkDegenerate  &&  shape(i) > 1)
//       return False;
//   }
//   return True;
// }



// Is this LatticeIndexer consistent, i.e. are the class invariants valid?
// return True if every thing is fine otherwise return False
Bool LatticeIndexer::ok() const
{
  ostringstream str;
  str << "LatticeIndexer::ok - ";
  if (itsNdim == 0) {
    str << "zero dimensions";
    throw AipsError (str.str());
    return False;
  }
  if (itsFullShape.nelements() != itsNdim) {
    str << "lattice has "
	<< itsFullShape.nelements() << " instead of "
	<< itsNdim << " dimensions";
    throw AipsError (str.str());
    return False;
  }
  for (uInt i=0; i < itsNdim; i++) {
    if (itsFullShape(i) < 0) {
      str << "lattice shape " << itsFullShape
	  << " has a negative element";
      throw AipsError (str.str());
      return False;
    }
  }
  if (itsAxisInc.nelements() != itsNdim) {
    str << "increments " << itsAxisInc
	<< " are the wrong dimension (ie. not " 
	<< itsNdim << ')';
      throw AipsError (String(str.str()));
    return False;
  }
  for (uInt j=0; j < itsNdim; j++) {
    if (itsAxisInc(j) <= 0 || itsAxisInc(j) > itsFullShape(j)) {
      str << "axis increments " << itsAxisInc
	  << " are negative OR larger than lattice shape "
	  << itsFullShape;
      throw AipsError (String(str.str()));
      return False;
    }
  }
  if (itsOffset.nelements() != itsNdim) {
    str << "offset " << itsOffset
	<< " is the wrong dimension (ie. not "
	<< itsNdim << ')';
      throw AipsError (String(str.str()));
    return False;
  }
  for (uInt k=0; k < itsNdim; k++) {
    if (itsOffset(k) < 0 || itsOffset(k) >= itsFullShape(k)) {
      str << "offset " << itsOffset
	  << " is larger than lattice shape "
	  << itsFullShape << " or negative";
      throw AipsError (String(str.str()));
      return False;
    }
  }
  if (itsShape.nelements() != itsNdim) {
    str << "sub-lattice shape " << itsShape
	<< " has wrong number of dimensions (ie. not "
	<< itsNdim << ')';
      throw AipsError (String(str.str()));
    return False;
  }
  for (uInt m=0; m < itsNdim; m++) {
    if (itsShape(m) <= 0 || itsShape > itsFullShape(m)) {
      str << "sub-lattice shape " << itsShape
	  << " is less than or equal to zero or larger than lattice shape "
	  << itsFullShape;
      throw AipsError (String(str.str()));
      return False;
    }
  }
  return True;
}

} //# NAMESPACE CASACORE - END

