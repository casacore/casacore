//# LatticeIndexer.cc: A class for stepping through (sub-)Lattices
//# Copyright (C) 1994,1996,1997
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

#include <trial/Lattices/LatticeIndexer.h>
#include <aips/Exceptions/Error.h>
#include <aips/Logging/LogIO.h>
#include <aips/Logging/LogOrigin.h>
#include <aips/Utilities/Assert.h>

// default constructor (one dimensional, unit-length instance)
LatticeIndexer::LatticeIndexer()
  :theFullShape(IPosition(1,1)), 
   theNdim(theFullShape.nelements()), 
   theShape(theFullShape),
   theAxisInc(IPosition(theNdim,1)),
   theOffset(IPosition(theNdim,0))
{
  DebugAssert(ok() == True, AipsError);
};

// Specify the size of the Lattice. Assume a full size sub-Lattice. 
LatticeIndexer::LatticeIndexer(const IPosition &shape)
  :theFullShape(shape),
   theNdim(theFullShape.nelements()), 
   theShape(theFullShape),
   theAxisInc(theNdim,1),
   theOffset(theNdim,0)
{
  DebugAssert(ok() == True, AipsError);
};

// Specify a Lattice and define a sub-Lattice within it.
LatticeIndexer::LatticeIndexer(const IPosition &shape, const IPosition &blc, 
			       const IPosition &trc, const IPosition &inc)
  :theFullShape(shape),
   theNdim(theFullShape.nelements()), 
   theShape(theFullShape),
   theAxisInc(theNdim, 1),
   theOffset(theNdim, 0)
{
  DebugAssert(ok() == True, AipsError);
  DebugAssert(blc.nelements() == theNdim, AipsError);
  DebugAssert(trc.nelements() == theNdim, AipsError);
  DebugAssert(inc.nelements() == theNdim, AipsError);
  for (uInt i=0; i < theNdim; i++) {
    DebugAssert(blc(i) >= 0 && blc(i) < theFullShape(i), AipsError);
    DebugAssert(trc(i) < theFullShape(i) && trc(i) >= blc(i), AipsError);
    DebugAssert(inc(i) > 0 && inc(i) <= theFullShape(i), AipsError);
  }
  theOffset = blc;
  theAxisInc = inc;
  theShape = (trc - blc + inc)/inc; 
  DebugAssert(ok() == True, AipsError);
};

// Copy constructor. This uses copy semantics.
LatticeIndexer::LatticeIndexer (const LatticeIndexer &other)
  :theFullShape(other.theFullShape),
   theNdim(other.theNdim),
   theShape(other.theShape), 
   theAxisInc(other.theAxisInc), 
   theOffset(other.theOffset)
{
  DebugAssert(ok() == True, AipsError);
};

// the destructor does nothing
LatticeIndexer::~LatticeIndexer()
{
  // does nothing
};

// Assignment operator. Uses copy semantics.
LatticeIndexer &LatticeIndexer::operator=(const LatticeIndexer &other)
{
  if (this != &other) {
    if (theNdim != other.theNdim) {
      theNdim = other.theNdim;
      theFullShape.resize(theNdim);
      theShape.resize(theNdim);
      theAxisInc.resize(theNdim);
      theOffset.resize(theNdim);
    }
    theFullShape = other.theFullShape;
    theShape = other.theShape;
    theAxisInc = other.theAxisInc;
    theOffset = other.theOffset;
  }
  DebugAssert(ok() == True, AipsError);
  return *this;
};

// function to change the shape of the Lattice. Resets the sub-Lattice to
// fullsize.
void LatticeIndexer::resize(const IPosition &newShape)
{
  if (newShape.nelements() != theNdim) {
    theNdim = newShape.nelements();
    theFullShape.resize(theNdim);
    theShape.resize(theNdim);
    theAxisInc.resize(theNdim);
    theOffset.resize(theNdim);
  }
  theFullShape = newShape;
  theShape = theFullShape;
  theAxisInc = 1;
  theOffset = 0;
  DebugAssert(ok() == True, AipsError);
};

// Returns the length of each axis in the parent Lattice
const IPosition &LatticeIndexer::fullShape() const
{
  DebugAssert(ok() == True, AipsError);
  return theFullShape;
};

// Returns the length of the requested axis in the parent Lattice
uInt LatticeIndexer::fullShape(uInt axis) const
{
  DebugAssert(ok() == True, AipsError);
  DebugAssert(axis < theNdim, AipsError);
  return theFullShape(axis);
};

// Returns the length of each axis in the sub-Lattice
const IPosition &LatticeIndexer::shape() const
{
  DebugAssert(ok() == True, AipsError);
  return theShape;
};

// Returns the length of the requested axis in the sub-Lattice
uInt LatticeIndexer::shape(uInt axis) const
{
  DebugAssert(ok() == True, AipsError);
  DebugAssert(axis < theNdim, AipsError);
  return theShape(axis);
};

// function to return the increments along each axis of the LatticeIndexer.
const IPosition &LatticeIndexer::increment() const
{
  DebugAssert(ok() == True, AipsError);
  return theAxisInc;
};

// function to return the increments along the requested axis of the
// Lattice.
uInt LatticeIndexer::increment(uInt axis) const
{
  DebugAssert(ok() == True, AipsError);
  DebugAssert(axis < theNdim, AipsError);
  return theAxisInc(axis);
};

// function to return the offset between the sub-Lattice and the parent
// one.
const IPosition & LatticeIndexer::offset() const
{
  DebugAssert(ok() == True, AipsError);
  return theOffset;
};

// function to return the offset on the specified axes between the
// sub-Lattice and the parent one.
uInt LatticeIndexer::offset(uInt axis) const
{
  DebugAssert(ok() == True, AipsError);
  DebugAssert(axis < theNdim, AipsError);
  return theOffset(axis);
};

// function which returns the number of dimensions in the LatticeIndexer.
uInt LatticeIndexer::ndim() const
{
  DebugAssert(ok() == True, AipsError);
  return theNdim;
};

// Revert from a sub-Lattice description back to the main Lattice. This is
// the only way to "increase" the the size of the sub-Lattice used by the
// LatticeIndexer.
void LatticeIndexer::fullSize()
{
  theShape = theFullShape;
  theAxisInc = 1;
  theOffset = 0;
  DebugAssert(ok() == True, AipsError);
}

// function which returns the number of elements in the sub-Lattice
// this value is equal to the product of shape().
uInt LatticeIndexer::nelements() const
{
  DebugAssert(ok() == True, AipsError);
  return theShape.product();
};

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
Bool LatticeIndexer::tiledCursorMove(Bool incr, IPosition &cursorPos, 
				     const IPosition &cursorShape,
				     const IPosition &cursorHeading) const
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

  DebugAssert(ok() == True, AipsError);
  DebugAssert(cursorPos.nelements() == theNdim, AipsError);
  DebugAssert(cursorShape.nelements() == theNdim, AipsError);
  DebugAssert(cursorHeading.nelements() == theNdim,AipsError);
  for (uInt i=0; i<theNdim; i++)
    DebugAssert(cursorShape(i) > 0 && cursorShape(i) <= theShape(i), AipsError);

  uInt activeAxis, indexToActiveAxis = 0;
  IPosition candidateCursorPos(cursorPos);

  while (indexToActiveAxis < theNdim) {
    activeAxis = cursorHeading(indexToActiveAxis);
    if (incr)
      candidateCursorPos(activeAxis) += cursorShape(activeAxis);
    else
      candidateCursorPos(activeAxis) -= cursorShape(activeAxis);
    if ((candidateCursorPos(activeAxis) < theShape(activeAxis)) &&
	(candidateCursorPos(activeAxis) + cursorShape(activeAxis) > 0)){
      cursorPos = candidateCursorPos;
      return True;
    }
    if (incr)
      candidateCursorPos(activeAxis) -= 
	((candidateCursorPos(activeAxis)+cursorShape(activeAxis)-1)
	 /cursorShape(activeAxis))
	*cursorShape(activeAxis);
    else 
      candidateCursorPos(activeAxis) +=
	((theShape(activeAxis)-candidateCursorPos(activeAxis)-1)
	 /cursorShape(activeAxis))
	* cursorShape(activeAxis);
    indexToActiveAxis++;
  } // while 
  return False;
};

// function which returns a value of True if the IPosition argument
// is within the sub-Lattice.  Returns False if the IPosition argument is 
// outside the sub-Lattice or if the argument doesn't conform to the 
// data members.
Bool LatticeIndexer::isInside(const IPosition &index) const
{
  DebugAssert(ok() == True, AipsError);
  DebugAssert(index.nelements () == theNdim, AipsError);

  for (uInt i=0; i<theNdim; i++)
    if ((index(i) < 0) || (index(i) >= theShape(i)))
      return False;
  return True;
};

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
void LatticeIndexer::subSection (const IPosition &blc, const IPosition &trc,
				 const IPosition &inc)
{
  DebugAssert(ok() == True, AipsError);
  DebugAssert(blc.nelements() == theNdim, AipsError);
  DebugAssert(trc.nelements() == theNdim, AipsError);
  DebugAssert(inc.nelements() == theNdim, AipsError);
  for (uInt i=0; i<theNdim; i++) {
    DebugAssert(blc(i) >= 0, AipsError);
    DebugAssert(trc(i) < theShape(i), AipsError);
    DebugAssert(blc(i) <= trc(i), AipsError);
    DebugAssert(inc(i) >= 1 && inc(i) <= theShape(i), AipsError);
  }
  theShape = (trc-blc+inc)/inc;
  theOffset = theOffset+blc*theAxisInc;
  theAxisInc = theAxisInc*inc;
  DebugAssert(ok() == True, AipsError);
};

void LatticeIndexer::subSection(const IPosition &blc, const IPosition &trc)
{
  DebugAssert(ok() == True, AipsError);
  subSection(blc, trc, IPosition(theNdim, 1));
};

// function which returns an IPosition in the parent Lattice given an
// IPostion in the sub-Lattice.  Accounting is taken of any offsets and
// increments caused by subSectioning. No checks are made to ensure the
// supplied IPosition or the returned one are within the bounds of the
// Lattice(s).
IPosition LatticeIndexer::absolutePosition(const IPosition &position) const
{
  DebugAssert(ok() == True, AipsError);
  DebugAssert(position.nelements () == theNdim, AipsError);
  return theOffset + position*theAxisInc;
};


// function which returns True if all the elements in this 
// sub-Lattice, are arranged contiguously, 
// i.e. without any gaps caused by increments or subSectioning.
// THIS FUNCTION IS NOT FINISHED YET.
// Bool LatticeIndexer::isContiguous() const
// {
//   DebugAssert(ok() == True, AipsError);
//   Bool checkDegenerate = False;
//   for (uInt i=0; i < theNdim; i++) {
//     if (theAxisInc(i) > 1) 
//       checkDegenerate = True;
//     if (theOffset(i) != 0 && i != 0)
//       return False;
//     if (checkDegenerate && shape(i) > 1)
//       return False;
//   }
//   return True;
// };

static  LogIO logErr(LogOrigin("LatticeIndexer", "ok()"));


// Is this LatticeIndexer consistent, i.e. are the class invariants valid?
// return True if every thing is fine otherwise return False
Bool LatticeIndexer::ok() const
{
  if (theNdim == 0){
    logErr << LogIO::SEVERE << "zero dimensions" << LogIO::POST;
    return False;
  }
  if (theFullShape.nelements() != theNdim){
    logErr << LogIO::SEVERE << "LatticeIndexer::ok() - Lattice has "
	   << theFullShape.nelements() << " dimensions"
	   << " instead of "
	   << theNdim << " dimensions"
	   << "(ie. inconsistancy)" << LogIO::POST;
    return False;
  }
  for (uInt i=0; i < theNdim; i++)
    if (theFullShape(i) < 0){
      logErr << LogIO::SEVERE << "an element of the Lattice shape"
	     << " (=" << theFullShape << ")"
	     << " is negative" << LogIO::POST;
      return False;
    }
  if (theAxisInc.nelements() != theNdim) {
    logErr << LogIO::SEVERE << "increments"
	   << " (=" << theAxisInc << ")"
	   << " are the wrong dimension" 
	   << " (ie. not" << theNdim << ")" << LogIO::POST;
    return False;
  }
  for (uInt j=0; j < theNdim; j++)
    if (theAxisInc(j) <= 0 || theAxisInc(j) > theFullShape(j)) {
      logErr << LogIO::SEVERE << "axis increments"
	     << "(=" << theAxisInc << ")"
	     << " are negative OR larger than lattice shape"
	     << " (=" << theFullShape << ")" << LogIO::POST;
      return False;
    }
  if (theOffset.nelements() != theNdim) {
    logErr << LogIO::SEVERE << "offset"
	   << " (=" << theOffset << ")"
	   << " is the wrong dimension"
	   << " (ie. not " << theNdim << ")" << LogIO::POST;
    return False;
  }
  for (uInt k=0; k < theNdim; k++)
    if (theOffset(k) < 0 || theOffset(k) >= theFullShape(k)) {
      logErr << LogIO::SEVERE << "offset"
	     << " (=" << theOffset << ")"
	     << " is larger than lattice shape "
	     << " (=" << theFullShape << ")" 
	     << " or negative" << LogIO::POST;
      return False;
    }
  if (theShape.nelements() != theNdim) {
    logErr << LogIO::SEVERE << "sub-Lattice shape"
	   << " (=" << theShape << ")"
	   << " has wrong number of dimensions"
	   << " (ie. not" << theNdim << ")" << LogIO::POST;
    return False;
  }
  for (uInt m=0; m < theNdim; m++)
    if (theShape(m) <= 0 || theShape > theFullShape(m)) {
      logErr << LogIO::SEVERE << "sub-Lattice shape"
	     << " (=" << theShape << ")"
	     << " is less than or equal to zero or larger than lattice shape"
	     << " (=" << theFullShape << ")" << LogIO::POST;
      return False;
    }
  return True;
};
