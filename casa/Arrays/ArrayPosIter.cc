//# ArrayPosIter.cc: Iterate an IPosition through the shape of an Array
//# Copyright (C) 1993,1994,1995,1999,2004
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

#include "ArrayPosIter.h"
#include "ArrayError.h"

namespace casacore { //# NAMESPACE CASACORE - BEGIN

ArrayPositionIterator::ArrayPositionIterator(const IPosition &shape, 
					     const IPosition &origin,
					     size_t byDim)
: Start(origin),
  Shape(shape),
  atOrBeyondEnd(false)
{
    setup(byDim);
}

ArrayPositionIterator::ArrayPositionIterator(const IPosition &shape, 
					     size_t byDim)
: Start(shape.nelements(), 0),
  Shape(shape),
  atOrBeyondEnd(false)
{
    setup(byDim);
}

ArrayPositionIterator::ArrayPositionIterator(const IPosition &shape, 
					     const IPosition &iterAxes,
					     bool axesAreCursor)
: Start(shape.nelements(), 0),
  Shape(shape),
  atOrBeyondEnd(false)
{
    setup(iterAxes, axesAreCursor);
}

// <thrown>
//     <item> ArrayIteratorError
// </thrown>
void ArrayPositionIterator::setup(size_t byDim)
{
    if (byDim > ndim()) {
	throw(ArrayIteratorError("ArrayPositionIterator::ArrayPositionIterator"
	    " - Stepping by dimension > Array dimension"));
    }
    IPosition cursorAxes(byDim);
    for (size_t i=0; i<byDim; i++) {
      cursorAxes(i) = i;
    }
    setup (cursorAxes, true);
}

void ArrayPositionIterator::setup(const IPosition &axes,
				  bool axesAreCursor)
{
    // Note that IPosition::otherAxes checks if axes are unique.
    // Get the iteration axes.
    if (axesAreCursor) {
        iterationAxes = IPosition::otherAxes (ndim(), axes);
    } else {
        iterationAxes = axes;
    }
    // Get the cursorAxes.
    // Do this also if axesAreCursor=true, so we are sure they are
    // in the correct order.
    cursAxes = IPosition::otherAxes (ndim(), iterationAxes);
    // Check shape.
    if (Start.nelements() != Shape.nelements()) {
	throw(ArrayIteratorError("ArrayPositionIterator::ArrayPositionIterator"
				 " - ndim of origin and shape differ"));
    }
    for (size_t i=0; i < ndim(); i++) {
	if (Shape(i) < 0)
         throw(ArrayIteratorError("ArrayPositionIterator::ArrayPositionIterator"
				     " - Shape(i) < 0"));
    }
    End = Start + Shape - 1;
    reset();
}

void ArrayPositionIterator::reset()
{
    Cursor = Start;
    // Immediately at end if first iteration axis is empty.
    if (iterationAxes.nelements() > 0) {
      int ax = iterationAxes[0];
      atOrBeyondEnd = End[ax] < Start[ax];
    } else {
      atOrBeyondEnd = Shape.nelements() == 0  ||  Shape[0] == 0;
    }
}

bool ArrayPositionIterator::atStart() const
{
    // Too expensive - we should set variables in next/previous
    return Cursor == Start;
}

// <thrown>
//     <item> ArrayIteratorError
// </thrown>
void ArrayPositionIterator::next()
{
    nextStep();
}

void ArrayPositionIterator::set (const IPosition& cursorPos)
{
    bool all = false;
    if (cursorPos.nelements() != iterationAxes.nelements()) {
        all = true;
	if (cursorPos.nelements() != ndim()) {
	    throw ArrayIteratorError ("ArrayPositionIterator::set - "
				      "length of cursorPos is invalid");
	}
    }
    atOrBeyondEnd = false;
    for (size_t i=0; i<cursorPos.nelements(); ++i) {
        // Only take the axis into account if it is an iteration axis.
        int axis = -1;
	if (!all) {
	  axis = iterationAxes(i);
	} else {
	  for (size_t j=0; j<iterationAxes.nelements(); ++j) {
	    if (i == size_t(iterationAxes[j])) {
	      axis = i;
	      break;
	    }
	  }
	}
	if (axis >= 0) {
	  Cursor[axis] = cursorPos[i];
	  if (Cursor[axis] > End[axis]) {
	    atOrBeyondEnd = true;
	  }
	}
    }
}

size_t ArrayPositionIterator::nextStep()
{
    // This could and should be made more efficient. 
    // next will step past the end (as it needs to for pastEnd to trigger).

    // Short circuit if we are iterating by the same dimensionality
    // as the array.
    if (iterationAxes.nelements() == 0){
        atOrBeyondEnd = true;
        Cursor = End;
	return ndim();
    }

    // Increment the cursor.
    int axis = 0;
    for (size_t i=0; i<iterationAxes.nelements(); i++) {
        axis = iterationAxes(i);
	Cursor(axis)++;
	if (Cursor(axis) <= End(axis)) {
	    break;
	}
	// Exceeded the axis. Reset it if not the last one.
	if (i < iterationAxes.nelements()-1) {
	    Cursor(axis) = Start(axis);
	} else {
	    atOrBeyondEnd = true;
	}
    }
    return axis;
}

IPosition ArrayPositionIterator::endPos() const
{
  IPosition endp = pos();
  for (size_t i=0; i<cursAxes.nelements(); i++) {
    size_t axis = cursAxes(i);
    endp(axis) = Shape(axis)-1;
  }
  return endp;
}

ArrayBase& ArrayPositionIterator::getArray()
{
  throw ArrayIteratorError ("ArrayPositionIterator::getArray cannot be used");
}

} //# NAMESPACE CASACORE - END
