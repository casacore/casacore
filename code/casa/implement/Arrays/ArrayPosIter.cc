//# ArrayPosIter.cc: Iterate an IPosition through the shape of an Array
//# Copyright (C) 1993,1994,1995,1999
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

#include <casa/Arrays/ArrayPosIter.h>
#include <casa/Arrays/ArrayError.h>

namespace casa { //# NAMESPACE CASA - BEGIN

ArrayPositionIterator::ArrayPositionIterator(const IPosition &shape, 
					     const IPosition &origin,
					     uInt byDim)
: Start(origin),
  Shape(shape),
  atOrBeyondEnd(False),
  iterationDim(byDim),
  stepsFromBegin(0)
{
    setup();
}

ArrayPositionIterator::ArrayPositionIterator(const IPosition &shape, 
					     uInt byDim)
: Start(shape.nelements(), 0),
  Shape(shape),
  atOrBeyondEnd(False),
  iterationDim(byDim),
  stepsFromBegin(0)
{
    setup();
}

// <thrown>
//     <item> ArrayIteratorError
// </thrown>
void ArrayPositionIterator::setup()
{
    if (iterationDim > ndim()) {
	throw(ArrayIteratorError("ArrayPositionIterator::ArrayPositionIterator"
	    " - Stepping by dimension > Array dimension"));
    }
    if (Start.nelements() != Shape.nelements()) {
	throw(ArrayIteratorError("ArrayPositionIterator::ArrayPositionIterator"
				 " - ndim of origin and shape differ"));
    }
    for (uInt i=0; i < ndim(); i++) {
	if (Shape(i) < 0)
         throw(ArrayIteratorError("ArrayPositionIterator::ArrayPositionIterator"
				     " - Shape(i) < 0"));
    }
    Cursor = Start;
    End = Start + Shape - 1;
}

void ArrayPositionIterator::origin()
{
    stepsFromBegin = 0;
    Cursor = Start;
    atOrBeyondEnd = False;
}

Bool ArrayPositionIterator::atStart() const
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

uInt ArrayPositionIterator::nextStep()
{
    // This could and should be made more efficient. 
    // next will step past the end (as it needs to for pastEnd to trigger).

    // Short circuit if we are iterating by the same dimensionality
    // as the array.
    if (iterationDim == ndim()){
        atOrBeyondEnd = True;
        Cursor = End;
	return ndim();
    }

    if (aips_debug) {
	// We can go past the end, but we should never be before the
	// start!
	if ((Start <= Cursor) == False)
	    throw(ArrayIteratorError("ArrayPositionIterator::next()"
				     " - Cursor before array start"));
    }

    Cursor(iterationDim)++;
    
    // When we reach the end of the current dimensionality, we have to
    // increment the next higher  one, and it might ripple (e.g. 999+1=1000).
    // We let the most significan digit keep climbing.
    uInt iterDim = iterationDim;
    while (Cursor(iterDim) > End(iterDim)  &&  iterDim < ndim() - 1) {
	Cursor(iterDim) = Start(iterDim);
	if (++iterDim > ndim() - 1)
	    break;
	Cursor(iterDim) += 1;
    }
    // We're at the end if the last index has rolled past the end
    if (Cursor(ndim() - 1) > End(ndim() - 1)) {
        atOrBeyondEnd = True;
    } else {
	stepsFromBegin++;
    }
    return iterDim;
}

} //# NAMESPACE CASA - END

