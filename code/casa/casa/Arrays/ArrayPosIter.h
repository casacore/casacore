//# ArrayPosIter.h: Iterate an IPosition through the shape of an Array
//# Copyright (C) 1993,1994,1995,1998,1999,2004
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

#ifndef CASA_ARRAYPOSITER_H
#define CASA_ARRAYPOSITER_H

#include <casa/aips.h>
//# Change the following to a forward declare?
#include <casa/Arrays/IPosition.h>

namespace casa { //# NAMESPACE CASA - BEGIN

// 
// <summary> Iterate an IPosition through the shape of an Array </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
//
// ArrayPositionIterator manipulates an IPosition "cursor" through some
// volume defined by an origin and shape. This position can in turn be
// used to index into, or otherwise define a position in, an Array. Normally
// users won't use this class directly, rather they will use an ArrayIterator,
// VectorIterator or MatrixIterator object, which in turn uses this class.
// ArrayPositionIterator is also used in the implementation of Array.
//
// <srcblock>
// template<class T> void verySlowArrayCopy(Array<T> &to, const Array<T> &from)
// {
//     if (! to.conform(from)) {
//        // throw some error
//     }
//     ArrayPositionIterator toiter(to.shape(), to.origin(),0);
//     ArrayPositionIterator fromiter(from.shape(), from.origin(),0);
//     // If to.origin() == from.origin() we only need one iterator
//     // or we could offset positions by the difference in origins.
//     // The "0" means we are stepping by scalars.
//     while (! toiter.pastEnd()) {    // we know arrays conform
//         to(toiter.pos()) = fromiter(fromiter.pos());
//         toiter.next(); fromiter.next();
//     }
// }
// </srcblock>
//
// Iteration can be done by any combination of axes, but it can only be
// done for full axes.
// <br>The iteration step always "fills up" its dimensionality.
// E.g., if we are stepping through a cube by matrices, the matrix completely
// fills up the plane.
// Class <linkto class=ArrayLattice>ArrayLattice</linkto> in the lattices
// package can be used to iterate with partial volumes.
//
// <note role=tip>
// All the array iterator classes should probably be reimplemented;
// their implementation is a bit ugly.
// </note>

class ArrayPositionIterator
{
public:
    // Define the shape and origin of the volume the cursor will step
    // through. Also define the dimensionality of the step. byDim==0 implies
    // we are stepping by scalars (i.e. every element), byDim==1 implies that
    // we are stepping by vector, ==2 by matrices, and so on.
    // If uses the first byDim axes as the cursor volume and it steps
    // through the remaining axes.
    // <group>
    ArrayPositionIterator(const IPosition &shape, const IPosition &origin,
			  uInt byDim);
    ArrayPositionIterator(const IPosition &shape,
			  uInt byDim);
    // </group>

    // Step through an array using the given axes.
    // The axes can be given in two ways:
    // <ol>
    // <li>axesAreCursor=True means that the axes form the cursor axes.
    //     The remaining axes will form the iteration axes.
    //     This is the default.
    // <li>axesAreCursor=False means the opposite.
    //     In this case the iteration axes can be given in any order.
    // </ol>
    // E.g. when using iteration axes 2,0 for an array with shape [5,3,7], each
    // iteration step returns a cursor (containing the data of axis 1).
    // During the iteration axis 2 will vary most rapidly (as it was
    // given first).
    // Iterate over the given axes in the given order.
    // The cursor axes are the remaining axes.
    // E.g. for a shape of [3,4,5,6] and iterAxes [3,1], the cursor size
    // is [3,5] (axes 0 and 2), while the iteration is fastest for axis 3.
    ArrayPositionIterator(const IPosition &shape,
			  const IPosition &axes,
			  Bool axesAreCursor=True);

    virtual ~ArrayPositionIterator() {};

    // Reset the cursor to the beginning of the volume.
    // <group>
    virtual void reset();
    void origin()
      { reset(); }
    // </group>

    // Returns true of the cursor is at the origin.
    Bool atStart() const;

    // Returns true if the cursor has moved past the end of its volume.
    Bool pastEnd() const;

    // Return the position of the cursor.
    const IPosition &pos() const {return Cursor;}

    // Return the end position of the cursor.
    IPosition endPos() const;

    // Advance the cursor to its next position.
    virtual void next();

    // What is the dimensionality of the volume we are iterating through?
    uInt ndim() const;

    // Return the iteration axes.
    const IPosition &iterAxes() const {return iterationAxes;}

    // Return the cursor axes.
    const IPosition &cursorAxes() const {return cursAxes;}

protected:
    // Advance cursor to its next position and tell which dimension stepped.
    uInt nextStep();
    // What is the dimensionality of the "step" the cursor takes, i.e.
    // 0 for scalars, 1 for vector, ....
    uInt dimIter() const {return cursAxes.nelements();}

private:
    // Setup the object for the constructor.
    // <group>
    void setup(uInt byDim);
    void setup(const IPosition &axes, Bool axesAreCursor);
    // </group>

    //# We should probably have mf's for getting at Start,Shape and End.
    IPosition Start, Shape, End, Cursor;
    Bool atOrBeyondEnd;
    IPosition cursAxes, iterationAxes;
};

// Dimensionality of the array we are iterating through.
inline uInt ArrayPositionIterator::ndim() const
{
    return Start.nelements();
}

// We are at the "end" if we cannot advance any more.
inline Bool ArrayPositionIterator::pastEnd() const
{
    return atOrBeyondEnd;
}


} //# NAMESPACE CASA - END

#endif
