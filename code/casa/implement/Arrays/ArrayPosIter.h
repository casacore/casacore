//# ArrayPosIter.h: Iterate an IPosition through the shape of an Array
//# Copyright (C) 1993,1994,1995,1998
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

#if !defined(AIPS_ARRAYPOSITER_H)
#define AIPS_ARRAYPOSITER_H

#include <aips/aips.h>
//# Change the following to a forward declare?
#include <aips/Lattices/IPosition.h>

// 
// <summary> Iterate an IPosition through the shape of an Array </summary>
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
// At the moment this only iterates by the simplest "bottom to top" order, and
// the iteration step always "fills up" its dimensionality. e.g., if we are
// stepping through a cube by matrices, the matrix completely fills up the
// plane. These restrictions should be removed eventually.
//
// <note role=tip> All the array iterator classes should probably be reimplemented;
//        their implementation is a bit ugly.
// 
class ArrayPositionIterator
{
public:
    // Define the shape and origin of the volume the cursor will step
    // through. Also define the dimensionality of the step. byDim==0 implies
    // we are stepping by scalars (i.e. every element), byDim==1 implies that
    // we are stepping by vector, ==2 by matrices, and so on.
    // <group>
    ArrayPositionIterator(const IPosition &shape, const IPosition &origin,
			  uInt byDim);
    ArrayPositionIterator(const IPosition &shape,
			  uInt byDim);
    // </group>
    virtual ~ArrayPositionIterator() {};
    // Reset the cursor to the beginning of the volume.
    virtual void origin();
    // Returns true of the cursor is at the origin.
    Bool atStart() const;
    // Returns true if the cursor has moved past the end of its volume.
    Bool pastEnd() const;
    // Return the position of the cursor.
    const IPosition &pos() const {return Cursor;}
    // Advance the cursor to its next position.
    virtual void next();
    // What is the dimensionality of the volume we are iterating through?
    uInt ndim() const;
    // What is the dimensionality of the "step" the cursor takes, i.e.
    // 0 for scalars, 1 for vector, ....
    uInt dimIter() const {return iterationDim;}
    // How many steps have we taken from the beginning?
    uInt nSteps() const {return stepsFromBegin;}
private:
    // Setup the object for the constructor.
    void setup();

    //# We should probably have mf's for getting at Start,Shape and End.
    IPosition Start, Shape, End, Cursor;
    Bool atOrBeyondEnd;
    uInt iterationDim;
    uInt stepsFromBegin;
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

#endif
