//# ArrayIter.h: Iterate an Array cursor through another Array.
//# Copyright (C) 1993,1994,1995,1996,1999
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

#if !defined (AIPS_ARRAYITER_H)
#define AIPS_ARRAYITER_H

#include <aips/aips.h>
#include <aips/Arrays/ArrayPosIter.h>
#include <aips/Arrays/Array.h>


// 
// <summary> Iterate an Array cursor through another Array. </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
//
// ArrayIterator steps an array section (the "cursor") through an array.
// The cursor "refers" to storage in the array, so that changing the
// values in the cursor changes values in the original array. Like with
// ArrayPositionIterator, the cursor presently only moves through the array from
// bottom to top in the obvious way; however one may of course iterate
// through a slice ("array section"). This class is derived from
// ArrayPositionIterator since it also has a position (the blc of the cursor)
// which moves through the array volume.
//
// <note role=tip> The origin of the cursor, i.e. the subarray that moves through the
//        larger array, is always zero.
//
// <srcblock>
// Array<Float> to, from;
// //... set to and from, check that they are conformant
// ArrayIterator toiter(to,1);
// ArrayIterator fromiter(from,1);
// while (! toiter.pastEnd() ) {
//     toiter.array() = fromiter.array();  // copy vector by vector
//     toiter.next(); fromiter.next();
// }
// 
// </srcblock>
//
// <linkfrom anchor=ArrayIterator classes="Array Vector Matrix Cube">
//    <here>ArrayIterator</here> -- Iterate an Array cursor through another Array.
// </linkfrom>
//
template<class T> class ArrayIterator : public ArrayPositionIterator
{
public:
    // Step through array "arr" using a cursor of dimensionality "byDim".
    ArrayIterator(Array<T> &arr, uInt byDim);
    // Step through "arr" with a cursor of dimensionality 1.
    ArrayIterator(Array<T> &arr);

    virtual ~ArrayIterator();

    // Move the cursor to the next position.
    void next();
    // Reset the cursor to the beginning.
    void origin();

    // Return the cursor. (Perhaps we should have a fn() that returns a
    // reference to the original array as well?)
    Array<T> &array() {return *ap;}
protected:
    // A pointer to the cursor.
    Array<T> *ap;
private:
    // helper function to centralize construction work
    void init(Array<T> &);
    // helper function to set the pointer to the new data position in ap
    // after a step in the given dimension. -1 resets it to the beginning.
    void apSetPointer(Int stepDim);
    Array<T> *pOriginalArray;
    Bool readOnly;
    IPosition offset;
    T* dataPtr;

    //# Presently the following are not defined.
    ArrayIterator(const ArrayIterator<T> &);
    ArrayIterator<T> &operator=(const ArrayIterator<T> &);
};

// 
// <summary> Iterate a const Array cursor through a const Array. </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
//
// This class behaves exactly like an ArrayIterator, only it iterates through
// const Arrays.
//
// <srcblock>
// void CopyArray(Array<Float> &to, const Array<Float> &from)
// {
//     //... check that they are conformant
//     ArrayIterator toiter(to,1);
//     ReadOnlyArrayIterator fromiter(from,1);
//     while (! toiter.pastEnd() ) {
//         toiter.array() = fromiter.array();  // copy vector by vector
//         toiter.next(); fromiter.next();
//     }
// }
// </srcblock>
// <note role=tip> This class is not derived from ArrayPositionIterator. For simplicity
//        it merely contains an ArrayIterator to which it forwards requests
//        and returns (const) results. The iterator classes should be 
//        rethought and reimplemented.
//
// <linkfrom anchor=ReadOnlyArrayIterator classes="Array Vector Matrix Cube">
//    <here>ReadOnlyArrayIterator</here> -- Iterate a const Array cursor through
//     a const Array.
// </linkfrom>
//
template<class T> class ReadOnlyArrayIterator
{
public:
    // Step through array "arr" using a cursor of dimensionality "byDim".
    ReadOnlyArrayIterator(const Array<T> &arr, uInt byDim=1) 
	: ai((Array<T> &)arr,byDim) {}

    // Not implemented.
    ReadOnlyArrayIterator<T> &operator=(const ReadOnlyArrayIterator<T> &);
    
    // Move the cursor to the next position.
    void next() {ai.next();}
    // Reset the cursor to the beginning.
    void origin() {ai.origin();}
    
    // Return the cursor. (Perhaps we should have a fn() that returns a
    // reference to the original array as well?)
    const Array<T> &array() {return ai.array();}
	
    // The same as the function in ArrayPositionIterator.
    // <group>
    Bool atStart() const {return ai.atStart();}
    Bool pastEnd() const {return ai.pastEnd();}
    const IPosition &pos() const {return ai.pos();}
    uInt ndim() const {return ai.ndim();}
    uInt dimIter() const {return ai.dimIter();}
    uInt nSteps() const {return ai.nSteps();}
    // </group>
private:
    ArrayIterator<T> ai;
};


#endif
