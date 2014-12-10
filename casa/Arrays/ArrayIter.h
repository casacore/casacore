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

#ifndef CASA_ARRAYITER_H
#define CASA_ARRAYITER_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/ArrayPosIter.h>
#include <casacore/casa/Arrays/Array.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

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
// <note role=tip> The origin of the cursor, i.e. the subarray that moves
//   through the larger array, is always zero.
// </note>
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
    // Step through array "arr" over the first byDim axes
    // (using a cursor of dimensionality "byDim").
    explicit ArrayIterator(const Array<T> &arr, uInt byDim=1);

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
    ArrayIterator(const Array<T> &arr, const IPosition &axes,
		  Bool axesAreCursor = True);

    virtual ~ArrayIterator();

    // Move the cursor to the next position.
    virtual void next();

    // Set the cursor to the given position.
    // The position can only contain the iteration axes or it can be the full
    // position.
    // <br>In the first case the position must to be given in the order
    // of the iteration axes as given in the constructor.
    // In the latter case the position must be given in natural order
    // (as given by function <src>pos</src> and only the cursor axes are taken
    // into account.
    virtual void set (const IPosition& cursorPos);

    // Reset the cursor to the beginning.
    // <group>
    virtual void reset();
    // </group>

    // Return the cursor. (Perhaps we should have a fn() that returns a
    // reference to the original array as well?)
    // <group>
    Array<T> &array() {return *ap_p;}
    virtual ArrayBase& getArray();
    // </group>


protected:
    // A pointer to the cursor.
    Array<T>* ap_p;

private:
    // helper function to centralize construction work
    void init(const Array<T> &);
    // helper function to set the pointer to the new data position in ap
    // after a step in the given dimension. -1 resets it to the beginning.
    void apSetPointer(Int stepDim);

    Array<T> pOriginalArray_p;
    IPosition offset_p;
    T* dataPtr_p;

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
// </note>
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
    explicit ReadOnlyArrayIterator(const Array<T> &arr, uInt byDim=1) 
	: ai(const_cast<Array<T>&>(arr),byDim) {}

    // Step through an array for the given iteration axes.
  ReadOnlyArrayIterator(const Array<T> &arr, const IPosition &axes,
			Bool axesAreCursor = True)
	: ai(const_cast<Array<T>&>(arr),axes,axesAreCursor) {}

    // Move the cursor to the next position.
    void next() {ai.next();}

    // Set the cursor to the given position.
    // The position can only contain the iteration axes or it can be the full
    // position.
    // <br>In the first case the position must to be given in the order
    // of the iteration axes as given in the constructor.
    // In the latter case the position must be given in natural order
    // (as given by function <src>pos</src> and only the cursor axes are taken
    // into account.
    void set (const IPosition& cursorPos) {ai.set(cursorPos);}

    // Reset the cursor to the beginning.
    // <group>
    void reset() {ai.origin();}
    void origin() {ai.origin();}
    // </group>
    
    // Return the cursor. (Perhaps we should have a fn() that returns a
    // reference to the original array as well?)
    const Array<T> &array() {return ai.array();}
	
    // The same as the functions in ArrayPositionIterator.
    // <group>
    Bool atStart() const {return ai.atStart();}
    Bool pastEnd() const {return ai.pastEnd();}
    const IPosition &pos() const {return ai.pos();}
    IPosition endPos() const {return ai.endPos();}
    uInt ndim() const {return ai.ndim();}
    // </group>
private:
    // Not implemented.
    // <group>
    ReadOnlyArrayIterator (const ReadOnlyArrayIterator<T> &);
    ReadOnlyArrayIterator<T> &operator=(const ReadOnlyArrayIterator<T> &);
    // </group>
    
    ArrayIterator<T> ai;
};



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/casa/Arrays/ArrayIter.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
