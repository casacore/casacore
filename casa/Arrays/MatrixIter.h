//# MatrixIter.h: Iterate a matrix cursor through another array
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

#ifndef CASA_MATRIXITER_2_H
#define CASA_MATRIXITER_2_H

#include "ArrayIter.h"
#include "Matrix.h"

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// 
// <summary> Iterate a Matrix cursor through another Array. </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
//
// MatrixIterator steps a Matrix (the "cursor") through an array.
// The cursor "refers" to storage in the array, so that changing the
// values in the cursor changes values in the original array.
//
// This class is derived from ArrayIterator; basically it only adds the
// matrix() member function which allows you to access the cursor as a Matrix.
//
// <note role=tip>
// The origin of the cursor, i.e. the subarray that moves through the
// larger array, is always zero.
// </note>
//
// In this example we want to make a "moment" map of a cube, i.e. collapse
// the "Z" axis by averaging it.
// <srcblock>
// Cube<float> cube;
// MatrixIterator planeIter(cube);
// Matrix<float> average(planeIter.matrix().copy()); // init with first plane
// planeIter.next(); // advance the iterator
// while (! planeIter.pastEnd()) {
//     average += planeIter.matrix(); // Sum the next plane
//     planeIter.next();
// }
// average /= float(cube.shape()(2));  // divide by the number of planes
// </srcblock>

template<typename T, typename Alloc=std::allocator<T>>
class MatrixIterator : public ArrayIterator<T, Alloc>
{
public:
    // Iterate by matrices through array "a".
    // The first 2 axes form the cursor axes.
    explicit MatrixIterator(Array<T, Alloc> &a);

    // Iterate by matrices through array "a".
    // The given axes form the cursor axes.
    MatrixIterator(Array<T, Alloc> &a, size_t cursorAxis1, size_t cursorAxis2);

    // Return the matrix at the current position.
    Matrix<T, Alloc> &matrix() {return *(Matrix<T, Alloc> *)(this->ap_p.get());}

private:
    // Not implemented.
    MatrixIterator(const MatrixIterator<T, Alloc> &) = delete;
    // Not implemented.
    MatrixIterator<T, Alloc> &operator=(const MatrixIterator<T, Alloc> &) = delete;
};

// 
// <summary> Iterate a Matrix cursor through a R/O Array. </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
//
// ReadOnlyMatrixIterator behaves exactly like MatrixIterator (cf.) only
// it should be used on const Arrays.
//
// <note role=tip> Note that the R/O MatrixIterator is not derived from R/O 
//        ArrayIterator.
// </note>
//
template<class T> class ReadOnlyMatrixIterator 
{
public:
    // <group>
    ReadOnlyMatrixIterator(const Array<T> &a) :
      mi(const_cast<Array<T>&>(a)) {}

    ReadOnlyMatrixIterator(const Array<T> &a,
			   size_t cursorAxis1, size_t cursorAxis2)
      : mi(const_cast<Array<T>&>(a), cursorAxis1, cursorAxis2) {}

    void next()   {mi.next();}
    void reset() {mi.origin();}
    void origin() {mi.origin();}
    
    const Array<T> &array() {return mi.array();}
    const Matrix<T> &matrix() {return mi.matrix();}

    bool atStart() const {return mi.atStart();}
    bool pastEnd() const {return mi.pastEnd();}
    const IPosition &pos() const {return mi.pos();}
    IPosition endPos() const {return mi.endPos();}
    size_t ndim() const {return mi.ndim();}
    // </group>
private:
    // Not implemented.
    ReadOnlyMatrixIterator(const ReadOnlyMatrixIterator<T> &) = delete;
    // Not implemented.
    ReadOnlyMatrixIterator<T> &operator=(const ReadOnlyMatrixIterator<T> &) = delete;

    MatrixIterator<T> mi;
};


} //# NAMESPACE CASACORE - END

#include "MatrixIter.tcc"

#endif
