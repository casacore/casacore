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

#if !defined (AIPS_MATRIXITER_H)
#define AIPS_MATRIXITER_H


#include <aips/aips.h>
#include <aips/Arrays/ArrayIter.h>
#include <aips/Arrays/Matrix.h>

// 
// <summary> Iterate a Matrix cursor through another Array. </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
//
// MatrixIterator steps a Vector (the "cursor") through an array.
// The cursor "refers" to storage in the array, so that changing the
// values in the cursor changes values in the original array. Like with
// ArrayPositionIterator, the cursor presently only moves through the array from
// bottom to top in the obvious way; however one may of course iterate
// through a slice ("array section"). This class is derived from
// ArrayIterator; basically it only adds the vector() member function which
// allows you to access the cursor as a Vector.
//
// <note role=tip> The origin of the cursor, i.e. the subarray that moves through the
//        larger array, is always zero.
//
// In this example we want to make a "moment" map of a cube, i.e. collapse
// the "Z" axis by averaging it.
// 
// <srcblock>
// Cube<Float> cube;
// MatrixIterator planeIter(cube);
// Matrix<Float> average(planeIter.matrix().copy()); // init with first plane
// planeIter.next(); // advance the iterator
// while (! planeIter.pastEnd()) {
//     average += planeIter.matrix(); // Sum the next plane
//     planeIter.next();
// }
// average /= Float(cube.shape()(2));  // divide by the number of planes
// </srcblock>
// <note role=tip> All ArrayIterator classes should be redone.
//

template<class T> class MatrixIterator : public ArrayIterator<T>
{
public:
    // Iterate by matrices through array "a".
    MatrixIterator(Array<T> &a);

    // Not implemented.
    MatrixIterator(const MatrixIterator<T> &);
    // Not implemented.
    MatrixIterator<T> &operator=(const MatrixIterator<T> &);

    // Return the matrix at the current position.
    Matrix<T> &matrix() {return *(Matrix<T> *)ap;}
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
//
template<class T> class ReadOnlyMatrixIterator 
{
public:
    // <group>
    ReadOnlyMatrixIterator(const Array<T> &a) : mi((Array<T> &)a) {}
    ReadOnlyMatrixIterator(const ReadOnlyMatrixIterator<T> &);
    ReadOnlyMatrixIterator<T> &operator=(const ReadOnlyMatrixIterator<T> &);

    void next()   {mi.next();}
    void origin() {mi.origin();}
    
    const Array<T> &array() {return mi.array();}
    const Matrix<T> &matrix() {return mi.matrix();}

    Bool atStart() const {return mi.atStart();}
    Bool pastEnd() const {return mi.pastEnd();}
    const IPosition &pos() const {return mi.pos();}
    uInt ndim() const {return mi.ndim();}
    uInt dimIter() const {return mi.dimIter();}
    uInt nSteps() const {return mi.nSteps();}
    // </group>
private:
    MatrixIterator<T> mi;
};

#endif
