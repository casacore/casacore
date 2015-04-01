//# Matrix.h: A 2-D Specialization of the Array Class
//# Copyright (C) 1993,1994,1995,1996,1999,2000,2001,2003
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

#ifndef CASA_MATRIX_H
#define CASA_MATRIX_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Array.h>

namespace casacore { //#Begin casa namespace

//# Forward Declarations
template<class T> class Vector;


// <summary> A 2-D Specialization of the Array class </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
//
// Matrix objects are two-dimensional specializations (e.g., more convenient
// and efficient indexing) of the general Array class. You might also want
// to look at the Array documentation to see inherited functionality. A
// tutorial on using the array classes in general is available in the
// "AIPS++ Programming Manual".
//
// Generally the member functions of Array are also available in
// Matrix versions which take a pair of integers where the array 
// needs an IPosition. Since the Matrix
// is two-dimensional, the IPositions are overkill, although you may
// use those versions if you want to.
// <srcblock>
// Matrix<Int> mi(100,100);  // Shape is 100x100
// mi.resize(50,50);         // Shape now 50x50
// </srcblock>
//
// Slices may be taken with the Slice class. To take a slice, one "indexes" 
// with one Slice(start, length, inc) for each axis,
// where end and inc are optional.
// Additionally, there are row(), column() and diagonal()
// member functions which return Vector's which refer to the storage back
// in the Matrix:
// <srcblock>
// Matrix<Float> mf(100, 100);
// mf.diagonal() = 1;
// </srcblock>
//
// Correct indexing order of a matrix is:
// <srcblock>
// Matrix<Int> mi(n1,n2)                  // [nrow, ncolumn]
// for (uInt j=0; j<mi.ncolumn(); j++) {
//    for (uInt i=0; i<mi.nrow(); i++) {
//       mi(i,j) = i*j;
//    }      
// }
// </srcblock>
//
//
// Element-by-element arithmetic and logical operations are available (in
// aips/ArrayMath.h and aips/ArrayLogical.h). Other Matrix operations (e.g.
// LU decomposition) are available, and more appear periodically.
//
// As with the Arrays, if the preprocessor symbol AIPS_DEBUG is
// defined at compile time invariants will be checked on entry to most
// member functions. Additionally, if AIPS_ARRAY_INDEX_CHECK is defined
// index operations will be bounds-checked. Neither of these should
// be defined for production code.

template<class T> class Matrix : public Array<T>
{
public:
    // A Matrix of length zero in each dimension; zero origin.
    Matrix();

    // A Matrix with "l1" rows and "l2" columns.
    Matrix(size_t l1, size_t l2);

    // A Matrix with "l1" rows and "l2" columns.
    // Fill it with the initial value.
    Matrix(size_t l1, size_t l2, const T &initialValue);

    // A matrix of shape with shape "len".
    Matrix(const IPosition &len);

    // A matrix of shape with shape "len".
    // Fill it with the initial value.
    Matrix(const IPosition &len, const T &initialValue);

    // The copy constructor uses reference semantics.
    Matrix(const Matrix<T> &other);

    // Construct a Matrix by reference from "other". "other must have
    // ndim() of 2 or less.
    Matrix(const Array<T> &other);

    // Create an Matrix of a given shape from a pointer.
    Matrix(const IPosition &shape, T *storage, StorageInitPolicy policy = COPY);
    // Create an Matrix of a given shape from a pointer. Because the pointer
    // is const, a copy is always made.
    Matrix(const IPosition &shape, const T *storage);

    // Define a destructor, otherwise the (SUN) compiler makes a static one.
    virtual ~Matrix();

    // Create an identity matrix of side length n. (Could not do this as a constructor
    // because of ambiguities with other constructors).
    static Matrix<T> identity (size_t n);

    // Assign the other array (which must be dimension 2) to this matrix.
    // If the shapes mismatch, this array is resized.
    virtual void assign (const Array<T>& other);

    // Make this matrix a reference to other. Other must be of dimensionality
    // 2 or less.
    virtual void reference(const Array<T> &other);

    // Resize to the given shape (must be 2-dimensional).
    // Resize without argument is equal to resize(0,0).
    // <group>
    void resize(size_t nx, size_t ny, Bool copyValues=False);
    virtual void resize();
    virtual void resize(const IPosition &newShape, Bool copyValues=False);
    // </group>

    // Copy the values from other to this Matrix. If this matrix has zero
    // elements then it will resize to be the same shape as other; otherwise
    // other must conform to this.
    // Note that the assign function can be used to assign a
    // non-conforming matrix.
    // <group>
    Matrix<T> &operator=(const Matrix<T> &other);
    virtual Array<T> &operator=(const Array<T> &other);
    // </group>

    // Copy val into every element of this Matrix; i.e. behaves as if
    // val were a constant conformant matrix.
    Array<T> &operator=(const T &val)
      { return Array<T>::operator=(val); }

    // Copy to this those values in marray whose corresponding elements
    // in marray's mask are True.
    Matrix<T> &operator= (const MaskedArray<T> &marray)
      { Array<T> (*this) = marray; return *this; }


    // Single-pixel addressing. If AIPS_ARRAY_INDEX_CHECK is defined,
    // bounds checking is performed.
    // <group>
    T &operator()(const IPosition &i)
      { return Array<T>::operator()(i); }
    const T &operator()(const IPosition &i) const 
      { return Array<T>::operator()(i); }
    T &operator()(size_t i1, size_t i2)
      {
#if defined(AIPS_ARRAY_INDEX_CHECK)
        this->validateIndex(i1, i2);   // Throws an exception on failure
#endif
	return this->contiguous_p ? this->begin_p[i1 + i2*yinc_p] :
	                            this->begin_p[i1*xinc_p + i2*yinc_p];
      }

    const T &operator()(size_t i1, size_t i2) const
      {
#if defined(AIPS_ARRAY_INDEX_CHECK)
        this->validateIndex(i1, i2);   // Throws an exception on failure
#endif
	return this->contiguous_p ? this->begin_p[i1 + i2*yinc_p] :
                                    this->begin_p[i1*xinc_p + i2*yinc_p];
      }
    // </group>


    // The array is masked by the input LogicalArray.
    // This mask must conform to the array.
    // <group>

    // Return a MaskedArray.
    MaskedArray<T> operator() (const LogicalArray &mask) const
      { return Array<T>::operator() (mask); }

    // Return a MaskedArray.
    MaskedArray<T> operator() (const LogicalArray &mask)
      { return Array<T>::operator() (mask); }

    // </group>


    // The array is masked by the input MaskedLogicalArray.
    // The mask is effectively the AND of the internal LogicalArray
    // and the internal mask of the MaskedLogicalArray.
    // The MaskedLogicalArray must conform to the array.
    // <group>

    // Return a MaskedArray.
    MaskedArray<T> operator() (const MaskedLogicalArray &mask) const
      { return Array<T>::operator() (mask); }

    // Return a MaskedArray.
    MaskedArray<T> operator() (const MaskedLogicalArray &mask)
      { return Array<T>::operator() (mask); }

    // </group>


    // Returns a reference to the i'th row.
    // <group>
    Vector<T> row(size_t i);
    const Vector<T> row(size_t i) const;
    // </group>

    // Returns a reference to the j'th column
    // <group>
    Vector<T> column(size_t j);
    const Vector<T> column(size_t j) const;
    // </group>

    // Returns a diagonal from the Matrix. The Matrix must be square.
    // n==0 is the main diagonal. n>0 is above the main diagonal, n<0
    // is below it.
    // <group>
    Vector<T> diagonal(Int64 n=0);
    const Vector<T> diagonal(Int64 n=0) const;
    // </group>

    // Take a slice of this matrix. Slices are always indexed starting
    // at zero. This uses reference semantics, i.e. changing a value
    // in the slice changes the original.
    // <srcblock>
    // Matrix<Double> vd(100,100);
    // //...
    // vd(Slice(0,10),Slice(10,10)) = -1.0; // 10x10 sub-matrix set to -1.0
    // </srcblock>
    // <group>
    Matrix<T> operator()(const Slice &sliceX, const Slice &sliceY);
    const Matrix<T> operator()(const Slice &sliceX, const Slice &sliceY) const;
    // </group>

    // Slice using IPositions. Required to be defined, otherwise the base
    // class versions are hidden.
    // <group>
    Array<T> operator()(const IPosition &blc, const IPosition &trc,
			const IPosition &incr)
      { return Array<T>::operator()(blc,trc,incr); }
    const Array<T> operator()(const IPosition &blc, const IPosition &trc,
                              const IPosition &incr) const
      { return Array<T>::operator()(blc,trc,incr); }
    Array<T> operator()(const IPosition &blc, const IPosition &trc)
      { return Array<T>::operator()(blc,trc); }
    const Array<T> operator()(const IPosition &blc, const IPosition &trc) const
      { return Array<T>::operator()(blc,trc); }
    Array<T> operator()(const Slicer& slicer)
      { return Array<T>::operator()(slicer); }
    const Array<T> operator()(const Slicer& slicer) const
      { return Array<T>::operator()(slicer); }
    // </group>

    // The length of each axis of the Matrix.
    const IPosition &shape() const
      { return this->length_p; }
    void shape(Int &s1, Int &s2) const
      { s1 = this->length_p(0); s2=this->length_p(1); }

    // The number of rows in the Matrix, i.e. the length of the first axis.
    size_t nrow() const
      { return this->length_p(0); }

    // The number of columns in the Matrix, i.e. the length of the 2nd axis.
    size_t ncolumn() const
      { return this->length_p(1); }

    // Replace the data values with those in the pointer <src>storage</src>.
    // The results are undefined is storage does not point at nelements() or
    // more data elements. After takeStorage() is called, <src>unique()</src>
    // is True.
    // <group>
    virtual void takeStorage(const IPosition &shape, T *storage,
			     StorageInitPolicy policy = COPY);
    // Since the pointer is const, a copy is always taken.
    virtual void takeStorage(const IPosition &shape, const T *storage);
    // </group>

    // Checks that the Matrix is consistent (invariants check out).
    virtual Bool ok() const;

protected:
    // Remove the degenerate axes from other and store result in this matrix.
    // An exception is thrown if removing degenerate axes does not result
    // in a matrix.
    virtual void doNonDegenerate(const Array<T> &other,
                                 const IPosition &ignoreAxes);

private:
    // Cached constants to improve indexing.
    size_t xinc_p, yinc_p;

    // Helper fn to calculate the indexing constants.
    void makeIndexingConstants();
};

} //#End casa namespace
#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/casa/Arrays/Matrix.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
