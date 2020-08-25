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

#ifndef CASA_MATRIX_2_H
#define CASA_MATRIX_2_H

//# Includes
#include "Array.h"

namespace casacore { //#Begin casa namespace

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
// Matrix<int> mi(100,100);  // Shape is 100x100
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
// Matrix<float> mf(100, 100);
// mf.diagonal() = 1;
// </srcblock>
//
// Correct indexing order of a matrix is:
// <srcblock>
// Matrix<int> mi(n1,n2)                  // [nrow, ncolumn]
// for (size_t j=0; j<mi.ncolumn(); j++) {
//    for (size_t i=0; i<mi.nrow(); i++) {
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

template<typename T, typename Alloc> class Matrix : public Array<T, Alloc>
{
public:
    // A Matrix of length zero in each dimension; zero origin.
    Matrix(const Alloc& allocator = Alloc());

    // A Matrix with "l1" rows and "l2" columns.
    // Fill it with the initial value.
    Matrix(size_t l1, size_t l2, const T &initialValue = T(), const Alloc& allocator = Alloc());

    // An uninitialized Matrix with "l1" rows and "l2" columns.
    Matrix(size_t l1, size_t l2, typename Array<T, Alloc>::uninitializedType, const Alloc& allocator = Alloc());

    // A matrix of shape with shape "len".
    // Fill it with the initial value.
    Matrix(const IPosition &len, const T &initialValue = T(), const Alloc& allocator = Alloc());

    // An uninitialized matrix of shape with shape "len".
    Matrix(const IPosition &len, typename Array<T, Alloc>::uninitializedType, const Alloc& allocator = Alloc());

    // The copy/move constructor uses reference semantics.
    Matrix(const Matrix<T, Alloc>& source);
    Matrix(Matrix<T, Alloc>&& source);

    // Construct a Matrix by reference from "source". "source must have
    // ndim() of 2 or less.
    Matrix(const Array<T, Alloc>& source);
    Matrix(Array<T, Alloc>&& source);

    // Create an Matrix of a given shape from a pointer.
    Matrix(const IPosition &shape, T *storage, StorageInitPolicy policy = COPY, const Alloc& allocator = Alloc());
    // Create an Matrix of a given shape from a pointer. Because the pointer
    // is const, a copy is always made.
    Matrix(const IPosition &shape, const T *storage);

    // Create an identity matrix of side length n. (Could not do this as a constructor
    // because of ambiguities with other constructors).
    static Matrix<T, Alloc> identity (size_t n);

    // Resize to the given shape
    // <group>
    using Array<T, Alloc>::resize;
    void resize(size_t nx, size_t ny, bool copyValues=false);
     // </group>

    Matrix<T, Alloc>& operator=(const Matrix<T, Alloc>& source)
    { return assign_conforming(source); }
    Matrix<T, Alloc>& operator=(Matrix<T, Alloc>&& source)
    { return assign_conforming(std::move(source)); }
    Matrix<T, Alloc>& operator=(const Array<T, Alloc>& source)
    { return assign_conforming(source); }
    Matrix<T, Alloc>& operator=(Array<T, Alloc>&& source)
    { return assign_conforming(std::move(source)); }
   
    // Copy the values from other to this Matrix. If this matrix has zero
    // elements then it will resize to be the same shape as other; otherwise
    // other must conform to this.
    // Note that the assign function can be used to assign a
    // non-conforming matrix.
    // <group>
    Matrix<T, Alloc>& assign_conforming(const Matrix<T, Alloc>& source)
    { Array<T, Alloc>::assign_conforming(source); return *this; }
    Matrix<T, Alloc>& assign_conforming(Matrix<T, Alloc>&& source)
    { Array<T, Alloc>::assign_conforming(std::move(source)); return *this; }
    
    Matrix<T, Alloc>& assign_conforming(const Array<T, Alloc>& source)
    {
      // TODO Should be supported by the Array class,
      // see Cube::operator=(const Array&)
      
      if (source.ndim() == 2) {
        Array<T, Alloc>::assign_conforming(source);
      } else {
        // This might work if a.ndim == 1 or 2
        (*this) = Matrix<T, Alloc>(source);
      }
      return *this;
    }
   
    Matrix<T, Alloc>& assign_conforming(Array<T, Alloc>&& source)
    {
      if (source.ndim() == 2) {
        Array<T, Alloc>::assign_conforming(std::move(source));
      } else {
        (*this) = Matrix<T, Alloc>(std::move(source));
      }
      return *this;
    }
    // </group>

    // Copy val into every element of this Matrix; i.e. behaves as if
    // val were a constant conformant matrix.
    Array<T, Alloc>& operator=(const T &val)
      { return Array<T, Alloc>::operator=(val); }

    // Copy to this those values in marray whose corresponding elements
    // in marray's mask are true.
    Matrix<T, Alloc>& assign_conforming (const MaskedArray<T> &marray)
      { Array<T, Alloc>::assign_conforming(marray); return *this; }

    // Single-pixel addressing. If AIPS_ARRAY_INDEX_CHECK is defined,
    // bounds checking is performed.
    // <group>
    T &operator()(const IPosition &i)
      { return Array<T, Alloc>::operator()(i); }
    const T &operator()(const IPosition &i) const 
      { return Array<T, Alloc>::operator()(i); }
    T &operator()(size_t i1, size_t i2)
      {
	return this->contiguous_p ? this->begin_p[i1 + i2*yinc()] :
	                            this->begin_p[i1*xinc() + i2*yinc()];
      }

    const T &operator()(size_t i1, size_t i2) const
      {
	return this->contiguous_p ? this->begin_p[i1 + i2*yinc()] :
                                    this->begin_p[i1*xinc() + i2*yinc()];
      }
    // </group>


    // The array is masked by the input LogicalArray.
    // This mask must conform to the array.
    // <group>

    // Return a MaskedArray.
    MaskedArray<T> operator() (const LogicalArray &mask) const
      { return Array<T, Alloc>::operator() (mask); }

    // Return a MaskedArray.
    MaskedArray<T> operator() (const LogicalArray &mask)
      { return Array<T, Alloc>::operator() (mask); }

    // </group>


    // The array is masked by the input MaskedLogicalArray.
    // The mask is effectively the AND of the internal LogicalArray
    // and the internal mask of the MaskedLogicalArray.
    // The MaskedLogicalArray must conform to the array.
    // <group>

    // Return a MaskedArray.
    MaskedArray<T> operator() (const MaskedLogicalArray &mask) const
      { return Array<T, Alloc>::operator() (mask); }

    // Return a MaskedArray.
    MaskedArray<T> operator() (const MaskedLogicalArray &mask)
      { return Array<T, Alloc>::operator() (mask); }

    // </group>


    // Returns a reference to the i'th row.
    // <group>
    Vector<T, Alloc> row(size_t i);
    const Vector<T, Alloc> row(size_t i) const;
    // </group>

    // Returns a reference to the j'th column
    // <group>
    Vector<T, Alloc> column(size_t j);
    const Vector<T, Alloc> column(size_t j) const;
    // </group>

    // Returns a diagonal from the Matrix. The Matrix must be square.
    // n==0 is the main diagonal. n>0 is above the main diagonal, n<0
    // is below it.
    // <group>
    Vector<T, Alloc> diagonal(long long n=0);
    const Vector<T, Alloc> diagonal(long long n=0) const;
    // </group>

    // Take a slice of this matrix. Slices are always indexed starting
    // at zero. This uses reference semantics, i.e. changing a value
    // in the slice changes the original.
    // <srcblock>
    // Matrix<double> vd(100,100);
    // //...
    // vd(Slice(0,10),Slice(10,10)) = -1.0; // 10x10 sub-matrix set to -1.0
    // </srcblock>
    // <group>
    Matrix<T, Alloc> operator()(const Slice &sliceX, const Slice &sliceY);
    const Matrix<T, Alloc> operator()(const Slice &sliceX, const Slice &sliceY) const;
    // </group>

    // Slice using IPositions. Required to be defined, otherwise the base
    // class versions are hidden.
    // <group>
    Array<T, Alloc> operator()(const IPosition &blc, const IPosition &trc,
			const IPosition &incr)
      { return Array<T, Alloc>::operator()(blc,trc,incr); }
    const Array<T, Alloc> operator()(const IPosition &blc, const IPosition &trc,
                              const IPosition &incr) const
      { return Array<T, Alloc>::operator()(blc,trc,incr); }
    Array<T, Alloc> operator()(const IPosition &blc, const IPosition &trc)
      { return Array<T, Alloc>::operator()(blc,trc); }
    const Array<T, Alloc> operator()(const IPosition &blc, const IPosition &trc) const
      { return Array<T, Alloc>::operator()(blc,trc); }
    Array<T, Alloc> operator()(const Slicer& slicer)
      { return Array<T, Alloc>::operator()(slicer); }
    const Array<T, Alloc> operator()(const Slicer& slicer) const
      { return Array<T, Alloc>::operator()(slicer); }
    // </group>

    // The length of each axis of the Matrix.
    const IPosition &shape() const
      { return this->length_p; }
    void shape(int &s1, int &s2) const
      { s1 = this->length_p(0); s2=this->length_p(1); }

    // The number of rows in the Matrix, i.e. the length of the first axis.
    size_t nrow() const
      { return this->length_p(0); }

    // The number of columns in the Matrix, i.e. the length of the 2nd axis.
    size_t ncolumn() const
      { return this->length_p(1); }

    // Checks that the Matrix is consistent (invariants check out).
    virtual bool ok() const override;

protected:
    virtual void preTakeStorage(const IPosition &shape) override;
    // Remove the degenerate axes from other and store result in this matrix.
    // An exception is thrown if removing degenerate axes does not result
    // in a matrix.
    virtual void doNonDegenerate(const Array<T, Alloc> &other,
                                 const IPosition &ignoreAxes) override;
                                 
    virtual size_t fixedDimensionality() const override { return 2; }

private:
    // Cached constants to improve indexing.
    // size_t xinc_p, yinc_p;
    
    size_t xinc() const { return this->inc_p(0); }
    size_t yinc() const { return this->inc_p(1)*this->originalLength_p(0); }
};

//# Declare extern templates for often used types.
  extern template class Matrix<bool>;
  extern template class Matrix<float>;
  extern template class Matrix<double>;

} //#End casa namespace

#include "Matrix.tcc"

#endif
