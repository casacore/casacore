//# Matrix.cc: A 2-D Specialization of the Array Class
//# Copyright (C) 1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003
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

#ifndef CASA_MATRIX_2_TCC
#define CASA_MATRIX_2_TCC

#include "Matrix.h"
#include "Vector.h"
#include "Slice.h"
#include "MaskedArray.h"
#include "ArrayError.h"

#include <cassert>

namespace casacore { //#Begin casa namespace

template<typename T, typename Alloc> Matrix<T, Alloc>::Matrix(const Alloc& allocator)
: Array<T, Alloc>(IPosition(2, 0), allocator)
{
  assert(ok());
}

/*template<typename T, typename Alloc> Matrix<T, Alloc>::Matrix(const IPosition &len, ArrayInitPolicy initPolicy)
  : Array<T, Alloc>(len, initPolicy)
{
    makeIndexingConstants();
    AlwaysAssert(len.nelements() == 2, ArrayError);
}*/

template<typename T, typename Alloc> Matrix<T, Alloc>::Matrix(const IPosition &length, const T &initialValue, const Alloc& allocator)
  : Array<T, Alloc>(length, initialValue, allocator)
{
  Array<T, Alloc>::checkBeforeResize(length);
}

template<typename T, typename Alloc> Matrix<T, Alloc>::Matrix(const IPosition &length, typename Array<T, Alloc>::uninitializedType, const Alloc& allocator)
  : Array<T, Alloc>(length, Array<T, Alloc>::uninitialized, allocator)
{
  Array<T, Alloc>::checkBeforeResize(length);
}

template<typename T, typename Alloc> Matrix<T, Alloc>::Matrix(size_t l1, size_t l2, const T &initialValue, const Alloc& allocator)
: Array<T, Alloc>(IPosition(2, l1, l2), initialValue, allocator)
{
  assert(ok());
}

template<typename T, typename Alloc> Matrix<T, Alloc>::Matrix(size_t l1, size_t l2, typename Array<T, Alloc>::uninitializedType, const Alloc& allocator)
: Array<T, Alloc>(IPosition(2, l1, l2), Array<T, Alloc>::uninitialized, allocator)
{
  assert(ok());
}

template<typename T, typename Alloc> Matrix<T, Alloc>::Matrix(const Matrix<T, Alloc>& source)
  : Array<T, Alloc>(source)
{
  assert(ok());
}

template<typename T, typename Alloc> Matrix<T, Alloc>::Matrix(Matrix<T, Alloc>&& source)
  : Array<T, Alloc>(std::move(source), IPosition(2, 0))
{
  assert(ok());
}

// <thrown>
//    <item> ArrayNDimError
// </thrown>
template<typename T, typename Alloc> Matrix<T, Alloc>::Matrix(const Array<T, Alloc>& source)
: Array<T, Alloc>(source)
{
  this->checkMatrixShape();
  assert(ok());
}

template<typename T, typename Alloc> Matrix<T, Alloc>::Matrix(Array<T, Alloc>&& source)
: Array<T, Alloc>(std::move(source))
{
  this->checkMatrixShape();
  assert(ok());
}

template<typename T, typename Alloc>
Matrix<T, Alloc>::Matrix(const IPosition &shape, T *storage,
                  StorageInitPolicy policy, const Alloc& allocator)
  : Array<T, Alloc>(shape, storage, policy, allocator)
{
  Array<T, Alloc>::checkBeforeResize(shape);
}

template<typename T, typename Alloc>
Matrix<T, Alloc>::Matrix(const IPosition &shape, const T *storage)
  : Array<T, Alloc>(shape, storage)
{
  Array<T, Alloc>::checkBeforeResize(shape);
}

template<typename T, typename Alloc> void Matrix<T, Alloc>::resize(size_t nx, size_t ny, bool copyValues)
{
  Array<T, Alloc>::resize(IPosition{ssize_t(nx), ssize_t(ny)}, copyValues);
  assert(ok());
}

// <thrown>
//    <item> ArrayError
// </thrown>
template<typename T, typename Alloc> Matrix<T, Alloc> Matrix<T, Alloc>::operator()(const Slice &sliceX,
						  const Slice &sliceY)
{
  assert(ok());
    long long b1, l1, s1, b2, l2, s2;       // begin length step
    if (sliceX.all()) {
	b1 = 0;
	l1 = this->length_p(0);
	s1 = 1;
    } else {
	b1 = sliceX.start();
	l1 = sliceX.length();
	s1 = sliceX.inc();
    }
    if (sliceY.all()) {
	b2 = 0;
	l2 = this->length_p(1);
	s2 = 1;
    } else {
	b2 = sliceY.start();
	l2 = sliceY.length();
	s2 = sliceY.inc();
    }

    // Check that the selected slice is valid
    if (s1 < 1 || s2<1) {
	throw(ArrayError("Matrix<T, Alloc>::operator()(Slice,Slice) : step < 1"));
    } else if (l1 < 0  || l2 < 0) {
	throw(ArrayError("Matrix<T, Alloc>::operator()(Slice,Slice) : length < 0"));
    } else if ((b1+(l1-1)*s1 >= this->length_p(0)) || 
	       (b2+(l2-1)*s2 >= this->length_p(1))) {
	throw(ArrayError("Matrix<T, Alloc>::operator()(Slice,Slice): desired slice"
			 " extends beyond the end of the array"));
    } else if (b1 < 0 || b2 < 0) {
	throw(ArrayError("Matrix<T, Alloc>::operator()(Slice,Slice) : start of slice "
			 "before beginning of matrix"));
    }

    // For simplicity, just use the Array<T, Alloc> slicing. If this is found to be
    // a performance drag, we could special case this as we do for Vector.
    IPosition blc(2,b1,b2);
    IPosition trc(2,b1+(l1-1)*s1,b2+(l2-1)*s2);
    IPosition incr(2,s1,s2);
    return this->operator()(blc,trc,incr);
}
template<typename T, typename Alloc> const Matrix<T, Alloc> Matrix<T, Alloc>::operator()
  (const Slice &sliceX, const Slice &sliceY) const
{
    return const_cast<Matrix<T, Alloc>*>(this)->operator() (sliceX, sliceY);
}

// <thrown>
//   <item> ArrayConformanceError
// </thrown>
template<typename T, typename Alloc> Vector<T, Alloc> Matrix<T, Alloc>::row(size_t n)
{
  assert(ok());
    if ((long long)(n) >= this->length_p(0)) {
	throw(ArrayConformanceError("Matrix<T, Alloc>::row - row < 0 or > end"));
    }
    Matrix<T, Alloc> tmp((*this)(n, Slice())); // A reference
    tmp.ndimen_p = 1;
    tmp.length_p(0) = tmp.length_p(1);
    tmp.inc_p(0) = this->steps_p(1);
    // "Lie" about the original length so that ok() doesn't spuriously fail
    // the test length[i] < originalLength (basically we've "swapped" axes).
    tmp.originalLength_p(0) = tmp.originalLength_p(1);
    tmp.length_p.resize (1);
    tmp.inc_p.resize (1);
    tmp.originalLength_p.resize (1);
    tmp.nels_p = tmp.length_p(0);
    tmp.contiguous_p = tmp.isStorageContiguous();
    tmp.makeSteps();
    return tmp; // should match Vector<T, Alloc>(const Array<T, Alloc> &)
}

// <thrown>
//   <item> ArrayConformanceError
// </thrown>
template<typename T, typename Alloc> Vector<T, Alloc> Matrix<T, Alloc>::column(size_t n)
{
  assert(ok());
    if ((long long)(n) >= this->length_p(1)) {
	throw(ArrayConformanceError("Matrix<T, Alloc>::column - column < 0 or > end"));
    }
    Matrix<T, Alloc> tmp((*this)(Slice(), n)); // A reference
    tmp.ndimen_p = 1;
    tmp.length_p.resize (1);
    tmp.inc_p.resize (1);
    tmp.originalLength_p.resize (1);
    tmp.nels_p = tmp.length_p(0);
    tmp.contiguous_p = tmp.isStorageContiguous();
    tmp.makeSteps();
    return tmp; // should match Vector<T, Alloc>(const Array<T, Alloc> &)

}

// <thrown>
//   <item> ArrayConformanceError
// </thrown>
template<typename T, typename Alloc> Vector<T, Alloc> Matrix<T, Alloc>::diagonal(long long n)
{
  assert(ok());
    Matrix<T, Alloc> tmp(*this);
    tmp.begin_p += tmp.makeDiagonal (0, n);
    tmp.makeSteps();
    return tmp;  // should match Vector<T, Alloc>(const Array<T, Alloc> &)
}

template<typename T, typename Alloc> const Vector<T, Alloc> Matrix<T, Alloc>::row(size_t n) const
{
    assert(ok());
    // Cast away constness of this so we do not have to duplicate code.
    // Because the return type is const we are not actually violating
    // constness.
    Matrix<T, Alloc> *This = const_cast<Matrix<T, Alloc>*>(this);
    return This->row(n);
}

template<typename T, typename Alloc> const Vector<T, Alloc> Matrix<T, Alloc>::column(size_t n) const
{
  assert(ok());
    // Cast away constness of this so we do not have to duplicate code.
    // Because the return type is const we are not actually violating
    // constness.
    Matrix<T, Alloc> *This = const_cast<Matrix<T, Alloc>*>(this);
    return This->column(n);
}

// If the matrix isn't square, this will throw an exception.
template<typename T, typename Alloc> const Vector<T, Alloc> Matrix<T, Alloc>::diagonal(long long n) const
{
  assert(ok());
    // Cast away constness of this so we do not have to duplicate code.
    // Because the return type is const we are not actually violating
    // constness.
    Matrix<T, Alloc> *This = const_cast<Matrix<T, Alloc>*>(this);
    return This->diagonal(n);
}

template<typename T, typename Alloc> Matrix<T, Alloc> Matrix<T, Alloc>::identity(size_t n)
{
    Matrix<T, Alloc> m(n, n, T(0));
    T* ptr = m.data();
    for (size_t i=0; i<n; i++) {
        *ptr = T(1);
        ptr += n+1;
    }
    return m;
}

template<typename T, typename Alloc>
void Matrix<T, Alloc>::doNonDegenerate (const Array<T, Alloc> &other,
                                 const IPosition &ignoreAxes)
{
    Array<T, Alloc> tmp(*this);
    tmp.nonDegenerate (other, ignoreAxes);
    if (tmp.ndim() != 2) {
	throw (ArrayError ("Matrix::nonDegenerate (other, ignoreAxes) - "
			   "removing degenerate axes from other "
			   "does not result in matrix"));
    }
    this->reference (tmp);
}

template<typename T, typename Alloc> bool Matrix<T, Alloc>::ok() const
{
    return ( (this->ndim() == 2) ? (Array<T, Alloc>::ok()) : false );
}

template<typename T, typename Alloc>
void Matrix<T, Alloc>::preTakeStorage(const IPosition &shape)
{
  Array<T, Alloc>::preTakeStorage(shape);
  if(shape.nelements() != 2)
    throw ArrayError("shape.nelements() != 2");
}

} //#End casa namespace

#endif
