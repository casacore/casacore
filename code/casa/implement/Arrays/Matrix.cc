//# Matrix.cc: A 2-D Specialization of the Array Class
//# Copyright (C) 1993,1994,1995,1996,1997,1998,1999
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

#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/ArrayError.h>
#include <aips/Utilities/Assert.h>
#include <iostream.h>

//# Implement rtti functions.
rtti_imp_mbrf_a1(Matrix);

template<class T> Matrix<T>::Matrix()
: Array<T>(IPosition(2, 0))
{
    makeIndexingConstants();
    DebugAssert(ok(), ArrayError);
}

template<class T> Matrix<T>::Matrix(const IPosition &len)
  : Array<T>(len)
{
    makeIndexingConstants();
    AlwaysAssert(len.nelements() == 2, ArrayError);
}

template<class T> Matrix<T>::Matrix(const IPosition &len, const T &initialValue)
  : Array<T>(len, initialValue)
{
    makeIndexingConstants();
    AlwaysAssert(len.nelements() == 2, ArrayError);
}

template<class T> Matrix<T>::Matrix(uInt l1, uInt l2)
: Array<T>(IPosition(2, l1, l2))
{
    makeIndexingConstants();
    DebugAssert(ok(), ArrayError);
}

template<class T> Matrix<T>::Matrix(uInt l1, uInt l2, const T &initialValue)
: Array<T>(IPosition(2, l1, l2), initialValue)
{
    makeIndexingConstants();
    DebugAssert(ok(), ArrayError);
}


template<class T> Matrix<T>::Matrix(const Matrix<T> &other)
: Array<T>(other)
{
    makeIndexingConstants();
    DebugAssert(ok(), ArrayError);
}

// <thrown>
//    <item> ArrayNDimError
// </thrown>
template<class T> Matrix<T>::Matrix(const Array<T> &other)
: Array<T>(other)
{
    if (ndim() > 2)
	throw(ArrayNDimError(2, other.ndim(), "Matrix<T>::Matrix"
			     "(const Array<T> &): ndim of other > 2"));
    // We need to fiddle a bit if the ndim is == 1
    if (ndim() == 1) {
	ndimen_p = 2;
	length_p.resize(2); 
	inc_p.resize(2);
	originalLength_p.resize(2);
	length_p(1) = 1;
	inc_p(1) = 1;
	originalLength_p(1) = 1;
    }
    nels_p = length_p.product();
    makeIndexingConstants();
    DebugAssert(ok(), ArrayError);
}

template<class T> Matrix<T>::~Matrix()
{}


// <thrown>
//   <item> ArrayConformanceError
// </thrown>
template<class T> void Matrix<T>::resize()
{
    resize (IPosition(2,0));
}
template<class T> void Matrix<T>::resize(const IPosition &l)
{
    DebugAssert(ok(), ArrayError);
    if (l.nelements() != 2)
	throw(ArrayConformanceError("Matrix<T>::resize() - attempt to form "
				    "non-Matrix"));
    Array<T>::resize(l);
    makeIndexingConstants();
}

template<class T> void Matrix<T>::resize(uInt nx, uInt ny)
{
    DebugAssert(ok(), ArrayError);
    IPosition l(2);
    l(0) = nx; l(1) = ny;
    Matrix<T>::resize(l);
}

// <thrown>
//    <item> ArrayNDimError
// </thrown>
template<class T> void Matrix<T>::reference(Array<T> &other)
{
    DebugAssert(ok(), ArrayError);
    if (other.ndim() == 2) {
	Array<T>::reference(other);
    } else if (other.ndim() == 1) {
	length_p(0) = other.length_p(0); length_p(1) = 1;
	nels_p = other.nels_p;
	originalLength_p(0) = other.originalLength_p(0);
	originalLength_p(1) = 1;
	inc_p(0) = other.inc_p(0); inc_p(1) = 1;
	data_p = other.data_p;
	begin_p = other.begin_p; 
    } else {
	throw(ArrayNDimError(2, other.ndim(), "Matrix<T>::reference()"
			     " - attempt to reference non-Matrix"));
    }
    makeIndexingConstants();
}

template<class T> Matrix<T> &Matrix<T>::operator=(const Matrix<T> &other)
{
    DebugAssert(ok(), ArrayError);
    if (this == &other)
        return *this;

    Bool Conform = conform(other);
    if (Conform == False && nelements() != 0)
	validateConformance(other);  // We can't overwrite, so throw exception

    Array<T>::operator=(other);
    if (!Conform) {
	makeIndexingConstants();
    }
    
    return *this;
}

template<class T> Array<T> &Matrix<T>::operator=(const Array<T> &a)
{
    DebugAssert(ok(), ArrayError);
    Bool Conform = conform(a);
    if (a.ndim() == 2) {
	Array<T>::operator=(a);
	if (!Conform) {
	    makeIndexingConstants();
	}
    } else {
	// This will work if a is 1D
	Matrix<T> tmp(a);
	(*this) = tmp;
    }
    return *this;
}

// <thrown>
//    <item> ArrayError
// </thrown>
template<class T> Matrix<T> Matrix<T>::operator()(const Slice &sliceX,
						  const Slice &sliceY)
{
    DebugAssert(ok(), ArrayError);
    Int b1, l1, s1, b2, l2, s2;       // begin length step
    if (sliceX.all()) {
	b1 = 0;
	l1 = length_p(0);
	s1 = 1;
    } else {
	b1 = sliceX.start();
	l1 = sliceX.length();
	s1 = sliceX.inc();
    }
    if (sliceY.all()) {
	b2 = 0;
	l2 = length_p(1);
	s2 = 1;
    } else {
	b2 = sliceY.start();
	l2 = sliceY.length();
	s2 = sliceY.inc();
    }

    // Check that the selected slice is valid
    if (s1 < 1 || s2<1) {
	throw(ArrayError("Matrix<T>::operator()(Slice,Slice) : step < 1"));
    } else if (l1 < 0  || l2 < 0) {
	throw(ArrayError("Matrix<T>::operator()(Slice,Slice) : length < 0"));
    } else if ((b1+(l1-1)*s1 >= length_p(0)) || 
	       (b2+(l2-1)*s2 >= length_p(1))) {
	throw(ArrayError("Matrix<T>::operator()(Slice,Slice): desired slice"
			 " extends beyond the end of the array"));
    } else if (b1 < 0 || b2 < 0) {
	throw(ArrayError("Matrix<T>::operator()(Slice,Slice) : start of slice "
			 "before beginning of matrix"));
    }

    // For simplicity, just use the Array<T> slicing. If this is found to be
    // a performance drag, we could special case this as we do for Vector.
    IPosition blc(2,b1,b2);
    IPosition trc(2,b1+(l1-1)*s1,b2+(l2-1)*s2);
    IPosition incr(2,s1,s2);
    return this->operator()(blc,trc,incr);
}

// <thrown>
//   <item> ArrayConformanceError
// </thrown>
template<class T> Vector<T> Matrix<T>::row(uInt n)
{
    DebugAssert(ok(), ArrayError);
    if (Int(n) >= length_p(0)) {
	throw(ArrayConformanceError("Matrix<T>::row - row < 0 or > end"));
    }
    Matrix<T> tmp((*this)(n, Slice())); // A reference
    tmp.ndimen_p = 1;
    tmp.length_p(0) = tmp.length_p(1);
    tmp.inc_p(0) = inc_p(1)*length_p(0)*inc_p(0);
    // "Lie" about the original length so that ok() doesn't spuriously fail
    // the test length[i] < originalLength (basically we've "swapped" axes).
    tmp.originalLength_p(0) = tmp.length_p(0)*tmp.inc_p(0);
    tmp.length_p.resize (1);
    tmp.inc_p.resize (1);
    tmp.originalLength_p.resize (1);
    tmp.nels_p = tmp.length_p(0);
    tmp.contiguous_p = tmp.isStorageContiguous();
    return tmp; // should match Vector<T>(const Array<T> &)
}

// <thrown>
//   <item> ArrayConformanceError
// </thrown>
template<class T> Vector<T> Matrix<T>::column(uInt n)
{
    DebugAssert(ok(), ArrayError);
    if (Int(n) >= length_p(1)) {
	throw(ArrayConformanceError("Matrix<T>::column - column < 0 or > end"));
    }
    Matrix<T> tmp((*this)(Slice(), n)); // A reference
    tmp.ndimen_p = 1;
    tmp.length_p.resize (1);
    tmp.inc_p.resize (1);
    tmp.originalLength_p.resize (1);
    tmp.nels_p = tmp.length_p(0);
    tmp.contiguous_p = tmp.isStorageContiguous();
    return tmp; // should match Vector<T>(const Array<T> &)

}

// <thrown>
//   <item> ArrayConformanceError
// </thrown>
template<class T> Vector<T> Matrix<T>::diagonal(Int n)
{
    DebugAssert(ok(), ArrayError);
    Int absn;
    if (n < 0) absn = -n; else absn = n;

    if (length_p(0) != length_p(1))
	throw(ArrayConformanceError("Matrix<T>::diagonal() - "
				    "non-square matrix"));

    if (absn >= length_p(0))
	throw(ArrayConformanceError("Matrix<T>::diagonal() - "
				    "diagonal out of range"));

    Int r, c;
    if ( n < 0 ) {
	r = absn;
	c = 0;
    } else {
	r = 0;
	c = absn;
    }
    Int len = length_p(0) - absn;
    Matrix<T> tmp((*this)(Slice(r,len), Slice(c)));
    tmp.ndimen_p = 1;
    tmp.length_p.resize (1);
    tmp.inc_p.resize (1);
    tmp.originalLength_p.resize (1);
    tmp.nels_p = tmp.length_p(0);
    tmp.inc_p(0) += inc_p(0)*length_p(0);
    tmp.contiguous_p = False;
	
    return tmp;  // should match Vector<T>(const Array<T> &)
}

#if defined (AIPS_IRIX)
template<class T> Vector<T> Matrix<T>::row(uInt n) const
#else
template<class T> const Vector<T> Matrix<T>::row(uInt n) const
#endif
{
    DebugAssert(ok(), ArrayError);
    // Cast away constness of this so we do not have to duplicate code.
    // Because the return type is const we are not actually violating
    // constness.
    Matrix<T> *This = const_cast<Matrix<T>*>(this);
    return This->row(n);
}

#if defined (AIPS_IRIX)
template<class T> Vector<T> Matrix<T>::column(uInt n) const
#else
template<class T> const Vector<T> Matrix<T>::column(uInt n) const
#endif
{
    DebugAssert(ok(), ArrayError);
    // Cast away constness of this so we do not have to duplicate code.
    // Because the return type is const we are not actually violating
    // constness.
    Matrix<T> *This = const_cast<Matrix<T>*>(this);
    return This->column(n);
}

// If the matrix isn't squre, this will throw an exception.
#if defined (AIPS_IRIX)
template<class T> Vector<T> Matrix<T>::diagonal(Int n) const
#else
template<class T> const Vector<T> Matrix<T>::diagonal(Int n) const
#endif
{
    DebugAssert(ok(), ArrayError);
    // Cast away constness of this so we do not have to duplicate code.
    // Because the return type is const we are not actually violating
    // constness.
    Matrix<T> *This = const_cast<Matrix<T>*>(this);
    return This->diagonal(n);
}

// Set up constants for efficient indexing
template<class T> void Matrix<T>::makeIndexingConstants()
{
    // No lAssert since the Matrix often isn't constructed yet when
    // calling this
    xinc_p = inc_p(0);
    yinc_p = inc_p(1)*originalLength_p(0);
}


template<class T>
void Matrix<T>::doNonDegenerate (Array<T> &other, const IPosition &ignoreAxes)
{
    Array<T> tmp(*this);
    tmp.nonDegenerate (other, ignoreAxes);
    if (tmp.ndim() != 1) {
	throw (ArrayError ("Matrix::nonDegenerate (other, ignoreAxes) - "
			   "removing degenerate axes from other "
			   "does not result in matrix"));
    }
    reference (tmp);
}

template<class T> IPosition Matrix<T>::end() const
{
    DebugAssert(ok(), ArrayError);
    return Array<T>::end();
}

template<class T> Bool Matrix<T>::ok() const
{
    return ( (ndim() == 2) ? (Array<T>::ok()) : False );
}


template<class T>
Matrix<T>::Matrix(const IPosition &shape, T *storage, 
		  StorageInitPolicy policy)
  : Array<T>(shape, storage, policy)
{
    AlwaysAssert(shape.nelements() == 2, ArrayError);
    makeIndexingConstants();
}

template<class T>
Matrix<T>::Matrix(const IPosition &shape, const T *storage)
  : Array<T>(shape, storage)
{
    AlwaysAssert(shape.nelements() == 2, ArrayError);
    makeIndexingConstants();
}


template<class T>
void Matrix<T>::takeStorage(const IPosition &shape, T *storage,
		     StorageInitPolicy policy)
{
    AlwaysAssert(shape.nelements() == 2, ArrayError);
    Array<T>::takeStorage(shape, storage, policy);
    makeIndexingConstants();
}

template<class T>
void Matrix<T>::takeStorage(const IPosition &shape, const T *storage)
{
    AlwaysAssert(shape.nelements() == 2, ArrayError);
    Array<T>::takeStorage(shape, storage);
    makeIndexingConstants();
}
