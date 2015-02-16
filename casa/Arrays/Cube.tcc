//# Cube.cc: A 3-D Specialization of the Array Class
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

#ifndef CASA_CUBE_TCC
#define CASA_CUBE_TCC

#include <casacore/casa/Arrays/Cube.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Slice.h>
#include <casacore/casa/Arrays/MaskedArray.h>
#include <casacore/casa/Arrays/ArrayError.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>


namespace casacore { //#Begin casa namespace

template<class T> Cube<T>::Cube()
: Array<T>(IPosition(3, 0))
{
    makeIndexingConstants();
    DebugAssert(ok(), ArrayError);
}

template<class T> Cube<T>::Cube(size_t l1, size_t l2, size_t l3)
: Array<T>(IPosition(3, l1, l2, l3))
{
    makeIndexingConstants();
    DebugAssert(ok(), ArrayError);
}

template<class T> Cube<T>::Cube(size_t l1, size_t l2, size_t l3,
				const T &initialValue)
: Array<T>(IPosition(3, l1, l2, l3), initialValue)
{
    makeIndexingConstants();
    DebugAssert(ok(), ArrayError);
}

template<class T> Cube<T>::Cube(const IPosition &len)
  : Array<T>(len)
{
    makeIndexingConstants();
    AlwaysAssert(len.nelements() == 3, ArrayError);
}

template<class T> Cube<T>::Cube(const IPosition &len, const T &initialValue)
  : Array<T>(len, initialValue)
{
    makeIndexingConstants();
    AlwaysAssert(len.nelements() == 3, ArrayError);
}

template<class T> Cube<T>::Cube(const Cube<T> &other)
  : Array<T>(other),
    xinc_p (other.xinc_p),
    yinc_p (other.yinc_p),
    zinc_p (other.zinc_p)
{
    DebugAssert(ok(), ArrayError);
}

// <thrown>
//   <item> ArrayNDimError
// </thrown>
template<class T> Cube<T>::Cube(const Array<T> &other)
: Array<T>(other)
{
    this->checkCubeShape();
    makeIndexingConstants();
    DebugAssert(ok(), ArrayError);
}

template<class T> Cube<T>::~Cube()
{}


template<class T> void Cube<T>::resize()
{
    resize (IPosition(3,0));
}
template<class T> void Cube<T>::resize(const IPosition &len, Bool copyValues)
{
    DebugAssert(ok(), ArrayError);
    if (len.nelements() != 3)
	throw(ArrayConformanceError("Cube<T>::resize() - attempt to form "
				    "non-Cube"));
    Array<T>::resize(len, copyValues);
    makeIndexingConstants();
}

template<class T> void Cube<T>::resize(size_t nx, size_t ny, size_t nz,
				       Bool copyValues)
{
    DebugAssert(ok(), ArrayError);
    IPosition l(3);
    l(0) = nx; l(1) = ny; l(2) = nz;
    Cube<T>::resize(l, copyValues);
}

// <thrown>
//    <item> ArrayNDimError
// </thrown>
template<class T> void Cube<T>::assign (const Array<T>& other)
{
    DebugAssert(ok(), ArrayError);
    if (other.ndim() != 3)
	throw(ArrayNDimError(3, other.ndim(), "Cube<T>::assign()"
			     " - attempt to assign from non-cube"));
    Array<T>::assign (other);
}

// <thrown>
//   <item> ArrayNDimError
// </thrown>
template<class T> void Cube<T>::reference(const Array<T> &other)
{
    DebugAssert(ok(), ArrayError);
    if (other.ndim() != 3)
	throw(ArrayNDimError(3, other.ndim(), "Cube<T>::reference()"
			     " - attempt to reference non-Cube"));
    Array<T>::reference(other);
    makeIndexingConstants();
}

template<class T> Cube<T> &Cube<T>::operator=(const Cube<T> &other)
{
    DebugAssert(ok(), ArrayError);
    if (this == &other)
        return *this;

    Bool Conform = this->conform(other);
    Array<T>::operator=(other);
    if (!Conform) {
	makeIndexingConstants();
    }

    return *this;
}

template<class T> Array<T> &Cube<T>::operator=(const Array<T> &a)
{
    DebugAssert(ok(), ArrayError);
    if (a.ndim() == 3) {
	Bool Conform = this->conform(a);
	Array<T>::operator=(a);
	if (!Conform) {
	    makeIndexingConstants();
	}
    } else {
	// This might work if a.ndim == 1 or 2
	Cube<T> tmp(a);
	(*this) = tmp;
    }
    return *this;
}

// <thrown>
//    <item> ArrayError
// </thrown>
template<class T> Cube<T> Cube<T>::operator()(const Slice &sliceX,
                                              const Slice &sliceY,
                                              const Slice &sliceZ)
{
    DebugAssert(ok(), ArrayError);
    Int64 b1, l1, s1, b2, l2, s2, b3,s3,l3;       // begin length step
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
    if (sliceZ.all()) {
	b3 = 0;
	l3 = this->length_p(2);
	s3 = 1;
    } else {
	b3 = sliceZ.start();
	l3 = sliceZ.length();
	s3 = sliceZ.inc();
    }

    // Check that the selected slice is valid
    if (s1 < 1 || s2 < 1 || s3 < 1) {
	throw(ArrayError("Cube<T>::operator()(Slice,Slice,Slice) : step < 1"));
    } else if (l1 < 0  || l2 < 0 || l3 < 0) {
	throw(ArrayError("Cube<T>::operator()(Slice,Slice,Slice): length < 0"));
    } else if ((b1+(l1-1)*s1 >= this->length_p(0)) || 
	       (b2+(l2-1)*s2 >= this->length_p(1)) ||
	       (b3+(l3-1)*s3 >= this->length_p(2))) {
	throw(ArrayError("Cube<T>::operator()(Slice,Slice,Slice) : "
			 "Desired slice extends beyond the end of the array"));
    } else if (b1 < 0 || b2 < 0 || b3 < 0) {
	throw(ArrayError("Cube<T>::operator()(Slice,Slice,Slice) : "
			 "start of slice before beginning of cube"));
   }

    // For simplicity, just use the Array<T> slicing. If this is found to be
    // a performance drag, we could special case this as we do for Vector.
    IPosition blc(3,b1,b2,b3);
    IPosition trc(3,b1+(l1-1)*s1,b2+(l2-1)*s2,b3+(l3-1)*s3);
    IPosition incr(3,s1,s2,s3);

    return this->operator()(blc,trc,incr);
}

template<class T> const Cube<T> Cube<T>::operator()
  (const Slice &sliceX, const Slice &sliceY, const Slice &sliceZ) const
{
    return const_cast<Cube<T>*>(this)->operator() (sliceX, sliceY, sliceZ);
}

template<class T> void Cube<T>::makeIndexingConstants()
{
    // No lAssert since the Cube often isn't constructed yet when
    // calling this
    xinc_p = this->inc_p(0);
    yinc_p = this->inc_p(1)*this->originalLength_p(0);
    zinc_p = this->inc_p(2)*this->originalLength_p(0)*this->originalLength_p(1);
}


template<class T>
void Cube<T>::doNonDegenerate (const Array<T> &other,
                               const IPosition &ignoreAxes)
{
    Array<T> tmp(*this);
    tmp.nonDegenerate (other, ignoreAxes);
    if (tmp.ndim() != 1) {
	throw (ArrayError ("Cube::nonDegenerate (other, ignoreAxes) - "
			   "removing degenerate axes from other "
			   "does not result in cube"));
    }
    reference (tmp);
}

// <thrown>
//   <item> ArrayConformanceError
// </thrown>
template<class T> Matrix<T> Cube<T>::xyPlane(size_t which)
{
    DebugAssert(ok(), ArrayError);
    if (Int64(which) >= this->length_p(2)) {
	throw(ArrayConformanceError("Cube<T>::xyPlane - plane > end"));
    }
    Cube<T> tmp((*this)(Slice(), Slice(), which));
    tmp.ndimen_p = 2;
    tmp.length_p.resize (2);
    tmp.inc_p.resize (2);
    tmp.originalLength_p.resize (2);
    tmp.makeSteps();
    return tmp; // should match Matrix<T>(const Array<T> &)
}


template<class T> const Matrix<T> Cube<T>::xyPlane(size_t which) const
{
    Cube<T> *This = const_cast<Cube<T>*>(this);
    // Cast away constness, but the return type is a const Matrix<T>, so
    // this should still be safe.
    return This->xyPlane(which);
}

template<class T> Matrix<T> Cube<T>::xzPlane(size_t which)
{
    DebugAssert(ok(), ArrayError);
    if (Int64(which) >= this->length_p(1)) {
	throw(ArrayConformanceError("Cube<T>::xzPlane - plane > end"));
    }
    Cube<T> tmp((*this)(Slice(), which, Slice()));
    // Keep axes 0 and 2, even if they have length 1.
    return tmp.nonDegenerate(IPosition(2,0,2));
}

template<class T> const Matrix<T> Cube<T>::xzPlane(size_t which) const
{
    Cube<T> *This = const_cast<Cube<T>*>(this);
    // Cast away constness, but the return type is a const Matrix<T>, so
    // this should still be safe.
    cout << "test" << endl;
    return This->xzPlane(which);
}

template<class T> Matrix<T> Cube<T>::yzPlane(size_t which)
{
    DebugAssert(ok(), ArrayError);
    if (Int64(which) >= this->length_p(0)) {
	throw(ArrayConformanceError("Cube<T>::yzPlane - plane > end"));
    }
    Cube<T> tmp((*this)(which, Slice(), Slice()));
    // Keep axes 1 and 2, even if they have length 1.
    return tmp.nonDegenerate(IPosition(2,1,2));
}


template<class T> const Matrix<T> Cube<T>::yzPlane(size_t which) const
{
    Cube<T> *This = const_cast<Cube<T>*>(this);
    // Cast away constness, but the return type is a const Matrix<T>, so
    // this should still be safe.
    return This->yzPlane(which);
}


template<class T> Bool Cube<T>::ok() const
{
    return ( (this->ndim() == 3) ? (Array<T>::ok()) : False );
}

template<class T>
Cube<T>::Cube(const IPosition &shape, T *storage, 
		  StorageInitPolicy policy)
  : Array<T>(shape, storage, policy)
{
    AlwaysAssert(shape.nelements() == 3, ArrayError);
    makeIndexingConstants();
}

template<class T>
Cube<T>::Cube(const IPosition &shape, const T *storage)
  : Array<T>(shape, storage)
{
    AlwaysAssert(shape.nelements() == 3, ArrayError);
    makeIndexingConstants();
}


template<class T>
void Cube<T>::takeStorage(const IPosition &shape, T *storage,
		     StorageInitPolicy policy)
{
    AlwaysAssert(shape.nelements() == 3, ArrayError);
    Array<T>::takeStorage(shape, storage, policy);
    makeIndexingConstants();
}

template<class T>
void Cube<T>::takeStorage(const IPosition &shape, const T *storage)
{
    AlwaysAssert(shape.nelements() == 3, ArrayError);
    Array<T>::takeStorage(shape, storage);
    makeIndexingConstants();
}

} //#End casa namespace

#endif
