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

#ifndef CASA_CUBE_2_TCC
#define CASA_CUBE_2_TCC

#include "Cube.h"
#include "Matrix.h"
#include "Slice.h"
#include "MaskedArray.h"
#include "ArrayError.h"

namespace casacore {

template<typename T, typename Alloc> Cube<T, Alloc>::Cube()
: Array<T>(IPosition(3, 0))
{
  assert(ok());
}

template<typename T, typename Alloc> Cube<T, Alloc>::Cube(size_t l1, size_t l2, size_t l3)
: Array<T>(IPosition(3, l1, l2, l3))
{
  assert(ok());
}

template<typename T, typename Alloc> Cube<T, Alloc>::Cube(size_t l1, size_t l2, size_t l3,
				const T &initialValue)
: Array<T>(IPosition(3, l1, l2, l3), initialValue)
{
    assert(ok());
}

template<typename T, typename Alloc> Cube<T, Alloc>::Cube(const IPosition &len)
  : Array<T>(len)
{
  this->checkBeforeResize(len);
}

template<typename T, typename Alloc> Cube<T, Alloc>::Cube(const IPosition &len, const T &initialValue)
  : Array<T>(len, initialValue)
{
  this->checkBeforeResize(len);
}

template<typename T, typename Alloc> Cube<T, Alloc>::Cube(const Cube<T, Alloc> &source)
  : Array<T>(source)
{
  assert(ok());
}

template<typename T, typename Alloc> Cube<T, Alloc>::Cube(Cube<T, Alloc>&& source)
  : Array<T>(std::move(source))
{
  assert(ok());
}

// <thrown>
//   <item> ArrayNDimError
// </thrown>
template<typename T, typename Alloc> Cube<T, Alloc>::Cube(const Array<T, Alloc> &source)
: Array<T>(source)
{
  this->checkCubeShape();
  assert(ok());
}

template<typename T, typename Alloc> Cube<T, Alloc>::Cube(Array<T, Alloc>&& source)
: Array<T>(std::move(source))
{
  this->checkCubeShape();
  assert(ok());
}

template<typename T, typename Alloc>
void Cube<T, Alloc>::resize(size_t nx, size_t ny, size_t nz, bool copyValues)
{
    assert(ok());
    IPosition l(3);
    l(0) = nx; l(1) = ny; l(2) = nz;
    Cube<T, Alloc>::resize(l, copyValues);
}

// <thrown>
//    <item> ArrayError
// </thrown>
template<typename T, typename Alloc> Cube<T, Alloc> Cube<T, Alloc>::operator()(const Slice &sliceX,
                                              const Slice &sliceY,
                                              const Slice &sliceZ)
{
    assert(ok());
    long long b1, l1, s1, b2, l2, s2, b3,s3,l3;       // begin length step
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
	throw(ArrayError("Cube<T, Alloc>::operator()(Slice,Slice,Slice) : step < 1"));
    } else if (l1 < 0  || l2 < 0 || l3 < 0) {
	throw(ArrayError("Cube<T, Alloc>::operator()(Slice,Slice,Slice): length < 0"));
    } else if ((b1+(l1-1)*s1 >= this->length_p(0)) || 
	       (b2+(l2-1)*s2 >= this->length_p(1)) ||
	       (b3+(l3-1)*s3 >= this->length_p(2))) {
	throw(ArrayError("Cube<T, Alloc>::operator()(Slice,Slice,Slice) : "
			 "Desired slice extends beyond the end of the array"));
    } else if (b1 < 0 || b2 < 0 || b3 < 0) {
	throw(ArrayError("Cube<T, Alloc>::operator()(Slice,Slice,Slice) : "
			 "start of slice before beginning of cube"));
   }

    // For simplicity, just use the Array<T> slicing. If this is found to be
    // a performance drag, we could special case this as we do for Vector.
    IPosition blc(3,b1,b2,b3);
    IPosition trc(3,b1+(l1-1)*s1,b2+(l2-1)*s2,b3+(l3-1)*s3);
    IPosition incr(3,s1,s2,s3);

    return this->operator()(blc,trc,incr);
}

template<typename T, typename Alloc> const Cube<T, Alloc> Cube<T, Alloc>::operator()
  (const Slice &sliceX, const Slice &sliceY, const Slice &sliceZ) const
{
    return const_cast<Cube<T, Alloc>*>(this)->operator() (sliceX, sliceY, sliceZ);
}

template<typename T, typename Alloc>
void Cube<T, Alloc>::doNonDegenerate (const Array<T> &other,
                               const IPosition &ignoreAxes)
{
    Array<T> tmp(*this);
    tmp.nonDegenerate (other, ignoreAxes);
    if (tmp.ndim() != 3) {
	throw (ArrayError ("Cube::nonDegenerate (other, ignoreAxes) - "
			   "removing degenerate axes from other "
			   "does not result in cube"));
    }
    this->reference (tmp);
}

// <thrown>
//   <item> ArrayConformanceError
// </thrown>
template<typename T, typename Alloc> Matrix<T, Alloc> Cube<T, Alloc>::xyPlane(size_t which)
{
    assert(ok());
    if ((long long)(which) >= this->length_p(2)) {
	throw(ArrayConformanceError("Cube<T, Alloc>::xyPlane - plane > end"));
    }
    Cube<T, Alloc> tmp((*this)(Slice(), Slice(), which));
    tmp.ndimen_p = 2;
    tmp.length_p.resize (2);
    tmp.inc_p.resize (2);
    tmp.originalLength_p.resize (2);
    tmp.makeSteps();
    return Matrix<T, Alloc>(tmp); // should match Matrix<T>(const Array<T> &)
}


template<typename T, typename Alloc> const Matrix<T, Alloc> Cube<T, Alloc>::xyPlane(size_t which) const
{
    Cube<T, Alloc> *This = const_cast<Cube<T, Alloc>*>(this);
    // Cast away constness, but the return type is a const Matrix<T>, so
    // this should still be safe.
    return This->xyPlane(which);
}

template<typename T, typename Alloc> Matrix<T, Alloc> Cube<T, Alloc>::xzPlane(size_t which)
{
    assert(ok());
    if ((long long)(which) >= this->length_p(1)) {
	throw(ArrayConformanceError("Cube<T, Alloc>::xzPlane - plane > end"));
    }
    Cube<T, Alloc> tmp((*this)(Slice(), which, Slice()));
    // Keep axes 0 and 2, even if they have length 1.
    return tmp.nonDegenerate(IPosition(2,0,2));
}

template<typename T, typename Alloc> const Matrix<T, Alloc> Cube<T, Alloc>::xzPlane(size_t which) const
{
    Cube<T, Alloc> *This = const_cast<Cube<T, Alloc>*>(this);
    // Cast away constness, but the return type is a const Matrix<T>, so
    // this should still be safe.
    return This->xzPlane(which);
}

template<typename T, typename Alloc> Matrix<T, Alloc> Cube<T, Alloc>::yzPlane(size_t which)
{
    assert(ok());
    if ((long long)(which) >= this->length_p(0)) {
	throw(ArrayConformanceError("Cube<T, Alloc>::yzPlane - plane > end"));
    }
    Cube<T, Alloc> tmp((*this)(which, Slice(), Slice()));
    // Keep axes 1 and 2, even if they have length 1.
    return tmp.nonDegenerate(IPosition(2,1,2));
}


template<typename T, typename Alloc> const Matrix<T, Alloc> Cube<T, Alloc>::yzPlane(size_t which) const
{
    Cube<T, Alloc> *This = const_cast<Cube<T, Alloc>*>(this);
    // Cast away constness, but the return type is a const Matrix<T>, so
    // this should still be safe.
    return This->yzPlane(which);
}


template<typename T, typename Alloc> bool Cube<T, Alloc>::ok() const
{
    return ( (this->ndim() == 3) ? (Array<T>::ok()) : false );
}

template<typename T, typename Alloc>
Cube<T, Alloc>::Cube(const IPosition &shape, T *storage, 
		  StorageInitPolicy policy)
  : Array<T>(shape, storage, policy)
{
  if(shape.nelements() != 3)
    throw ArrayError("len.nelements() != 3");
}

template<typename T, typename Alloc>
Cube<T, Alloc>::Cube(const IPosition &shape, T *storage,
                  StorageInitPolicy policy, const Alloc& allocator)
  : Array<T>(shape, storage, policy, allocator)
{
  if(shape.nelements() != 3)
    throw ArrayError("len.nelements() != 3");
}

template<typename T, typename Alloc>
Cube<T, Alloc>::Cube(const IPosition &shape, const T *storage)
  : Array<T>(shape, storage)
{
   if(shape.nelements() != 3)
    throw ArrayError("len.nelements() != 3");
}


template<typename T, typename Alloc>
void Cube<T, Alloc>::preTakeStorage(const IPosition &shape)
{
  Array<T>::preTakeStorage(shape);
  if(shape.nelements() != 3)
    throw ArrayError("len.nelements() != 3");
}

} //#End casa namespace

#endif
