//# Cube.cc: A 3-D Specialization of the Array Class
//# Copyright (C) 1993,1994,1995,1996,1997
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

#include <aips/Arrays/Cube.h>
#include <aips/Arrays/ArrayError.h>
#include <aips/Utilities/Assert.h>
#include <iostream.h>

//# Implement rtti functions.
rtti_imp_mbrf_a1(Cube);

template<class T> Cube<T>::Cube()
: Array<T>(IPosition(3, 0, 0, 0))
{
    makeIndexingConstants();
    DebugAssert(ok(), ArrayError);
}


template<class T> Cube<T>::Cube(uInt l1, uInt l2, uInt l3, 
                                Int o1, Int o2, Int o3)
: Array<T>(IPosition(3, l1, l2, l3), IPosition(3, o1, o2, o3))
{
    makeIndexingConstants();
    DebugAssert(ok(), ArrayError);
}

template<class T> Cube<T>::Cube(uInt l1, uInt l2, uInt l3)
: Array<T>(IPosition(3, l1, l2, l3))
{
    makeIndexingConstants();
    DebugAssert(ok(), ArrayError);
}

template<class T> Cube<T>::Cube(const IPosition &len, const IPosition &or)
  : Array<T>(len,or)
{
    makeIndexingConstants();
    AlwaysAssert(len.nelements() == 3 && or.nelements() == 3, ArrayError);
}

template<class T> Cube<T>::Cube(const Cube<T> &other)
: Array<T>(other)
{
    makeIndexingConstants();
    DebugAssert(ok(), ArrayError);
}

// <thrown>
//   <item> ArrayNDimError
// </thrown>
template<class T> Cube<T>::Cube(const Array<T> &other)
: Array<T>(other)
{
    if (ndim() > 3 || ndim() < 1)
	throw(ArrayNDimError(2, other.ndim(), "Cube<T>::Cube"
			     "(const Array<T> &): ndim of other > 3"));
    // We need to fiddle a bit if the ndim is == 1 or 2
    if (ndim() == 1) {
	ndimen = 3;
	start.resize(3); length.resize(3); inc.resize(3); 
	originalLength.resize(3);
	start[1] = 0; length[1] = 1; inc[1] = 1;
	start[2] = 0; length[2] = 1; inc[2] = 1;
	originalLength[1] = 1;
	originalLength[2] = 1;
    } else if (ndim() == 2) {
	ndimen = 3;
	start.resize(3); length.resize(3); inc.resize(3);
	start[2] = 0; length[2] = 1; inc[2] = 1;
	originalLength.resize(3);
	originalLength[2] = 1;
    }
    ArrayVolume(ndimen, length.storage());
    makeIndexingConstants();
    DebugAssert(ok(), ArrayError);
}

template<class T> Cube<T>::~Cube()
{}


// <thrown>
//   <item> ArrayConformanceError
// </thrown>
template<class T> void Cube<T>::resize(const IPosition &l, const IPosition &o)
{
    DebugAssert(ok(), ArrayError);
    if (l.nelements() != 3 || o.nelements() != 3)
	throw(ArrayConformanceError("Cube<T>::resize() - attempt to form "
				    "non-Cube"));
    Array<T>::resize(l,o);
    makeIndexingConstants();
}

template<class T> void Cube<T>::resize(const IPosition &len)
{
    DebugAssert(ok(), ArrayError);
    IPosition or(len.nelements());
    or = 0;
    resize(len,or);
}

template<class T> void Cube<T>::resize(uInt nx, uInt ny, uInt nz,
				       Int ox, Int oy, Int oz)
{
    DebugAssert(ok(), ArrayError);
    IPosition l(3), o(3);
    l(0) = nx; l(1) = ny; l(2) = nz;
    o(0) = ox; o(1) = oy; o(2) = oz;
    Cube<T>::resize(l,o);
}

// <thrown>
//   <item> ArrayNDimError
// </thrown>
template<class T> void Cube<T>::reference(Array<T> &other)
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

    Bool Conform = conform(other);
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
	Bool Conform = conform(a);
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
    Int b1, l1, s1, b2, l2, s2, b3,s3,l3;       // begin length step
    if (sliceX.all()) {
	b1 = start[0];
	l1 = length[0];
	s1 = 1;
    } else {
	b1 = sliceX.start();
	l1 = sliceX.length();
	s1 = sliceX.inc();
    }
    if (sliceY.all()) {
	b2 = start[1];
	l2 = length[1];
	s2 = 1;
    } else {
	b2 = sliceY.start();
	l2 = sliceY.length();
	s2 = sliceY.inc();
    }
    if (sliceZ.all()) {
	b3 = start[2];
	l3 = length[2];
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
    } else if ((b1+(l1-1)*s1 >start[0]+length[0] - 1) || 
	       (b2+(l2-1)*s2 >start[1]+length[1] - 1) ||
	       (b3+(l3-1)*s3 >start[2]+length[2] - 1)) {
	throw(ArrayError("Cube<T>::operator()(Slice,Slice,Slice) : "
			 "Desired slice extends beyond the end of the array"));
    } else if (b1 < start[0] || b2 < start[1] || b3 < start[2]) {
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

template<class T> void Cube<T>::makeIndexingConstants()
{
    // No lAssert since the Cube often isn't constructed yet when
    // calling this
    xyzoffset = -start[0]*inc[0] - start[1]*inc[1]*originalLength[0] - 
	start[2]*inc[2]*originalLength[0]*originalLength[1];
    xinc = inc[0];
    yinc = inc[1]*originalLength[0];
    zinc = inc[2]*originalLength[0]*originalLength[1];
}

// <thrown>
//   <item> ArrayConformanceError
// </thrown>
template<class T> Matrix<T> Cube<T>::xyPlane(Int which)
{
    DebugAssert(ok(), ArrayError);
    if ((which < start[2]) || which > (start[2] + length[2] - 1)) {
	throw(ArrayConformanceError("Cube<T>::xyPlane - "
				    "plane < start or > end"));
    }
    Cube<T> tmp((*this)(Slice(), Slice(), which));
    tmp.ndimen = 2;
    return tmp; // should match Matrix<T>(const Array<T> &)
}

template<class T> const Matrix<T> Cube<T>::xyPlane(Int which) const
{
    Cube<T> *This = (Cube<T> *)this;
    // Cast away constness, but the return type is a const Matrix<T>, so
    // this should still be safe.
    return This->xyPlane(which);
}


template<class T> IPosition Cube<T>::origin() const
{
    DebugAssert(ok(), ArrayError);
    return Array<T>::origin();
}

template<class T> IPosition Cube<T>::shape() const
{
    DebugAssert(ok(), ArrayError);
    return Array<T>::shape();
}

template<class T> IPosition Cube<T>::end() const
{
    DebugAssert(ok(), ArrayError);
    return Array<T>::end();
}

template<class T> Bool Cube<T>::ok() const
{
    return ( (ndim() == 3) ? (Array<T>::ok()) : False );
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
