//# Vector.cc: A 1-D Specialization of the Array Class
//# Copyright (C) 1993,1994,1995,1996
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

#include <aips/Arrays/Vector.h>
#include <aips/Arrays/ArrayError.h>
#include <aips/Utilities/Assert.h>
#include <aips/Mathematics/Math.h>
#include <aips/Utilities/Copy.h>
#include <iostream.h>

//# Implement rtti functions.
rtti_imp_mbrf_a1(Vector);

template<class T> Vector<T>::Vector()
{
    DebugAssert(ok(), ArrayError);
}


template<class T> Vector<T>::Vector(uInt Length, Int Origin)
: Array<T>(IPosition(1, Length), IPosition(1, Origin))
{
    DebugAssert(ok(), ArrayError);
}

template<class T> Vector<T>::Vector(uInt Length)
: Array<T>(IPosition(1, Length))
{
    DebugAssert(ok(), ArrayError);
}


template<class T> Vector<T>::Vector(const Block<T> &other, Int nr)
: Array<T>(IPosition(1, other.nelements()))
{
    initVector (other, nr);
    DebugAssert(ok(), ArrayError);
}

template<class T> Vector<T>::Vector(const Block<T> &other)
: Array<T>(IPosition(1, other.nelements()))
{
    initVector (other, 0);
    DebugAssert(ok(), ArrayError);
}

// Copy from the block. Copy the number of elements specified or
// the number of elements in the block if nr <= 0.
// <thrown>
//    <item> ArrayError
// </thrown>
template<class T> void Vector<T>::initVector(const Block<T> &other, Int nr)
{
    uInt n = nr;
    if (nr <= 0) 
	n = other.nelements();
    if (n > other.nelements())
	throw(ArrayError("Vector<T>::initVector(const Block<T> &other"
				   ", Int nr) - nr > other.nelements()"));
    if (this->nelements() != n) {
	this->resize(n);
    }
    for (Int i=0; i < n; i++)
	(*data)[i] = other[i];

    return;
}

template<class T> Vector<T>::Vector(const Vector<T> &other)
: Array<T>(other)
{
    DebugAssert(ok(), ArrayError);
}

// <thrown>
//    <item> ArrayNDimError
// </thrown>
template<class T> Vector<T>::Vector(const Array<T> &other)
: Array<T>(other)
{
    if (ndim() != 1)
	throw(ArrayNDimError(1, other.ndim(), "Vector<T>::Vector"
			     " (const Array<T> &) : ndim != 1"));
    DebugAssert(ok(), ArrayError);
}

template<class T>
Vector<T>::Vector(const IPosition &shape, T *storage, 
		  StorageInitPolicy policy)
  : Array<T>(shape, storage, policy)
{
    AlwaysAssert(shape.nelements() == 1, ArrayError);
}

template<class T>
Vector<T>::Vector(const IPosition &shape, const T *storage)
  : Array<T>(shape, storage)
{
    AlwaysAssert(shape.nelements() == 1, ArrayError);
}

template<class T> Vector<T>::~Vector()
{
    // Nothing
}


// <thrown>
//    <item> ArrayConformanceError
// </thrown>
template<class T> void Vector<T>::resize(const IPosition &l, const IPosition &o,
					 Bool copyValues)
{
    DebugAssert(ok(), ArrayError);
    if (l.nelements() != 1 || o.nelements() != 1)
	throw(ArrayConformanceError("Vector<T>::resize() - attempt to form "
				    "non-vector"));
    if (copyValues) {
        Vector<T> oldref(*this);
	Array<T>::resize(l,o);
	uInt minNels = min(this->nelements(), oldref.nelements());
	objcopy(begin, oldref.begin, minNels, uInt(inc[0]), uInt(oldref.inc[0]));
    } else {
	Array<T>::resize(l,o);
    }
}

template<class T> void Vector<T>::resize(const IPosition &l, const IPosition &o)
{
    resize(l,o,False);
}

template<class T> void Vector<T>::resize(const IPosition &l)
{
    resize(l,False);
}


template<class T> void Vector<T>::resize(const IPosition &len, Bool copyValues)
{
    DebugAssert(ok(), ArrayError);
    IPosition or(len.nelements());
    or = 0;
    resize(len,or, copyValues);
}

template<class T> void Vector<T>::resize(uInt len, Int orign, Bool copyValues)
{
    DebugAssert(ok(), ArrayError);
    IPosition l(1), o(1);
    l(0) = len;
    o(0) = orign;
    Vector<T>::resize(l,o, copyValues);
}

// <thrown>
//    <item> ArrayNDimError
// </thrown>
template<class T> void Vector<T>::reference(Array<T> &other)
{
    DebugAssert(ok(), ArrayError);
    if (other.ndim() != 1)
	throw(ArrayNDimError(1, other.ndim(), "Vector<T>::reference()"
			     " - attempt to reference non-vector"));
    Array<T>::reference(other);
}

template<class T> Vector<T> &Vector<T>::operator=(const Vector<T> &other)
{
    DebugAssert(ok(), ArrayError);

    if (this == &other)
        return *this;

    Bool Conform = conform(other);
    if (Conform == False && length[0] != 0)
	validateConformance(other);  // We can't overwrite, so throw exception


    Int i;
    if (Conform == True) { // copy in place
	Int indexOffset = other.start[0] - start[0];
	for(i=start[0]; i < start[0] + length[0]; i++)
	    (*this)(i) = other(i + indexOffset);
    } else {
        data = new Block<T>(other.length[0]);
        begin = data->storage();
        start = other.start;
        length = other.length;
	nels = other.nels;
	originalLength = length;
        inc[0] = 1; // flatten
	Int indexOffset = other.start[0] - start[0];
	for(i=start[0]; i < start[0] + length[0]; i++)
	    (*this)(i) = other(i + indexOffset);
    }
    return *this;
}

// Copy a vector to a block. 
template<class T> void Vector<T>::toBlock(Block<T> & other) const
{
  DebugAssert(ok(), ArrayError);
  uInt vec_length = nelements();
// Make sure the block has enough space, but there is no need to copy elements
  other.resize(vec_length, True, False);
// Do not assume that the input Vector starts at zero.
  Int j;
  (*this).origin(j);
// Now copy the data across
  for (uInt i = 0; i < vec_length; )
    other[i++] = (*this)(j++);
}

template<class T> Array<T> &Vector<T>::operator=(const Array<T> &a)
{
    DebugAssert(ok(), ArrayError);
    Vector<T> tmp(a);
    (*this) = tmp;
    return *this;
}

// <thrown>
//    <item> ArrayError
// </thrown>
template<class T> Vector<T> Vector<T>::operator()(const Slice &slice)
{
    DebugAssert(ok(), ArrayError);
    Int b, l, s;       // begin length step
    if (slice.all()) {
	b = start[0];
	l = length[0];
	s = 1;
    } else {
	b = slice.start();
	l = slice.length();
	s = slice.inc();
    }

    // Check that the selected slice is valid
    if (s < 1) {
	throw(ArrayError("Vector<T>::operator()(Slice) : step < 1"));
    } else if (l < 0) {
	throw(ArrayError("Vector<T>::operator()(Slice) : length < 0"));
    } else if (b+(l-1)*s > start[0]+length[0] - 1) {
	throw(ArrayError("Vector<T>::operator()(Slice) : Desired slice extends"
			 " beyond the end of the array"));
    } else if (b < start[0]) {
	throw(ArrayError("Vector<T>::operator()(Slice) : start of slice before "
			 "beginning of vector"));
    }

    // Create the slice. This could also be done with the Array<T>::operator()
    // slice functions, however it's simple for vectors, and this will be
    // more efficient.

    // Create the vector that will be the slice into this
    Vector<T> vp(*this);

    // Increment vp's begin so that it is at the selected position
    vp.begin += (b - vp.start[0])*vp.inc[0];

    vp.start[0] = 0;    // Slices always zero based
    vp.inc[0] *= s;

    vp.length[0] = l;
    vp.nels = l;

    return vp;
}

template<class T> IPosition Vector<T>::origin() const
{
    DebugAssert(ok(), ArrayError);
    return Array<T>::origin();
}

template<class T> IPosition Vector<T>::shape() const
{
    DebugAssert(ok(), ArrayError);
    return Array<T>::shape();
}

template<class T> IPosition Vector<T>::end() const
{
    DebugAssert(ok(), ArrayError);
    return Array<T>::end();
}

template<class T>
void Vector<T>::takeStorage(const IPosition &shape, T *storage,
		     StorageInitPolicy policy)
{
    AlwaysAssert(shape.nelements() == 1, ArrayError);
    Array<T>::takeStorage(shape, storage, policy);
}

template<class T>
void Vector<T>::takeStorage(const IPosition &shape, const T *storage)
{
    AlwaysAssert(shape.nelements() == 1, ArrayError);
    Array<T>::takeStorage(shape, storage);
}

template<class T> Bool Vector<T>::ok() const
{
    return ( (ndim() == 1) ? (Array<T>::ok()) : False );
}

