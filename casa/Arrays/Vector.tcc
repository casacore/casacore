//# Vector.cc: A 1-D Specialization of the Array Class
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

#ifndef CASA_VECTOR_TCC
#define CASA_VECTOR_TCC

#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayError.h>
#include <casacore/casa/Arrays/Slice.h>
#include <casacore/casa/Arrays/MaskedArray.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Utilities/Copy.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>

namespace casacore { //#Begin casa namespace

template<class T> Vector<T>::Vector()
  : Array<T>(IPosition(1,0))
{
    DebugAssert(ok(), ArrayError);
}


template<class T> Vector<T>::Vector(size_t Length)
: Array<T>(IPosition(1, Length))
{
    DebugAssert(ok(), ArrayError);
}

template<class T> Vector<T>::Vector(const IPosition& len)
  : Array<T>(len)
{
    if (len.nelements() != 1) this->throwNdimVector();
}

template<class T> Vector<T>::Vector(size_t Length, const T &initialValue)
: Array<T>(IPosition(1, Length), initialValue)
{
    DebugAssert(ok(), ArrayError);
}

template<class T> Vector<T>::Vector(const IPosition& len, const T &initialValue)
  : Array<T>(len, initialValue)
{
    if (len.nelements() != 1) this->throwNdimVector();
}

template<class T> Vector<T>::Vector(const Block<T> &other, Int64 nr)
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
template<class T> void Vector<T>::initVector(const Block<T> &other, Int64 nr)
{
    size_t n = nr;
    if (nr <= 0) {
	n = other.nelements();
    }
    if (n > other.nelements())
	throw(ArrayError("Vector<T>::initVector(const Block<T> &other"
				   ", Int64 nr) - nr > other.nelements()"));
    if (this->nelements() != n) {
	this->resize(n);
    }
    for (size_t i=0; i < n; i++) {
	this->begin_p[i] = other[i];
    }
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
    // If not 1 dimension, adjust shape if possible.
    if (this->ndim() != 1) {
        this->checkVectorShape();
    }
    DebugAssert(ok(), ArrayError);
}

template<class T>
Vector<T>::Vector(const IPosition &shape, T *storage, 
		  StorageInitPolicy policy)
  : Array<T>(shape, storage, policy)
{
    if (shape.nelements() != 1) this->throwNdimVector();
}

template<class T>
Vector<T>::Vector(const IPosition &shape, const T *storage)
  : Array<T>(shape, storage)
{
    if (shape.nelements() != 1) this->throwNdimVector();
}

template<class T> Vector<T>::~Vector()
{
    // Nothing
}


template<class T> void Vector<T>::resize()
{
    resize (IPosition(1,0), False);
}

// <thrown>
//    <item> ArrayConformanceError
// </thrown>
template<class T> void Vector<T>::resize(const IPosition &l, Bool copyValues)
{
    DebugAssert(ok(), ArrayError);
    if (l.nelements() != 1) this->throwNdimVector();
    if (copyValues) {
        Vector<T> oldref(*this);
	Array<T>::resize(l);
	size_t minNels = std::min(this->nelements(), oldref.nelements());
	objcopy(this->begin_p, oldref.begin_p, minNels,
		this->inc_p(0), oldref.inc_p(0));
    } else {
	Array<T>::resize(l);
    }
    DebugAssert(ok(), ArrayError);
}


// <thrown>
//    <item> ArrayError
// </thrown>
template<class T> void Vector<T>::assign (const Array<T>& other)
{
    DebugAssert(ok(), ArrayError);
    if (other.ndim() != 1) this->throwNdimVector();
    Array<T>::assign (other);
}

// <thrown>
//    <item> ArrayError
// </thrown>
template<class T> void Vector<T>::reference(const Array<T> &other)
{
    DebugAssert(ok(), ArrayError);
    if (other.ndim() != 1) this->throwNdimVector();
    Array<T>::reference(other);
}

template<class T> Vector<T> &Vector<T>::operator=(const Vector<T> &other)
{
    DebugAssert(ok(), ArrayError);
    if (this != &other) {
        if (! this->copyVectorHelper (other)) {
	    // Block was empty, so allocate new block.
	    this->data_p  = new Block<T> (this->length_p(0));
	    this->begin_p = this->data_p->storage();
	}
	this->setEndIter();
	objcopy (this->begin_p, other.begin_p, this->nels_p,
		 this->inc_p(0), other.inc_p(0));
    }
    return *this;
}

// Copy a vector to a block. 
template<class T> void Vector<T>::toBlock(Block<T> & other) const
{
    DebugAssert(ok(), ArrayError);
    size_t vec_length = this->nelements();
    // Make sure the block has enough space, but there is no need to copy elements
    other.resize(vec_length, True, False);
    objcopy(other.storage(), this->begin_p, this->nels_p, 1U, this->inc_p(0));
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
    Int64 b, l, s;       // begin length step
    if (slice.all()) {
	b = 0;
	l = this->length_p(0);
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
    } else if (b+(l-1)*s >= this->length_p(0)) {
	throw(ArrayError("Vector<T>::operator()(Slice) : Desired slice extends"
			 " beyond the end of the array"));
    } else if (b < 0) {
	throw(ArrayError("Vector<T>::operator()(Slice) : start of slice before "
			 "beginning of vector"));
    }

    // Create the slice. This could also be done with the Array<T>::operator()
    // slice functions, however it's simple for vectors, and this will be
    // more efficient.

    // Create the vector that will be the slice into this
    Vector<T> vp(*this);

    // Increment vp's begin so that it is at the selected position
    vp.begin_p += b*this->steps()[0];
    vp.inc_p(0) *= s;
    vp.length_p(0) = l;
    vp.nels_p = l;
    vp.contiguous_p = vp.isStorageContiguous();
    vp.makeSteps();

    return vp;
}

template<class T> const Vector<T> Vector<T>::operator()
  (const Slice &slice) const
{
    return const_cast<Vector<T>*>(this)->operator() (slice);
}

template<class T>
void Vector<T>::doNonDegenerate (const Array<T> &other,
                                 const IPosition &ignoreAxes)
{
    Array<T> tmp(*this);
    tmp.nonDegenerate (other, ignoreAxes);
    if (tmp.ndim() != 1) this->throwNdimVector();
    reference (tmp);
}

template<class T>
void Vector<T>::takeStorage(const IPosition &shape, T *storage,
			    StorageInitPolicy policy)
{
    if (shape.nelements() != 1) this->throwNdimVector();
    Array<T>::takeStorage(shape, storage, policy);
}

template<class T>
void Vector<T>::takeStorage(const IPosition &shape, const T *storage)
{
    if (shape.nelements() != 1) this->throwNdimVector();
    Array<T>::takeStorage(shape, storage);
}

template<class T> Bool Vector<T>::ok() const
{
    return  this->ndim() == 1  &&  Array<T>::ok();
}

} //#End casa namespace

#endif
