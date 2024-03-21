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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#ifndef CASA_VECTOR_2_TCC
#define CASA_VECTOR_2_TCC

#include "Vector.h"
#include "ArrayError.h"
#include "Slice.h"
#include "MaskedArray.h"

#include <cassert>
#include <iterator>

namespace casacore { //#Begin casa namespace

template<typename T> Vector<T>::Vector()
  : Array<T>(IPosition(1,0))
{
  assert(ok());
}


template<typename T> Vector<T>::Vector(size_t Length)
: Array<T>(IPosition(1, Length))
{
  assert(ok());
}

template<typename T> Vector<T>::Vector(const IPosition& len)
  : Array<T>(len)
{ 
  Array<T>::checkBeforeResize(len);
}

template<typename T> Vector<T>::Vector(size_t length, const T &initialValue)
: Array<T>(IPosition(1, length), initialValue)
{
  assert(ok());
}

template<typename T> Vector<T>::Vector(const IPosition& length, const T &initialValue)
  : Array<T>(length, initialValue)
{
  Array<T>::checkBeforeResize(length);
}

template<typename T>
Vector<T>::Vector(size_t length, typename Array<T>::uninitializedType)
  : Array<T>(IPosition(1, length), Array<T>::uninitialized)
{
}

template<typename T>
Vector<T>::Vector(const IPosition& length, typename Array<T>::uninitializedType)
  : Array<T>(length, Array<T>::uninitialized)
{
  Array<T>::checkBeforeResize(length);
}

template<typename T>
Vector<T>::Vector(const std::vector<T> &other, long long nr)
: Array<T>(IPosition(1, other.size()))
{
  initVector (other, nr);
  assert(ok());
}

template<typename T>
Vector<T>::Vector(const std::vector<T> &other)
: Array<T>(IPosition(1, other.size()), const_cast<T*>(other.data()))
{
  assert(ok());
}

template<>
inline Vector<bool>::Vector(const std::vector<bool>& input)
: Array<bool>(IPosition(1, input.size()), input.begin())
{
  assert(ok());
}

template<typename T>
template<typename InputIterator>
Vector<T>::Vector(InputIterator startIter, InputIterator endIter) : Vector<T>(startIter, endIter, std::is_integral<InputIterator>())
{ }

// Constructor for Vector(nonintegral, nonintegral)
template<typename T>
template<typename InputIterator>
Vector<T>::Vector(InputIterator startIter, InputIterator endIter, std::false_type) :
  Array<T>(IPosition(1, std::distance(startIter, endIter)), startIter)
{
  assert(ok());
} 

// Constructor for Vector(integral, integral)
template<typename T>
template<typename Integral>
Vector<T>::Vector(Integral length, Integral initialValue, std::true_type) :
Array<T>(IPosition(1, length), initialValue)
{
  assert(ok());
}

template<typename T>
Vector<T>::Vector(std::initializer_list<T> list)
: Array<T>(list)
{
  assert(ok());
}

template<typename T>
Vector<T>::Vector(const Vector<T> &other)
: Array<T>(other)
{
  assert(ok());
}

template<typename T>
Vector<T>::Vector(Vector<T>&& source) noexcept
: Array<T>(std::move(source), IPosition(1, 0))
{
  assert(ok());
}

// <thrown>
//    <item> ArrayNDimError
// </thrown>
template<typename T>
Vector<T>::Vector(const Array<T> &other)
: Array<T>(other)
{
    // If not 1 dimension, adjust shape if possible.
    if (this->ndim() != 1) {
        this->checkVectorShape();
    }
    assert(ok());
}

template<typename T>
Vector<T>::Vector(const IPosition &shape, T *storage,
		  StorageInitPolicy policy)
  : Array<T>(shape, storage, policy)
{ }

template<typename T>
Vector<T>::Vector(const IPosition &shape, const T *storage)
  : Array<T>(shape, storage)
{ }

// Copy from the block. Copy the number of elements specified or
// the number of elements in the block if nr <= 0.
// <thrown>
//    <item> ArrayError
// </thrown>
template<typename T>
void Vector<T>::initVector(const std::vector<T> &other, long long nr)
{
    size_t n = nr;
    if (nr <= 0) {
      n = other.size();
    }
    if (n > other.size())
      throw(ArrayError("Vector<T>::initVector(const Block<T> &other"
        ", long long nr) - nr > other.nelements()"));
    if (this->nelements() != n) {
      this->resize(n);
    }
    for (size_t i=0; i < n; i++) {
      this->begin_p[i] = other[i];
    }
    return;
}

// <thrown>
//    <item> ArrayConformanceError
// </thrown>
template<typename T> void Vector<T>::resize(const IPosition& l, bool copyValues)
{
  assert(ok());
  if (copyValues) {
    Vector<T> oldref(*this);
    Array<T>::resize(l, false);
    size_t minNels = std::min(this->nelements(), oldref.nelements());
    move_n_with_stride(oldref.begin_p, minNels, this->begin_p,
    this->inc_p(0), oldref.inc_p(0));
  } else {
    Array<T>::resize(l, false);
  }
  assert(ok());
}

template<typename T> Vector<T>& Vector<T>::assign_conforming(const Array<T>& a)
{
  assert(ok());
  Vector<T> tmp(a);
  assign_conforming(tmp);
  return *this;
}

template<typename T> Vector<T>& Vector<T>::assign_conforming(Array<T>&& a)
{
  assert(ok());
  Vector<T> tmp(std::move(a));
  assign_conforming(tmp);
  return *this;
}

template<typename T> Vector<T>& Vector<T>::assign_conforming_implementation(const Vector<T> &, std::false_type /*movable?*/)
{
  throw std::runtime_error("assign called for which a copy is required, while element type is not copyable");
}

template<typename T> Vector<T>& Vector<T>::assign_conforming_implementation(const Vector<T> &other, std::true_type /*movable?*/)
{
    assert(ok());
    if (this != &other) {
        if (! this->copyVectorHelper (other)) {
	    // Block was empty, so allocate new block.
          // TODO think about semantics of allocator!
	    this->data_p.reset( new arrays_internal::Storage<T>(this->length_p(0)) );
	    this->begin_p = this->data_p->data();
	}
	this->setEndIter();
	copy_n_with_stride (other.begin_p, this->nels_p, this->begin_p,
		 this->inc_p(0), other.inc_p(0));
    }
    return *this;
}

template<typename T>
Vector<T>& Vector<T>::assign_conforming(Vector<T>&& source)
{
  assert(ok());
  if(this->nrefs() > 1 || source.nrefs() > 1 || this->data_p->is_shared() || source.data_p->is_shared())
    assign_conforming(source);
  else if(source.ndim() == 0)
  {
    Vector<T> empty;
    casacore::swap(empty, *this);
  }
  else
    casacore::swap(source, *this);
  return *this;
}

// <thrown>
//    <item> ArrayError
// </thrown>
template<typename T>
Vector<T> Vector<T>::operator()(const Slice &slice)
{
    assert(ok());
    long long b, l, s;       // begin length step
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

template<typename T> const Vector<T> Vector<T>::operator()
  (const Slice &slice) const
{
    return const_cast<Vector<T>*>(this)->operator() (slice);
}

template<typename T>
void Vector<T>::doNonDegenerate (const Array<T> &other,
                                 const IPosition &ignoreAxes)
{
    Array<T> tmp(*this);
    tmp.nonDegenerate (other, ignoreAxes);
    Array<T>::reference (tmp);
}

template<typename T> bool Vector<T>::ok() const
{
    return  this->ndim() == 1  &&  Array<T>::ok();
}

//# Declare extern templates for often used types.
extern template class Vector<bool>;
extern template class Vector<char>;
extern template class Vector<short>;
extern template class Vector<unsigned short>;
extern template class Vector<int>;
extern template class Vector<unsigned int>;
extern template class Vector<long long>;
extern template class Vector<float>;
extern template class Vector<double>;

} //#End casa namespace

#endif
