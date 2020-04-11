//# Array.cc: A templated N-D Array class with zero origin
//# Copyright (C) 1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2015
//# Associated Universities, Inc. Washington DC, USA.
//# National Astronomical Observatory of Japan
//# 2-21-1, Osawa, Mitaka, Tokyo, 181-8588, Japan.
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
//# $Id: Array.tcc 21561 2015-02-16 06:57:35Z gervandiepen $

#ifndef CASA_ARRAY_2_TCC
#define CASA_ARRAY_2_TCC

#include "Array.h"
#include "ArrayError.h"
#include "ArrayIter.h"
#include "ArrayPosIter.h"
#include "Copy.h"
#include "MaskedArray.h"
#include "Slicer.h"

#include <algorithm>
#include <cassert>

namespace casacore {//#Begin casa namespace

template<typename T, typename Alloc> Array<T, Alloc>::Array(const Alloc& allocator)
: data_p(new Storage<T, Alloc>(0, allocator)),
  begin_p(nullptr),
  end_p(nullptr)
{
  assert(ok());
}

// <thrown>
//   <item> ArrayShapeError
// </thrown>
template<class T, typename Alloc>
Array<T, Alloc>::Array(const IPosition &shape,
        const Alloc& allocator)
: ArrayBase(shape),
  data_p(new Storage<T, Alloc>(nelements(), allocator)),
  begin_p(data_p->data())
{
  setEndIter();
  assert(ok());
}

// <thrown>
//   <item> ArrayShapeError
// </thrown>
template<typename T, typename Alloc> Array<T, Alloc>::Array(const IPosition &shape,
  const T &initialValue, const Alloc& allocator)
: ArrayBase(shape),
  data_p(new Storage<T, Alloc>(nelements(), initialValue, allocator)),
  begin_p(data_p->data())
{
  setEndIter();
  assert(ok());
}

template<typename T, typename Alloc> Array<T, Alloc>::Array(std::initializer_list<T> list, const Alloc& allocator)
: ArrayBase (IPosition(1, list.size())),
  data_p(new Storage<T, Alloc>(list.begin(), list.end(), allocator)),
  begin_p(data_p->data())
{
  setEndIter();
  assert(ok());
}

template<typename T, typename Alloc> Array<T, Alloc>::Array(const Array<T, Alloc> &other)
: ArrayBase (other),
  data_p(other.data_p),
  begin_p   (other.begin_p),
  end_p     (other.end_p)
{
  assert(ok());
}

template<typename T, typename Alloc> Array<T, Alloc>::Array(Array<T, Alloc>&& source) noexcept
: ArrayBase (std::move(source)),
  data_p(source.data_p),
  begin_p(source.begin_p),
  end_p(source.end_p)
{
  // We can't free the storage of the source object yet, because this would require either
  // to allow data_p be nullptr, which requires check everywhere, or would require allocating
  // an empty storage, which would forbid noexcept.
  
  // Empty source
  source.begin_p = nullptr;
  source.end_p = nullptr;
  
  assert(ok());
}

template<typename T, typename Alloc> Array<T, Alloc>::Array(Array<T, Alloc>&& source, const IPosition& shapeForSource) noexcept
: ArrayBase (std::move(source), shapeForSource),
  data_p(source.data_p),
  begin_p(source.begin_p),
  end_p(source.end_p)
{
  source.begin_p = nullptr;
  source.end_p = nullptr;
  assert(ok());
}

template<class T, typename Alloc>
Array<T, Alloc>::Array(const IPosition &shape, T *storage,
                StorageInitPolicy policy, const Alloc& allocator)
: ArrayBase(shape),
  data_p(),
  begin_p(nullptr),
  end_p(nullptr)
{
  takeStorage(shape, storage, policy, allocator);
  assert(ok());
}

template<class T, typename Alloc>
Array<T, Alloc>::Array(const IPosition &shape, const T *storage)
: ArrayBase(shape),
  data_p(),
  begin_p(nullptr),
  end_p(nullptr)
{
  takeStorage(shape, storage, static_cast<Alloc&>(*data_p));
  assert(ok());
}

template<typename T, typename Alloc>
template<typename InputIterator>
Array<T, Alloc>::Array(const IPosition &shape, InputIterator startIter, const Alloc& allocator)
: Array<T, Alloc>(shape, startIter, allocator, std::is_integral<InputIterator>())
{ }

template<typename T, typename Alloc>
template<typename InputIterator>
Array<T, Alloc>::Array(const IPosition &shape, InputIterator startIter, const Alloc& allocator, std::false_type)
: ArrayBase(shape),
  data_p(new Storage<T, Alloc>(startIter, std::next(startIter, nelements()), allocator)),
  begin_p(data_p->data())
{
  setEndIter();
  assert(ok());
}

template<typename T, typename Alloc>
template<typename Integral>
Array<T, Alloc>::Array(const IPosition &shape, Integral initialValue, const Alloc& allocator, std::true_type)
: ArrayBase(shape),
  data_p(new Storage<T, Alloc>(nelements(), initialValue, allocator)),
  begin_p(data_p->data())
{
  setEndIter();
  assert(ok());
}


template<typename T, typename Alloc> Array<T, Alloc>::~Array() noexcept
{ }

template<class T, typename Alloc>
std::unique_ptr<ArrayBase> Array<T, Alloc>::makeArray() const
{
  return std::unique_ptr<ArrayBase>(new Array<T, Alloc>(static_cast<const Alloc&>(*data_p)));
}

// It is better for a move assignment to be noexcept, but that would allow
// assigning Matrix to Vector (etc) without being able to throw. Hence, I
// think it is not possible to do this without changing such semantics.
template<class T, typename Alloc>
Array<T, Alloc>& Array<T, Alloc>::operator= (Array<T, Alloc>&& source)
{
  if(nrefs() > 1 || source.nrefs() > 1 || data_p->is_shared() || source.data_p->is_shared())
  {
    // We can't move: this or the source is a shared array, so we can't
    // just replace the storage. Non-moveable types will cause
    // this to throw :-(. TODO should be solved.
    assign_conforming(source);
  }
  else {
    // There's no good reason to require conformance here, but some of the software seems
    // to assume that assignment always checks for the shape to fit. :-(
    if (!conform(source)  &&  nelements() != 0) {
      validateConformance(source);
    }
    else {
      if(source.fixedDimensionality() != 0 && ndim() != source.ndim())
      {
        // We can't directly swap the two, because the lhs doesn't match in rhs requirements
        resize(IPosition(source.fixedDimensionality(), 0));
      }
      swap(source);
    }
  }
  return *this;
}

template<class T, typename Alloc>
void Array<T, Alloc>::swap(Array<T, Alloc>& other)
{
  checkBeforeResize(other.shape());
  other.checkBeforeResize(shape());

  ArrayBase::swap(other);
  
  // Take storage
  std::swap(begin_p, other.begin_p);
  std::swap(end_p, other.end_p);
  std::swap(data_p, other.data_p);
}

template<class T, typename Alloc>
void Array<T, Alloc>::assign (const Array<T, Alloc>& other)
{
  assert(ok());
  if (! shape().isEqual (other.shape())) {
    checkBeforeResize(other.shape());
    resize (other.shape());
  }
  assign_conforming (other);
}

template<class T, typename Alloc> void Array<T, Alloc>::assignBase (const ArrayBase& other, bool checkType)
{
  assert(ok());
  // Checking the type can be expensive, so only do if needed or in debug mode.
  if (checkType  /*||  aips_debug*/) {
    const Array<T, Alloc>* pa = dynamic_cast<const Array<T, Alloc>*>(&other);
    if (pa == nullptr) {
      throw ArrayError("assign(ArrayBase&) has incorrect template type");
    }
  }
  assign (static_cast<const Array<T, Alloc>&>(other));
}

template<class T, typename Alloc> void Array<T, Alloc>::reference(const Array<T, Alloc> &other)
{
  assert(ok());
  
  // It is allowed to reference from a higher dimensional Array to a lower dimensional one,
  // e.g. reference an Array from a Matrix.
  if(other.ndim() < fixedDimensionality())
  {
    IPosition newShape(fixedDimensionality());
    for(size_t i=0; i!=other.ndim(); ++i)
      newShape[i] = other.shape()[i];
    for(size_t i=other.ndim(); i!=fixedDimensionality(); ++i)
      newShape[i] = 1;
    Array<T, Alloc> tmp(*other.data_p);
    tmp.reference(other);
    other.baseReform(tmp, newShape);
    reference( tmp );
  }
  else {
    // First copy data, then meta data.
    // This is better in case of multi-threading because it makes it possible
    // to test the size and be sure that the data is there.
    checkBeforeResize(other.shape());
    data_p  = other.data_p;
    begin_p = other.begin_p;
    end_p   = other.end_p;
    ArrayBase::assign (other);
  }
}

template<class T, typename Alloc> void Array<T, Alloc>::copyToContiguousStorage(T *storage, Array<T, Alloc> const& src, std::true_type)
{
  if (src.contiguousStorage()) {
    std::copy_n(src.begin_p, src.nels_p, storage);
  } else if (src.ndim() == 1) {
    copy_n_with_stride(src.begin_p, src.length_p(0), storage, 1U, src.inc_p(0));
  } else if (src.length_p(0) == 1  &&  src.ndim() == 2) {
    // Special case which can be quite common (e.g. row in a matrix).
    copy_n_with_stride(src.begin_p, src.length_p(1), storage, 1U,
      src.originalLength_p(0) * src.inc_p(1));
  } else if (src.length_p(0) <= 25) {
    // If not many elements on a line, it's better to use this loop.
    T* ptr = storage;
    const_iterator iterend = src.end();
    for (const_iterator iter = src.begin(); iter != iterend; ++iter) {
      *ptr++ = *iter;
    }
  } else {
    // Step through Vector by Vector
    // The output is guaranteed to have all incs set to 1
    ArrayPositionIterator ai(src.shape(), 1);
    IPosition index(src.ndim());
    size_t count = 0;
    size_t const size = src.length_p(0);
    while (!ai.pastEnd()) {
      index = ai.pos();
      size_t offset = ArrayIndexOffset(src.ndim(),
        src.originalLength_p.storage(), src.inc_p.storage(),
        index);
      copy_n_with_stride(src.begin_p + offset, size, storage + count * size,
        1U, src.inc_p(0));
      ai.next();
      count++;
    }
  }
}

template<typename T, typename Alloc>
Array<T, Alloc> Array<T, Alloc>::copy(const Alloc& allocator) const
{
    assert(ok());

    Array<T, Alloc> vp(shape(), allocator);
    if (ndim() != 0)
        copyToContiguousStorage(vp.begin_p, *this);
    
    return vp;
}

template<typename T, typename Alloc>
Array<T, Alloc>& Array<T, Alloc>::assign_conforming_implementation(const Array<T, Alloc>& other, std::true_type)
{
  assert(ok());

  if (this == &other) {
    return *this;
  }
  bool Conform = conform(other);
  if (!Conform  &&  nelements() != 0) {
    validateConformance(other);  // We can't overwrite, so throw exception
  }
  size_t offset, offset2;
  IPosition index(other.ndim());

  if (Conform == true) { // Copy in place
    if (ndim() == 0) {
	    return *this;
    } else if (contiguousStorage() && other.contiguousStorage()) {
      std::copy_n(other.begin_p, nels_p, begin_p);
    } else if (ndim() == 1) {
      copy_n_with_stride (other.begin_p, length_p(0), begin_p, inc_p(0), other.inc_p(0));
    } else if (length_p(0) == 1  &&  ndim() == 2) {
      // Special case which can be quite common (e.g. row in a matrix).
      copy_n_with_stride(other.begin_p, length_p(1), begin_p,
        originalLength_p(0)*inc_p(1),
        other.originalLength_p(0)*other.inc_p(1));
    } else if (length_p(0) <= 25) {
      // If not many elements on a line, it's better to use this loop.
      const_iterator from(other.begin());
      iterator iterend=end();
      for (iterator iter=begin(); iter!=iterend; ++iter)
      {
        *iter = *from;
        ++from;
      }
    } else {
      ArrayPositionIterator ai(other.shape(), 1);
      // Step through Vector by Vector
      while (! ai.pastEnd()) {
        index = ai.pos();
        offset = ArrayIndexOffset(ndim(), originalLength_p.storage(),
          inc_p.storage(), index);
        offset2 = ArrayIndexOffset(other.ndim(),
        other.originalLength_p.storage(),
        other.inc_p.storage(), index);
        copy_n_with_stride(other.begin_p+offset2, length_p(0),
          begin_p+offset, inc_p(0), other.inc_p(0));
        ai.next();
      }
    }
  } else {
    // Array was empty; make a new copy and reference it.
    Array<T, Alloc> tmp (other.copy(static_cast<Alloc>(*data_p)));
    reference (tmp);
  }
  return *this;
}

template<typename T, typename Alloc>
Array<T, Alloc> &Array<T, Alloc>::operator=(const T &val)
{
    assert(ok());

    set (val);
    return *this;
}

template<typename T, typename Alloc>
Array<T, Alloc>& Array<T, Alloc>::assign_conforming(const MaskedArray<T> &marray)
{
    assert(ok());

    if (!conform(marray)) {
        throw(ArrayConformanceError(
            "Array<T> & Array<T, Alloc>::assign_conforming (const MaskedArray<T> &marray)"
            "- Conformance error."));
    }

    bool deleteThis;
    T *thisStorage = getStorage(deleteThis);
    T *thisS = thisStorage;

    bool deleteArr;
    const T *arrStorage = marray.getArrayStorage(deleteArr);
    const T *arrS = arrStorage;

    bool deleteMask;
    const LogicalArrayElem *maskStorage
        = marray.getMaskStorage(deleteMask);
    const LogicalArrayElem *maskS = maskStorage;

    size_t ntotal = nelements();
    while (ntotal--) {
        if (*maskS) {
            *thisS = *arrS;
        }
        thisS++;
        maskS++;
        arrS++;
    }

    putStorage(thisStorage, deleteThis);
    marray.freeArrayStorage(arrStorage, deleteArr);
    marray.freeMaskStorage(maskStorage, deleteMask);

    return *this;
}


template<class T, typename Alloc> void Array<T, Alloc>::set(const T &Value)
{
  assert(ok());

  // Ultimately we should go to RawFillAll functions
  // RawFillAll(ndim(), begin_p, inc_p.storage(), length_p.storage(), Value);
  // Step through Vector by Vector
  size_t offset;
  if (ndim() == 0) {
      return;
  } else if (contiguousStorage()) {
    std::fill_n(begin_p, nels_p, Value);
  } else if (ndim() == 1) {
    fill_n_with_stride (begin_p, length_p(0), Value, inc_p(0));
  } else if (length_p(0) == 1  &&  ndim() == 2) {
    // Special case which can be quite common (e.g. row in a matrix).
    fill_n_with_stride (begin_p, length_p(1), Value,
      originalLength_p(0)*inc_p(1));
  } else if (length_p(0) <= 25) {
      // If not many elements on a line, it's better to use this loop.
      iterator iterend=end();
      for (iterator iter=begin(); iter!=iterend; ++iter) {
    *iter = Value;
    }
  } else {
    // Step through Vector by Vector
    ArrayPositionIterator ai(shape(), 1);
    IPosition index(ndim());
    while (! ai.pastEnd())
    {
      index = ai.pos();
      offset = ArrayIndexOffset(ndim(), originalLength_p.storage(),
              inc_p.storage(), index);
      fill_n_with_stride(begin_p+offset, length_p(0), Value, inc_p(0));
      ai.next();
    }
  }
}

template<class T, typename Alloc>
template<typename Callable>
void Array<T, Alloc>::apply(Callable function)
{
    assert(ok());

    if (nelements() == 0) {
        return; // short-circuit
    }

    if (contiguousStorage()) {
	for (size_t i=0; i<nels_p; i++) {
	    begin_p[i] = function(begin_p[i]);
	}
    } else {
	// Step through Vector by Vector
	ArrayPositionIterator ai(shape(), 1);
	IPosition index(ndim());

	size_t len  = length_p(0);
	size_t incr = inc_p(0);
	size_t offset;

	while (! ai.pastEnd()) {
	    index = ai.pos();
	    offset = ArrayIndexOffset(ndim(), originalLength_p.storage(),
				      inc_p.storage(), index);
	    for (size_t i=0; i < len; i++) {
		begin_p[offset+i*incr] = function(begin_p[offset+i*incr]);
	    }
	    ai.next();
	}
    }
}

template<class T, typename Alloc> void Array<T, Alloc>::unique()
{
  assert(ok());

  // short circuit when we are unique and flat
  if (contiguousStorage()  &&  nrefs() == 1) {
    return;
  }
  // OK, we know we are going to need to copy.
  Array<T, Alloc> tmp(copy(static_cast<Alloc&>(*data_p)));
  reference (tmp);
}

// <thrown>
//   <item> ArrayConformanceError
// </thrown>
template<typename T, typename Alloc>
Array<T, Alloc> Array<T, Alloc>::reform(const IPosition& len) const
{
  assert(ok());
  // Check if reform is possible and needed.
  // If not needed, simply return a copy.
  Array<T, Alloc> tmp(*this);
  baseReform (tmp, len);
  tmp.setEndIter();
  return tmp;
}

template <typename T, typename Alloc>
bool Array<T, Alloc>::adjustLastAxis (const IPosition& newShape,
  size_t resizePercentage, bool resizeIfNeeded)
{
    assert(ok());
    
    IPosition currentShape = shape();
    if (newShape.size() == currentShape.size()){ // Let base method handle attempt dimensionality changes
	for (size_t i = 0; i < newShape.size() - 1; i++){
	    if (currentShape (i) != newShape (i)){
        std::string message = "Array<T, Alloc>::extend - New shape can only change last dimension:"
          " current=" + currentShape.toString() + ", new=" + newShape.toString();
        throw ArrayConformanceError (message);
	    }
	}
    }
        
    long long originalElements = data_p->size();

    bool resetEnd = ArrayBase::reformOrResize (newShape, resizeIfNeeded, data_p.use_count(), data_p->size(),
					       true, resizePercentage);

    if (resetEnd){
	setEndIter();
    }

    return originalElements != (long long) data_p->size();
}


template<class T, typename Alloc>
bool
Array<T, Alloc>::reformOrResize (const IPosition & newShape,
                          size_t resizePercentage,
                          bool resizeIfNeeded)
{
    assert(ok());
    checkBeforeResize(newShape);

    long long originalElements = data_p->size();

    bool resetEnd = ArrayBase::reformOrResize (newShape, resizeIfNeeded, data_p.use_count(), data_p->size(),
					       false, resizePercentage);

    if (resetEnd){
	setEndIter();
    }

    return originalElements != (long long) data_p->size();
}

template<class T, typename Alloc>
inline size_t
Array<T, Alloc>::capacity () const
{
    return data_p->size(); // returns the number of elements allocated.
}

template<class T, typename Alloc>
Array<T, Alloc> Array<T, Alloc>::nonDegenerate (size_t startingAxis, bool throwIfError) const
{
    Array<T, Alloc> tmp(static_cast<Alloc&>(*data_p));
    assert(ok());
    tmp.nonDegenerate (*this, startingAxis, throwIfError);
    return tmp;
}

template<class T, typename Alloc>
void Array<T, Alloc>::nonDegenerate (const Array<T, Alloc> &other, size_t startingAxis,
			      bool throwIfError)
{
  if (startingAxis < other.ndim()) {
    IPosition ignoreAxes(startingAxis);
    for (size_t i=0; i<startingAxis; i++) {
      ignoreAxes(i) = i;
    }
    nonDegenerate (other, ignoreAxes);
  } else {
    if (throwIfError && startingAxis >= other.ndim()) throw ArrayError();
    reference (other);
  }
}

template<class T, typename Alloc>
Array<T, Alloc> Array<T, Alloc>::nonDegenerate (const IPosition &ignoreAxes) const
{
    Array<T, Alloc> tmp(static_cast<Alloc&>(*data_p));
    assert(ok());
    tmp.nonDegenerate(*this, ignoreAxes);
    return tmp;
}

template<class T, typename Alloc>
void Array<T, Alloc>::removeDegenerate (size_t startingAxis, bool throwIfError)
{
    Array<T, Alloc> tmp(static_cast<Alloc&>(*data_p));
    assert(ok());
    tmp.nonDegenerate (*this, startingAxis, throwIfError);
    reference (tmp);
}

template<class T, typename Alloc>
void Array<T, Alloc>::removeDegenerate (const IPosition &ignoreAxes)
{
    Array<T, Alloc> tmp(static_cast<Alloc&>(*data_p));
    assert(ok());
    tmp.nonDegenerate(*this, ignoreAxes);
    reference (tmp);
}

template<class T, typename Alloc>
void Array<T, Alloc>::doNonDegenerate (const Array<T, Alloc> &other,
                                const IPosition &ignoreAxes)
{
    assert(ok());
    baseNonDegenerate (other, ignoreAxes);
    begin_p = other.begin_p;
    data_p  = other.data_p;
    setEndIter();
}

template<class T, typename Alloc>
const Array<T, Alloc> Array<T, Alloc>::addDegenerate(size_t numAxes) const
{
    Array<T, Alloc> * This = const_cast<Array<T, Alloc>*>(this);
    const Array<T, Alloc> tmp(This->addDegenerate(numAxes));
    return tmp;
}

template<class T, typename Alloc>
Array<T, Alloc> Array<T, Alloc>::addDegenerate(size_t numAxes)
{
    assert(ok());
    Array<T, Alloc> tmp(*this);
    if (numAxes > 0) {
        baseAddDegenerate (tmp, numAxes);
	tmp.setEndIter();
    }
    return tmp;
}


template<class T, typename Alloc> bool Array<T, Alloc>::conform(const MaskedArray<T> &other) const
{
    return conform (other.getArray());
}

// <thrown>
//   <item> ArrayConformanceError
// </thrown>
template<class T, typename Alloc> void Array<T, Alloc>::resize()
{
  IPosition emptyShape(fixedDimensionality(), 0);
  resize (emptyShape);
}

template<class T, typename Alloc> void Array<T, Alloc>::resize(const IPosition& len, bool copyValues)
{
  assert(ok());
  // Maybe we don't need to resize; let's see if we can short circuit
  if (len.isEqual (shape())) {
    return;
  }
  // OK we differ, so we really have to resize ourselves.
  Array<T, Alloc> tmp(len, static_cast<Alloc&>(*data_p));
  // Copy the contents if needed.
  if (copyValues) {
    tmp.copyMatchingPart (*this);
  }
  this->reference(tmp);
}

template<class T, typename Alloc>
void Array<T, Alloc>::copyMatchingPart (const Array<T, Alloc>& from)
{
  if (nelements() > 0  &&  from.nelements() > 0) {
    // Create IPositions of the correct length.
    IPosition endto (ndim(), 0);
    IPosition endfr (from.ndim(), 0);
    // Put the minimum length in each axis.
    size_t nd = from.ndim();
    if (ndim() < nd) {
      nd = ndim();
    }
    const IPosition& lento = shape();
    const IPosition& lenfr = from.shape();
    for (size_t i=0; i<nd; i++) {
      int sz = std::min(lento[i], lenfr[i]);
      endto[i] = sz-1;
      endfr[i] = sz-1;
    }
    // Get the subsection of to and from array.
    Array<T, Alloc> subto = (*this)(IPosition(ndim(), 0), endto);
    Array<T, Alloc> fromc(from);    // make non-const
    Array<T, Alloc> subfr = fromc(IPosition(from.ndim(), 0), endfr);
    // Reform to if the dimensionalities differ.
    if (subto.ndim() != subfr.ndim()) {
      Array<T, Alloc> tmp = subto.reform (endfr+1);
      subto.reference (tmp);
    }
    subto.assign_conforming(subfr);
  }    
}

template<class T, typename Alloc>
T &Array<T, Alloc>::operator()(const IPosition &index)
{
  assert(ok());

  /*if (aips_debug) {
    validateIndex(index);
  }*/
  size_t offs=0;
  for (size_t i=0; i<ndimen_p; i++) {
    offs += index(i) * steps_p(i);
  }
  return begin_p[offs];
}

template<class T, typename Alloc>
const T &Array<T, Alloc>::operator()(const IPosition &index) const
{
  assert(ok());
  size_t offs=0;
  for (size_t i=0; i<ndimen_p; i++) {
    offs += index(i) * steps_p(i);
  }
  return begin_p[offs];
}

// <thrown>
//     <item> ArrayError
// </thrown>
template<typename T, typename Alloc>
Array<T, Alloc> Array<T, Alloc>::operator()(const IPosition& b,
  const IPosition& e,
  const IPosition& i)
{
    assert(ok());
    Array<T, Alloc> tmp(*this);
    size_t offs = makeSubset (tmp, b, e, i);
    tmp.begin_p += offs;
    tmp.setEndIter();
    assert(tmp.ok());
    return tmp;
}

template<class T, typename Alloc>
const Array<T, Alloc> Array<T, Alloc>::operator()(
  const IPosition &b, const IPosition &e, const IPosition &i) const
{
    return const_cast<Array<T, Alloc>*>(this)->operator() (b,e,i);
}

template<typename T, typename Alloc> Array<T, Alloc> Array<T, Alloc>::operator()(const IPosition &b,
						const IPosition &e)
{
    IPosition i(e.nelements());
    i = 1;
    return (*this)(b,e,i);
}
template<class T, typename Alloc> const Array<T, Alloc> Array<T, Alloc>::operator()(const IPosition &b,
                                                      const IPosition &e) const
{
    return const_cast<Array<T, Alloc>*>(this)->operator() (b,e);
}

template<typename T, typename Alloc> Array<T, Alloc> Array<T, Alloc>::operator()(const Slicer& slicer)
{
    if (slicer.isFixed()) {
        return operator() (slicer.start(), slicer.end(), slicer.stride());
    }
    IPosition blc, trc, inc;
    slicer.inferShapeFromSource (shape(), blc, trc, inc);
    return operator() (blc, trc, inc);
}
template<class T, typename Alloc>
const Array<T, Alloc> Array<T, Alloc>::operator()(const Slicer& slicer) const
{
    return const_cast<Array<T, Alloc>*>(this)->operator() (slicer);
}

template<class T, typename Alloc>
std::unique_ptr<ArrayBase> Array<T, Alloc>::getSection(const Slicer& slicer) const
{
    return std::unique_ptr<ArrayBase>(new Array<T, Alloc>(operator()(slicer)));
}

template<typename T, typename Alloc> Array<T, Alloc> Array<T, Alloc>::operator[](size_t i) const
{
    assert(ok());
    size_t nd = ndim();
    IPosition s(nd, 0);
    IPosition e(shape() - 1);
    if (nd > 0) {
      nd--;
      s[nd] = i;
      e[nd] = i;
    }
    Array<T> tmp(*this);
    tmp.reference (tmp(s,e));
    return nd == 0  ?  tmp : tmp.nonDegenerate(nd);
}


template<class T, typename Alloc>
const MaskedArray<T> Array<T, Alloc>::operator() (const LogicalArray &mask) const
{
    MaskedArray<T> ret (*this, mask, true);
    return ret;
}

template<class T, typename Alloc>
MaskedArray<T> Array<T, Alloc>::operator() (const LogicalArray &mask)
{
    MaskedArray<T> ret (*this, mask);
    return ret;
}

template<class T, typename Alloc>
const MaskedArray<T> Array<T, Alloc>::operator() (const MaskedLogicalArray &mask) const
{
    MaskedArray<T> ret (*this, mask, true);
    return ret;
}

template<class T, typename Alloc>
MaskedArray<T> Array<T, Alloc>::operator() (const MaskedLogicalArray &mask)
{
    MaskedArray<T> ret (*this, mask);
    return ret;
}

template<class T, typename Alloc>
Array<T, Alloc> Array<T, Alloc>::diagonals (size_t firstAxis, long long diag) const
{
  assert(ok());
  Array<T, Alloc> tmp(*this);
  tmp.begin_p += tmp.makeDiagonal (firstAxis, diag);
  tmp.makeSteps();
  return tmp;
}


template<class T, typename Alloc> size_t Array<T, Alloc>::nrefs() const
{
  assert(ok());
  return data_p.use_count();
}

// This is relatively expensive
template<class T, typename Alloc> bool Array<T, Alloc>::ok() const
{
  assert(ArrayBase::ok());
  assert(data_p != nullptr);
  assert(!(nelements() > 0 && (begin_p == nullptr || data_p==nullptr)));
  assert(!(begin_p != nullptr && data_p->data() > begin_p));
  assert(!(begin_p != nullptr && begin_p > data_p->data() + data_p->size()));
  
  if (! ArrayBase::ok()) {
    return false;
  }
  if(data_p == nullptr)
    return false;
  if (nelements() > 0 && (begin_p == nullptr || data_p==nullptr))
    return false;
  // This test may not be portable.
  if (begin_p != nullptr && data_p->data() > begin_p) {
    return false;
  }
  // This test may not be portable.
  if (begin_p != nullptr && begin_p > data_p->data() + data_p->size()) {
    return false;
  }
  return true;
}

// <thrown>
//    <item> ArrayError
// </thrown>
template<class T, typename Alloc>
T* Array<T, Alloc>::getStorage(bool& deleteIt)
{
  assert(ok());
  deleteIt = false;

  if (ndim() == 0) {
    return nullptr;
  }

  if (contiguousStorage()) {
    return begin_p;
  }

  // We need to do a copy
  size_t n = nelements();
  T* storage = data_p->allocate(n);
  try {
    for(size_t i=0; i!=n; ++i)
      new (&storage[i]) T();
    copyToContiguousStorage(storage, *this);
  } catch (...) {
    // TODO To be correct, the destructors of the already
    // constructed object should be called, but this is
    // a border case so ignored for now.
    data_p->deallocate(storage, nelements());
    throw;
  }
  deleteIt = true;
  return storage;
}

template<class T, typename Alloc> void Array<T, Alloc>::putStorage(T *&storage, bool deleteAndCopy)
{
  assert(ok());

  if (deleteAndCopy == false) {
    storage = nullptr;
    return;
  }

  if (ndim() == 1) {
    move_n_with_stride(storage, length_p(0), begin_p, inc_p(0), 1U);
  } else if (length_p(0) == 1  &&  ndim() == 2) {
    // Special case which can be quite common (e.g. row in a matrix).
    move_n_with_stride(storage, length_p(1), begin_p,
      originalLength_p(0)*inc_p(1), 1U);
  } else if (length_p(0) <= 25) {
    // If not many elements on a line, it's better to use this loop.
    T* ptr = storage;
    iterator iterend=end();
    for (iterator iter=begin(); iter!=iterend; ++iter) {
      *iter = std::move(*ptr);
      ++ptr;
    }
  } else {
    ArrayPositionIterator ai(this->shape(), 1);
    size_t offset;
    IPosition index(ndim());
    size_t count=0;
    while (! ai.pastEnd()) {
      index = ai.pos();
      offset = ArrayIndexOffset(ndim(), originalLength_p.storage(),
        inc_p.storage(), index);
      move_n_with_stride(storage+count*length_p(0), length_p(0), begin_p+offset, inc_p(0), 1U);
      ai.next();
      count++;
    }
  }
  T const * &fakeStorage = const_cast<T const *&>(storage);
  freeStorage(fakeStorage, deleteAndCopy);
}

template<class T, typename Alloc>
void Array<T, Alloc>::freeStorage(const T*&storage, bool deleteIt) const
{
  assert(ok());

  if (deleteIt) {
    // The cast is required since you can't delete a const array; however
    // if deleteIt is set the array came from new.
    T* ptr = const_cast<T*>(storage);
    size_t n = nelements();
    for(size_t i=0; i!=n; ++i)
      ptr[i].~T();
    // TODO this is only allowed when allocator is always equal, but is done for
    // now to keep the method const.
    // see e.g. std::allocator_traits<allocator_type>::is_always_equal
    data_p->deallocate(ptr, n);
  }
  storage = nullptr;
}

template<class T, typename Alloc>
void *Array<T, Alloc>::getVStorage(bool &deleteIt)
{
    return getStorage (deleteIt);
}
template<class T, typename Alloc>
const void *Array<T, Alloc>::getVStorage(bool &deleteIt) const
{
    return getStorage (deleteIt);
}
template<class T, typename Alloc>
void Array<T, Alloc>::putVStorage(void *&storage, bool deleteAndCopy)
{
  T* &ptr = reinterpret_cast<T*&>(storage);
  putStorage (ptr, deleteAndCopy);
}
template<class T, typename Alloc>
void Array<T, Alloc>::freeVStorage(const void *&storage, bool deleteAndCopy) const
{
  const T* &ptr = reinterpret_cast<const T*&>(storage);
  freeStorage (ptr, deleteAndCopy);
}

template<class T, typename Alloc>
void Array<T, Alloc>::takeStorage(const IPosition &shape, T *storage,
                           StorageInitPolicy policy, const Alloc& allocator)
{
  preTakeStorage(shape);

  size_t new_nels = shape.product();
  
  if(policy == SHARE)
  {
    data_p = Storage<T, Alloc>::MakeFromSharedData(storage, new_nels, allocator);
  }
  else {
    if (data_p==nullptr || data_p.use_count() > 1 || data_p->is_shared()
            || data_p->size() != new_nels) {
      data_p = Storage<T, Alloc>::MakeFromMove(storage, storage+new_nels, allocator);
    } else {
        std::move(storage, storage+new_nels, data_p->data());
    }
  }
  ArrayBase::assign(ArrayBase(shape));

  begin_p = data_p->data();
  setEndIter();
  
  if(policy == TAKE_OVER)
  {
    // TODO this is not consistent with old behaviour
    for(size_t i=0; i!=new_nels; ++i)
      storage[new_nels-i-1].~T();
    Alloc(allocator).deallocate(storage, new_nels);
  }
  
  // Call OK at the end rather than the beginning since this might
  // be called from a constructor.
  assert(ok());

  postTakeStorage();
}

template<class T, typename Alloc>
void Array<T, Alloc>::takeStorage(const IPosition &shape, const T *storage,
  const Alloc& allocator)
{
    // This cast is safe since a copy will be made
    T *storagefake = const_cast<T*>(storage);
    takeStorage(shape, storagefake, COPY, allocator);
}


template<class T, typename Alloc>
std::unique_ptr<ArrayPositionIterator> Array<T, Alloc>::makeIterator (size_t byDim) const
{
    return std::unique_ptr<ArrayPositionIterator>( new ArrayIterator<T, Alloc> (*this, byDim) );
}



template<class T, typename Alloc>
Array<T, Alloc>::BaseIteratorSTL::BaseIteratorSTL (const Array<T, Alloc>& arr)
: itsLineIncr (0),
  itsCurPos   (arr.ndim(), 0),
  itsArray    (&arr),
  itsContig   (arr.contiguousStorage())
{
  // An empty array has to be handled.
  if (arr.nelements() == 0) {
    itsPos = 0;
    itsContig = true;
  } else {
    // Set the last cursor position.
    // Handle the case for the end iterator.
    itsLastPos = arr.shape() - 1;
    // If the array is not contiguous, we iterate "line by line" in
    // the increment function. Optimize for the case where the length
    // of the lower dimensions is 1. All such dimensions can be included
    // in the "line".
    // At the end itsLineAxis gives the axis where the next "line" starts.
    itsPos = &((*itsArray)(itsCurPos));
    if (!itsContig) {
      itsLineAxis = 0;
      while (itsLineAxis < arr.ndim()-1
	     &&  itsLastPos(itsLineAxis) == 0) {
	itsLineAxis++;
      }
      itsCurPos(itsLineAxis) = 1;
      itsLineIncr = itsArray->steps()(itsLineAxis) - 1;
      itsLineEnd = itsPos + itsLastPos(itsLineAxis) * (itsLineIncr+1);
      itsCurPos(itsLineAxis) = 0;
    }
  }
}

template<class T, typename Alloc>
void Array<T, Alloc>::BaseIteratorSTL::increment()
{
  size_t axis;
  for (axis=itsLineAxis+1; axis<itsCurPos.nelements(); axis++) {
    if (itsCurPos(axis) < itsLastPos(axis)) {
      itsCurPos(axis)++;
      itsLineEnd += itsArray->steps()(axis);
      break;
    }
    itsCurPos(axis) = 0;
    itsLineEnd -= itsLastPos(axis) * itsArray->steps()(axis);
  }
  if (axis == itsCurPos.nelements()) {
    itsPos = itsArray->cend();
  } else {
    itsPos = itsLineEnd - itsLastPos(itsLineAxis) * (itsLineIncr+1);
  }
}


template<class T, typename Alloc>
std::vector<T> Array<T, Alloc>::tovector() const {
  bool deleteIt;
  const T *stor = getStorage(deleteIt);
  std::vector<T> out;
  out.assign(stor, stor+nelements());
  // TODO this is formally not allowed: the allocator is not const, so
  // might cause the object to change. This tovector() method can obviously be implemented
  // in a const manner, so this needs to be rewritten.
  const_cast<Array<T, Alloc>*>(this)->freeStorage(stor, deleteIt);
  return out;
}

template<class T, typename Alloc>
template<class U>
void Array<T, Alloc>::tovector(std::vector<T, U> &out) const {
  bool deleteIt;
  const T *stor = getStorage(deleteIt);
  out.assign(stor, stor+nelements());
  /// See note above for @ref tovector()
  const_cast<Array<T, Alloc>*>(this)->freeStorage(stor, deleteIt);
}

} //#End casa namespace

#endif
