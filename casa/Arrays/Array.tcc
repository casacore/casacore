//# Array.cc: A templated N-D Array class with zero origin
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

#ifndef CASA_ARRAY_TCC
#define CASA_ARRAY_TCC

#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/MaskedArray.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Arrays/ArrayIter.h>
#include <casacore/casa/Arrays/ArrayError.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/BasicMath/Functional.h>
#include <casacore/casa/Utilities/Copy.h>

namespace casacore {//#Begin casa namespace


template<class T> Array<T>::Array()
: data_p   (new Block<T>(0)),
  end_p    (0)
{
    begin_p = data_p->storage();
    DebugAssert(ok(), ArrayError);
}

// <thrown>
//   <item> ArrayShapeError
// </thrown>
template<class T> Array<T>::Array(const IPosition &Shape)
: ArrayBase (Shape)
{
    data_p = new Block<T>(nelements());
    begin_p = data_p->storage();
    setEndIter();
    DebugAssert(ok(), ArrayError);
}

// <thrown>
//   <item> ArrayShapeError
// </thrown>
template<class T> Array<T>::Array(const IPosition &Shape,
				  const T &initialValue)
: ArrayBase (Shape)
{
    data_p = new Block<T>(nelements());
    begin_p = data_p->storage();
    setEndIter();
    DebugAssert(ok(), ArrayError);
    objset (begin_p, initialValue, nels_p);
}


template<class T> Array<T>::Array(const Array<T> &other)
: ArrayBase (other),
  begin_p   (other.begin_p),
  end_p     (other.end_p)
{
    data_p = other.data_p;
    DebugAssert(ok(), ArrayError);
}

template<class T>
Array<T>::Array(const IPosition &shape, T *storage, 
		StorageInitPolicy policy)
: ArrayBase (shape)
{
    takeStorage(shape, storage, policy);
    DebugAssert(ok(), ArrayError);
}

template<class T>
Array<T>::Array (const IPosition &shape, const T *storage)
: ArrayBase (shape)
{
    takeStorage(shape, storage);
    DebugAssert(ok(), ArrayError);
}



template<class T> Array<T>::~Array()
{
}

template<class T> CountedPtr<ArrayBase> Array<T>::makeArray() const
{
  return new Array<T>();
}

template<class T> void Array<T>::assign (const Array<T>& other)
{
    DebugAssert(ok(), ArrayError);
    if (! shape().isEqual (other.shape())) {
        resize (other.shape());
    }
    operator= (other);
}

template<class T> void Array<T>::assignBase (const ArrayBase& other, Bool checkType)
{
    DebugAssert(ok(), ArrayError);
    // Checking the type can be expensive, so only do if needed or in debug mode.
    if (checkType  ||  aips_debug) {
      const Array<T>* pa = dynamic_cast<const Array<T>*>(&other);
      if (pa == 0) {
        throw ArrayError("assign(ArrayBase&) has incorrect template type");
      }
    }
    assign (static_cast<const Array<T>&>(other));
}

template<class T> void Array<T>::reference(const Array<T> &other)
{
    DebugAssert(ok(), ArrayError);
    // First copy data, then meta data.
    // This is better in case of multi-threading because it makes it possible
    // to test the size and be sure that the data is there.
    data_p  = other.data_p;
    begin_p = other.begin_p;
    end_p   = other.end_p;
    baseCopy (other);
}

template<class T> Array<T> Array<T>::copy() const
{
    DebugAssert(ok(), ArrayError);

    Array<T> vp(shape());
    if (ndim() == 0) {
        return vp;
    } else if (contiguousStorage()) {
	objcopy (vp.begin_p, begin_p, nels_p);
    } else if (ndim() == 1) {
	objcopy (vp.begin_p, begin_p, length_p(0), 1U, inc_p(0));
    } else if (length_p(0) == 1  &&  ndim() == 2) {
        // Special case which can be quite common (e.g. row in a matrix).
	objcopy (vp.begin_p, begin_p, length_p(1), 1U,
		 originalLength_p(0)*inc_p(1));
    } else if (length_p(0) <= 25) {
        // If not many elements on a line, it's better to use this loop.
        T* ptr = vp.begin_p;
        const_iterator iterend=end();
        for (const_iterator iter=begin(); iter!=iterend; ++iter) {
	    *ptr++ = *iter;
	}
    } else {
	// Step through Vector by Vector
	// The output is guaranteed to have all incs set to 1
	ArrayPositionIterator ai(shape(), 1);
	IPosition index(ndim());
        size_t offset;
        size_t count=0;
	while (! ai.pastEnd()) {
	    index = ai.pos();
	    offset = ArrayIndexOffset(ndim(), originalLength_p.storage(),
				      inc_p.storage(), index);
	    objcopy (vp.begin_p + count*length_p(0), begin_p+offset,
		     length_p(0), 1U, inc_p(0));
	    ai.next(); count++;
	}
    }
    return vp;
}

template<class T> Array<T> &Array<T>::operator=(const Array<T> &other)
{
    DebugAssert(ok(), ArrayError);

    if (this == &other) {
	return *this;
    }
    Bool Conform = conform(other);
    if (!Conform  &&  nelements() != 0) {
	validateConformance(other);  // We can't overwrite, so throw exception
    }
    size_t offset, offset2;
    IPosition index(other.ndim());

    if (Conform == True) { // Copy in place
        if (ndim() == 0) {
	    return *this;
	} else if (contiguousStorage() && other.contiguousStorage()) {
	    objcopy (begin_p, other.begin_p, nels_p);
	} else if (ndim() == 1) {
	    objcopy (begin_p, other.begin_p, length_p(0), inc_p(0),
		     other.inc_p(0));
	} else if (length_p(0) == 1  &&  ndim() == 2) {
            // Special case which can be quite common (e.g. row in a matrix).
	    objcopy (begin_p, other.begin_p, length_p(1),
		     originalLength_p(0)*inc_p(1),
		     other.originalLength_p(0)*other.inc_p(1));
	} else if (length_p(0) <= 25) {
	    // If not many elements on a line, it's better to use this loop.
	    const_iterator from(other.begin());
	    iterator iterend=end();
	    for (iterator iter=begin(); iter!=iterend; ++iter) {
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
		objcopy (begin_p+offset, other.begin_p+offset2,
			 length_p(0), inc_p(0),
			 other.inc_p(0));
		ai.next();
	    }
	}
    } else {
	// Array was empty; make a new copy and reference it.
	Array<T> tmp (other.copy());
	reference (tmp);
    }
    return *this;
}

template<class T> Array<T> &Array<T>::operator=(const T &val)
{
    DebugAssert(ok(), ArrayError);

    set (val);
    return *this;
}

template<class T> Array<T> &Array<T>::operator= (const MaskedArray<T> &marray)
{
    DebugAssert(ok(), ArrayError);

    if (!conform(marray)) {
        throw(ArrayConformanceError(
            "Array<T> & Array<T>::operator= (const MaskedArray<T> &marray)"
            "- Conformance error."));
    }

    Bool deleteThis;
    T *thisStorage = getStorage(deleteThis);
    T *thisS = thisStorage;

    Bool deleteArr;
    const T *arrStorage = marray.getArrayStorage(deleteArr);
    const T *arrS = arrStorage;

    Bool deleteMask;
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


template<class T> void Array<T>::set(const T &Value)
{
    DebugAssert(ok(), ArrayError);

    // Ultimately we should go to RawFillAll functions
    // RawFillAll(ndim(), begin_p, inc_p.storage(), length_p.storage(), Value);
    // Step through Vector by Vector
    size_t offset;
    if (ndim() == 0) {
        return;
    } else if (contiguousStorage()) {
	objset (begin_p, Value, nels_p);
    } else if (ndim() == 1) {
	objset (begin_p, Value, length_p(0), inc_p(0));
    } else if (length_p(0) == 1  &&  ndim() == 2) {
        // Special case which can be quite common (e.g. row in a matrix).
        objset (begin_p, Value, length_p(1),
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
	while (! ai.pastEnd()) {
	    index = ai.pos();
	    offset = ArrayIndexOffset(ndim(), originalLength_p.storage(),
				      inc_p.storage(), index);
	    objset(begin_p+offset, Value, length_p(0), inc_p(0));
	    ai.next();
	}
    }
}

template<class T> void Array<T>::apply(T (*function)(T))
{
    DebugAssert(ok(), ArrayError);

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
		begin_p[offset + i*incr] = function(begin_p[offset + i*incr]);
	    }
	    ai.next();
	}
    }
}

template<class T> void Array<T>::apply(T (*function)(const T &))
{
    DebugAssert(ok(), ArrayError);

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

template<class T> void Array<T>::apply(const Functional<T,T> &function)
{
    DebugAssert(ok(), ArrayError);

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

template<class T> void Array<T>::unique()
{
    DebugAssert(ok(), ArrayError);

    // short circuit when we are unique and flat
    if (contiguousStorage()  &&  nrefs() == 1) {
	return;
    }
    // OK, we know we are going to need to copy.
    Array<T> tmp (copy());
    reference (tmp);
}

// <thrown>
//   <item> ArrayConformanceError
// </thrown>
template<class T> Array<T> Array<T>::reform(const IPosition &len) const
{
    DebugAssert(ok(), ArrayError);
    // Check if reform is possible and needed.
    // If not needed, simply return a copy.
    Array<T> tmp(*this);
    baseReform (tmp, len);
    tmp.setEndIter();
    return tmp;
}

template<class T>
Array<T> Array<T>::nonDegenerate (uInt startingAxis, Bool throwIfError) const
{
    Array<T> tmp;
    DebugAssert(ok(), ArrayError);
    tmp.nonDegenerate (*this, startingAxis, throwIfError);
    return tmp;
}

template<class T>
void Array<T>::nonDegenerate (const Array<T> &other, uInt startingAxis,
			      Bool throwIfError)
{
    if (startingAxis < other.ndim()) {
	IPosition ignoreAxes(startingAxis);
	for (uInt i=0; i<startingAxis; i++) {
	    ignoreAxes(i) = i;
	}
	nonDegenerate (other, ignoreAxes);
    } else {
        if (throwIfError) {
	    AlwaysAssert(startingAxis < other.ndim(), ArrayError);

	}
	reference (other);
    }
}

template<class T>
Array<T> Array<T>::nonDegenerate (const IPosition &ignoreAxes) const
{
    Array<T> tmp;
    DebugAssert(ok(), ArrayError);
    tmp.nonDegenerate(*this, ignoreAxes);
    return tmp;
}

template<class T>
void Array<T>::removeDegenerate (uInt startingAxis, Bool throwIfError)
{
    Array<T> tmp;
    DebugAssert(ok(), ArrayError);
    tmp.nonDegenerate (*this, startingAxis, throwIfError);
    reference (tmp);
}

template<class T>
void Array<T>::removeDegenerate (const IPosition &ignoreAxes)
{
    Array<T> tmp;
    DebugAssert(ok(), ArrayError);
    tmp.nonDegenerate(*this, ignoreAxes);
    reference (tmp);
}

template<class T>
void Array<T>::doNonDegenerate (const Array<T> &other,
                                const IPosition &ignoreAxes)
{
    DebugAssert(ok(), ArrayError);
    baseNonDegenerate (other, ignoreAxes);
    begin_p = other.begin_p;
    data_p  = other.data_p;
    setEndIter();
}

template<class T>
const Array<T> Array<T>::addDegenerate(uInt numAxes) const
{
    Array<T> * This = const_cast<Array<T>*>(this);
    const Array<T> tmp(This->addDegenerate(numAxes));
    return tmp;
}

template<class T>
Array<T> Array<T>::addDegenerate(uInt numAxes)
{
    DebugAssert(ok(), ArrayError);
    Array<T> tmp(*this);
    if (numAxes > 0) {
        baseAddDegenerate (tmp, numAxes);
	tmp.setEndIter();
    }
    return tmp;
}


template<class T> Bool Array<T>::conform(const MaskedArray<T> &other) const
{
    return conform (other.getArray());
}

// <thrown>
//   <item> ArrayConformanceError
// </thrown>
template<class T> void Array<T>::resize()
{
    resize (IPosition());
}
template<class T> void Array<T>::resize(const IPosition &len, Bool copyValues)
{
    DebugAssert(ok(), ArrayError);
    // Maybe we don't need to resize; let's see if we can short circuit
    if (len.isEqual (shape())) {
      return;
    }
    // OK we differ, so we really have to resize ourselves.
    Array<T> tmp(len);
    // Copy the contents if needed.
    if (copyValues) {
      tmp.copyMatchingPart (*this);
    }
    this->reference(tmp);
}

template<class T> void Array<T>::copyMatchingPart (const Array<T> &from)
{
  if (nelements() > 0  &&  from.nelements() > 0) {
    // Create IPositions of the correct length.
    IPosition endto (ndim(), 0);
    IPosition endfr (from.ndim(), 0);
    // Put the minimum length in each axis.
    uInt nd = from.ndim();
    if (ndim() < nd) {
      nd = ndim();
    }
    const IPosition& lento = shape();
    const IPosition& lenfr = from.shape();
    for (uInt i=0; i<nd; i++) {
      Int sz = std::min(lento[i], lenfr[i]);
      endto[i] = sz-1;
      endfr[i] = sz-1;
    }
    // Get the subsection of to and from array.
    Array<T> subto = (*this)(IPosition(ndim(), 0), endto);
    Array<T> fromc(from);    // make non-const
    Array<T> subfr = fromc(IPosition(from.ndim(), 0), endfr);
    // Reform to if the dimensionalities differ.
    if (subto.ndim() != subfr.ndim()) {
      Array<T> tmp = subto.reform (endfr+1);
      subto.reference (tmp);
    }
    subto = subfr;
  }    
}

template<class T> T &Array<T>::operator()(const IPosition &index)
{
    DebugAssert(ok(), ArrayError);

    if (aips_debug) {
	validateIndex(index);
    }
    size_t offs=0;
    for (uInt i=0; i<ndimen_p; i++) {
        offs += index(i) * steps_p(i);
    }
    return begin_p[offs];
}

template<class T> const T &Array<T>::operator()(const IPosition &index) const
{
    DebugAssert(ok(), ArrayError);
    size_t offs=0;
    for (uInt i=0; i<ndimen_p; i++) {
        offs += index(i) * steps_p(i);
    }
    return begin_p[offs];
}

// <thrown>
//     <item> ArrayError
// </thrown>
template<class T> Array<T> Array<T>::operator()(const IPosition &b,
						const IPosition &e,
						const IPosition &i)
{
    DebugAssert(ok(), ArrayError);
    Array<T> tmp(*this);
    size_t offs = makeSubset (tmp, b, e, i);
    tmp.begin_p += offs;
    tmp.setEndIter();
    DebugAssert (tmp.ok(), ArrayError);
    return tmp;
}
template<class T> const Array<T> Array<T>::operator()(const IPosition &b,
                                                      const IPosition &e,
                                                      const IPosition &i) const
{
    return const_cast<Array<T>*>(this)->operator() (b,e,i);
}

template<class T> Array<T> Array<T>::operator()(const IPosition &b,
						const IPosition &e)
{
    IPosition i(e.nelements());
    i = 1;
    return (*this)(b,e,i);
}
template<class T> const Array<T> Array<T>::operator()(const IPosition &b,
                                                      const IPosition &e) const
{
    return const_cast<Array<T>*>(this)->operator() (b,e);
}

template<class T> Array<T> Array<T>::operator()(const Slicer& slicer)
{
    if (slicer.isFixed()) {
        return operator() (slicer.start(), slicer.end(), slicer.stride());
    }
    IPosition blc, trc, inc;
    slicer.inferShapeFromSource (shape(), blc, trc, inc);
    return operator() (blc, trc, inc);
}
template<class T> const Array<T> Array<T>::operator()(const Slicer& slicer) const
{
    return const_cast<Array<T>*>(this)->operator() (slicer);
}

template<class T>
CountedPtr<ArrayBase> Array<T>::getSection(const Slicer& slicer) const
{
    return new Array<T>(operator()(slicer));
}

template<class T> Array<T> Array<T>::operator[](size_t i) const
{
    DebugAssert(ok(), ArrayError);
    uInt nd = ndim();
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


template<class T>
const MaskedArray<T> Array<T>::operator() (const LogicalArray &mask) const
{
    MaskedArray<T> ret (*this, mask, True);
    return ret;
}

template<class T>
MaskedArray<T> Array<T>::operator() (const LogicalArray &mask)
{
    MaskedArray<T> ret (*this, mask);
    return ret;
}

template<class T>
const MaskedArray<T> Array<T>::operator() (const MaskedLogicalArray &mask) const
{
    MaskedArray<T> ret (*this, mask, True);
    return ret;
}

template<class T>
MaskedArray<T> Array<T>::operator() (const MaskedLogicalArray &mask)
{
    MaskedArray<T> ret (*this, mask);
    return ret;
}


template<class T> uInt Array<T>::nrefs() const
{
    DebugAssert(ok(), ArrayError);
    return data_p.nrefs();
}

// This is relatively expensive
template<class T> Bool Array<T>::ok() const
{
    if (! ArrayBase::ok()) {
        return False;
    }
    if (nelements() > 0 && (begin_p == 0 || data_p.null()))
	return False;
    // This test may not be portable.
    if (data_p->storage() > begin_p) {
	return False;
    }
    // This test may not be portable.
    if (begin_p > data_p->storage() + data_p->nelements()) {
	return False;
    }
    return True;
}


// <thrown>
//    <item> ArrayError
// </thrown>
template<class T> T *Array<T>::getStorage(Bool &deleteIt)
{
    DebugAssert(ok(), ArrayError);
    deleteIt = (!contiguousStorage());

    if (ndim() == 0) {
	return 0;
    }

    if (deleteIt == False) {
	return begin_p;
    }

    // OK, we are unlucky so we need to do a copy
    T *storage = new T[nelements()];
    if (storage == 0) {
	throw(ArrayError("Array<T>::getStorage - new of copy buffer fails"));
    }
    // ok - copy it
    if (ndim() == 1) {
	objcopy(storage, begin_p, length_p(0), 1U, inc_p(0));
    } else if (length_p(0) == 1  &&  ndim() == 2) {
        // Special case which can be quite common (e.g. row in a matrix).
	objcopy(storage, begin_p, length_p(1), 1U,
		originalLength_p(0)*inc_p(1));
    } else if (length_p(0) <= 25) {
        // If not many elements on a line, it's better to use this loop.
        T* ptr = storage;
	iterator iterend=end();
        for (iterator iter=begin(); iter!=iterend; ++iter) {
	    *ptr++ = *iter;
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
	    objcopy(storage + count*length_p(0), begin_p+offset,
		    length_p(0), 1U, inc_p(0));
	    ai.next(); count++;
	}
    }
    return storage;
}

template<class T> void Array<T>::putStorage(T *&storage, Bool deleteAndCopy)
{
    DebugAssert(ok(), ArrayError);

    if (deleteAndCopy == False) {
	storage = 0;
	return;
    }

    if (ndim() == 1) {
	objcopy(begin_p, storage, length_p(0), inc_p(0), 1U);
    } else if (length_p(0) == 1  &&  ndim() == 2) {
        // Special case which can be quite common (e.g. row in a matrix).
	objcopy(begin_p, storage, length_p(1),
		originalLength_p(0)*inc_p(1), 1U);
    } else if (length_p(0) <= 25) {
        // If not many elements on a line, it's better to use this loop.
        const T* ptr = storage;
        iterator iterend=end();
        for (iterator iter=begin(); iter!=iterend; ++iter) {
	    *iter = *ptr++;
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
	    objcopy(begin_p+offset, storage+count*length_p(0),
		    length_p(0), inc_p(0), 1U);
	    ai.next(); count++;
	}
    }
    delete [] storage;
    storage = 0;
}

template<class T>
void Array<T>::freeStorage(const T*&storage, Bool deleteIt) const
{
    DebugAssert(ok(), ArrayError);

    if (deleteIt) {
	// The cast is required since you can't delete a const array; however
	// if deleteIt is set the array came from new.
	delete [] const_cast<T*>(storage);
    }
    storage = 0;
}

template<class T>
void *Array<T>::getVStorage(Bool &deleteIt)
{
    return getStorage (deleteIt);
}
template<class T>
const void *Array<T>::getVStorage(Bool &deleteIt) const
{
    return getStorage (deleteIt);
}
template<class T>
void Array<T>::putVStorage(void *&storage, Bool deleteAndCopy)
{
  T* ptr = static_cast<T*>(storage);
  putStorage (ptr, deleteAndCopy);
  storage = 0;
}
template<class T>
void Array<T>::freeVStorage(const void *&storage, Bool deleteAndCopy) const
{
  const T* ptr = static_cast<const T*>(storage);
  freeStorage (ptr, deleteAndCopy);
  storage = 0;
}


template<class T>
void Array<T>::takeStorage(const IPosition &shape, T *storage,
			   StorageInitPolicy policy)
{
    baseCopy (ArrayBase(shape));
    size_t new_nels = shape.product();

    switch(policy) {
    case COPY:
	if (data_p.null()  ||  data_p.nrefs() > 1
        ||  data_p->nelements() != new_nels) {
	    data_p = new Block<T>(new_nels);
	}
	objcopy(data_p->storage(), storage, new_nels);
	break;
    case TAKE_OVER:
    case SHARE:
	if (data_p.null() || data_p.nrefs() > 1) {
	    data_p = new Block<T>(0);
	}
	data_p->replaceStorage(new_nels, storage, (policy == TAKE_OVER));
	break;
    default:
	throw(AipsError("Array<T>::takeStorage - unknown policy"));
    }
    begin_p = data_p->storage();
    setEndIter();
    // Call OK at the end rather than the beginning since this might
    // be called from a constructor.
    DebugAssert(ok(), ArrayError);
}

template<class T>
void Array<T>::takeStorage(const IPosition &shape, const T *storage)
{
    // This cast is safe since a copy will be made
    T *storagefake = const_cast<T*>(storage);
    takeStorage(shape, storagefake, COPY);
}


template<class T>
CountedPtr<ArrayPositionIterator> Array<T>::makeIterator (uInt byDim) const
{
    return new ArrayIterator<T> (*this, byDim);
}



template<class T>
Array<T>::BaseIteratorSTL::BaseIteratorSTL (const Array<T>& arr)
: itsLineIncr (0),
  itsCurPos   (arr.ndim(), 0),
  itsArray    (&arr),
  itsContig   (arr.contiguousStorage())
{
  // An empty array has to be handled.
  if (arr.nelements() == 0) {
    itsPos = 0;
    itsContig = True;
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

template<class T>
void Array<T>::BaseIteratorSTL::increment()
{
  uInt axis;
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


template<class T>
vector<T> Array<T>::tovector() const {
  Bool deleteIt;
  const T *stor = this->getStorage(deleteIt);
  vector<T> out;
  out.assign(stor, stor+nelements());
  this->freeStorage(stor, deleteIt);
  return out;
}


} //#End casa namespace

#endif
