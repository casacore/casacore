//# Array.cc: A templated N-D Array class with zero origin
//# Copyright (C) 1993,1994,1995,1996,1997,1998
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

#include <aips/Arrays/MaskedArray.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayPosIter.h>
#include <aips/Arrays/ArrayError.h>
#include <aips/Utilities/Assert.h>
#include <aips/Functionals/Functional.h>
#include <aips/Utilities/Copy.h>


//# Implement rtti functions.
rtti_imp_mbrf_a1(Array);


template<class T> Array<T>::Array()
: nels_p   (0),
  ndimen_p (0),
  data_p   (new Block<T>(0)),
  contiguous_p (True)
{
    begin_p = data_p->storage();
    DebugAssert(ok(), ArrayError);
}

// <thrown>
//   <item> ArrayShapeError
// </thrown>
template<class T> Array<T>::Array(const IPosition &Shape)
: nels_p   (Shape.product()),
  ndimen_p (Shape.nelements()),
  length_p (Shape),
  inc_p    (Shape.nelements(), 1),
  originalLength_p(Shape),
  data_p   (0),
  contiguous_p (True)
{
    for (uInt i = 0; i < ndimen_p; i++) {
	if (Shape(i) < 0) {
	    throw(ArrayShapeError(shape(), Shape,
	      "Array<T>::Array(const IPosition &)"
	      " - Negative shape"));
	}
    }
    data_p = new Block<T>(nelements());
    begin_p = data_p->storage();
    DebugAssert(ok(), ArrayError);
}

// <thrown>
//   <item> ArrayShapeError
// </thrown>
template<class T> Array<T>::Array(const IPosition &Shape, const T &initialValue)
: nels_p   (Shape.product()),
  ndimen_p (Shape.nelements()),
  length_p (Shape),
  inc_p    (Shape.nelements(), 1),
  originalLength_p(Shape),
  data_p   (0),
  contiguous_p (True)
{
    for (uInt i = 0; i < ndimen_p; i++) {
	if (Shape(i) < 0) {
	    throw(ArrayShapeError(shape(), Shape,
	      "Array<T>::Array(const IPosition &, const T &)"
	      " - Negative shape"));
	}
    }
    data_p = new Block<T>(nelements());
    begin_p = data_p->storage();
    DebugAssert(ok(), ArrayError);
    objset (begin_p, initialValue, nels_p);
}


template<class T> Array<T>::Array(const Array<T> &other)
: nels_p   (other.nels_p),
  ndimen_p (other.ndimen_p),
  length_p (other.length_p),
  inc_p    (other.inc_p),
  originalLength_p(other.originalLength_p),
  begin_p  (other.begin_p),
  contiguous_p (other.contiguous_p)
{
    data_p = other.data_p;
    DebugAssert(ok(), ArrayError);
}

template<class T>
Array<T>::Array(const IPosition &shape, T *storage, 
		StorageInitPolicy policy)
: nels_p   (0),
  ndimen_p (shape.nelements()),
  length_p (shape), 
  inc_p    (shape.nelements(), 1),
  originalLength_p(shape.nelements(), 0), 
  data_p   (0),
  contiguous_p (True)
{
    takeStorage(shape, storage, policy);
    DebugAssert(ok(), ArrayError);
}

template<class T>
Array<T>::Array (const IPosition &shape, const T *storage)
: nels_p   (0),
  ndimen_p (shape.nelements()),
  length_p (shape), 
  inc_p    (shape.nelements(), 1),
  originalLength_p(shape.nelements(), 0), 
  data_p   (0),
  contiguous_p (True)
{
    takeStorage(shape, storage);
    DebugAssert(ok(), ArrayError);
}



template<class T> Array<T>::~Array()
{
    // Nothing
}

template<class T> void Array<T>::cleanup()
{
    this->Array<T>::~Array();
}

template<class T> void Array<T>::reference(Array<T> &other)
{
    DebugAssert(ok(), ArrayError);

    ndimen_p = other.ndimen_p;
    length_p.resize (ndimen_p);
    length_p = other.length_p;
    nels_p   = other.nels_p;
    originalLength_p.resize (ndimen_p);
    originalLength_p = other.originalLength_p;
    inc_p.resize (ndimen_p);
    inc_p    = other.inc_p;
    data_p   = other.data_p;
    begin_p  = other.begin_p;
    contiguous_p = other.contiguous_p;
}

template<class T> Array<T> Array<T>::copy() const
{
    DebugAssert(ok(), ArrayError);

    Array<T> vp(shape());
    uInt offset;

    if (ndim() == 0) {
        return vp;
    } else if (contiguousStorage()) {
	objcopy (vp.begin_p, begin_p, nels_p);
    } else if (ndim() == 1) {
	objcopy (vp.begin_p, begin_p, uInt(length_p(0)), 1U, uInt(inc_p(0)));
    } else {
	// Step through Vector by Vector
	// The output is guaranteed to have all incs set to 1
	ArrayPositionIterator ai(shape(), 1);
	IPosition index(ndim());
        uInt count=0;
	while (! ai.pastEnd()) {
	    index = ai.pos();
	    offset = ArrayIndexOffset(ndim(), originalLength_p.storage(),
				      inc_p.storage(), index);
	    objcopy (vp.begin_p + count*length_p(0), begin_p+offset,
		     uInt(length_p(0)), 1U, uInt(inc_p(0)));
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
    uInt offset, offset2;
    IPosition index(other.ndim());

    if (Conform == True) { // Copy in place
      if (ndim() == 0) {
	    return *this;
      } else if (contiguousStorage() && other.contiguousStorage()) {
	  objcopy (begin_p, other.begin_p, nels_p);
      } else if (ndim() == 1) {
	    objcopy (begin_p, other.begin_p, uInt(length_p(0)), uInt(inc_p(0)),
		     uInt(other.inc_p(0)));
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
			 uInt(length_p(0)), uInt(inc_p(0)),
			 uInt(other.inc_p(0)));
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

    uInt ntotal = nelements();
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
    uInt offset;
    if (ndim() == 0) {
        return;
    } else if (contiguousStorage()) {
	objset (begin_p, Value, nels_p);
    } else if (ndim() == 1) {
	objset (begin_p, Value, uInt(length_p(0)), uInt(inc_p(0)));
    } else {
	// Step through Vector by Vector
	ArrayPositionIterator ai(shape(), 1);
	IPosition index(ndim());
	while (! ai.pastEnd()) {
	    index = ai.pos();
	    offset = ArrayIndexOffset(ndim(), originalLength_p.storage(),
				      inc_p.storage(), index);
	    objset(begin_p+offset, Value, uInt(length_p(0)), uInt(inc_p(0)));
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
	for (uInt i=0; i<nels_p; i++) {
	    begin_p[i] = function(begin_p[i]);
	}
    } else {
	// Step through Vector by Vector
	ArrayPositionIterator ai(shape(), 1);
	IPosition index(ndim());

	uInt len  = length_p(0);
	uInt incr = inc_p(0);
	uInt offset;

	while (! ai.pastEnd()) {
	    index = ai.pos();
	    offset = ArrayIndexOffset(ndim(), originalLength_p.storage(),
				      inc_p.storage(), index);
	    for (uInt i=0; i < len; i++) {
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
	for (uInt i=0; i<nels_p; i++) {
	    begin_p[i] = function(begin_p[i]);
	}
    } else {
	// Step through Vector by Vector
	ArrayPositionIterator ai(shape(), 1);
	IPosition index(ndim());

	uInt len  = length_p(0);
	uInt incr = inc_p(0);
	uInt offset;

	while (! ai.pastEnd()) {
	    index = ai.pos();
	    offset = ArrayIndexOffset(ndim(), originalLength_p.storage(),
				      inc_p.storage(), index);
	    for (uInt i=0; i < len; i++) {
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
	for (uInt i=0; i<nels_p; i++) {
	    begin_p[i] = function(begin_p[i]);
	}
    } else {
	// Step through Vector by Vector
	ArrayPositionIterator ai(shape(), 1);
	IPosition index(ndim());

	uInt len  = length_p(0);
	uInt incr = inc_p(0);
	uInt offset;

	while (! ai.pastEnd()) {
	    index = ai.pos();
	    offset = ArrayIndexOffset(ndim(), originalLength_p.storage(),
				      inc_p.storage(), index);
	    for (uInt i=0; i < len; i++) {
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
    if (len.product() != Int(nelements())) {
	throw(ArrayConformanceError("Array<T>::reform() - "
				    "total elements differ"));
    }
    // When the new shape equals the current one, simply return a copy.
    if (len.isEqual (length_p)) {
	return *this;
    }
    uInt newNdim = len.nelements();
    // When the data is contiguous, a reform can simply be done
    // by making a copy of the array and inserting the new shape.
    if (contiguousStorage()) {
	Array<T> tmp(*this);
	tmp.ndimen_p = newNdim;
	tmp.length_p.resize (newNdim);
	tmp.length_p = len;
	tmp.inc_p.resize (newNdim);
	tmp.inc_p = 1;
	tmp.originalLength_p.resize (newNdim);
	tmp.originalLength_p = tmp.length_p;
	return tmp;
    }
    // A reform of a non-contiguous array has to be done.
    // This is only possible if axes with length 1 are left out and/or added.
    Bool valid = True;
    uInt oldPos=0;
    uInt newPos=0;
    Int oldLen = length_p(0);
    Int newLen = len(0);
    // Find the axes corresponding to the old shape.
    // copyAxes(i)<0 indicates that an axis with length 1 has been added.
    // When a shape array array is exhausted, its length variable is set
    // to 0. In that way trailing dimensions are handled without problem.
    IPosition copyAxes(newNdim, -1);
    while (valid  &&  (oldLen>0  ||  newLen>0)) {
	if (oldLen == newLen) {
	    copyAxes(newPos) = oldPos;
	    oldPos++;
	    newPos++;
	} else if (oldLen == 1) {
	    oldPos++;
	} else if (newLen == 1) {
	    newPos++;
	} else {
	    // A new axis with length>1 has no corresponding original axis.
	    valid = False;
	}
	oldLen = (oldPos >= length_p.nelements()  ?  0 : length_p(oldPos));
	newLen = (newPos >= len.nelements()  ?  0 : len(newPos));
    }
    if (!valid) {
	throw(ArrayConformanceError("Array<T>::reform() - "
				 "data not contiguous nor similarly shaped"));
    }
    // Great, the shapes match. Create a copy and adjust the IPositions.
    // Set inc and originalLength initially to 1 (caters for added axes).
    Array<T> tmp(*this);
    tmp.ndimen_p = newNdim;
    tmp.length_p.resize (newNdim);
    tmp.length_p = len;
    tmp.inc_p.resize (newNdim);
    tmp.inc_p = 1;
    tmp.originalLength_p.resize (newNdim);
    tmp.originalLength_p = 1;
    // When an axis has been removed Inc and originalLength have to be adjusted
    // by multiplying them with the originalLength of the removed axes.
    uInt startAxis = 0;
    for (uInt i=0; i<newNdim; i++) {
	if (copyAxes(i) >= 0) {
	    tmp.inc_p(i) = inc_p(copyAxes(i));
	    tmp.originalLength_p(i) = originalLength_p(copyAxes(i));
	    for (Int j=startAxis; j<copyAxes(i); j++) {
		tmp.inc_p(i) *= originalLength_p(j);
		tmp.originalLength_p(i) *= originalLength_p(j);
	    }
	    startAxis = copyAxes(i) + 1;
	}
    }
    return tmp;
}

template<class T>
const Array<T> Array<T>::nonDegenerate (uInt startingAxis) const
{
    return ((Array<T>*) this)->nonDegenerate (startingAxis);
}

template<class T>
Array<T> Array<T>::nonDegenerate (uInt startingAxis)
{
    Array<T> tmp;
    DebugAssert(ok(), ArrayError);
    tmp.nonDegenerate (*this, startingAxis);
    return tmp;
}

template<class T>
void Array<T>::nonDegenerate (Array<T> &other, uInt startingAxis)
{
    AlwaysAssert(startingAxis < other.ndim(), ArrayError);
    IPosition ignoreAxes(startingAxis);
    for (uInt i=0; i<startingAxis; i++) {
	ignoreAxes(i) = i;
    }
    nonDegenerate (other, ignoreAxes);
}

template<class T>
const Array<T> Array<T>::nonDegenerate (const IPosition &ignoreAxes) const
{
    return ((Array<T>*)this)->nonDegenerate(ignoreAxes);
}

template<class T>
Array<T> Array<T>::nonDegenerate (const IPosition &ignoreAxes)
{
    Array<T> tmp;
    DebugAssert(ok(), ArrayError);
    tmp.nonDegenerate(*this, ignoreAxes);
    return tmp;
}

template<class T>
void Array<T>::doNonDegenerate (Array<T> &other, const IPosition &ignoreAxes)
{
    DebugAssert(ok(), ArrayError);
    AlwaysAssert(other.ndim() > 0, AipsError);

    // These data members are the same irrespective of the degenerate axes. 
    nels_p  = other.nels_p;
    begin_p = other.begin_p;
    data_p  = other.data_p;
    contiguous_p = other.contiguous_p;
  
    // To remove degenerate axes use two passes - first find out how many axes
    // have to be kept.
    uInt i;
    uInt nd = other.ndim();
    // First determine which axes have to be ignored, thus always be kept.
    // Do not count here, because in theory ignoreAxes can contain the
    // same axis more than once.
    IPosition keepAxes(nd, 0);
    for (i=0; i<ignoreAxes.nelements(); i++) {
	AlwaysAssert (ignoreAxes(i) < Int(nd), AipsError);
	keepAxes(ignoreAxes(i)) = 1;
    }
    // Now count all axes to keep.
    uInt count=0;
    for (i=0; i<nd; i++) {
	if (keepAxes(i) == 1) {
	    count++;
	}else{
	    if (other.length_p(i) != 1) {
		keepAxes(i) = 1;
		count++;
	    }
	}
    }
    
    // A special case - all axes have length=1
    if (count == 0) {
	ndimen_p = 1;
	length_p.resize(1, False);
	length_p(0) = other.length_p(0);
	inc_p.resize(1, False);
	inc_p(0) = other.inc_p(0);
	originalLength_p.resize(1, False);
	originalLength_p(0) = other.originalLength_p(0);
	return;        // early exit - special case
    }
    
    ndimen_p = count;
    length_p.resize(count, False);
    inc_p.resize(count, False);
    originalLength_p.resize(count, False);
    // Maybe we have no axes to remove
    if (count == other.ndim()){
	length_p = other.length_p;
	originalLength_p = other.originalLength_p;
	inc_p = other.inc_p;
	return;
    }
    
    // OK, we have some axes to remove
    uInt skippedVolume = 1;
    count=0;
    for (i=0; i<nd; i++) {
	if (keepAxes(i) == 1) {
	    length_p(count) = other.length_p(i);
	    originalLength_p(count) = other.originalLength_p(i);
	    inc_p(count) = skippedVolume*other.inc_p(i);
	    skippedVolume = 1;
	    count++;
	}else{
	    skippedVolume *= other.originalLength_p(i);
	}
    }
}

template<class T>
const Array<T> Array<T>::addDegenerate(uInt numAxes) const
{
    Array<T> * This = (Array<T> *) this;
    const Array<T> tmp(This->addDegenerate(numAxes));
    return tmp;
}

template<class T>
Array<T> Array<T>::addDegenerate(uInt numAxes)
{
    DebugAssert(ok(), ArrayError);
    Array<T> tmp(*this);
    if (numAxes == 0)
	return tmp;
    
    const uInt newDim = ndim() + numAxes;
    IPosition newLength(newDim), newInc(newDim), newOriginal(newDim);
    
    uInt i;
    for (i=0; i < ndim(); i++) {
	newLength(i) = length_p(i);
	newOriginal(i) = originalLength_p(i);
	newInc(i) = inc_p(i);
    }
    for (i=ndim(); i < newDim; i++){
	newLength(i) = 1;
	newOriginal(i) = 1;
	newInc(i) = 1;
    }
    tmp.ndimen_p = newDim;
    tmp.length_p.resize (newDim);
    tmp.length_p = newLength;
    tmp.inc_p.resize (newDim);
    tmp.inc_p = newInc;
    tmp.originalLength_p.resize (newDim);
    tmp.originalLength_p = newOriginal;
    return tmp;
}

template<class T> Bool Array<T>::conform(const Array<T> &other) const
{
    DebugAssert(ok(), ArrayError);

    if (ndim() != other.ndim()) {
	return False;
    }
    for (uInt i=0; i < ndim(); i++) {
	if (length_p(i) != other.length_p(i)) {
	    return False;
	}
    }
    return True;
}

template<class T> Bool Array<T>::conform(const MaskedArray<T> &other) const
{
    return conform (other.getArray());
}

// <thrown>
//   <item> ArrayConformanceError
// </thrown>
template<class T> void Array<T>::resize(const IPosition &len)
{
    DebugAssert(ok(), ArrayError);

    // Maybe we don't need to resize; let's see if we can short circuit
    Bool sameShape = True;
    if (len.nelements() == ndim()) {
	for (uInt i=0; i < ndim(); i++) {
	    if (length_p(i) != len(i)) {
		sameShape = False;
		break;
	    }
	}
	if (sameShape) {
	    return;
	}
    }

    // OK we differ, so we really have to resize ourselves
    Array<T> tmp(len);
    this->reference(tmp);
}

template<class T> T &Array<T>::operator()(const IPosition &index)
{
    DebugAssert(ok(), ArrayError);

    if (aips_debug) {
	validateIndex(index);
    }
    uInt i = ArrayIndexOffset(ndim(), originalLength_p.storage(),
			      inc_p.storage(), index);
    return begin_p[i];
}

template<class T> const T &Array<T>::operator()(const IPosition &index) const
{
    DebugAssert(ok(), ArrayError);

    uInt i = ArrayIndexOffset(ndim(), originalLength_p.storage(),
			      inc_p.storage(), index);

    return begin_p[i];
}

// <thrown>
//     <item> ArrayError
// </thrown>
template<class T> Array<T> Array<T>::operator()(const IPosition &b,
						const IPosition &e,
						const IPosition &i)
{
    DebugAssert(ok(), ArrayError);

    if (b.nelements() != ndim() || e.nelements() != ndim() ||
	i.nelements() != ndim()) {
	throw(ArrayError("Array<T>::operator()(b,e,i) - ndim() differs from"
			 " an iposition size"));
    }
    uInt j;
    for (j=0; j < ndim(); j++) {
	if (b(j) < 0 || b(j) > e(j)
	||  e(j) >= length_p(j)  ||  i(j) < 1) {
	    throw(ArrayError("Array<T>::operator()(b,e,i) - b,e or i "
			     "incorrectly specified"));
	}
    }
    Array<T> tmp(*this);
    Int offset = ArrayIndexOffset(ndim(), originalLength_p.storage(),
				  inc_p.storage(), b);
    tmp.begin_p += offset;
    for (j=0; j < ndim(); j++) {
	tmp.inc_p(j) *= i(j);
	tmp.length_p(j) = (e(j) - b(j) + i(j))/i(j);
    }
    tmp.nels_p = tmp.length_p.product();
    tmp.contiguous_p = tmp.isStorageContiguous();
    DebugAssert (tmp.ok(), ArrayError);
    return tmp;
}

template<class T> Array<T> Array<T>::operator()(const IPosition &b,
						const IPosition &e)
{
    DebugAssert(ok(), ArrayError);

    IPosition i(e.nelements());
    i = 1;
    return (*this)(b,e,i);
}

template<class T>
MaskedArray<T> Array<T>::operator() (const LogicalArray &mask) const
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
MaskedArray<T> Array<T>::operator() (const MaskedLogicalArray &mask) const
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



// <thrown>
//    <item> ArrayNDimErrror
//    <item> ArrayShapeError
// </thrown>
template<class T> void Array<T>::validateConformance(const Array<T> &o) const
{
    DebugAssert(ok(), ArrayError);

    if (conform(o) == False) {
	if (ndim() != o.ndim()) {
	    throw(ArrayNDimError(ndim(), o.ndim(),
				 "Array<T>::validateConformance<T>"));
	} else {
	    throw(ArrayShapeError(shape(), o.shape(),
				  "Array<T>::validateConformance"));
	}
    }
}

// <thrown>
//    <item> ArrayNDimErrror
//    <item> ArrayIndexError
// </thrown>
template<class T> void Array<T>::validateIndex(const IPosition &i) const
{
    DebugAssert(ok(), ArrayError);

    if (ndim() != i.nelements()) {
	throw(ArrayNDimError(ndim(), i.nelements(),
			     "Array<T>::validateIndex - ndims of index"
			     " and array differ"));
    }
    for (uInt j=0; j < ndim(); j++) {
	if (i(j) < 0  ||  i(j) >= length_p(j)) {
	    throw(ArrayIndexError(i, length_p));
	}
    }
    // OK - normal return
}

template<class T> Bool Array<T>::isStorageContiguous() const
{
    Int nd = ndim();
    if (nd == 0) {
	return True;
    }

    // If we have increments, we're definitely not contiguous (unless the axis
    // length is one!)
    for (Int i=0; i < nd; i++) {
	if ((inc_p(i) != 1) && (length_p(i) != 1)) {
	    return False;
	}
    }

    // If we don't fill up the region (except for the last dimension), then
    // we're also not contiguous
    //
    //   -------------------------
    //   |                       |
    //   |                       |
    //   |                       |
    //   |           +---+       |
    //   |           |   |       |
    //   |           |   |       |
    //   |           +---+       |
    //   -------------------------
    //
    // Here, even though the increment is one, we need to make a copy since
    // all the elements in the sub-region aren't contiguous. Note, though, that
    // the lengths don't need to be identical in the last axis.
    // Trailing lengths equal to 1 can be skipped.

    while (nd > 1  &&  length_p(nd-1) == 1) {
	nd--;
    }
    for (i=0; i < nd - 1; i++) {
	if (length_p(i) != originalLength_p(i)) {
	    return False;
	}
    }
    // If we've made it here, we are contiguous!
    return True;
}

template<class T> uInt Array<T>::nrefs() const
{
    DebugAssert(ok(), ArrayError);

    return data_p.nrefs();
}

// This is relatively expensive
template<class T> Bool Array<T>::ok() const
{
    if (ndimen_p != ndim())
	return False;
    // We don't check for exact equality because sometimes for efficiency
    // the dimensionality of start et al can be greater than that which is
    // required (e.g. when making a slice.
    if (length_p.nelements() != ndim())
        return False;
    if (inc_p.nelements() != ndim())
        return False;
    if (originalLength_p.nelements() != ndim())
	return False;

    uInt i;
    uInt count = 1;

    for (i=0; i < ndim(); i++) {
	if (length_p(i) < 0  ||  inc_p(i) < 1
	||  originalLength_p(i) < length_p(i))
	    return False;
	count *= length_p(i);
    }
    if (ndim() == 0)
	count = 0;
    if (count != nelements())
	return False;
    if (nelements() > 0 && (begin_p == 0 || data_p.null()))
	return False;
    // This test likely isn't portable
    if (data_p->storage() > begin_p) {
	return False;
    }
    // This test likely isn't portable
    if (begin_p > data_p->storage() + data_p->nelements()*sizeof(T)) {
	return False;
    }
    if (contiguous_p != isStorageContiguous()) {
	return False;
    }
    return True;
}

template<class T> IPosition Array<T>::end() const
{
    DebugAssert(ok(), ArrayError);

    IPosition tmp(ndim());
    for (uInt i=0; i < ndim(); i++) {
	tmp(i) = length_p(i) - 1;
    }
    return tmp;
}

// <thrown>
//    <item> ArrayError
// </thrown>
template<class T> T *Array<T>::getStorage(Bool &deleteIt)
{
    DebugAssert(ok(), ArrayError);
    deleteIt = ToBool(!contiguousStorage());

    if (ndim() == 0) {
	return 0;
    }

    if (deleteIt == False)
	return begin_p;

    // OK, we are unlucky so we need to do a copy
    T *storage = new T[nelements()];
    if (storage == 0) {
	throw(ArrayError("Array<T>::getStorage - new of copy buffer fails"));
    }
    // ok - copy it
    if (ndim() == 1) {
	objcopy(storage, begin_p, uInt(length_p(0)), 1U, uInt(inc_p(0)));
    } else {
	ArrayPositionIterator ai(this->shape(), 1);
	uInt offset;
	IPosition index(ndim());
	uInt count=0;
	while (! ai.pastEnd()) {
	    index = ai.pos();
	    offset = ArrayIndexOffset(ndim(), originalLength_p.storage(),
				      inc_p.storage(), index);
	    objcopy(storage + count*length_p(0), begin_p+offset,
		    uInt(length_p(0)), 1U, uInt(inc_p(0)));
	    ai.next(); count++;
	}
    }
    return storage;
}

template<class T> const T *Array<T>::getStorage(Bool &deleteIt) const
{
    DebugAssert(ok(), ArrayError);

    // The cast is OK because the return pointer will be cast to const
    Array<T> *This = (Array<T> *)this;
    return This->getStorage(deleteIt);
}

template<class T> void Array<T>::putStorage(T *&storage, Bool deleteAndCopy)
{
    DebugAssert(ok(), ArrayError);

    if (deleteAndCopy == False) {
	storage = 0;
	return;
    }

    if (ndim() == 1) {
	objcopy(begin_p, storage, uInt(length_p(0)), uInt(inc_p(0)), 1U);
    } else {
	ArrayPositionIterator ai(this->shape(), 1);
	uInt offset;
	IPosition index(ndim());
	uInt count=0;
	while (! ai.pastEnd()) {
	    index = ai.pos();
	    offset = ArrayIndexOffset(ndim(), originalLength_p.storage(),
				      inc_p.storage(), index);
	    objcopy(begin_p+offset, storage+count*length_p(0),
		    uInt(length_p(0)), uInt(inc_p(0)), 1U);
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
	delete [] (T *)storage;
    }
    storage = 0;
}

template<class T>
void Array<T>::takeStorage(const IPosition &shape, T *storage,
			   StorageInitPolicy policy)
{
    uInt new_ndimen = shape.nelements();
    uInt new_nels = shape.product();

    if (ndim() != shape.nelements()) {
	length_p.resize(new_ndimen);
	originalLength_p.resize(new_ndimen);
	inc_p.resize(new_ndimen);
    }

    inc_p    = 1;
    length_p = shape;
    originalLength_p = length_p;

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
	data_p->replaceStorage(new_nels, storage, ToBool(policy == TAKE_OVER));
	break;
    default:
	throw(AipsError("Array<T>::takeStorage - unknown policy"));
    }
    begin_p  = data_p->storage();
    nels_p   = new_nels;
    ndimen_p = new_ndimen;
    // Call OK at the end rather than the beginning since this might
    // be called from a constructor.
    DebugAssert(ok(), ArrayError);
}

template<class T>
void Array<T>::takeStorage(const IPosition &shape, const T *storage)
{
    // This cast is safe since a copy will be made
    T *storagefake = (T *)storage;
    takeStorage(shape, storagefake, COPY);
}



template<class T, class U>
  Bool conform2 (const Array<T> &left, const Array<U> &right)
{

    IPosition leftShape (left.shape());
    IPosition rightShape (right.shape());

    return ( (leftShape.conform (rightShape)) && (leftShape == rightShape) )
           ? True : False;
}


