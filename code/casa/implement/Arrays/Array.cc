//# Array.cc: A templated N-D Array class with variable origin
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

#include <aips/Arrays/MaskedArray.h>
#include <aips/Arrays/Array.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Arrays/ArrayPosIter.h>
#include <aips/Arrays/ArrayError.h>
#include <aips/Utilities/Assert.h>
#include <aips/Functionals/Functional.h>
#include <aips/Utilities/Copy.h>


//# Implement rtti functions.
rtti_imp_mbrf_a1(Array);


template<class T> Array<T>::Array()
: start(1,0), length(1,0), inc(1,1), originalLength(1,0), data(new Block<T>(0))
{
    ndimen = 1;
    nels = 0;
    begin = data->storage();
    DebugAssert(ok(), ArrayError);
}

#if defined(__GNUG__)
typedef Block<Int> forgnugpp; // Who knows why this is necessary!
template<class T> Array<T>::Array(uInt Ndim, const forgnugpp &Shape)
#else
template<class T> Array<T>::Array(uInt Ndim, const Block<Int> &Shape)
#endif
: start(Ndim,0), length(Ndim,1), inc(Ndim,1), originalLength(Ndim, 1)
{
    for (Int i = 0; i < Ndim; i++)
	if (Shape[i] < 0) {
	    // Copy the block to an IPosition so we can throw it
	    IPosition Shape2(Shape.nelements());
	    for (Int j=0; j < Shape2.nelements(); j++) {
		Shape2(j) = Shape[j];
	    }
	    throw(ArrayShapeError(shape(), Shape2,
				  "Array<T>::Array(uInt, Block<Int>"));
	}

    ndimen = Ndim;

    for (i=0; i < Ndim; i++) {
	length[i] = Shape[i];
	originalLength[i] = Shape[i];
    }
    nels = ArrayVolume(ndimen, length.storage());
    data = new Block<T>(nelements());
    begin = data->storage();
    DebugAssert(ok(), ArrayError);
}

// <thrown>
//    <item> ArrayNDimError
//    <item> ArrayShapeError
// </thrown>
template<class T> Array<T>::Array(const IPosition &Shape,
				     const IPosition &Origin)
: start(Shape.nelements()), length(Shape.nelements(),1),
  inc(Shape.nelements(),1), originalLength(Shape.nelements(), 1)
{
    ndimen = Shape.nelements();
    if (ndimen != Origin.nelements())
	throw(ArrayNDimError(ndim(), Origin.nelements(),
              "Array<T>::Array(const IPosition &,const IPosition &) - ndims of Shape and Origin differ"));

    for (Int i = 0; i < ndimen; i++)
	if (Shape(i) < 0)
	    throw(ArrayShapeError(shape(), Shape,
			  "Array<T>::Array(constIPosition &, const IPosition &)"
			  " - Negative shape"));

    for (i=0; i < Shape.nelements(); i++) {
	length[i] = Shape(i);
	originalLength[i] = Shape(i);
	start[i] = Origin(i);
    }
    nels = ArrayVolume(ndimen, length.storage());
    data = new Block<T>(nelements());
    begin = data->storage();

    DebugAssert(ok(), ArrayError);
}

// <thrown>
//   <item> ArrayShapeError
// </thrown>
template<class T> Array<T>::Array(const IPosition &Shape)
: start(Shape.nelements(),0), length(Shape.nelements(),1),
  inc(Shape.nelements(),1),  originalLength(Shape.nelements(), 1)
{
    ndimen = Shape.nelements();

    for (Int i = 0; i < ndimen; i++)
	if (Shape(i) < 0)
	    throw(ArrayShapeError(shape(), Shape,
	      "Array<T>::Array(constIPosition &, const IPosition &)"
	      " - Negative shape"));


    for (i=0; i < Shape.nelements(); i++) {
	length[i] = Shape(i);
	originalLength[i] = Shape(i);
    }
    nels = ArrayVolume(ndimen, length.storage());
    data = new Block<T>(nelements());
    begin = data->storage();

    DebugAssert(ok(), ArrayError);
}


template<class T> Array<T>::Array(const Array<T> &other)
: start(other.start), length(other.length), inc(other.inc),
  begin(other.begin), ndimen(other.ndimen),
  originalLength(other.originalLength), nels(other.nels)
{
    data = other.data;

    DebugAssert(ok(), ArrayError);
}

template<class T>
Array<T>::Array(const IPosition &shape, T *storage, 
		StorageInitPolicy policy)
: start(shape.nelements(),0), length(shape.nelements(),0), 
  inc(shape.nelements(),1), originalLength(shape.nelements(),0), 
  data(0)
{
    ndimen = shape.nelements();
    nels = 0;
    begin = 0;
    takeStorage(shape, storage, policy);
    DebugAssert(ok(), ArrayError);
}

template<class T>
Array<T>::Array(const IPosition &shape, const T *storage)
: start(shape.nelements(),0), length(shape.nelements(),0), 
  inc(shape.nelements(),1), originalLength(shape.nelements(),0), 
  data(0)
{
    ndimen = shape.nelements();
    nels = 0;
    begin = 0;
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

    ndimen = other.ndimen;
    start = other.start;
    length = other.length;
    nels = other.nels;
    originalLength = other.originalLength;
    inc = other.inc;
    data = other.data;
    begin = other.begin;
}

template<class T> Array<T> Array<T>::copy() const
{
    DebugAssert(ok(), ArrayError);

    Array<T> vp(shape(), origin());
    uInt offset;

    if (ndim() == 0) {
        return vp;
    } else if (contiguousStorage()) {
	objcopy(vp.begin, begin, nels);
    } else if (ndim() == 1) {
	objcopy(vp.begin, begin, uInt(length[0]), 1U, uInt(inc[0]));
    } else {
	// Step through Vector by Vector
	// The output is guaranteed to have all incs set to 1
	ArrayPositionIterator ai(shape(), origin(), 1);
	IPosition index(ndim());
        uInt count=0;
	while (! ai.pastEnd()) {
	    index = ai.pos();
	    offset = ArrayIndexOffset(ndim(), originalLength.storage(),
				      start.storage(), inc.storage(), index);
	    objcopy(vp.begin + count*length[0], begin+offset, uInt(length[0]),
		    1U, uInt(inc[0]));
	    ai.next(); count++;
	}
    }
    return vp;
}

template<class T> Array<T> &Array<T>::operator=(const Array<T> &other)
{
    DebugAssert(ok(), ArrayError);

    if (this == &other)
	return *this;

    Bool Conform = conform(other);
    if (Conform == False && nelements() != 0)
	validateConformance(other);  // We can't overwrite, so throw exception

    uInt offset, offset2;
    IPosition index(other.ndim());

    if (Conform == True) { // Copy in place
      if (ndim() == 0) {
	    return *this;
      } else if (contiguousStorage() && other.contiguousStorage()) {
	  objcopy(begin, other.begin, nels);
      } else if (ndim() == 1) {
	    objcopy(begin, other.begin, uInt(length[0]), uInt(inc[0]),
		    uInt(other.inc[0]));
	} else {
	    ArrayPositionIterator ai(other.shape(), other.origin(), 1);
	    // Step through Vector by Vector
	    // The output is guaranteed to have all incs set to 1
	    while (! ai.pastEnd()) {
		index = ai.pos();
		offset = ArrayIndexOffset(ndim(), originalLength.storage(),
					  start.storage(),inc.storage(),
					  index);
		offset2 = ArrayIndexOffset(other.ndim(),
					   other.originalLength.storage(),
					   other.start.storage(),
					   other.inc.storage(), index);
		objcopy(begin+offset, other.begin+offset2, uInt(length[0]),
			uInt(inc[0]), uInt(other.inc[0]));
		ai.next();
	    }
	}
    } else {
	// We're a 0-sized; resize and copy

	data = new Block<T>(other.nelements());
	begin = data->storage();
	start = other.start;
	length = other.length;
	nels = other.nels;
	originalLength = length;
	inc.resize(other.inc.nelements()); inc.set(1);
	ndimen = other.ndimen;
	if (ndim() == 0) {
	    return *this;
	} 
	if (other.contiguousStorage()) {
	    objcopy(begin, other.begin, nels);
	} else if (ndim() == 1) {
	    objcopy(begin, other.begin, uInt(length[0]), uInt(inc[0]),
		    uInt(other.inc[0]));
	} else {
	    // Step through Vector by Vector
	    // The output is guaranteed to have all incs set to 1
	    ArrayPositionIterator ai(other.shape(), other.origin(), 1);
	    uInt count=0;
	    while (! ai.pastEnd()) {
		index = ai.pos();
		offset2 = ArrayIndexOffset(other.ndim(),
					   other.originalLength.storage(),
					  other.start.storage(),
					   other.inc.storage(), index);
		objcopy(begin  + count*length[0]*inc[0], other.begin+offset2,
			uInt(length[0]), uInt(inc[0]), uInt(other.inc[0]));
		ai.next(); count++;
	    }
	}
    }
    return *this;
}

template<class T> Array<T> &Array<T>::operator=(const T &val)
{
    DebugAssert(ok(), ArrayError);

    set(val);
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
    // RawFillAll(ndim(), begin, inc.storage(), length.storage(), Value);
    // Step through Vector by Vector
    uInt offset;
    if (ndim() == 0) {
        return;
    } else if (contiguousStorage()) {
	objset(begin, Value, nels);
    } else if (ndim() == 1) {
	objset(begin, Value, uInt(length[0]), uInt(inc[0]));
    } else {
	// Step through Vector by Vector
	ArrayPositionIterator ai(shape(), origin(), 1);
	IPosition index(ndim());
	while (! ai.pastEnd()) {
	    index = ai.pos();
	    offset = ArrayIndexOffset(ndim(), originalLength.storage(),
				      start.storage(), inc.storage(), index);
	    objset(begin+offset, Value, uInt(length[0]), uInt(inc[0]));
	    ai.next();
	}
    }
}

// Set all of the array (but honoring the increments) to Value.
template<class T> void Array<T>::apply(T (*function)(T))
{
    DebugAssert(ok(), ArrayError);

    if (nelements() == 0) {
        return; // short-circuit
    }

    if (contiguousStorage()) {
	for (uInt i=0; i<nels; i++) {
	    begin[i] = function(begin[i]);
	}
    } else {
	// Step through Vector by Vector
	ArrayPositionIterator ai(shape(), origin(), 1);
	IPosition index(ndim());

	uInt len  = length[0];
	uInt incr = inc[0];
	uInt offset;

	while (! ai.pastEnd()) {
	    index = ai.pos();
	    offset = ArrayIndexOffset(ndim(), originalLength.storage(),
				      start.storage(), inc.storage(), index);
	    for (uInt i=0; i < len; i++) {
		begin[offset + i*incr] = function(begin[offset + i*incr]);
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
	for (uInt i=0; i<nels; i++) {
	    begin[i] = function(begin[i]);
	}
    } else {
	// Step through Vector by Vector
	ArrayPositionIterator ai(shape(), origin(), 1);
	IPosition index(ndim());

	uInt len  = length[0];
	uInt incr = inc[0];
	uInt offset;

	while (! ai.pastEnd()) {
	    index = ai.pos();
	    offset = ArrayIndexOffset(ndim(), originalLength.storage(),
				      start.storage(), inc.storage(), index);
	    for (uInt i=0; i < len; i++) {
		begin[offset+i*incr] = function(begin[offset+i*incr]);
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
	for (uInt i=0; i<nels; i++) {
	    begin[i] = function(begin[i]);
	}
    } else {
	// Step through Vector by Vector
	ArrayPositionIterator ai(shape(), origin(), 1);
	IPosition index(ndim());

	uInt len  = length[0];
	uInt incr = inc[0];
	uInt offset;

	while (! ai.pastEnd()) {
	    index = ai.pos();
	    offset = ArrayIndexOffset(ndim(), originalLength.storage(),
				      start.storage(), inc.storage(), index);
	    for (uInt i=0; i < len; i++) {
		begin[offset+i*incr] = function(begin[offset+i*incr]);
	    }
	    ai.next();
	}
    }
}

template<class T> void Array<T>::unique()
{
    DebugAssert(ok(), ArrayError);

    // short circuit when we are unique and flat
    Bool flat = contiguousStorage();
    if (flat && nrefs() == 1) {
	return;
    }

    // OK, we know we are going to need to copy
    Block<T> *vp = new Block<T>(nelements());
    uInt i, offset;
    if (ndim() == 0) {
      // Nothing
    } else if (flat) {
	objcopy(vp->storage(), begin, nels);
    } else if (ndim() == 1) {
	objcopy(vp->storage(), begin, uInt(length[0]), 1U, uInt(inc[0]));
    } else {
	// Step through Vector by Vector
	// The output is guaranteed to have all incs set to 1
	ArrayPositionIterator ai(shape(), origin(), 1);
	IPosition index(ndim());
	uInt count = 0;
	while (! ai.pastEnd()) {
	    index = ai.pos();
	    offset = ArrayIndexOffset(ndim(), originalLength.storage(),
				      start.storage(),
				      inc.storage(), index);
	    objcopy(vp->storage() + count*length[0], begin + offset,
		    uInt(length[0]), 1U, uInt(inc[0]));
	    ai.next(); count++;
	}
    }

    data = vp;
    begin = data->storage();
    for (i=0; i < ndim(); i++)
	inc[i] = 1;

}

// <thrown>
//   <item> ArrayConformanceError
// </thrown>
template<class T> Array<T> Array<T>::reform(const IPosition &len,
					    const IPosition &or) const
{
    DebugAssert(ok(), ArrayError);

    if (len.nelements() != or.nelements())
	    throw(ArrayConformanceError("Array<T>::reform() - "
					"illegal origin or length specifed"));

    uInt total = 1;
    for (Int i = 0; i < len.nelements(); i++)
	total *= len(i);

    if (total != nelements())
	throw(ArrayConformanceError("Array<T>::reform() - "
				    "total elements differ"));

    for (i = 0; i < ndim(); i++) {
	if (inc[i] != 1) {
	    throw(ArrayConformanceError("Array<T>::reform() - "
					"increment not unity"));
	    break;
	}
    }

    Array<T> tmp(*this);
    tmp.ndimen = len.nelements();
    tmp.length.resize(tmp.ndim());
    tmp.start.resize(tmp.ndim());
    tmp.inc.resize(tmp.ndim()); tmp.inc.set(1);
    for (i = 0; i < tmp.ndim(); i++) {
	tmp.start[i] = or(i);
	tmp.length[i] = len(i);
    }
    tmp.nels = ArrayVolume(tmp.ndimen, tmp.length.storage());

    tmp.originalLength = tmp.length; // ok since the lengths are one
    return tmp;
}

template<class T> 
const Array<T> Array<T>::nonDegenerate(uInt startingAxis) const {
  Array<T> * This = (Array<T> *) this;
  const Array<T> tmp((*This).nonDegenerate(startingAxis));
  return tmp;
}

template<class T> Array<T> Array<T>::nonDegenerate(uInt startingAxis)
{
    DebugAssert(ok(), ArrayError);
    if (ndim() > 0) {
        AlwaysAssert(startingAxis < ndim(), ArrayError);
    }

    Array<T> tmp(*this);

//     if (nelements() == 0) {
// 	tmp.ndimen = 1;
// 	tmp.originalLength.resize(1, True);
// 	tmp.inc.resize(1, True);
// 	tmp.start.resize(1, True);
// 	tmp.length.resize(1, True);
// 	tmp.length[0] = 0;
// 	return tmp;        // early exit - special case
//     }

    // OK - remove degenerate axes. Two passes - first find out how many
    // axes are of length one.
    uInt count=0;
    for (Int i=startingAxis; i < ndim(); i++)
	if (length[i] != 1) count++;
    count += startingAxis;

     // Another special case - all lengths are one.
     if (count == 0) {
 	tmp.ndimen = 1;
 	tmp.originalLength.resize(1, True);
 	tmp.inc.resize(1, True);
 	tmp.start.resize(1, True);
 	tmp.length.resize(1, True);
 	tmp.length[0] = 1;
 	return tmp;        // early exit - special case
     }

    // Maybe we are COMPLETELY non-degenerate
    if (ndim() == count)
	return tmp;        // early exit - special case

    // OK, we have some length==1 axes
    Block<Int> newLength(count), newInc(count), newStart(count),
	newOriginal(count);
    uInt skippedVolume = 1;
    count=0;
    for (i=0; i < ndim(); i++) {
	if (length[i] != 1 || i < startingAxis) {
	    newLength[count] = length[i];
	    newStart[count] = start[i];
	    newOriginal[count] = originalLength[i];
	    newInc[count] = skippedVolume*inc[i];
	    skippedVolume = 1;
	    count++;
	} else {
	    skippedVolume *= originalLength[i];
	}
    }
    tmp.ndimen = count;
    tmp.length = newLength;
    tmp.inc = newInc;
    tmp.start = newStart;
    tmp.originalLength = newOriginal;
    return tmp;
}

template<class T> 
const Array<T> Array<T>::addDegenerate(uInt numAxes) const {
  Array<T> * This = (Array<T> *) this;
  const Array<T> tmp((*This).addDegenerate(numAxes));
  return tmp;
}

template<class T> Array<T> Array<T>::addDegenerate(uInt numAxes)
{
    DebugAssert(ok(), ArrayError);
    Array<T> tmp(*this);
    if (numAxes == 0)
      return tmp;

    const uInt newDim = ndim() + numAxes;
    Block<Int> newLength(newDim), newInc(newDim), newStart(newDim),
	newOriginal(newDim);

    uInt i;
    for (i=0; i < ndim(); i++) {
      newLength[i] = length[i];
      newStart[i] = start[i];
      newOriginal[i] = originalLength[i];
      newInc[i] = inc[i];
    }
    for (i=ndim(); i < newDim; i++){
      newLength[i] = 1;
      newStart[i] = 0;
      newOriginal[i] = 1;
      newInc[i] = 1;
    }
    tmp.ndimen = newDim;
    tmp.length = newLength;
    tmp.inc = newInc;
    tmp.start = newStart;
    tmp.originalLength = newOriginal;
    return tmp;
}

template<class T> Bool Array<T>::conform(const Array<T> &other) const
{
    DebugAssert(ok(), ArrayError);

    if (ndim() != other.ndim())
	return False;

    for (int i=0; i < ndim(); i++) {
	if (length[i] != other.length[i])
	    return False;
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
template<class T> void Array<T>::resize(const IPosition &len,
					const IPosition &or)
{
    DebugAssert(ok(), ArrayError);

    if (len.nelements() != or.nelements())
	    throw(ArrayConformanceError("Array<T>::resize() - "
					"illegal origin or length specifed"));
    // Maybe we don't need to resize; let's see if we can short circuit
    Bool sameShape = True;
    if (len.nelements() == ndim()) {
	for (Int i=0; i < ndim(); i++) {
	    if (length[i] != len(i)) {
		sameShape = False;
		break;
	    }
	}
	if (sameShape) {
	    for (i=0; i < ndim(); i++)
		start[i] = or(i);
	    return;
	}
    }

    // OK we differ, so we really have to resize ourselves
    Array<T> tmp(len, or);
    this->reference(tmp);
}

template<class T> void Array<T>::resize(const IPosition &len)
{
    DebugAssert(ok(), ArrayError);

    IPosition or(len.nelements());
    or = 0;
    resize(len,or);
}

template<class T> T &Array<T>::operator()(const IPosition &index)
{
    DebugAssert(ok(), ArrayError);

    if (aips_debug) {
	validateIndex(index);
    }
    uInt i = ArrayIndexOffset(ndim(), originalLength.storage(),
			      start.storage(),
			      inc.storage(), index);
    return begin[i];
}

template<class T> const T &Array<T>::operator()(const IPosition &index) const
{
    DebugAssert(ok(), ArrayError);

    uInt i = ArrayIndexOffset(ndim(), originalLength.storage(),
			      start.storage(), inc.storage(), index);

    return begin[i];
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
    for (Int j=0; j < ndim(); j++) {
	if (b(j) < start[j] || b(j) > e(j) ||
	    e(j) > start[j] + length[j] - 1 || i(j) < 1)
	    throw(ArrayError("Array<T>::operator()(b,e,i) - b,e or i "
			     "incorrectly specified"));
    }
    Array<T> tmp(*this);
    Int offset = ArrayIndexOffset(ndim(), originalLength.storage(),
				  start.storage(), inc.storage(), b);
    tmp.begin += offset;
    tmp.start.set(0);
    for (j=0; j < ndim(); j++) {
	tmp.inc[j] *= i(j);
	tmp.length[j] = (e(j) - b(j) + i(j))/i(j);
    }
    tmp.nels = ArrayVolume(tmp.ndimen, tmp.length.storage());
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

    if (ndim() != i.nelements())
	throw(ArrayNDimError(ndim(), i.nelements(),
			     "Array<T>::validateIndex - ndims of index"
			     " and array differ"));

    IPosition s, e; // start end
    s = origin();
    e = end();
    for(Int j=0; j < ndim(); j++)
	if(i(j) < s(j) || i(j) > e(j))
	   throw(ArrayIndexError(i, s, shape()));

    // OK - normal return
}

template<class T> Bool Array<T>::contiguousStorage() const
{
    const Int nd = ndim();

    if (nd == 0) {
	return True;
    }

    // If we have increments, we're definitely not contiguous
    for (Int i=0; i < nd; i++) {
	if (inc[i] != 1) {
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

    for (i=0; i < nd - 1; i++) {
	if (length[i] != originalLength[i]) {
	    return False;
	}
    }

    // If we've made it here, we are contiguous!
    return True;
}

template<class T> uInt Array<T>::nrefs() const
{
    DebugAssert(ok(), ArrayError);

    return data.nrefs();
}

// This is relatively expensive
template<class T> Bool Array<T>::ok() const
{
    if (ndimen != ndim())
	return False;
    // We don't check for exact equality because sometimes for efficiency
    // the dimensionality of start et al can be greater than that which is
    // required (e.g. when making a slice.
    if (start.nelements() < ndim())
	return False;
    if (length.nelements() < ndim())
        return False;
    if (inc.nelements() < ndim())
        return False;
    if (originalLength.nelements() < ndim())
	return False;

    Int i;
    uInt count = 1;

    for (i=0; i < ndim(); i++) {
	if (length[i] < 0 || inc[i] < 1 || originalLength[i] < length[i])
	    return False;
	count *= length[i];
    }
    if (ndim() == 0)
	count = 0;
    if (count != nelements())
	return False;
    if (nelements() > 0 && (begin == 0 || data.null()))
	return False;
    // This test likely isn't portable
    if (data->storage() > begin) {
	return False;
    }
    // This test likely isn't portable
    if (begin > data->storage() + data->nelements()*sizeof(T)) {
	return False;
    }

    return True;
}

template<class T> IPosition Array<T>::origin() const
{
    DebugAssert(ok(), ArrayError);

    IPosition tmp(ndim());
    for (Int i=0; i < ndim(); i++)
	tmp(i) = start[i];

    return tmp;
}

template<class T> IPosition Array<T>::shape() const
{
    DebugAssert(ok(), ArrayError);

    IPosition tmp(ndim());
    for (Int i=0; i < ndim(); i++)
	tmp(i) = length[i];

    return tmp;
}

template<class T> IPosition Array<T>::end() const
{
    DebugAssert(ok(), ArrayError);

    IPosition tmp(ndim());
    for (Int i=0; i < ndim(); i++)
	tmp(i) = start[i] + length[i] - 1;

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
	return begin;

    // OK, we are unlucky so we need to do a copy
    T *storage = new T[nelements()];
    if (storage == 0)
	throw(ArrayError("Array<T>::getStorage - new of copy buffer fails"));

    // ok - copy it
    if (ndim() == 1) {
	objcopy(storage, begin, uInt(length[0]), 1U, uInt(inc[0]));
    } else {
	ArrayPositionIterator ai(this->shape(), this->origin(), 1);
	uInt offset;
	IPosition index(ndim());
	uInt count=0;
	while (! ai.pastEnd()) {
	    index = ai.pos();
	    offset = ArrayIndexOffset(ndim(), originalLength.storage(),
				      start.storage(), inc.storage(), index);
	    objcopy(storage + count*length[0], begin+offset, uInt(length[0]),
		    1U, uInt(inc[0]));
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
	objcopy(begin, storage, uInt(length[0]), uInt(inc[0]), 1U);
    } else {
	ArrayPositionIterator ai(this->shape(), this->origin(), 1);
	uInt offset;
	IPosition index(ndim());
	uInt count=0;
	while (! ai.pastEnd()) {
	    index = ai.pos();
	    offset = ArrayIndexOffset(ndim(), originalLength.storage(),
				      start.storage(), inc.storage(), index);
	    objcopy(begin+offset, storage+count*length[0], uInt(length[0]),
		    uInt(inc[0]), 1U);
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
        start.resize(new_ndimen);
	length.resize(new_ndimen);
	inc.resize(new_ndimen);
    }

    start.set(0);
    inc.set(1);
    for (uInt i=0; i < new_ndimen; i++) {
        length[i] = shape(i);
    }
    originalLength = length;

    switch(policy) {
    case COPY:
      if (data.null() || data.nrefs() > 1 || data->nelements() != new_nels) {
	data = new Block<T>(new_nels);
      }
      objcopy(data->storage(), storage, new_nels);
      break;
    case TAKE_OVER:
    case SHARE:
      if (data.null() || data.nrefs() > 1) {
	data = new Block<T>(0);
      }
      data->replaceStorage(new_nels, storage, ToBool(policy == TAKE_OVER));
      break;
    default:
      throw(AipsError("Array<T>::takeStorage - unknown policy"));}
      ;
    begin = data->storage();
    nels = new_nels;
    ndimen = new_ndimen;
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


