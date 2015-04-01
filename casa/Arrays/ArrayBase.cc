//# ArrayBase.cc: Non-templated base class for templated Array class
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

#include <casacore/casa/Arrays/ArrayBase.h>
#include <casacore/casa/Arrays/ArrayError.h>
#include <casacore/casa/Utilities/Assert.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

ArrayBase::ArrayBase()
: nels_p       (0),
  ndimen_p     (0),
  contiguous_p (True)
{}

// <thrown>
//   <item> ArrayShapeError
// </thrown>
ArrayBase::ArrayBase (const IPosition& Shape)
: nels_p           (Shape.product()),
  ndimen_p         (Shape.nelements()),
  contiguous_p     (True),
  length_p         (Shape),
  inc_p            (Shape.nelements(), 1),
  originalLength_p (Shape)
{
  for (uInt i = 0; i < ndimen_p; i++) {
    if (Shape(i) < 0) {
      throw(ArrayShapeError(shape(), Shape,
			    "ArrayBase::Array(const IPosition&)"
			    " - Negative shape"));
    }
  }
  baseMakeSteps();
}

ArrayBase::ArrayBase (const ArrayBase& other)
: nels_p           (other.nels_p),
  ndimen_p         (other.ndimen_p),
  contiguous_p     (other.contiguous_p),
  length_p         (other.length_p),
  inc_p            (other.inc_p),
  originalLength_p (other.originalLength_p),
  steps_p          (other.steps_p)
{}

ArrayBase& ArrayBase::operator= (const ArrayBase& other)
{
  if (this != &other) {
    nels_p           = other.nels_p;
    ndimen_p         = other.ndimen_p;
    contiguous_p     = other.contiguous_p;
    if (ndimen_p != length_p.nelements()) {
      length_p.resize (ndimen_p);
      inc_p.resize (ndimen_p);
      originalLength_p.resize (ndimen_p);
      steps_p.resize (ndimen_p);
    }
    length_p         = other.length_p;
    inc_p            = other.inc_p;
    originalLength_p = other.originalLength_p;
    steps_p          = other.steps_p;
  }
  return *this;
}

ArrayBase::~ArrayBase()
{}

void ArrayBase::baseReform (ArrayBase& tmp, const IPosition& len) const
{
  // Check if reform can be done.
  if (len.product() != Int64(nelements())) {
    throw(ArrayConformanceError("ArrayBase::reform() - "
				"total elements differ"));
  }
  // Return if the new shape equals the current one.
  if (len.isEqual(length_p)) {
    return;
  }
  uInt newNdim = len.nelements();
  // If the data is contiguous, a reform can simply be done
  // by inserting the new shape.
  if (contiguousStorage()) {
    tmp.ndimen_p = newNdim;
    tmp.length_p.resize (newNdim);
    tmp.length_p = len;
    tmp.inc_p.resize (newNdim);
    tmp.inc_p = 1;
    tmp.originalLength_p.resize (newNdim);
    tmp.originalLength_p = tmp.length_p;
    tmp.baseMakeSteps();
    return;
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
    throw(ArrayConformanceError("ArrayBase::reform() - "
				"data not contiguous nor similarly shaped"));
  }
  // Great, the shapes match. Adjust the IPositions.
  // Set inc and originalLength initially to 1 (caters for added axes).
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
  tmp.baseMakeSteps();
}


void ArrayBase::baseNonDegenerate (const ArrayBase& other,
				   const IPosition& ignoreAxes)
{
  AlwaysAssert (this != &other, AipsError);
  AlwaysAssert(other.ndim() > 0, AipsError);
  // These data members are the same irrespective of the degenerate axes. 
  nels_p       = other.nels_p;
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
  } else {
    ndimen_p = count;
    length_p.resize(count, False);
    inc_p.resize(count, False);
    originalLength_p.resize(count, False);
    // Maybe we have no axes to remove
    if (count == other.ndim()){
      length_p = other.length_p;
      originalLength_p = other.originalLength_p;
      inc_p = other.inc_p;
    } else {
      // OK, we have some axes to remove
      uInt skippedVolume = 1;
      count = 0;
      for (i=0; i<nd; i++) {
	if (keepAxes(i) == 1) {
	  length_p(count) = other.length_p(i);
	  originalLength_p(count) =
	    skippedVolume*other.originalLength_p(i);
	  inc_p(count) = skippedVolume*other.inc_p(i);
	  skippedVolume = 1;
	  count++;
	}else{
	  skippedVolume *= other.originalLength_p(i);
	}
      }
    }
  }
  baseMakeSteps();
}

void ArrayBase::baseAddDegenerate (ArrayBase& tmp, uInt numAxes)
{
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
  tmp.baseMakeSteps();
}


// <thrown>
//     <item> ArrayError
// </thrown>
size_t ArrayBase::makeSubset (ArrayBase& out,
                              const IPosition& b,
                              const IPosition& e,
                              const IPosition& i)
{
  if (b.nelements() != ndim() || e.nelements() != ndim() ||
      i.nelements() != ndim()) {
    throw(ArrayError("ArrayBase::operator()(b,e,i) - ndim() differs from"
		     " the array ndim"));
  }
  uInt j;
  for (j=0; j < ndim(); j++) {
    if (b(j) < 0 || b(j) > e(j)+1
    ||  e(j) >= length_p(j)  ||  i(j) < 1) {
      throw(ArrayError("ArrayBase::operator()(b,e,i) - b,e or i "
		       "incorrectly specified"));
    }
  }
  size_t offs=0;
  for (j=0; j<ndimen_p; j++) {
    offs += b(j) * steps_p(j);
  }
  for (j=0; j < ndim(); j++) {
    out.inc_p(j) *= i(j);
    out.length_p(j) = (e(j) - b(j) + i(j))/i(j);
  }
  out.nels_p = out.length_p.product();
  out.contiguous_p = out.isStorageContiguous();
  out.baseMakeSteps();
  return offs;
}


// <thrown>
//    <item> ArrayNDimErrror
//    <item> ArrayShapeError
// </thrown>
void ArrayBase::validateConformance (const ArrayBase& other) const
{
  DebugAssert(ok(), ArrayError);
  if (! conform2(other)) {
    if (ndim() != other.ndim()) {
      throw(ArrayNDimError(ndim(), other.ndim(),
			   "ArrayBase::validateConformance"));
    } else {
      throw(ArrayShapeError(shape(), other.shape(),
			    "ArrayBase::validateConformance"));
    }
  }
}

// <thrown>
//    <item> ArrayNDimErrror
//    <item> ArrayIndexError
// </thrown>
void ArrayBase::validateIndex (const IPosition& i) const
{
  DebugAssert(ok(), ArrayError);
  if (ndim() != i.nelements()) {
    throw(ArrayNDimError(ndim(), i.nelements(),
			 "ArrayBase::validateIndex - ndims of index"
			 " and array differ"));
  }
  for (uInt j=0; j < ndim(); j++) {
    if (i(j) < 0  ||  i(j) >= length_p(j)) {
      throw(ArrayIndexError(i, length_p));
    }
  }
  // OK - normal return
}

void ArrayBase::validateIndex (uInt index) const
{
  validateIndex (IPosition(1, index));
}
void ArrayBase::validateIndex (uInt index1, uInt index2) const
{
  IPosition inx(2);
  inx[0] = index1;
  inx[1] = index2;
  validateIndex (inx);
}
void ArrayBase::validateIndex (uInt index1, uInt index2, uInt index3) const
{
  IPosition inx(3);
  inx[0] = index1;
  inx[1] = index2;
  inx[2] = index3;
  validateIndex (inx);
}

void ArrayBase::throwNdimVector()
{
  throw ArrayError ("Expected dimensionality 1 for a Vector<T> object");
}

Bool ArrayBase::copyVectorHelper (const ArrayBase& other)
{
  Bool Conform = conform2(other);
  if (!Conform  &&  length_p(0) != 0) {
    validateConformance(other);  // We can't overwrite, so throw exception
  }
  if (!Conform) { // copy in place
    length_p         = other.length_p;
    nels_p           = other.nels_p;
    originalLength_p = length_p;
    baseMakeSteps();
  }
  return Conform;
}

Bool ArrayBase::isStorageContiguous() const
{
  Int i;
  Int nd = ndim();
  if (nd == 0) {
    return True;
  }
  // If we have increments, we're definitely not contiguous (unless the axis
  // length is one!)
  for (i=0; i < nd; i++) {
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

void ArrayBase::baseMakeSteps()
{
  // No Assert since the Array may not be constructed yet when
  // calling this.
  steps_p.resize (ndimen_p);
  Int size = 1;
  for (uInt i=0; i<inc_p.nelements(); i++) {
    steps_p(i) = inc_p(i) * size;
    size *= originalLength_p(i);
  }
}

IPosition ArrayBase::endPosition() const
{
  DebugAssert(ok(), ArrayError);
  IPosition tmp(ndim());
  for (uInt i=0; i < ndim(); i++) {
    tmp(i) = length_p(i) - 1;
  }
  return tmp;
}

Bool ArrayBase::ok() const
{
  if (ndimen_p != ndim()) {
    return False;
  }
  // We don't check for exact equality because sometimes for efficiency
  // the dimensionality of start et al can be greater than that which is
  // required (e.g. when making a slice.
  if (length_p.nelements() != ndim()) {
    return False;
  }
  if (inc_p.nelements() != ndim()) {
    return False;
  }
  if (originalLength_p.nelements() != ndim()) {
    return False;
  }
  uInt i;
  size_t count = 1;
  IPosition pos(ndimen_p, 0);
  for (i=0; i < ndim(); i++) {
    if (length_p(i) < 0  ||  inc_p(i) < 1
    ||  originalLength_p(i) < length_p(i)) {
      return False;
    }
    count *= length_p(i);
    if (length_p(i) > 1) {
      pos(i) = 1;
      size_t off = ArrayIndexOffset(ndim(), originalLength_p.storage(),
                                    inc_p.storage(), pos);
      pos(i) = 0;
      if (size_t(steps_p(i)) != off) {
	return False;
      }
    }
  }
  if (ndim() == 0) {
    count = 0;
  }
  if (count != nelements()) {
    return False;
  }
  if (contiguous_p != isStorageContiguous()) {
    return False;
  }
  return True;
}


void ArrayBase::checkVectorShape()
{
  if (ndim() != 1) {
    // Check if all elements are 1 or nels_p. In this way we are sure that
    // only one axis remains (i.e. at most one axis has length > 1).
    // Keep original increment and length of the remaining axis.
    Int inc   = 1;
    Int orLen = 1;
    Int skippedVolume = 1;
    for (uInt i=0; i<ndim(); ++i) {
      if (length_p[i] == 1) {
	skippedVolume *= originalLength_p(i);
      } else {
	if (length_p[i] != Int(nels_p)) {
	  throw(ArrayNDimError(1, ndim(),
			       "Vector<T>: ndim of other array > 1"));
	}
	inc = inc_p(i) * skippedVolume;
	orLen = originalLength_p(i) * skippedVolume;
	break;
      }
    }
    ndimen_p = 1;
    length_p.resize(1); 
    inc_p.resize(1);
    originalLength_p.resize(1);
    steps_p.resize(1);
    length_p(0) = nels_p;
    inc_p(0) = inc;
    originalLength_p(0) = orLen;
    steps_p(0) = inc;
  }
}

void ArrayBase::checkMatrixShape()
{
  if (ndim() > 2) {
    throw(ArrayNDimError(2, ndim(),
			 "Matrix<T>: ndim of other array > 2"));
  }
  if (ndim() < 2) {
    // We need to fiddle a bit if ndim < 2.
    length_p.resize(2); 
    inc_p.resize(2);
    originalLength_p.resize(2);
    int len = 1;
    if (ndim() == 0) {
      len = 0;
      length_p(0) = 0;
      inc_p(0) = 1;
      originalLength_p(0) = 0;
    }
    length_p(1) = len;
    inc_p(1) = 1;
    originalLength_p(1) = len;
    ndimen_p = 2;
    baseMakeSteps();
  }
}

void ArrayBase::checkCubeShape()
{
  if (ndim() > 3) {
    throw(ArrayNDimError(3, ndim(),
			 "Cube<T>: ndim of other array > 3"));
  }
  // We need to fiddle a bit if ndim < 3.
  if (ndim() < 3) {
    length_p.resize(3); 
    inc_p.resize(3);
    originalLength_p.resize(3);
    int len = 1;
    if (ndim() == 0) {
      len = 0;
      length_p(0) = 0;
      inc_p(0) = 1;
      originalLength_p(0) = 0;
    }
    if (ndim() < 2) {
      length_p(1) = len;
      inc_p(1) = 1;
      originalLength_p(1) = len;
    }
    length_p(2) = len;
    inc_p(2) = 1;
    originalLength_p(2) = len;
    ndimen_p = 3;
    baseMakeSteps();
  }
}

CountedPtr<ArrayBase> ArrayBase::makeArray() const
{
  throw ArrayError ("ArrayBase::makeArray cannot be used");
}
void ArrayBase::resize(const IPosition&, Bool)
{
  throw ArrayError ("ArrayBase::resize cannot be used");
}
CountedPtr<ArrayPositionIterator> ArrayBase::makeIterator (uInt) const
{
  throw ArrayError ("ArrayBase::makeIterator cannot be used");
}
CountedPtr<ArrayBase> ArrayBase::getSection (const Slicer&) const
{
  throw ArrayError ("ArrayBase::getSection cannot be used");
}
void ArrayBase::assignBase (const ArrayBase&, Bool)
{
  throw ArrayError ("ArrayBase::assign cannot be used");
}
void* ArrayBase::getVStorage (Bool&)
{
  throw ArrayError ("ArrayBase::getVStorage cannot be used");
}
const void* ArrayBase::getVStorage (Bool&) const
{
  throw ArrayError ("ArrayBase::getVStorage cannot be used");
}
  void ArrayBase::putVStorage(void*&, Bool)
{
  throw ArrayError ("ArrayBase::putVStorage cannot be used");
}
void ArrayBase::freeVStorage(const void*&, Bool) const
{
  throw ArrayError ("ArrayBase::freeVStorage cannot be used");
}


} //# NAMESPACE CASACORE - END

