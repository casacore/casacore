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
//#        internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#include "ArrayBase.h"
#include "ArrayError.h"

#include <cassert>
#include <sstream>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

ArrayBase::ArrayBase() noexcept
: nels_p       (0),
  ndimen_p     (0),
  contiguous_p (true)
{}

// <thrown>
//   <item> ArrayShapeError
// </thrown>
ArrayBase::ArrayBase (const IPosition& Shape)
: nels_p           (Shape.product()),
  ndimen_p         (Shape.nelements()),
  contiguous_p     (true),
  length_p         (Shape),
  inc_p            (Shape.nelements(), 1),
  originalLength_p (Shape)
{
  for (size_t i = 0; i < ndimen_p; i++) {
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

ArrayBase::ArrayBase (ArrayBase&& source) noexcept
: nels_p           (source.nels_p),
  ndimen_p         (source.ndimen_p),
  contiguous_p     (source.contiguous_p),
  length_p         (source.length_p),
  inc_p            (source.inc_p),
  originalLength_p (source.originalLength_p),
  steps_p          (source.steps_p)
{
  // Make source empty (self-assignment of move object is not allowed)
  source.nels_p = 0;
  source.ndimen_p = 0;
  source.contiguous_p = true;
  source.length_p = IPosition();
  source.inc_p = IPosition();
  source.originalLength_p = IPosition();
  source.steps_p = IPosition();
}

ArrayBase::ArrayBase (ArrayBase&& source, const IPosition& shapeForSource) noexcept
: nels_p           (source.nels_p),
  ndimen_p         (source.ndimen_p),
  contiguous_p     (source.contiguous_p),
  length_p         (source.length_p),
  inc_p            (source.inc_p),
  originalLength_p (source.originalLength_p),
  steps_p          (source.steps_p)
{
  // Set source to have given shape
  source.nels_p = shapeForSource.product();
  source.ndimen_p = shapeForSource.nelements();
  source.contiguous_p = true;
  source.length_p = shapeForSource;
  source.inc_p = IPosition(shapeForSource.nelements(), 1);
  source.originalLength_p = shapeForSource;
  source.steps_p = IPosition();
}

ArrayBase& ArrayBase::assign (const ArrayBase& other)
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

ArrayBase& ArrayBase::operator=(ArrayBase&& source) noexcept
{
  nels_p = source.nels_p;
  ndimen_p = source.ndimen_p;
  contiguous_p = source.contiguous_p;
  length_p = std::move(source.length_p);
  inc_p = std::move(source.inc_p);
  originalLength_p = std::move(source.originalLength_p);
  steps_p = std::move(source.steps_p);
  
  // Make source empty (self-assignment of move object is not allowed)
  source.nels_p = 0;
  source.ndimen_p = 0;
  source.contiguous_p = true;
  source.length_p = IPosition();
  source.inc_p = IPosition();
  source.originalLength_p = IPosition();
  source.steps_p = IPosition();
  
  return *this;
}

ArrayBase::~ArrayBase() noexcept
{}

void ArrayBase::swap(ArrayBase& source) noexcept
{
  std::swap(nels_p, source.nels_p);
  std::swap(ndimen_p, source.ndimen_p);
  std::swap(contiguous_p, source.contiguous_p);
  std::swap(length_p, source.length_p);
  std::swap(inc_p, source.inc_p);
  std::swap(originalLength_p, source.originalLength_p);
  std::swap(steps_p, source.steps_p);
}

void ArrayBase::baseReform (ArrayBase& tmp, const IPosition& len, bool strict) const
{
  // Check if reform can be done.
  long long prod = len.nelements()==0 ? 0 : len.product();
  if (strict && prod != (long long)(nelements())) {
    throw(ArrayConformanceError("ArrayBase::reform() - "
          "total elements differ: " + to_string(len) + " vs " + to_string(shape())));
  }
  // Return if the new shape equals the current one.
  if (len.isEqual(length_p)) {
    return;
  }
  size_t newNdim = len.nelements();
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
    tmp.nels_p = len.product();
    tmp.baseMakeSteps();
    return;
  }
  // A reform of a non-contiguous array has to be done.
  // This is only possible if axes with length 1 are left out and/or added.
  bool valid = true;
  size_t oldPos=0;
  size_t newPos=0;
  int oldLen = length_p(0);
  int newLen = len(0);
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
      valid = false;
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
  size_t startAxis = 0;
  for (size_t i=0; i<newNdim; i++) {
    if (copyAxes(i) >= 0) {
      tmp.inc_p(i) = inc_p(copyAxes(i));
      tmp.originalLength_p(i) = originalLength_p(copyAxes(i));
      for (int j=startAxis; j<copyAxes(i); j++) {
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
  assert (this != &other);
  assert (other.ndim() > 0);
  // These data members are the same irrespective of the degenerate axes. 
  nels_p       = other.nels_p;
  contiguous_p = other.contiguous_p;
  // To remove degenerate axes use two passes - first find out how many axes
  // have to be kept.
  size_t i;
  size_t nd = other.ndim();
  // First determine which axes have to be ignored, thus always be kept.
  // Do not count here, because in theory ignoreAxes can contain the
  // same axis more than once.
  IPosition keepAxes(nd, 0);
  for (i=0; i<ignoreAxes.nelements(); i++) {
    assert (ignoreAxes(i) < int(nd));
    keepAxes(ignoreAxes(i)) = 1;
  }
  // Now count all axes to keep.
  size_t count=0;
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
    length_p.resize(1, false);
    length_p(0) = other.length_p(0);
    inc_p.resize(1, false);
    inc_p(0) = other.inc_p(0);
    originalLength_p.resize(1, false);
    originalLength_p(0) = other.originalLength_p(0);
  } else {
    ndimen_p = count;
    length_p.resize(count, false);
    inc_p.resize(count, false);
    originalLength_p.resize(count, false);
    // Maybe we have no axes to remove
    if (count == other.ndim()){
      length_p = other.length_p;
      originalLength_p = other.originalLength_p;
      inc_p = other.inc_p;
    } else {
      // OK, we have some axes to remove
      size_t skippedVolume = 1;
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

void ArrayBase::baseAddDegenerate (ArrayBase& tmp, size_t numAxes)
{
  const size_t newDim = ndim() + numAxes;
  IPosition newLength(newDim), newInc(newDim), newOriginal(newDim);
  size_t i;
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

bool
ArrayBase::reformOrResize (const IPosition & newShape,
                           bool resizeIfNeeded,
			   size_t nReferences,
			   long long nElementsAllocated,
                           bool copyDataIfNeeded,
                           size_t resizePercentage)
{
    assert(ok());

    if (newShape.isEqual (shape())){
        return false; // No op
    }

    // Check to see if the operation is legal in this context
    // ======================================================

    // The operation must not change the dimensionality as this could cause a base class
    // such as a vector to become a Matrix, etc.

    if (newShape.size() != shape().size()){
        std::string message = "ArrayBase::reformOrResize() - Cannot change number of dimensions.";
	throw ArrayConformanceError (message);
    }

    // This operation only makes sense if the storage is contiguous.

    if (! contiguousStorage()){
        std::string message = "ArrayBase::reformOrResize() - array must be contiguous";
        throw ArrayConformanceError(message);
    }

    // If the array is sharing storage, then the other array could become dangerously invalid
    // as the result of this operation, so prohibit sharing while this operation is being
    // performed.  

    if (nReferences != 1){
        std::string message = "ArrayBase::reformOrResize() - array must not be shared during this call";
        throw ArrayConformanceError(message);
    }

    bool resizeNeeded = (newShape.product() > nElementsAllocated);

    if (resizeNeeded && ! resizeIfNeeded){

	// User did not permit resizing but it is required so throw and exception.

	std::string message = "ArrayBase::reformOrResize() - insufficient storage for reform: "
			    "nElementInAllocation=" + std::to_string(nElementsAllocated) +
          ", nElementsRequested=" + std::to_string(newShape.product());
	throw ArrayConformanceError(message);
    }

    // The operation is legal, so perform it
    // =====================================

    bool resetEnd = true; // Caller will need to reset the end iterator

    if (resizeNeeded){

        // Insufficient storage so resize required, with or without padding

        if (resizePercentage <= 0){

            // Perform an exact resize

            resize (newShape, copyDataIfNeeded);
	    resetEnd = false;

        } else {

            // Padding was requested so resize to match the padded shape
            // and then reform it to use the desired shape.

            IPosition paddedShape;
            paddedShape = newShape;
            paddedShape.last() = (paddedShape.last() * (100 + resizePercentage)) / 100;
            resize (paddedShape, copyDataIfNeeded);

            // Reform it

            baseReform (* this, newShape, false);
        }
    } else {

        baseReform (* this, newShape, false);
    }

    return resetEnd;
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
      std::ostringstream os;
      os << "ArrayBase::operator()(b,e,i) - ndim() b: " << b.nelements()
         << " e: " << e.nelements() << " i: "
         << i.nelements() << " differs from the array ndim " << ndim();
      throw(ArrayError(os.str()));
  }
  size_t j;
  for (j=0; j < ndim(); j++) {
    if (b(j) < 0 || b(j) > e(j)+1
    ||  e(j) >= length_p(j)  ||  i(j) < 1) {
      std::ostringstream os;
      os << "ArrayBase::operator()(b,e,i) - incorrectly specified\n";
      os << "begin: " << b << '\n';
      os << "end:   " << e << '\n';
      os << "incr:  " << i << '\n';
      os << '\n';
      os << "array shape: " << length_p << '\n';
      os << "required: b >= 0; b <= e; e < shape; i >= 0" << '\n';
      throw(ArrayError(os.str()));
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

size_t ArrayBase::makeDiagonal (size_t firstAxis, long long diag)
{
  assert (firstAxis+1 < ndimen_p);
  if (length_p[firstAxis] != length_p[firstAxis+1]) {
    throw ArrayConformanceError("ArrayBase::diagonal() - "
                                "non-square matrix");
  }
  if (std::abs(diag) >= length_p[firstAxis])
    throw ArrayConformanceError("ArrayBase::diagonal() - "
                                "diagonal out of range");
  ///  cout<<length_p<<inc_p<<originalLength_p<<steps_p<<firstAxis<<'\n';
  // Remove the first axis to use.
  ndimen_p--;
  // Set originalLength and stride to both axes.
  // Stride (in original array) is basically length+1.
  ///  inc_p[firstAxis] *= inc_p[firstAxis+1];
  inc_p[firstAxis] += (inc_p[firstAxis+1] * originalLength_p[firstAxis]);
  originalLength_p[firstAxis] *= originalLength_p[firstAxis+1];
  for (size_t i=firstAxis+1; i<ndimen_p; ++i) {
    length_p[i] = length_p[i+1];
    inc_p[i] = inc_p[i+1];
    originalLength_p[i] = originalLength_p[i+1];
  }
  length_p.resize (ndimen_p);
  inc_p.resize (ndimen_p);
  originalLength_p.resize (ndimen_p);
  // An off-diagonal 'diagonal' has a shorter length.
  length_p[firstAxis] -= std::abs(diag);
  nels_p = length_p.product();
  contiguous_p = isStorageContiguous();
  // Determine the offset of the first diagonal element (in original array).
  size_t offs=0;
  if (diag >= 0) {
    offs = diag * steps_p[firstAxis+1];
  } else {
    offs = (-diag) * steps_p[firstAxis];
  }
  baseMakeSteps();
  ///  cout<<length_p<<inc_p<<originalLength_p<<steps_p<<offs<<'\n';
  return offs;
}

// <thrown>
//    <item> ArrayNDimErrror
//    <item> ArrayShapeError
// </thrown>
void ArrayBase::validateConformance (const ArrayBase& other) const
{
  assert(ok());
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
  assert(ok());
  if (ndim() != i.nelements()) {
    throw(ArrayNDimError(ndim(), i.nelements(),
			 "ArrayBase::validateIndex - ndims of index"
			 " and array differ"));
  }
  for (size_t j=0; j < ndim(); j++) {
    if (i(j) < 0  ||  i(j) >= length_p(j)) {
      throw(ArrayIndexError(i, length_p));
    }
  }
  // OK - normal return
}

void ArrayBase::validateIndex (size_t index) const
{
  validateIndex (IPosition(1, index));
}
void ArrayBase::validateIndex (size_t index1, size_t index2) const
{
  IPosition inx(2);
  inx[0] = index1;
  inx[1] = index2;
  validateIndex (inx);
}
void ArrayBase::validateIndex (size_t index1, size_t index2, size_t index3) const
{
  IPosition inx(3);
  inx[0] = index1;
  inx[1] = index2;
  inx[2] = index3;
  validateIndex (inx);
}

bool ArrayBase::copyVectorHelper (const ArrayBase& other)
{
  bool Conform = conform2(other);
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

bool ArrayBase::isStorageContiguous() const
{
  int i;
  int nd = ndim();
  if (nd == 0) {
    return true;
  }
  // If we have increments, we're definitely not contiguous (unless the axis
  // length is one!)
  for (i=0; i < nd; i++) {
    if ((inc_p(i) != 1) && (length_p(i) != 1)) {
      return false;
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
      return false;
    }
  }
  // If we've made it here, we are contiguous!
  return true;
}

void ArrayBase::baseMakeSteps()
{
  // No Assert since the Array may not be constructed yet when
  // calling this.
  steps_p.resize (ndimen_p);
  int size = 1;
  for (size_t i=0; i<inc_p.nelements(); i++) {
    steps_p(i) = inc_p(i) * size;
    size *= originalLength_p(i);
  }
}

IPosition ArrayBase::endPosition() const
{
  assert(ok());
  IPosition tmp(ndim());
  for (size_t i=0; i < ndim(); i++) {
    tmp(i) = length_p(i) - 1;
  }
  return tmp;
}

bool ArrayBase::ok() const
{
  assert(ndimen_p == ndim());
  assert(length_p.nelements() == ndim());
  assert(inc_p.nelements() == ndim());
  assert(originalLength_p.nelements() == ndim());
  
  if (ndimen_p != ndim()) {
    return false;
  }
  // We don't check for exact equality because sometimes for efficiency
  // the dimensionality of start et al can be greater than that which is
  // required (e.g. when making a slice.
  if (length_p.nelements() != ndim()) {
    return false;
  }
  if (inc_p.nelements() != ndim()) {
    return false;
  }
  if (originalLength_p.nelements() != ndim()) {
    return false;
  }
  size_t i;
  size_t count = 1;
  IPosition pos(ndimen_p, 0);
  for (i=0; i < ndim(); i++) {
    assert(length_p(i) >= 0);
    assert(inc_p(i) >= 1);
    assert(originalLength_p(i) >= length_p(i));
    if (length_p(i) < 0  ||  inc_p(i) < 1
    ||  originalLength_p(i) < length_p(i)) {
      return false;
    }
    count *= length_p(i);
    if (length_p(i) > 1) {
      pos(i) = 1;
      size_t off = ArrayIndexOffset(ndim(), originalLength_p.storage(),
                                    inc_p.storage(), pos);
      pos(i) = 0;
      assert(size_t(steps_p(i)) == off);
      if (size_t(steps_p(i)) != off) {
	return false;
      }
    }
  }
  if (ndim() == 0) {
    count = 0;
  }
  assert(count == nelements());
  if (count != nelements()) {
    return false;
  }
  assert(contiguous_p == isStorageContiguous());
  if (contiguous_p != isStorageContiguous()) {
    return false;
  }
  return true;
}


void ArrayBase::checkVectorShape()
{
  if (ndim() != 1) {
    // Check if all elements are 1 or nels_p. In this way we are sure that
    // only one axis remains (i.e. at most one axis has length > 1).
    // Keep original increment and length of the remaining axis.
    int inc   = 1;
    int orLen = 1;
    int skippedVolume = 1;
    for (size_t i=0; i<ndim(); ++i) {
      if (length_p[i] == 1) {
	skippedVolume *= originalLength_p(i);
      } else {
	if (length_p[i] != int(nels_p)) {
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
    if (ndim() == 0) {
      length_p(0) = 0;
      length_p(1) = 0;
      inc_p(0) = 1;
      inc_p(1) = 1;
      originalLength_p(0) = 0;
      originalLength_p(1) = 0;
    }
    else {
      length_p(1) = (nelements() == 0) ? 0 : 1;
      originalLength_p(1) = length_p(1);
      inc_p(1) = 1;
    }
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
    int len = (nelements()==0) ? 0 : 1;
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

std::unique_ptr<ArrayBase> ArrayBase::makeArray() const
{
  throw ArrayError ("ArrayBase::makeArray cannot be used");
}
void ArrayBase::resize(const IPosition&, bool)
{
  throw ArrayError ("ArrayBase::resize cannot be used");
}
std::unique_ptr<ArrayPositionIterator> ArrayBase::makeIterator (size_t) const
{
  throw ArrayError ("ArrayBase::makeIterator cannot be used");
}
std::unique_ptr<ArrayBase> ArrayBase::getSection (const Slicer&) const
{
  throw ArrayError ("ArrayBase::getSection cannot be used");
}
void ArrayBase::assignBase (const ArrayBase&, bool)
{
  throw ArrayError ("ArrayBase::assign cannot be used");
}
void* ArrayBase::getVStorage (bool&)
{
  throw ArrayError ("ArrayBase::getVStorage cannot be used");
}
const void* ArrayBase::getVStorage (bool&) const
{
  throw ArrayError ("ArrayBase::getVStorage cannot be used");
}
void ArrayBase::putVStorage(void*&, bool)
{
  throw ArrayError ("ArrayBase::putVStorage cannot be used");
}
void ArrayBase::freeVStorage(const void*&, bool) const
{
  throw ArrayError ("ArrayBase::freeVStorage cannot be used");
}

void throwArrayShapes (const IPosition& shape1,
                       const IPosition& shape2,
                       const char* name)
{
  throw ArrayConformanceError ("ArrayMath/Logical function " + std::string(name) +
                               ": array shapes " + shape1.toString() +
                               " and " + shape2.toString() + " differ");
}

} //# NAMESPACE CASACORE - END

