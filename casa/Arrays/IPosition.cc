//# IPosition.cc: A vector of integers, used to index into arrays.
//# Copyright (C) 1994,1995,1996,1997,1998,1999,2000,2001,2002
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

#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/ArrayError.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/sstream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

IPosition::IPosition (uInt length)
: size_p (length),
  data_p (buffer_p)
{
    if (length > BufferLength) {
	allocateBuffer();
    }
}

IPosition::~IPosition()
{
    if (data_p != &buffer_p[0]) {
        delete [] data_p;
    }
}

void IPosition::allocateBuffer()
{
    if (size_p <= BufferLength) {
        data_p = buffer_p;
    } else {
	data_p = new ssize_t[size_p];
    }
    DebugAssert(ok(), AipsError);
}

IPosition::IPosition (uInt length, ssize_t val)
: size_p (length),
  data_p (buffer_p)
{
    if (size_p > BufferLength) {
	allocateBuffer();
    }
    for (uInt i=0; i<size_p; i++) {
	data_p[i] = val;
    }
}

// <thrown>
//    <item> AipsError
// </thrown>
IPosition::IPosition (uInt length, ssize_t val0, ssize_t val1, ssize_t val2,
                      ssize_t val3, ssize_t val4, ssize_t val5, ssize_t val6,
                      ssize_t val7, ssize_t val8, ssize_t val9)
: size_p (length),
  data_p (buffer_p)
{
    if (size_p > BufferLength) {
	allocateBuffer();
    }
    if (size_p > 10  ||  length < 1) {
	throw(AipsError("IPosition::IPosition(uInt length, val0, ...) - "
			 "Can only initialize from 1 to 10 elements"));
    }
    switch (length) {
         case 10: data_p[9] = val9;    // Fall through
	 case 9:  data_p[8] = val8;    // Fall through
	 case 8:  data_p[7] = val7;    // Fall through
	 case 7:  data_p[6] = val6;    // Fall through
	 case 6:  data_p[5] = val5;    // Fall through
	 case 5:  data_p[4] = val4;    // Fall through
	 case 4:  data_p[3] = val3;    // Fall through
	 case 3:  data_p[2] = val2;    // Fall through
	 case 2:  data_p[1] = val1;    // Fall through
	 case 1:  data_p[0] = val0; break;
	 default:
             throw(AipsError("IPosition::IPosition(uInt length, val0, ...) - "
			     "Can only initialize from 1 to 10 elements"));
    }
    for (uInt i=0; i<size_p; i++) {
	if (data_p[i] == MIN_INT) {
	    throw(AipsError("IPosition::IPosition(uInt length, val0, ...) - "
		    "One or more valn == INT_MIN. Probably haven't defined "
		    "enough values. Otherwise specify after construction."));
	}
    }
    DebugAssert(ok(), AipsError);
}

IPosition::IPosition (const IPosition& other)
: size_p (other.size_p),
  data_p (buffer_p)
{
    if (size_p > BufferLength) {
	allocateBuffer();
    }
    for (uInt i=0; i<size_p; i++) {
	data_p[i] = other.data_p[i];
    }
    DebugAssert(ok(), AipsError);
}

IPosition IPosition::nonDegenerate (uInt startingAxis) const
{
    if (startingAxis >= size_p) {
        return *this;
    }
    IPosition ignoreAxes(startingAxis);
    for (uInt i=0; i<startingAxis; i++) {
	ignoreAxes(i) = i;
    }
    return nonDegenerate (ignoreAxes);
}
IPosition IPosition::nonDegenerate (const IPosition& ignoreAxes) const
{
    DebugAssert(ok(), AipsError);
    // First determine which axes have to be ignored, thus always be kept.
    // Do not count here, because in theory ignoreAxes can contain the
    // same axis more than once.
    // To remove degenerate axes use two passes - first find out how many axes
    // have to be kept.
    uInt i;
    IPosition keepAxes(size_p, 0);
    for (i=0; i<ignoreAxes.nelements(); i++) {
	AlwaysAssert (ignoreAxes(i) < ssize_t(size_p), AipsError);
	keepAxes(ignoreAxes(i)) = 1;
    }
    // Now count all axes to keep.
    uInt count=0;
    for (i=0; i<size_p; i++) {
	if (keepAxes(i) == 1) {
	    count++;
	}else{
	    if (data_p[i] != 1) {
		keepAxes(i) = 1;
		count++;
	    }
	}
    }
    if (count == size_p) return *this;
    if (count == 0) count = 1;
    IPosition nondegenerateIP(count,1);
    count = 0;
    for (i=0; i<size_p; i++) {
	if (keepAxes(i)) {
	    nondegenerateIP(count++) = data_p[i];    
	}
    }
    return nondegenerateIP;
}

void IPosition::resize (uInt newSize, Bool copy)
{
    DebugAssert(ok(), AipsError);
    // If the size is unchanged, just return (more efficient)
    if (newSize == size_p) {
	return;
    }
    ssize_t* oldData = data_p;
    uInt oldSize = size_p;
    size_p = newSize;
    allocateBuffer();
    if (oldData != data_p  &&  copy) {
	for (uInt i=0; i<min(size_p, oldSize); i++) {
	    data_p[i] = oldData[i];
	}
    }
    // Delete the old data, if any.
    if (oldData != &buffer_p[0]) {
	delete [] oldData;
    }
    DebugAssert(ok(), AipsError);
}

// <thrown>
//    <item> ArrayConformanceError
// </thrown>
IPosition& IPosition::operator= (const IPosition& other)
{
    DebugAssert(ok(), AipsError);
    if (&other == this) {
	return *this;
    }
    if (size_p == 0) {
	this->resize (other.nelements(), False);
    } else if (! conform(other)) {
	throw(ArrayConformanceError("IPosition::operator=(const IPosition&  - "
				    "this and other differ in length"));
    }
    for (uInt i=0; i<size_p; i++) {
	data_p[i] = other.data_p[i];
    }
    DebugAssert(ok(), AipsError);
    return *this;
}

IPosition& IPosition::operator= (ssize_t value)
{
    DebugAssert(ok(), AipsError);
    for (uInt i=0; i<size_p; i++) {
	data_p[i] = value;
    }
    return *this;
}

IPosition IPosition::operator() (const IPosition& axes) const
{
  IPosition ipos(axes.nelements());
  uInt i = 0;
  for (IPosition::const_iterator iter=axes.begin();
       iter!=axes.end(); ++iter, ++i) {
    if (*iter >= Int(size_p)) {
      throw AipsError("IPosition::operator()(const IPosition&): "
                      "Axis number must be less than size of current object");
    }
    ipos[i] = data_p[*iter];
  }
  return ipos;
}

void IPosition::append (const IPosition& other)
{
    uInt j = size_p;
    resize (size_p + other.size_p);
    for (uInt i=0; i<other.size_p; i++) {
	data_p[j++] = other.data_p[i];
    }
}

void IPosition::prepend (const IPosition& other)
{
    uInt i;
    uInt j = size_p;
    resize (size_p + other.size_p);
    for (i=size_p; j>0;) {
	data_p[--i] = data_p[--j];
    }
    for (i=0; i<other.size_p; i++) {
	data_p[i] = other.data_p[i];
    }
}

IPosition IPosition::concatenate (const IPosition& other) const
{
    IPosition tmp (*this);
    tmp.append (other);
    return tmp;
}

void IPosition::setFirst (const IPosition& other)
{
    if (size_p < other.size_p) {
	throw (AipsError ("IPosition::setFirst(other); other is too long"));
    }
    for (uInt i=0; i<other.size_p; i++) {
	data_p[i] = other.data_p[i];
    }
}

void IPosition::setLast (const IPosition& other)
{
    if (size_p < other.size_p) {
	throw (AipsError ("IPosition::setLast(other); other is too long"));
    }
    uInt j = size_p - other.size_p;
    for (uInt i=0; i<other.size_p; i++) {
	data_p[j++] = other.data_p[i];
    }
}

IPosition IPosition::getFirst (uInt n) const
{
    if (size_p < n) {
	throw (AipsError ("IPosition::getFirst(n); n is too high"));
    }
    IPosition tmp(n);
    for (uInt i=0; i<n; i++) {
	tmp.data_p[i] = data_p[i];
    }
    return tmp;
}

IPosition IPosition::getLast (uInt n) const
{
    if (size_p < n) {
	throw (AipsError ("IPosition::getLast(n); n is too high"));
    }
    IPosition tmp(n);
    uInt j = size_p - n;
    for (uInt i=0; i<n; i++) {
	tmp.data_p[i] = data_p[j++];
    }
    return tmp;
}

IPosition IPosition::removeAxes (const IPosition& axes) const
{
  // Get the axes to keep.
  // It also checks if axes are specified correctly.
  IPosition resAxes = IPosition::otherAxes (size_p, axes);
  uInt ndimRes = resAxes.nelements();
  // Create the result shape.
  IPosition resShape(ndimRes);
  if (ndimRes == 0) {
    resShape.resize(1);
    resShape[0] = 1;
  } else {
    for (uInt i=0; i<ndimRes; ++i) {
      resShape[i] = data_p[resAxes[i]];
    }
  }
  return resShape;
}

IPosition IPosition::keepAxes (const IPosition& axes) const
{
  return removeAxes (otherAxes(size_p, axes));
}

// <thrown>
//    <item> ArrayConformanceError
// </thrown>
void IPosition::operator += (const IPosition& other)
{
    DebugAssert(ok(), AipsError);
    if (! conform(other)) {
	throw(ArrayConformanceError("IPosition::operator += "
				    "(const IPosition&) - "
				    "this and other differ in length"));
    }
    for (uInt i=0; i<size_p; i++) {
	data_p[i] += other.data_p[i];
    }
}

// <thrown>
//    <item> ArrayConformanceError
// </thrown>
void IPosition::operator -= (const IPosition& other)
{
    DebugAssert(ok(), AipsError);
    if (! conform(other)) {
	throw(ArrayConformanceError("IPosition::operator -= "
				    "(const IPosition&) - "
				    "this and other differ in length"));
    }
    for (uInt i=0; i<size_p; i++) {
	data_p[i] -= other.data_p[i];
    }
}

// <thrown>
//    <item> ArrayConformanceError
// </thrown>
void IPosition::operator *= (const IPosition& other)
{
    DebugAssert(ok(), AipsError);
    if (! conform(other)) {
	throw(ArrayConformanceError("IPosition::operator *= "
				    "(const IPosition&) - "
				    "this and other differ in length"));
    }
    for (uInt i=0; i<size_p; i++) {
	data_p[i] *= other.data_p[i];
    }
}

// <thrown>
//    <item> ArrayConformanceError
// </thrown>
void IPosition::operator /= (const IPosition& other)
{
    DebugAssert(ok(), AipsError);
    if (! conform(other)) {
	throw(ArrayConformanceError("IPosition::operator /= "
				    "(const IPosition&) - "
				    "this and other differ in length"));
    }
    for (uInt i=0; i<size_p; i++) {
	data_p[i] /= other.data_p[i];
    }
}

void IPosition::operator += (ssize_t val)
{
    DebugAssert(ok(), AipsError);
    for (uInt i=0; i<size_p; i++) {
	data_p[i] += val;
    }
}

void IPosition::operator -= (ssize_t val)
{
    DebugAssert(ok(), AipsError);
    for (uInt i=0; i<size_p; i++) {
	data_p[i] -= val;
    }
}

void IPosition::operator *= (ssize_t val)
{
    DebugAssert(ok(), AipsError);
    for (uInt i=0; i<size_p; i++) {
	data_p[i] *= val;
    }
}

void IPosition::operator /= (ssize_t val)
{
    DebugAssert(ok(), AipsError);
    for (uInt i=0; i<size_p; i++) {
	data_p[i] /= val;
    }
}

Bool IPosition::isEqual (const IPosition& other) const
{
    return isEqual (other, nelements());
}

Bool IPosition::isEqual (const IPosition& other,
			 Bool skipDegeneratedAxes) const
{
    if (!skipDegeneratedAxes) {
	return isEqual (other, nelements());
    }
    uInt nrthis = nelements();
    uInt nrthat = other.nelements();
    uInt i;
    uInt j=0;
    for (i=0; i<nrthis; i++) {
	if (data_p[i] != 1) {
	    while (j < nrthat  &&  other(j) == 1) {
		j++;
	    }
	    if (j >= nrthat) {
		break;
	    }
	    if (data_p[i] != other(j)) {
		return False;
	    }
	    j++;
	}
    }
    for (; i<nrthis; i++) {
	if (data_p[i] != 1) {
	    return False;
	}
    }
    for (; j<nrthat; j++) {
	if (other(j) != 1) {
	    return False;
	}
    }
    return True;
}

Bool IPosition::isEqual (const IPosition& other, uInt nrCompare) const
{
    if (! conform (other)) {
	return False;
    }
    if (nrCompare > nelements()) {
	nrCompare = nelements();
    }
    for (uInt i=0; i<nrCompare; i++) {
	if (data_p[i] != other(i)) {
	    return False;
	}
    }
    return True;
}


Bool IPosition::isSubSet (const IPosition& other) const
{
    uInt nrthis = nelements();
    uInt nrthat = other.nelements();
    uInt j=0;
    for (uInt i=0; i<nrthis; i++) {
      if (j < nrthat) {
	if (other(j) == data_p[i]  ||  other(j) == 1) {
	  j++;
	}
      }
    }
    return (j == nrthat);
}


// <thrown>
//    <item> ArrayConformanceError
// </thrown>
IPosition operator + (const IPosition& left, const IPosition& right)
{
    if (! left.conform(right)) {
	throw(ArrayConformanceError("::operator + "
			"(const IPosition&, const IPosition&) - "
			"left and right operand do not conform "));
    }
    IPosition result(left);
    result += right;
    return result;
}

// <thrown>
//    <item> ArrayConformanceError
// </thrown>
IPosition operator - (const IPosition& left, const IPosition& right)
{
    if (! left.conform(right)) {
	throw(ArrayConformanceError("::operator - "
			"(const IPosition&, const IPosition&) - "
			"left and right operand do not conform "));
    }
    IPosition result(left);
    result -= right;
    return result;
}

// <thrown>
//    <item> ArrayConformanceError
// </thrown>
IPosition operator * (const IPosition& left, const IPosition& right)
{
    if (! left.conform(right)) {
	throw(ArrayConformanceError("::operator * "
			"(const IPosition&, const IPosition&) - "
			"left and right operand do not conform "));
    }
    IPosition result(left);
    result *= right;
    return result;
}

// <thrown>
//    <item> ArrayConformanceError
// </thrown>
IPosition operator / (const IPosition& left, const IPosition& right)
{
    if (! left.conform(right)) {
	throw(ArrayConformanceError("::operator / "
			"(const IPosition&, const IPosition&) - "
			"left and right operand do not conform "));
    }
    IPosition result(left);
    result /= right;
    return result;
}

IPosition operator + (const IPosition& left, ssize_t val)
{
    IPosition result(left);
    result += val;
    return result;
}

IPosition operator - (const IPosition& left, ssize_t val)
{
    IPosition result(left);
    result -= val;
    return result;
}

IPosition operator * (const IPosition& left, ssize_t val)
{
    IPosition result(left);
    result *= val;
    return result;
}

IPosition operator / (const IPosition& left, ssize_t val)
{
    IPosition result(left);
    result /= val;
    return result;
}

IPosition operator + (ssize_t val, const IPosition& right)
{
    IPosition result(right.nelements());
    result = val;
    result += right;
    return result;
}

IPosition operator - (ssize_t val, const IPosition& right)
{
    IPosition result(right.nelements());
    result = val;
    result -= right;
    return result;
}

IPosition operator * (ssize_t val, const IPosition& right)
{
    IPosition result(right.nelements());
    result = val;
    result *= right;
    return result;
}

IPosition operator / (ssize_t val, const IPosition& right)
{
    IPosition result(right.nelements());
    result = val;
    result /= right;
    return result;
}

IPosition max (const IPosition& left, const IPosition& right)
{
    if (! left.conform(right)) {
        throw(ArrayConformanceError("::max "
                        "(const IPosition&, const IPosition&) - "
                        "left and right operand do not conform "));
    }
    IPosition result(left);
    const uInt ndim = result.nelements();
    ssize_t max;
    for (uInt i = 0; i < ndim; i++) {
      if (result(i) < (max = right(i))) {
        result(i) = max;
      }
    }
    return result;
}

// <thrown>
//    <item> ArrayConformanceError
// </thrown>
IPosition min (const IPosition& left, const IPosition& right)
{
    if (! left.conform(right)) {
        throw(ArrayConformanceError("::min "
                        "(const IPosition&, const IPosition&) - "
                        "left and right operand do not conform "));
    }
    IPosition result(left);
    const uInt ndim = result.nelements();
    ssize_t min;
    for (uInt i = 0; i < ndim; i++) {
      if (result(i) > (min = right(i))) {
        result(i) = min;
      }
    }
    return result;
}

Int64 IPosition::product() const
{
    if (nelements() ==  0) {
	return 0;
    }
    Int64 total = 1;
    for (uInt i=0; i<nelements(); i++) {
	total *= data_p[i];
    }
    return total;
}

Bool IPosition::allOne() const
{
    for (uInt i=0; i<nelements(); ++i) {
        if (data_p[i] != 1) {
            return False;
        }
    }
    return True;
}

// <thrown>
//    <item> ArrayConformanceError
// </thrown>
Bool operator == (const IPosition& left, const IPosition& right)
{
    if (! left.conform(right)) {
	throw(ArrayConformanceError("::operator== "
				    "(const IPosition&, const IPosition&) - "
				    "left and right operand do not conform "));
    }
    uInt n=left.nelements();
    Bool result = True;
    for (uInt i=0; i<n; i++) {
	if (left(i) == right(i)) {
	    // Nothing - written to make cut and paste easier
	} else {
	    result = False;
	    break;
	}
    }
    return result;
}

// <thrown>
//    <item> ArrayConformanceError
// </thrown>
Bool operator != (const IPosition& left, const IPosition& right)
{
    if (! left.conform(right)) {
	throw(ArrayConformanceError("::operator!= "
				    "(const IPosition&, const IPosition&) - "
				    "left and right operand do not conform "));
    }
    uInt n=left.nelements();
    Bool result = False;
    for (uInt i=0; i<n; i++) {
	if (left(i) != right(i)) {
	    result = True;
	    break;
	} else {
	    // Nothing - written to make cut and paste easier
	}
    }
    return result;
}

// <thrown>
//    <item> ArrayConformanceError
// </thrown>
Bool operator < (const IPosition& left, const IPosition& right)
{
    if (! left.conform(right)) {
	throw(ArrayConformanceError("::operator< "
			"(const IPosition&, const IPosition&) - "
			"left and right operand do not conform "));
    }
    uInt n=left.nelements();
    Bool result = True;
    for (uInt i=0; i<n; i++) {
	if (left(i) < right(i)) {
	    // Nothing - written to make cut and paste easier
	} else {
	    result = False;
	    break;
	}
    }
    return result;
}

// <thrown>
//    <item> ArrayConformanceError
// </thrown>
Bool operator <= (const IPosition& left, const IPosition& right)
{
    if (! left.conform(right)) {
	throw(ArrayConformanceError("::operator<= "
				    "(const IPosition&, const IPosition&) - "
				    "left and right operand do not conform "));
    }
    uInt n=left.nelements();
    Bool result = True;
    for (uInt i=0; i<n; i++) {
	if (left(i) <= right(i)) {
	    // Nothing - written to make cut and paste easier
	} else {
	    result = False;
	    break;
	}
    }
    return result;
}

// <thrown>
//    <item> ArrayConformanceError
// </thrown>
Bool operator > (const IPosition& left, const IPosition& right)
{
    if (! left.conform(right)) {
	throw(ArrayConformanceError("::operator> "
				    "(const IPosition&, const IPosition&) - "
				    "left and right operand do not conform "));
    }
    uInt n=left.nelements();
    Bool result = True;
    for (uInt i=0; i<n; i++) {
	if (left(i) > right(i)) {
	    // Nothing - written to make cut and paste easier
	} else {
	    result = False;
	    break;
	}
    }
    return result;
}

// <thrown>
//    <item> ArrayConformanceError
// </thrown>
Bool operator >= (const IPosition& left, const IPosition& right)
{
    if (! left.conform(right)) {
	throw(ArrayConformanceError("::operator>= "
				    "(const IPosition&, const IPosition&) - "
				    "left and right operand do not conform "));
    }
    uInt n=left.nelements();
    Bool result = True;
    for (uInt i=0; i<n; i++) {
	if (left(i) >= right(i)) {
	    // Nothing - written to make cut and paste easier
	} else {
	    result = False;
	    break;
	}
    }
    return result;
}

Bool operator == (const IPosition& left, ssize_t val)
{
    Bool result = True;
    uInt n = left.nelements();
    for (uInt i=0; i<n; i++) {
	if (left(i) == val) {
	    // Nothing
	} else {
	    result = False;
	    break;
	}
    }
    return result;
}

Bool operator != (const IPosition& left, ssize_t val)
{
    Bool result = False;
    uInt n = left.nelements();
    for (uInt i=0; i<n; i++) {
	if (left(i) != val) {
	    result = True;
	    break;
	} else {
	    // Nothing
	}
    }
    return result;
}

Bool operator < (const IPosition& left, ssize_t val)
{
    Bool result = True;
    uInt n = left.nelements();
    for (uInt i=0; i<n; i++) {
	if (left(i) < val) {
	    // Nothing
	} else {
	    result = False;
	    break;
	}
    }
    return result;
}

Bool operator <= (const IPosition& left, ssize_t val)
{
    Bool result = True;
    uInt n = left.nelements();
    for (uInt i=0; i<n; i++) {
	if (left(i) <= val) {
	    // Nothing
	} else {
	    result = False;
	    break;
	}
    }
    return result;
}

Bool operator > (const IPosition& left, ssize_t val)
{
    Bool result = True;
    uInt n = left.nelements();
    for (uInt i=0; i<n; i++) {
	if (left(i) > val) {
	    // Nothing
	} else {
	    result = False;
	    break;
	}
    }
    return result;
}

Bool operator >= (const IPosition& left, ssize_t val)
{
    Bool result = True;
    uInt n = left.nelements();
    for (uInt i=0; i<n; i++) {
	if (left(i) >= val) {
	    // Nothing
	} else {
	    result = False;
	    break;
	}
    }
    return result;
}

Bool operator == (ssize_t val, const IPosition& right)
{
    Bool result = True;
    uInt n = right.nelements();
    for (uInt i=0; i<n; i++) {
	if (val == right(i)) {
	    // Nothing
	} else {
	    result = False;
	    break;
	}
    }
    return result;
}

Bool operator != (ssize_t val, const IPosition& right)
{
    Bool result = False;
    uInt n = right.nelements();
    for (uInt i=0; i<n; i++) {
	if (val != right(i)) {
	    result = True;
	    break;
	} else {
	    // Nothing
	}
    }
    return result;
}

Bool operator < (ssize_t val, const IPosition& right)
{
    Bool result = True;
    uInt n = right.nelements();
    for (uInt i=0; i<n; i++) {
	if (val < right(i)) {
	    // Nothing
	} else {
	    result = False;
	    break;
	}
    }
    return result;
}

Bool operator <= (ssize_t val, const IPosition& right)
{
    Bool result = True;
    uInt n = right.nelements();
    for (uInt i=0; i<n; i++) {
	if (val <= right(i)) {
	    // Nothing
	} else {
	    result = False;
	    break;
	}
    }
    return result;
}

Bool operator > (ssize_t val, const IPosition& right)
{
    Bool result = True;
    uInt n = right.nelements();
    for (uInt i=0; i<n; i++) {
	if (val > right(i)) {
	    // Nothing
	} else {
	    result = False;
	    break;
	}
    }
    return result;
}

Bool operator >= (ssize_t val, const IPosition& right)
{
    Bool result = True;
    uInt n = right.nelements();
    for (uInt i=0; i<n; i++) {
	if (val >= right(i)) {
	    // Nothing
	} else {
	    result = False;
	    break;
	}
    }
    return result;
}

String IPosition::toString() const
{
  ostringstream oss;
  oss << *this;
  return oss.str();
}

std::ostream& operator<< (std::ostream& os, const IPosition& ip)
{
    os << "[";
    for (uInt i=0; i<ip.nelements(); i++) {
	if (i > 0) {
	    os << ", ";
	}
	os << ip(i);
    }
    os << "]";
    return os;
}

Bool IPosition::ok() const
{
    Bool retval = True;
    if (size_p <= BufferLength && data_p != &buffer_p[0]) { retval = False; }
    if (data_p == 0) { retval = False; }
    return retval;
}


IPosition toIPositionInArray (Int64 offset, const IPosition& shape)
{
    if (! isInsideArray (offset, shape) ) {
	throw (ArrayIndexError(
            "IPosition ::toIPositionInArray (Int64 offset,"
            " const IPosition& shape)"
             " - Invalid offset."));
    }

    IPosition iposition (shape.nelements());
    Int64 divisor = 1;

    uInt ndim = shape.nelements();
    for (uInt idim = 0; idim < ndim; idim++) {
        iposition(idim) = ((offset / divisor) % shape(idim));
        divisor *= shape(idim);
    }

    return iposition;

}

Int64 toOffsetInArray (const IPosition& iposition, const IPosition& shape)
{
    if (! (iposition.conform(shape)) ) {
	throw (ArrayConformanceError(
            "Int64 ::toOffsetInArray (const IPosition& iposition,"
            " const IPosition& shape)"
             " - IPositions do not conform"));
    }

    if (! isInsideArray (iposition, shape) ) {
	throw (ArrayIndexError(
            "Int64 ::toOffsetInArray (const IPosition& iposition,"
            " const IPosition& shape)"
             " - Invalid iposition."));
    }

    Int64 offset = 0;
    Int64 multiplier = 1;

    uInt ndim = shape.nelements();
    for (uInt idim = 0; idim < ndim; idim++) {
        offset += (iposition(idim) * multiplier);
        multiplier *= shape(idim);
    }

    return offset;
}


Bool isInsideArray (Int64 offset, const IPosition& shape)
{
    return (offset < shape.product()) ? True : False;
}


Bool isInsideArray (const IPosition& iposition, const IPosition& shape)
{
    if (! (iposition.conform(shape)) ) {
	throw (ArrayConformanceError(
            "Bool ::isInsideArray (const IPosition& iposition,"
            " const IPosition& shape)"
             " - IPositions do not conform"));
    }

    Bool result = True;
    ssize_t ioff;

    uInt ndim = shape.nelements();
    for (uInt idim = 0; idim < ndim; idim++) {
        ioff = iposition(idim);
        if ( (ioff < 0) || (ioff >= shape(idim)) ) {
            result = False;
            break;
        }
    }

    return result;

}


IPosition IPosition::makeAxisPath (uInt nrdim, const IPosition& partialPath)
{
    // Check if the specified traversal axes are correct and unique.
    AlwaysAssert (partialPath.nelements() <= nrdim, AipsError);
    IPosition path(nrdim);
    IPosition done(nrdim, 0);
    uInt i,j;
    for (i=0; i<partialPath.nelements(); i++) {
        path(i) = partialPath(i);
        if (path(i) >= Int(nrdim)  ||  done(path(i)) != 0) {
            throw (AipsError ("IPosition::makeAxisPath: invalid defined axes"));
        }
        done(path(i)) = 1;
    }
    // Fill unspecified axes with the natural order.
    for (j=0; j<nrdim; j++) {
        if (done(j) == 0) {
            path(i++) = j;
        }
    }
    return path;
}

IPosition IPosition::otherAxes (uInt nrdim, const IPosition& axes)
{
   AlwaysAssert (nrdim>=axes.nelements(),AipsError);
   return makeAxisPath(nrdim, axes).getLast(nrdim-axes.nelements());
}

void IPosition::throwIndexError() const
{
    // This should be an IndexError<uInt> - but that causes multiply
    // defined symbols with the current objectcenter.
    throw(AipsError("IPosition::operator() - index error"));
}

} //# NAMESPACE CASACORE - END

