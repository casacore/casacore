//# IPosition.cc: A vector of integers, used to index into arrays.
//# Copyright (C) 1994,1995,1996,1997
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

#include <aips/Lattices/IPosition.h>
#include <aips/Exceptions/Error.h>
#include <aips/Arrays/ArrayError.h>
#include <aips/IO/AipsIO.h>
#include <aips/Utilities/Assert.h>
#include <aips/Logging/LogIO.h>
#include <iostream.h>

IPosition::IPosition() : size(0), data(&buffer_p[0])
{
    // Nothing
    DebugAssert(ok(), AipsError);
}

// <thrown>
//    <item> AllocError
// </thrown>
IPosition::IPosition(uInt length) : size(length)
{
    if (size <= BufferLength) {
        data = buffer_p;
    } else {
	data = new Int[size];
	if (data == 0) {
	    throw(AllocError("IPosition::IPosition(uInt length) - "
			     "Cannot allocate storage", size));
	}
    }
    DebugAssert(ok(), AipsError);
}

// <thrown>
//    <item> AllocError
// </thrown>
IPosition::IPosition(uInt length, Int val) : size(length)
{
    if (size <= BufferLength) {
        data = buffer_p;
    } else {
	data = new Int[size];
	if (data == 0) {
	    throw(AllocError("IPosition::IPosition(uInt length, val) - "
			     "Cannot allocate storage", size));
	}
    }
    for (uInt i=0; i<size; i++) {
	data[i] = val;
    }
    DebugAssert(ok(), AipsError);
}

// <thrown>
//    <item> AllocError
//    <item> AipsError
// </thrown>
IPosition::IPosition (uInt length, Int val0, Int val1, Int val2, Int val3, 
		      Int val4, Int val5, Int val6, Int val7, Int val8, 
		      Int val9)
: size(length), data(0)
{
    if (size <= BufferLength) {
        data = buffer_p;
    } else {
	data = new Int[size];
	if (data == 0) {
	    throw(AllocError("IPosition::IPosition(uInt length, val0, ...) - "
			     "Cannot allocate storage", size));
	}
    }
    if (size > 10 || length < 1) {
	throw(AipsError("IPosition::IPosition(uInt length, val0, ...) - "
			 "Can only initialize from 1 to 10 elements"));
    }

    switch (length) {
         case 10: data[9] = val9;    // Fall through
	 case 9: data[8] = val8;    // Fall through
	 case 8: data[7] = val7;    // Fall through
	 case 7: data[6] = val6;    // Fall through
	 case 6: data[5] = val5;    // Fall through
	 case 5: data[4] = val4;    // Fall through
	 case 4: data[3] = val3;    // Fall through
	 case 3: data[2] = val2;    // Fall through
	 case 2: data[1] = val1;    // Fall through
	 case 1: data[0] = val0; break;
	 default: DebugAssert(0, AipsError); // NOTREACHED
    }
    for (uInt i=0; i < size; i++) {
	if (data[i] == MIN_INT) {
	throw(AipsError("IPosition::IPosition(uInt length, val0, ...) - "
		"One or more valn == INT_MIN. Probably haven't defined "
		"enough values. Otherwise specify after construction."));
	}
    }

    DebugAssert(ok(), AipsError);
}

// <thrown>
//    <item> AllocError
// </thrown>
IPosition::IPosition(const IPosition &other) : size(other.size)
{
    if (size <= BufferLength) {
        data = buffer_p;
    } else {
	data = new Int[size];
	if (data == 0) {
	    throw(AllocError("IPosition::IPosition(const IPosition &) - "
			     "Cannot allocate storage", size));
	}
    }
    for (uInt i=0; i < size; i++) {
      data[i] = other.data[i];
    }
    DebugAssert(ok(), AipsError);
}

IPosition::~IPosition()
{
    if (data != &buffer_p[0]) {
	// We could also unconditionally delete data; however in the past
	// some compilers have complained about deleting a zero-pointer even
	// though it is supposed to be OK.
	delete [] data;
    }
}

IPosition IPosition::nonDegenerate(uInt startingAxis) const
{
  if (startingAxis >= size) 
    throw( AipsError("IPosition::nonDegenerate: startingAxis is greater than"
		     " size of this IPosition."));
  IPosition ignoreAxes(startingAxis);
  for (uInt i=0; i<startingAxis; i++) {
    ignoreAxes(i) = i;
  }
  return nonDegenerate (ignoreAxes);
}
IPosition IPosition::nonDegenerate(const IPosition& ignoreAxes) const
{
  DebugAssert(ok(), AipsError);
  // First determine which axes have to be ignored, thus always be kept.
  // Do not count here, because in theory ignoreAxes can contain the
  // same axis more than once.
  // To remove degenerate axes use two passes - first find out how many axes
  // have to be kept.
  uInt i;
  IPosition keepAxes(size, 0);
  for (i=0; i<ignoreAxes.nelements(); i++) {
      AlwaysAssert (ignoreAxes(i) < size, AipsError);
      keepAxes(ignoreAxes(i)) = 1;
  }
  // Now count all axes to keep.
  uInt count=0;
  for (i=0; i<size; i++) {
    if (keepAxes(i) == 1) {
      count++;
    }else{
      if (data[i] != 1) {
	keepAxes(i) = 1;
        count++;
      }
    }
  }
  if (count == size) return *this;
  if (count == 0) count = 1;
  IPosition nondegenerateIP(count,1);
  count = 0;
  for (i=0; i<size; i++) {
    if (keepAxes(i)) {
      nondegenerateIP(count++) = data[i];    
    }
  }
  return nondegenerateIP;
};

// <thrown>
//    <item> AllocError
// </thrown>
void IPosition::resize(uInt newSize, Bool copy)
{
    DebugAssert(ok(), AipsError);

    // If the size is unchanged, just return (more efficient)
    if (newSize == size) {
	return;
    }

    Int *newData = 0;
    if (newSize <= BufferLength) {
        newData = buffer_p;
    } else {
        newData = new Int[newSize];
	if (newData == 0) {
	    throw(AllocError("IPosition::resize(Int newSize.Bool copy) - "
			     "Cannot allocate storage", size));
	}
    }
    
    if (newData != data && copy) {
	for (uInt i=0; i < min(size, newSize); i++) {
	    newData[i] = data[i];
	}
    }

    // Delete the old data, if any
    if (size > 0 && data != &buffer_p[0]) {
	delete [] data;
        data = 0;
    }
    // Point to new data
    size = newSize;
    data = newData;
    DebugAssert(ok(), AipsError);
}

// <thrown>
//    <item> ArrayConformanceError
// </thrown>
IPosition &IPosition::operator=(const IPosition &other)
{
    DebugAssert(ok(), AipsError);
    if (&other == this) {
	return *this;
    }
    if (size == 0) {
	this->resize(other.nelements());
    }
    if (conform(other) == False) {
	throw(ArrayConformanceError("IPosition::operator=(const IPosition & - "
				    "this and other differ in length"));
    }
    for (uInt i=0; i < size; i++) {
	data[i] = other.data[i];
    }
    DebugAssert(ok(), AipsError);
    return *this;
}

IPosition &IPosition::operator=(Int value)
{
    DebugAssert(ok(), AipsError);
    for (uInt i=0; i < size; i++) {
	data[i] = value;
    }
    return *this;
}

void IPosition::append (const IPosition& other)
{
    uInt j = size;
    resize (size + other.size);
    for (uInt i=0; i<other.size; i++) {
	data[j++] = other.data[i];
    }
}

void IPosition::prepend (const IPosition& other)
{
    uInt j = size;
    resize (size + other.size);
    for (uInt i=size; j>0;) {
	data[--i] = data[--j];
    }
    for (i=0; i<other.size; i++) {
	data[i] = other.data[i];
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
    if (size < other.size) {
	throw (AipsError ("IPosition::setFirst(other); other is too long"));
    }
    for (uInt i=0; i<other.size; i++) {
	data[i] = other.data[i];
    }
}

void IPosition::setLast (const IPosition& other)
{
    if (size < other.size) {
	throw (AipsError ("IPosition::setLast(other); other is too long"));
    }
    uInt j = size - other.size;
    for (uInt i=0; i<other.size; i++) {
	data[j++] = other.data[i];
    }
}

IPosition IPosition::getFirst (uInt n) const
{
    if (size < n) {
	throw (AipsError ("IPosition::getFirst(n); n is too high"));
    }
    IPosition tmp(n);
    for (uInt i=0; i<n; i++) {
	tmp.data[i] = data[i];
    }
    return tmp;
}

IPosition IPosition::getLast (uInt n) const
{
    if (size < n) {
	throw (AipsError ("IPosition::getLast(n); n is too high"));
    }
    IPosition tmp(n);
    uInt j = size - n;
    for (uInt i=0; i<n; i++) {
	tmp.data[i] = data[j++];
    }
    return tmp;
}


// <thrown>
//    <item> ArrayConformanceError
// </thrown>
void IPosition::operator += (const IPosition  &other)
{
    DebugAssert(ok(), AipsError);
    if (conform(other) == False) {
	throw(ArrayConformanceError("IPosition::operator += "
				    "(const IPosition &) - "
				    "this and other differ in length"));
    }
    for (uInt i=0; i < size; i++) {
	data[i] += other.data[i];
    }
}

// <thrown>
//    <item> ArrayConformanceError
// </thrown>
void IPosition::operator -= (const IPosition  &other)
{
    DebugAssert(ok(), AipsError);
    if (conform(other) == False) {
	throw(ArrayConformanceError("IPosition::operator -= "
				    "(const IPosition &) - "
				    "this and other differ in length"));
    }
    for (uInt i=0; i < size; i++) {
	data[i] -= other.data[i];
    }
}

// <thrown>
//    <item> ArrayConformanceError
// </thrown>
void IPosition::operator *= (const IPosition  &other)
{
    DebugAssert(ok(), AipsError);
    if (conform(other) == False) {
	throw(ArrayConformanceError("IPosition::operator *= "
				    "(const IPosition &) - "
				    "this and other differ in length"));
    }
    for (uInt i=0; i < size; i++) {
	data[i] *= other.data[i];
    }
}

// <thrown>
//    <item> ArrayConformanceError
// </thrown>
void IPosition::operator /= (const IPosition  &other)
{
    DebugAssert(ok(), AipsError);
    if (conform(other) == False) {
	throw(ArrayConformanceError("IPosition::operator /= "
				    "(const IPosition &) - "
				    "this and other differ in length"));
    }
    for (uInt i=0; i < size; i++) {
	data[i] /= other.data[i];
    }
}

void IPosition::operator += (Int val)
{
    DebugAssert(ok(), AipsError);
    for (uInt i=0; i < size; i++) {
	data[i] += val;
    }
}

void IPosition::operator -= (Int val)
{
    DebugAssert(ok(), AipsError);
    for (uInt i=0; i < size; i++) {
	data[i] -= val;
    }
}

void IPosition::operator *= (Int val)
{
    DebugAssert(ok(), AipsError);
    for (uInt i=0; i < size; i++) {
	data[i] *= val;
    }
}

void IPosition::operator /= (Int val)
{
    DebugAssert(ok(), AipsError);
    for (uInt i=0; i < size; i++) {
	data[i] /= val;
    }
}

Bool IPosition::isEqual (const IPosition &other) const
{
    return isEqual (other, nelements());
}

Bool IPosition::isEqual (const IPosition &other,
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
	if (data[i] != 1) {
	    while (j < nrthat  &&  other(j) == 1) {
		j++;
	    }
	    if (j >= nrthat) {
		break;
	    }
	    if (data[i] != other(j)) {
		return False;
	    }
	    j++;
	}
    }
    for (; i<nrthis; i++) {
	if (data[i] != 1) {
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

Bool IPosition::isEqual (const IPosition &other, uInt nrCompare) const
{
    if (! conform (other)) {
	return False;
    }
    if (nrCompare > nelements()) {
	nrCompare = nelements();
    }
    for (uInt i=0; i<nrCompare; i++) {
	if (data[i] != other(i)) {
	    return False;
	}
    }
    return True;
}


// <thrown>
//    <item> ArrayConformanceError
// </thrown>
IPosition operator + (const IPosition &left, const IPosition &right)
{
    if (left.conform(right) == False) {
	throw(ArrayConformanceError("::operator + "
			"(const IPosition &, const IPosition &) - "
			"left and right operand do not conform "));
    }
    IPosition result(left);
    result += right;
    return result;
}

// <thrown>
//    <item> ArrayConformanceError
// </thrown>
IPosition operator - (const IPosition &left, const IPosition &right)
{
    if (left.conform(right) == False) {
	throw(ArrayConformanceError("::operator - "
			"(const IPosition &, const IPosition &) - "
			"left and right operand do not conform "));
    }
    IPosition result(left);
    result -= right;
    return result;
}

// <thrown>
//    <item> ArrayConformanceError
// </thrown>
IPosition operator * (const IPosition &left, const IPosition &right)
{
    if (left.conform(right) == False) {
	throw(ArrayConformanceError("::operator * "
			"(const IPosition &, const IPosition &) - "
			"left and right operand do not conform "));
    }
    IPosition result(left);
    result *= right;
    return result;
}

// <thrown>
//    <item> ArrayConformanceError
// </thrown>
IPosition operator / (const IPosition &left, const IPosition &right)
{
    if (left.conform(right) == False) {
	throw(ArrayConformanceError("::operator / "
			"(const IPosition &, const IPosition &) - "
			"left and right operand do not conform "));
    }
    IPosition result(left);
    result /= right;
    return result;
}

IPosition operator + (const IPosition &left, Int val)
{
    IPosition result(left);
    result += val;
    return result;
}

IPosition operator - (const IPosition &left, Int val)
{
    IPosition result(left);
    result -= val;
    return result;
}

IPosition operator * (const IPosition &left, Int val)
{
    IPosition result(left);
    result *= val;
    return result;
}

IPosition operator / (const IPosition &left, Int val)
{
    IPosition result(left);
    result /= val;
    return result;
}

IPosition operator + (Int val, const IPosition &right)
{
    IPosition result(right.nelements());
    result = val;
    result += right;
    return result;
}

IPosition operator - (Int val, const IPosition &right)
{
    IPosition result(right.nelements());
    result = val;
    result -= right;
    return result;
}

IPosition operator * (Int val, const IPosition &right)
{
    IPosition result(right.nelements());
    result = val;
    result *= right;
    return result;
}

IPosition operator / (Int val, const IPosition &right)
{
    IPosition result(right.nelements());
    result = val;
    result /= right;
    return result;
}

Int IPosition::product() const
{
    if (nelements() ==  0) {
	return 0;
    }
    Int total = 1;
    for (uInt i=0; i<nelements(); i++) {
	total *= (*this)(i);
    }
    return total;
}

// <thrown>
//    <item> ArrayConformanceError
// </thrown>
Bool operator == (const IPosition &left, const IPosition &right)
{
    if (left.conform(right) == False) {
	throw(ArrayConformanceError("::operator== "
				    "(const IPosition &, const IPosition &) - "
				    "left and right operand do not conform "));
    }
    uInt n=left.nelements();
    Bool result = True;
    for (uInt i=0; i < n; i++) {
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
Bool operator != (const IPosition &left, const IPosition &right)
{
    if (left.conform(right) == False) {
	throw(ArrayConformanceError("::operator!= "
				    "(const IPosition &, const IPosition &) - "
				    "left and right operand do not conform "));
    }
    uInt n=left.nelements();
    Bool result = False;
    for (uInt i=0; i < n; i++) {
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
Bool operator < (const IPosition &left, const IPosition &right)
{
    if (left.conform(right) == False) {
	throw(ArrayConformanceError("::operator< "
			"(const IPosition &, const IPosition &) - "
			"left and right operand do not conform "));
    }
    uInt n=left.nelements();
    Bool result = True;
    for (uInt i=0; i < n; i++) {
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
Bool operator <= (const IPosition &left, const IPosition &right)
{
    if (left.conform(right) == False) {
	throw(ArrayConformanceError("::operator<= "
				    "(const IPosition &, const IPosition &) - "
				    "left and right operand do not conform "));
    }
    uInt n=left.nelements();
    Bool result = True;
    for (uInt i=0; i < n; i++) {
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
Bool operator > (const IPosition &left, const IPosition &right)
{
    if (left.conform(right) == False) {
	throw(ArrayConformanceError("::operator> "
				    "(const IPosition &, const IPosition &) - "
				    "left and right operand do not conform "));
    }
    uInt n=left.nelements();
    Bool result = True;
    for (uInt i=0; i < n; i++) {
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
Bool operator >= (const IPosition &left, const IPosition &right)
{
    if (left.conform(right) == False) {
	throw(ArrayConformanceError("::operator>= "
				    "(const IPosition &, const IPosition &) - "
				    "left and right operand do not conform "));
    }
    uInt n=left.nelements();
    Bool result = True;
    for (uInt i=0; i < n; i++) {
	if (left(i) >= right(i)) {
	    // Nothing - written to make cut and paste easier
	} else {
	    result = False;
	    break;
	}
    }
    return result;
}

Bool operator == (const IPosition &left, Int val)
{
    Bool result = True;
    uInt n = left.nelements();
    for (uInt i=0; i < n; i++) {
	if (left(i) == val) {
	    // Nothing
	} else {
	    result = False;
	    break;
	}
    }
    return result;
}

Bool operator != (const IPosition &left, Int val)
{
    Bool result = False;
    uInt n = left.nelements();
    for (uInt i=0; i < n; i++) {
	if (left(i) != val) {
	    result = True;
	    break;
	} else {
	    // Nothing
	}
    }
    return result;
}

Bool operator < (const IPosition &left, Int val)
{
    Bool result = True;
    uInt n = left.nelements();
    for (uInt i=0; i < n; i++) {
	if (left(i) < val) {
	    // Nothing
	} else {
	    result = False;
	    break;
	}
    }
    return result;
}

Bool operator <= (const IPosition &left, Int val)
{
    Bool result = True;
    uInt n = left.nelements();
    for (uInt i=0; i < n; i++) {
	if (left(i) <= val) {
	    // Nothing
	} else {
	    result = False;
	    break;
	}
    }
    return result;
}

Bool operator > (const IPosition &left, Int val)
{
    Bool result = True;
    uInt n = left.nelements();
    for (uInt i=0; i < n; i++) {
	if (left(i) > val) {
	    // Nothing
	} else {
	    result = False;
	    break;
	}
    }
    return result;
}

Bool operator >= (const IPosition &left, Int val)
{
    Bool result = True;
    uInt n = left.nelements();
    for (uInt i=0; i < n; i++) {
	if (left(i) >= val) {
	    // Nothing
	} else {
	    result = False;
	    break;
	}
    }
    return result;
}

Bool operator == (Int val, const IPosition &right)
{
    Bool result = True;
    uInt n = right.nelements();
    for (uInt i=0; i < n; i++) {
	if (val == right(i)) {
	    // Nothing
	} else {
	    result = False;
	    break;
	}
    }
    return result;
}

Bool operator != (Int val, const IPosition &right)
{
    Bool result = False;
    uInt n = right.nelements();
    for (uInt i=0; i < n; i++) {
	if (val != right(i)) {
	    result = True;
	    break;
	} else {
	    // Nothing
	}
    }
    return result;
}

Bool operator < (Int val, const IPosition &right)
{
    Bool result = True;
    uInt n = right.nelements();
    for (uInt i=0; i < n; i++) {
	if (val < right(i)) {
	    // Nothing
	} else {
	    result = False;
	    break;
	}
    }
    return result;
}

Bool operator <= (Int val, const IPosition &right)
{
    Bool result = True;
    uInt n = right.nelements();
    for (uInt i=0; i < n; i++) {
	if (val <= right(i)) {
	    // Nothing
	} else {
	    result = False;
	    break;
	}
    }
    return result;
}

Bool operator > (Int val, const IPosition &right)
{
    Bool result = True;
    uInt n = right.nelements();
    for (uInt i=0; i < n; i++) {
	if (val > right(i)) {
	    // Nothing
	} else {
	    result = False;
	    break;
	}
    }
    return result;
}

Bool operator >= (Int val, const IPosition &right)
{
    Bool result = True;
    uInt n = right.nelements();
    for (uInt i=0; i < n; i++) {
	if (val >= right(i)) {
	    // Nothing
	} else {
	    result = False;
	    break;
	}
    }
    return result;
}

ostream &operator<<(ostream &os, const IPosition &ip)
{
    os << "[";
    for(Int i=0; i < ip.nelements(); i++) {
	os << ip(i);
	if (i < ip.nelements() - 1) {
	    os << ", ";
	}
    }
    os << "]";
    return os;
}

LogIO &operator<<(LogIO &os, const IPosition &ip)
{
    os.output() << ip;
    return os;
}

AipsIO &operator<<(AipsIO &aio, const IPosition &ip)
{
    aio.putstart("IPosition", IPosition::IPositionVersion);
    // Write out the values
    aio.put(ip.size, ip.data);
    aio.putend();
    return aio;
}

// <thrown>
//    <item> AipsError
// </thrown>
AipsIO &operator>>(AipsIO &aio, IPosition &ip)
{
    if (aio.getstart("IPosition") != IPosition::IPositionVersion) {
	throw(AipsError("AipsIO &operator>>(AipsIO &aio, IPosition &ip) - "
			"version on disk and in class do not match"));
    }
    uInt nel;
    aio >> nel;
    ip.resize (nel);
    aio.get (nel, ip.data);
    aio.getend();
    DebugAssert (ip.ok(), AipsError);
    return aio;
}

Bool IPosition::ok() const
{
    Bool retval = True;
    if (size <= BufferLength && data != &buffer_p[0]) { retval = False; }
    if (data == 0) { retval = False; }
    return retval;
}

IPosition toIPositionInArray (const uInt offset,
                              const IPosition &shape, const IPosition &origin)
{
    if ( ! origin.conform(shape) ) {
	throw (ArrayConformanceError(
            "IPosition ::toIPositionInArray (const uInt offset,"
            " const IPosition &origin, const IPosition &shape)"
             " - IPositions do not conform"));
    }

    if ( ! isInsideArray (offset, shape, origin) ) {
	throw (ArrayIndexError(
            "IPosition ::toIPositionInArray (const uInt offset,"
            " const IPosition &origin, const IPosition &shape)"
             " - Invalid offset."));
    }

    IPosition iposition (origin.nelements());
    uInt divisor = 1;

    uInt ndim = origin.nelements();
    for (uInt idim = 0; idim < ndim; idim++) {
        iposition(idim) = origin(idim) + ((offset / divisor) % shape(idim));
        divisor *= shape(idim);
    }

    return iposition;

}

uInt toOffsetInArray (const IPosition &iposition,
                      const IPosition &shape, const IPosition &origin)
{
    if ( ! (iposition.conform(origin) && iposition.conform(shape)) ) {
	throw (ArrayConformanceError(
            "uInt ::toOffsetInArray (const IPosition &iposition,"
            " const IPosition &origin, const IPosition &shape)"
             " - IPositions do not conform"));
    }

    if ( ! isInsideArray (iposition, shape, origin) ) {
	throw (ArrayIndexError(
            "uInt ::toOffsetInArray (const IPosition &iposition,"
            " const IPosition &origin, const IPosition &shape)"
             " - Invalid iposition."));
    }

    uInt offset = 0;
    uInt multiplier = 1;

    uInt ndim = origin.nelements();
    for (uInt idim = 0; idim < ndim; idim++) {
        offset += ((iposition(idim) - origin(idim)) * multiplier);
        multiplier *= shape(idim);
    }

    return offset;
}


Bool isInsideArray (const uInt offset,
                    const IPosition &shape, const IPosition &origin)
{
    if ( ! origin.conform(shape) ) {
	throw (ArrayConformanceError(
            "Bool ::isInsideArray (const uInt offset,"
            " const IPosition &origin, const IPosition &shape)"
             " - IPositions do not conform"));
    }

    uInt nelements = 1;

    uInt ndim = origin.nelements();
    for (uInt idim = 0; idim < ndim; idim++) {
        nelements *= shape(idim);
    }

    return (offset < nelements) ? True : False;
}


Bool isInsideArray (const IPosition &iposition,
                    const IPosition &shape, const IPosition &origin)
{
    if ( ! (iposition.conform(origin) && iposition.conform(shape)) ) {
	throw (ArrayConformanceError(
            "Bool ::isInsideArray (const IPosition &iposition,"
            " const IPosition &origin, const IPosition &shape)"
             " - IPositions do not conform"));
    }

    Bool result = True;
    Int ioff;

    uInt ndim = origin.nelements();
    for (uInt idim = 0; idim < ndim; idim++) {
        ioff = iposition(idim) - origin(idim);
        if ( (ioff < 0) || (ioff >= shape(idim)) ) {
            result = False;
            break;
        }
    }

    return result;

}
// Local Variables: 
// compile-command: "gmake OPTLIB=1 IPosition"
// End: 
