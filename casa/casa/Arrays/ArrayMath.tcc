//# ArrayMath.cc: Arithmetic functions defined on Arrays
//# Copyright (C) 1993,1994,1995,1996,1997,1998,1999,2001,2003
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

#include <casa/iostream.h>

#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/ArrayUtil.h>
#include <casa/Arrays/Array.h>
#include <casa/Arrays/ArrayIter.h>
#include <casa/Arrays/VectorIter.h>
#include <casa/Arrays/ArrayError.h>
#include <casa/BasicMath/Math.h>
#include <casa/BasicMath/ConvertScalar.h>
#include <casa/Utilities/GenSort.h>
#include <casa/Utilities/Assert.h>

namespace casa { //# NAMESPACE CASA - BEGIN

// <thrown>
//   <item> ArrayError
// </thrown>
template<class ScalarType> 
void minMax(ScalarType &minVal, ScalarType &maxVal, 
	    IPosition &minPos, IPosition &maxPos,
	    const Array<ScalarType> &array) 
{
    if (array.nelements() == 0) {
        throw(ArrayError("void minMax(T &min, T &max, IPosition &minPos,"
			 "IPosition &maxPos, const Array<T> &array) - "
                         "Array has no elements"));	
    }
    minPos.resize (array.ndim());
    maxPos.resize (array.ndim());

    ReadOnlyVectorIterator<ScalarType> ai(array);
    ScalarType val;

    // Initialize
    minPos = 0;
    maxPos = 0;
    minVal = maxVal = array(minPos);
    uInt n = ai.vector().nelements();

    while (! ai.pastEnd()) {
	for (uInt i=0; i<n; i++) {
	    val = ai.vector()(i);
	    if (val < minVal) {
	        minVal = val;
		minPos = ai.pos();
		minPos(0) += i;
	    }
	    if (val > maxVal) {
	        maxVal = val;
		maxPos = ai.pos();
		maxPos(0) += i;
	    }
	}
	ai.next(); 
    }
}



// <thrown>
//   <item> ArrayError
// </thrown>
template<class ScalarType> 
void minMaxMasked(ScalarType &minVal, ScalarType &maxVal, 
	    IPosition &minPos, IPosition &maxPos,
	    const Array<ScalarType> &array, const Array<ScalarType> &mask) 
{
    if (array.nelements() == 0) {
      throw(ArrayError("void minMaxMasked(T &min, T &max, IPosition &minPos,"
		       "IPosition &maxPos, const Array<T> &array) - "
		       "Array has no elements"));	
    }
    if (array.shape() != mask.shape()) {
      throw(ArrayConformanceError("void minMaxMasked(T &min, T &max,"
				  "IPosition &minPos, IPosition &maxPos, const Array<T> &array, "
				  "const Array<T> &mask) - " 
				  "array and mask do not have the same shape()"));
    } 
    minPos.resize (array.ndim());
    maxPos.resize (array.ndim());

    ReadOnlyVectorIterator<ScalarType> ai(array);
    ReadOnlyVectorIterator<ScalarType> mi(mask);
    ScalarType val;

    // Initialize
    minPos = 0;
    maxPos = 0;
    minVal = maxVal = array(minPos) * mask(minPos);
    uInt n = ai.vector().nelements();

    while (! ai.pastEnd()) {
	for (uInt i=0; i<n; i++) {
	    val = (ai.vector()(i)) * (mi.vector()(i));
	    if (val < minVal) {
	        minVal = val;
		minPos = ai.pos();
		minPos(0) += i;
	    }
	    if (val > maxVal) {
	        maxVal = val;
		maxPos = ai.pos();
		maxPos(0) += i;
	    }
	}
	ai.next(); 
    }
}


// <thrown>
//   <item> ArrayError
//   <item> AipsError
// </thrown>
template<class ScalarType> 
void minMax(ScalarType &minVal, ScalarType &maxVal, 
	    IPosition &minPos, IPosition &maxPos,
	    const Array<ScalarType> &array, const Array<Bool> &mask)
{
    if (array.nelements() == 0) {
      throw(ArrayError("void minMax(T &min, T &max, IPosition &minPos,"
		       "IPosition &maxPos, const Array<T> &array, "
		       "const Array<Bool> &mask) - "
		       "Array has no elements"));	
    }
    if (array.shape() != mask.shape()) {
      throw(ArrayConformanceError("void minMax(T &min, T &max,"
	    "IPosition &minPos, IPosition &maxPos, const Array<T> &array, "
	    "const Array<Bool> &mask) - " 
	    "array and mask do not have the same shape()"));
    } 
    minPos.resize (array.ndim());
    maxPos.resize (array.ndim());

    ReadOnlyVectorIterator<ScalarType> ai(array);
    ReadOnlyVectorIterator<Bool> mi(mask);

    // Initialize
    // have to find a valid value in array to init min and max
    Bool found=False;
    uInt ifound=0;
    uInt n = mi.vector().nelements();
    while (! mi.pastEnd() && !found) {
	for (uInt i=0; i<n; i++) {
	    if (mi.vector()(i)) {
		maxPos = ai.pos();
		maxPos(0) += i;
		found =  True;
                ifound = i;
		break;
	    }
	}
        if (!found) {
	    mi.next();
	    ai.next();
        }
    }
    if (!found) {
      throw(AipsError("void minMax(T &min, T &max,"
	    "IPosition &minPos, IPosition &maxPos, const Array<T> &array, "
	    "const Array<Bool> &mask) - mask==False, no valid array elements"));
    }
    minPos = maxPos;
    minVal = maxVal = array(minPos);

    ScalarType val; 

    // Finish with vector where first value was found.
    {
        for (uInt i=++ifound; i<n; i++) {
            if (mi.vector()(i)) {
                val = ai.vector()(i);
                if (val < minVal) {
                    minVal = val;
                    minPos = ai.pos();
                    minPos(0) += i;
                }
                if (val > maxVal) {
                    maxVal = val;
                    maxPos = ai.pos();
                    maxPos(0) += i;
                }
            }
        }
        ai.next(); mi.next();
    }

    // Continue with the rest of the array.
    while (! ai.pastEnd()) {
        for (uInt i=0; i<n; i++) {
            if (mi.vector()(i)) {
                val = ai.vector()(i);
                if (val < minVal) {
                    minVal = val;
                    minPos = ai.pos();
                    minPos(0) += i;
                }
                if (val > maxVal) {
                    maxVal = val;
                    maxPos = ai.pos();
                    maxPos(0) += i;
                }
            }
        }
	ai.next(); mi.next();
    }
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<class T> void operator+= (Array<T> &left, const Array<T> &other)
{
    if (left.conform(other) == False) {
	throw(ArrayConformanceError("::operator+=(Array<T> &, const Array<T> &)"
				    " - arrays do not conform"));
    }

    uInt ntotal = left.nelements(); // conform , so == other.nelements()
    
    Bool leftDelete, otherDelete;
    T *leftStorage = left.getStorage(leftDelete);
    T *ls = leftStorage;
    
    const T *otherStorage = other.getStorage(otherDelete);
    const T *os = otherStorage;
    while (ntotal--) {
	*ls++ += *os++;
    }

    left.putStorage(leftStorage, leftDelete);
    other.freeStorage(otherStorage, otherDelete);
}

template<class T>  T min(const Array<T> &a)
    { T Min, Max; minMax(Min, Max, a); return Min; }

template<class T>  T max(const Array<T> &a)
    { T Min, Max; minMax(Min, Max, a); return Max; }

template<class T>  void indgen(Array<T> &a) {indgen(a, T(0), T(1));}

template<class T>  void indgen(Array<T> &a, T start) 
{indgen(a, start, T(1));}

template<class T> void operator+= (Array<T> &left, const T &other)
{
    uInt ntotal = left.nelements();
    
    Bool leftDelete;
    T *leftStorage = left.getStorage(leftDelete);
    T *ls = leftStorage;

    while (ntotal--) {
	*ls++ += other;
    }
    left.putStorage(leftStorage, leftDelete);
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<class T> void operator-= (Array<T> &left, const Array<T> &other)
{
    if (left.conform(other) == False) {
	throw(ArrayConformanceError("::operator-=(Array<T> &, const Array<T> &)"
				    " - arrays do not conform"));
    }

    uInt ntotal = left.nelements(); // conform , so == other.nelements()
    
    Bool leftDelete, otherDelete;
    T *leftStorage = left.getStorage(leftDelete);
    T *ls = leftStorage;

    const T *otherStorage = other.getStorage(otherDelete);
    const T *os = otherStorage;

    while (ntotal--) {
	*ls++ -= *os++;
    }

    left.putStorage(leftStorage, leftDelete);
    other.freeStorage(otherStorage, otherDelete);
}

template<class T> void operator-= (Array<T> &left, const T &other)
{
    uInt ntotal = left.nelements(); // conform , so == other.nelements()
    
    Bool leftDelete;
    T *leftStorage = left.getStorage(leftDelete);
    T *ls = leftStorage;

    while (ntotal--) {
	*ls++ -= other;
    }
    left.putStorage(leftStorage, leftDelete);
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<class T> void operator*= (Array<T> &left, const Array<T> &other)
{
    if (left.conform(other) == False) {
	throw(ArrayConformanceError("::operator*=(Array<T> &, const Array<T> &)"
				    " - arrays do not conform"));
    }

    uInt ntotal = left.nelements(); // conform , so == other.nelements()
    
    Bool leftDelete, otherDelete;
    T *leftStorage = left.getStorage(leftDelete);
    T *ls = leftStorage;

    const T *otherStorage = other.getStorage(otherDelete);
    const T *os = otherStorage;

    while (ntotal--) {
	*ls++ *= *os++;
    }

    left.putStorage(leftStorage, leftDelete);
    other.freeStorage(otherStorage, otherDelete);
}

template<class T> void operator*= (Array<T> &left, const T &other)
{
    uInt ntotal = left.nelements(); // conform , so == other.nelements()
    
    Bool leftDelete;
    T *leftStorage = left.getStorage(leftDelete);
    T *ls = leftStorage;

    while (ntotal--) {
	*ls++ *= other;
    }
    left.putStorage(leftStorage, leftDelete);
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<class T> void operator/= (Array<T> &left, const Array<T> &other)
{
    if (left.conform(other) == False) {
	throw(ArrayConformanceError("::operator/=(Array<T> &, const Array<T> &)"
				    " - arrays do not conform"));
    }

    uInt ntotal = left.nelements(); // conform , so == other.nelements()
    
    Bool leftDelete, otherDelete;
    T *leftStorage = left.getStorage(leftDelete);
    T *ls = leftStorage;

    const T *otherStorage = other.getStorage(otherDelete);
    const T *os = otherStorage;

    while (ntotal--) {
	*ls++ /= *os++;
    }

    left.putStorage(leftStorage, leftDelete);
    other.freeStorage(otherStorage, otherDelete);
}

template<class T> void operator/= (Array<T> &left, const T &other)
{
    uInt ntotal = left.nelements(); // conform , so == other.nelements()
    
    Bool leftDelete;
    T *leftStorage = left.getStorage(leftDelete);
    T *ls = leftStorage;

    while (ntotal--) {
	*ls++ /= other;
    }
    left.putStorage(leftStorage, leftDelete);
}

template<class T> Array<T> operator+(const Array<T> &a)
{
    return a.copy();
}

template<class T> Array<T> operator-(const Array<T> &a)
{
    Array<T> tmp = a.copy();
    Bool zapIt;
    T *storage = tmp.getStorage(zapIt);
    uInt ntotal = tmp.nelements();

    for (uInt i=0; i < ntotal; i++) {
	storage[i] = - storage[i];
    }
    tmp.putStorage(storage, zapIt);
    return tmp;
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<class T>
   Array<T> operator+(const Array<T> &left, const Array<T> &right)
{
    if (left.conform(right) == False) {
	throw(ArrayConformanceError("::operator+(const Array<T> &, const "
				    "Array<T> &)"
				    " - arrays do not conform"));
    }
    
    Array<T> tmp(left.copy());
    tmp += right;
    return tmp;
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<class T>
   Array<T> operator-(const Array<T> &left, const Array<T> &right)
{
    if (left.conform(right) == False) {
	throw(ArrayConformanceError("::operator-(const Array<T> &, const "
				    "Array<T> &)"
				    " - arrays do not conform"));
    }
    
    Array<T> tmp(left.copy());
    tmp -= right;
    return tmp;
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<class T>
   Array<T> operator*(const Array<T> &left, const Array<T> &right)
{
    if (left.conform(right) == False) {
	throw(ArrayConformanceError("::operator*(const Array<T> &, const "
				    "Array<T> &)"
				    " - arrays do not conform"));
    }
    
    Array<T> tmp(left.copy());
    tmp *= right;
    return tmp;
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<class T>
   Array<T> operator/(const Array<T> &left, const Array<T> &right)
{
    if (left.conform(right) == False) {
	throw(ArrayConformanceError("::operator/(const Array<T> &, const "
				    "Array<T> &)"
				    " - arrays do not conform"));
    }
    
    Array<T> tmp(left.copy());
    tmp /= right;
    return tmp;
}

template<class T> 
Array<T> operator+ (const Array<T> &left, const T &right)
{
    Array<T> tmp(left.copy());
    tmp += right;
    return tmp;
}

template<class T> 
Array<T> operator- (const Array<T> &left, const T &right)
{
    Array<T> tmp(left.copy());
    tmp -= right;
    return tmp;
}

template<class T>
Array<T> operator* (const Array<T> &left, const T &right)
{
    Array<T> tmp(left.copy());
    tmp *= right;
    return tmp;
}

template<class T> 
Array<T> operator/ (const Array<T> &left, const T &right)
{
    Array<T> tmp(left.copy());
    tmp /= right;
    return tmp;
}

template<class T> 
Array<T> operator+ (const T &left, const Array<T> &right)
{
    Array<T> tmp(right.shape());
    tmp = left;
    tmp += right;
    return tmp;
}

template<class T> 
Array<T> operator- (const T &left, const Array<T> &right)
{
    Array<T> tmp(right.shape());
    tmp = left;
    tmp -= right;
    return tmp;
}

template<class T> 
Array<T> operator* (const T &left, const Array<T> &right)
{
    Array<T> tmp(right.shape());
    tmp = left;
    tmp *= right;
    return tmp;
}

template<class T> 
Array<T> operator/ (const T &left, const Array<T> &right)
{
    Array<T> tmp(right.shape());
    tmp = left;
    tmp /= right;
    return tmp;
}


// <thrown>
//   </item> ArrayError
// </thrown>
template<class T> void minMax(T &min, T &max, const Array<T> &a)
{
    uInt ntotal = a.nelements();
    if (ntotal == 0) {
	throw(ArrayError("void minMax(T &min, T &max, const Array<T> &a) - "
			 "Array has no elements"));
    }

    Bool deleteIt;
    const T *storage = a.getStorage(deleteIt);

    min = *storage;
    max = *storage;

    // Account for the fact we've seen the first position
    ntotal--;
    const T *ts = storage + 1;

    while (ntotal--) {
	min = (min < (*ts)) ? min : (*ts);
	max = (max > (*ts)) ? max : (*ts);
	ts++;
    }

    a.freeStorage(storage, deleteIt);
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<class T> void max(Array<T> &result, const Array<T> &a, 
			   const Array<T> &b)
{
    if (result.nelements() == 0 && a.nelements() == 0 && b.nelements() == 0) {
	return; // short circuit
    }

    if (result.nelements() != a.nelements() || a.nelements() != b.nelements()) {
	throw(ArrayConformanceError(" void max(Array<T> &result, const Array<T>"
				    " &a, const Array<T> &b) - result, a and b "
				    " do not have the same nelements()"));
    }
    Bool delr, dela, delb;
    T *sr = result.getStorage(delr);
    const T *sa = a.getStorage(dela);
    const T *sb = b.getStorage(delb);
    uInt n = result.nelements();
    while (n) {
	n--;
	sr[n] = sa[n] > sb[n] ? sa[n] : sb[n];
    }
    result.putStorage(sr, delr);
    a.freeStorage(sa, dela);
    b.freeStorage(sb, delb);
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<class T> void min(Array<T> &result, const Array<T> &a, 
			   const Array<T> &b)
{
    if (result.nelements() == 0 && a.nelements() == 0 && b.nelements() == 0) {
	return; // short circuit
    }

    if (result.nelements() != a.nelements() || a.nelements() != b.nelements()) {
	throw(ArrayConformanceError(" void min(Array<T> &result, const Array<T>"
				    " &a, const Array<T> &b) - result, a and b "
				    " do not have the same nelements()"));
    }
    Bool delr, dela, delb;
    T *sr = result.getStorage(delr);
    const T *sa = a.getStorage(dela);
    const T *sb = b.getStorage(delb);
    uInt n = result.nelements();
    while (n) {
	n--;
	sr[n] = sa[n] < sb[n] ? sa[n] : sb[n];
    }
    result.putStorage(sr, delr);
    a.freeStorage(sa, dela);
    b.freeStorage(sb, delb);
}

template<class T> Array<T> max(const Array<T> &a, const Array<T> &b)
{
    Array<T> result(a.shape());
    max(result, a, b);
    return result;
}

template<class T> Array<T> min(const Array<T> &a, const Array<T> &b)
{
    Array<T> result(a.shape());
    min(result, a, b);
    return result;
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<class T> void max(Array<T> &result, const Array<T> &a, 
			   const T &b)
{
    if (result.nelements() == 0 && a.nelements() == 0) {
	return; // short circuit
    }

    if (result.nelements() != a.nelements()) {
	throw(ArrayConformanceError(" void max(Array<T> &result, const Array<T>"
				    " &a, const T &b) - result and a "
				    " do not have the same nelements()"));
    }
    Bool delr, dela;
    T *sr = result.getStorage(delr);
    const T *sa = a.getStorage(dela);
    uInt n = result.nelements();
    while (n) {
	n--;
	sr[n] = sa[n] > b ? sa[n] : b;
    }
    result.putStorage(sr, delr);
    a.freeStorage(sa, dela);
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<class T> void min(Array<T> &result, const Array<T> &a, 
			   const T &b)
{
    if (result.nelements() == 0 && a.nelements() == 0) {
	return; // short circuit
    }

    if (result.nelements() != a.nelements()) {
	throw(ArrayConformanceError(" void min(Array<T> &result, const Array<T>"
				    " &a, const T &b) - result and a "
				    " do not have the same nelements()"));
    }
    Bool delr, dela;
    T *sr = result.getStorage(delr);
    const T *sa = a.getStorage(dela);
    uInt n = result.nelements();
    while (n) {
	n--;
	sr[n] = sa[n] < b ? sa[n] : b;
    }
    result.putStorage(sr, delr);
    a.freeStorage(sa, dela);
}

template<class T> Array<T> max(const Array<T> &a, const T &b)
{
    Array<T> result(a.shape());
    max(result, a, b);
    return result;
}

template<class T> Array<T> min(const Array<T> &a, const T &b)
{
    Array<T> result(a.shape());
    min(result, a, b);
    return result;
}

template<class T>
void indgen(Array<T> &a, T start, T inc)
{
    uInt ntotal = a.nelements();
    Bool deleteIt;
    T *storage = a.getStorage(deleteIt);
    T *ts = storage;
    
    while (ntotal--) {
	*ts = start;
	ts++;
        start += inc;
    }

    a.putStorage(storage, deleteIt);
}

template<class T> Array<T> cos(const Array<T> &a)
{
    uInt ntotal = a.nelements();
    Array<T> tmp(a.copy());
    Bool deleteIt;
    T *storage = tmp.getStorage(deleteIt);
    T *ts = storage;

    while (ntotal--) {
	*ts = cos(*ts);
	ts++;
    }

    tmp.putStorage(storage, deleteIt); // should be a no-op
    return tmp;
}

template<class T> Array<T> cosh(const Array<T> &a)
{
    uInt ntotal = a.nelements();
    Array<T> tmp(a.copy());
    Bool deleteIt;
    T *storage = tmp.getStorage(deleteIt);
    T *ts = storage;

    while (ntotal--) {
	*ts = cosh(*ts);
	ts++;
    }

    tmp.putStorage(storage, deleteIt); // should be a no-op
    return tmp;
}

template<class T> Array<T> exp(const Array<T> &a)
{
    uInt ntotal = a.nelements();
    Array<T> tmp(a.copy());
    Bool deleteIt;
    T *storage = tmp.getStorage(deleteIt);
    T *ts = storage;

    while (ntotal--) {
	*ts = exp(*ts);
	ts++;
    }

    tmp.putStorage(storage, deleteIt); // should be a no-op
    return tmp;
}

template<class T> Array<T> log(const Array<T> &a)
{
    uInt ntotal = a.nelements();
    Array<T> tmp(a.copy());
    Bool deleteIt;
    T *storage = tmp.getStorage(deleteIt);
    T *ts = storage;

    while (ntotal--) {
	*ts = log(*ts);
	ts++;
    }

    tmp.putStorage(storage, deleteIt); // should be a no-op
    return tmp;
}

template<class T> Array<T> log10(const Array<T> &a)
{
    uInt ntotal = a.nelements();
    Array<T> tmp(a.copy());
    Bool deleteIt;
    T *storage = tmp.getStorage(deleteIt);
    T *ts = storage;

    while (ntotal--) {
	*ts = log10(*ts);
	ts++;
    }

    tmp.putStorage(storage, deleteIt); // should be a no-op
    return tmp;
}

template<class T> Array<T> sin(const Array<T> &a)
{
    uInt ntotal = a.nelements();
    Array<T> tmp(a.copy());
    Bool deleteIt;
    T *storage = tmp.getStorage(deleteIt);
    T *ts = storage;

    while (ntotal--) {
	*ts = sin(*ts);
	ts++;
    }

    tmp.putStorage(storage, deleteIt); // should be a no-op
    return tmp;
}

template<class T> Array<T> sinh(const Array<T> &a)
{
    uInt ntotal = a.nelements();
    Array<T> tmp(a.copy());
    Bool deleteIt;
    T *storage = tmp.getStorage(deleteIt);
    T *ts = storage;

    while (ntotal--) {
	*ts = sinh(*ts);
	ts++;
    }

    tmp.putStorage(storage, deleteIt); // should be a no-op
    return tmp;
}

template<class T> Array<T> sqrt(const Array<T> &a)
{
    uInt ntotal = a.nelements();
    Array<T> tmp(a.copy());
    Bool deleteIt;
    T *storage = tmp.getStorage(deleteIt);
    T *ts = storage;

    while (ntotal--) {
	*ts = sqrt(*ts);
	ts++;
    }

    tmp.putStorage(storage, deleteIt); // should be a no-op
    return tmp;
}

template<class T> Array<T> acos(const Array<T> &a)
{
    uInt ntotal = a.nelements();
    Array<T> tmp(a.copy());
    Bool deleteIt;
    T *storage = tmp.getStorage(deleteIt);
    T *ts = storage;

    while (ntotal--) {
	*ts = acos(*ts);
	ts++;
    }

    tmp.putStorage(storage, deleteIt); // should be a no-op
    return tmp;
}

template<class T> Array<T> asin(const Array<T> &a)
{
    uInt ntotal = a.nelements();
    Array<T> tmp(a.copy());
    Bool deleteIt;
    T *storage = tmp.getStorage(deleteIt);
    T *ts = storage;

    while (ntotal--) {
	*ts = asin(*ts);
	ts++;
    }

    tmp.putStorage(storage, deleteIt); // should be a no-op
    return tmp;
}

template<class T> Array<T> atan(const Array<T> &a)
{
    uInt ntotal = a.nelements();
    Array<T> tmp(a.copy());
    Bool deleteIt;
    T *storage = tmp.getStorage(deleteIt);
    T *ts = storage;

    while (ntotal--) {
	*ts = atan(*ts);
	ts++;
    }

    tmp.putStorage(storage, deleteIt); // should be a no-op
    return tmp;
}

template<class T> Array<T> ceil(const Array<T> &a)
{
    uInt ntotal = a.nelements();
    Array<T> tmp(a.copy());
    Bool deleteIt;
    T *storage = tmp.getStorage(deleteIt);
    T *ts = storage;

    while (ntotal--) {
	*ts = ceil(*ts);
	ts++;
    }

    tmp.putStorage(storage, deleteIt); // should be a no-op
    return tmp;
}

template<class T> Array<T> fabs(const Array<T> &a)
{
    uInt ntotal = a.nelements();
    Array<T> tmp(a.copy());
    Bool deleteIt;
    T *storage = tmp.getStorage(deleteIt);
    T *ts = storage;

    while (ntotal--) {
	*ts = fabs(*ts);
	ts++;
    }

    tmp.putStorage(storage, deleteIt); // should be a no-op
    return tmp;
}

template<class T> Array<T> abs(const Array<T> &a)
{
    uInt ntotal = a.nelements();
    Array<T> tmp(a.copy());
    Bool deleteIt;
    T *storage = tmp.getStorage(deleteIt);
    T *ts = storage;

    while (ntotal--) {
	*ts = abs(*ts);
	ts++;
    }

    tmp.putStorage(storage, deleteIt); // should be a no-op
    return tmp;
}

template<class T> Array<T> floor(const Array<T> &a)
{
    uInt ntotal = a.nelements();
    Array<T> tmp(a.copy());
    Bool deleteIt;
    T *storage = tmp.getStorage(deleteIt);
    T *ts = storage;

    while (ntotal--) {
	*ts = floor(*ts);
	ts++;
    }

    tmp.putStorage(storage, deleteIt); // should be a no-op
    return tmp;
}

template<class T> Array<T> tan(const Array<T> &a)
{
    uInt ntotal = a.nelements();
    Array<T> tmp(a.copy());
    Bool deleteIt;
    T *storage = tmp.getStorage(deleteIt);
    T *ts = storage;

    while (ntotal--) {
	*ts = tan(*ts);
	ts++;
    }

    tmp.putStorage(storage, deleteIt); // should be a no-op
    return tmp;
}

template<class T> Array<T> tanh(const Array<T> &a)
{
    uInt ntotal = a.nelements();
    Array<T> tmp(a.copy());
    Bool deleteIt;
    T *storage = tmp.getStorage(deleteIt);
    T *ts = storage;

    while (ntotal--) {
	*ts = tanh(*ts);
	ts++;
    }

    tmp.putStorage(storage, deleteIt); // should be a no-op
    return tmp;
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<class T> Array<T> pow(const Array<T> &a, const Array<T> &b)
{
    if (a.conform(b) == False) {
	throw(ArrayConformanceError("pow(const Array<T> &a, const Array<T> &b) - "
				    "- a and b not conformant"));
    }

    uInt ntotal = a.nelements();
    Array<T> tmp(a.copy());
    Bool deleteIt;
    T *storage = tmp.getStorage(deleteIt);
    T *ts = storage;

    Bool deleteOther;
    const T *otherStorage = b.getStorage(deleteOther);
    const T *ots = otherStorage;

    while (ntotal--) {
	*ts = pow(*ts,*ots);
	ts++;
	ots++;
    }

    tmp.putStorage(storage, deleteIt); // should be a no-op
    b.freeStorage(otherStorage, deleteOther);

    return tmp;
}

template<class T> Array<T> pow(const T &a, const Array<T> &b)
{
    uInt ntotal = b.nelements();
    Array<T> tmp(b.copy());
    Bool deleteIt;
    T *storage = tmp.getStorage(deleteIt);
    T *ts = storage;

    while (ntotal--) {
	*ts = pow(a,*ts);
	ts++;
    }

    tmp.putStorage(storage, deleteIt); // should be a no-op

    return tmp;
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<class T> Array<T> atan2(const Array<T> &a, const Array<T> &b)
{
    if (a.conform(b) == False) {
	throw(ArrayConformanceError("atan2(const Array<T>&a,const Array<T>&b) "
				    "- a and b not conformant"));
    }

    uInt ntotal = a.nelements();
    Array<T> tmp(a.copy());
    Bool deleteIt;
    T *storage = tmp.getStorage(deleteIt);
    T *ts = storage;

    Bool deleteOther;
    const T *otherStorage = b.getStorage(deleteOther);
    const T *ots = otherStorage;

    while (ntotal--) {
	*ts = atan2(*ts,*ots);
	ts++;
	ots++;
    }

    tmp.putStorage(storage, deleteIt); // should be a no-op
    b.freeStorage(otherStorage, deleteOther);

    return tmp;
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<class T> Array<T> fmod(const Array<T> &a, const Array<T> &b)
{
    if (a.conform(b) == False) {
	throw(ArrayConformanceError("fmod(const Array<T>&a,const Array<T>&b) "
				    "- a and b not conformant"));
    }

    uInt ntotal = a.nelements();
    Array<T> tmp(a.copy());
    Bool deleteIt;
    T *storage = tmp.getStorage(deleteIt);
    T *ts = storage;

    Bool deleteOther;
    const T *otherStorage = b.getStorage(deleteOther);
    const T *ots = otherStorage;

    while (ntotal--) {
	*ts = fmod(*ts,*ots);
	ts++;
	ots++;
    }

    tmp.putStorage(storage, deleteIt); // should be a no-op
    b.freeStorage(otherStorage, deleteOther);

    return tmp;
}

template<class T> Array<T> fmod(const T &a, const Array<T> &b)
{
    uInt ntotal = b.nelements();
    Array<T> tmp(b.copy());
    Bool deleteIt;
    T *storage = tmp.getStorage(deleteIt);
    T *ts = storage;

    while (ntotal--) {
	*ts = fmod(a,*ts);
	ts++;
    }

    tmp.putStorage(storage, deleteIt); // should be a no-op

    return tmp;
}

template<class T> Array<T> fmod(const Array<T> &a, const T &b)
{
    uInt ntotal = a.nelements();
    Array<T> tmp(a.copy());
    Bool deleteIt;
    T *storage = tmp.getStorage(deleteIt);
    T *ts = storage;

    while (ntotal--) {
	*ts = fmod(*ts,b);
	ts++;
    }

    tmp.putStorage(storage, deleteIt); // should be a no-op

    return tmp;
}

template<class T> Array<T> pow(const Array<T> &a, const Double &b)
{
    uInt ntotal = a.nelements();
    Array<T> tmp(a.copy());
    Bool deleteIt;
    T *storage = tmp.getStorage(deleteIt);
    T *ts = storage;

    if (b == 2) {
	while (ntotal--) {
	    *ts *= *ts;
	    ts++;
	}
    } else {
	while (ntotal--) {
	    *ts = pow(*ts,b);
	    ts++;
	}
    }

    tmp.putStorage(storage, deleteIt); // should be a no-op
    return tmp;
}


// <thrown>
//    </item> ArrayError
// </thrown>
template<class T> T sum(const Array<T> &a)
{
    uInt ntotal = a.nelements();
    if (ntotal == 0) {
	throw(ArrayError("void sum(const Array<T> &a) - "
			 "Array has no elements"));
    }

    Bool deleteIt;
    const T *storage = a.getStorage(deleteIt);

    T sum = *storage;
    // Account for the fact we've seen the first position
    ntotal--;
    const T *ts = storage + 1;

    while (ntotal--) {
	sum += *ts++;
    }
    a.freeStorage(storage, deleteIt);
    return sum;
}

// <thrown>
//    </item> ArrayError
// </thrown>
template<class T> T product(const Array<T> &a)
{
    uInt ntotal = a.nelements();
    if (ntotal == 0) {
	throw(ArrayError("void product(const Array<T> &a) - "
			 "Array has no elements"));
    }

    Bool deleteIt;
    const T *storage = a.getStorage(deleteIt);

    T prod = *storage;
    // Account for the fact we've seen the first position
    ntotal--;
    const T *ts = storage + 1;

    while (ntotal--) {
	prod *= *ts++;
    }
    a.freeStorage(storage, deleteIt);
    return prod;
}

// <thrown>
//    </item> ArrayError
// </thrown>
template<class T> T mean(const Array<T> &a)
{
    if (a.nelements() == 0) {
	throw(ArrayError("::mean(const Array<T> &) - 0 element array"));
    }
    return T(sum(a)/(1.0*a.nelements()));
}

// <thrown>
//    </item> ArrayError
// </thrown>
template<class T> T variance(const Array<T> &a, T mean)
{
    if (a.nelements() < 2) {
	throw(ArrayError("::variance(const Array<T> &,T) - Need at least 2 "
			 "elements"));
    }
    uInt ntotal = a.nelements();
    Bool deleteIt;
    const T* data = a.getStorage(deleteIt);
    T sum = 0;
    for (uInt i=0; i<ntotal; ++i) {
        T tmp = data[i] - mean;
        sum += tmp*tmp;
    }
    a.freeStorage(data, deleteIt);
    return T(sum/(1.0*ntotal - 1));
}

// <thrown>
//    </item> ArrayError
// </thrown>
template<class T> T variance(const Array<T> &a)
{
    if (a.nelements() < 2) {
	throw(ArrayError("::variance(const Array<T> &) - Need at least 2 "
			 "elements"));
    }
    return variance(a, mean(a));
}

// <thrown>
//    </item> ArrayError
// </thrown>
template<class T> T stddev(const Array<T> &a)
{
    if (a.nelements() < 2) {
	throw(ArrayError("::stddev(const Array<T> &) - Need at least 2 "
			 "elements"));
    }
    return sqrt(variance(a));
}

// <thrown>
//    </item> ArrayError
// </thrown>
template<class T> T stddev(const Array<T> &a, T mean)
{
    if (a.nelements() < 2) {
	throw(ArrayError("::stddev(const Array<T> &,T) - Need at least 2 "
			 "elements"));
    }
    return sqrt(variance(a, mean));
}

// <thrown>
//    </item> ArrayError
// </thrown>
template<class T> T avdev(const Array<T> &a)
{
    if (a.nelements() < 1) {
	throw(ArrayError("::avdev(const Array<T> &,) - Need at least 1 "
			 "element"));
    }
    return avdev(a, mean(a));
}

// <thrown>
//    </item> ArrayError
// </thrown>
template<class T> T avdev(const Array<T> &a, T mean)
{
    if (a.nelements() < 1) {
	throw(ArrayError("::avdev(const Array<T> &,T) - Need at least 1 "
			 "element"));
    }
    uInt ntotal = a.nelements();
    Bool deleteIt;
    const T* data = a.getStorage(deleteIt);
    T sum = 0;
    for (uInt i=0; i<ntotal; ++i) {
        sum += fabs(data[i] - mean);
    }
    a.freeStorage(data, deleteIt);
    return T(sum/(1.0*ntotal));
}

// <thrown>
//    </item> ArrayError
// </thrown>
template<class T> T rms(const Array<T> &a)
{
    if (a.nelements() < 1) {
	throw(ArrayError("::rms(const Array<T> &) - Need at least 1 "
			 "element"));
    }
    uInt ntotal = a.nelements();
    Bool deleteIt;
    const T* data = a.getStorage(deleteIt);
    T sum = 0;
    for (uInt i=0; i<ntotal; ++i) {
	sum += data[i] * data[i];
    }
    a.freeStorage(data, deleteIt);
    return T(sqrt(sum/(1.0*ntotal)));
}

// <thrown>
//    </item> ArrayError
// </thrown>
template<class T> T median(const Array<T> &a, Bool sorted,
			   Bool takeEvenMean, Bool inPlace)
{
    uInt nelem = a.nelements();
    if (nelem < 1) {
	throw(ArrayError("::median(const Array<T>&) - Need at least 1 "
			 "elements"));
    }
    //# Mean does not have to be taken for odd number of elements.
    if (nelem%2 != 0) {
	takeEvenMean = False;
    }
    T medval = T();
    Bool deleteIt;
    const T *storage = a.getStorage(deleteIt);
    T *data = const_cast<T*>(storage);
    T *copy = 0;
    uInt n2 = (nelem - 1)/2;
    if (!sorted) {
	// Sort a copy (if not inPlace).
	// If deleteIt is true, storage already points to copied storage;
        // So we can optimize away a possible copy in that case by
        // casting away const and sorting in place.
        if (!deleteIt && !inPlace) {
	    copy = new T[nelem];
	    memcpy (copy, storage, nelem*sizeof(T));
	    data = copy;
	}
	// If needed take the mean for an even number of elements.
	// If the array is small, it is faster to fully sort it.
	if (nelem > 50) {
	    medval = GenSort<T>::kthLargest (data, nelem, n2);
	    if (takeEvenMean) {
		medval = T(0.5 * (medval +
				  GenSort<T>::kthLargest (data, nelem, n2+1)));
	    }
	} else {
	    GenSort<T>::sort (data, nelem);
	    sorted = True;
	}
    }
    if (sorted) {
	if (takeEvenMean) {
	    medval = T(0.5 * (data[n2] + data[n2+1]));
	} else {
	    medval = data[n2];
	}
    }
    delete [] copy;
    a.freeStorage(storage, deleteIt);
    return medval;
}

// <thrown>
//    </item> ArrayError
// </thrown>
template<class T> T fractile(const Array<T> &a, Float fraction, Bool sorted,
			     Bool inPlace)
{
    if (fraction < 0  ||  fraction > 1) {
        throw(ArrayError("::fractile(const Array<T>&) - fraction <0 or >1 "));
    }    
    uInt nelem = a.nelements();
    if (nelem < 1) {
	throw(ArrayError("::fractile(const Array<T>&) - Need at least 1 "
			 "elements"));
    }
    T fracval = T();
    Bool deleteIt;
    const T *storage = a.getStorage(deleteIt);
    T *data = const_cast<T*>(storage);
    T *copy = 0;
    uInt n2 = uInt((nelem - 1) * fraction);
    if (!sorted) {
	// Sort a copy (if not inPlace).
	// If deleteIt is true, storage already points to copied storage;
        // So we can optimize away a possible copy in that case by
        // casting away const and sorting in place.
        if (!deleteIt && !inPlace) {
	    copy = new T[nelem];
	    memcpy (copy, storage, nelem*sizeof(T));
	    data = copy;
	}
	// If the array is small, it is faster to fully sort it.
	if (nelem > 50) {
	    fracval = GenSort<T>::kthLargest (data, nelem, n2);
	} else {
	    GenSort<T>::sort (data, nelem);
	    sorted = True;
	}
    }
    if (sorted) {
        fracval = data[n2];
    }
    delete [] copy;
    a.freeStorage(storage, deleteIt);
    return fracval;
}


template<class T, class U> void convertArray(Array<T> &to,
					     const Array<U> &from)
{
    if (to.nelements() == 0 && from.nelements() == 0) {
	return;
    }
    if (to.shape() != from.shape()) {
	throw(ArrayConformanceError("void ::convertArray(Array<T> &to, "
				    "const Array<U> &from)"
				    " - arrays do not conform"));
    }

    Bool deleteTo, deleteFrom;
    T *toptr = to.getStorage(deleteTo);
    const U *fromptr = from.getStorage(deleteFrom);
    uInt n = from.nelements();
    for (uInt i=0; i<n; i++) {
	convertScalar (toptr[i], fromptr[i]);
    }
    to.putStorage(toptr, deleteTo);
    from.freeStorage(fromptr, deleteFrom);
}


template<class T> Array<T> partialSums (const Array<T>& array,
					const IPosition& collapseAxes)
{
  if (collapseAxes.nelements() == 0) {
    return array.copy();
  }
  const IPosition& shape = array.shape();
  uInt ndim = shape.nelements();
  if (ndim == 0) {
    return Array<T>();
  }
  IPosition resShape, incr;
  Int nelemCont = 0;
  uInt stax = partialFuncHelper (nelemCont, resShape, incr, shape,
				 collapseAxes);
  Array<T> result (resShape);
  result = 0;
  Bool deleteData, deleteRes;
  const T* arrData = array.getStorage (deleteData);
  const T* data = arrData;
  T* resData = result.getStorage (deleteRes);
  T* res = resData;
  // Find out how contiguous the data is, i.e. if some contiguous data
  // end up in the same output element.
  // cont tells if any data are contiguous.
  // stax gives the first non-contiguous axis.
  // n0 gives the number of contiguous elements.
  Bool cont = True;
  uInt n0 = nelemCont;
  Int incr0 = incr(0);
  if (nelemCont <= 1) {
    cont = False;
    n0 = shape(0);
    stax = 1;
  }
  // Loop through all data and assemble as needed.
  IPosition pos(ndim, 0);
  while (True) {
    if (cont) {
      T tmp = *res;
      for (uInt i=0; i<n0; i++) {
	tmp += *data++;
      }
      *res = tmp;
    } else {
      for (uInt i=0; i<n0; i++) {
	*res += *data++;
	res += incr0;
      }
    }
    uInt ax;
    for (ax=stax; ax<ndim; ax++) {
      res += incr(ax);
      if (++pos(ax) < shape(ax)) {
	break;
      }
      pos(ax) = 0;
    }
    if (ax == ndim) {
      break;
    }
  }
  array.freeStorage (arrData, deleteData);
  result.putStorage (resData, deleteRes);
  return result;
}

template<class T> Array<T> partialProducts (const Array<T>& array,
					    const IPosition& collapseAxes)
{
  if (collapseAxes.nelements() == 0) {
    return array.copy();
  }
  const IPosition& shape = array.shape();
  uInt ndim = shape.nelements();
  if (ndim == 0) {
    return Array<T>();
  }
  IPosition resShape, incr;
  Int nelemCont = 0;
  uInt stax = partialFuncHelper (nelemCont, resShape, incr, shape,
				 collapseAxes);
  Array<T> result (resShape);
  result = 0;
  Bool deleteData, deleteRes;
  const T* arrData = array.getStorage (deleteData);
  const T* data = arrData;
  T* resData = result.getStorage (deleteRes);
  T* res = resData;
  // Find out how contiguous the data is, i.e. if some contiguous data
  // end up in the same output element.
  // cont tells if any data are contiguous.
  // stax gives the first non-contiguous axis.
  // n0 gives the number of contiguous elements.
  Bool cont = True;
  uInt n0 = nelemCont;
  Int incr0 = incr(0);
  if (nelemCont <= 1) {
    cont = False;
    n0 = shape(0);
    stax = 1;
  }
  // Loop through all data and assemble as needed.
  IPosition pos(ndim, 0);
  while (True) {
    if (cont) {
      T tmp = *res;
      for (uInt i=0; i<n0; i++) {
	tmp *= *data++;
      }
      *res = tmp;
    } else {
      for (uInt i=0; i<n0; i++) {
	*res *= *data++;
	res += incr0;
      }
    }
    uInt ax;
    for (ax=stax; ax<ndim; ax++) {
      res += incr(ax);
      if (++pos(ax) < shape(ax)) {
	break;
      }
      pos(ax) = 0;
    }
    if (ax == ndim) {
      break;
    }
  }
  array.freeStorage (arrData, deleteData);
  result.putStorage (resData, deleteRes);
  return result;
}

template<class T> Array<T> partialMins (const Array<T>& array,
					const IPosition& collapseAxes)
{
  if (collapseAxes.nelements() == 0) {
    return array.copy();
  }
  const IPosition& shape = array.shape();
  uInt ndim = shape.nelements();
  if (ndim == 0) {
    return Array<T>();
  }
  IPosition resShape, incr;
  Int nelemCont = 0;
  uInt stax = partialFuncHelper (nelemCont, resShape, incr, shape,
				 collapseAxes);
  Array<T> result (resShape);
  result = 0;
  Bool deleteData, deleteRes;
  const T* arrData = array.getStorage (deleteData);
  const T* data = arrData;
  T* resData = result.getStorage (deleteRes);
  T* res = resData;
  // Initialize the minima with the first value of collapsed axes.
  IPosition end(shape-1);
  for (uInt i=0; i<collapseAxes.nelements(); i++) {
    uInt axis = collapseAxes(i);
    end(axis) = 0;
  }
  Array<T> tmp(array);           // to get a non-const array for operator()
  result = tmp(IPosition(ndim,0), end).reform (resShape);
  // Find out how contiguous the data is, i.e. if some contiguous data
  // end up in the same output element.
  // cont tells if any data are contiguous.
  // stax gives the first non-contiguous axis.
  // n0 gives the number of contiguous elements.
  Bool cont = True;
  uInt n0 = nelemCont;
  Int incr0 = incr(0);
  if (nelemCont <= 1) {
    cont = False;
    n0 = shape(0);
    stax = 1;
  }
  // Loop through all data and assemble as needed.
  IPosition pos(ndim, 0);
  while (True) {
    if (cont) {
      T tmp = *res;
      for (uInt i=0; i<n0; i++) {
	if (*data < tmp) {
	  tmp = *data;
	}
	data++;
      }
      *res = tmp;
    } else {
      for (uInt i=0; i<n0; i++) {
	if (*data < *res) {
	  *res = *data;
	}
	data++;
	res += incr0;
      }
    }
    uInt ax;
    for (ax=stax; ax<ndim; ax++) {
      res += incr(ax);
      if (++pos(ax) < shape(ax)) {
	break;
      }
      pos(ax) = 0;
    }
    if (ax == ndim) {
      break;
    }
  }
  array.freeStorage (arrData, deleteData);
  result.putStorage (resData, deleteRes);
  return result;
}

template<class T> Array<T> partialMaxs (const Array<T>& array,
					const IPosition& collapseAxes)
{
  if (collapseAxes.nelements() == 0) {
    return array.copy();
  }
  const IPosition& shape = array.shape();
  uInt ndim = shape.nelements();
  if (ndim == 0) {
    return Array<T>();
  }
  IPosition resShape, incr;
  Int nelemCont = 0;
  uInt stax = partialFuncHelper (nelemCont, resShape, incr, shape,
				 collapseAxes);
  Array<T> result (resShape);
  result = 0;
  Bool deleteData, deleteRes;
  const T* arrData = array.getStorage (deleteData);
  const T* data = arrData;
  T* resData = result.getStorage (deleteRes);
  T* res = resData;
  // Initialize the maxima with the first value of collapsed axes.
  IPosition end(shape-1);
  for (uInt i=0; i<collapseAxes.nelements(); i++) {
    uInt axis = collapseAxes(i);
    end(axis) = 0;
  }
  Array<T> tmp(array);           // to get a non-const array for operator()
  result = tmp(IPosition(ndim,0), end).reform (resShape);
  // Find out how contiguous the data is, i.e. if some contiguous data
  // end up in the same output element.
  // cont tells if any data are contiguous.
  // stax gives the first non-contiguous axis.
  // n0 gives the number of contiguous elements.
  Bool cont = True;
  uInt n0 = nelemCont;
  Int incr0 = incr(0);
  if (nelemCont <= 1) {
    cont = False;
    n0 = shape(0);
    stax = 1;
  }
  // Loop through all data and assemble as needed.
  IPosition pos(ndim, 0);
  while (True) {
    if (cont) {
      T tmp = *res;
      for (uInt i=0; i<n0; i++) {
	if (*data > tmp) {
	  tmp = *data;
	}
	data++;
      }
      *res = tmp;
    } else {
      for (uInt i=0; i<n0; i++) {
	if (*data > *res) {
	  *res = *data;
	}
	data++;
	res += incr0;
      }
    }
    uInt ax;
    for (ax=stax; ax<ndim; ax++) {
      res += incr(ax);
      if (++pos(ax) < shape(ax)) {
	break;
      }
      pos(ax) = 0;
    }
    if (ax == ndim) {
      break;
    }
  }
  array.freeStorage (arrData, deleteData);
  result.putStorage (resData, deleteRes);
  return result;
}

template<class T> Array<T> partialMeans (const Array<T>& array,
					 const IPosition& collapseAxes)
{
  if (collapseAxes.nelements() == 0) {
    return array.copy();
  }
  Array<T> result = partialSums (array, collapseAxes);
  uInt nr = result.nelements();
  if (nr > 0) {
    uInt factor = array.nelements() / nr;
    Bool deleteRes;
    T* res = result.getStorage (deleteRes);
    for (uInt i=0; i<nr; i++) {
      res[i] /= 1.0 * factor;
    }
    result.putStorage (res, deleteRes);
  }
  return result;
}

template<class T> Array<T> partialVariances (const Array<T>& array,
					     const IPosition& collapseAxes,
					     const Array<T>& means)
{
  const IPosition& shape = array.shape();
  uInt ndim = shape.nelements();
  if (ndim == 0) {
    return Array<T>();
  }
  IPosition resShape, incr;
  Int nelemCont = 0;
  uInt stax = partialFuncHelper (nelemCont, resShape, incr, shape,
				 collapseAxes);
  if (! resShape.isEqual (means.shape())) {
    throw AipsError ("partialVariances: shape of means array mismatches "
		     "shape of result array");
  }
  Array<T> result (resShape);
  result = 0;
  uInt nr = result.nelements();
  uInt factor = array.nelements() / nr - 1;
  if (factor == 0) {
    return result;
  }
  Bool deleteData, deleteRes, deleteMean;
  const T* arrData = array.getStorage (deleteData);
  const T* data = arrData;
  const T* meanData = means.getStorage (deleteMean);
  const T* mean = meanData;
  T* resData = result.getStorage (deleteRes);
  T* res = resData;
  // Find out how contiguous the data is, i.e. if some contiguous data
  // end up in the same output element.
  // cont tells if any data are contiguous.
  // stax gives the first non-contiguous axis.
  // n0 gives the number of contiguous elements.
  Bool cont = True;
  uInt n0 = nelemCont;
  Int incr0 = incr(0);
  if (nelemCont <= 1) {
    cont = False;
    n0 = shape(0);
    stax = 1;
  }
  // Loop through all data and assemble as needed.
  IPosition pos(ndim, 0);
  while (True) {
    if (cont) {
      T tmp = *res;
      T tmpm = *mean;
      for (uInt i=0; i<n0; i++) {
	T var = *data++ - tmpm;
	tmp += var*var;
      }
      *res = tmp;
    } else {
      for (uInt i=0; i<n0; i++) {
	T var = *data++ - *mean;
	*res += var*var;
	res += incr0;
	mean += incr0;
      }
    }
    uInt ax;
    for (ax=stax; ax<ndim; ax++) {
      res += incr(ax);
      mean += incr(ax);
      if (++pos(ax) < shape(ax)) {
	break;
      }
      pos(ax) = 0;
    }
    if (ax == ndim) {
      break;
    }
  }
  res = resData;
  for (uInt i=0; i<nr; i++) {
    res[i] /= 1.0 * factor;
  }
  array.freeStorage (arrData, deleteData);
  means.freeStorage (meanData, deleteMean);
  result.putStorage (resData, deleteRes);
  return result;
}

template<class T> Array<T> partialAvdevs (const Array<T>& array,
					  const IPosition& collapseAxes,
					  const Array<T>& means)
{
  const IPosition& shape = array.shape();
  uInt ndim = shape.nelements();
  if (ndim == 0) {
    return Array<T>();
  }
  IPosition resShape, incr;
  Int nelemCont = 0;
  uInt stax = partialFuncHelper (nelemCont, resShape, incr, shape,
				 collapseAxes);
  if (! resShape.isEqual (means.shape())) {
    throw AipsError ("partialAvdevs: shape of means array mismatches "
		     "shape of result array");
  }
  Array<T> result (resShape);
  result = 0;
  uInt nr = result.nelements();
  uInt factor = array.nelements() / nr;
  Bool deleteData, deleteRes, deleteMean;
  const T* arrData = array.getStorage (deleteData);
  const T* data = arrData;
  const T* meanData = means.getStorage (deleteMean);
  const T* mean = meanData;
  T* resData = result.getStorage (deleteRes);
  T* res = resData;
  // Find out how contiguous the data is, i.e. if some contiguous data
  // end up in the same output element.
  // cont tells if any data are contiguous.
  // stax gives the first non-contiguous axis.
  // n0 gives the number of contiguous elements.
  Bool cont = True;
  uInt n0 = nelemCont;
  Int incr0 = incr(0);
  if (nelemCont <= 1) {
    cont = False;
    n0 = shape(0);
    stax = 1;
  }
  // Loop through all data and assemble as needed.
  IPosition pos(ndim, 0);
  while (True) {
    if (cont) {
      T tmp = *res;
      T tmpm = *mean;
      for (uInt i=0; i<n0; i++) {
	tmp += fabs(*data++ - tmpm);
      }
      *res = tmp;
    } else {
      for (uInt i=0; i<n0; i++) {
	*res += fabs(*data++ - *mean);
	res += incr0;
	mean += incr0;
      }
    }
    uInt ax;
    for (ax=stax; ax<ndim; ax++) {
      res += incr(ax);
      mean += incr(ax);
      if (++pos(ax) < shape(ax)) {
	break;
      }
      pos(ax) = 0;
    }
    if (ax == ndim) {
      break;
    }
  }
  res = resData;
  for (uInt i=0; i<nr; i++) {
    res[i] /= 1.0 * factor;
  }
  array.freeStorage (arrData, deleteData);
  means.freeStorage (meanData, deleteMean);
  result.putStorage (resData, deleteRes);
  return result;
}

template<class T> Array<T> partialRmss (const Array<T>& array,
					const IPosition& collapseAxes)
{
  if (collapseAxes.nelements() == 0) {
    return array.copy();
  }
  const IPosition& shape = array.shape();
  uInt ndim = shape.nelements();
  if (ndim == 0) {
    return Array<T>();
  }
  IPosition resShape, incr;
  Int nelemCont = 0;
  uInt stax = partialFuncHelper (nelemCont, resShape, incr, shape,
				 collapseAxes);
  Array<T> result (resShape);
  result = 0;
  uInt nr = result.nelements();
  uInt factor = array.nelements() / nr;
  Bool deleteData, deleteRes;
  const T* arrData = array.getStorage (deleteData);
  const T* data = arrData;
  T* resData = result.getStorage (deleteRes);
  T* res = resData;
  // Find out how contiguous the data is, i.e. if some contiguous data
  // end up in the same output element.
  // cont tells if any data are contiguous.
  // stax gives the first non-contiguous axis.
  // n0 gives the number of contiguous elements.
  Bool cont = True;
  uInt n0 = nelemCont;
  Int incr0 = incr(0);
  if (nelemCont <= 1) {
    cont = False;
    n0 = shape(0);
    stax = 1;
  }
  // Loop through all data and assemble as needed.
  IPosition pos(ndim, 0);
  while (True) {
    if (cont) {
      T tmp = *res;
      for (uInt i=0; i<n0; i++) {
	tmp += *data * *data;
	data++;
      }
      *res = tmp;
    } else {
      for (uInt i=0; i<n0; i++) {
	*res += *data * *data;
	data++;
	res += incr0;
      }
    }
    uInt ax;
    for (ax=stax; ax<ndim; ax++) {
      res += incr(ax);
      if (++pos(ax) < shape(ax)) {
	break;
      }
      pos(ax) = 0;
    }
    if (ax == ndim) {
      break;
    }
  }
  res = resData;
  for (uInt i=0; i<nr; i++) {
    res[i] = T(sqrt (res[i] / factor));
  }
  array.freeStorage (arrData, deleteData);
  result.putStorage (resData, deleteRes);
  return result;
}

template<class T> Array<T> partialMedians (const Array<T>& array,
					   const IPosition& collapseAxes,
					   Bool takeEvenMean,
					   Bool inPlace)
{
  // Need to make shallow copy because operator() is non-const.
  Array<T> arr = array;
  // Is there anything to collapse?
  if (collapseAxes.nelements() == 0) {
    return (inPlace  ?  array : array.copy());
  }
  const IPosition& shape = array.shape();
  uInt ndim = shape.nelements();
  if (ndim == 0) {
    return Array<T>();
  }
  // Get the remaining axes.
  // It also checks if axes are specified correctly.
  IPosition resAxes = IPosition::otherAxes (ndim, collapseAxes);
  uInt ndimRes = resAxes.nelements();
  // Create the result shape.
  // Create blc and trc to step through the input array.
  IPosition resShape(ndimRes);
  IPosition blc(ndim, 0);
  IPosition trc(shape-1);
  for (uInt i=0; i<ndimRes; ++i) {
    resShape[i] = shape[resAxes[i]];
    trc[resAxes[i]] = 0;
  }
  if (ndimRes == 0) {
    resShape.resize(1);
    resShape[0] = 1;
  }
  Array<T> result (resShape);
  Bool deleteRes;
  T* resData = result.getStorage (deleteRes);
  T* res = resData;
  // Loop through all data and assemble as needed.
  IPosition pos(ndimRes, 0);
  while (True) {
    *res++ = median(arr(blc,trc), False, takeEvenMean, inPlace);
    uInt ax;
    for (ax=0; ax<ndimRes; ax++) {
      if (++pos(ax) < resShape(ax)) {
	blc[resAxes[ax]]++;
	trc[resAxes[ax]]++;
	break;
      }
      pos(ax) = 0;
      blc[resAxes[ax]] = 0;
      trc[resAxes[ax]] = 0;
    }
    if (ax == ndimRes) {
      break;
    }
  }
  result.putStorage (resData, deleteRes);
  return result;
}

template<class T> Array<T> partialFractiles (const Array<T>& array,
					     const IPosition& collapseAxes,
					     Float fraction,
					     Bool inPlace)
{
  if (fraction < 0  ||  fraction > 1) {
    throw(ArrayError("::fractile(const Array<T>&) - fraction <0 or >1 "));
  }    
  // Need to make shallow copy because operator() is non-const.
  Array<T> arr = array;
  // Is there anything to collapse?
  if (collapseAxes.nelements() == 0) {
    return (inPlace  ?  array : array.copy());
  }
  const IPosition& shape = array.shape();
  uInt ndim = shape.nelements();
  if (ndim == 0) {
    return Array<T>();
  }
  // Get the remaining axes.
  // It also checks if axes are specified correctly.
  IPosition resAxes = IPosition::otherAxes (ndim, collapseAxes);
  uInt ndimRes = resAxes.nelements();
  // Create the result shape.
  // Create blc and trc to step through the input array.
  IPosition resShape(ndimRes);
  IPosition blc(ndim, 0);
  IPosition trc(shape-1);
  for (uInt i=0; i<ndimRes; ++i) {
    resShape[i] = shape[resAxes[i]];
    trc[resAxes[i]] = 0;
  }
  if (ndimRes == 0) {
    resShape.resize(1);
    resShape[0] = 1;
  }
  Array<T> result (resShape);
  Bool deleteRes;
  T* resData = result.getStorage (deleteRes);
  T* res = resData;
  // Loop through all data and assemble as needed.
  IPosition pos(ndimRes, 0);
  while (True) {
    *res++ = fractile(arr(blc,trc), fraction, False, inPlace);
    uInt ax;
    for (ax=0; ax<ndimRes; ax++) {
      if (++pos(ax) < resShape(ax)) {
	blc[resAxes[ax]]++;
	trc[resAxes[ax]]++;
	break;
      }
      pos(ax) = 0;
      blc[resAxes[ax]] = 0;
      trc[resAxes[ax]] = 0;
    }
    if (ax == ndimRes) {
      break;
    }
  }
  result.putStorage (resData, deleteRes);
  return result;
}


template <typename T>
Array<T> boxedArrayMath (const Array<T>& array, const IPosition& boxSize,
			 T (*reductionFunc) (const Array<T>&))
{
  uInt ndim = array.ndim();
  const IPosition& shape = array.shape();
  // Set missing axes to 1.
  IPosition boxsz (boxSize);
  if (boxsz.size() != ndim) {
    uInt sz = boxsz.size();
    boxsz.resize (ndim);
    for (uInt i=sz; i<ndim; ++i) {
      boxsz[i] = 1;
    }
  }
  // Determine the output shape.
  IPosition resShape(ndim);
  for (uInt i=0; i<ndim; ++i) {
    // Set unspecified axes to full length.
    if (boxsz[i] <= 0  ||  boxsz[i] > shape[i]) {
      boxsz[i] = shape[i];
    }
    resShape[i] = (shape[i] + boxsz[i] - 1) / boxsz[i];
  }
  // Need to make shallow copy because operator() is non-const.
  Array<T> arr (array);
  Array<T> result (resShape);
  DebugAssert (result.contiguousStorage(), AipsError);
  T* res = result.data();
  // Loop through all data and assemble as needed.
  IPosition blc(ndim, 0);
  IPosition trc(boxsz-1);
  while (True) {
    *res++ = reductionFunc(arr(blc,trc));
    uInt ax;
    for (ax=0; ax<ndim; ++ax) {
      blc[ax] += boxsz[ax];
      if (blc[ax] < shape[ax]) {
	trc[ax] += boxsz[ax];
	if (trc[ax] >= shape[ax]) {
	  trc[ax] = shape[ax]-1;
	}
	break;
      }
      blc[ax] = 0;
      trc[ax] = boxsz[ax]-1;
    }
    if (ax == ndim) {
      break;
    }
  }
  return result;
}

template <typename T>
Array<T> slidingArrayMath (const Array<T>& array, const IPosition& halfBoxSize,
			   T (*reductionFunc) (const Array<T>&),
			   Bool fillEdge)
{
  uInt ndim = array.ndim();
  const IPosition& shape = array.shape();
  // Set full box size (-1) and resize/fill as needed.
  IPosition hboxsz (2*halfBoxSize);
  if (hboxsz.size() != ndim) {
    uInt sz = hboxsz.size();
    hboxsz.resize (ndim);
    for (uInt i=sz; i<hboxsz.size(); ++i) {
      hboxsz[i] = 0;
    }
  }
  // Determine the output shape. See if anything has to be done.
  IPosition resShape(ndim);
  for (uInt i=0; i<ndim; ++i) {
    resShape[i] = shape[i] - hboxsz[i];
    if (resShape[i] <= 0) {
      if (!fillEdge) {
	return Array<T>();
      }
      Array<T> res(shape);
      res = T();
      return res;
    }
  }
  // Need to make shallow copy because operator() is non-const.
  Array<T> arr (array);
  Array<T> result (resShape);
  if (arr.size() == 0) {
    return result;
  }
  DebugAssert (result.contiguousStorage(), AipsError);
  T* res = result.data();
  // Loop through all data and assemble as needed.
  IPosition blc(ndim, 0);
  IPosition trc(hboxsz);
  IPosition pos(ndim, 0);
  while (True) {
    *res++ = reductionFunc(arr(blc,trc));
    uInt ax;
    for (ax=0; ax<ndim; ++ax) {
      if (++pos[ax] < resShape[ax]) {
	blc[ax]++;
	trc[ax]++;
	break;
      }
      pos(ax) = 0;
      blc[ax] = 0;
      trc[ax] = hboxsz[ax];
    }
    if (ax == ndim) {
      break;
    }
  }
  if (!fillEdge) {
    return result;
  }
  Array<T> fullResult(shape);
  fullResult = T();
  hboxsz /= 2;
  fullResult(hboxsz, resShape+hboxsz-1) = result;
  return fullResult;
}

} //# NAMESPACE CASA - END
