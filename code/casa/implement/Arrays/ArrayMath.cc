//# ArrayMath.cc: Arithmetic functions defined on Arrays
//# Copyright (C) 1993,1994,1995,1996,1997,1998,1999
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

#include <iostream.h>

#include <aips/Arrays/ArrayMath.h>
#include <aips/Mathematics/Math.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayError.h>
#include <aips/Arrays/ArrayIter.h>
#include <aips/Arrays/VectorIter.h>
#include <aips/Utilities/GenSort.h>

//#### CFront (3.0.2) bug. The following typedefs are needed to get the position
//#### finding minMax functions to work (the range of types will need to be
//#### extended if more types are instantiated). This might be true of all
//#### functions that end up using iterators.

typedef VectorIterator<Float> vif;
typedef VectorIterator<Bool> vib;
typedef VectorIterator<Double> vid;
typedef VectorIterator<Int> vii;
typedef VectorIterator<Complex> vic;


// <thrown>
//   <item> ArrayError
// </thrown>
template<class ScalarType> 
void minMax(ScalarType &minVal, ScalarType &maxVal, 
	    IPosition &minPos, IPosition &maxPos,
	    const Array<ScalarType> &array) 
{
    if (minPos.nelements() != array.ndim() ||
	maxPos.nelements() != array.ndim() ) {
      throw(ArrayError("void minMax(T &min, T &max, IPosition &minPos,"
		       "IPosition &maxPos, const Array<T> &array) - "
                       "minPos, maxPos dimensionality inconsistent with array"));
    }
    if (array.nelements() == 0) {
        throw(ArrayError("void minMax(T &min, T &max, IPosition &minPos,"
			 "IPosition &maxPos, const Array<T> &array) - "
                         "Array has no elements"));	
    }

    ReadOnlyVectorIterator<ScalarType> ai(array);
    ScalarType val;

    // Initialize
    minPos = maxPos = IPosition (array.ndim(), 0);
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
    if (minPos.nelements() != array.ndim() ||
	maxPos.nelements() != array.ndim() ) {
      throw(ArrayError("void minMaxMasked(T &min, T &max, IPosition &minPos,"
		       "IPosition &maxPos, const Array<T> &array) - "
                       "minPos, maxPos dimensionality inconsistent with array"));
    }
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

    ReadOnlyVectorIterator<ScalarType> ai(array);
    ReadOnlyVectorIterator<ScalarType> mi(mask);
    ScalarType val;

    // Initialize
    minPos = maxPos = IPosition (array.ndim(), 0);
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
    if (minPos.nelements() != array.ndim() ||
	maxPos.nelements() != array.ndim() ) {
      throw(ArrayError("void minMax(T &min, T &max, IPosition &minPos,"
		       "IPosition &maxPos, const Array<T> &array, "
		       "const Array<Bool> &mask) - "
                       "minPos, maxPos dimensionality inconsistent with array"));
    }
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
    ReadOnlyVectorIterator<ScalarType> ai(array);
    ReadOnlyVectorIterator<Bool> mi(mask);

    // Initialize
    // have to find a valid value in array to init min and max
    Bool found=False;
    uInt ifound;
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
	throw(ArrayError("::sum(const Array<T> &) - 0 element array"));
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
    Array<T> deviations(a - mean);
    deviations *= deviations;
    return sum(deviations)/((1.0*a.nelements() - 1));
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
	throw(ArrayError("::variance(const Array<T> &,) - Need at least  "
			 "elements"));
    }
    return avdev(a, mean(a));
}

// <thrown>
//    </item> ArrayError
// </thrown>
template<class T> T avdev(const Array<T> &a,T mean)
{
    if (a.nelements() < 1) {
	throw(ArrayError("::variance(const Array<T> &,T) - Need at least 1 "
			 "elements"));
    }
    Array<T> avdeviations(fabs(a - mean));
    return sum(avdeviations)/(1.0*a.nelements());
}

// <thrown>
//    </item> ArrayError
// </thrown>
template<class T> T median(const Array<T> &a, Bool sorted,
			   Bool takeEvenMean)
{
    uInt nelem = a.nelements();
    if (nelem < 1) {
	throw(ArrayError("::median(const Array<T> &) - Need at least 1 "
			 "elements"));
    }
    //# Mean does not have to be taken for odd number of elements.
    if (nelem%2 != 0) {
	takeEvenMean = False;
    }
    T medval = T();
    Bool deleteIt;
    const T *storage = a.getStorage(deleteIt);
    const T *data = storage;
    T *copy = 0;
    uInt n2 = (nelem - 1)/2;
    if (! sorted) {
	// Sort a copy. Perhaps we should give an option for copying in place?
	// If deleteIt is true, storage already points to copied storage; we
	// could optimize away a possible copy in that case by casting away
	// const and sorting in place.
	copy = new T[nelem];
	if (copy == 0) {
	    a.freeStorage(storage, deleteIt);
	    throw(AllocError("::median(const Array<T> &) - sort buffer",
			     nelem));
	}
	memcpy((char *)copy, (char *)storage, nelem*sizeof(T));
	data = copy;
	// Use a faster algorithm when the array is big enough.
	// If needed take the mean for an even number of elements.
	// Sort a small array in ascending order.
	if (nelem > 50) {
	    if (takeEvenMean) {
		medval = T(0.5 * (GenSort<T>::kthLargest (copy, nelem, n2) +
				  GenSort<T>::kthLargest (copy, nelem, n2+1)));
	    } else {
		medval = GenSort<T>::kthLargest (copy, nelem, n2);
	    }
	} else {
	    GenSort<T>::sort (copy, nelem);
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
    T *origTo = toptr;
    const U *fromptr = from.getStorage(deleteFrom);
    const U *origFrom = fromptr;
    const U *endFrom = fromptr + from.nelements();
    while (fromptr < endFrom) {
	*toptr++ = *fromptr++;
    }
    to.putStorage(origTo, deleteTo);
    from.freeStorage(origFrom, deleteFrom);
}
