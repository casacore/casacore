//# TabVecMath.h: Global functions for table vector mathematics
//# Copyright (C) 1994,1995,1996,1999,2003
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

#ifndef TABLES_TABVECMATH_H
#define TABLES_TABVECMATH_H

//# Global functions similar to those defined in ArrayMath are defined for
//# the table vectors. Furthermore vector functions like norm are defined.

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/tables/Tables/TableVector.h>
#include <casacore/tables/Tables/TVecMath.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Basic math for table vectors.
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <synopsis>
// These global functions do the basic math for table vectors.
// This means addition, subtraction, multiplication, division
// and negation.
// In case two table vectors are used, the left and right operand
// must be conformant (i.e. have equal length).
// </synopsis>

// <group name=basicMath>
// Add 2 table vectors storing result in first one.
template<class T> inline
    void operator+= (TableVector<T>& left, const TableVector<T>& right);
// Subtract 2 table vectors storing result in first one.
template<class T> inline
    void operator-= (TableVector<T>& left, const TableVector<T>& right);
// Multiple 2 table vectors storing result in first one.
template<class T> inline
    void operator*= (TableVector<T>& left, const TableVector<T>& right);
// Divide 2 table vectors storing result in first one.
template<class T> inline
    void operator/= (TableVector<T>& left, const TableVector<T>& right);

// Add a scalar to each element in the table vector.
template<class T> inline
    void operator+= (TableVector<T>& left, const T& right);
// Subtract a scalar from each element in the table vector.
template<class T> inline
    void operator-= (TableVector<T>& left, const T& right);
// Multiple each element in the table vector with a scalar.
template<class T> inline
    void operator*= (TableVector<T>& left, const T& right);
// Divide each element in the table vector by a scalar.
template<class T> inline
    void operator/= (TableVector<T>& left, const T& right);

// Unary plus.
template<class T> inline
    TableVector<T> operator+ (const TableVector<T>&);
// Unary minus.
template<class T> inline
    TableVector<T> operator- (const TableVector<T>&);

// Add 2 table vectors storing result in a new one.
template<class T> inline 
    TableVector<T> operator+ (const TableVector<T>& left,
			      const TableVector<T>& right);
// Subtract 2 table vectors storing result in a new one.
template<class T> inline 
    TableVector<T> operator- (const TableVector<T>& left,
			      const TableVector<T>& right);
// Multiple 2 table vectors storing result in a new one.
template<class T> inline 
    TableVector<T> operator* (const TableVector<T>& left,
			      const TableVector<T>& right);
// Divide 2 table vectors storing result in a new one.
template<class T> inline 
    TableVector<T> operator/ (const TableVector<T>& left,
			      const TableVector<T>& right);

// Add a scalar to each element in the table vector storing result
// in a new table vector.
template<class T> inline 
    TableVector<T> operator+ (const TableVector<T>& left, const T& right);
// Subtract a scalar from each element in the table vector storing result
// in a new table vector.
template<class T> inline 
    TableVector<T> operator- (const TableVector<T>& left, const T& right);
// Multiple each element in the table vector with a scalar storing result
// in a new table vector.
template<class T> inline 
    TableVector<T> operator* (const TableVector<T>& left, const T& right);
// Divide each element in the table vector by a scalar storing result
// in a new table vector.
template<class T> inline 
    TableVector<T> operator/ (const TableVector<T>& left, const T& right);

// Add a scalar to each element in the table vector storing result
// in a new table vector.
template<class T> inline  
    TableVector<T> operator+ (const T& left, const TableVector<T>& right);
// Subtract a scalar from each element in the table vector storing result
// in a new table vector.
template<class T> inline  
    TableVector<T> operator- (const T& left, const TableVector<T>& right);
// Multiple each element in the table vector with a scalar storing result
// in a new table vector.
template<class T> inline  
    TableVector<T> operator* (const T& left, const TableVector<T>& right);
// Divide each element in the table vector by a scalar storing result
// in a new table vector.
template<class T> inline  
    TableVector<T> operator/ (const T& left, const TableVector<T>& right);
// </group>



// <summary>
// Transcendental math for table vectors.
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <synopsis>
// These global functions do the transcendental math for table vectors
// for essentially all numeric types.
// The functions are sin, sinh, exp, log, pow, etc..
// In case two table vectors are used, the left and right operand
// must be conformant (i.e. have equal length).
// </synopsis>

// <group name=basicTransMath>
template<class T> inline TableVector<T> cos  (const TableVector<T>&);
template<class T> inline TableVector<T> cosh (const TableVector<T>&);
template<class T> inline TableVector<T> exp  (const TableVector<T>&);
template<class T> inline TableVector<T> log  (const TableVector<T>&);
template<class T> inline TableVector<T> log10(const TableVector<T>&);
template<class T> inline TableVector<T> pow  (const TableVector<T>& value,
					      const TableVector<T>& exponent);
template<class T> inline TableVector<T> sin  (const TableVector<T>&);
template<class T> inline TableVector<T> sinh (const TableVector<T>&);
template<class T> inline TableVector<T> sqrt (const TableVector<T>&);
// </group>



// <summary>
// Further transcendental math for table vectors.
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <synopsis>
// These global functions do the transcendental math for table vectors
// for a limited set of numeric types.
// The functions are asin, ceil, etc..
// In case two table vectors are used, the left and right operand
// must be conformant (i.e. have equal length).
// </synopsis>

// <group name=advTransMath>
template<class T> inline TableVector<T> acos (const TableVector<T>&);
template<class T> inline TableVector<T> asin (const TableVector<T>&);
template<class T> inline TableVector<T> atan (const TableVector<T>&);
template<class T> inline TableVector<T> atan2(const TableVector<T>& y,
					      const TableVector<T>& x);
template<class T> inline TableVector<T> ceil (const TableVector<T>&);
template<class T> inline TableVector<T> fabs (const TableVector<T>&);
template<class T> inline TableVector<T> floor(const TableVector<T>&);
template<class T> inline TableVector<T> fmod (const TableVector<T>& value,
					      const TableVector<T>& modulo);
template<class T> inline TableVector<T> pow  (const TableVector<T>& value,
					      const double& exponent);
template<class T> inline TableVector<T> tan  (const TableVector<T>&);
template<class T> inline TableVector<T> tanh (const TableVector<T>&);
// </group>



// <summary>
// Miscellaneous table vector operations.
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <synopsis>
// Fill a table vector or calculate the sum, product, minimum or
// maximum of its elements.
// </synopsis>

// <group name=miscellaneous>
// This sets min and max to the min and max of the vector to avoid having
// to do two passes with max() and min() separately.
// Requires that the type "T" has comparison operators.
template<class T> inline
    void minMax (T& min, T& max, const TableVector<T>&);

// The minimum element of the table vector.
// Requires that the type "T" has comparison operators.
template<class T> inline
    T min (const TableVector<T>&);

// The maximum element of the table vector.
// Requires that the type "T" has comparison operators.
template<class T> inline
    T max (const TableVector<T>&);

// Fills all elements of the table vector with a sequence starting with
// "start" and incrementing by "inc" for each element.
template<class T> inline
    void indgen (TableVector<T>&, Int start, Int inc);

// Fills all elements of the table vector with a sequence starting with
// "start" incremented by one for each position in the table vector.
template<class T> inline
    void indgen (TableVector<T>&, Int start);

// Fills all elements of the table vector with a sequence starting with
// 0 and ending with nelements() - 1.
template<class T> inline
    void indgen (TableVector<T>&);

// Sum of all the elements of a table vector.
template<class T> inline
    T sum (const TableVector<T>&);

// Product of all the elements of a table vector.
// <note role=warning>
// product can easily overflow.
// </note>
template<class T> inline T
    product (const TableVector<T>&);
// </group>




// <summary>
// Vector operations on a table vector.
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <synopsis>
// Do vector operations on a table vector (like inner product).
// </synopsis>

// <group name=vectorMath>
// The inner product of 2 table vectors.
// The left and right operands must be conformant (i.e. have equal length).
template<class T> inline
    T innerProduct (const TableVector<T>& left,
		    const TableVector<T>& right);

// The norm of a table vector.
template<class T> inline
    T norm (const TableVector<T>&);

// The cross product of 2 table vectors containing 3 elements.
template<class T> inline
    TableVector<T> crossProduct (const TableVector<T>& left,
				 const TableVector<T>& right);
// </group>



//# Inline all these functions.
//# The actual work is done by functions (tabVecRep...) operating on TabVecRep.
//# Because the preprocessor of gcc-3 gives warnings when using the macro as
//# e.g. TABVECMATHOPER(add,+,+=), the r is removed from the function name and
//# put befroe the + in the macro call.

#define TABVECMATHOPER(NAME,OP,OPA) \
template<class T> inline \
TableVector<T> aips_name2(operato,OP) (const TableVector<T>& tv, \
					const T& v) \
    { return TableVector<T> (aips_name2(tabVecRepvalr,NAME) (tv.tabVec(), \
							     v)); } \
template<class T> inline \
TableVector<T> aips_name2(operato,OP) (const T& v, \
					const TableVector<T>& tv) \
    { return TableVector<T> (aips_name2(tabVecRepvall,NAME) (v, \
							     tv.tabVec())); } \
template<class T> inline \
TableVector<T> aips_name2(operato,OP) (const TableVector<T>& l, \
					const TableVector<T>& r) \
    { return TableVector<T> (aips_name2(tabVecReptv,NAME) (l.tabVec(), \
							   r.tabVec())); } \
template<class T> inline \
void aips_name2(operato,OPA) (TableVector<T>& tv, const T& v) \
    { aips_name2(tabVecRepvalass,NAME) (tv.tabVec(), v); } \
template<class T> inline \
void aips_name2(operato,OPA) (TableVector<T>& l, \
			       const TableVector<T>& r) \
    { aips_name2(tabVecReptvass,NAME) (l.tabVec(), r.tabVec()); }

TABVECMATHOPER(add,r+,r+=)
TABVECMATHOPER(sub,r-,r-=)
TABVECMATHOPER(tim,r*,r*=)
TABVECMATHOPER(div,r/,r/=)


#define TABVECMATHFUNC(NAME) \
template<class T> inline \
TableVector<T> NAME (const TableVector<T>& tv) \
    { return TableVector<T> (aips_name2(tabVecRep,NAME) (tv.tabVec())); }
#define TABVECMATHFUNC2(NAME) \
template<class T> inline \
TableVector<T> NAME (const TableVector<T>& l, \
		     const TableVector<T>& r) \
    { return TableVector<T> (aips_name2(tabVecRep,NAME) (l.tabVec(), \
							 r.tabVec())); }

TABVECMATHFUNC (cos)
TABVECMATHFUNC (cosh)
TABVECMATHFUNC (exp)
TABVECMATHFUNC (log)
TABVECMATHFUNC (log10)
TABVECMATHFUNC2(pow)
TABVECMATHFUNC (sin)
TABVECMATHFUNC (sinh)
TABVECMATHFUNC (sqrt)
TABVECMATHFUNC (acos)
TABVECMATHFUNC (asin)
TABVECMATHFUNC (atan)
TABVECMATHFUNC2(atan2)
TABVECMATHFUNC (ceil)
TABVECMATHFUNC (fabs)
TABVECMATHFUNC (floor)
TABVECMATHFUNC2(fmod)
TABVECMATHFUNC (tan)
TABVECMATHFUNC (tanh)

template<class T> inline
TableVector<T> pow (const TableVector<T>& tv, const double& exp)
    { return TableVector<T> (tabVecReppowd (tv.tabVec(), exp)); }


template<class T> inline
T sum (const TableVector<T>& tv)
    { return tabVecRepsum (tv.tabVec()); }
template<class T> inline
T product (const TableVector<T>& tv)
    { return tabVecRepproduct (tv.tabVec()); }


template<class T> inline
void minMax (T& min, T& max, const TableVector<T>& tv)
    { tabVecRepminmax (min, max, tv.tabVec()); }
template<class T> inline
T min (const TableVector<T>& tv)
    { T Min,Max; tabVecRepminmax (Min, Max, tv.tabVec()); return Min; }
template<class T> inline
T max (const TableVector<T>& tv)
    { T Min,Max; tabVecRepminmax (Min, Max, tv.tabVec()); return Max; }
			       
template<class T> inline
void indgen (TableVector<T>& tv, Int start, Int inc)
    { tabVecRepindgen (tv.tabVec(), start, inc); }
template<class T> inline
void indgen (TableVector<T>& tv, Int start)
    { tabVecRepindgen (tv.tabVec(), start, 1); }
template<class T> inline
void indgen (TableVector<T>& tv)
    { tabVecRepindgen (tv.tabVec(), 0, 1); }


template<class T> inline
T innerProduct (const TableVector<T>& l, const TableVector<T>& r)
    { return tabVecRepinnerproduct (l.tabVec(), r.tabVec()); }
template<class T> inline
T norm (const TableVector<T>& tv)
    { return tabVecRepnorm (tv.tabVec()); }
template<class T> inline
TableVector<T> crossProduct (const TableVector<T>& l,
			     const TableVector<T>& r)
    { return TableVector<T> (tabVecRepcrossproduct (l.tabVec(), r.tabVec())); }



} //# NAMESPACE CASACORE - END

#endif
