//# AutoDiffMath.h: Implements all mathematical functions for AutoDiff.
//# Copyright (C) 1995,1999,2001,2002,2004
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
//#
//# $Id$

#ifndef SCIMATH_AUTODIFFMATH_H
#define SCIMATH_AUTODIFFMATH_H

//# Includes

#include <casacore/casa/aips.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/scimath/Mathematics/AutoDiff.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Implements all mathematical operators and functions for AutoDiff.
// </summary>
//
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tAutoDiff" demos="">
// </reviewed>
//
// <prerequisite>
// <li> <linkto class=AutoDiff>AutoDiff</linkto> class
// </prerequisite>
//
// <etymology>
// Implements all mathematical operators and functions for AutoDiff.
// </etymology>
//
// <todo asof="20001/08/12">
//  <li> nothing I know of
// </todo>
 
// <group name="AutoDiff mathematical operations">

// Unary arithmetic operators.
// <group>
template<class T>
AutoDiff<T> operator+(const AutoDiff<T> &other);
template<class T>
AutoDiff<T> operator-(const AutoDiff<T> &other);
// </group>

// Arithmetic on two AutoDiff objects, returning an AutoDiff object
// <group>
template<class T> 
AutoDiff<T> operator+(const AutoDiff<T> &left, const AutoDiff<T> &right);
template<class T> 
AutoDiff<T> operator-(const AutoDiff<T> &left, const AutoDiff<T> &right);
template<class T> 
AutoDiff<T> operator*(const AutoDiff<T> &left, const AutoDiff<T> &right);
template<class T> 
AutoDiff<T> operator/(const AutoDiff<T> &left, const AutoDiff<T> &right);
// </group>

// Arithmetic on an AutoDiff and a scalar, returning an AutoDiff
// <group>
template<class T> 
AutoDiff<T> operator+(const AutoDiff<T> &left, const T &right);
template<class T> 
AutoDiff<T> operator-(const AutoDiff<T> &left, const T &right);
template<class T> 
AutoDiff<T> operator*(const AutoDiff<T> &left, const T &right);
template<class T> 
AutoDiff<T> operator/(const AutoDiff<T> &left, const T &right);
// </group>

// Arithmetic between a scalar and an AutoDiff returning an AutoDiff
// <group>
template<class T> 
AutoDiff<T> operator+(const T &left, const AutoDiff<T> &right);
template<class T> 
AutoDiff<T> operator-(const T &left, const AutoDiff<T> &right);
template<class T> 
AutoDiff<T> operator*(const T &left, const AutoDiff<T> &right);
template<class T> 
AutoDiff<T> operator/(const T &left, const AutoDiff<T> &right);
// </group>

// Transcendental functions
// <group>
template<class T> AutoDiff<T> acos(const AutoDiff<T> &ad);
template<class T> AutoDiff<T> asin(const AutoDiff<T> &ad);
template<class T> AutoDiff<T> atan(const AutoDiff<T> &ad);
template<class T> AutoDiff<T> atan2(const AutoDiff<T> &y, 
				    const AutoDiff<T> &x);
template<class T> AutoDiff<T> cos(const AutoDiff<T> &ad);
template<class T> AutoDiff<T> cosh(const AutoDiff<T> &ad);
template<class T> AutoDiff<T> exp(const AutoDiff<T> &ad);
template<class T> AutoDiff<T> log(const AutoDiff<T> &ad);
template<class T> AutoDiff<T> log10(const AutoDiff<T> &ad);
template<class T> AutoDiff<T> erf(const AutoDiff<T> &ad);
template<class T> AutoDiff<T> erfc(const AutoDiff<T> &ad);
template<class T> AutoDiff<T> pow(const AutoDiff<T> &a, 
				  const AutoDiff<T> &b);
template<class T> AutoDiff<T> pow(const AutoDiff<T> &a, const T &b);
template<class T> AutoDiff<T> square(const AutoDiff<T> &ad);
template<class T> AutoDiff<T> cube(const AutoDiff<T> &ad);
template<class T> AutoDiff<T> sin(const AutoDiff<T> &ad);
template<class T> AutoDiff<T> sinh(const AutoDiff<T> &ad);
template<class T> AutoDiff<T> sqrt(const AutoDiff<T> &ad);
template<class T> AutoDiff<T> tan(const AutoDiff<T> &ad);
template<class T> AutoDiff<T> tanh(const AutoDiff<T> &ad);
template<class T> AutoDiff<T> abs(const AutoDiff<T> &ad);
// </group>
// Floating-point remainder of x/c, with the same sign as x, where c is
// a constant.
// <group>
template<class T> AutoDiff<T> fmod(const AutoDiff<T> &x, const T &c);
template<class T> AutoDiff<T> fmod(const AutoDiff<T> &x,
				   const AutoDiff<T> &c);
// </group>
// Floor and ceil of values
// <group>
template<class T> AutoDiff<T> floor(const AutoDiff<T> &ad);
template<class T> AutoDiff<T> ceil(const AutoDiff<T> &ad);
// </group>
 
// Comparison operators.  Only the values are compared: in the actual
// functions, comparisons are used to decide on algorithms. To check
// if two AutoDiff values are equal, use comparison for both 
// value and derivatives.
// <note role=tip> To check if two AutoDiff values are equal, use the
// member method <src>equals()</src> (e.g. for debugging and testing).
// </note>
// <group>
// Compare two AutoDiff's
template<class T> Bool operator>(const AutoDiff<T> &left,
				 const AutoDiff<T> &right);
template<class T> Bool operator<(const AutoDiff<T> &left,
				 const AutoDiff<T> &right);
template<class T> Bool operator>=(const AutoDiff<T> &left,
				  const AutoDiff<T> &right);
template<class T> Bool operator<=(const AutoDiff<T> &left,
				  const AutoDiff<T> &right);
template<class T> Bool operator==(const AutoDiff<T> &left,
				  const AutoDiff<T> &right);
template<class T> Bool operator!=(const AutoDiff<T> &left,
				  const AutoDiff<T> &right);
template<class T> Bool near(const AutoDiff<T> &left,
			    const AutoDiff<T> &right);
template<class T> Bool near(const AutoDiff<T> &left,
			    const AutoDiff<T> &right, const Double tol);
template<class T> Bool allnear(const AutoDiff<T> &left,
			       const AutoDiff<T> &right, const Double tol);
template<class T> Bool nearAbs(const AutoDiff<T> &left,
			       const AutoDiff<T> &right, const Double tol);
template<class T> Bool allnearAbs(const AutoDiff<T> &left,
				  const AutoDiff<T> &right, const Double tol);
// </group>
// Compare an AutoDiff and a constant
// <group>
template<class T> Bool operator>(const AutoDiff<T> &left, const T &right);
template<class T> Bool operator<(const AutoDiff<T> &left, const T &right);
template<class T> Bool operator>=(const AutoDiff<T> &left, const T &right);
template<class T> Bool operator<=(const AutoDiff<T> &left, const T &right);
template<class T> Bool operator==(const AutoDiff<T> &left, const T &right);
template<class T> Bool operator!=(const AutoDiff<T> &left, const T &right);
template<class T> Bool near(const AutoDiff<T> &left, const T &right);
template<class T> Bool near(const AutoDiff<T> &left, const T &right,
			    const Double tol);
template<class T> Bool allnear(const AutoDiff<T> &left, const T &right,
			       const Double tol);
template<class T> Bool nearAbs(const AutoDiff<T> &left, const T &right,
			       const Double tol);
template<class T> Bool allnearAbs(const AutoDiff<T> &left, const T &right,
				  const Double tol);
// </group>
// Compare a constant and an AutoDiff
// <group>
template<class T> Bool operator>(const T &left, const AutoDiff<T> &right);
template<class T> Bool operator<(const T &left, const AutoDiff<T> &right);
template<class T> Bool operator>=(const T &left, const AutoDiff<T> &right);
template<class T> Bool operator<=(const T &left, const AutoDiff<T> &right);
template<class T> Bool operator==(const T &left, const AutoDiff<T> &right);
template<class T> Bool operator!=(const T &left, const AutoDiff<T> &right);
template<class T> Bool near(const T &left, const AutoDiff<T> &right,
			    const Double tol);
template<class T> Bool allnear(const T &left, const AutoDiff<T> &right,
			       const Double tol);
template<class T> Bool nearAbs(const T &left, const AutoDiff<T> &right,
			       const Double tol);
template<class T> Bool allnearAbs(const T &left, const AutoDiff<T> &right,
				  const Double tol);
// </group>
// Test special values
// <group>
template<class T> Bool isNaN(const AutoDiff<T> &val);
template<class T> Bool isInf(AutoDiff<T> &val);
// </group>
// Minimum/maximum
// <group>
template<class T> AutoDiff<T> min(const AutoDiff<T> &left,
				  const AutoDiff<T> &right);
template<class T> AutoDiff<T> max(const AutoDiff<T> &left,
				  const AutoDiff<T> &right);
// </group>

// </group>


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Mathematics/AutoDiffMath.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
