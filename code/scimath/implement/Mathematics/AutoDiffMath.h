//# AutoDiffMath.h: an automatic differential class for arameterized functions
//# Copyright (C) 1995,1999
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

#if !defined(AIPS_AUTO_DIFF__MATH_H)
#define AIPS_AUTO_DIFF__MATH_H


#include <aips/aips.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Mathematics/Math.h>
#include <trial/Mathematics/AutoDiff.h>

// <summary>
// Implements all mathematical operators and functions for AutoDiff.
// </summary>
//
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
//
// <prerequisite>
// <li> AutoDiff
// </prerequisite>
//
// <etymology>
// Implements all mathematical operators and functions for AutoDiff.
// </etymology>
//
// <todo asof="yyyy/mm/dd">
// </todo>
 
// Unary arithmetic operators.
// <group>
template<class T> AutoDiff<T> operator+(const AutoDiff<T> &ad);
template<class T> AutoDiff<T> operator-(const AutoDiff<T> &ad);
// </group>

// arithmetic on two AutoDiff objects, returning an AutoDiff object
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
template<class T> AutoDiff<T> acos(const AutoDiff<T>& ad);
template<class T> AutoDiff<T> asin(const AutoDiff<T>& ad);
template<class T> AutoDiff<T> atan(const AutoDiff<T>& ad);
template<class T> AutoDiff<T> atan2(const AutoDiff<T>& y, 
				      const AutoDiff<T>& x);
template<class T> AutoDiff<T> cos(const AutoDiff<T>& ad);
template<class T> AutoDiff<T> cosh(const AutoDiff<T>& ad);
template<class T> AutoDiff<T> exp(const AutoDiff<T>& ad);
template<class T> AutoDiff<T> log(const AutoDiff<T>& ad);
template<class T> AutoDiff<T> log10(const AutoDiff<T>& ad);
template<class T> AutoDiff<T> pow(const AutoDiff<T>& a, 
				    const AutoDiff<T>& b);
template<class T> AutoDiff<T> pow(const AutoDiff<T>& a, const T &b);
template<class T> AutoDiff<T> sin(const AutoDiff<T>& ad);
template<class T> AutoDiff<T> sinh(const AutoDiff<T>& ad);
template<class T> AutoDiff<T> sqrt(const AutoDiff<T>& ad);
template<class T> AutoDiff<T> tan(const AutoDiff<T>& ad);
template<class T> AutoDiff<T> tanh(const AutoDiff<T>& ad);
template<class T> AutoDiff<T> abs(const AutoDiff<T>& ad);
// Floating-point remainder of x/c, with the same sign as x, where c is
// a constant.
template<class T> AutoDiff<T> fmod(const AutoDiff<T>& x,const T c);
// </group>
 
// Comparison operators.  Except "==", "!=" and "near", only the values are 
// compared.  The derivatives are not compared.  For the exceptions,
// both value and derivatives are compared.  near calls near functions
// in aips/Mathematics for corresponding data type with default tolerance.
// n
// <group>
// Compare two AutoDiff's
template<class T> Bool operator>(const AutoDiff<T>& right,const AutoDiff<T>& left);
template<class T> Bool operator<(const AutoDiff<T>& right,const AutoDiff<T>& left);
template<class T> Bool operator>=(const AutoDiff<T>& right,const AutoDiff<T>& left);
template<class T> Bool operator<=(const AutoDiff<T>& right,const AutoDiff<T>& left);
template<class T> Bool operator==(const AutoDiff<T>& right,const AutoDiff<T>& left);
template<class T> Bool operator!=(const AutoDiff<T>& right,const AutoDiff<T>& left);
template<class T> Bool near(const AutoDiff<T>& right,const AutoDiff<T>& left);

// Compare an AutoDiff and a constant
template<class T> Bool operator>(const AutoDiff<T>& right,const T& left);
template<class T> Bool operator<(const AutoDiff<T>& right,const T& left);
template<class T> Bool operator>=(const AutoDiff<T>& right,const T& left);
template<class T> Bool operator<=(const AutoDiff<T>& right,const T& left);
template<class T> Bool operator==(const AutoDiff<T>& right,const T& left);
template<class T> Bool operator!=(const AutoDiff<T>& right,const T& left);
template<class T> Bool near(const AutoDiff<T>& right,const T& left);


// Compare a constant and an AutoDiff
template<class T> Bool operator>(const T& right,const AutoDiff<T>& left);
template<class T> Bool operator<(const T& right,const AutoDiff<T>& left);
template<class T> Bool operator>=(const T& right,const AutoDiff<T>& left);
template<class T> Bool operator<=(const T& right,const AutoDiff<T>& left);
template<class T> Bool operator==(const T& right,const AutoDiff<T>& left);
template<class T> Bool operator!=(const T& right,const AutoDiff<T>& left);
template<class T> Bool near(const T& right,const AutoDiff<T>& left);


// </group>



#endif


