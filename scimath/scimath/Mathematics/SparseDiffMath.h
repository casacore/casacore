//# SparseDiffMath.h: Implements all mathematical functions for SparseDiff.
//# Copyright (C) 2007
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
//# $Id: SparseDiffMath.h,v 1.1 2007/11/16 04:34:46 wbrouw Exp $

#ifndef SCIMATH_SPARSEDIFFMATH_H
#define SCIMATH_SPARSEDIFFMATH_H

//# Includes

#include <casa/aips.h>
#include <casa/BasicMath/Math.h>
#include <scimath/Mathematics/SparseDiff.h>

namespace casa { //# NAMESPACE CASA - BEGIN

  // <summary>
  // Implements all mathematical operators and functions for SparseDiff.
  // </summary>
  //
  // <reviewed reviewer="UNKNOWN" date="" tests="tSparseDiff" demos="">
  // </reviewed>
  //
  // <prerequisite>
  // <li> <linkto class=SparseDiff>SparseDiff</linkto> class
  // </prerequisite>
  //
  // <etymology>
  // Implements all mathematical operators and functions for SparseDiff.
  // </etymology>
  //
  // <todo asof="20001/08/12">
  //  <li> nothing I know of
  // </todo>
 
  // <group name="SparseDiff mathematical operations">

  // Unary arithmetic operators.
  // <group>
  template<class T>
  SparseDiff<T> operator+(const SparseDiff<T> &other);
  template<class T>
  SparseDiff<T> operator-(const SparseDiff<T> &other);
  // </group>

  // Arithmetic on two SparseDiff objects, returning a SparseDiff object
  // <group>
  template<class T> 
  SparseDiff<T> operator+(const SparseDiff<T> &left, const SparseDiff<T> &right);
  template<class T> 
  SparseDiff<T> operator-(const SparseDiff<T> &left, const SparseDiff<T> &right);
  template<class T> 
  SparseDiff<T> operator*(const SparseDiff<T> &left, const SparseDiff<T> &right);
  template<class T> 
  SparseDiff<T> operator/(const SparseDiff<T> &left, const SparseDiff<T> &right);
  // </group>

  // Arithmetic on a SparseDiff and a scalar, returning a SparseDiff
  // <group>
  template<class T> 
  SparseDiff<T> operator+(const SparseDiff<T> &left, const T &right);
  template<class T> 
  SparseDiff<T> operator-(const SparseDiff<T> &left, const T &right);
  template<class T> 
  SparseDiff<T> operator*(const SparseDiff<T> &left, const T &right);
  template<class T> 
  SparseDiff<T> operator/(const SparseDiff<T> &left, const T &right);
  // </group>

  // Arithmetic between a scalar and a SparseDiff returning a SparseDiff
  // <group>
  template<class T> 
  SparseDiff<T> operator+(const T &left, const SparseDiff<T> &right);
  template<class T> 
  SparseDiff<T> operator-(const T &left, const SparseDiff<T> &right);
  template<class T> 
  SparseDiff<T> operator*(const T &left, const SparseDiff<T> &right);
  template<class T> 
  SparseDiff<T> operator/(const T &left, const SparseDiff<T> &right);
  // </group>

  // Transcendental functions
  // <group>
  template<class T> SparseDiff<T> acos(const SparseDiff<T> &ad);
  template<class T> SparseDiff<T> asin(const SparseDiff<T> &ad);
  template<class T> SparseDiff<T> atan(const SparseDiff<T> &ad);
  template<class T> SparseDiff<T> atan2(const SparseDiff<T> &y, 
					const SparseDiff<T> &x);
  template<class T> SparseDiff<T> cos(const SparseDiff<T> &ad);
  template<class T> SparseDiff<T> cosh(const SparseDiff<T> &ad);
  template<class T> SparseDiff<T> exp(const SparseDiff<T> &ad);
  template<class T> SparseDiff<T> log(const SparseDiff<T> &ad);
  template<class T> SparseDiff<T> log10(const SparseDiff<T> &ad);
  template<class T> SparseDiff<T> erf(const SparseDiff<T> &ad);
  template<class T> SparseDiff<T> erfc(const SparseDiff<T> &ad);
  template<class T> SparseDiff<T> pow(const SparseDiff<T> &a, 
				      const SparseDiff<T> &b);
  template<class T> SparseDiff<T> pow(const SparseDiff<T> &a, const T &b);
  template<class T> SparseDiff<T> square(const SparseDiff<T> &ad);
  template<class T> SparseDiff<T> cube(const SparseDiff<T> &ad);
  template<class T> SparseDiff<T> sin(const SparseDiff<T> &ad);
  template<class T> SparseDiff<T> sinh(const SparseDiff<T> &ad);
  template<class T> SparseDiff<T> sqrt(const SparseDiff<T> &ad);
  template<class T> SparseDiff<T> tan(const SparseDiff<T> &ad);
  template<class T> SparseDiff<T> tanh(const SparseDiff<T> &ad);
  template<class T> SparseDiff<T> abs(const SparseDiff<T> &ad);
  // </group>
  // Floating-point remainder of x/c, with the same sign as x, where c is
  // a constant.
  // <group>
  template<class T> SparseDiff<T> fmod(const SparseDiff<T> &x, const T &c);
  template<class T> SparseDiff<T> fmod(const SparseDiff<T> &x,
				       const SparseDiff<T> &c);
  // </group>
  // Floor and ceil of values
  // <group>
  template<class T> SparseDiff<T> floor(const SparseDiff<T> &ad);
  template<class T> SparseDiff<T> ceil(const SparseDiff<T> &ad);
  // </group>
 
  // Comparison operators.  Only the values are compared: in the actual
  // functions, comparisons are used to decide on algorithms. To check
  // if two SparseDiff values are equal, use comparison for both 
  // value and derivatives.
  // <note role=tip> To check if two SparseDiff values are equal, use the
  // member method <src>equals()</src> (e.g. for debugging and testing).
  // </note>
  // <group>
  // Compare two SparseDiff's
  template<class T> Bool operator>(const SparseDiff<T> &left,
				   const SparseDiff<T> &right);
  template<class T> Bool operator<(const SparseDiff<T> &left,
				   const SparseDiff<T> &right);
  template<class T> Bool operator>=(const SparseDiff<T> &left,
				    const SparseDiff<T> &right);
  template<class T> Bool operator<=(const SparseDiff<T> &left,
				    const SparseDiff<T> &right);
  template<class T> Bool operator==(const SparseDiff<T> &left,
				    const SparseDiff<T> &right);
  template<class T> Bool operator!=(const SparseDiff<T> &left,
				    const SparseDiff<T> &right);
  template<class T> Bool near(const SparseDiff<T> &left,
			      const SparseDiff<T> &right);
  template<class T> Bool near(const SparseDiff<T> &left,
			      const SparseDiff<T> &right, const Double tol);
  template<class T> Bool allnear(const SparseDiff<T> &left,
				 const SparseDiff<T> &right, const Double tol);
  template<class T> Bool nearAbs(const SparseDiff<T> &left,
				 const SparseDiff<T> &right, const Double tol);
  template<class T> Bool allnearAbs(const SparseDiff<T> &left,
				    const SparseDiff<T> &right, const Double tol);
  // </group>
  // Compare a SparseDiff and a constant
  // <group>
  template<class T> Bool operator>(const SparseDiff<T> &left, const T &right);
  template<class T> Bool operator<(const SparseDiff<T> &left, const T &right);
  template<class T> Bool operator>=(const SparseDiff<T> &left, const T &right);
  template<class T> Bool operator<=(const SparseDiff<T> &left, const T &right);
  template<class T> Bool operator==(const SparseDiff<T> &left, const T &right);
  template<class T> Bool operator!=(const SparseDiff<T> &left, const T &right);
  template<class T> Bool near(const SparseDiff<T> &left, const T &right);
  template<class T> Bool near(const SparseDiff<T> &left, const T &right,
			      const Double tol);
  template<class T> Bool allnear(const SparseDiff<T> &left, const T &right,
				 const Double tol);
  template<class T> Bool nearAbs(const SparseDiff<T> &left, const T &right,
				 const Double tol);
  template<class T> Bool allnearAbs(const SparseDiff<T> &left, const T &right,
				    const Double tol);
  // </group>
  // Compare a constant and a SparseDiff
  // <group>
  template<class T> Bool operator>(const T &left, const SparseDiff<T> &right);
  template<class T> Bool operator<(const T &left, const SparseDiff<T> &right);
  template<class T> Bool operator>=(const T &left, const SparseDiff<T> &right);
  template<class T> Bool operator<=(const T &left, const SparseDiff<T> &right);
  template<class T> Bool operator==(const T &left, const SparseDiff<T> &right);
  template<class T> Bool operator!=(const T &left, const SparseDiff<T> &right);
  template<class T> Bool near(const T &left, const SparseDiff<T> &right,
			      const Double tol);
  template<class T> Bool allnear(const T &left, const SparseDiff<T> &right,
				 const Double tol);
  template<class T> Bool nearAbs(const T &left, const SparseDiff<T> &right,
				 const Double tol);
  template<class T> Bool allnearAbs(const T &left, const SparseDiff<T> &right,
				    const Double tol);
  // </group>
  // Test special values
  // <group>
  template<class T> Bool isNaN(const SparseDiff<T> &val);
  template<class T> Bool isInf(SparseDiff<T> &val);
  // </group>
  // Minimum/maximum
  // <group>
  template<class T> SparseDiff<T> min(const SparseDiff<T> &left,
				      const SparseDiff<T> &right);
  template<class T> SparseDiff<T> max(const SparseDiff<T> &left,
				      const SparseDiff<T> &right);
  // </group>

  // </group>


} //# NAMESPACE CASA - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <scimath/Mathematics/SparseDiffMath.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
