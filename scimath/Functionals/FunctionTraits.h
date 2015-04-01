//# FunctionTraits.h: Function data types for parameters and arguments
//# Copyright (C) 2001,2002
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

#ifndef SCIMATH_FUNCTIONTRAITS_H
#define SCIMATH_FUNCTIONTRAITS_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/scimath/Mathematics/AutoDiff.h>
#include <casacore/scimath/Mathematics/AutoDiffA.h>
#include <casacore/scimath/Mathematics/AutoDiffX.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//
// <summary> Function data types for parameters and arguments
// </summary>
// <use visibility=local>
//
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="t">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="Function">Function</linkto>
//   <li> <linkto class="AutoDiff">AutoDiff</linkto>
// </prerequisite>

// <etymology>
// A trait is a characteristic feature. FunctionTraits defines relationships
// between the data types of result, parameters and arguments of function
// objects.
// </etymology>
//
// <synopsis>
// This templated class contains a number of typedefs that
// describe the relationship between different numeric data types used for
// the calculation of <src>Function</src> and the function parameter and
// arguments.
//
// Its main use is to optimize the speed of the calculation of function
// values and derivatives in the case of <src>AutoDiff</src> use and
// manual calculation of derivatives, by allowing the data type of the
// the arguments and/or parameters to be plain numeric in cases where the
// derivatives wrt these are not needed.
// To enable this, the following definitions are used for the use of the
// Function template. Bear in mind that the Function operator is defined
// as <src>result = f<T>(x; parameters)</src>.
// <ol>
//  <li> Simple numeric type (Double, Complex, etc): result, parameters and
// 	arguments: all the one defined templated type.
//  <li> <src>AutoDiff<T></src> indicates the calculation (either automatic or
//	with specialized implementations) of the result with a <src>T</src>
//	function value for <src>T</src> arg, and <src>AutoDiff<T></src>
//	parameters.
//   <li> <src>AutoDiffA</src> calculate form <src>AutoDiff<T></src>
//	arguments and parameters (note that either could be simple
//	values with zero derivatives)
//   <li> <src>AutoDiffX<T></src> : calculate only with respect to
//	the arguments the derivatives, by using <src>T</src> 
// 	parameters
// </ol>
// The following types are defined:
// <dl>
// <dt> <src>Type</src>
// <dd> The template argument
// <dt> <src>BaseType</src>
// <dd> One down in the template hierarchy if possible (e.g. <src>Double</src>
//	for <src>AutoDiff<Double></src>)
// <dt> <src>NumericType</src>
// <dd> Ultimate numeric type (e.g. <src>Double</src> for
//	<src>AutoDiff<AutoDiff<Double> ></src>
// <dt> <src>ParamType</src>
// <dd> Type used for parameters
// <dt> <src>ArgType</src>
// <dd> Type used for arguments
// <dt> <src>DiffType</src>
// <dd> The default differentiation type (e.g. <src>AutoDiff<Double></src>
//		for <src>AutoDiff<Double></src>)
// <dt> <src>getValue()</src> 
// <dd>		get the value of a simple numeric or of an <src>AutoDiff</src>
// <dt> <src>setValue()</src> 
// <dd>		set the value of a simple numeric or of an <src>AutoDiff</src>
// </dl>
//
// The specializations are done in such a way that higher order
// derivatives (e.g. <src>AutoDiff<AutoDiff<Double> ></src>) are catered for.
//
// Note that the class names in the following definitions are extended with
// some individual id (like <src>_PA</src>): do not use them in programming,
// they are only necessary for the <src>cxx2html</src> interpreter)
//
// This class is implemented as a number of specializations for the
// following data types.
// <ul> 
// <li> <src>T</src>
// <li> <src>AutoDiff<T></src>
// <li> <src>AutoDiffA<T></src>
// <li> <src>AutoDiffX<T></src>
// </ul>
// </synopsis>
//
// <example>
// See the <linkto class=Function>Function</linkto> class code.
// </example>
//
// <motivation>
// To keep the Function class single templated
// </motivation>
//
// <todo asof="2002/06/19">
//  <li> Additional <src>AutoDiff*</src> classes if and when needed
// </todo>
//

template <class T> class FunctionTraits {
public:
  // Actual template type
  typedef T Type; 
  // Template base type
  typedef T BaseType;
  // Numeric type of template
  typedef T NumericType;
  // Type for parameters
  typedef T ParamType;
  // Type for arguments
  typedef T ArgType;
  // Default type for differentiation
  typedef AutoDiff<T> DiffType;
  // Get the value
  static const T &getValue(const T &in) { return in; }
  // Set a value (and possible derivative)
  static void setValue(T &out, const T &val, const uInt,
		       const uInt) { out = val; }
};

//# Following are specializations. Naming only for documentation
//# purposes (a problem with cxx2html)

#define FunctionTraits_P FunctionTraits

// <summary> FunctionTraits specialization for AutoDiff
// </summary>

template <class T> class FunctionTraits_P<AutoDiff<T> > {
public:
  // Actual template type
  typedef AutoDiff<T> Type; 
  // Template base type
  typedef T BaseType;
  // Template numeric type
  typedef typename FunctionTraits_P<T>::NumericType NumericType;
  // Type for parameters
  typedef AutoDiff<T> ParamType;
  // Type for arguments
  typedef T ArgType;
  // Default type for differentiation
  typedef AutoDiff<T> DiffType;
  // Get the value
  static const T &getValue(const Type &in) {
    return FunctionTraits<T>::getValue(in.value()); }
  // Set a value (and possible derivative)
  static void setValue(Type &out, const T &val, const uInt nder,
		       const uInt i) { out = Type(val, nder, i); }
};

#undef FunctionTraits_P

#define FunctionTraits_PA FunctionTraits

// <summary> FunctionTraits specialization for AutoDiffA
// </summary>

template <class T> class FunctionTraits_PA<AutoDiffA<T> > {
public:
  // Actual template type
  typedef AutoDiffA<T> Type; 
  // Template base type
  typedef T BaseType;
  // Template numeric type
  typedef typename FunctionTraits_PA<T>::NumericType NumericType;
  // Type for parameters
  typedef AutoDiffA<T> ParamType;
  // Type for arguments
  typedef AutoDiffA<T> ArgType;
  // Default type for differentiation
  typedef AutoDiffA<T> DiffType;
  // Get the value
  static const T &getValue(const Type &in) {
    return FunctionTraits<T>::getValue(in.value()); }
  // Set a value (and possible derivative)
  static void setValue(Type &out, const T &val, const uInt nder,
		       const uInt i) { out = Type(val, nder, i); }
};

#undef FunctionTraits_PA

#define FunctionTraits_PX FunctionTraits

// <summary> FunctionTraits specialization for AutoDiffX
// </summary>

template <class T> class FunctionTraits_PX<AutoDiffX<T> > {
public:
  // Actual template type
  typedef AutoDiffX<T> Type; 
  // Template base type
  typedef T BaseType;
  // Template numeric type
  typedef typename FunctionTraits_PX<T>::NumericType NumericType;
  // Type for parameters
  typedef T ParamType;
  // Type for arguments
  typedef AutoDiffX<T> ArgType;
  // Default type for differentiation
  typedef AutoDiffX<T> DiffType;
  // Get the value
  static const T &getValue(const Type &in) {
    return FunctionTraits<T>::getValue(in.value()); }
  // Set a value (and possible derivative)
  static void setValue(Type &out, const T &val, const uInt nder,
		       const uInt i) { out = Type(val, nder, i); }
};

#undef FunctionTraits_PX


} //# NAMESPACE CASACORE - END

#endif
