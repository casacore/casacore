//# FunctionTraits.h: Function data types for parameters and arguments
//# Copyright (C) 2001
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

#if !defined(AIPS_FUNCTIONTRAITS_H)
#define AIPS_FUNCTIONTRAITS_H

//# Includes
#include <aips/aips.h>
#include <aips/Mathematics/AutoDiff.h>
#include <aips/Mathematics/AutoDiffA.h>

//
// <summary> Function data types for parameters and arguments
// </summary>
// <use visibility=local>
//
// <reviewed reviewer="" date="" tests="t">
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
//   <li> <src>AutoDiffX<T></src> (not yet implemented): calculate wrt
//	the arguments the derivatives only, by using <src>T</src> 
// 	parameters
// </ol>
// The following types are defined:
// <dl>
// <dt> <src>Type</src>
// <dd> The template argument
// <dt> <src>BaseType</src>
// <dd> One down in the template hierarchy if possible (e.g. <src>Double</src>
//	for </src>AutoDiff<Double></src>)
// <dt> <src>ParamType</src>
// <dd> Type used for parameters
// <dt> <src>ArgType</src>
// <dd> Type used for arguments
// </dl>
//
// The specializations are done in such a way that higher order
// derivatives (e.g. <src>AutoDiff<AutoDiff<Double> ></src>) are catered for.
//
// Note that the class names in the following definitions are extended with
// some individual id (like <src>_PA</src>): do not use them in programming,
// they are only necessary for the <src>cxx2html</src> interpreter)
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
// <templating arg=T>
// This class is implemented as a number of specializations for the
// following data types.
// <ul> 
// <li> <src>T</src>
// <li> <src>AutoDiff<T></src>
// <li> <src>AutoDiffA<T></src>
// </ul>
// </templating>
//
// <todo asof="2001/10/19">
//  <li> Additional <src>AutoDiff*</src> classes if and when needed
// </todo>
//

template <class T> class FunctionTraits {
public:
  // Actual template type
  typedef T Type; 
  // possible template base type
  typedef T BaseType;
  // Type for parameters
  typedef T ParamType;
  // Type for arguments
  typedef T ArgType;
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
  // possible template base type
  typedef T BaseType;
  // Type for parameters
  typedef AutoDiff<T> ParamType;
  // Type for arguments
  typedef T ArgType;
};

#undef FunctionTraits_P

#define FunctionTraits_PA FunctionTraits

// <summary> FunctionTraits specialization for AutoDiffA
// </summary>

template <class T> class FunctionTraits_PA<AutoDiffA<T> > {
public:
  // Actual template type
  typedef AutoDiffA<T> Type; 
  // possible template base type
  typedef T BaseType;
  // Type for parameters
  typedef AutoDiffA<T> ParamType;
  // Type for arguments
  typedef AutoDiffA<T> ArgType;
};

#undef FunctionTraits_PA

#endif
