//# ArrayOpsDiffShapes.h: Operations for 2 Arrays with possibly different shapes.
//# Copyright (C) 2009
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify
//# it under the terms of the GNU General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or
//# (at your option) any later version.
//#
//# This program is distributed in the hope that it will be useful,
//# but WITHOUT ANY WARRANTY; without even the implied warranty of
//# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//# GNU General Public License for more details.
//# 
//# You should have received a copy of the GNU General Public License
//# along with this program; if not, write to the Free Software
//# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

#ifndef CASA_ARRAYOPSDIFFSHAPES_H
#define CASA_ARRAYOPSDIFFSHAPES_H

#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/ArrayLogical.h>
//#include <casa/Arrays/ArrayUtil.h>
#include <casa/Arrays/IPosition.h>
//#include <casa/Arrays/Slice.h>
//#include <casa/BasicSL/String.h>

// Don't forget a .tcc file is included at the end!

namespace casa { //# NAMESPACE CASA - BEGIN

// <summary>
//    Operations for 2 Arrays with possibly different shapes.
// </summary>
// <!-- <reviewed reviewer="UNKNOWN" date="" tests=""> -->
//
// <prerequisite>
//   <li> <linkto class=Array>Array</linkto>
//   <li> <linkto>ArrayMath</linkto>
// </prerequisite>
//
// <etymology>
// This file contains global functions that attempt binary operations with
// arrays that have possibly differing shapes.
// </etymology>
//
// <synopsis>
// These functions perform operations on two arrays, left and right, which go
// chunk by chunk in left and element by element in right, as long as right
// spans a subspace of left.  If left's shape has more dimensions than right's,
// each entry in right will effectively be replicated as necessary to act on
// each entry in the corresponding chunk of left.
// e.g. if left's shape is (256, 256, 1, 4), and right's is (256, 256), 
// left(i, j, 1, l) will be operated on with right(i, j) for i & j from 0 to
// 255 and l from 0 to 3.  Note that right must be either reformable to left's
// shape (same # of elements) or its shape must equal left's shape "as far as
// it goes", i.e. where right's dimensions are defined.
// </synposis>
//
// <example>
// <srcblock>
//   Array<Complex> a(10, 6);
//   Vector<Int>    b(10);
//   Array<Complex> c(10, 6);
//   
//   c = binOpExpandR(a, b, std::plus<Complex>());
// </srcblock>
// This example sets c(i, j) to a(i, j) + b(i).  It checks that either b's
// shape can be reformed to a's (same # of elements) or that a's shape is the
// same as b's where b's dimensions are defined.
// The result of this operation is an Array.
// </example>

// Returns a LogicalArray with elements (at pos) set to (data(pos) ==
// truthvalue).  data is effectively collapsed using anyEQ if necessary to
// fit desiredform.  Throws an exception if that does not work.
template<typename T>
LogicalArray reformedMask(const Array<T>& data, const T truthvalue,
			  const IPosition& desiredform);

// Can arrays left and right with respective shapes leftShape and rightShape be
// used in function(left, right, ...) for the other functions declared here?
Bool rightExpandableToLeft(const IPosition& leftShape, const IPosition& rightShape);

// Apply op elementwise to left and right, replicating elements of right as
// necessary (see example above).  Throws an ArrayConformanceError exception if
// that cannot be done.
//
// Currently assumes that it is the trailing axes of left that right will be
// replicated along.  e.g. if left's shape is (1, 2, 4) and right's is (1, 2),
// the result will be left(i, j, k) op right(i, j) for all (i, j, k).
//
// template<typename L, typename R, typename BinaryOperator, typename RES>
// Array<RES> binOpExpandR(const Array<L>& left, const Array<R>& right,
// 			BinaryOperator op);

// Like binOpExpandR(left, right, res, op), but work on left in place.
template<typename L, typename R, typename BinaryOperator>
void binOpExpandInPlace(Array<L>& left, const Array<R>& right, BinaryOperator op);

} //#End casa namespace

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casa/Arrays/ArrayOpsDiffShapes.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
