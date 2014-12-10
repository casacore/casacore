//# TVecMath.h: Global helper functions for table vector mathematics
//# Copyright (C) 1994,1995,1999
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

#ifndef TABLES_TVECMATH_H
#define TABLES_TVECMATH_H

//# Includes
#include <casacore/casa/aips.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
template<class T> class TabVecRep;


// <summary>
// Basic math for table vectors.
// </summary>

// <use visibility=local>

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
template<class T> void tabVecReptvassadd (TabVecRep<T>&, const TabVecRep<T>&);
// Subtract 2 table vectors storing result in first one.
template<class T> void tabVecReptvasssub (TabVecRep<T>&, const TabVecRep<T>&);
// Multiple 2 table vectors storing result in first one.
template<class T> void tabVecReptvasstim (TabVecRep<T>&, const TabVecRep<T>&);
// Divide 2 table vectors storing result in first one.
template<class T> void tabVecReptvassdiv (TabVecRep<T>&, const TabVecRep<T>&);

// Add a scalar to each element in the table vector.
template<class T> void tabVecRepvalassadd (TabVecRep<T>&, const T&);
// Subtract a scalar from each element in the table vector.
template<class T> void tabVecRepvalasssub (TabVecRep<T>&, const T&);
// Multiple each element in the table vector with a scalar.
template<class T> void tabVecRepvalasstim (TabVecRep<T>&, const T&);
// Divide each element in the table vector by a scalar.
template<class T> void tabVecRepvalassdiv (TabVecRep<T>&, const T&);

// Unary minus - store result in a new vector.
// <note role=tip>
// (unary plus is already handled in TabVecMath).
// </note>
//
template<class T> TabVecRep<T>& tabVecRepnegate (const TabVecRep<T>&);

// Add 2 table vectors storing result in a new one.
template<class T> TabVecRep<T>& tabVecReptvadd (const TabVecRep<T>&,
						const TabVecRep<T>&);
// Subtract 2 table vectors storing result in a new one.
template<class T> TabVecRep<T>& tabVecReptvsub (const TabVecRep<T>&,
						const TabVecRep<T>&);
// Multiple 2 table vectors storing result in a new one.
template<class T> TabVecRep<T>& tabVecReptvtim (const TabVecRep<T>&,
						const TabVecRep<T>&);
// Divide 2 table vectors storing result in a new one.
template<class T> TabVecRep<T>& tabVecReptvdiv (const TabVecRep<T>&,
						const TabVecRep<T>&);

// Add a scalar to each element in the table vector storing result
// in a new table vector.
template<class T> TabVecRep<T>& tabVecRepvalradd (const TabVecRep<T>&,
						  const T&);
// Subtract a scalar from each element in the table vector storing result
// in a new table vector.
template<class T> TabVecRep<T>& tabVecRepvalrsub (const TabVecRep<T>&,
						  const T&);
// Multiple each element in the table vector with a scalar storing result
// in a new table vector.
template<class T> TabVecRep<T>& tabVecRepvalrtim (const TabVecRep<T>&,
						  const T&);
// Divide each element in the table vector by a scalar storing result
// in a new table vector.
template<class T> TabVecRep<T>& tabVecRepvalrdiv (const TabVecRep<T>&,
						  const T&);

// Add a scalar to each element in the table vector storing result
// in a new table vector.
template<class T> TabVecRep<T>& tabVecRepvalladd (const T&,
						  const TabVecRep<T>&);
// Subtract a scalar from each element in the table vector storing result
// in a new table vector.
template<class T> TabVecRep<T>& tabVecRepvallsub (const T&,
						  const TabVecRep<T>&);
// Multiple each element in the table vector with a scalar storing result
// in a new table vector.
template<class T> TabVecRep<T>& tabVecRepvalltim (const T&,
						  const TabVecRep<T>&);
// Divide each element in the table vector by a scalar storing result
// in a new table vector.
template<class T> TabVecRep<T>& tabVecRepvalldiv (const T&,
						  const TabVecRep<T>&);
// </group>



// <summary>
// Transcendental math for table vectors.
// </summary>

// <use visibility=local>

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
template<class T> TabVecRep<T>& tabVecRepcos  (const TabVecRep<T>&);
template<class T> TabVecRep<T>& tabVecRepcosh (const TabVecRep<T>&);
template<class T> TabVecRep<T>& tabVecRepexp  (const TabVecRep<T>&);
template<class T> TabVecRep<T>& tabVecReplog  (const TabVecRep<T>&);
template<class T> TabVecRep<T>& tabVecReplog10(const TabVecRep<T>&);
template<class T> TabVecRep<T>& tabVecReppow  (const TabVecRep<T>&,
					       const TabVecRep<T>&);
template<class T> TabVecRep<T>& tabVecRepsin  (const TabVecRep<T>&);
template<class T> TabVecRep<T>& tabVecRepsinh (const TabVecRep<T>&);
template<class T> TabVecRep<T>& tabVecRepsqrt (const TabVecRep<T>&);
// </group>



// <summary>
// Further transcendental math for table vectors.
// </summary>

// <use visibility=local>

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
template<class T> TabVecRep<T>& tabVecRepacos (const TabVecRep<T>&);
template<class T> TabVecRep<T>& tabVecRepasin (const TabVecRep<T>&);
template<class T> TabVecRep<T>& tabVecRepatan (const TabVecRep<T>&);
template<class T> TabVecRep<T>& tabVecRepatan2(const TabVecRep<T>&,
					       const TabVecRep<T>&);
template<class T> TabVecRep<T>& tabVecRepceil (const TabVecRep<T>&);
template<class T> TabVecRep<T>& tabVecRepfabs (const TabVecRep<T>&);
template<class T> TabVecRep<T>& tabVecRepfloor(const TabVecRep<T>&);
template<class T> TabVecRep<T>& tabVecRepfmod (const TabVecRep<T>&,
					       const TabVecRep<T>&);
template<class T> TabVecRep<T>& tabVecReppow  (const TabVecRep<T>&,
					       const double&);
template<class T> TabVecRep<T>& tabVecReptan  (const TabVecRep<T>&);
template<class T> TabVecRep<T>& tabVecReptanh (const TabVecRep<T>&);
// </group>



// <summary>
// Miscellaneous table vector operations.
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <synopsis>
// Fill a table vector or calculate the sum, product, minimum or
// maximum of its elements.
// </synopsis>

// <group name=miscellaneous>
// Determine minimum and maximum value in a table vector.
// Requires that the type "T" has comparison operators.
template<class T> void tabVecRepminmax (T& min, T& max, const TabVecRep<T>&);

// Fills all elements of the table vector with a sequence starting with
// "start" and incrementing by "inc" for each element.
template<class T> void tabVecRepindgen (TabVecRep<T>&, Int start, Int inc);

// Sum of all the elements of a table vector.
template<class T> T tabVecRepsum (const TabVecRep<T>&);

// Product of all the elements of a table vector.
// <note role=warning>
// product can easily overflow.
// </note>
template<class T> T tabVecRepproduct (const TabVecRep<T>&);
// </group>




// <summary>
// Vector operations on a table vector.
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <synopsis>
// Do vector operations on a table vector (like inner product).
// </synopsis>

// <group name=vectorMath>
// The inner product of 2 table vectors.
template<class T> T tabVecRepinnerproduct (const TabVecRep<T>&,
					   const TabVecRep<T>&);

// The norm of a table vector.
template<class T> T tabVecRepnorm (const TabVecRep<T>&);

// The cross product of 2 table vectors containing 3 elements.
template<class T> TabVecRep<T>& tabVecRepcrossproduct (const TabVecRep<T>&,
						       const TabVecRep<T>&);
// </group>



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/tables/Tables/TVecMath.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
