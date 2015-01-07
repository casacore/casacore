//# LELFunctionEnums.h: Enums of function names
//# Copyright (C) 1997,1998,1999,2001,2003
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This library is free software; you can redistribute it and/or modify it
//# under the terms of the GNU Library General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or (at your
//# option) any later version.
//#
//# This library is distributed in the hope that it will be useful, but WITHOUT//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
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

#ifndef LATTICES_LELFUNCTIONENUMS_H
#define LATTICES_LELFUNCTIONENUMS_H

#include <casacore/casa/aips.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary> Each LEL function is described in this enum </summary>
//
// <use visibility=local>
//
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class="Lattice"> Lattice</linkto>
//   <li> <linkto class="LatticeExpr"> LatticeExpr</linkto>
//   <li> <linkto class="LatticeExprNode"> LatticeExprNode</linkto>
//   <li> <linkto class="LELInterface"> LELInterface</linkto>
// </prerequisite>
//
// <etymology>
//  This enum provides a value for each function accepted
//  by the Lattice Expression Language classes.
// </etymology>
//
// <synopsis>
//  Each function name accepted by the bridging class LatticeExprNode
//  and passed on to the
//  <linkto class="LELFunction1D">LELFunction</linkto>
//  letter classes is labelled internally 
//  with a value from this enum.  
// </synopsis> 
//
// <todo asof="1998/01/21">
// </todo>


class LELFunctionEnums 
{
public:
   enum Function {

// sin
   SIN,

// sinh
   SINH,

// asin 
   ASIN,

// cos
   COS,

// cosh
   COSH,

// acos
   ACOS,

// tan
   TAN,

// tanh
   TANH,

// atan; atan(x) returns the arc tangent of x in the range -pi/2 to  pi/2.
   ATAN,

// atan2; atan2(y,x) computes an arc tangent of y/x in the range -pi to pi
   ATAN2,

// exp
   EXP,

// log
   LOG,

// log10
   LOG10,

// power pow(x,y) ==  x**y or x^y
   POW,

// sqrt
   SQRT,

// round
   ROUND,

// sign (-1 if <0; 0 if 0; 1 if >0)
   SIGN,

// ceil; returns the least  integral  value  greater  than  or  equal  to  x. 
   CEIL,

// floor; returns the greatest integral  value  less  than  or equal  to  x.
   FLOOR,

// abs
   ABS,

// phase (of complex number)
   ARG,

// real (part of number)
   REAL,

// imag (inary part of complex number)
   IMAG,

// conj (ugate complex number)
   CONJ,

// complex (form complex from 2 reals)
   COMPLEX,

// fmod; fmod(x,y) returns the remainder  of  x  with  respect  to  y;  that is, 
// the result r is one of the numbers that differ from x by an integral multiple of y.  
   FMOD,

// min; min(x,y)
   MIN,

// max; max(x,y)
   MAX,

// min; min(x) (is a scalar)
   MIN1D,

// max; max(x) (is a scalar)
   MAX1D,

// mean; mean(x) (is a scalar)
   MEAN1D,

// median; median(x) (is a scalar)
   MEDIAN1D,

// fractile; fractile(x,fraction) (is a scalar)
   FRACTILE1D,

// fractilerange; fractilerange(x,fraction1[,fraction2]) (is a scalar)
   FRACTILERANGE1D,

// sum; sum(x) (is a scalar)
   SUM,

// nelements; nelements(x) (is a scalar)
   NELEM,

// all (true) (is a scalar)
   ALL,

// any (true) (is a scalar)
   ANY,

// ntrue 
   NTRUE,

// nfalse
   NFALSE,

// mask
   MASK,

// value
   VALUE,

// iif (similar to ?: in C++)
   IIF,

// replace
   REPLACE,

// dimensionality
   NDIM,

// length (of an axis)
   LENGTH,

// is the value a NaN?
   ISNAN,

// a bool array telling which indices of an axis are to be used
   INDEXIN,

// number of functions
   NFUNCTIONS

};

};


} //# NAMESPACE CASACORE - END

#endif
