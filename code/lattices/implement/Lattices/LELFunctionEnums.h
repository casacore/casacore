//# LELFunctionEnums.h: Enums of function names
//# Copyright (C) 1997
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

#if !defined(AIPS_LELFUNCTIONENUMS_H)
#define AIPS_LELFUNCTIONENUMS_H


// <summary>
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="Lattice"> Lattice</linkto>
// </prerequisite>

// <etymology>
// </etymology>
//
// <synopsis>
// </synopsis> 
//
// <example>
// </example>
//
// <motivation>
// </motivation>
//
// <todo asof="1996/07/01">
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

// ceil; returns the least  integral  value  greater  than  or  equal  to  x. 
   CEIL,

// floor; returns the greatest integral  value  less  than  or equal  to  x.
   FLOOR,

// abs
   ABS,

// fmod; fmod(x,y) returns the remainder  of  x  with  respect  to  y;  that is, 
// the result r is one of the numbers that differ from x by an integral multiple of y.  
   FMOD,

// min; min(x,y)
   MIN,

// max; max(x,y)
   MAX,

// min; min(x)
   MIN1D,

// max; max(x)
   MAX1D,

// mean; mean(x)
   MEAN1D,

// sum; sum(x)
   SUM,

// nelements; nelements(x)
   NELEM,

// all
   ALL,

// any
   ANY,

// ntrue
   NTRUE,

// nfalse
   NFALSE,

// number of functions
   NFUNCTIONS

};

};

#endif
