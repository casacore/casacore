//# Math.h: AIPS++ interface to <math.h> and other scalar math functions
//# Copyright (C) 1993,1994,1995,1996,1997,1998
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

#if !defined (AIPS_MATH_H)
#define AIPS_MATH_H

#include <aips/aips.h>
#include <math.h>
//# The following is to get abs(int)
#include <stdlib.h>
// <summary>AIPS++ interface to math.h and other scalar math functions</summary>

// <synopsis> 

// AIPS++ interface to <src><math.h></src>. You should include this file rather
// than <src><math.h></src> directly.  It will be used to cover up any
// deficiencies in the system <src><math.h></src>.

// This file does not include things like element-by-element
// array operations. See the 
// <linkto group="ArrayMath.h#Array mathematical operations">ArrayMath</linkto>
// functions for these functions.

// This file includes the standard math library. Hence besides the functions
// defined here the following functions are also available.
// <srcblock>
// Double sin(Double x)       Sine function
// Double cos(Double x)       Cosine function
// Double tan(Double x)       Tangent function
// Double asin(Double x)      Inverse sine function
// Double acos(Double x)      Inverse cosine function
// Double atan(Double x)      Inverse tangent function
// Double atan2(Double y, Double x) Four quandrant inverse tangent function
// Double hypot(Double y, Double x) Euclidean distance sqrt(x*x+y*y)

// Double sinh(Double x)      Hyperbolic sine
// Double cosh(Double x)      Hyperbolic cosine
// Double tanh(Double x)      Hyperbolic tangent
// Double acosh(Double x)     Inverse hyperbolic sine
// Double asinh(Double x)     Inverse hyperbolic cosine
// Double atanh(Double x)     Inverse hyperbolic tangent

// Double sqrt(Double x)      Square root
// Double cbrt(Double x)      Cube root

// Double pow(Double x, Double y) x raised to the power of y
// Double exp(Double x)       Exponental function
// Double expm1(Double x)     exp(x)-1. Use when x is small.
// Double log(Double x)       Natural logarithm
// Double log10(Double x)     Base ten logarithm
// Double log1p(Double x)     log(x+1). Use when x is small

// Double j0(Double x)        Bessel function of the first kind, zeroth order 
// Double j1(Double x)        Bessel function of the first kind, first order
// Double jn(Int n, Double x) Bessel function of the first kind nth order
// Double y0(Double x)        Bessel function of the second kind, zeroth order 
// Double y1(Double x)        Bessel function of the second kind, first order 
// Double yn(Int n, Double x) Bessel function of the second kind, nth order
//
// Double lgamma(Double x)    Natural Log of the absolute value of the gamma
//                            function
// Double lgamma_r(Double x, Int* sign) Same as lgamma. The sign of the gamma
//                            function is returned in the second argument.

// Double erf(Double x)       Error function
// Double erfc(Double x)      Complementary error function (1 - erf(x)). 
//                            Use for large x.

// Double ceil(Double x)      Returns the least integral value greater than or
//                            equal to x
// Double floor(Double x)     Returns the least integral value than than or
//                            equal to x
// Double rint(Double x)      Round to an integer using the current direction.

// Double fabs(Double x)      Absolute value of x
// Double remainder(Double x, Double y) the remainder. x - y*Int(x/y)
// Double fmod(Double x, Double y) As above. May differ by +/- y
// Int isnan(Double x) Returns 1 if x is a NaN, zero otherwise
// Int ilogb(Double x) Unbiased exponent of x
// Double logb(Double x)      As above but returns floating point result
// Double scalbn(Double x, Int n) x*2**n. Uses exponent manipulation.
// Double scalb(Double x, Double n) x*2**n. As above but n is a Double
// Double significand(Double x) Returns the fractional part of x 
//                            (between 1 and 2)
// Double copysign(Double x, Double y)  returns a value with the magnitude of
//                                      x and the sign bit of y.
// Double nextafter(Double x, Double y) Returns the next machine representable
//                            number after x in the direction specified by y
// </srcblock>
// 

// This file also includes the standard C library (stdlib.h). This is to obtain
// a definition of the following functions.
// <srcblock>
// Int abs(Int x)             absolute value function
// </synopsis>

// <group name="Math.h interface for AIPS++">

// Returns f1**f2. The Double precision version is defined in the standard
// library. But many compilers are not good enough to automatically do the type
// promotion. Hence these functions are explicitly defined.
// <group>
inline Float pow(Float f1, Float f2) 
{return Float(pow(Double(f1), Double(f2)));}
inline Float pow(Float f1, Double f2) {return Float(pow(Double(f1), f2));}
inline Float pow(Double f1, Float f2) {return Float(pow(f1, Double(f2)));}
inline Int pow(Int f1, Int f2) {return Int(pow(Double(f1), Double(f2)));}
// </group>

// Return the integer "less than" point (i.e. the one further from zero if
// "point" is negative.
// <group>
inline Int ifloor(Float point)
{ if (point >= 0.0) return Int (point); else return Int(point - 1.0); }
inline Int ifloor(Double point) 
{ if (point >= 0.0) return Int(point); else return Int(point - 1.0); }
// </group>

//  Functions to get the max or min of two numbers.
// <group>
inline Int max(Int a, Int b) { if (a > b) return a; else return b; } 
inline Int min(Int a, Int b) { if (a > b) return b; else return a; }

inline uInt max(uInt a, uInt b){ if (a>b) return a; else return b; }
inline uInt min(uInt a, uInt b){ if (a>b) return b; else return a; }

inline Double max(Double a, Double b) { if (a > b) return a; else return b; } 
inline Double min(Double a, Double b) { if (a > b) return b; else return a; }

inline Float max(Float a, Float b) { if (a > b) return a; else return b; } 
inline Float min(Float a, Float b) { if (a > b) return b; else return a; }
// </group>

// Get the absolute value of Float or Double values. Should already be defined
// for integers in <src><stdlib.h></src>.  Define it for uInts so that certain
// compilers can resolve the ambiguity when used in a templated class.
// <group>
inline Float abs(Float Val) {if (Val >= 0) return Val; else return -Val;}
inline Double abs(Double Val) {return fabs(Val);}
inline uInt abs(uInt Val) {return Val;}
// </group>

// Return the square of a value.
// <group>
inline Int square(Int val) {return val*val;}
inline Float square(Float val) {return val*val;}
inline Double square(Double val) {return val*val;}
// </group>

// Return the cube of a value.
// <group>
inline Int cube(Int val) {return val*val*val;}
inline Float cube(Float val) {return val*val*val;}
inline Double cube(Double val) {return val*val*val;}
// </group>

// Functions to return whether a value is "relatively" near another. Returns
// <src> tol > abs(val2 - val1)/max(abs(val1),(val2))</src>. 
// If tol <= 0, returns val1 == val2. If either val is 0.0, take care of area
// around the minimum number that can be represented.
// <group>
Bool near(uInt val1, uInt val2, Double tol = 1.0e-5);
Bool near(Int val1, Int val2, Double tol = 1.0e-5);
Bool near(Float val1, Float val2, Double tol = 1.0e-5);
Bool near(Double val1, Double val2, Double tol = 1.0e-13);
// </group>

// The "allNear" versions are aliases for the normal "near" versions. They
// exist to make template functions that work for both arrays and scalars
// easier to write. These functions should be moved to ArrayMath.h
// <group>
inline Bool allNear(uInt val1, uInt val2, Double tol = 1.0e-5)
    { return near(val1, val2, tol); }
inline Bool allNear(Int val1, Int val2, Double tol = 1.0e-5)
    { return near(val1, val2, tol); }
inline Bool allNear(Float val1, Float val2, Double tol = 1.0e-5)
    { return near(val1, val2, tol); }
inline Bool allNear(Double val1, Double val2, Double tol = 1.0e-13)
    { return near(val1, val2, tol); }
// </group>

// Functions to return whether a value is "absolutely" near another. Returns
// <src> tol > abs(val2 - val1)</src> 
// <group>
Bool nearAbs(uInt val1, uInt val2, Double tol = 1.0e-5);
Bool nearAbs(Int val1, Int val2, Double tol = 1.0e-5);
Bool nearAbs(Float val1, Float val2, Double tol = 1.0e-5);
Bool nearAbs(Double val1, Double val2, Double tol = 1.0e-13);
// </group>

// The "allNearAbs" versions are aliases for the normal "nearAbs"
// versions. They exist to make template functions that work for both arrays
// and scalars easier to write. These functions should be in ArrayMath.h
// <group>
inline Bool allNearAbs(uInt val1, uInt val2, uInt tol = 1)
    { return nearAbs(val1, val2, tol); }
inline Bool allNearAbs(Int val1, Int val2, Int tol = 1)
    { return nearAbs(val1, val2, tol); }
inline Bool allNearAbs(Float val1, Float val2, Double tol = 1.0e-5)
    { return nearAbs(val1, val2, tol); }
inline Bool allNearAbs(Double val1, Double val2, Double tol = 1.0e-13)
    { return nearAbs(val1, val2, tol); }
// </group>


// Functions to set and test for IEEE NaN's.
// <group>
Bool isNaN(const Float &val);
Bool isNaN(const Double &val);
void setNaN(Float &val);
void setNaN(Double &val);
// </group>
// </group>

#endif
