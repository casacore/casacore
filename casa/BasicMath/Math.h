//# Math.h: Casacore interface to <math.h> and other scalar math functions
//# Copyright (C) 1993,1994,1995,1996,1997,1998,1999,2000,2001
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

#ifndef CASA_MATH_H
#define CASA_MATH_H

#include <casacore/casa/aips.h>
//# The following is to get abs(int) and (is)finite.
#include <casacore/casa/math.h>
#include <casacore/casa/stdlib.h>

// On some systems the following is needed to get the finite function
#if defined (AIPS_SOLARIS) || defined(AIPS_IRIX)
#include <ieeefp.h>
#endif

namespace casacore { //# NAMESPACE CASACORE - BEGIN


// <summary>
// Casacore interface to math.h and other scalar math functions
// </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>

// <synopsis> 

// Casacore interface to <src><math.h></src>. You should include this file
// rather than <src><math.h></src> directly.  It will be used to cover up any
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
// Int isNaN(Double x) Returns 1 if x is a NaN, zero otherwise
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
// </srcblock>
// </synopsis>

// <group name="Math interface for casacore">

// Returns f1**f2. The Double precision version is defined in the standard
// library. But many compilers are not good enough to automatically do the type
// promotion. Hence these functions are explicitly defined.
// <group>
inline Float pow(Float f1, Double f2) {return Float(std::pow(Double(f1), f2));}
inline Float pow(Double f1, Float f2) {return Float(std::pow(f1, Double(f2)));}
inline Int pow(Int f1, Int f2) {return Int(std::pow(Double(f1), Double(f2)));}
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
inline Double max(Double a, Float b) { if (a > b) return a; else return b; } 
inline Double min(Double a, Float b) { if (a > b) return b; else return a; }
inline Double max(Float a, Double b) { if (a > b) return a; else return b; } 
inline Double min(Float a, Double b) { if (a > b) return b; else return a; }

inline Float max(Float a, Float b) { if (a > b) return a; else return b; } 
inline Float min(Float a, Float b) { if (a > b) return b; else return a; }
// </group>

// Get the absolute value of uInt. Should already be defined
// for integers in <src><stdlib.h></src>.  Define it for uInts so that certain
// compilers can resolve the ambiguity when used in a templated class.
// <group>
#if defined(AIPS_BSD)                                                          
  inline Int64 abs(Int64 Val) {return Val;}                                    
#else                                                                          
  inline uInt abs(uInt Val) {return Val;}  
#endif
// </group>

// Return the square of a value.
// <group>
inline Int    square(Int val)    {return val*val;}
inline Float  square(Float val)  {return val*val;}
inline Double square(Double val) {return val*val;}
// </group>

// Return the cube of a value.
// <group>
inline Int    cube(Int val)    {return val*val*val;}
inline Float  cube(Float val)  {return val*val*val;}
inline Double cube(Double val) {return val*val*val;}
// </group>

// Return the sign of a value.
// <group>
inline Int    sign(Int val)    {return val<0 ? -1 : (val>0 ? 1:0);}
inline Float  sign(Float val)  {return val<0 ? -1 : (val>0 ? 1:0);}
inline Double sign(Double val) {return val<0 ? -1 : (val>0 ? 1:0);}
// </group>

// Functions to return whether a value is "relatively" near another. Returns
// <src> tol > abs(val2 - val1)/max(abs(val1),(val2))</src>. 
// If tol <= 0, returns val1 == val2. If either val is 0.0, take care of area
// around the minimum number that can be represented.
// <group>
Bool near(uInt val1, uInt val2, Double tol = 1.0e-5);
Bool near(Int val1, Int val2, Double tol = 1.0e-5);
Bool near(Float val1, Float val2, Double tol = 1.0e-5);
Bool near(Float val1, Double val2, Double tol = 1.0e-5);
Bool near(Double val1, Float val2, Double tol = 1.0e-5);
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
inline Bool allNear(Float val1, Double val2, Double tol = 1.0e-5)
    { return near(val1, val2, tol); }
inline Bool allNear(Double val1, Float val2, Double tol = 1.0e-5)
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
Bool nearAbs(Float val1, Double val2, Double tol = 1.0e-5);
Bool nearAbs(Double val1, Float val2, Double tol = 1.0e-5);
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
inline Bool allNearAbs(Float val1, Double val2, Double tol = 1.0e-5)
    { return nearAbs(val1, val2, tol); }
inline Bool allNearAbs(Double val1, Float val2, Double tol = 1.0e-5)
    { return nearAbs(val1, val2, tol); }
inline Bool allNearAbs(Double val1, Double val2, Double tol = 1.0e-13)
    { return nearAbs(val1, val2, tol); }
// </group>


// Functions to test if a floating point number is finite.
// It is if it is NaN nor infinity.
// <group>
inline Bool isFinite (const Float& val)
{
#if defined(AIPS_DARWIN)
  return std::isfinite(val);
#else
  return finite(val);
#endif
}
inline Bool isFinite (const Double& val)
{
#if defined(AIPS_DARWIN)
  return std::isfinite(val);
#else
  return finite(val);
#endif
}
// </group>

// Functions to test for IEEE NaN's.  The Float variant uses an in-line
// Macro examining the bit pattern (for portability and efficiency). The
// Double version invokes the IEEE function isnan found in ieeefp.h or math.h
// <group>
inline Bool isNaN (const Float& val)
{
  return (((*(Int *)&(val) & 0x7f800000) == 0x7f800000) &&
		((*(Int *)&(val) & 0x007fffff) != 0x00000000));
}
inline Bool isNaN(Double val)
{
  return ( std::isnan(val) );
}
// </group>

// Round a number to <src>ndigit</src> significant digits, usually used
// for formatting for printing.
// <br>A non-integer <src>ndigit=N+F<src>, with integer N and fraction F,
// is interpreted as follows. 
// For <src>x = A*10^B</src>, where B is an integer, A is rounded to N digits
// if <src>A > 10^F</src>, otherwise N+1 digits.
// <br>For the default 2.5, a value of 32157 is rounded to 32000,
// while 22157 is rounded to 22200.
Double roundDouble(Double val, Double ndigit=2.5);

// Functions that return IEEE NaN's. The specific NaN returned has all bits
// set. This is 'quiet' NaN, and because the sign bit is set it may be
// considered a negative number (but NaN's are not numbers!).
// <group>
Float floatNaN();
Double doubleNaN();
void setNaN(Float& val);
void setNaN(Double& val);
// </group>

// Functions to test for IEEE Infinity's. Should work for positive or negative
// infinity.
// <group>
Bool isInf(Float val);
Bool isInf(Double val);
// </group>

// Functions that return an IEEE Infinity,  (positive infinity).
// <group>
Float floatInf();
Double doubleInf();
void setInf(Float& val);
void setInf(Double& val);
// </group>
// </group>


} //# NAMESPACE CASACORE - END

#endif
