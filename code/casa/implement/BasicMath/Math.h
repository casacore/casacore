//# Math.h: AIPS++ interface to <src><math.h></src> and other general functions
//# Copyright (C) 1993,1994,1995,1996,1997
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

#include <aips/aips_exit.h>
#include <math.h>
//# The following is to get abs(int)
#include <stdlib.h>
#include <aips/aips.h>

// <summary> AIPS++ interface to <src><math.h></src> and other general
// functions. </summary>

// <synopsis> 
// AIPS++ interface to <src><math.h></src> .

// You should include this file, <src><aips/Mathematics/Math.h></src>
// rather than <src><math.h></src> directly.  It will be
// used to cover up any deficiencies in the system <src><math.h></src>. This
// does not include things like element-by-element array operations, just
// scalar mathematical functions.
// </synopsis>

// <group name="Math.h interface for AIPS++">


// To get rid of ambiguities on Suns
// Some machines might have efficient versions of these (require ifdefs)
// <group>
#if !defined(AIPS_STDLIB)
inline float pow(float f1, float f2) {return pow (double(f1), double(f2));}
#endif
inline int pow(int f1,int f2) {return int(pow(double(f1), double(f2)));}
// </group>

// Return the integer "less than" point (i.e. the one further from zero if
// "point" is negative.
// <group>
inline Int ifloor(float point)
{
Int result;
if (point >= 0.0)
   result = Int (point);
else
   result = Int (point - 1.0);
return result;
}
inline Int ifloor(double point)
{
Int result;
if (point >= 0.0)
   result = Int (point);
else
   result = Int (point - 1.0);
return result;
}
// </group>

//  Functions to get the max or min of two numbers. This is implemented as
//  an inline function.
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
#if !defined(AIPS_STDLIB)
inline Float abs(Float Val) {return Val > 0 ? Val : -Val;}
#endif

#if !defined(AIPS_IBM) && !defined(AIPS_CL) && !defined(AIPS_STDLIB)
// AIX and CenterLine provde an <src>abs(double)</src>
// (which is probably slower!)
inline Double abs(Double Val) {return Val > 0 ? Val : -Val;}
#endif

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
// exist to make template functions that work for both arrays and scalars easier
// to write.
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
Bool nearAbs(uInt val1, uInt val2, uInt tol = 1);
Bool nearAbs(Int val1, Int val2, Int tol = 1);
Bool nearAbs(Float val1, Float val2, Double tol = 1.0e-5);
Bool nearAbs(Double val1, Double val2, Double tol = 1.0e-13);
// </group>
// The "allNearAbs" versions are aliases for the normal "nearAbs" versions. They
// exist to make template functions that work for both arrays and scalars easier
// to write.
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

// </group>

// Functions to set and test for IEEE NaN's.
// <group>
Bool isNaN(const float &val);
Bool isNaN(const double &val);
void setNaN(float &val);
void setNaN(double &val);
// </group>

#endif
