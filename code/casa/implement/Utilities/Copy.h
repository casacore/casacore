//# Copy.h: Copy objects from one C-style array to another.
//# Copyright (C) 1994,1995,1996,1997,1999,2000
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

#if !defined(AIPS_COPY_H)
#define AIPS_COPY_H

//# Includes
#include <aips/aips.h>
#include <aips/Mathematics/Complex.h>
#include <string.h>           // for memmove, etc.


// <summary>
// Copy objects from one C-style array to another.
// </summary>

// <use visibility=export>
// <reviewed reviewer="Friso Olnon" date="1995/03/14" tests="tCopy" demos="">
// </reviewed>

// <synopsis>
// Objset is used to fill a C-style array of objects.
//
// Objcopy and objmove are used to copy objects from one place to
// another. Optionally a stride can be supplied. The C++ standard library
// will have functions much like these.
// The functions are equivalent to C's <src>memcpy</src> and
// <src>memmove</src>.
// In fact, when possible memcpy and memmove will be used.
//
// Similar to memcpy and memmove, the difference between objcopy
// and objmove is that objmove takes account of an overlap of source and
// destination. In general, objcopy is slighty (but only slighty) faster.
// Only when memcpy can be used for overlapping source and destination,
// objcopy is much faster (but that is in fact an error).
// <note> A product like TestCenter will issue a warning when memcpy
// is used with an overlapping source and destination.
// </synopsis>

// <example>
// Setting and copying arrays of built-in types:
// <srcblock>
// // Create uInt array of 4 elements
// uInt size=4;
// int* ia = new int[size];
// // Initialize all elements to value 99
// objset(ia, 99, size);
// // Change all odd elements to 66 -> [99 66 99 66]
// objset(ia+1,66, uInt(5), uInt(2));
//
// // Create another 4-element uInt array
// int* ia2 = new int[size];
// // Copy array ia into array ia2 -> [99 66 99 66]
// objmove(ia2, ia, size);
// // Copy the even elements of ia to the odd elements of ia2
// //                              -> [99 99 99 99]
// objcopy(ia2+1, ia, uInt(size/2), uInt(2), uInt(2));
// </srcblock>
//
// Setting and copying arrays of a randomly chosen type:
// <srcblock>
// // Create 4-element array of 3-element Block<int> objects 
// uInt size=4;
// Block<int>* ta = new Block<int>[size];
// Block<int> set(3);
// // Initialize the array -> [[123][123][123][123]]
// set[0] = 1; set[1] = 2; set[2] = 3;
// objset(ta, set, size);
// // Change odd Blocks to [777]-> [[123][777][123][777]]
// set[0] = set[1] = set[2] = 7;
// objset(ta + 1, set, uInt(size/2), uInt(2));
//
// // Create another Block<int> array 
// Block<int>* ta2 = new Block<int>[size];
// // Copy the even elements of ta to the first elements of ta2
// //                      -> [[123][123]...]
// objcopy(ta2, ta, uInt(size/2), uInt(1), uInt(2));
// </srcblock>
// </example>

// <group name=copy>


// The preferred function to copy <src>n</src> objects from one place
// to another.  Strides may be specified, i.e. you may copy from every
// <src>fromStride</src>-th position into every <src>toStride</src>-th
// one.
//
// For built-in types the function will call <src>memmove</src> when possible.
// Objmove works correctly if the source and destination overlap in any way.
//
// An exception will be thrown if the source or the destination does not
// exist or if the strides are non-positive.
// <thrown>
//  <li> AipsError
// </thrown>
//
// <group>
template<class T> void objmove (T* to, const T* from, uInt n);
template<class T> void objmove (T* to, const T* from, uInt n, uInt toStride,
				uInt fromStride);
// </group> 

//# Specializations of objmove for the built-in types.
inline void objmove (Bool* to, const Bool* from, uInt n)
    { memmove (to, from, n*sizeof(Bool)); }
inline void objmove (Char* to, const Char* from, uInt n)
    { memmove (to, from, n*sizeof(Char)); }
inline void objmove (uChar* to, const uChar* from, uInt n)
    { memmove (to, from, n*sizeof(uChar)); }
inline void objmove (Short* to, const Short* from, uInt n)
    { memmove (to, from, n*sizeof(Short)); }
inline void objmove (uShort* to, const uShort* from, uInt n)
    { memmove (to, from, n*sizeof(uShort)); }
inline void objmove (Int* to, const Int* from, uInt n)
    { memmove (to, from, n*sizeof(Int)); }
inline void objmove (uInt* to, const uInt* from, uInt n)
    { memmove (to, from, n*sizeof(uInt)); }
inline void objmove (float* to, const float* from, uInt n)
    { memmove (to, from, n*sizeof(float)); }
inline void objmove (double* to, const double* from, uInt n)
    { memmove (to, from, n*sizeof(double)); }
inline void objmove (Complex* to, const Complex* from, uInt n)
    { memmove (to, from, n*sizeof(Complex)); }
inline void objmove (DComplex* to, const DComplex* from, uInt n)
    { memmove (to, from, n*sizeof(DComplex)); }

//# To support a container of const void*.
inline void objmove (const void** to, const void*const *from, uInt n)
    { memmove (to, from, n*sizeof(void*)); }
inline void objmove (const char** to, const char*const *from, uInt n)
    { memmove (to, from, n*sizeof(char*)); }

//# To support a container of void*.
// On the HP g++ is unhappy with these specializations (only!) for some reason
#if !defined(__hpux__)
inline void objmove (void** to, void*const *from, uInt n)
    { memmove (to, from, n*sizeof(void*)); }
inline void objmove (char** to, char*const *from, uInt n)
    { memmove (to, from, n*sizeof(char*)); }
#endif



// The non-preferred function to copy <src>n</src> objects from one place
// to another.  Strides may be specified, i.e. you may copy from every
// <src>fromStride</src>-th position into every <src>toStride</src>-th
// one.
//
// For built-in types the function will call <src>memcpy</src> when possible.
// Objcopy does not take an overlap of source and destination into account.
// Objmove should be used if that is an issue.
//
// An exception will be thrown if the source or the destination does not
// exist or if the strides are non-positive.
// <thrown>
//  <li> AipsError
// </thrown>
//
// <group>
template<class T> void objcopy (T* to, const T* from, uInt n);
template<class T> void objcopy (T* to, const T* from, uInt n, uInt toStride,
				uInt fromStride);
// </group> 

//# Specializations of objcopy for the built-in types.
inline void objcopy (Bool* to, const Bool* from, uInt n)
    { memcpy (to, from, n*sizeof(Bool)); }
inline void objcopy (Char* to, const Char* from, uInt n)
    { memcpy (to, from, n*sizeof(Char)); }
inline void objcopy (uChar* to, const uChar* from, uInt n)
    { memcpy (to, from, n*sizeof(uChar)); }
inline void objcopy (Short* to, const Short* from, uInt n)
    { memcpy (to, from, n*sizeof(Short)); }
inline void objcopy (uShort* to, const uShort* from, uInt n)
    { memcpy (to, from, n*sizeof(uShort)); }
inline void objcopy (Int* to, const Int* from, uInt n)
    { memcpy (to, from, n*sizeof(Int)); }
inline void objcopy (uInt* to, const uInt* from, uInt n)
    { memcpy (to, from, n*sizeof(uInt)); }
inline void objcopy (float* to, const float* from, uInt n)
    { memcpy (to, from, n*sizeof(float)); }
inline void objcopy (double* to, const double* from, uInt n)
    { memcpy (to, from, n*sizeof(double)); }
inline void objcopy (Complex* to, const Complex* from, uInt n)
    { memcpy (to, from, n*sizeof(Complex)); }
inline void objcopy (DComplex* to, const DComplex* from, uInt n)
    { memcpy (to, from, n*sizeof(DComplex)); }

//# To support a container of const void*.
inline void objcopy (const void** to, const void*const *from, uInt n)
    { memcpy (to, from, n*sizeof(void*)); }
inline void objcopy (const char** to, const char*const *from, uInt n)
    { memcpy (to, from, n*sizeof(char*)); }

//# To support a container of void*.
// On the HP g++ is unhappy with these specializations (only!) for some reason
#if !defined(__hpux__)
inline void objcopy (void** to, void*const *from, uInt n)
    { memcpy (to, from, n*sizeof(void*)); }
inline void objcopy (char** to, char*const *from, uInt n)
    { memcpy (to, from, n*sizeof(char*)); }
#endif



// Fill <src>n</src> elements of an array of objects with the given
// value, optionally with a stride. Note that the fillValue is passed
// by value.
//
// An exception will be thrown if the destination array does not exist
// or if the stride is non-positive.
//
// <thrown>
//  <li> AipsError
// </thrown>
//
// <group>
template<class T> void objset (T* to, T fillValue, uInt n);
template<class T> void objset (T* to, T fillValue, uInt n, uInt toStride);
// </group>

// </group>

#endif
