//# aips.h: Global initialization for namespace management, standard types, etc.
//# Copyright (C) 1993-1997,1998,1999
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

#if !defined(AIPS_ENTER_)
#include <aips/aips_enter.h>
#endif

#if !defined(AIPS_AIPS_H)
#define AIPS_AIPS_H

//# For size_t
#include <stdlib.h>


// Including aips.h should guarantee that AIPS++ namespace management is
// switched on - it may have been explicitly switched off beforehand by
// aips_exit.h.

// Defined the "aips_" macro which is used for name space management, and
// the "aips_name2" macro which is used to join two tokens.

#if defined(__STDC__) || defined(__ANSI_CPP__) || defined(__hpux)
#define aips_(x)  aips##x
#define aips_name2(a,b) a##b
#else
#define aips_(x)  aips/**/x
#define aips_name2(a,b) a/**/b
#endif

// This section contains the various standard types used by AIPS++.
typedef bool Bool;
const Bool True = true;
const Bool False = false;

typedef char Char;
typedef unsigned char uChar;
typedef short Short;
typedef unsigned short uShort;
typedef int Int;
typedef unsigned int uInt;
typedef long Long;
typedef unsigned long uLong;
typedef float Float;
typedef double Double;
typedef long double lDouble;

#define at_c(X) X
#define at_cc(X) X

// This is an inline guaranteed-correct conversion function which converts
// to Bool any type which has meaning in a C++ logical expression.
// <group>
inline Bool ToBool (Bool val) {return val;}
inline Bool ToBool (const void *val) {return Bool(val!=0);}
inline Bool ToBool (long val) {return Bool(val!=0);}
inline Bool ToBool (unsigned long val) {return Bool(val!=0);}
inline Bool ToBool (int val) {return Bool(val!=0);}
inline Bool ToBool (unsigned int val) {return Bool(val!=0);}
inline Bool ToBool (double val) {return Bool(val!=0.0);}
// </group>

// If AIPS_DEBUG is not defined, then the symbol expands to (0) which in an
// if should be removed by the dead code eliminator of any optimizer; thus
// using this in your code should have no performance penalty in the normal
// case. If compiled with AIPS_DEBUG, then aips_debug is (defined to )
// a global boolean variable (so it can be turned on and off in a debugger) 
// which is initialized to True.

extern Bool aips_debug_on;

#if !defined(AIPS_DEBUG)
#define aips_debug (0)
#else
// The reason that we just don't make this a variable here is so that
// we can link against libraries compiled with or without AIPS_DEBUG
// without having any missing symbols.
#define aips_debug aips_debug_on
#endif

// This is just syntactic sugar to tell the namespace management that a
// particular class is not under control of namespace management. The only
// place this will normally be needed is forward declarations of imported
// classes, e.g. from InterViews.
#define imported /* Nothing */


#endif
