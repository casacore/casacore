//# aipsenv.h: Global initialization for special aips++ macros
//# Copyright (C) 2000,2001,2002,2003,2004
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

// this file contains all the compiler specific defines

#if !defined(AIPS_AIPS_ENV_H)
#define AIPS_AIPS_ENV_H

// AIPS++ project compiler
#if defined(AIPS_GCC)
#undef AIPS_GCC
#endif
#if defined(__GNUC__)
#define AIPS_GCC
#endif

#if defined(AIPS_GCC2)
#undef AIPS_GCC2
#endif
#if (defined(AIPS_GCC) && __GNUC_CC == 2)
#define AIPS_GCC2
#endif

#if defined(AIPS_GCC295)
#undef AIPS_GCC295
#endif
#if (defined(AIPS_GCC2) && __GNUC_MINOR__ == 95)
#define AIPS_GCC295
#endif

// Coming soon as gnu fixes the bugs
#if defined(AIPS_GCC3)
#undef AIPS_GCC3
#endif
#if defined(AIPS_GCC) && __GNUC__ == 3
#define AIPS_GCC3
#endif

// Alternate project compiler 
// Note we only support 64bit builds on SGI
#if defined(AIPS_SGI)
#undef AIPS_SGI
#endif
#if defined(__sgi)
#define AIPS_SGI
#if defined(_MIPS_SZPTR) && (_MIPS_SZPTR == 64)
#define AIPS_64B
#define SGI64
#endif
#endif

// Alternate project compiler 
#if defined(AIPS_SUN_NATIVE)
#undef AIPS_SUN_NATIVE
#endif
#if defined(__SUNPRO_CC)
#define AIPS_SUN_NATIVE
#endif

#if defined(AIPS_SOLARIS)
#undef AIPS_SOLARIS
#endif
#if defined(__sun)
#define AIPS_SOLARIS
#endif

#if defined(AIPS_HP)
#undef AIPS_HP
#endif
#if defined(__hp)
#define AIPS_HP
#endif

#if defined(AIPS_ALPHA)
#undef AIPS_ALPHA
#endif
#if defined(__alpha)
#define AIPS_ALPHA
#define AIPS_64B
#endif

#if defined(AIPS_KAI)
#undef AIPS_KAI
#endif
#if defined(__kai)
#define AIPS_KAI
#endif

#if defined(AIPS_AIX)
#undef AIPS_AIX
#endif
#if defined(_AIX)
#define AIPS_AIX
#endif

#if defined(AIPS_INTELCC)
#undef AIPS_INTELCC
#endif
#if defined(__INTEL_COMPILER)
#define AIPS_INTELCC
#endif

#if defined(__ia64)
#define AIPS_64B
#endif

#if defined(AIPS_I386)
#undef AIPS_I386
#endif
#if defined(i386)
#define AIPS_I386
#endif

#if defined(AIPS_DARWIN)
#undef AIPS_DARWIN
#if defined(__APPLE_CC__)
#define AIPS_DARWIN
//  No need for largefile definition as it is the default under DARWIN
#define AIPS_NOLARGEFILE
#endif
#endif

//  Automatically configure for known LITTLE ENDIAN systems
#if !(defined(AIPS_LITTLE_ENDIAN))
#if (defined(AIPS_ALPHA) || defined(AIPS_I386))
#define AIPS_LITTLE_ENDIAN
#endif
#endif

#endif
