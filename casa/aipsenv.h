//# aipsenv.h: Global initialization for special Casacore macros
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

#ifndef CASA_AIPSENV_H
#define CASA_AIPSENV_H

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// Set if compiler supports C++11.
#if __cplusplus == 201103L
#define AIPS_CXX11
#endif

// Set if GCC compiler is used.
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

#if defined(AIPS_GCC3)
#undef AIPS_GCC3
#endif
#if (defined(AIPS_GCC) && __GNUC__ == 3)
#define AIPS_GCC3
#endif

#if defined(AIPS_GCC4)
#undef AIPS_GCC4
#endif
#if (defined(AIPS_GCC) && __GNUC__ == 4)
#define AIPS_GCC4
#endif

// Alternate project compiler 
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

#if defined(AIPS_BSD)
#undef AIPS_BSD
#endif
#if defined(__FreeBSD__)
#define AIPS_BSD
#define AIPS_NOLARGEFILE
#endif

#if defined(AIPS_LINUX)
#undef AIPS_LINUX
#endif
#if defined(__linux)
#define AIPS_LINUX
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

#if defined(AIPS_CRAY_PGI)
#undef AIPS_CRAY_PGI
#endif
#if defined(__QK_USER__)
#define AIPS_CRAY_PGI
#if !defined(AIPS_NOLARGEFILE)
#define AIPS_NOLARGEFILE
#endif
#if !defined(AIPS_NO_LEA_MALLOC)
#define AIPS_NO_LEA_MALLOC
#endif
#endif

#if defined(AIPS_CRAY_CATAMOUNT)
#undef AIPS_CRAY_CATAMOUNT
#endif
#if defined(__LIB_CATAMOUNT__)
#define AIPS_CRAY_CATAMOUNT
#endif

#if (defined(__ia64) || defined(__x86_64__))
# if !defined(AIPS_64B)
# define AIPS_64B
# endif
#endif

#if defined(AIPS_I386)
#undef AIPS_I386
#endif
#if defined(i386)
#define AIPS_I386
#endif

#if defined(AIPS_DARWIN)
#undef AIPS_DARWIN
#endif
#if defined(__APPLE__)
#define AIPS_DARWIN
//  No need for largefile definition as it is the default under DARWIN
#define AIPS_NOLARGEFILE
  // Don't use AIPS_LITTLE_ENDIAN as this would prevent universal builds
  // from working. Auto-detect from AIPS_I386 for intel Macs
#define AIPS_NO_LEA_MALLOC 
# if defined(AIPS_LITTLE_ENDIAN)
# undef AIPS_LITTLE_ENDIAN
# endif
#endif

//  Automatically configure for known LITTLE ENDIAN systems
#if !(defined(AIPS_LITTLE_ENDIAN))
#if (defined(AIPS_ALPHA) || defined(AIPS_I386) || defined(__x86_64__))
#define AIPS_LITTLE_ENDIAN
#endif
#endif


//  If needed, define the LFS variables (needed in code using cfitsio).
#ifndef AIPS_NOLARGEFILE
# undef _FILE_OFFSET_BITS
# define _FILE_OFFSET_BITS 64
# ifndef _LARGEFILE_SOURCE
#  define _LARGEFILE_SOURCE
# endif
# ifndef _LARGEFILE64_SOURCE
#  define _LARGEFILE64_SOURCE
# endif
#endif


} //# NAMESPACE CASACORE - END
#endif
