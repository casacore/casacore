//# LargeIOFuncDef.cc: Header to define possible large IO function names
//# Copyright (C) 2001,2002,2003
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

#ifndef CASA_LARGEIOFUNCDEF_H
#define CASA_LARGEIOFUNCDEF_H


// <summary>
// Defines for correct name of functions to access large files.
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>

// </synopsis> 
// The defines in this file let us instrument the IO system using PABLO.
// See www-pablo.cs.uiuc.edu for more about pablo.
//
// If AIPS_NOLARGEFILE is not defined, use the large file functions.
// Define _LARGEFILE64_SOURCE for Linux systems.
// <synopsis> 


#if !defined(AIPS_NOLARGEFILE)
#if defined(AIPS_LINUX)
#  if !defined(_LARGEFILE64_SOURCE)
#   define _LARGEFILE64_SOURCE
#  endif
# endif
#if defined(PABLO_IO)
#  include "IOTrace.h"
#  define traceFOPEN fopen64
#  define traceFSEEK fseeko64
#  define traceFTELL ftello64
#  define trace2OPEN open64
#  define traceLSEEK lseek64
#  define trace3OPEN open64
# else
#  define traceFOPEN fopen64
#  define traceFCLOSE fclose
#  define traceFSEEK fseeko64
#  define traceFTELL ftello64
#  define traceFREAD fread
#  define traceFWRITE fwrite
#  define traceREAD read
#  define traceWRITE write
#  define trace2OPEN open64
#  define traceLSEEK lseek64
#  define trace3OPEN open64
#  define traceCLOSE close
# endif
#else
# define traceFTELL ftell
#if defined(PABLO_IO)
#  include "IOTrace.h"
# else
#  define traceFOPEN fopen
#  define traceFCLOSE fclose
#  define traceFSEEK fseek
#  define traceFREAD fread
#  define traceFWRITE fwrite
#  define traceREAD read
#  define traceWRITE write
#  define trace2OPEN open
#  define traceLSEEK lseek
#  define trace3OPEN open
#  define traceCLOSE close
# endif
#endif

namespace casacore { //# NAMESPACE CASACORE - BEGIN


} //# NAMESPACE CASACORE - END

#endif
