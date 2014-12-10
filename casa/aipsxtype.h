//# aipsxtype.h: Global initialization for special Casacore types
//# Copyright (C) 2000,2001,2002,2004
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

#ifndef CASA_AIPSXTYPE_H
#define CASA_AIPSXTYPE_H

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// Define the extra non-standard types used by Casacore
// (like proposed uSize, Size)

// A guaranteed 64-bit long integer (for a.o. large file systems).
// An implementation must support the + and - operators.
typedef long long Int64;
typedef unsigned long long uInt64;

//# All FITS code seems to assume longs are 4 bytes. Take care of machines 
//# for which this isn't true here by defining FitsLong to be the 4 byte int.
//# Use FitsLong instead of long in the FITS code where it matters.
#if (defined(AIPS_ALPHA) || defined(AIPS_SGI) || defined(__x86_64__))
    typedef int FitsLong;
#else
    typedef long FitsLong;
#endif 

} //# NAMESPACE CASACORE - END

#endif
