//# PycArrayNA.h: Class to convert an Array to/from a Python numarray array
//# Copyright (C) 2006
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
//# $Id: PycArrayNA.h,v 1.1 2006/11/06 00:14:44 gvandiep Exp $


#ifndef PYRAP_PYCARRAYNA_H
#define PYRAP_PYCARRAYNA_H

// include first to avoid _POSIX_C_SOURCE redefined warnings
#include <boost/python.hpp>
#include <boost/python/object.hpp>
#include <casa/Containers/ValueHolder.h>
#include <casa/Arrays/Array.h>

namespace casa { namespace pyrap { namespace numarray {

//# Define the common functions if numarray is used.
#if defined(AIPS_USENUMARRAY)
#define PYC_USE_PYARRAY "numarray"
#endif
#include <pyrap/Converters/PycArrayComH.h>
#undef PYC_USE_PYARRAY

}}}

#endif
