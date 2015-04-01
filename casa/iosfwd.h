//# iosfwd.h: Interim solution for standard/nonstandard system iosfwd
//# Copyright (C) 2001,2002
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

#ifndef CASA_IOSFWD_H
#define CASA_IOSFWD_H

//# Make sure any special macros are set
#include <casacore/casa/aips.h>
#include <iosfwd>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// Define the IO system forward declarations. Note that if fully standard
// suppliant, the iosfwd will also forward declare the stringstream classes.
// If strstream classes have to be known, include <aips/aipsiosstrfwd.h>
// instead.

using std::ios;
using std::istream;
using std::ostream;
using std::iostream;
using std::streambuf;
using std::filebuf;
using std::ifstream;
using std::ofstream;
using std::fstream;
using std::fpos;
using std::streampos;

} //# NAMESPACE CASACORE - END

#endif
