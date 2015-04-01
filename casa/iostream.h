//# iostream.h: Interim solution for standard/nonstandard system iostream
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

#ifndef CASA_IOSTREAM_H
#define CASA_IOSTREAM_H

//# Define the C standard C++ include file. 
//# This is an interim solution to cater for the SGI non-existence of
//# them (e.g. <cstring>)

//# Make sure any special macros are set
#include <casacore/casa/aips.h>
#include <iostream>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

using std::istream;
using std::ostream;
using std::iostream;
using std::cin;
using std::cout;
using std::cerr;
using std::endl;
using std::flush;
using std::ws;

using std::ios;
using std::streampos;
using std::streamoff;
using std::streamsize;
using std::dec;
using std::hex;
using std::oct;

using std::internal;
using std::left;
using std::right;
using std::fixed;
using std::scientific;

using std::boolalpha;
using std::noboolalpha;
using std::showbase;
using std::noshowbase;
using std::showpoint;
using std::noshowpoint;
using std::showpos;
using std::noshowpos;
using std::skipws;
using std::noskipws;
using std::uppercase;
using std::nouppercase;

} //# NAMESPACE CASACORE - END

#endif
