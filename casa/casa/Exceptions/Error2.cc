//# Error2.cc: Base class for all AIPS++ errors (non-templated classes)
//# Copyright (C) 1993,1994,1995,1996,1997,2000,2001
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

#include <casa/Exceptions/Error.h>
#include <casa/stdlib.h>
#include <casa/iostream.h>


namespace casa { //# NAMESPACE CASA - BEGIN

AipsError::AipsError(const Char *str,Category c)
  : message(str), category(c)
{}

AipsError::AipsError(const String &str,Category c)
  : message(str), category(c)
{}

AipsError::~AipsError() throw()
{}


AllocError::~AllocError() throw()
{}


IndexError::~IndexError() throw()
{}


DuplError::~DuplError() throw()
{}


// Exception which causes an abort instead of continuing
AbortError::AbortError(const Char *str,Category c)
: AipsError(str,c)
{
    cerr << "An unrecoverable error occurred: " << endl;
    cerr << str << endl;
#ifndef CASACORE_NOEXIT
    exit(1);
#endif
}

AbortError::AbortError(const String &str,Category c)
: AipsError(str,c)
{
    cerr << "An unrecoverable error occurred: " << endl;
    cerr << str << endl;
#ifndef CASACORE_NOEXIT
    exit(1);
#endif
}

AbortError::~AbortError() throw()
{}

} //# NAMESPACE CASA - END

