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
#include <casa/string.h>      //# needed for strerror

namespace casa { //# NAMESPACE CASA - BEGIN

AipsError::AipsError(const Char *str,Category c)
  : message(str), category(c)
{}

AipsError::AipsError(const String &str,Category c)
  : message(str), category(c)
{}

AipsError::AipsError (const String &msg, const String& filename,
                      uInt lineNumber, Category c)
  : category(c)
{
  ostringstream os;
  os << msg << " at File: " << filename << ", line: " << lineNumber;
  message = os.str();
}

AipsError::~AipsError() throw()
{}


AllocError::~AllocError() throw()
{}


IndexError::~IndexError() throw()
{}


DuplError::~DuplError() throw()
{}


SystemCallError::SystemCallError(const String& funcName, int error, Category c)
  : AipsError("Error in " + funcName + ": " + errorMessage(error), c),
    itsError (error)
{}
SystemCallError::~SystemCallError() throw()
{}
String SystemCallError::errorMessage(int error)
{
  // Use strerror_r for thread-safety.
  char buffer[128];
  // There are two incompatible versions of versions of strerror_r()
#if !__linux__ || (!_GNU_SOURCE && (_POSIX_C_SOURCE >= 200112L || _XOPEN_SOURCE >= 600))
  if (strerror_r(error, buffer, sizeof buffer) == 0) {
    return String(buffer);
  }
  return "errno " + String::toString(error);
#else
  return strerror_r(error, buffer, sizeof buffer);
#endif
}


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

