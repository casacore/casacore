//# Error2.cc: Base class for all Casacore errors (non-templated classes)
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

#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Exceptions/CasaErrorTools.h>
#include <casacore/casa/stdlib.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/string.h>      //# needed for strerror_r

//# Stacktracing requires some extra includes.
#ifdef USE_STACKTRACE
# include <casacore/casa/System/AipsrcValue.h>
# include <execinfo.h>
# define AddStackTrace() addStackTrace()
#else
# define AddStackTrace()
#endif

namespace casacore { //# NAMESPACE CASACORE - BEGIN

AipsError::AipsError (Category c)
: message(), category(c)
{
    AddStackTrace();
}


AipsError::AipsError(const Char *str,Category c)
  : message(str), category(c)
{
    AddStackTrace();
}

AipsError::AipsError(const String &str,Category c)
  : message(str), category(c)
{
    AddStackTrace();
}

AipsError::AipsError (const String &msg, const String& filename,
                      uInt lineNumber, Category c)
  : category(c)
{
  ostringstream os;
  os << msg << " at File: " << filename << ", line: " << lineNumber;
  message = os.str();
  AddStackTrace ();
}

AipsError::~AipsError() throw()
{}


#ifndef USE_STACKTRACE
  String AipsError::getStackTrace () const
    { return stackTrace; }
  void AipsError::addStackTrace()
    {}
  void AipsError::getLastInfo (String&, String&)
    {}
  String AipsError::getLastMessage ()
    { return String(); }
  String AipsError::getLastStackTrace ()
    { return String(); }
  void AipsError::clearLastInfo ()
    {}

#else

  // error message from last exception
  static String lastMessage = "*none*";
  // stack trace from last exception
  static String lastStackTrace = "*no-stack-trace*";
  // protects the lastMessage and lastStackTrace statics
  static Mutex  lastErrorMutex;

  void AipsError::getLastInfo (String & message, String & stackTrace)
  {
    ScopedMutexLock lock(lastErrorMutex);
    message = getLastMessage();
    stackTrace = getLastStackTrace();
  }
  String AipsError::getLastMessage ()
    { return lastMessage; }
  String AipsError::getLastStackTrace ();
    { return CasaErrorTools::replaceStackAddresses (lastStackTrace); }
  void AipsError::clearLastInfo ()
  {
    ScopedMutexLock lock(lastErrorMutex);
    lastMessage = "*none*";
    lastStackTrace = "*no-stack-trace*";
  }

  void AipsError::addStackTrace()
  {
    // Always generate a stack trace and keep it around in a static
    // for later retrieval via casapy
    stackTrace = CasaErrorTools::generateStackTrace();
    {
      ScopedMutexLock lock(lastErrorMutex);
      lastMessage = message;
      lastStackTrace = stackTrace;
    }
    // See if the default is to tack on the stack trace on the exception
    // message.  N.B.: Turning this on will break some of the low-level tests
    // which simply compare expected to actual output.
    Bool enabled;
    AipsrcValue<Bool>::find (enabled, "AipsError.enableStackTrace", False);
    if (enabled) {
      // If permitted, append to the error message.
      message += stackTrace;
    }
  }

  String AipsError::getStackTrace () const
  {
    return CasaErrorTools::replaceStackAddresses (stackTrace);
  }

#endif


void AipsError::throwIf (bool condition, const String& message,
                         const char* file, Int line, const char* func)
{
  // If the condition is met then throw an AipsError
  if (condition) {
    String m = String::format ("Exception: %s.\n... thrown by %s",
                               message.c_str(), func);
    AipsError e (m.c_str(), file, line);
    throw e;
  }
}

void AipsError::throwIfError (int errorCode, const String& prefix,
                              const char* file, Int line, const char* func)
{
  // If the provided error code is not equal to success (0) then
  // throw an AipsError using the provided prefix and then details
  // of the error.
  if (errorCode != 0) {
    AipsError e (String::format ("Exception: %s.\n...Thrown by %s\n...(errno=%d): %s",
                                 prefix.c_str(), func,
                                 errorCode, strerror (errorCode)),
                 file, line);
    throw e;
  }
}

AipsError AipsError::repackageAipsError (AipsError& error,
                                         const String& message,
                                         const char* file,
                                         Int line, const char* func)
{
  ostringstream os;
  AipsError tmp (message, file, line);
  os << "+++Exception: " << tmp.getMesg() << ".\n...Thrown by " << func << ": "
     << "\n...Lower level exception: " << error.getMesg()
     << "\n--- end exception\n";
  return AipsError (os.str());
}



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
SystemCallError::SystemCallError (int error, const String &msg,
                                  const String &filename,
                                  uInt lineNumber, Category c)
  : AipsError (msg + String::format (": errno=%d: %s", error,
				     errorMessage (error).c_str()),
               filename, lineNumber, c),
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

} //# NAMESPACE CASACORE - END

