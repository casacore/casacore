//# CasaErrorTools.cc: Tools for error printing
//# Copyright (C) 2012
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
//#
//#      Author: jjacobs

#include <casacore/casa/Exceptions/CasaErrorTools.h>
#include <map>


#if !defined(USE_STACKTRACE)

namespace casacore {
  // Stub out the related functions if functionality not enabled.
  void CasaErrorTools::generateSharedObjectMap()
  {}

  String CasaErrorTools::generateStackTrace()
  {
    return String();
  }

  String CasaErrorTools::replaceStackAddresses (const String & stackTrace)
  {
    return "-->CasaErrorTools:: FYI: Could not replace stack addresses\n\n" +
      stackTrace;
  }

} // end namespace casacore


#else

#include <link.h>
#include <stdlib.h>
#include <stdio.h>

namespace {

  std::map<casacore::String, casacore::uInt64> sharedObjectMap;

  extern "C"
  int callback (struct dl_phdr_info *info, size_t, void *)
  {
    sharedObjectMap[info->dlpi_name] = info->dlpi_addr;
    return 0;
  }

} // end namespace UNNAMED


namespace casacore {

  void CasaErrorTools::generateSharedObjectMap ()
  {
    sharedObjectMap.clear();
    dl_iterate_phdr(callback, NULL);
  }

  String CasaErrorTools::generateStackTrace()
  {
    // Get the most recent 512 levels of the stack trace.
    // This ought to handle all cases but very deep recursion.
    void* stack[512]; // Allow for up to 512 levels
    int nLevels = backtrace (stack, 512);

    // Convert the internal stack representation into one string
    // per level.
    char ** trace = backtrace_symbols (stack, nLevels);

    // Put a header on the message and then append all of the
    // strings onto the message.
    String stackTrace;
    stackTrace += "\n||> Stack trace (use c++filt to demangle):\n";
    for (int i = 0; i < nLevels; i++){
      stackTrace += trace[i] + String ("\n");
    }

    // Free up the array returned by backtrace_symbols.  The
    // strings it points to must not be deleted by this method.
    free (trace);
    return stackTrace;
  }

  String CasaErrorTools::replaceStackAddresses (const String & stackTrace)
  {

    if (sharedObjectMap.empty()){
      generateSharedObjectMap ();
    }

    String cleanedStackTrace;

    // Break the stack trace into lines by splitting at the '\n'

    string lines [500];
    int nSplits = split (stackTrace, lines, 500, "\n");

    for (int i = 0; i < nSplits; i++){

      string & line = lines [i];
      String cleanedLine;

      try {

        // Find the shared object name.

        if (line [0] != '/'){
          throw True;
        }

        size_t openParenthesis = line.find ("(");
        if (openParenthesis == String::npos){
          throw True;
        }

        String objectName = line.substr (0, openParenthesis);

        // Find the stack address.
        // This will be a hex number enclosed in square brackets
        // relative to the start of the shared object.

        size_t leftSquare = line.find ("[");
        size_t rightSquare = line.find ("]");

        if (leftSquare == String::npos || rightSquare == String::npos ||
            rightSquare <= leftSquare){
          throw True;
        }

        // Extract the address and subtract the base of the shared object's
        // starting address to produce the offset within the shared object.

        String addressText = line.substr (leftSquare + 1,
                                          rightSquare - leftSquare - 1);

        uInt64 address = strtoll (addressText.c_str(), NULL, 16);

        uInt64 objectBase = sharedObjectMap [objectName];

        uInt64 offset = address - objectBase;

        // Now rebuild the line replacing the original address ([0xHHHH...])
        // with the offset (format [+0xHHHH...]

        char offsetInHex [128];

        snprintf (offsetInHex, 127, "0x%llx", offset);

        cleanedLine = line.substr (0, leftSquare) + "[+" + offsetInHex + "]";
      }
      catch (Bool){

        // The line was not parseable so just copy it to the result.

        cleanedLine = line;
      }

      // Put the cleaned line into the result.

      cleanedStackTrace += "\n" + cleanedLine;

    }

    return cleanedStackTrace;

  }

} // end namespace casacore

#endif
