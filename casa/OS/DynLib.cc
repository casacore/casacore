//# DynLib.cc: Class to handle loading of dynamic libraries
//# Copyright (C) 2009
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

//# For the time being assume that all systems have dlopen.
#ifndef HAVE_DLOPEN
# define HAVE_DLOPEN
#endif

//# Includes
#include <casacore/casa/OS/DynLib.h>
#include <casacore/casa/OS/EnvVar.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/IO/ArrayIO.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/Exceptions/Error.h>
#ifdef HAVE_DLOPEN
#include <dlfcn.h>
#endif

using namespace std;

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  DynLib::DynLib (const std::string& library,
                  const std::string& prefix,
                  const std::string& funcName,
                  bool closeOnDestruction)
    : itsHandle  (0),
      itsDoClose (closeOnDestruction)
  {
    attach (library, prefix, std::string(), funcName);
  }

  DynLib::DynLib (const std::string& library,
                  const std::string& prefix,
                  const std::string& version,
                  const std::string& funcName,
                  bool closeOnDestruction)
    : itsHandle  (0),
      itsDoClose (closeOnDestruction)
  {
    // Add a dot to the version if needed.
    std::string vers(version);
    if (! vers.empty()  &&  vers[0] != '.') {
      vers = '.' + vers;
    }
    attach (library, prefix, vers, funcName);
  }

  DynLib::DynLib (const std::string& library,
                  Bool closeOnDestruction,
                  const std::string& prefix,
                  const std::string& suffix)
    : itsHandle  (0),
      itsDoClose (closeOnDestruction)
  {
    open (prefix + library + suffix);
  }

  DynLib::~DynLib()
  {
    if (itsDoClose) {
      close();
    }
  }

  void* DynLib::getFunc (const std::string& funcName)
  {
    itsError.clear();
#ifdef HAVE_DLOPEN
    if (itsHandle ) {
      void* fptr = dlsym (itsHandle, funcName.c_str());
      if (fptr == 0) {
        itsError = dlerror();
      }
      return fptr;
    }
#endif
    return 0;
  }

  void DynLib::open (const std::string& name)
  {
#ifdef HAVE_DLOPEN
    itsHandle = dlopen (name.c_str(), RTLD_NOW | RTLD_GLOBAL);
    if (itsHandle == 0) {
      itsError += string(dlerror()) + '\n';
    }
#endif
  }

  void DynLib::close()
  {
    if (itsHandle) {
#ifdef HAVE_DLOPEN
      dlclose (itsHandle);
#endif
      itsHandle= 0;
    }
  }

  std::string DynLib::tryOpen (const std::string& library,
                               const std::string& dir,
                               const std::string& prefix,
                               const std::string& version)
  {
    std::string pref(prefix);
    std::string vers(version);
    std::string fullName;
    // Try a maximum 4 times (1 or 2 prefix, 1 or 2 version, 1 ext).
    int i=0;
    while (i<4  &&  itsHandle==0) {
#ifdef __APPLE__
      fullName = dir + pref + library + vers + ".dylib";
#else
      fullName = dir + pref + library + ".so" + vers;
#endif
      open (fullName);
      i++;
      if (i == 2) {
        if (pref == "lib") i += 2;     // no specific prefix given
        pref = "lib";
      }
      if (i%2 == 1) {
        vers = std::string();
        if (version.empty()) i++;      // no version given
      } else {
        vers = version; 
      }
    }
    return (itsHandle==0  ?  std::string() : fullName);
  }

  void DynLib::attach (const std::string& library,
                       const std::string& prefix,
                       const std::string& version,
                       const std::string& funcName)
  {
    std::string fullName = tryCasacorePath (library, prefix, version);
    if (fullName.empty()) {
      fullName = tryOpen (library, string(), prefix, version);
    }
    if (itsHandle == 0) {
      throw AipsError ("Shared library " + library +
                       " not found in CASACORE_LDPATH or (DY)LD_LIBRARY_PATH\n"
                       + itsError);
    }
    LogIO os(LogOrigin("DynLib"));
    os << LogIO::NORMAL3
       << "Loaded shared library " << fullName
       << LogIO::POST;
    if (itsHandle  &&  !funcName.empty()) {
      // Found the dynamic library.
      // Now find and execute the given function.
      // Because a compiler like g++ gives a warning when casting a pointer
      // to a function pointer, a union is used to achieve this.
      // Ensure the pointer sizes are the same.
      typedef void (*func_ptr)();
      AlwaysAssert (sizeof(func_ptr) == sizeof(void*), AipsError);
      typedef union {
	func_ptr funcPtr;
	void* ptr;
      } ptrCastUnion;
      ptrCastUnion ptrCast;
      ptrCast.ptr = getFunc (funcName.c_str());
      if (! ptrCast.ptr) {
        close();
        throw AipsError("Found dynamic library " + fullName +
                        ", but not its " + funcName + " function\n  " +
                        itsError);
      }
/// Note: the following is a g++ specific way to avoid the warning.
///#ifdef __GNUC__
///__extension__
///#endif
      // Execute the function.
      ptrCast.funcPtr();
      os << LogIO::NORMAL3
         << "Executed " << funcName << " in shared library " << fullName
         << LogIO::POST;
    }
  }

  std::string DynLib::tryCasacorePath (const std::string& library,
                                       const std::string& prefix,
                                       const std::string& version)
  {
    // Check if CASACORE_LDPATH is defined.
    String casapath("CASACORE_LDPATH");
    String path = EnvironmentVariable::get(casapath);
    if (! path.empty()) {
      // Split using : as delimiter.
      Vector<string> parts = strToVector (path, ':');
      for (uInt j=0; j<parts.size(); ++j) {
        if (! parts[j].empty()) {
          string libDir = parts[j] + '/';
          // Check if shared library can be found there.
          std::string fullName = tryOpen (library, libDir, prefix, version);
          if (itsHandle) {
            return fullName;
          }
        }
      }
    }
    return std::string();
  }

} //# NAMESPACE CASACORE - END
