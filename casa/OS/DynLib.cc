//# DynLib.cc: Class to handle loadig of dynamic libraries
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
#include <casacore/casa/Utilities/Assert.h>
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
    std::string fullName = tryCasacorePath (library, prefix);
    if (fullName.empty()) {
      string pref("lib");
      string ext;
      for (int i=0; i<4; ++i) {
        ext = (i%2==0 ? ".so" : ".dylib");
        if (i == 2) pref = prefix;
        fullName = pref + library + ext;
        open (fullName);
        if (itsHandle) {
          break;
        }
      }
    }
    if (itsHandle  &&  !funcName.empty()) {
      // Found the dynamic library.
      // Now find and execute the given function.
      // Because a compiler like g++ gives a warning when casting a pointer to a
      // function pointer, a union is used to achieve this.
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
                        ", but not its " + funcName + " function");
      }
/// Note: the following is a g++ specific way to avoid the warning.
///#ifdef __GNUC__
///__extension__
///#endif
      ptrCast.funcPtr();
    }
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
#ifdef HAVE_DLOPEN
    if (itsHandle ) {
      return dlsym (itsHandle, funcName.c_str());
    }
#endif
    return 0;
  }

  void DynLib::open (const std::string& name)
  {
#ifdef HAVE_DLOPEN
    itsHandle = dlopen (name.c_str(), RTLD_NOW | RTLD_GLOBAL);
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

  std::string DynLib::tryCasacorePath (const std::string& library,
                                       const std::string& prefix)
  {
    // Check if CASACORE_LDPATH is defined.
    String casapath("CASACORE_LDPATH");
    String path = EnvironmentVariable::get(casapath);
    if (! path.empty()) {
      // Split using : as delimiter.
      Vector<String> parts = stringToVector (path, ':');
      for (uInt j=0; j<parts.size(); ++j) {
        if (! parts[j].empty()) {
          string libDir = parts[j];
          // Check if shared library can be found there.
          std::string pref("lib");
          std::string ext;
          for (int i=0; i<4; ++i) {
            ext = (i%2==0 ? ".so" : ".dylib");
            if (i == 2) pref = prefix;
            std::string fullName(libDir + pref + library + ext);
            open (fullName);
            if (itsHandle) {
              return fullName;
            }
          }
        }
      }
    }
    return std::string();
  }

} //# NAMESPACE CASACORE - END
