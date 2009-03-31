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

//# Includes
#include <casa/OS/DynLib.h>
#include <casa/Exceptions/Error.h>
#ifdef HAVE_DLOPEN
#include <dlfcn.h>
#endif

using namespace std;

namespace casa { //# NAMESPACE CASA - BEGIN

  DynLib::DynLib (const std::string& library,
                  const std::string& funcName,
                  Bool closeOnDestruction)
    : itsHandle  (0),
      itsDoClose (closeOnDestruction)
  {
    string pref("lib");
    string ext;
    for (int i=0; i<4; ++i) {
      ext = (i%2==0 ? ".so" : ".dylib");
      if (i == 2) pref = "libcasa_";
      open (pref + library + ext);
      if (itsHandle) {
        break;
      }
    }
    if (itsHandle  &&  !funcName.empty()) {
      // Found the dynamic library.
      // Now find and execute the given function.
      void* initfunc = getFunc (funcName.c_str());
      if (!initfunc) {
        close();
        throw AipsError("Found dynamic library " + pref + library + ext +
                        ", but not its " + funcName + " function");
      }
      // Execute the register function.
      reinterpret_cast<void(*)()>(initfunc)();
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
    if (itsHandle) {
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

} //# NAMESPACE CASA - END
