//# DynLib.h: Class to handle loadig of dynamic libraries
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

#ifndef CASA_DYNLIB_H
#define CASA_DYNLIB_H

//# Includes
#include <casa/aips.h>
#include <string>

namespace casa { //# NAMESPACE CASA - BEGIN

  // <summary> 
  // Class to handle loadig of dynamic libraries
  // </summary>
  // <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
  // </reviewed>

  // <use visibility=export>

  // <prerequisite> 
  //    <li> Basic knowledge of the dlopen function family
  // </prerequisite>

  // <synopsis> 
  // This class makes it possible to load a dynamic library and execute an
  // initialization function. Furthermore one can get a pointer to any function
  // in the dynamic library and close the library.
  //
  // It is a wrapper around functions dlopen, dlsym, and dlclose.
  //
  // If dlopen and so is not supported on a platform, the class acts as if
  // the shared library could not be found.
  // </synopsis>

  // <example>
  // <srcblock>
  //    DynLib dl("bitflagsengine", "register_bitflagsengine");
  //    AlwaysAssert (dl.getHandle());
  // </srcblock>
  //  loads the executes library <src>bitflagsengine</src> and executes
  //  the given register initialization function.
  // </example>

  // <motivation> 
  // dlopen is a standard UNIX system call, but some operating systems
  // do not support it or have different function names (notably Windows).
  // In this way use of dynamic libraries is centralized and can easily b
  // tailored as needed.
  // </motivation>

  class DynLib
  {
  public:

    // Load the dynamic library. It is tried with prefixes "lib" and <src>prefix</src>
    // and suffixes ".so" and ".dylib".
    // If not loaded successfully, the internal handle is NULL.
    // <br>If a non-empty funcName is given, that function is looked up and
    // executed. Its signature must be <src>void func()</src>. Note that the
    // function name should not be mangled, thus declared <src>extern "C"</src>.
    // An exception is thrown if the library is loaded successfully, but
    // <src>funcName</src> could not be found.
    // <br>If <src>closeOnDestruction=True</src>, the dynamic library is closed
    // on destruction of the DynLib object.
    DynLib (const std::string& library,
            const std::string& prefix=std::string(),
            const std::string& funcName=std::string(),
            bool closeOnDestruction=True);

    // Load the dynamic library with the given name, prefix, and suffix.
    // If not loaded successfully, the internal handle is NULL.
    // <br>If <src>closeOnDestruction=True</src>, the dynamic library is closed
    // when the DynLib object is destructed.
    DynLib (const std::string& library,
            Bool closeOnDestruction,
            const std::string& prefix="lib",
#ifdef __APPLE__
            const std::string& suffix=".dylib");
#else
            const std::string& suffix=".so");
#endif

    // Close the dynamic library if told so in the constructor.
    ~DynLib();

    // Get a pointer to a function in the dynamic library.
    // The pointer has to be casted with a reinterpret_cast to a function
    // pointer with the correct signature. When compiling with -pedantic the
    // compiler will give a warning for such a cast, because on some systems
    // (in particular some micro-controllers) a data pointer differs from a
    // function pointer. However, that problem cannot be solved.
    // For example:
    // <srcblock>
    //   typedef Int MyFunc(Int, Int);
    //   void* initfunc = DynLib::getFunc (mod, ("register_"+name).c_str());
    //   if (initFunc) {
    //     MyFunc* func = reinterpret_cast<MyFunc*>(initfunc);
    //     Int result = func(1,2);
    //   }
    // </srcblock>
    // casts to a function returning Int and taking two Ints.
    // <br>A null pointer is returned if the function could not be found.
    void* getFunc (const std::string& funcName);

    // Get the dynamic library handle.
    void* getHandle() const
      { return itsHandle; }

  private:
    // Open (load)the dynamic library.
    void open (const std::string& name);

    // Close (unload) the dynamic library (if opened).
    void close();

    //# Handle to dynamic library; note that the pointer is not owned, so the
    //# generated copy ctor and assignment are fine.
    void* itsHandle;
    Bool  itsDoClose;
  };

} //# NAMESPACE CASA - END

#endif
