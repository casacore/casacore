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

#ifndef CASA_DYNLIB_H
#define CASA_DYNLIB_H

//# Includes
#include <casacore/casa/aips.h>
#include <string>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  // <summary> 
  // Class to handle loading of dynamic libraries
  // </summary>
  // <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
  // </reviewed>

  // <use visibility=export>

  // <prerequisite> 
  //    <li> Basic knowledge of the dlopen function family
  // </prerequisite>

  // <synopsis> 
  // This class makes it possible to load a dynamic library and execute an
  // initialization function. Furthermore, one can get a pointer to any function
  // in the dynamic library and close the library.
  //
  // The search path of the shared library is as follows:
  // <ul>
  //  <li> If the environment library CASACORE_LDPATH is defined, it is tried to
  //       find the library using that path.
  //  <li> If not defined or not found, the system's (DY)LD_LIBRARY_PATH is used.
  //  <li> The library looked for has the name 'prefix'libname'suffix'.
  //       <br>As prefix first "lib" is used, thereafter the given one
  //       (e.g., "libcasa_").
  //       <br>As suffix first ".so" is used, thereafter ".dylib" (for OS-X).
  // </ul>
  //
  // It is a wrapper around functions dlopen, dlsym, and dlclose.
  // If dlopen and so are not supported on a platform, the class acts as if
  // the shared library could not be found.
  // </synopsis>

  // <example>
  // <srcblock>
  //    DynLib dl("derivedmscal", "libcasa_", "register_derivedmscal");
  //    AlwaysAssert (dl.getHandle());
  // </srcblock>
  // Using this 
  //  loads the shared library <src>libcasa_derivedmscal.so</src> and
  //  executes the given register initialization function.
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

    // Load the dynamic library. It is tried with prefixes <src>prefix</src>
    // and "lib" (in that order) and with suffix ".so" or ".dylib" (for Apple).
    // No library version number is used.
    // If not loaded successfully, an exception is thrown.
    // <br>If a non-empty funcName is given, that function is looked up and
    // executed for initialization purposes. Its signature must be
    // <src>void func()</src>.
    // Note that the function name should not be mangled, thus declared
    // <src>extern "C"</src>.
    // An exception is thrown if the library is loaded successfully, but
    // <src>funcName</src> could not be found.
    // <br>If <src>closeOnDestruction=True</src>, the dynamic library is
    // closed on destruction of the DynLib object.
    DynLib (const std::string& library,
            const std::string& prefix=std::string(),
            const std::string& funcName=std::string(),
            bool closeOnDestruction=True);

    // The same as above, but it is tried with and without the given version
    // (in that order).
    DynLib (const std::string& library,
            const std::string& prefix,
            const std::string& version,
            const std::string& funcName,
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

    // Get the possible error.
    const std::string& getError() const
      { return itsError; }

  private:
    // Try to open the library with some prefixes, suffixes and versions
    // and to execute the initialization function.
    // If successful, itsHandle is filled. Otherwise an exception is thrown.
    void attach (const std::string& name,
                 const std::string& prefix,
                 const std::string& version,
                 const std::string& funcName);

    // Try to open the library with some prefixes, suffixes and versions
    // If successful, itsHandle is filled and the full library name is
    // returned. Otherwise an empty name is returned.
    std::string tryOpen (const std::string& name,
                         const std::string& libdir,
                         const std::string& prefix,
                         const std::string& version);

    // Open (load) the dynamic library.
    void open (const std::string& name);

    // Close (unload) the dynamic library (if opened).
    void close();

    // Try if the library can be opened using CASACORE_LDPATH.
    std::string tryCasacorePath (const std::string& library,
                                 const std::string& prefix,
                                 const std::string& version);

    //# Handle to dynamic library; note that the pointer is not owned, so the
    //# generated copy ctor and assignment are fine.
    void*       itsHandle;
    Bool        itsDoClose;
    std::string itsError;
  };

} //# NAMESPACE CASACORE - END

#endif
