//# MemoryTrace.cc: Simple memory usage tracing mechanism
//# Copyright (C) 2015
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
//# $Id: Block.h 21120 2011-09-01 13:51:56Z gervandiepen $

//# Comment out for time being because __malloc_hook is deprecated.
//# Do so by #ifdef on AIPS_LINUX_DEPR instead of AIPS_LINUX.

#include <casacore/casa/OS/MemoryTrace.h>
#include <casacore/casa/OS/EnvVar.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Exceptions/Error.h>
#ifdef AIPS_LINUX_DEPR
# include <malloc.h>
#endif

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  // Initialize statics.
  Bool MemoryTrace::theirDoTrace = False;
  std::ofstream MemoryTrace::theirFile;
  Timer MemoryTrace::theirTimer;
  void* (*MemoryTrace::theirOldMallocHook)(size_t, const void*) = 0;
  void (*MemoryTrace::theirOldFreeHook)(void*, const void*) = 0;

  void MemoryTrace::open()
  {
    if (! theirFile.is_open()) {
      String name (EnvironmentVariable::get ("CASACORE_MEMORYTRACE"));
      if (name.empty()) {
        name = "casacore_memorytrace.log";
      }
      theirFile.open (name.c_str());
      if (!theirFile) {
        throw AipsError ("Could not create memorytrace file " + name);
      }
    }
  }

  void MemoryTrace::start()
  {
    if (! theirFile.is_open()) {
      open();
    }
    if (!theirDoTrace) {
#ifdef AIPS_LINUX_DEPR
      theirOldMallocHook = __malloc_hook;
      theirOldFreeHook   = __free_hook;
      __malloc_hook      = &mallocHook;
      __free_hook        = &freeHook;
#endif
      theirDoTrace = True;
    }
  }

  void MemoryTrace::stop()
  {
    if (theirDoTrace) {
#ifdef AIPS_LINUX_DEPR
      __malloc_hook = theirOldMallocHook;
      __free_hook   = theirOldFreeHook;
#endif
      theirDoTrace = False;
    }
  }

  std::ofstream& MemoryTrace::writeAlloc (const void* ptr, size_t size)
  {
    theirFile << Int64(1000 * theirTimer.real()) << " a b-" << ptr
              << ' ' << size << ' ';
    return theirFile;
  }

  std::ofstream& MemoryTrace::writeFree (const void* ptr)
  {
    theirFile << Int64(1000 * theirTimer.real()) << " f b-" << ptr
              << ' ';
    return theirFile;
  }

  void MemoryTrace::close()
  {
    stop();
    if (theirFile.is_open()) {
      theirFile.close();
    }
  }

#ifdef AIPS_LINUX_DEPR
  void* MemoryTrace::mallocHook (size_t size, const void* caller)
  {
    if (size == 0) {
      return NULL;
    }
    // Restore the old hooks.
    __malloc_hook = theirOldMallocHook;
    __free_hook   = theirOldFreeHook;
    // Call recursively.
    void* ptr = malloc(size);
    // Save hooks (to be safe).
    theirOldMallocHook = __malloc_hook;
    theirOldFreeHook   = __free_hook;
    // iostream might call malloc/free; it is protected too.
    theirFile << Int64(1000 * theirTimer.real()) << " a " << ptr << ' '
              << size << ' ' << caller << std::endl;
    // Restore our own hooks.
    __malloc_hook = &mallocHook;
    __free_hook   = &freeHook;
    return ptr;
  }
#else
  void* MemoryTrace::mallocHook (size_t, const void*)
  {
    return NULL;
  }
#endif

#ifdef AIPS_LINUX_DEPR
  void MemoryTrace::freeHook (void* ptr, const void* caller)
  {
    if (ptr != NULL) {
      // Restore the old hooks.
      __malloc_hook = theirOldMallocHook;
      __free_hook   = theirOldFreeHook;
      // Call recursively.
      free (ptr);
      // Save hooks (to be safe).
      theirOldMallocHook = __malloc_hook;
      theirOldFreeHook   = __free_hook;
      // iostream might call malloc/free; it is protected too.
      theirFile << Int64(1000 * theirTimer.real()) << " f " << ptr << ' '
                << caller << std::endl;
      // Restore our own hooks.
      __malloc_hook = &mallocHook;
      __free_hook   = &freeHook;
    }
  }
#else
  void MemoryTrace::freeHook (void*, const void*)
  {}
#endif

  void MemoryTrace::writeBlock (const char* msg, const std::string& name)
  {
    if (isOpen()) {
#ifdef AIPS_LINUX_DEPR
      if (theirDoTrace) {
        // Restore the old hooks.
        __malloc_hook = theirOldMallocHook;
        __free_hook   = theirOldFreeHook;
      }
#endif
      theirFile << Int64(1000 * theirTimer.real()) << msg
                << name << std::endl;
#ifdef AIPS_LINUX_DEPR
      if (theirDoTrace) {
        // Restore our own hooks.
        __malloc_hook = &mallocHook;
        __free_hook   = &freeHook;
      }
#endif
    }
  }

  void MemoryTrace::writeBlock (const char* msg, const char* name)
  {
    if (isOpen()) {
#ifdef AIPS_LINUX_DEPR
      if (theirDoTrace) {
        // Restore the old hooks.
        __malloc_hook = theirOldMallocHook;
        __free_hook   = theirOldFreeHook;
      }
#endif
      theirFile << Int64(1000 * theirTimer.real()) << msg
                << name << std::endl;
#ifdef AIPS_LINUX_DEPR
      if (theirDoTrace) {
        // Restore our own hooks.
        __malloc_hook = &mallocHook;
        __free_hook   = &freeHook;
      }
#endif
    }
  }

  std::string MemoryTrace::makeString (const char* name)
  {
#ifdef AIPS_LINUX_DEPR
    if (theirDoTrace) {
      // Restore the old hooks to avoid trace messages when making the string.
      __malloc_hook = theirOldMallocHook;
      __free_hook   = theirOldFreeHook;
    }
#endif
    std::string str(name);
#ifdef AIPS_LINUX_DEPR
    if (theirDoTrace) {
      // Restore our own hooks.
      __malloc_hook = &mallocHook;
      __free_hook   = &freeHook;
    }
#endif
    return str;
  }




  MemoryTraceBlock::MemoryTraceBlock (const std::string& name)
    : itsName (name)
  {
    traceMemoryBlockBegin (itsName);
  }

  MemoryTraceBlock::MemoryTraceBlock (const char* name)
    : itsName (MemoryTrace::makeString(name))
  {
    traceMemoryBlockBegin (itsName);
  }

  MemoryTraceBlock::~MemoryTraceBlock()
  {
    traceMemoryBlockEnd (itsName);
  }


} //# NAMESPACE CASACORE - END



// Note: another way to do memory tracing is by preloading a shared library
// containing the overloaded malloc and free. This could be done in a
// casacore package memorytrace or so.
// The disadvantage of it is that it is less selective; furthermore it always
// has to find the symbol which can be a bit expensive.
// See the following code.

/*
//# See http://www.stev.org/post/2012/11/03/HowTo-Override-malloc-free-in-c.aspx

#include <stdio.h>
#include <dlfcn.h>
#include <stdlib.h>
 
void* malloc(size_t size) throw()
{
  if (size == 0) {
    return NULL;
  }
  // For the compiler data pointers and function pointers are different, which
  // can be the case for special hardware. Not here, so make them equal.
  typedef void* (*func_ptr)(size_t);
  typedef union {
    func_ptr funcPtr;
    void* ptr;
  } ptrCastUnion;
  ptrCastUnion ptrCast;

  void* handle = RTLD_NEXT;
  ptrCast.ptr = dlsym(handle, "malloc");
  if (ptrCast.ptr == NULL) {
    printf("libcasa_malloclog cannot find malloc\n");
    exit(1);
  }
  void* addr = (ptrCast.funcPtr)(size);
  printf("Alloc = %p Size: %ld\n", addr, size);
  return addr;
}

void free(void* addr) throw()
{
  if (addr == NULL) {
    return;
  }
  typedef void (*func_ptr)(void*);
  typedef union {
    func_ptr funcPtr;
    void* ptr;
  } ptrCastUnion;
  ptrCastUnion ptrCast;

  printf("free %p\n", addr);
  void* handle = RTLD_NEXT;
  ptrCast.ptr = dlsym(handle, "free");
  if (ptrCast.ptr == NULL) {
    exit(1);
  }
  (ptrCast.funcPtr)(addr);
}
*/

/*
See http://stackoverflow.com/questions/17803456/an-alternative-for-the-deprecated-malloc-hook-functionality-of-glibc
See also http://elinux.org/images/b/b5/Elc2013_Kobayashi.pdf

My complete hooking function now looks like this:

extern void *__libc_malloc(size_t size);

int malloc_hook_active = 0;

void*
malloc (size_t size)
{
  void *caller = __builtin_return_address(0);
  if (malloc_hook_active)
    return my_malloc_hook(size, caller);
  return __libc_malloc(size);
}

where my_malloc_hook looks like this:

void*
my_malloc_hook (size_t size, void *caller)
{
  void *result;

  // deactivate hooks for logging
  malloc_hook_active = 0;

  result = malloc(size);

  // do logging
  [ ... ]

  // reactivate hooks
  malloc_hook_active = 1;

  return result;
}

Of course, the hooks for calloc, realloc and free work similarly.
*/
