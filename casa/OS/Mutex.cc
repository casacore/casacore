//# Mutex.cc: Classes to handle mutexes and (un)locking
//# Copyright (C) 2011
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

#include <casacore/casa/OS/Mutex.h>
#include <errno.h>
#include <casacore/casa/Exceptions/Error.h>

//# Define a macro to cast the void* to pthread_mutex_t*.
#define ITSMUTEX \
  (static_cast<pthread_mutex_t*>(itsMutex))

namespace casacore {

#ifdef USE_THREADS

  Mutex::Mutex (Mutex::Type type)
  {
    // Set the type of mutex.
    // Use default ERRORCHECK if in debug mode.
    int ptype = PTHREAD_MUTEX_DEFAULT;
    switch (type) {
    case Normal:
      ptype = PTHREAD_MUTEX_NORMAL;
      break;
    case ErrorCheck:
      ptype = PTHREAD_MUTEX_ERRORCHECK;
      break;
    case Recursive:
      ptype = PTHREAD_MUTEX_RECURSIVE;
      break;
    case Default:
      break;
    case Auto:
#ifdef AIPS_DEBUG
      ptype = PTHREAD_MUTEX_ERRORCHECK;
#endif
      break;
    }
    // Create the mutex.
    itsMutex = new pthread_mutex_t;
    int error;
    if (ptype == PTHREAD_MUTEX_DEFAULT) {
      error = pthread_mutex_init (ITSMUTEX, 0);
      if (error != 0) throw SystemCallError ("pthread_mutex_init",error);
    } else {
      pthread_mutexattr_t attr;
      error = pthread_mutexattr_init (&attr);
      if (error != 0) throw SystemCallError ("pthread_mutexattr_init",error);
      error = pthread_mutexattr_settype (&attr, ptype);
      if (error != 0) throw SystemCallError ("pthread_mutexattr_settype",error);
      error = pthread_mutex_init (ITSMUTEX, &attr);
      if (error != 0) throw SystemCallError ("pthread_mutex_init",error);
      // The attribute can be destroyed right after the init.
      error = pthread_mutexattr_destroy (&attr);
      if (error != 0) throw SystemCallError ("pthread_mutexattr_destroy",error);
    }
  }

  Mutex::~Mutex()
  {
    int error = pthread_mutex_destroy (ITSMUTEX);
    if (error != 0) throw SystemCallError ("pthread_mutex_destroy", error);
    delete ITSMUTEX;
  }

  void Mutex::lock()
  {
    int error = pthread_mutex_lock (ITSMUTEX);
    if (error != 0) throw SystemCallError ("pthread_mutex_lock", error);
  }

  void Mutex::unlock()
  {
    int error = pthread_mutex_unlock (ITSMUTEX);
    if (error != 0) throw SystemCallError ("pthread_mutex_unlock", error);
  }

  Bool Mutex::trylock()
  {
    int error = pthread_mutex_trylock(ITSMUTEX);
    switch (error) {
    case 0:
      return True;
    case EBUSY:
    case EDEADLK: // returned by error_check mutexes
      return False;
    default:
      throw SystemCallError ("pthread_mutex_trylock", error);
    }
  }

#else

  Mutex::Mutex (Mutex::Type)
    : itsMutex(0) {}
  Mutex::~Mutex()
  {}
  void Mutex::lock()
  {}
  void Mutex::unlock()
  {}
  Bool Mutex::trylock()
  { return True; }

#endif


  MutexedInit::MutexedInit (InitFunc* func, void* arg, Mutex::Type type)
    : itsMutex  (type),
      itsFunc   (func),
      itsArg    (arg),
      itsDoExec (True)
  {}

  void MutexedInit::doExec()
  {
    // The exec function said we have to execute the initializion function.
    // Obtain a lock and test the flag again to see if another thread hasn't
    // executed the function in the mean time.
    ScopedMutexLock locker(itsMutex);
    if (itsDoExec) {
      itsFunc (itsArg);
      itsDoExec = False;
    }
  }


} // namespace casacore
