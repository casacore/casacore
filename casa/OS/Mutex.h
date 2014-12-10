//# Mutex.h: Classes to handle mutexes and (un)locking
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

#ifndef CASA_MUTEX_H
#define CASA_MUTEX_H

#include <casacore/casa/aips.h>

//# Mostly copied from the LOFAR software.

namespace casacore {

  // <summary>Wrapper around a pthreads mutex</summary>
  // <use visibility=export>
  //
  // <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
  // </reviewed>
  //
  // <synopsis>
  // This class is a wrapper around a phtreads mutex.
  // <br>Although the Mutex class has a lock function, class ScopedMutexLock
  // should be used to obtain a lock, because it makes locking exception-safe.
  // </synopsis>

  class Mutex
  {
  public:
    // Define the type of mutex.
    // (see phtread_mutexattr_settype for their meaning).
    // In Debug mode, type Auto will use PTHREAD_MUTEX_ERRORCHECK,
    // otherwise PTHREAD_MUTEX_DEFAULT.
    enum Type {Normal, ErrorCheck, Recursive, Default, Auto};

    // Create the mutex.
    Mutex (Type type=Auto);

    // Destroy the mutex.
    ~Mutex();

    // Set a lock on the mutex. It waits till it gets the lock.
    void lock();

    // Unlock the mutex.
    void unlock();

    // Try to lock the mutex. True is returned if it succeeded.
    bool trylock();

  private:
    // Forbid copy constructor.
    Mutex (const Mutex&);
    // Forbid assignment.
    Mutex& operator= (const Mutex&);

    //# Data members
    //# Use void*, because we cannot forward declare pthread_mutex_t.
    void* itsMutex;
  };


  // <summary>Exception-safe lock/unlock of a mutex</summary>
  // <use visibility=export>
  //
  // <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
  // </reviewed>
  //
  // <synopsis>
  // The constructor of this class locks a mutex, while the destructor
  // unlocks it. In this way the user does not need to take care of
  // unlocking a mutex and is mutex locking fully exception-safe
  // </synopsis>

  class ScopedMutexLock
  {
  public:
    // Create a lock on the mutex.
    ScopedMutexLock (Mutex& mutex)
      : itsMutexRef(mutex)
      { itsMutexRef.lock(); }

    // The destructor automatically unlocks the mutex.
    ~ScopedMutexLock()
      { itsMutexRef.unlock(); }
    
  private:
    // Forbid copy constructor.
    ScopedMutexLock (const ScopedMutexLock&);
    // Forbid assignment.
    ScopedMutexLock& operator= (const ScopedMutexLock&);

    Mutex& itsMutexRef;
  };


  // <summary>Thread-safe initialization of global variables</summary>
  // <use visibility=export>
  //
  // <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
  // </reviewed>
  //
  // <synopsis>
  // This class does a double checked lock.
  // <br>
  // Often data needs to be initialized once and accessed many times. To do
  // this in a thread-safe way, a mutex lock needs to be used. However, that
  // is relatively expensive.
  // The double checked lock idiom overcomes this problem. A Bool flag tells
  // if an operation needs to be done. If so, a lock is set and the flag is
  // tested again in case another thread happened to do the operation between
  // the test and acquiring the lock.
  // At the end of the operation the flag is cleared.
  // 
  //
  // The flag needs to be declared volatile to avoid execution ordering
  // problems in case a compiler optimizes too much.
  // <note role-warning>
  // This idiom is not fully portable, because on more exotic machines the
  // caches in different cores may not be synchronized well.
  // </note>
  // </synopsis>
  //
  // <example>
  // <srcblock>
  // // Declare static variables.
  // static volatile Bool needInit = True;
  // static Mutex mutex;
  // // Execute the code in a scope, so the destructor is called automatically.
  // {
  //   CheckedMutexLock locker(mutex, needInit);
  //   if (locker.doIt()) {
  //      .. do the initialization
  //   }
  // }
  // </srcblock>
  // </example>

  class MutexedInit
  {
  public:
    // Define the initialization function to call.
    typedef void InitFunc (void*);

    // Create the mutex and set that the initialization should be done.
    MutexedInit (InitFunc* func, void* arg=0, Mutex::Type type=Mutex::Auto);

    // Execute the initialization function if not done yet.
    void exec()
      { if (itsDoExec) doExec(); }

    // Get the mutex (to make it possible to lock for other purposes).
    Mutex& mutex()
      { return itsMutex; }

  private:
    // Forbid copy constructor.
    MutexedInit (const MutexedInit&);
    // Forbid assignment.
    MutexedInit& operator= (const MutexedInit&);

    // Thread-safe execution of the initialization function (if still needed).
    void doExec();

    //# Data members
    Mutex         itsMutex;
    InitFunc*     itsFunc;
    void*         itsArg;
    volatile Bool itsDoExec;
  };


} // namespace casacore

#endif
