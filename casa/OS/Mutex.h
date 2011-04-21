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

#include <casa/aips.h>
#include <pthread.h>
#include <errno.h>
#include <casa/Exceptions/Error.h>

//# Mostly copied from the LOFAR software.

namespace casa {

  // <summary>Wrapper around a pthreads mutex</summary>
  // <use visibility=export>
  //
  // <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
  // </reviewed>
  //
  // <synopsis>
  // This class is a wrapper around a phtreads mutex.
  // <br>Normally class ScopedLock should be used to obtain a lock, because
  // it makes locking exception-safe.
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
    pthread_mutex_t itsMutex;
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

  class ScopedLock
  {
  public:
    // Create a lock on the mutex.
    ScopedLock (Mutex&);

    // The destructor automatically unlocks the mutex.
    ~ScopedLock();
    
  private:
    // Forbid copy constructor.
    ScopedLock (const ScopedLock&);
    // Forbid assignment.
    ScopedLock& operator= (const ScopedLock&);

    Mutex& itsMutexRef;
  };


  //# Implementation.
#ifdef USE_THREADS

  inline void Mutex::lock()
  {
    int error = pthread_mutex_lock (&itsMutex);
    if (error != 0) throw SystemCallError ("pthread_mutex_lock", error);
  }

  inline void Mutex::unlock()
  {
    int error = pthread_mutex_unlock (&itsMutex);
    if (error != 0) throw SystemCallError ("pthread_mutex_unlock", error);
  }

  inline Bool Mutex::trylock()
  {
    int error = pthread_mutex_trylock(&itsMutex);
    switch (error) {
    case 0:
      return True;
    case EBUSY:
      return False;
    default:
      throw SystemCallError ("pthread_mutex_trylock", error);
    }
  }

#else
  inline void Mutex::lock()
  {}
  inline void Mutex::unlock()
  {}
  inline Bool Mutex::trylock()
  { return True; }
#endif

  inline ScopedLock::ScopedLock(Mutex &mutex)
  : itsMutexRef(mutex)
  {
    itsMutexRef.lock();
  }
  
  inline ScopedLock::~ScopedLock()
  {
    itsMutexRef.unlock();
  }


} // namespace casa

#endif
