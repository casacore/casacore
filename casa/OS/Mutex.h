//# Mutex.h: Classes to handle mutexes and (un)locking
//# Copyright (C) 2011,2016
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

#ifdef USE_THREADS
# include <cerrno>
# ifdef AIPS_CXX11
#  include <mutex>
#  include <atomic>
# endif
# include <pthread.h>
#endif

#include <casacore/casa/Exceptions/Error.h>

//# Mostly copied from the LOFAR software.

namespace casacore {

  // <summary>Wrapper around a pthreads mutex</summary>
  // <use visibility=export>
  //
  // <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
  // </reviewed>
  //
  // <synopsis>
  // This class is a wrapper around a pthreads mutex.
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

#ifdef USE_THREADS

    // Create the mutex.
    Mutex (Type type=Auto);

    // Destroy the mutex.
    ~Mutex();

    // Lock the mutex. It blocks until it can get exclusive access to the lock.
    void lock()
    {
      int error = pthread_mutex_lock(&itsMutex);
      if (AIPS_UNLIKELY(error != 0)) {
        throw SystemCallError("pthread_mutex_lock", error);
      }
    }

    // Unlock the mutex.
    void unlock()
    {
      int error = pthread_mutex_unlock(&itsMutex);
      if (AIPS_UNLIKELY(error != 0)) {
        // Terminates if in a destructor during exception, but acceptable,
        // since it would be a serious bug hopefully exposed by a test case.
        throw SystemCallError("pthread_mutex_unlock", error);
      }
    }

    // Try to lock the mutex. True is returned if it succeeded.
    bool trylock()
    {
      int error = pthread_mutex_trylock(&itsMutex);
      if (error == 0) {
        return true;
      } else if (error == EBUSY ||
                 error == EDEADLK) { // returned by ErrorCheck mutex
        return false;
      } else {
        throw SystemCallError("pthread_mutex_trylock", error);
      }
    }

#else

    Mutex (Mutex::Type=Auto) { }
    ~Mutex() { }
    void lock() { }
    void unlock() { }
    bool trylock() { return true; }

#endif

  private:
    // Forbid copy constructor.
    Mutex (const Mutex&);
    // Forbid assignment.
    Mutex& operator= (const Mutex&);

    //# Data members
#ifdef USE_THREADS
    pthread_mutex_t itsMutex;
#endif
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


  // <summary>Wrapper around pthread_once() and its flag type and init value.
  // </summary>
  // <use visibility=export>
  //
  // <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
  // </reviewed>
  //
  // <synopsis>
  // Ease correct lazy initialization of static data in the easy cases.
  // <br>
  // Often data needs to be initialized once and accessed many times. To do
  // this in a thread-safe way, a mutex is expensive, and double-checked
  // locking (as well as 'static' function scope inits) can only be written
  // correctly portably from C++11 on. Prefer to use C++11 std::call_once().
  // Instead, resort to pthread_once(). Unlike std::call_once() it cannot
  // call with args or return values (can use C++11 lambda to do it) and does
  // not do stack unwinding (although glibc built with -fexceptions does it).
  //
  // Restrictions with any once call primitive (C++11, C11, pthread, boost)
  // are that the called function cannot safely call itself (recursion),
  // and that the flag should not (or cannot) be reset (meaning of 'once').
  // Note that if the function and flag reside in a (dynamically) loaded
  // library and you load (and unload) it more than once, then the function
  // can be called multiple times.
  //
  // Note that non-constant (i.e. dynamic) initialization of a static decl
  // in a function is not guaranteed thread-safe before C++11 (although GCC
  // and Clang make it so). This is hard to get right, since Mutex and CallOnce
  // itself have the same issue. (One could declare a static smart ptr.)
  // We resort to C++11 guarantees and depend on GCC/Clang/... for pre-C++11.
  // </synopsis>
  //
  // <example>
  // (Note that this simple case can also be solved with C++11 thread-safe
  //  initialization of function statics (can be slightly more efficient).)
  // all under namespace casacore:
  // .h:
  //     class A {
  //       static std::vector<int> theirSharedInts;
  //       static CallOnce         theirInitOnce;
  //       void initX();
  //
  //     public:
  //       int getX(size_t idx);
  //     };
  // .cc:
  //     vector<int>  A::theirSharedInts(1);
  //     CallOnce     A::theirInitOnce;
  //
  //     void A::initX(int val) {
  //       theirSharedInts[0] = val;
  //     }
  //
  //     int A::getX(size_t idx) {
  //       theirInitOnce(initX, 42);
  //       return theirSharedInts.at(idx);
  //     }
  // </example>

  // CallOnce0: func has 0 args. For pre-C++11 use pthread_once(), no Mutex.
  class CallOnce0
  {
  public:
    CallOnce0()
#ifdef USE_THREADS
# ifndef AIPS_CXX11 // pre-C++11
    : itsFlag(PTHREAD_ONCE_INIT)
# endif
#else
    : itsFlag(false)
#endif
    { }

    void operator()(void (*fn)()) {
#ifdef USE_THREADS
# ifdef AIPS_CXX11
      std::call_once(itsFlag, fn);
# else // pre-C++11
      pthread_once(&itsFlag, fn);
# endif
#else
      if (!itsFlag) {
        (*fn)();
        itsFlag = true;
      }
#endif
    }

private:
    // Forbid copy constructor.
    CallOnce0(const CallOnce0&);
    // Forbid assignment.
    CallOnce0& operator= (const CallOnce0&);

#ifdef USE_THREADS
# ifdef AIPS_CXX11
    std::once_flag itsFlag;
# else // pre-C++11
    pthread_once_t itsFlag;
# endif
#else
    bool itsFlag;
#endif
  };

  // CallOnce: func has one arg. For pre-C++11: resort to a Mutex.
  // One arg can also be used as an output or to refer to an object (more args).
  // Don't fully expose std::call_once() i/f until we drop pre-C++11 support.
  class CallOnce
  {
  public:
    CallOnce()
#if not defined(USE_THREADS) || not defined(AIPS_CXX11)
    : itsFlag(false)
#endif
    { }

    template<typename T>
    void operator()(void (*fn)(T), T t) {
#if defined(USE_THREADS) && defined(AIPS_CXX11)
      std::call_once(itsFlag, fn, t);
#else // !USE_THREADS (empty Mutex impl) or pre-C++11
      // Warning: do not wrap with a check on itsFlag (unsafe double-checked locking)!
      ScopedMutexLock lock(itsMutex);
      if (!itsFlag) {
        (*fn)(t);
        itsFlag = true;
      }
#endif
    }

  private:
    // Forbid copy constructor.
    CallOnce(const CallOnce&);
    // Forbid assignment.
    CallOnce& operator= (const CallOnce&);

#if defined(USE_THREADS) && defined(AIPS_CXX11)
    std::once_flag itsFlag;
#else // !USE_THREADS (empty Mutex impl) or pre-C++11
    bool  itsFlag;
    Mutex itsMutex;
#endif
  };

} // namespace casacore

#endif
