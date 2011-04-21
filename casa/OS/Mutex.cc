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

#include <casa/OS/Mutex.h>
#include <errno.h>
#include <casa/Exceptions/Error.h>

namespace casa {

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
    int error;
    if (ptype == PTHREAD_MUTEX_DEFAULT) {
      error = pthread_mutex_init (&itsMutex, 0);
      if (error != 0) throw SystemCallError ("pthread_mutex_init",error);
    } else {
      pthread_mutexattr_t attr;
      error = pthread_mutexattr_init (&attr);
      if (error != 0) throw SystemCallError ("pthread_mutexattr_init",error);
      error = pthread_mutexattr_settype (&attr, ptype);
      if (error != 0) throw SystemCallError ("pthread_mutexattr_settype",error);
      error = pthread_mutex_init (&itsMutex, &attr);
      if (error != 0) throw SystemCallError ("pthread_mutex_init",error);
      // The attribute can be destroyed right after the init.
      error = pthread_mutexattr_destroy (&attr);
      if (error != 0) throw SystemCallError ("pthread_mutexattr_destroy",error);
    }
  }

  Mutex::~Mutex()
  {
    int error = pthread_mutex_destroy (&itsMutex);
    if (error != 0) throw SystemCallError ("pthread_mutex_destroy", error);
  }

} // namespace casa
