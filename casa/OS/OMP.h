//# Copyright (C) 1993,1994,1995,1996,2000,2003
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#

#ifndef CASA_OS_OMP_H
#define CASA_OS_OMP_H

#include <casacore/casa/aips.h>
#ifdef _OPENMP
#include <omp.h>
#endif

namespace casacore {
  namespace OMP {

    // Get the maximum number of threads.
    // OpenMP sets it to the env.var. OMP_NUM_THREADS. If undefined, it is
    // the number of cores.
    // If OpenMP is not used, 1 is returned.
    inline uInt maxThreads()
    {
#ifdef _OPENMP
      return omp_get_max_threads();
#else
      return 1;
#endif
    }
    // Backward 
    uInt nMaxThreads();

    // Set the number of threads to use. Note it can be overridden
    // for a parallel section by 'omp parallel num_threads(n)'.
    // Nothing is done if OpenMP is not used.
#ifdef _OPENMP
    inline void setNumThreads (uInt n)
      { omp_set_num_threads (n); }
#else
    inline void setNumThreads (uInt)
      {}
#endif

    // Get the number of threads used in a parallel piece of code.
    // If OpenMP is not used, 1 is returned.
    inline uInt numThreads()
    {
#ifdef _OPENMP
      return omp_get_num_threads();
#else
      return 1;
#endif
    }

    // Get the thread number (0 till numThreads).
    // If OpenMP is not used, 0 is returned.
    inline uInt threadNum()
    {
#ifdef _OPENMP
      return omp_get_thread_num();
#else
      return 0;
#endif
    }

    // Set if nested parallel sections are possible or not.
    // Nothing is done if OpenMP is not used.
#ifdef _OPENMP
    inline void setNested (Bool nest)
      { omp_set_nested (nest); }
#else
    inline void setNested (Bool)
      {}
#endif

    // Test if nested parallel sections are possible.
    // If OpenMP is not used, false is returned.
    inline bool nested()
    {
#ifdef _OPENMP
      return omp_get_nested();
#else
      return false;
#endif
    }

  } // end namespace
} // end namespace

#endif
