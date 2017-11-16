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
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#

#ifndef CASA_OS_OMP_H
#define CASA_OS_OMP_H

#include <casacore/casa/aips.h>

namespace casacore {

class OMP {

// <summary> 
// OpenMP helper functions
// </summary>

public:

    // maximum number of available threads. If openmp is not enabled, returns 1. If openmp
    // is enabled, then if being called from a single-threaded block, returns
    // omp_get_max_threads(), if called from a multi-threaded block, returns 1.
    static uInt nMaxThreads();

};

}

#endif
