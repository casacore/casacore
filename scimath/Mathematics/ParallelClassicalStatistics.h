//# Copyright (C) 2000,2001
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
//# $Id: Array.h 21545 2015-01-22 19:36:35Z gervandiepen $

#ifndef SCIMATH_PARALLELCLASSICALSTATS_H
#define SCIMATH_PARALLELCLASSICALSTATS_H

#include <casacore/casa/aips.h>

#include <casacore/scimath/Mathematics/ClassicalStatistics.h>

#include <casacore/scimath/Mathematics/StatisticsTypes.h>
#include <casacore/scimath/Mathematics/StatisticsUtilities.h>

#include <set>
#include <vector>
#include <utility>

namespace casacore {

// Class which provides parallelization support of the ClassicalStatistics class.
// Currently, the _getStatistics() method is parallelized.

template <class AccumType, class DataIterator, class MaskIterator=const Bool*, class WeightsIterator=DataIterator> 
class ParallelClassicalStatistics
	: public ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator> {
public:

    // <src>maxThreads</src> indicates the maximum number of threads to use.
	ParallelClassicalStatistics(uInt maxThreads=omp_get_max_threads());

	~ParallelClassicalStatistics();

	// Throws an exception if <src>b</src> = True; currently this feature is not
	// supported.
    void setCalculateAsAdded(Bool b);

protected:

	StatsData<AccumType> _getStatistics();

private:
    uInt _nThreadsMax;

};

}

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Mathematics/ParallelClassicalStatistics.tcc>
#endif 

#endif
