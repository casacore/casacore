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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#

#ifndef SCIMATH_STATSDATAPROVIDER_H
#define SCIMATH_STATSDATAPROVIDER_H

#include <casacore/scimath/StatsFramework/StatisticsTypes.h>

#include <casacore/casa/aips.h>

namespace casacore {

// Abstract base class which defines interface for providing "datasets" to the
// statistics framework in cases where the data structure involved does not
// allow for a trivial means of doing so (eg, in the case of a Lattice).

template <
    class AccumType, class DataIterator, class MaskIterator=const Bool *,
    class WeightsIterator=DataIterator
>
class StatsDataProvider {
public:

	virtual ~StatsDataProvider();

	// increment the data provider to the next dataset, mask, range set, and
	// weights.
	virtual void operator++() = 0;

	// Are there any data sets left to provide?
	virtual Bool atEnd() const = 0;

	// Take any actions necessary to finalize the provider. This will be called
	// when atEnd() returns True.
	virtual void finalize() = 0;

	// get the count of elements in the current data set. When implementing this
	// method, be certain to take stride into account; ie for a data set with
	// nominally 100 elements that is to have a stride of two, this method
	// should return 50.
	virtual uInt64 getCount() = 0;

	// get an iterator to the first element of the current dataset
	virtual DataIterator getData() = 0;

	// Get an iterator to the first element of the mask for the current dataset.
	// Only called if hasMask() returns True;
	virtual MaskIterator getMask() = 0;

	// Get the stride for the current mask.
	// Only called if hasMask() returns True.
	virtual uInt getMaskStride() = 0;

	// If OpenMP is enabled and statistics methods are being called in a
	// multi-threaded context, get maximum number of threads that should be
	// used. If zero is returned, the statistics classes will use the maximum
	// number of threads available to openmp. Returning less than that helps to
	// decrease overhead used by statistics methods when the maximum number of
	// threads available to openmp are unnecessary. The base class
	// implmentation returns 0.
	virtual uInt getNMaxThreads() const;

	// Get the associated range(s) of the current dataset. Only called if
	// hasRanges() returns True;
	virtual DataRanges getRanges() = 0;

	// Get the stride for the current data set.
	virtual uInt getStride() = 0;

	// Get an iterator to the first weights element of the current dataset.
	// Only called if hasWeights() returns True;
	virtual WeightsIterator getWeights() = 0;

	// Does the current data set have an associated mask?
	virtual Bool hasMask() const = 0;

	// Does the current data set have associated range(s)?
	virtual Bool hasRanges() const = 0;

	// Does the current data set have associated weights?
	virtual Bool hasWeights() const = 0;

	// If the associated data set has ranges, are these include (return True) or
	// exclude (return False) ranges?
	virtual Bool isInclude() const = 0;

	// reset the provider to point to the beginning of the first data set it
	// manages.
	virtual void reset() = 0;

	// <group>
	// In general, unless you are writing statistics algorithm code, you
	// shouldn't need to call these methods.
	// The statistics framework calls these methods when the min and max
	// posiitons are updated. It passes in the relevant index of the current sub
	// dataset it is processing. Data providers can use this information to
	// transform into something more useful, eg an IPosition for lattice data
	// providers, so that they may be retreived easily after statistics have
	// been calculated. The default implementations do nothing.
	virtual void updateMaxPos(const LocationType&) {}

	virtual void updateMinPos(const LocationType&) {}
	// </group>

protected:

	StatsDataProvider();

};

}

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/StatsFramework/StatsDataProvider.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES

#endif
