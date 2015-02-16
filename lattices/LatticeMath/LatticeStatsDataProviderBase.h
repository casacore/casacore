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

#ifndef LATTICES_LATTICESTATSDATAPROVIDERBASE_H
#define LATTICES_LATTICESTATSDATAPROVIDERBASE_H

#include <casacore/scimath/Mathematics/StatsDataProvider.h>

#include <casacore/scimath/Mathematics/NumericTraits.h>
#include <casacore/lattices/Lattices/LatticeIterator.h>
#include <casacore/lattices/LatticeMath/LattStatsProgress.h>

#include <casacore/casa/aips.h>

namespace casacore {

class LatticeProgress;

// Abstract base class of data providers which allows stats framework to iterate through a lattice.

template <class T> class LatticeStatsDataProviderBase
	: public StatsDataProvider<typename NumericTraits<T>::PrecisionType, const T*, const Bool*> {

public:

	//typedef typename NumericTraits<T>::PrecisionType AccumType;

	virtual ~LatticeStatsDataProviderBase();

	// estimated number of steps to iterate through the the lattice
	virtual uInt estimatedSteps() const = 0;

	virtual void finalize();

	// Get the stride for the current mask (only called if hasMask() returns True).
	uInt getMaskStride();

	// Get the associated range(s) of the current dataset. Only called if hasRanges() returns True;
	std::vector<std::pair<typename NumericTraits<T>::PrecisionType, typename NumericTraits<T>::PrecisionType> > getRanges();

	// Get the stride for the current data set.
	uInt getStride();

	// Returns NULL; lattices do not have associated weights.
	const T* getWeights();

	// Does the current data set have associated range(s)?
	Bool hasRanges() const;

	// returns False; lattices do not have associated weights.
	Bool hasWeights() const;

	// If the associated data set has ranges, are these include (return True) or
	// exclude (return False) ranges?
	Bool isInclude() const;

	// get the positions of the min and max
	void minMaxPos(IPosition& minpos, IPosition& maxpos) const;

	virtual void reset();

	void setProgressMeter(CountedPtr<LattStatsProgress> pm);

	// set the data ranges
	void setRanges(
		const std::vector<std::pair<typename NumericTraits<T>::PrecisionType, typename NumericTraits<T>::PrecisionType> >& ranges,
		Bool isInclude
	);

protected:
	LatticeStatsDataProviderBase();

	//virtual uInt _nsteps() const = 0;

	void _updateMaxPos(const IPosition& maxPos) { _maxPos = maxPos; }

	void _updateMinPos(const IPosition& minPos) { _minPos = minPos; }

	void _updateProgress();

private:
	Bool _hasRanges, _isInclude;
	std::vector<std::pair<typename NumericTraits<T>::PrecisionType, typename NumericTraits<T>::PrecisionType> > _ranges;
	CountedPtr<LattStatsProgress> _progressMeter;
	IPosition _minPos, _maxPos;
};

}

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/lattices/LatticeMath/LatticeStatsDataProviderBase.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES

#endif
