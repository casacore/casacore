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

#ifndef LATTICES_LATTICESTATSDATAPROVIDERBASE_H
#define LATTICES_LATTICESTATSDATAPROVIDERBASE_H

#include <casacore/casa/aips.h>
#include <casacore/lattices/Lattices/LatticeIterator.h>
#include <casacore/scimath/Mathematics/StatsDataProvider.h>


namespace casacore {

// Abstract base class of data providers which allows stats framework to iterate through a lattice.

template <class AccumType, class T, class InputIterator=const T*> class LatticeStatsDataProviderBase
	: public StatsDataProvider<AccumType, InputIterator, const Bool*> {
public:

	virtual ~LatticeStatsDataProviderBase();

	// Get the stride for the current mask (only called if hasMask() returns True).
	uInt getMaskStride();

	// Get the associated range(s) of the current dataset. Only called if hasRanges() returns True;
	DataRanges getRanges();

	// Get the stride for the current data set.
	uInt getStride();

	// Returns NULL; lattices do not have associated weights.
	InputIterator getWeights();

	// Does the current data set have associated range(s)?
	Bool hasRanges() const;

	// returns False; lattices do not have associated weights.
	Bool hasWeights() const;

	// If the associated data set has ranges, are these include (return True) or
	// exclude (return False) ranges?
	Bool isInclude() const;

	// set the data ranges
	void setRanges(const DataRanges& ranges, Bool isInclude);

protected:
	LatticeStatsDataProviderBase();

private:
	//RO_LatticeIterator<T> _iter;
	//Array<T> _currentSlice;
	//const T* _currentPtr;
	//Bool _delData,
	Bool _hasRanges, _isInclude;
	DataRanges _ranges;

	//void _freeStorage();
};

}

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/lattices/LatticeMath/LatticeStatsDataProviderBase.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES

#endif
