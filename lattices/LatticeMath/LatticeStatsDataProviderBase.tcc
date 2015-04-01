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

#ifndef LATTICES_LATTICESTATSDATAPROVIDERBASE_TCC
#define LATTICES_LATTICESTATSDATAPROVIDERBASE_TCC

#include <casacore/lattices/LatticeMath/LatticeStatsDataProviderBase.h>

#include <casacore/lattices/LatticeMath/LatticeProgress.h>

namespace casacore {

template <class T>
LatticeStatsDataProviderBase<T>::LatticeStatsDataProviderBase()
: _hasRanges(False), _isInclude(True), _ranges(),
  _progressMeter(NULL), _minPos(), _maxPos() {}

template <class T>
LatticeStatsDataProviderBase<T>::~LatticeStatsDataProviderBase() {}

template <class T>
uInt LatticeStatsDataProviderBase<T>::getMaskStride() {
	return 1;
}

template <class T>
void LatticeStatsDataProviderBase<T>::finalize() {}

template <class T>
std::vector<std::pair<typename NumericTraits<T>::PrecisionType, typename NumericTraits<T>::PrecisionType> > LatticeStatsDataProviderBase<T>::getRanges() {
	return _ranges;
}

template <class T>
uInt LatticeStatsDataProviderBase<T>::getStride() {
	return 1;
}

template <class T>
const T* LatticeStatsDataProviderBase<T>::getWeights() {
	return NULL;
}

template <class T>
Bool LatticeStatsDataProviderBase<T>::hasRanges() const {
	return _hasRanges;
}

template <class T>
Bool LatticeStatsDataProviderBase<T>::hasWeights() const {
	return False;
}

template <class T>
Bool LatticeStatsDataProviderBase<T>::isInclude() const {
	return _isInclude;
}

template <class T>
void LatticeStatsDataProviderBase<T>::minMaxPos(
	IPosition& minPos, IPosition& maxPos) const {
	minPos = _minPos;
	maxPos = _maxPos;
}

template <class T>
void LatticeStatsDataProviderBase<T>::reset() {
	_minPos.resize(0);
	_maxPos.resize(0);
    /*
    if (! _progressMeter.null()) {
        cout << "reset progress" << endl;
        _progressMeter->init(_progressMeter->expectedNsteps());
	}
    */
}

template <class T>
void LatticeStatsDataProviderBase<T>::setProgressMeter(
	CountedPtr<LattStatsProgress> pm
) {
	_progressMeter = pm;
}

template <class T>
void LatticeStatsDataProviderBase<T>::setRanges(
	const std::vector<std::pair<typename NumericTraits<T>::PrecisionType, typename NumericTraits<T>::PrecisionType> >& ranges,
	Bool isInclude
) {
	_hasRanges = ! ranges.empty();
	_ranges = ranges;
	_isInclude = isInclude;
}

template <class T>
void LatticeStatsDataProviderBase<T>::_updateProgress() {
	if (! _progressMeter.null()) {
		(*_progressMeter)++;
	}
}

}


#endif
