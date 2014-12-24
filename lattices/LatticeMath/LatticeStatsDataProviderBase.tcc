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

#include <casacore/lattices/LatticeMath/LatticeStatsDataProviderBase.h>

namespace casacore {

template <class AccumType, class T, class InputIterator>
LatticeStatsDataProviderBase<AccumType, T, InputIterator>::LatticeStatsDataProviderBase()
: _hasRanges(False), _isInclude(True), _ranges() {}

template <class AccumType, class T, class InputIterator>
LatticeStatsDataProviderBase<AccumType, T, InputIterator>::~LatticeStatsDataProviderBase() {}

template <class AccumType, class T, class InputIterator>
uInt LatticeStatsDataProviderBase<AccumType, T, InputIterator>::getMaskStride() {
	return 1;
}

template <class AccumType, class T, class InputIterator>
DataRanges LatticeStatsDataProviderBase<AccumType, T, InputIterator>::getRanges() {
	return _ranges;
}

template <class AccumType, class T, class InputIterator>
uInt LatticeStatsDataProviderBase<AccumType, T, InputIterator>::getStride() {
	return 1;
}

template <class AccumType, class T, class InputIterator>
InputIterator LatticeStatsDataProviderBase<AccumType, T, InputIterator>::getWeights() {
	return NULL;
}

template <class AccumType, class T, class InputIterator>
Bool LatticeStatsDataProviderBase<AccumType, T, InputIterator>::hasRanges() const {
	return _hasRanges;
}

template <class AccumType, class T, class InputIterator>
Bool LatticeStatsDataProviderBase<AccumType, T, InputIterator>::hasWeights() const {
	return False;
}

template <class AccumType, class T, class InputIterator>
Bool LatticeStatsDataProviderBase<AccumType, T, InputIterator>::isInclude() const {
	return _isInclude;
}

template <class AccumType, class T, class InputIterator>
void LatticeStatsDataProviderBase<AccumType, T, InputIterator>::setRanges(
	const DataRanges& ranges, Bool isInclude
) {
	_hasRanges = ranges.size() > 0;
	_ranges = ranges;
	_isInclude = isInclude;
}

}

