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

#ifndef SCIMATH_STATSHISTOGRAM_TCC
#define SCIMATH_STATSHISTOGRAM_TCC

#include <casacore/scimath/StatsFramework/StatsHistogram.h>

#include <algorithm>
#include <iterator>
#include <iomanip>

namespace casacore {

template <class AccumType>
StatsHistogram<AccumType>::StatsHistogram(
    AccumType minLimit, AccumType maxLimit, uInt nBins
) : _binWidth(0), _minHistLimit(minLimit), _maxHistLimit(maxLimit),
    _nBins(nBins), _maxBinLimits(nBins) {
    ThrowIf (minLimit > maxLimit, "minLimit must be less than maxLimit");
    _binWidth = (_maxHistLimit - _minHistLimit)/(AccumType)nBins;
    // in case of AccumType = Int, this can happen even if max and min are
    // different. One would hope that AccumType=Int would never be used, but
    // just in case. The check incurs a negligible performance hit.
    ThrowIf(_binWidth == AccumType(0), "Histogram bin width is 0");
    uInt j = 1;
    for_each(
        _maxBinLimits.begin(), _maxBinLimits.end(), [&j, this]
        (AccumType& val) {
            val = _minHistLimit + _binWidth * (AccumType)(j);
            ++j;
        }
    );
}

template <class AccumType> StatsHistogram<AccumType>::~StatsHistogram() {}

template <class AccumType>
AccumType StatsHistogram<AccumType>::getBinWidth() const {
    return _binWidth;
}

template <class AccumType>
uInt StatsHistogram<AccumType>::getIndex(AccumType value) const {
    // we do not explicitly check if the value is within the histogram,
    // because the caller has already done that
    // estimate the index
    auto idx = _getUInt((value - _minHistLimit)/_binWidth);
    auto mymin = idx == 0 ? _minHistLimit : _maxBinLimits[idx - 1];
    if (value >= mymin && value < _maxBinLimits[idx]) {
        return idx;
    }
    auto higher = value >= _maxBinLimits[idx];
    Int testIdx = higher ? idx + 1 : idx - 1;
    // should never happen, but check just in case...
    if (higher) {
        ThrowIf(testIdx >= (Int)_nBins, "testIdx >= nBins");
    }
    else {
        ThrowIf(testIdx < 0, "testIdx < 0");
    }
    Int minIdx = higher ? idx : testIdx;
    Int maxIdx = higher ? testIdx : idx;
    // we must first establish a bin index
    // range which includes the target value
    _minMaxIdxRange(minIdx, maxIdx, value, higher);
    // bin index limits established, so now do binary search to find the
    // correct bin
    while (True) {
        ThrowIf(
            maxIdx < minIdx,
            "Logic Error: maxIdx (" + String::toString(maxIdx) + ") < minIdx ("
            + String::toString(minIdx) + ")"
        );
        // integer division
        testIdx = (minIdx + maxIdx)/2;
        if (
            value >= _maxBinLimits[testIdx - 1]
            && value < _maxBinLimits[testIdx]
        ) {
            // bin found
            return testIdx;
        }
        // the = part is important, for machine precision issues if the mean
        // binwidth is very small, since some (but not all) bins for all
        // intents and purposes may have zero width. See eg CAS-11828
        if (value >= _maxBinLimits[testIdx - 1]) {
            minIdx = testIdx + 1;
        }
        else {
            maxIdx = testIdx - 1;
        }
    }
    ostringstream os;
    os << std::setprecision(20) << "Logic Error: Unable to locate bin "
        << "containing value " << value << endl;
    os << "Histogram spec " << *this << endl;
    os << "Guessed index " << idx << " with limits " << mymin << ", "
        << _maxBinLimits[idx] << endl;
    ThrowCc(os.str());
}

template <class AccumType>
const std::vector<AccumType>&
StatsHistogram<AccumType>::getMaxBinLimits() const {
    return _maxBinLimits;
}

template <class AccumType>
AccumType StatsHistogram<AccumType>::getMaxHistLimit() const {
    return _maxHistLimit;
}

template <class AccumType>
AccumType StatsHistogram<AccumType>::getMinHistLimit() const {
    return _minHistLimit;
}

template <class AccumType> uInt StatsHistogram<AccumType>::getNBins() const {
    return _nBins;
}

template <class AccumType> void StatsHistogram<AccumType>::_minMaxIdxRange(
    Int& minIdx, Int& maxIdx, AccumType value, Bool higher
) const {
    Int mult = 2;
    while(True) {
        auto mymin = minIdx == 0 ? _minHistLimit : _maxBinLimits[minIdx - 1];
        if (value >= mymin && value < _maxBinLimits[maxIdx]) {
            // limits established
            return;
        }
        mult *= 2;
        if (higher) {
            minIdx = maxIdx + 1;
            if (minIdx >= (Int)_nBins) {
                minIdx = _nBins - 1;
                maxIdx = minIdx;
                // minIdx can't get any larger, so return
                return;
            }
            maxIdx = minIdx + mult;
            if (maxIdx >= (Int)_nBins) {
                maxIdx = _nBins - 1;
                // maxIdx can't get any larger, so return
                return;
            }
        }
        else {
            maxIdx = minIdx - 1;
            if (maxIdx <= 0) {
                maxIdx = 0;
                minIdx = 0;
                // maxIdx can't get any smaller, so return
                return;
            }
            minIdx = maxIdx - mult;
            if (minIdx < 0) {
                minIdx = 0;
                // minIdx can't get any smaller, so return
                return;
            }
        }
    }
}

}

#endif
