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

#include <iterator>
#include <iomanip>

namespace casacore {

template <class AccumType> StatsHistogram<AccumType>::StatsHistogram()
    : _binWidth(0), _maxHistLimit(0), _minHistLimit(0), _nBins(0),
      _maxBinLimits(0) {}

template <class AccumType>
StatsHistogram<AccumType>::StatsHistogram(
    AccumType minLimit, AccumType maxLimit, uInt nBins
) : _binWidth(0), _minHistLimit(minLimit), _maxHistLimit(maxLimit),
    _nBins(nBins), _maxBinLimits(nBins) {
    ThrowIf (minLimit > maxLimit, "minData must be less than max data");
    _binWidth = (_maxHistLimit - _minHistLimit)/(AccumType)nBins;
    // in case of AccumType = Int, this can happen even if max and min are
    // different.
    ThrowIf(_binWidth == AccumType(0), "Histogram bin width is 0");
    uInt j = 1;
    for_each(
        _maxBinLimits.begin(), _maxBinLimits.end(), [&] (AccumType& val) {
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
    // did not find in initial guessed bin, test in some bins greater than and
    // less than idx. It tests out to 10, but in practice, if this needs to
    // be done, the value is in a bin that is only one or two bins from the
    // idx bin. Start at a diff of 1, as we've already tested a diff of 0.
    for (uInt i=1; i<10; ++i) {
        // check bin above idx
        auto upIdx = idx + i;
        auto tried = False;
        if (upIdx < _nBins) {
            if (
                value >= _maxBinLimits[upIdx - 1]
                    && value < _maxBinLimits[upIdx]
            ) {
                return upIdx;
            }
            tried = True;
        }
        // check bin below idx
        // avoid uInt underflow
        if (i <= idx) {
            auto downIdx = idx - i;
            mymin = downIdx == 0 ? _minHistLimit : _maxBinLimits[downIdx - 1];
            if (value >= mymin && value < _maxBinLimits[downIdx]) {
                return downIdx;
            }
            tried = True;
        }
        if (! tried) {
            // idx +/- i are both outside histogram, so exit loop
            break;
        }
    }
    ostringstream os;
    os << std::setprecision(10) << "Unable to locate bin containing value "
        << value << endl;
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

template <class AccumType>
uInt StatsHistogram<AccumType>::getNBins() const {
    return _nBins;
}

}

#endif
