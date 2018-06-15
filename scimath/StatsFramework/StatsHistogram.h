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

#ifndef SCIMATH_STATSHISTOGRAM_H
#define SCIMATH_STATSHISTOGRAM_H

#include <casacore/casa/Utilities/DataType.h>
#include <casacore/casa/aips.h>

#include <iostream>
#include <casacore/casa/iosfwd.h>

#include <vector>

namespace casacore {

// Represents an unfilled histogram with equal width bins for binning used for
// quantile computations. It is necessary to carry along the min/max values of
// the bins, because precision issues when the bin width is sufficiently small
// can cause slightly different results for these when different methods are
// used to compute them, leading to accounting errors when the histogram is
// filled with data.

template <class AccumType> class StatsHistogram {
public:

    // Construct a histogram by specifying its minimum value, its maximum value
    // and the number of desired bins. No padding of the min/max values is done
    // internally, so the caller should do that prior to construction if
    // necessary. The number of resulting bins may be slightly less than
    // requested, because precision issues, particularly for small bin widths,
    // can result in the need for fewer bins than requested when the vector of
    // maximum bin values is filled.
    StatsHistogram(AccumType minLimit, AccumType maxLimit, uInt nBins);

    ~StatsHistogram();

    AccumType getBinWidth() const;

    // get the index of the bin containing the specified value
    uInt getIndex(AccumType value) const;

    // max edges for all bins
    const std::vector<AccumType>& getMaxBinLimits() const;

    // max edge of histogram
    AccumType getMaxHistLimit() const;

    // The minimum edge of the histogram
    AccumType getMinHistLimit() const;

    uInt getNBins() const;

private:

    AccumType _binWidth, _minHistLimit, _maxHistLimit;
    uInt _nBins;
    // maximum values for all bins
    std::vector<AccumType> _maxBinLimits;

    StatsHistogram();

    // This does the obvious conversions. The Complex and DComplex versions
    // (implemented after the class definition) are used solely to permit
    // compilation. In general, these versions should never actually be called
    inline static uInt _getUInt(const AccumType& v) {
        return (uInt)v;
    }

};

// <group>
// The Complex and DComplex versions
// are used solely to permit compilation. In general, these versions should
// never actually be called
template<>
inline uInt StatsHistogram<casacore::Complex>::_getUInt(const casacore::Complex&) {
    ThrowCc("This version for complex data types should never be called");
}

template<>
inline uInt StatsHistogram<casacore::DComplex>::_getUInt(const casacore::DComplex&) {
    ThrowCc("Logic Error: This version for complex data types should never be called");
}
// </group>


// for use in debugging
template <class AccumType>
ostream &operator<<(ostream &os, const StatsHistogram<AccumType> &hist) {
	os << "min limit " << hist.getMinHistLimit() << " max limit "
        << hist.getMaxHistLimit() << "  bin width "
	    << hist.getBinWidth() << " nbins " << hist.getNBins();
	return os;
}

}

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include "StatsHistogram.tcc"
#endif //# CASACORE_NO_AUTO_TEMPLATES

#endif
