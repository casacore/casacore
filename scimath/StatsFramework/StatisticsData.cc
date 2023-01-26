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

#include <casacore/scimath/StatsFramework/StatisticsData.h>

#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/BasicMath/Math.h>

#include <algorithm>

namespace casacore {

String StatisticsData::toString(STATS stat) {
    switch(stat) {
    case MAX:
        return "max";
    case MEAN:
        return "mean";
    case MIN:
        return "min";
    case NPTS:
        return "npts";
    case RMS:
        return "rms";
    case STDDEV:
        return "stddev";
    case SUM:
        return "sum";
    case SUMSQ:
        return "sumsq";
    case SUMWEIGHTS:
        return "sumOfWeights";
    case VARIANCE:
        return "variance";
    case MEDIAN:
        return "median";
    case MEDABSDEVMED:
        return "median of the absolute devation from the median";
    case FIRST_QUARTILE:
        return "first quartile";
    case THIRD_QUARTILE:
        return "third quartile";
    case INNER_QUARTILE_RANGE:
        return "inner quartile range";
    default:
        ThrowCc(
            "Logic error: Unhandled value in switch statement"
            + String::toString(stat)
        );
    }
}

std::map<double, uint64_t> StatisticsData::indicesFromFractions(
    uint64_t npts, const std::set<double>& fractions
) {
    std::map<double, uint64_t> fractionToIndexMap;
    for_each(
        fractions.cbegin(), fractions.cend(),
        [&fractionToIndexMap, &npts](double q) {
        auto idxWRT1 = q * npts;
        auto myfloor = floor(idxWRT1);
        if (near(idxWRT1, myfloor)) {
            // prevent rounding due to finite machine precision
            idxWRT1 = myfloor;
        }
        fractionToIndexMap[q] = ((uint64_t)ceil(idxWRT1) - 1);
    });
    return fractionToIndexMap;
}

}
