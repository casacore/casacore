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

#include <casacore/scimath/StatsFramework/BiweightStatisticsData.h>

namespace casacore {

std::set<StatisticsData::STATS> BiweightStatisticsData::_unsupportedStats
    = std::set<StatisticsData::STATS>();

std::mutex BiweightStatisticsData::_mutex;

std::set<StatisticsData::STATS> BiweightStatisticsData::getUnsupportedStats() {
    std::lock_guard<std::mutex> sc(_mutex);
    if (_unsupportedStats.empty()) {
        _unsupportedStats.insert(StatisticsData::RMS);
        _unsupportedStats.insert(StatisticsData::SUM);
        _unsupportedStats.insert(StatisticsData::SUMSQ);
        _unsupportedStats.insert(StatisticsData::SUMWEIGHTS);
        _unsupportedStats.insert(StatisticsData::VARIANCE);
        _unsupportedStats.insert(StatisticsData::MEDIAN);
        _unsupportedStats.insert(StatisticsData::MEDABSDEVMED);
        _unsupportedStats.insert(StatisticsData::FIRST_QUARTILE);
        _unsupportedStats.insert(StatisticsData::THIRD_QUARTILE);
        _unsupportedStats.insert(StatisticsData::INNER_QUARTILE_RANGE);
    }
    return _unsupportedStats;
}

}
