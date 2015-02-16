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

#ifndef SCIMATH_STATISTICSTYPES_H
#define SCIMATH_STATISTICSTYPES_H

#include <casacore/casa/aips.h>

#include <utility>
#include <vector>

namespace casacore {

class Record;
template <class T> class CountedPtr;

// Commonly used types in statistics framework.

#define DataRanges std::vector<std::pair<AccumType, AccumType> >

template <class AccumType> struct StatsData {
	Bool masked;
	CountedPtr<AccumType> max;
	std::pair<Int64, Int64> maxpos;
	AccumType mean;
	CountedPtr<AccumType> median;
	CountedPtr<AccumType> medAbsDevMed;
	CountedPtr<AccumType> min;
	std::pair<Int64, Int64> minpos;
	Double npts;
	AccumType nvariance;
	AccumType rms;
	AccumType stddev;
	AccumType sum;
	AccumType sumsq;
	AccumType sumweights;
	AccumType variance;
	Bool weighted;
};

template <class AccumType>
StatsData<AccumType> initializeStatsData();

template <class AccumType>
StatsData<AccumType> copy(const StatsData<AccumType>& stats);

template <class AccumType>
Record toRecord(const StatsData<AccumType>& stats);

}

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Mathematics/StatisticsTypes.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES

#endif
