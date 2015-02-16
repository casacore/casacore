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
//# $Id: HostInfoDarwin.h 21521 2014-12-10 08:06:42Z gervandiepen $

#ifndef SCIMATH_STATSISTICSDATA_H
#define SCIMATH_STATSISTICSDATA_H

#include <casacore/casa/aips.h>

#include <map>
#include <set>
#include <math.h>

namespace casacore {

class String;

/*
 * This class simply defines the enum of supported statistics types
 * in the statistics framework.
 */

class StatisticsData {
public:

	// implemented algorithms
	enum ALGORITHM {
		CHAUVENETCRITERION,
		CLASSICAL,
		FITTOHALF,
		HINGESFENCES
	};

	enum STATS {
		MAX,
		MEAN,
		MIN,
		NPTS,
		RMS,
		STDDEV,
		SUM,
		SUMSQ,
		// sum of weights
		SUMWEIGHTS,
		VARIANCE
	};

	// get the zero-based indices of the specified fractions in a CDF with npts
	// number of good points. The returned map maps fractions to indices.
	static std::map<Double, uInt64> indicesFromFractions(
		uInt64 npts, const std::set<Double>& fractions
	);

	static String toString(STATS stat);

};

}

#endif
