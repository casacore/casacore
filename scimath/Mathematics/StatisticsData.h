//# Copyright (C) 2014
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

#ifndef SCIMATH_STATISTICSDATA_H
#define SCIMATH_STATISTICSDATA_H

#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore {

/*
 * This class simply defines the enum of supported statistics types
 * in the statistics framework.
 */

class StatisticsData {
public:

	enum STATS {
		MAX,
		MEAN,
		MIN,
		NPTS,
		// QUANTILE includes things like median, quartile, etc
		// QUANTILE,
		RMS,
		STDDEV,
		SUM,
		SUMSQ,
		// sum of weights
		SUMWEIGHTS,
		VARIANCE
	};

	/*
	// There are various conflicting definitions of what the median is for
	// a dataset with an even number of points. This enum attempts to account for
	// those, providing API users to specify which they wish to use.
	enum EVEN_MEDIAN {
		// use the mean value of the middle two values
		MEDIAN_IS_MEAN,
		// use the minimum value of the middle two values
		LOWER_VALUE,
		// use the maximum value of the middle two values
		UPPER_VALUE
	};
*/
	static String toString(STATS stat);
};

}

#endif
