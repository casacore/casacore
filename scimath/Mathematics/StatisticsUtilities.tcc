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

#ifndef SCIMATH_STATISTICSUTILITIES_TCC
#define SCIMATH_STATISTICSUTILITIES_TCC

#include <casacore/scimath/Mathematics/StatisticsUtilities.h>

#include <iostream>

namespace casacore {

template <class AccumType>
const AccumType StatisticsUtilities<AccumType>::TWO = AccumType(2);

// For performance reasons, we ensure code is inlined rather than
// calling other functions. The performance
// benefits become important for very large datasets

#define _NLINEAR \
	npts++; \
	sum += datum; \
	mean += (datum - mean)/npts;

#define _WLINEAR \
	npts++; \
	sumweights += weight; \
	wsum += weight*datum; \
	wmean += weight/sumweights*(datum - wmean);

#define _NQUAD \
	sumsq += datum*datum; \
	AccumType prevMean = mean; \
	_NLINEAR \
	nvariance += (datum - prevMean)*(datum - mean);

#define _WQUAD \
	wsumsq += weight*datum*datum; \
	AccumType prevMean = wmean; \
	_WLINEAR \
	wnvariance += weight*(datum - prevMean)*(datum - wmean);

#define _MAXMIN \
	if (npts == 1) { \
		datamax = datum; \
		maxpos = location; \
		datamin = datum; \
		minpos = location; \
	} \
	else if (datum > datamax) { \
		datamax = datum; \
		maxpos = location; \
	} \
	else if (datum < datamin) { \
		datamin = datum; \
		minpos = location; \
	}

template <class AccumType> void StatisticsUtilities<AccumType>::accumulate (
	Double& npts, AccumType& sum, AccumType& mean, const AccumType& datum
) {
	_NLINEAR
}

template <class AccumType> void StatisticsUtilities<AccumType>::waccumulate (
	Double& npts, AccumType& sumweights, AccumType& wsum, AccumType& wmean,
	const AccumType& datum, const AccumType& weight
) {
	_WLINEAR
}

template <class AccumType> void StatisticsUtilities<AccumType>::accumulate (
	Double& npts, AccumType& sum, AccumType& mean, AccumType& nvariance,
	AccumType& sumsq, const AccumType& datum
) {
	_NQUAD
}

template <class AccumType> void StatisticsUtilities<AccumType>::waccumulate (
	Double& npts, AccumType& sumweights, AccumType& wsum,
	AccumType& wmean, AccumType& wnvariance, AccumType& wsumsq,
	const AccumType& datum, const AccumType& weight
) {
	_WQUAD
}

template <class AccumType> template <class LocationType>
void StatisticsUtilities<AccumType>::accumulate (
	Double& npts, AccumType& sum, AccumType& mean, AccumType& nvariance,
	AccumType& sumsq, AccumType& datamin,
	AccumType& datamax, LocationType& minpos, LocationType& maxpos,
	const AccumType& datum, const LocationType& location
) {
	_NQUAD
	_MAXMIN
}

template <class AccumType> template <class LocationType>
void StatisticsUtilities<AccumType>::waccumulate (
	Double& npts, AccumType& sumweights, AccumType& wsum, AccumType& wmean, AccumType& wnvariance,
	AccumType& wsumsq, AccumType& datamin, AccumType& datamax,
	LocationType& minpos, LocationType& maxpos,
	const AccumType& datum, const AccumType& weight, const LocationType& location
) {
	_WQUAD
	_MAXMIN
}

template <class AccumType> template <class LocationType>
Bool StatisticsUtilities<AccumType>::doMax(
	AccumType& datamax, LocationType& maxpos, Bool isFirst,
	const AccumType& datum, const LocationType& location
) {
	if (isFirst || datum > datamax) {
		datamax = datum;
		maxpos = location;
		return True;
	}
	return False;
}

template <class AccumType> template <class LocationType>
Bool StatisticsUtilities<AccumType>::doMin(
	AccumType& datamin, LocationType& minpos, Bool isFirst,
	const AccumType& datum, const LocationType& location
) {
	if (isFirst || datum < datamin) {
		datamin = datum;
		minpos = location;
		return True;
	}
	return False;
}

/*
#define _NLINEARSYM \
	npts += 2; \
	sum += 2*center; \

#define _WLINEARSYM \
	npts += 2; \
	sumweights += 2*weight; \
	wsum += 2*weight*center; \
*/

#define _NQUADSYM \
	npts += 2; \
	AccumType reflect = TWO*center - datum; \
	sumsq += datum*datum + reflect*reflect; \
	AccumType diff = datum - center; \
	nvariance += TWO*diff*diff;

#define _WQUADSYM \
	npts += 2; \
	sumweights += TWO*weight; \
	AccumType reflect = TWO*center - datum; \
	wsumsq += weight*(datum*datum + reflect*reflect); \
	AccumType diff = datum - center; \
	wnvariance += TWO*weight*diff*diff;

#define _MAXMINSYM \
	if (npts == 2) { \
		datamax = datum; \
		maxpos = location; \
		datamin = datum; \
		minpos = location; \
	} \
	else if (datum > datamax) { \
		datamax = datum; \
		maxpos = location; \
	} \
	else if (datum < datamin) { \
		datamin = datum; \
		minpos = location; \
	}

template <class AccumType> void StatisticsUtilities<AccumType>::accumulateSym (
	Double& npts, AccumType& nvariance,
	AccumType& sumsq, const AccumType& datum, const AccumType& center
) {
	_NQUADSYM
}

template <class AccumType> void StatisticsUtilities<AccumType>::waccumulateSym (
	Double& npts, AccumType& sumweights, AccumType& wnvariance, AccumType& wsumsq,
	const AccumType& datum, const AccumType& weight, const AccumType& center
) {
	_WQUADSYM
}

template <class AccumType> template <class LocationType>
void StatisticsUtilities<AccumType>::accumulateSym (
	Double& npts, AccumType& nvariance,
	AccumType& sumsq, AccumType& datamin,
	AccumType& datamax, LocationType& minpos, LocationType& maxpos,
	const AccumType& datum, const LocationType& location, const AccumType& center
) {
	_NQUADSYM
	_MAXMINSYM
}

template <class AccumType> template <class LocationType>
void StatisticsUtilities<AccumType>::waccumulateSym (
	Double& npts, AccumType& sumweights, AccumType& wnvariance,
	AccumType& wsumsq, AccumType& datamin, AccumType& datamax,
	LocationType& minpos, LocationType& maxpos,
	const AccumType& datum, const AccumType& weight, const LocationType& location,
	const AccumType& center
) {
	_WQUADSYM
	_MAXMINSYM
}

template <class AccumType>
Bool StatisticsUtilities<AccumType>::includeDatum(
    const AccumType& datum, typename DataRanges::const_iterator beginRange,
    typename DataRanges::const_iterator endRange, Bool isInclude
) {
    typename DataRanges::const_iterator riter = beginRange;
    while (riter != endRange) {
        if (datum >= (*riter).first && datum <= (*riter).second) {
            return isInclude;
        }
        ++riter;
    }
    return ! isInclude;
}

}


#endif
