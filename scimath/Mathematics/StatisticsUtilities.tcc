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

#include <casacore/scimath/Mathematics/StatisticsUtilities.h>

#include <iostream>

namespace casacore {

// For performance reasons, we prefer repeating code rather than
// calling other functions which do the same thing. The performance
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
	T prevMean = mean; \
	_NLINEAR \
	nvariance += (datum - prevMean)*(datum - mean);

#define _WQUAD \
	wsumsq += weight*datum*datum; \
	T prevMean = wmean; \
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

template <class T> void StatisticsUtilities<T>::accumulate (
	Double& npts, T& sum, T& mean, const T& datum
) {
	_NLINEAR
}

template <class T> void StatisticsUtilities<T>::waccumulate (
	Double& npts, T& sumweights, T& wsum, T& wmean,
	const T& datum, const T& weight
) {
	_WLINEAR
}

template <class T> void StatisticsUtilities<T>::accumulate (
	Double& npts, T& sum, T& mean, T& nvariance,
	T& sumsq, const T& datum
) {
	_NQUAD
}

template <class T> void StatisticsUtilities<T>::waccumulate (
	Double& npts, T& sumweights, T& wsum,
	T& wmean, T& wnvariance, T& wsumsq,
	const T& datum, const T& weight
) {
	_WQUAD
}

template <class T> template <class LocationType>
void StatisticsUtilities<T>::accumulate (
	Double& npts, T& sum, T& mean, T& nvariance,
	T& sumsq, T& datamin,
	T& datamax, LocationType& minpos, LocationType& maxpos,
	const T& datum, const LocationType& location
) {
	_NQUAD
	_MAXMIN
}

template <class T> template <class LocationType>
void StatisticsUtilities<T>::waccumulate (
	Double& npts, T& sumweights, T& wsum, T& wmean, T& wnvariance,
	T& wsumsq, T& datamin, T& datamax,
	LocationType& minpos, LocationType& maxpos,
	const T& datum, const T& weight, const LocationType& location
) {
	_WQUAD
	_MAXMIN
}

template <class T> template <class LocationType>
Bool StatisticsUtilities<T>::doMax(
	T& datamax, LocationType& maxpos, Bool isFirst,
	const T& datum, const LocationType& location
) {
	if (isFirst || datum > datamax) {
		datamax = datum;
		maxpos = location;
		return True;
	}
	return False;
}

template <class T> template <class LocationType>
Bool StatisticsUtilities<T>::doMin(
	T& datamin, LocationType& minpos, Bool isFirst,
	const T& datum, const LocationType& location
) {
	if (isFirst || datum < datamin) {
		datamin = datum;
		minpos = location;
		return True;
	}
	return False;
}

}

