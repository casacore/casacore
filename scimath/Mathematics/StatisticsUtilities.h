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

#ifndef SCIMATH_STATISTICSUTILITIES_H
#define SCIMATH_STATISTICSUTILITIES_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Utilities/DataType.h>

namespace casacore {

// Various statistics related methods for the statistics framework.

template <class T> class StatisticsUtilities {
public:

	~StatisticsUtilities() {}

	// <group>
	// accumulate values. It is the responsibility of the caller to keep track
	// of the accumulated values after each call. This class does not since it
	// has not state.
	// The accumulation derivation for mean and variance can be found at
	// www.itl.nist.gov/div898/software/dataplot/refman2/ch2/weighvar.pdf
	// nvariance is an accumulated value. It is related to the variance via
	// variance = nvariance/npts or nvariance/(npts-1) depending on your preferred definition
	// in the non-weighted case and
	// wvariance = wnvariance/sumofweights or wnvariance/(sumofweights-1) in the weighted case
	// It's basic definition is nvariance = sum((x_i - mean)**2),
	// wnvariance = sum((weight_i*(x_i - mean)**2)
	// npts is a Double rather than an Int64 because of compilation issues when T is a Complex
	static void accumulate (
		Double& npts, T& sum, T& mean, const T& datum
	);

	// in order to optimize performance, no checking is done for the weight == 0 case
	// callers should ensure that the weigth is not zero before calling this method,
	// and shouldn't call this method if the weight is 0. Expect a segfault because of
	// division by zero if sumweights and weight are both zero.
	static void waccumulate (
		Double& npts, T& sumweights, T& wsum, T& wmean,
		const T& datum, const T& weight
	);

	static void accumulate (
		Double& npts, T& sum, T& mean, T& nvariance,
		T& sumsq, const T& datum
	);

	// wsumsq is the weighted sum of squares, sum(w_i*x_i*x_i)
	static void waccumulate (
		Double& npts, T& sumweights, T& wsum,
		T& wmean, T& wnvariance, T& wsumsq,
		const T& datum, const T& weight
	);
	// </group>

	// <group>
	// The assignment operator of class LocationType should use copy, not reference,
	// semantics.
	template <class LocationType>
	static void accumulate (
		Double& npts, T& sum, T& mean, T& nvariance,
		T& sumsq, T& datamin,
		T& datamax, LocationType& minpos, LocationType& maxpos,
		const T& datum, const LocationType& location
	);

	template <class LocationType>
	static void waccumulate (
		Double& npts, T& sumofweights, T& sum, T& mean,
		T& nvariance, T& sumsq, T& datamin, T& datamax,
		LocationType& minpos, LocationType& maxpos,
		const T& datum, const T& weight, const LocationType& location
	);

	// <group>

	// <group>
	// return True if the max or min was updated, False otherwise.
	template <class LocationType>
	static Bool doMax(
		T& datamax, LocationType& maxpos, Bool isFirst,
		const T& datum, const LocationType& location
	);

	template <class LocationType>
	static Bool doMin(
		T& datamin, LocationType& minpos, Bool isFirst,
		const T& datum, const LocationType& location
	);
	// </group>

	// This does the obvious conversions. The Complex and DComplex versions
	// (implemented after the class definition) are used solely to permit compilation. In general, these versions should
	// never actually be called
	inline static Int getInt(const T& v) {
		return (Int)v;
	}
/*
	template <class U>
	inline static U convert(const T& v) {
		return (U)v;
	}
	*/

private:
	StatisticsUtilities() {}

};

// The Complex and DComplex versions
// are used solely to permit compilation. In general, these versions should
// never actually be called
template<>
inline Int StatisticsUtilities<Complex>::getInt(const Complex&) {
	ThrowCc("This version for complex data types should never be called");
}

template<>
inline Int StatisticsUtilities<DComplex>::getInt(const DComplex&) {
	ThrowCc("Logic Error: This version for complex data types should never be called");
}

/*
template<>
template <class T>
inline DComplex StatisticsUtilities<T>::convert<DComplex>(const T&) {
	ThrowCc("Logic Error: This version for complex data types should never be called");
}
*/


}

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Mathematics/StatisticsUtilities.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES

#endif
