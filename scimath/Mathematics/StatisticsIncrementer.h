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

#ifndef SCIMATH_STATISTICSINCREMENTER_H
#define SCIMATH_STATISTICSINCREMENTER_H

#include <casacore/casa/aips.h>

namespace casacore {

// Utility functions used for incrementing pointers in a data set used by the stats framework.

template <class InputIterator, class MaskIterator=const Bool*>  class StatisticsIncrementer {
public:

	~StatisticsIncrementer() {}

	//<group>
	// <src> loopCount is always incremented by one, independent of the values
	// of <src>dataStride</src> and <src>maskStride</src>
	inline static void increment(
		InputIterator& datum, Int64& loopCount, Bool unityStride, uInt dataStride
	);

	inline static void increment(
		InputIterator& datum, Int64& loopCount, InputIterator& weight,
		Bool unityStride, uInt dataStride
	);

	inline static void increment(
		InputIterator& datum, Int64& loopCount, MaskIterator& mask,
		Bool unityStride, uInt dataStride, uInt maskStride
	);

	inline static void increment(
		InputIterator& datum, Int64& loopCount,
		InputIterator& weight, MaskIterator& mask,
		Bool unityStride, uInt dataStride, uInt maskStride
	);
	// </group>

private:
	// Just static methods, disallow constructor
	StatisticsIncrementer() {}

};

}

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Mathematics/StatisticsIncrementer.tcc>
#endif

#endif
