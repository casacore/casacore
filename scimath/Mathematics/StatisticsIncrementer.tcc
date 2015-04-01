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

#ifndef SCIMATH_STATISTICSINCREMENTER_TCC
#define SCIMATH_STATISTICSINCREMENTER_TCC

#include <casacore/scimath/Mathematics/StatisticsIncrementer.h>

namespace casacore {

template <class InputIterator, class MaskIterator>
void StatisticsIncrementer<InputIterator, MaskIterator>::increment(
	InputIterator& datum, Int64& loopCount, Bool unityStride, uInt dataStride
) {
	if (unityStride) {
		++datum;
	}
	else {
		uInt c = 0;
		while (c < dataStride) {
			++datum;
			++c;
		}

	}
	++loopCount;
}

template <class InputIterator, class MaskIterator>
void StatisticsIncrementer<InputIterator, MaskIterator>::increment(
	InputIterator& datum, Int64& loopCount, InputIterator& weight,
	Bool unityStride, uInt dataStride
) {
	if (unityStride) {
		++datum;
		++weight;
	}
	else {
		uInt c = 0;
		while (c < dataStride) {
			++datum;
			++weight;
			++c;
		}
	}
	++loopCount;
}

template <class InputIterator, class MaskIterator>
void StatisticsIncrementer<InputIterator, MaskIterator>::increment(
	InputIterator& datum, Int64& loopCount, InputIterator& weight,
	MaskIterator& mask, Bool unityStride, uInt dataStride, uInt maskStride
) {
	if (unityStride) {
		++datum;
		++weight;
		++mask;
	}
	else if (dataStride == maskStride) {
		uInt c = 0;
		while (c < dataStride) {
			++datum;
			++weight;
			++mask;
			++c;
		}
	}
	else {
		// dataStride != maskStride
		uInt c = 0;
		while (c < dataStride) {
			++datum;
			++weight;
			++c;
		}
		c = 0;
		while (c < maskStride) {
			++mask;
			++c;
		}
	}
	++loopCount;
}

template <class InputIterator, class MaskIterator>
void StatisticsIncrementer<InputIterator, MaskIterator>::increment(
	InputIterator& datum, Int64& loopCount, MaskIterator& mask,
	Bool unityStride, uInt dataStride, uInt maskStride
) {
	if (unityStride) {
		++datum;
		++mask;
	}
	else if (dataStride == maskStride) {
		uInt c = 0;
		while (c < dataStride) {
			++datum;
			++mask;
			++c;
		}
	}
	else {
		// dataStride != maskStride
		uInt c = 0;
		while (c < dataStride) {
			++datum;
			++c;
		}
		c = 0;
		while (c < maskStride) {
			++mask;
			++c;
		}
	}
	++loopCount;
}

}


#endif
