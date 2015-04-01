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

#include <casacore/scimath/Mathematics/ZScoreCalculator.h>
#include <stdlib.h>

#include <casacore/casa/BasicMath/Math.h>

namespace casacore {

std::map<uInt64, Double> ZScoreCalculator::_nptsToMaxZScore;

Double ZScoreCalculator::getMaxZScore(uInt64 npts) {
	if (_nptsToMaxZScore.empty()) {
		// initialize the map
		_nptsToMaxZScore[0] = 0.5;
		_nptsToMaxZScore[1] = 1;
		_nptsToMaxZScore[3] = 1.5;
		_nptsToMaxZScore[10] = 2;
		_nptsToMaxZScore[40] = 2.5;
		_nptsToMaxZScore[185] = 3;
		_nptsToMaxZScore[1074] = 3.5;
		_nptsToMaxZScore[7893] = 4;
		_nptsToMaxZScore[73579] = 4.5;
		_nptsToMaxZScore[872138] = 5;
		_nptsToMaxZScore[13165126] = 5.5;
		_nptsToMaxZScore[253398672] = 6;
		_nptsToMaxZScore[6225098696ULL] = 6.5;
		_nptsToMaxZScore[195341107722ULL] = 7;
	}
	if (_nptsToMaxZScore.find(npts) != _nptsToMaxZScore.end()) {
		return _nptsToMaxZScore[npts];
	}
	std::map<uInt64, Double>::const_iterator lowiter = _nptsToMaxZScore.begin();
	std::map<uInt64, Double>::const_iterator upiter = _nptsToMaxZScore.begin();
	++upiter;
	if (npts > _nptsToMaxZScore.rbegin()->first) {
		Double zscoreMax = _nptsToMaxZScore.rbegin()->second;
		Double z = zscoreMax + 0.5;
		while (True) {
			uInt64 nptsmin = zscoreToNpts(z);
			if (nptsmin >= npts) {
				_nptsToMaxZScore[nptsmin] = z;
				if (nptsmin == npts) {
					return z;
				}
				else {
					uInt increment = _nptsToMaxZScore.size() - 2;
					advance(lowiter, increment);
					advance(upiter, increment);
					break;
				}
			}
			z += 0.5;
		}
	}
	else {
		Int distance = _nptsToMaxZScore.size()/2;
		while (True) {
			advance(lowiter, distance);
			advance(upiter, distance);
			if (lowiter->first < npts && upiter->first > npts) {
				break;
			}
			distance /= 2;
			if (distance == 0) {
				distance = 1;
			}
			distance = lowiter->first > npts
				? -std::abs(distance)
				: std::abs(distance);
		}
	}
	Double lz = lowiter->second;
	Double uz = upiter->second;
	Double z = (lz + uz)/2;
	while (True) {
		uInt64 nptsmin = zscoreToNpts(z);
		if (_nptsToMaxZScore.size() < 1000000) {
			_nptsToMaxZScore[nptsmin] = z;
		}
		if (nptsmin == npts || near(lz, uz)) {
			return z;
		}
		if (nptsmin > npts) {
			uz = z;
		}
		else {
			lz = z;
		}
		z = (lz + uz)/2;
	}
}

}
