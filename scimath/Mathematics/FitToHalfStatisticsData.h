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

#ifndef SCIMATH_FITTOHALFSTATISTICSDATA_H
#define SCIMATH_FITTOHALFSTATISTICSDATA_H

#include <casacore/casa/aips.h>

namespace casacore {

// Various data for FitToHalfStatistics

class FitToHalfStatisticsData {
public:

	// choice of center point based on the corresponding statistics from the
	// entire distribution of data, or simply an arbitrary value
	enum CENTER {
		CMEAN,
		CMEDIAN,
		CVALUE
	};

	// which section of data to use, greater than or less than the center value
	enum USE_DATA {
		LE_CENTER,
		GE_CENTER
	};
};

}

#endif
