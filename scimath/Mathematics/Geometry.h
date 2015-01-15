//# Copyright (C) 2010 by ESO (in the framework of the ALMA collaboration)
//# Copyright (C) 1996,1997,1998,1999,2000,2001
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
//# $Id: Combinatorics.h 21116 2011-07-21 11:23:15Z gervandiepen $

#ifndef SCIMATH_GEOMETRY_H
#define SCIMATH_GEOMETRY_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <utility>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Geometry related methods.
// </summary>

// <use visibility=export>

//# <author>Dave Mehringer</author>
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
// <etymology>
// self-explanatory
// </etymology>

// <synopsis>
// Various geometry related functions
// </synopsis>

// <motivation>
// Commonly used geometrical functions for various image and region
// applications.
// </motivation>

class Geometry {
  
public:
 
	// Get result of rotating a 2-D vector counterclockwise through an angle
	// <src>theta</src>. The output pair will have x as the first
	// member and y as the second. <src>theta</src> must have angular units (no explicit
	// checking is done, but an exception will be thrown from the Quanta code if not).
	static std::pair<Double, Double> rotate2D(
		Double x, Double y, const Quantity& theta
	);

	// Determine if two coplanar line segments, a and b, intersect. Line segment a
	// has end points a0 and a1, and line segment b has endpoints b0 and b1.
	// Algorithm from
	// http://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect
	static Bool doLineSegmentsIntersect(
		Double a0x, Double a0y, Double a1x, Double a1y,
		Double b0x, Double b0y, Double b1x, Double b1y
	);

};
} //# NAMESPACE CASACORE - END

#endif

