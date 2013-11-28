//# Copyright (C) 2010 by ESO (in the framework of the ALMA collaboration)
//# Copyright (C) 1995,1996,1997,1998,1999,2000,2001,2002
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
//# $Id: Combinatorics.cc 21100 2011-06-28 12:49:00Z gervandiepen $
//   

#include <scimath/Mathematics/Geometry.h>

#include <casa/Arrays/MatrixMath.h>

#include <utility>

namespace casa {

	std::pair<Double, Double> Geometry::rotate2D(
		Double x, Double y, const Quantity& theta
	) {
		Double thetaRad = theta.getValue("rad");
		Double c = cos(thetaRad);
		Double s = sin(thetaRad);
		return std::make_pair(x*c - y*s, x*s + y*c);
	}


	Bool Geometry::doLineSegmentsIntersect(
		Double a0x, Double a0y, Double a1x, Double a1y,
		Double b0x, Double b0y, Double b1x, Double b1y
	) {
		Vector<Double > line0point0(2);
		line0point0[0] = a0x;
		line0point0[1] = a0y;
		Vector<Double > line0point1(2);
		line0point1[0] = a1x;
		line0point1[1] = a1y;
		Vector<Double > line1point0(2);
		line1point0[0] = b0x;
		line1point0[1] = b0y;
		Vector<Double > line1point1(2);
		line1point1[0] = b1x;
		line1point1[1] = b1y;

		Vector<Double> p = line0point0;
		Vector<Double> r = line0point1 - line0point0;
		Vector<Double> q = line1point0;
		Vector<Double> s = line1point1 - line1point0;
		Double rCrossS = crossProduct2D(r, s);
		Vector<Double> diffQP = q-p;

		if (rCrossS == 0) {
			if (crossProduct2D(diffQP, r) == 0) {
				// lines are coincident
				return True;
			}
			else {
				// lines are parallel
				return False;
			}
		}
		Double t = crossProduct2D(diffQP, s)/rCrossS;
		Double u = crossProduct2D(diffQP, r)/rCrossS;
		return t >= 0 && t <= 1 && u >= 0 && u <= 1;
	}



} //# NAMESPACE CASA - END

