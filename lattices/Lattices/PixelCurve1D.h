//# PixelCurve1D.h: Arbitrary 1-dim curve in a lattice plane
//# Copyright (C) 2003
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
//# You should have receied a copy of the GNU Library General Public License
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
//# $Id$

#ifndef LATTICES_PIXELCURVE1D_H
#define LATTICES_PIXELCURVE1D_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/scimath/Functionals/Function1D.h>
#include <casacore/casa/Arrays/Vector.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations


// <summary>
// Arbitrary 1-dim curve in a lattice plane.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tPixelCurve1D.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=Function1D>Function1D</linkto>
//   <li> <linkto class=CurvedLattice2D>CurvedLattice2D</linkto>
// </prerequisite>

// <synopsis>
// PixelCurve1D represents a 1-dim curve in a lattice plane to be
// used by CurvedLattice2D.
// The curve can be any function supported in the
// <linkto module=Functionals>Functionals</linkto> module.
// <br>A special constructor exists to define a straight line.
// <br>Another special constructor exists for a polyline.
//
// The domain for which the curve is valid is given by the interval
// [x1,x2]. The granularity of the domain is given by the number of
// points. The number of points also define the length of the new axis
// in the CurvedLattice2D object.
// </synopsis>

// <example>
// <srcblock>
//  // Use function y=cos(2*pi*x) on the interval [0,2] with 5 points.
//  Sinusoid1D<float> fn;
//  PixelCurve1D pcurve2(fn, 0., 2., 5);
//  AlwaysAssertExit (pcurve2.npoints() == 5);
//  pcurve2.getPixelCoord (x, y, 0, 4);
//  cout << x << y << endl;
// </srcblock>
// The result of x is [0, 0.5, 1, 1.5, 2].
// The result of y is [1, -1, 1, -1, 1]
// </example>

// <motivation>
// The viewer must be able to show a crosscut through an image using
// an arbitrary curve.
// </motivation>

// <todo asof=2003/10/23>
//  <li> Maybe it is better to make itsNpoints part of CurvedLattice
//  <li> If itsNpoint is still part of this class, it is possible to
//       precompute all possible Y values in the constructors. This may
//       speed things up for the polyline case.
// </todo>

class PixelCurve1D
{
public:
  // Define a straight line from (x1,y1) to (x2,y2).
  // The default number of points is the length of the line.
  explicit PixelCurve1D (double x1=0, double y1=0, double x2=1, double y2=1,
			 uInt npoints=0);

  // Define a curve with an arbitrary function from x1 to x2.
  // The default number of points is the length of the curve.
  // The length of the curve is determined numerically by integration
  // of sqrt(1+sqr(df/dx)).
  PixelCurve1D (const Function1D<float,float>&,
		float x1, float x2, uInt npoints=0);

  // Define a curve from a polyline with the given points.
  // Both vectors have to be equally long and at least 2 long.
  // The argument <src>npoints</src> defines the number of points
  // (with regular steps) in which the curve is divided.
  // The default is the length of the polyline.
  PixelCurve1D (const Vector<Int>& x, const Vector<Int>& y,
		uInt npoints=0);
  PixelCurve1D (const Vector<float>& x, const Vector<float>& y,
		uInt npoints=0);
  PixelCurve1D (const Vector<double>& x, const Vector<double>& y,
		uInt npoints=0);

  PixelCurve1D (const PixelCurve1D& that);

  ~PixelCurve1D();

  PixelCurve1D& operator= (const PixelCurve1D& that);

  uInt npoints() const
    { return itsNpoints; }

  // Get the pixel coordinates in the original lattice for point start
  // till end with given step.
  void getPixelCoord (Vector<float>& x, Vector<float>& y,
		      uInt start, uInt end, uInt incr=1) const;

private:
  // Initialize the object.
  void init (const Vector<double>& x, const Vector<double>& y, uInt npoints);

  uInt           itsNpoints;
  Vector<double> itsX;
  Vector<double> itsY;
};



} //# NAMESPACE CASACORE - END

#endif
