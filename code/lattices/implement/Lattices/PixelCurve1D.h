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

#if !defined(AIPS_PIXELCURVE1D_H)
#define AIPS_PIXELCURVE1D_H


//# Includes
#include <aips/Functionals/Function1D.h>
#include <aips/Arrays/Vector.h>

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
//  Sinusoid1D<Float> fn;
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
  // Define a straight line from (x1,y1) to (x2,y2) with
  // the given number of points.
  explicit PixelCurve1D (Float x1=0, Float y1=0, Float x2=1, Float y2=1,
			 uInt npoints=2);

  // Define a curve with an arbitrary function from x1 to x2 with
  // the given number of points.
  PixelCurve1D (const Function1D<Float,Float>&,
		Float x1, Float x2, uInt npoints);

  // Define a curve from a polyline with the given points.
  // Both vectors have to be equally long and at least 2 long.
  // The X-points have to be in ascending order.
  // When pixel coordinates are asked (using getPixelCoord), it uses
  // linear interpolation between the points.
  // The argument <src>npoints</src> defines the number of points
  // (with regular steps) in which the curve is divided.
  PixelCurve1D (const Vector<Float>& x, const Vector<Float>& y,
		uInt npoints);

  PixelCurve1D (const PixelCurve1D& that);

  ~PixelCurve1D();

  PixelCurve1D& operator= (const PixelCurve1D& that);

  uInt npoints() const
    { return itsNpoints; }

  // Get the pixel coordinates in the original lattice for point start
  // till end with given step.
  void getPixelCoord (Vector<Float>& x, Vector<Float>& y,
		      uInt start, uInt end, uInt incr=1) const;

private:
  Function<Float,Float>* itsFunc;
  Float itsX1;
  Float itsX2;
  Float itsStep;
  uInt  itsNpoints;
  Vector<Float> itsX;
  Vector<Float> itsY;
  Vector<Float> itsSlope;
};


#endif
