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
		float x1, float x2, uInt npoints);

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
};


#endif
