//# PixelCurve1D.cc: Arbitrary 1-dim curve in a lattice plane
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


#include <casacore/lattices/Lattices/PixelCurve1D.h>
#include <casacore/scimath/Functionals/Polynomial.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

PixelCurve1D::PixelCurve1D (double x1, double y1, double x2, double y2,
			    uInt npoints)
{
  Vector<double> x(2), y(2);
  x(0) = x1;
  x(1) = x2;
  y(0) = y1;
  y(1) = y2;
  init (x, y, npoints);
}

PixelCurve1D::PixelCurve1D (const Function1D<float,float>& func,
			    float x1, float x2, uInt npoints)
{
  // Calculate the length of the curve numerically.
  // Analytically it is the integral of (sqrt(1 + sqr(df/dx)).
  // Use 1000 times the number of pixels in x or y for the numeric calculation.
  uInt np = uInt(1000 * max(abs(x2-x1), abs(func(x2) - func(x1))));
  Vector<double> x(np), y(np);
  double step = (double(x2)-x1) / (np-1);
  for (uInt i=0; i<np; i++) {
    x[i] = x1;
    y[i] = func(x1);
    x1 += step;
  }
  init (x, y, npoints);
}

PixelCurve1D::PixelCurve1D (const Vector<Int>& x, const Vector<Int>& y,
			    uInt npoints)
{
  Vector<double> xd(x.nelements());
  convertArray (xd, x);
  Vector<double> yd(y.nelements());
  convertArray (yd, y);
  init (xd, yd, npoints);
}

PixelCurve1D::PixelCurve1D (const Vector<float>& x, const Vector<float>& y,
			    uInt npoints)
{
  Vector<double> xd(x.nelements());
  convertArray (xd, x);
  Vector<double> yd(y.nelements());
  convertArray (yd, y);
  init (xd, yd, npoints);
}

PixelCurve1D::PixelCurve1D (const Vector<double>& x, const Vector<double>& y,
			    uInt npoints)
{
  init (x, y, npoints);
}

PixelCurve1D::PixelCurve1D (const PixelCurve1D& that)
{
  operator= (that);
}
  
PixelCurve1D& PixelCurve1D::operator= (const PixelCurve1D& that)
{
  if (this != &that) {
    itsNpoints = that.itsNpoints;
    itsX.resize (0);
    itsY.resize (0);
    itsX = that.itsX;
    itsY = that.itsY;
  }
  return *this;
}
  
PixelCurve1D::~PixelCurve1D()
{}

void PixelCurve1D::init (const Vector<double>& x, const Vector<double>& y,
			 uInt npoints)
{
  AlwaysAssert (x.nelements() == y.nelements(), AipsError);
  AlwaysAssert (x.nelements() >= 2, AipsError);
  uInt nr = x.nelements() - 1;
  // Calculate the total length of the curve.
  // Also calculate the scaling in x and y for each line segment.
  Vector<double> leng(nr);
  Vector<double> scx(nr);
  Vector<double> scy(nr);
  double totleng = 0;
  for (uInt i=0; i<nr; i++) {
    double dx = x[i+1] - x[i];
    double dy = y[i+1] - y[i];
    double lng = sqrt(dx*dx + dy*dy);
    leng[i] = lng;
    scx[i] = dx/lng;
    scy[i] = dy/lng;
    totleng += lng;
  }
  // Set nr of points if not defined.
  if (npoints == 0) {
    npoints = 1 + uInt(totleng + 0.1);
  }
  itsNpoints = npoints;
  // Get step length along the curve.
  double step = totleng / (npoints-1);
  // Now calculate the X and Y value for each point along the curve.
  itsX.resize (npoints);
  itsY.resize (npoints);
  uInt np=0;
  // Step through all line segments. Calculate the points on each segment.
  // Keep track of the remaining length.
  double rem = 0;
  for (uInt i=0; i<nr; i++) {
    double lng = leng[i];
    if (rem < lng) {
      double dx = rem*scx[i];
      double dy = rem*scy[i];
      itsX[np] = x[i] + dx;
      itsY[np] = y[i] + dy;
      np++;
      dx = step*scx[i];
      dy = step*scy[i];
      double ln = rem+step;
      while (ln <= lng) {
	itsX[np] = itsX[np-1] + dx;
	itsY[np] = itsY[np-1] + dy;
	np++;
	ln += step;
      }
      rem = ln - lng;
    } else {
      rem -= lng;
    }
  }
  if (np < npoints) {
    itsX[np] = x[nr];
    itsY[np] = y[nr];
    np++;
  }
  AlwaysAssert (np == npoints, AipsError);
}

void PixelCurve1D::getPixelCoord (Vector<float>& x, Vector<float>& y,
				  uInt start, uInt end, uInt incr) const
{
  AlwaysAssert (start<=end && end<itsNpoints, AipsError);
  uInt nr = 1 + (end-start) / incr;
  x.resize (nr);
  y.resize (nr);
  for (uInt i=0; i<nr; i++) {
    x[i] = itsX[start];
    y[i] = itsY[start];
    start += incr;
  }
}  

} //# NAMESPACE CASACORE - END

