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


#include <trial/Lattices/PixelCurve1D.h>
#include <aips/Functionals/Polynomial.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>


PixelCurve1D::PixelCurve1D (float x1, float y1, float x2, float y2,
			    uInt npoints)
: itsFunc    (0),
  itsX1      (x1),
  itsX2      (x2),
  itsStep    (0),
  itsNpoints (npoints)
{
  AlwaysAssert (npoints>0 && (x1<x2 || (npoints==1 && x1<=x2)), AipsError);
  float a=1;
  if (npoints > 1) {
    a = (y2-y1) / (x2-x1);
    itsStep = (x2-x1) / (npoints-1);
  }
  float b = y1 - a*x1;
  Polynomial<Float>* func = new Polynomial<Float>(1);
  itsFunc = func;
  func->setCoefficient (0, b);
  func->setCoefficient (1, a);
}

PixelCurve1D::PixelCurve1D (const Function1D<Float,Float>& func,
			    float x1, float x2, uInt npoints)
: itsFunc    (func.clone()),
  itsX1      (x1),
  itsX2      (x2),
  itsStep    (0),
  itsNpoints (npoints)
{
  AlwaysAssert (npoints>0 && (x1<x2 || (npoints==1 && x1<=x2)), AipsError);
  if (npoints > 1) {
    itsStep = (x2-x1) / (npoints-1);
  }
}

PixelCurve1D::PixelCurve1D (const Vector<Float>& x, const Vector<Float>& y,
			    uInt npoints)
: itsFunc    (0),
  itsStep    (0),
  itsNpoints (npoints),
  itsX       (x),
  itsY       (y),
  itsSlope   (x.nelements())
{
  AlwaysAssert (npoints >= 2, AipsError);
  AlwaysAssert (x.nelements() >= 2, AipsError);
  AlwaysAssert (x.nelements() == y.nelements(), AipsError);
  uInt nr = x.nelements();
  for (uInt i=1; i<nr; i++) {
    AlwaysAssert (x[i] > x[i-1], AipsError);
    itsSlope[i] = (y[i] - y[i-1]) / (x[i] - x[i-1]);
  }
  itsX1 = x[0];
  itsX2 = x[nr-1];
  itsStep = (itsX2-itsX1) / (npoints-1);
}

PixelCurve1D::PixelCurve1D (const PixelCurve1D& that)
: itsFunc (0)
{
  operator= (that);
}
  
PixelCurve1D& PixelCurve1D::operator= (const PixelCurve1D& that)
{
  if (this != &that) {
    delete itsFunc;
    itsFunc = 0;
    if (that.itsFunc) {
      itsFunc  = that.itsFunc->clone();
    }
    itsX1      = that.itsX1;
    itsX2      = that.itsX2;
    itsStep    = that.itsStep;
    itsNpoints = that.itsNpoints;
    itsX.resize     (0);
    itsY.resize     (0);
    itsSlope.resize (0);
    itsX     = that.itsX;
    itsY     = that.itsY;
    itsSlope = that.itsSlope;
  }
  return *this;
}
  
PixelCurve1D::~PixelCurve1D()
{
  delete itsFunc;
}

void PixelCurve1D::getPixelCoord (Vector<Float>& x, Vector<Float>& y,
				  uInt start, uInt end, uInt incr) const
{
  AlwaysAssert (start<=end && end<itsNpoints, AipsError);
  uInt nr = 1 + (end-start) / incr;
  x.resize (nr);
  y.resize (nr);
  Float valx = itsX1 + start*itsStep;
  Float stepx = incr*itsStep;
  if (itsFunc) {
    for (uInt i=0; i<nr; i++) {
      x(i) = valx;
      y(i) = (*itsFunc)(valx);
      valx += stepx;
    }
  } else {
    uInt inx=0;
    uInt i=0;
    while (valx >= itsX[inx]) inx++;
    while (i < nr  &&  inx < itsX.nelements()) {
      Float valy = itsY[inx-1] + (valx - itsX[inx-1]) * itsSlope[inx];
      Float stepy = stepx * itsSlope[inx];
      while (i < nr  &&  valx < itsX[inx]) {
	x(i)   = valx;
	y(i++) = valy;
	valx += stepx;
	valy += stepy;
      }
      inx++;
    }
    // Due to rounding it might be possible that the last element is
    // not found. So take care of that.
    if (i < nr) {
      AlwaysAssert (i == nr-1, AipsError);
      x(i) = itsX[itsX.nelements()-1];
      y(i) = itsY[itsY.nelements()-1];
    }
  }
}  
