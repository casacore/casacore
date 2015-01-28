//# tLCPolygon.cc: Test program for LCPolygon class
//# Copyright (C) 2011
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
//# $Id$

#include <casacore/lattices/LRegions/LCPolygon.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>

void doIt (const IPosition& latticeShape,
	   const Vector<Float>& x,
	   const Vector<Float>& y)
{
  LCPolygon polygon (x, y, latticeShape);
  Array<Bool> mask(polygon.maskArray());
  //cout << mask(IPosition(2,498,498), IPosition(2,525,525));
  cout << mask;
}


int main()
{
  try {
    {
      // A circle like polygon.
      Float x[] = {523.974, 523.933, 523.809, 523.604, 523.317, 522.953, 522.515, 522.002, 521.422, 520.776, 520.068, 519.306, 518.493, 517.635, 516.738, 515.809, 514.852, 513.877, 512.889, 511.893, 510.901, 509.913, 508.941, 507.991, 507.068, 506.179, 505.329, 504.527, 503.775, 503.082, 502.448, 501.882, 501.386, 500.962, 500.614, 500.347, 500.157, 500.052, 500.028, 500.087, 500.229, 500.452, 500.754, 501.133, 501.59, 502.117, 502.711, 503.372, 504.09, 504.864, 505.687, 506.554, 507.457, 508.394, 509.355, 510.333, 511.325, 512.319, 513.313, 514.297, 515.264, 516.209, 517.127, 518.007, 518.847, 519.638, 520.378, 521.06, 521.677, 522.231, 522.712, 523.12, 523.449, 523.7, 523.871, 523.96};
      Float y[] = {512.001, 512.997, 513.985, 514.963, 515.918, 516.846, 517.741, 518.595, 519.406, 520.165, 520.866, 521.508, 522.083, 522.589, 523.02, 523.377, 523.655, 523.851, 523.966, 523.999, 523.95, 523.816, 523.601, 523.306, 522.935, 522.485, 521.966, 521.376, 520.722, 520.006, 519.237, 518.418, 517.554, 516.65, 515.716, 514.755, 513.775, 512.785, 511.787, 510.79, 509.804, 508.831, 507.882, 506.96, 506.073, 505.227, 504.427, 503.68, 502.991, 502.364, 501.803, 501.312, 500.898, 500.557, 500.298, 500.116, 500.019, 500.005, 500.073, 500.223, 500.454, 500.766, 501.156, 501.62, 502.156, 502.758, 503.427, 504.153, 504.934, 505.764, 506.638, 507.548, 508.488, 509.454, 510.435, 511.43};
      IPosition shape(2, 1024,1024);
      Vector<Float> xv(IPosition(1,sizeof(x)/sizeof(Float)), x, SHARE);
      Vector<Float> yv(IPosition(1,sizeof(y)/sizeof(Float)), y, SHARE);
      doIt (shape, xv, yv);
    }
    {
      // The letter E.
      Float x[] = {1,11,11,4, 4,11,11, 4, 4,11,11, 1};
      Float y[] = {1, 1, 4,4,10,10,13,13,19,19,22,22};
      IPosition shape(2, 1024,1024);
      Vector<Float> xv(IPosition(1,sizeof(x)/sizeof(Float)), x, SHARE);
      Vector<Float> yv(IPosition(1,sizeof(y)/sizeof(Float)), y, SHARE);
      doIt (shape, xv, yv);
    }
    {
      // A diabolo.
      Float x[] = {1, 5, 1, 5};
      Float y[] = {1, 5, 5, 1};
      IPosition shape(2, 1024,1024);
      Vector<Float> xv(IPosition(1,sizeof(x)/sizeof(Float)), x, SHARE);
      Vector<Float> yv(IPosition(1,sizeof(y)/sizeof(Float)), y, SHARE);
      doIt (shape, xv, yv);
    }
  } catch (AipsError x) {
    cout << "Caught exception: " << x.getMesg() << endl;
    return 1;
  } 
  cout << "OK" << endl;
  return 0;
}
