//# tPixelCurve1D.cc: Test program for class PixelCurve1D
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
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/scimath/Functionals/Sinusoid1D.h>
#include <casacore/scimath/Functionals/Polynomial.h>
#include <casacore/casa/iomanip.h>

#include <casacore/casa/namespace.h>

int main()
{
  Vector<Float> x,y;

  // Construct from straight line.
  PixelCurve1D pcurve(1.0, 2.0, 5.0, 6.0, 9);
  AlwaysAssertExit (pcurve.npoints() == 9);
  pcurve.getPixelCoord (x, y, 0, 8);
  cout << x << y << endl;
  pcurve.getPixelCoord (x, y, 0, 8, 2);
  cout << x << y << endl;
  pcurve.getPixelCoord (x, y, 1, 8, 3);
  cout << x << y << endl;

  // The same, but let class determine #points.
  {
    PixelCurve1D pcurve1(1.0, 2.0, 5.0, 6.0);
    AlwaysAssertExit (pcurve1.npoints() == 6);
    pcurve1.getPixelCoord (x, y, 0, 5);
    cout << x << y << endl;
    pcurve1.getPixelCoord (x, y, 0, 5, 2);
    cout << x << y << endl;
    pcurve1.getPixelCoord (x, y, 1, 5, 3);
    cout << x << y << endl;
  }
  // The same, but using a polynomial.
  {
    Polynomial<Float> func(1);
    func.setCoefficient (0, 1.);
    func.setCoefficient (1, 1.);
    PixelCurve1D pcurve1(func, 1., 5.);
    AlwaysAssertExit (pcurve1.npoints() == 6);
    pcurve1.getPixelCoord (x, y, 0, 5);
    cout << x << y << endl;
    float dx = 1;
    float dy = 2;
    for (uInt i=0; i<5; i++) {
      AlwaysAssertExit (near(x[i], dx, 0.00001));
      AlwaysAssertExit (near(y[i], dy, 0.00001));
      dx += 0.8;
      dy += 0.8;
    }
  }

  // Construct from a cosine function.
  Sinusoid1D<Float> fn;
  PixelCurve1D pcurve2(fn, 0., 2., 5);
  {
    AlwaysAssertExit (pcurve2.npoints() == 5);
    pcurve2.getPixelCoord (x, y, 0, 4);
    cout << x << y << endl;
    double dx = x[1] - x[0];
    double dy = y[1] - y[0];
    double lng = sqrt(dx*dx + dy*dy);
    for (uInt i=1; i<5; i++) {
      double dx = x[i] - x[i-1];
      double dy = y[i] - y[i-1];
      AlwaysAssertExit (near(lng, sqrt(dx*dx + dy*dy), 1e-5));
    }
  }
  cout << setprecision(3);
  {
    PixelCurve1D pcurve2a(fn, 0., 2.);
    AlwaysAssertExit (pcurve2a.npoints() == 9);
    pcurve2a.getPixelCoord (x, y, 0, 8);
    cout << x << y << endl;
    double dx = x[1] - x[0];
    double dy = y[1] - y[0];
    double lng = sqrt(dx*dx + dy*dy);
    for (uInt i=1; i<9; i++) {
      double dx = x[i] - x[i-1];
      double dy = y[i] - y[i-1];
      AlwaysAssertExit (near(lng, sqrt(dx*dx + dy*dy), 1e-4));
    }
  }
  {
    PixelCurve1D pcurve2b(fn, 0., 2., 81);
    AlwaysAssertExit (pcurve2b.npoints() == 81);
    pcurve2b.getPixelCoord (x, y, 0, 80, 10);
    cout << x << y << endl;
    double dx = x[1] - x[0];
    double dy = y[1] - y[0];
    double lng = sqrt(dx*dx + dy*dy);
    for (uInt i=1; i<9; i++) {
      double dx = x[i] - x[i-1];
      double dy = y[i] - y[i-1];
      AlwaysAssertExit (near(lng, sqrt(dx*dx + dy*dy), 1e-4));
    }
  }
  cout << setprecision(6);

  // Copy constructor and self assignment.
  PixelCurve1D pcurve3(pcurve);
  AlwaysAssertExit (pcurve3.npoints() == 9);
  pcurve3 = pcurve3;
  AlwaysAssertExit (pcurve3.npoints() == 9);
  pcurve3.getPixelCoord (x, y, 0, 8);
  cout << x << y << endl;

  // Assignment.
  pcurve3 = pcurve2;
  AlwaysAssertExit (pcurve2.npoints() == 5);
  pcurve2.getPixelCoord (x, y, 0, 4);

  {
    // Construct from a very simple polyline.
    Vector<Float> xp(3);
    Vector<Float> yp(3);
    xp[0]=0; xp[1]=4; xp[2]=4;
    yp[0]=0; yp[1]=0; yp[2]=4;
    PixelCurve1D pcurve4(xp,yp);
    AlwaysAssertExit (pcurve4.npoints() == 9);
    pcurve4.getPixelCoord (x, y, 0, 8);
    cout << x << y << endl;
  }
  {
    // Construct from a square.
    Vector<Float> xp(5);
    Vector<Float> yp(5);
    xp[0]=2; xp[1]=4; xp[2]=2; xp[3]=0; xp[4]=2;
    yp[0]=0; yp[1]=2; yp[2]=4; yp[3]=2; yp[4]=0;
    PixelCurve1D pcurve4(xp,yp,9);
    AlwaysAssertExit (pcurve4.npoints() == 9);
    pcurve4.getPixelCoord (x, y, 0, 8);
    cout << x << y << endl;
  }
  {
    // Construct from another polyline.
    Vector<Float> xp(5);
    Vector<Float> yp(5);
    xp[0]=2; xp[1]=4; xp[2]=7; xp[3]=8; xp[4]=12;
    yp[0]=2; yp[1]=6; yp[2]=9; yp[3]=6; yp[4]=6;
    PixelCurve1D pcurve4(xp,yp,21);
    AlwaysAssertExit (pcurve4.npoints() == 21);
    pcurve4 = pcurve4;
    AlwaysAssertExit (pcurve4.npoints() == 21);
    pcurve4.getPixelCoord (x, y, 0, 20);
    cout << x << y << endl;
    pcurve3 = pcurve4;
    AlwaysAssertExit (pcurve3.npoints() == 21);
    pcurve3.getPixelCoord (x, y, 0, 20);
    cout << x << y << endl;
  }
}
