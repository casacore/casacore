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


#include <trial/Lattices/PixelCurve1D.h>
#include <aips/Arrays/ArrayIO.h>
#include <aips/Utilities/Assert.h>
#include <aips/Functionals/Sinusoid1D.h>


int main()
{
  Vector<Float> x,y;

  // Construct polynomial.
  PixelCurve1D pcurve(1.0, 2.0, 5.0, 6.0, 9);
  AlwaysAssertExit (pcurve.npoints() == 9);
  pcurve.getPixelCoord (x, y, 0, 8);
  cout << x << y << endl;
  pcurve.getPixelCoord (x, y, 0, 8, 2);
  cout << x << y << endl;
  pcurve.getPixelCoord (x, y, 1, 8, 3);
  cout << x << y << endl;

  // Construct from an arbitrary function.
  Sinusoid1D<Float> fn;
  PixelCurve1D pcurve2(fn, 0., 2., 5);
  AlwaysAssertExit (pcurve2.npoints() == 5);
  pcurve2.getPixelCoord (x, y, 0, 4);
  cout << x << y << endl;

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

  // Construct from a polyline.
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
