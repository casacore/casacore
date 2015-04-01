//# tLELSpectralIndex.cc: Test program for class LELSpectralIndex
//# Copyright (C) 2001
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

#include <casacore/images/Images/PagedImage.h>
#include <casacore/coordinates/Coordinates/CoordinateSystem.h>
#include <casacore/coordinates/Coordinates/CoordinateUtil.h>
#include <casacore/coordinates/Coordinates/SpectralCoordinate.h>
#include <casacore/lattices/LEL/LatticeExpr.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Cube.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>



#include <casacore/casa/namespace.h>
Bool doIt()
{
  Bool ok = True;
  IPosition shp1(2,10,10);
  IPosition shp1a(3,10,10,1);
  IPosition shp2(3,10,10,15);
  Double freq, freql, freqstep, scafreq;
  Array<Float> arr1(shp1);
  Array<Float> arr2(shp2);
  Array<Float> arr1a(shp2);
  Array<Float> arr1b(shp1a);
  indgen(arr1, Float(1));
  indgen(arr1b, Float(1));
  indgen(arr2, Float(10));
  for (Int i=0; i<shp2(2); i++) {
    Array<Float> sub = arr1a(IPosition(3,0,0,i), IPosition(3,9,9,i));
    sub = arr1b;
  }
  {
    CoordinateSystem cSys1 = CoordinateUtil::defaultCoords3D();
    const SpectralCoordinate& scrd = cSys1.spectralCoordinate (1);
    AlwaysAssertExit (scrd.toWorld (freq, 0.));
    AlwaysAssertExit (scrd.toWorld (freql, Float(shp2(2)-1)));
    freqstep = (freql - freq) / Float(shp2(2)-1);
    scafreq = freq + freqstep * Double(shp2(2)) / 2;
    // Remove the pixel axis (and replace by middle element).
    cSys1.removePixelAxis (2, Double(shp2(2)) / 2);
    CoordinateSystem cSys2 = CoordinateUtil::defaultCoords3D();
    CoordinateSystem cSys3;
    CoordinateUtil::addFreqAxis(cSys3);
    CoordinateUtil::addDirAxes(cSys3);
    cSys3.removePixelAxis (0, Double(shp2(2)) / 2 + 1);
    PagedImage<Float> pa1(shp1, cSys1, "tLELSpectralIndex_tmp.pa1");
    PagedImage<Float> pa1a(shp1a, cSys2, "tLELSpectralIndex_tmp.pa1a");
    PagedImage<Float> pa2(TiledShape(shp2, IPosition(3,4,3,6)),
				     cSys2, "tLELSpectralIndex_tmp.pa2");
    PagedImage<Float> pa3(shp1, cSys3, "tLELSpectralIndex_tmp.pa3");
    pa1.put (arr1);
    pa1a.put (arr1b);
    pa2.put (arr2);
    pa3.put (arr1 + Float(100));
  }
  {
    PagedImage<Float> pa1("tLELSpectralIndex_tmp.pa1");
    PagedImage<Float> pa2("tLELSpectralIndex_tmp.pa2");
    LatticeExpr<Float> expr = spectralindex(pa1,pa2);
    Array<Float> result = expr.get();
    Cube<Float> arrf(shp2);
    for (Int i=0; i<shp2(0); i++) {
      for (Int j=0; j<shp2(1); j++) {
	for (Int k=0; k<shp2(2); k++) {
	  arrf(i,j,k) = log (scafreq / (freq + k*freqstep));
	}
      }
    }
    Array<Float> expect = log(arr1a / arr2) / arrf;
    if (! allNear (result, expect, 1e-5)) {
      cout << expect << endl;
      cout << result << endl;
      ok = False;
    }
  }
  {
    PagedImage<Float> pa1("tLELSpectralIndex_tmp.pa1a");
    PagedImage<Float> pa2("tLELSpectralIndex_tmp.pa2");
    LatticeExpr<Float> expr = spectralindex(pa1,pa2);
    Array<Float> result = expr.get();
    Cube<Float> arrf(shp2);
    for (Int i=0; i<shp2(0); i++) {
      for (Int j=0; j<shp2(1); j++) {
	arrf(i,j,0) = 1;
	for (Int k=1; k<shp2(2); k++) {
	  arrf(i,j,k) = log (freq / (freq + k*freqstep));
	}
      }
    }
    Array<Float> expect = log(arr1a / arr2) / arrf;
    Array<Float> subarr = expect(IPosition(3, 0, 0, 0),
				 IPosition(3, shp2(0)-1, shp2(1)-1, 0));
    subarr = 0;
    if (! allNear (result, expect, 1e-5)) {
      cout << expect << endl;
      cout << result << endl;
      ok = False;
    }
  }
  {
    PagedImage<Float> pa3("tLELSpectralIndex_tmp.pa3");
    PagedImage<Float> pa1("tLELSpectralIndex_tmp.pa1");
    LatticeExpr<Float> expr = spectralindex(pa1,pa3);
    Array<Float> result = expr.get();
    Matrix<Float> arrf(shp1);
    for (Int i=0; i<shp1(0); i++) {
      for (Int j=0; j<shp1(1); j++) {
	arrf(i,j) = log (scafreq / (scafreq+freqstep));
      }
    }
    Array<Float> expect = log(arr1 / (arr1+Float(100))) / arrf;
    if (! allNear (result, expect, 1e-5)) {
      cout << expect << endl;
      cout << result << endl;
      ok = False;
    }
  }
  {
    PagedImage<Float> pa3("tLELSpectralIndex_tmp.pa3");
    PagedImage<Float> pa2("tLELSpectralIndex_tmp.pa2");
    LatticeExpr<Float> expr = spectralindex(pa2,pa3);
    Array<Float> result = expr.get();
    Cube<Float> arrf(shp2);
    for (Int i=0; i<shp2(0); i++) {
      for (Int j=0; j<shp2(1); j++) {
	for (Int k=0; k<shp2(2); k++) {
	  arrf(i,j,k) = log ((freq + k*freqstep) / (scafreq+freqstep));
	}
      }
    }
    Array<Float> expect = log(arr2 / (arr1a+Float(100))) / arrf;
    if (! allNear (result, expect, 1e-5)) {
      cout << expect << endl;
      cout << result << endl;
      ok = False;
    }
  }
  return ok;
}


int main ()
{
  Bool ok = True;
  try {
    ok = doIt();
  } catch (AipsError x) {
    cerr << "Caught exception: " << x.getMesg() << endl;
    ok = False;
  } 
  if (!ok) {
    cout << "FAIL" << endl;
    return 1;
  }
  cout << "OK" << endl;
  return 0;
}
