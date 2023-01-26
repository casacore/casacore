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
#include <casacore/casa/IO/ArrayIO.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>



#include <casacore/casa/namespace.h>
bool doIt()
{
  bool ok = true;
  IPosition shp1(2,10,10);
  IPosition shp1a(3,10,10,1);
  IPosition shp2(3,10,10,15);
  double freq, freql, freqstep, scafreq;
  Array<float> arr1(shp1);
  Array<float> arr2(shp2);
  Array<float> arr1a(shp2);
  Array<float> arr1b(shp1a);
  indgen(arr1, float(1));
  indgen(arr1b, float(1));
  indgen(arr2, float(10));
  for (int32_t i=0; i<shp2(2); i++) {
    Array<float> sub = arr1a(IPosition(3,0,0,i), IPosition(3,9,9,i));
    sub = arr1b;
  }
  {
    CoordinateSystem cSys1 = CoordinateUtil::defaultCoords3D();
    const SpectralCoordinate& scrd = cSys1.spectralCoordinate (1);
    AlwaysAssertExit (scrd.toWorld (freq, 0.));
    AlwaysAssertExit (scrd.toWorld (freql, float(shp2(2)-1)));
    freqstep = (freql - freq) / float(shp2(2)-1);
    scafreq = freq + freqstep * double(shp2(2)) / 2;
    // Remove the pixel axis (and replace by middle element).
    cSys1.removePixelAxis (2, double(shp2(2)) / 2);
    CoordinateSystem cSys2 = CoordinateUtil::defaultCoords3D();
    CoordinateSystem cSys3;
    CoordinateUtil::addFreqAxis(cSys3);
    CoordinateUtil::addDirAxes(cSys3);
    cSys3.removePixelAxis (0, double(shp2(2)) / 2 + 1);
    PagedImage<float> pa1(shp1, cSys1, "tLELSpectralIndex_tmp.pa1");
    PagedImage<float> pa1a(shp1a, cSys2, "tLELSpectralIndex_tmp.pa1a");
    PagedImage<float> pa2(TiledShape(shp2, IPosition(3,4,3,6)),
				     cSys2, "tLELSpectralIndex_tmp.pa2");
    PagedImage<float> pa3(shp1, cSys3, "tLELSpectralIndex_tmp.pa3");
    pa1.put (arr1);
    pa1a.put (arr1b);
    pa2.put (arr2);
    pa3.put (arr1 + float(100));
  }
  {
    PagedImage<float> pa1("tLELSpectralIndex_tmp.pa1");
    PagedImage<float> pa2("tLELSpectralIndex_tmp.pa2");
    LatticeExpr<float> expr = spectralindex(pa1,pa2);
    Array<float> result = expr.get();
    Cube<float> arrf(shp2);
    for (int32_t i=0; i<shp2(0); i++) {
      for (int32_t j=0; j<shp2(1); j++) {
	for (int32_t k=0; k<shp2(2); k++) {
	  arrf(i,j,k) = log (scafreq / (freq + k*freqstep));
	}
      }
    }
    Array<float> expect = log(arr1a / arr2) / arrf;
    if (! allNear (result, expect, 1e-5)) {
      cout << expect << endl;
      cout << result << endl;
      ok = false;
    }
  }
  {
    PagedImage<float> pa1("tLELSpectralIndex_tmp.pa1a");
    PagedImage<float> pa2("tLELSpectralIndex_tmp.pa2");
    LatticeExpr<float> expr = spectralindex(pa1,pa2);
    Array<float> result = expr.get();
    Cube<float> arrf(shp2);
    for (int32_t i=0; i<shp2(0); i++) {
      for (int32_t j=0; j<shp2(1); j++) {
	arrf(i,j,0) = 1;
	for (int32_t k=1; k<shp2(2); k++) {
	  arrf(i,j,k) = log (freq / (freq + k*freqstep));
	}
      }
    }
    Array<float> expect = log(arr1a / arr2) / arrf;
    Array<float> subarr = expect(IPosition(3, 0, 0, 0),
				 IPosition(3, shp2(0)-1, shp2(1)-1, 0));
    subarr = 0;
    if (! allNear (result, expect, 1e-5)) {
      cout << expect << endl;
      cout << result << endl;
      ok = false;
    }
  }
  {
    PagedImage<float> pa3("tLELSpectralIndex_tmp.pa3");
    PagedImage<float> pa1("tLELSpectralIndex_tmp.pa1");
    LatticeExpr<float> expr = spectralindex(pa1,pa3);
    Array<float> result = expr.get();
    Matrix<float> arrf(shp1);
    for (int32_t i=0; i<shp1(0); i++) {
      for (int32_t j=0; j<shp1(1); j++) {
	arrf(i,j) = log (scafreq / (scafreq+freqstep));
      }
    }
    Array<float> expect = log(arr1 / (arr1+float(100))) / arrf;
    if (! allNear (result, expect, 1e-5)) {
      cout << expect << endl;
      cout << result << endl;
      ok = false;
    }
  }
  {
    PagedImage<float> pa3("tLELSpectralIndex_tmp.pa3");
    PagedImage<float> pa2("tLELSpectralIndex_tmp.pa2");
    LatticeExpr<float> expr = spectralindex(pa2,pa3);
    Array<float> result = expr.get();
    Cube<float> arrf(shp2);
    for (int32_t i=0; i<shp2(0); i++) {
      for (int32_t j=0; j<shp2(1); j++) {
	for (int32_t k=0; k<shp2(2); k++) {
	  arrf(i,j,k) = log ((freq + k*freqstep) / (scafreq+freqstep));
	}
      }
    }
    Array<float> expect = log(arr2 / (arr1a+float(100))) / arrf;
    if (! allNear (result, expect, 1e-5)) {
      cout << expect << endl;
      cout << result << endl;
      ok = false;
    }
  }
  return ok;
}


int main ()
{
  bool ok = true;
  try {
    ok = doIt();
  } catch (std::exception& x) {
    cerr << "Caught exception: " << x.what() << endl;
    ok = false;
  } 
  if (!ok) {
    cout << "FAIL" << endl;
    return 1;
  }
  cout << "OK" << endl;
  return 0;
}
