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

#include <trial/Images/PagedImage.h>
#include <trial/Coordinates/CoordinateSystem.h>
#include <trial/Coordinates/CoordinateUtil.h>
#include <trial/Coordinates/SpectralCoordinate.h>
#include <trial/Lattices/LatticeExpr.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Cube.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/ArrayIO.h>
#include <aips/Arrays/IPosition.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>
#include <aips/iostream.h>



Bool doIt()
{
  Bool ok = True;
  IPosition shp1(2,10,10);
  IPosition shp2(3,10,10,5);
  Double freq, freql, freqstep, scafreq;
  Array<Float> arr1(shp1);
  Array<Float> arr2(shp2);
  Array<Float> arr1a(shp2);
  Array<Float> arr1b(IPosition(3,10,10,1));
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
    PagedImage<Float> pa1(shp1, cSys1, "tExtendImage_tmp.pa1");
    PagedImage<Float> pa2(shp2, cSys2, "tExtendImage_tmp.pa2");
    pa1.put (arr1);
    pa2.put (arr2);
  }
  {
    PagedImage<Float> pa1("tExtendImage_tmp.pa1");
    PagedImage<Float> pa2("tExtendImage_tmp.pa2");
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
    if (! allNear (result, expect, 1e-6)) {
      cout << expect << endl;
      cout << result << endl;
      ok = False;
    }
  }
  {
    PagedImage<Float> pa1("tExtendImage_tmp.pa1");
    PagedImage<Float> pa2("tExtendImage_tmp.pa2");
    LatticeExpr<Float> expr = spectralindex(pa2,pa1);
    Array<Float> result = expr.get();
    Cube<Float> arrf(shp2);
    for (Int i=0; i<shp2(0); i++) {
      for (Int j=0; j<shp2(1); j++) {
	for (Int k=0; k<shp2(2); k++) {
	  arrf(i,j,k) = log ((freq + k*freqstep) / scafreq);
	}
      }
    }
    Array<Float> expect = log(arr2 / arr1a) / arrf;
    if (! allNear (result, expect, 1e-6)) {
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
