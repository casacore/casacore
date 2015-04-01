//# tLELMedian.cc:  Tests the fractile function in LELFunction
//# Copyright (C) 1999,2000,2001
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


#include <casacore/lattices/Lattices/ArrayLattice.h>
#include <casacore/lattices/Lattices/TempLattice.h>
#include <casacore/lattices/Lattices/LatticeIterator.h>
#include <casacore/lattices/LatticeMath/LatticeFractile.h>
#include <casacore/lattices/LEL/LatticeExpr.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Inputs/Input.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
int main (int argc, const char* argv[])
{
  try {
    cout << ">>>" << endl;
    Input inp(1);
    inp.version(" ");
    inp.create("nx", "16", "Number of pixels along the x-axis", "int");
    inp.create("ny", "16", "Number of pixels along the y-axis", "int");
    inp.readArguments(argc, argv);
    cout << "<<<" << endl;

    const uInt nx=inp.getInt("nx");
    const uInt ny=inp.getInt("ny");

//
// Test various sized arrays.
//

    {
      // Test lattice with all equal values.
      IPosition shape(2, nx, ny);
      Array<Float> arr(shape);
      arr = 1.;
      ArrayLattice<Float> aF(arr);

      cout << median (arr) << endl;
      cout << LatticeFractile<Float>::unmaskedFractile(aF, 0.5) << endl;
      cout << LatticeFractile<Float>::unmaskedFractile(aF, 0.5, 16) << endl;
      cout << LatticeFractile<Float>::unmaskedFractile(aF, 0.5, 4) << endl;
      cout << LatticeFractile<Float>::unmaskedFractile(aF, 0.5, 2) << endl;
      LatticeExprNode afExpr(aF);
      LatticeExpr<Float> expr(afExpr[aF>4]);
      cout << LatticeFractile<Float>::maskedFractile(expr, 0.5) << endl;
    }

    {
      IPosition shape(2, nx, ny);
      Array<Float> arr(shape);
      indgen (arr);
      ArrayLattice<Float> aF(arr);

      cout << median (arr) << endl;
      cout << LatticeFractile<Float>::unmaskedFractile(aF, 0.5) << endl;
      cout << LatticeFractile<Float>::unmaskedFractile(aF, 0.5, 16) << endl;
      cout << LatticeFractile<Float>::unmaskedFractile(aF, 0.5, 4) << endl;
      cout << LatticeFractile<Float>::unmaskedFractile(aF, 0.5, 2) << endl;
      LatticeExprNode afExpr(aF);
      LatticeExpr<Float> expr(afExpr[aF>4]);
      cout << LatticeFractile<Float>::maskedFractile(expr, 0.5) << endl;
    }

    {
      IPosition shape(2, 10*nx, 10*ny);
      Array<Float> arr(shape);
      indgen (arr);
      ArrayLattice<Float> aF(arr);

      cout << median (arr) << endl;
      cout << LatticeFractile<Float>::unmaskedFractile(aF, 0.5) << endl;
      cout << LatticeFractile<Float>::unmaskedFractile(aF, 0.5, 16) << endl;
      cout << LatticeFractile<Float>::unmaskedFractile(aF, 0.5, 4) << endl;
      cout << LatticeFractile<Float>::unmaskedFractile(aF, 0.5, 2) << endl;
      LatticeExprNode afExpr(aF);
      LatticeExpr<Float> expr(afExpr[aF>4]);
      cout << LatticeFractile<Float>::maskedFractile(expr, 0.5) << endl;
    }

    {
      IPosition shape(2, 32*nx, 32*ny);
      Array<Float> arr(shape);
      indgen (arr);
      ArrayLattice<Float> aF(arr);

      cout << median (arr) << endl;
      cout << LatticeFractile<Float>::unmaskedFractile(aF, 0.5) << endl;
      cout << LatticeFractile<Float>::unmaskedFractile(aF, 0.5, 128) << endl;
      LatticeExprNode afExpr(aF);
      LatticeExpr<Float> expr(afExpr[aF>4]);
      cout << LatticeFractile<Float>::maskedFractile(expr, 0.5) << endl;
    }

    // Hereafter numbers can be different on different machines.
    // So 'outcomment' it for assay (and also outcomment the timings).
    cout << ">>>" << endl;
    {
      IPosition shape(2, 100*nx, 100*ny);
      Array<Float> arr(shape);
      indgen (arr, float(0), float(0.01));
      ArrayLattice<Float> aF(arr);
      cout << "Last value = " << arr(shape-1) << endl;
      Timer timer;

      cout << median (arr) << endl;
      timer.show ("normal median");
      timer.mark();
      cout << LatticeFractile<Float>::unmaskedFractile(aF, 0.5) << endl;
      timer.show ("ArrayLattice ");
      timer.mark();
      cout << LatticeFractile<Float>::unmaskedFractile(aF, 0.5, 1280)
 << endl;
      timer.show ("ArrayLat 1280");

      timer.mark();
      LatticeExprNode afExpr(aF);
      LatticeExpr<Float> expr(afExpr[aF>4]);
      cout << LatticeFractile<Float>::maskedFractile(expr, 0.5) << endl;
      timer.show ("MaskedLattice");
    }

    {
      IPosition shape(2, 100*nx, 100*ny);
      Array<Float> arr(shape);
      indgen (arr, float(0), float(0.01));
      TempLattice<Float> aF(shape);
      aF.put (arr);
      Timer timer;

      cout << LatticeFractile<Float>::unmaskedFractile(aF, 0.5) << endl;
      timer.show ("PagedArray   ");
      timer.mark();
      cout << LatticeFractile<Float>::unmaskedFractile(aF, 0.5, 4) << endl;
      timer.show ("PagedArr 4   ");
      timer.mark();
      cout << LatticeFractile<Float>::unmaskedFractile(aF, 0.5, 1280)
 << endl;
      timer.show ("PagedArr 1280");
    }

    {
      IPosition fshape(2, 500*nx, 500*ny);
      TempLattice<Float> aF(fshape);
      IPosition shape = aF.niceCursorShape();
      cout << "tileshape = " << shape << endl;
      Array<Float> arr(shape);
      indgen (arr, float(0), float(0.01));
      Timer timer;
      LatticeIterator<Float> iter(aF, shape);
      while (! iter.atEnd()) {
	iter.woCursor() = arr;
	iter++;
      }
      timer.show ("Fill PagedArr");
      timer.mark();

      cout << LatticeFractile<Float>::unmaskedFractile(aF, 0.5) << endl;
      timer.show ("BigPagedArray");
      timer.mark();
      cout << LatticeFractile<Float>::unmaskedFractile(aF, 0.5, 1280*25)
	   << endl;
      timer.show ("PagedArr 1280");
    }
    cout << "<<<" << endl;

  } catch (const AipsError& x) {
    cout << "Unexpected exception: " << x.getMesg() << endl;
    return 1;
  }

  return 0;
}
