//# tLatticeFractile.cc:  Tests the functions in LatticeFractile
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


#include <lattices/Lattices/ArrayLattice.h>
#include <lattices/Lattices/TempLattice.h>
#include <lattices/Lattices/LatticeIterator.h>
#include <lattices/Lattices/LatticeFractile.h>
#include <lattices/Lattices/LatticeExpr.h>
#include <casa/Arrays/Vector.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/ArrayIO.h>
#include <casa/Inputs/Input.h>
#include <casa/OS/Timer.h>
#include <casa/Exceptions/Error.h>
#include <casa/iostream.h>


#include <casa/namespace.h>
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

      cout << "Fractiles (left 50%, right 50%) test" << endl;
      cout << LatticeFractile<Float>::unmaskedFractiles (aF, 0.5, 0.5)
	   << endl;
      cout << LatticeFractile<Float>::unmaskedFractiles (aF, 0.5, 0.5, 16)
	   << endl;
      cout << LatticeFractile<Float>::unmaskedFractiles (aF, 0.5, 0.5, 4)
	   << endl;
      cout << LatticeFractile<Float>::unmaskedFractiles (aF, 0.5, 0.5, 2)
	   << endl;
      LatticeExprNode afExpr(aF);
      LatticeExpr<Float> expr(afExpr[aF>4]);
      cout << LatticeFractile<Float>::maskedFractile(expr, 0.5) << endl;
      cout << "Fractiles (left 40%, right 45%) test" << endl;
      cout << LatticeFractile<Float>::unmaskedFractiles (aF, 0.40, 0.55, 2)
	   << endl;
      cout << LatticeFractile<Float>::maskedFractiles(expr, 0.4, 0.45) << endl;
      cout << LatticeFractile<Float>::maskedFractiles(expr, 0.4, 0.45, 2)
	   << endl;
    }

    {
      IPosition shape(2, nx, ny);
      Array<Float> arr(shape);
      indgen (arr);
      ArrayLattice<Float> aF(arr);

      cout << "Fractiles (left 50%, right 50%) test: ";
      cout << LatticeFractile<Float>::unmaskedFractiles (aF, 0.5, 0.5)
	   << endl;
      cout << LatticeFractile<Float>::unmaskedFractiles (aF, 0.5, 0.5, 16)
	   << endl;
      cout << LatticeFractile<Float>::unmaskedFractiles (aF, 0.5, 0.5, 4)
	   << endl;
      cout << LatticeFractile<Float>::unmaskedFractiles (aF, 0.5, 0.5, 2)
	   << endl;
      LatticeExprNode afExpr(aF);
      LatticeExpr<Float> expr(afExpr[aF>4]);
      cout << LatticeFractile<Float>::maskedFractile(expr, 0.5) << endl;
      cout << "Fractiles (left 40%, right 45%) test" << endl;
      cout << LatticeFractile<Float>::unmaskedFractiles (aF, 0.40, 0.55, 2)
	   << endl;
      cout << LatticeFractile<Float>::maskedFractiles(expr, 0.40, 0.55) << endl;
      cout << LatticeFractile<Float>::maskedFractiles(expr, 0.40, 0.55, 2)
	   << endl;
    }

    {
      IPosition shape(2, 10*nx, 10*ny);
      Array<Float> arr(shape);
      indgen (arr);
      ArrayLattice<Float> aF(arr);

      cout << "Fractiles (left 50%, right 50%) test: ";
      cout << LatticeFractile<Float>::unmaskedFractiles (aF, 0.5, 0.5)
	   << endl;
      cout << LatticeFractile<Float>::unmaskedFractiles (aF, 0.5, 0.5, 16)
	   << endl;
      cout << LatticeFractile<Float>::unmaskedFractiles (aF, 0.5, 0.5, 4)
	   << endl;
      cout << LatticeFractile<Float>::unmaskedFractiles (aF, 0.5, 0.5, 2)
	   << endl;
      LatticeExprNode afExpr(aF);
      LatticeExpr<Float> expr(afExpr[aF>4]);
      cout << LatticeFractile<Float>::maskedFractiles (expr, 0.5, 0.5) << endl;
      cout << LatticeFractile<Float>::maskedFractiles (expr, 0.5, 0.5, 2)
	   << endl;
    }

    {
      IPosition shape(2, 32*nx, 32*ny);
      Array<Float> arr(shape);
      indgen (arr);
      ArrayLattice<Float> aF(arr);

      cout << "Fractiles (left 50%, right 50%) test: ";
      cout << LatticeFractile<Float>::unmaskedFractiles (aF, 0.5, 0.5)
	   << endl;
      cout << LatticeFractile<Float>::unmaskedFractiles (aF, 0.5, 0.5, 16)
	   << endl;
      cout << LatticeFractile<Float>::unmaskedFractiles (aF, 0.5, 0.5, 4)
	   << endl;
      cout << LatticeFractile<Float>::unmaskedFractiles (aF, 0.5, 0.5, 2)
	   << endl;
      LatticeExprNode afExpr(aF);
      LatticeExpr<Float> expr(afExpr[aF>4]);
      cout << LatticeFractile<Float>::maskedFractiles (expr, 0.5, 0.5) << endl;
      cout << LatticeFractile<Float>::maskedFractiles (expr, 0.5, 0.5, 2)
	   << endl;
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
      timer.mark();
      cout << LatticeFractile<Float>::unmaskedFractiles (aF, 0.2, 0.8, 512*512)
	   << endl;
      timer.show ("ArrayLattice  20% fractiles");
      timer.mark();
      cout << LatticeFractile<Float>::unmaskedFractiles (aF, 0.2, 0.8, 1280)
	   << endl;
      timer.show ("ArrayLat 1280 20% fractiles");
      timer.mark();
      LatticeExprNode afExpr(aF);
      LatticeExpr<Float> expr(afExpr[aF>4]);
      cout << LatticeFractile<Float>::maskedFractiles (expr, 0.2, 0.8) << endl;
      timer.show ("MaskedLattice 20% fractiles");
    }

    {
      IPosition shape(2, 100*nx, 100*ny);
      Array<Float> arr(shape);
      indgen (arr, float(0), float(0.01));
      TempLattice<Float> aF(shape);
      aF.put (arr);
      Timer timer;

      cout << LatticeFractile<Float>::unmaskedFractiles(aF, 0.2, 0.8) << endl;
      timer.show ("PagedArray    20% fractiles");
      timer.mark();
      cout << LatticeFractile<Float>::unmaskedFractiles(aF, 0.2, 0.8, 4)
	   << endl;
      timer.show ("PagedArr 4    20% fractiles");
      timer.mark();
      cout << LatticeFractile<Float>::unmaskedFractiles(aF, 0.2, 0.8, 1280)
	   << endl;
      timer.show ("PagedArr 1280 20% fractiles");
      timer.mark();
      LatticeExprNode afExpr(aF);
      LatticeExpr<Float> expr(afExpr[aF>4]);
      cout << LatticeFractile<Float>::maskedFractiles (expr, 0.2, 0.8) << endl;
      timer.show ("MaskedLattice 20% fractiles");
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

      cout << LatticeFractile<Float>::unmaskedFractiles(aF, 0.5, 0.5) << endl;
      timer.show ("BigPagedArray");
      timer.mark();
      cout << LatticeFractile<Float>::unmaskedFractiles(aF, 0.5, 0.5, 1280*25)
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
