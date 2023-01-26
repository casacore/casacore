//# tLatticeTwoPtCorr.cc: Test program for class LatticeTwoPtCorr
//# Copyright (C) 1998,1999,2000,2001,2003
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

#include <casacore/lattices/LatticeMath/LatticeTwoPtCorr.h>
#include <casacore/lattices/Lattices/ArrayLattice.h>
#include <casacore/lattices/Lattices/LatticeIterator.h>
#include <casacore/lattices/Lattices/LatticeStepper.h>
#include <casacore/lattices/Lattices/TempLattice.h>
#include <casacore/lattices/Lattices/SubLattice.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
int main ()
{
  try {

// Make Lattice

      uint32_t nx = 4;
      uint32_t ny = 8;
      uint32_t nz = 16;
      IPosition shape(3, nx, ny, nz);
      Array<float> tArr(shape);
      indgen (tArr);
//
      TiledShape tShapeIn(shape);
      TempLattice<float>  latIn(tShapeIn);
      latIn.put(tArr);
//
      AxesSpecifier spec;
      SubLattice<float> mLatIn(latIn, false, spec);

// Make Structure Functions.  No validation of output values
// just make sure it runs.

// x-y plane

      {
         cerr << "XY plane" << endl;
         IPosition axes(2, 0, 1);
         IPosition shapeOut = LatticeTwoPtCorr<float>::setUpShape (shape, axes);
         cerr << "Shape in, out = " << shape << shapeOut << endl;
         TiledShape tShapeOut(shapeOut);
         TempLattice<float> latOut(tShapeOut);
         SubLattice<float> mLatOut(latOut, true, spec);
//
         LatticeTwoPtCorr<float> twoPt;
         twoPt.autoCorrelation (mLatOut, mLatIn, axes, 
                                LatticeTwoPtCorr<float>::STRUCTUREFUNCTION,
                                false);
      }

// x-z plane

      {
         cerr << "XZ plane" << endl;
         IPosition axes(2, 0, 2);
         IPosition shapeOut = LatticeTwoPtCorr<float>::setUpShape (shape, axes);
         cerr << "Shape in, out = " << shape << shapeOut << endl;
         TiledShape tShapeOut(shapeOut);
         TempLattice<float> latOut(tShapeOut);
         SubLattice<float> mLatOut(latOut, true, spec);
//
         LatticeTwoPtCorr<float> twoPt;
         twoPt.autoCorrelation (mLatOut, mLatIn, axes, 
                                LatticeTwoPtCorr<float>::STRUCTUREFUNCTION,
                                false);
      }

// y-z plane

      {
         cerr << "YZ plane" << endl;
         IPosition axes(2, 1, 2);
         IPosition shapeOut = LatticeTwoPtCorr<float>::setUpShape (shape, axes);
         cerr << "Shape in, out = " << shape << shapeOut << endl;
         TiledShape tShapeOut(shapeOut);
         TempLattice<float> latOut(tShapeOut);
         SubLattice<float> mLatOut(latOut, true, spec);
//
         LatticeTwoPtCorr<float> twoPt;
         twoPt.autoCorrelation (mLatOut, mLatIn, axes, 
                                LatticeTwoPtCorr<float>::STRUCTUREFUNCTION,
                                false);
      }

// Copy Constructor

      {
        LatticeTwoPtCorr<float> t;
        LatticeTwoPtCorr<float> t2(t);
      }

// Assignment

      {
        LatticeTwoPtCorr<float> t;
        LatticeTwoPtCorr<float> t2;
        t = t2;
      }

  } catch (std::exception& x) {
    cerr << "Caught exception: " << x.what() << endl;
    cout << "FAIL" << endl;
    return 1;
  } 

  cout << "OK" << endl;
  return 0;
}

