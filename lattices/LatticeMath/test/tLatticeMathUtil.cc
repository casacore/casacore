//# tLatticeMathUtil.cc:
//# Copyright (C) 1997,1998,1999,2000,2001
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
//# $Id: tLatticeUtilities.cc 21509 2014-11-21 12:25:09Z gervandiepen $

#include <casacore/casa/aips.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/MaskArrLogi.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/lattices/LatticeMath/LatticeMathUtil.h>
#include <casacore/lattices/Lattices/TempLattice.h>
#include <casacore/lattices/Lattices/LatticeStepper.h>
#include <casacore/lattices/Lattices/LatticeIterator.h>
#include <casacore/lattices/Lattices/SubLattice.h>
#include <casacore/lattices/LEL/LatticeExprNode.h>
#include <casacore/lattices/LEL/LatticeExpr.h>
#include <casacore/lattices/Lattices/ArrayLattice.h>

#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
void doMinMax();
void doCollapse();

int main()
{
  try {
    
// minMax

     doMinMax();

// Collapse

     doCollapse();

  } catch (const AipsError& x) {
    cout<< "FAIL"<< endl;
    cerr << x.getMesg() << endl;
    return 1;
  } 
  cout<< "OK"<< endl;
  return 0;
}




void doMinMax()
{
    cerr << "minMax " << endl;
    TempLattice<Float> lat(IPosition(3,512,512,10), 1.0);
    lat.set(0.0);
    lat.putAt( 1.0, IPosition(3, 10, 10, 0) );
    lat.putAt( -1.0, IPosition(3, 20, 20, 2) );
    lat.putAt( 1.0, IPosition(3, 500, 400, 3) );
    lat.putAt( -1.0, IPosition(3, 502, 490, 4) );
    lat.putAt( 2.0, IPosition(3, 400, 500, 5) );
    lat.putAt( -2.0, IPosition(3, 10, 400, 6) );
    lat.putAt( 3.0, IPosition(3, 400, 100, 7) );
    lat.putAt( -3.0, IPosition(3, 500, 100, 8) );

    Float lmin, lmax;
    IPosition lminPos(3, 0);
    IPosition lmaxPos(3, 0);

    minMax(lmin, lmax, lminPos, lmaxPos, lat);

    IPosition trueMaxPos = IPosition(3, 400, 100, 7);
    IPosition trueMinPos = IPosition(3, 500, 100, 8);
    AlwaysAssert(trueMaxPos == lmaxPos && lmax == 3.0, AipsError);
    AlwaysAssert(trueMinPos == lminPos && lmin == -3.0, AipsError);
}


void doCollapse ()
{
    cerr << "Collapse" << endl;
    IPosition shape(3, 10, 20, 30);
    ArrayLattice<Float> latIn(shape);
    latIn.set(1.0);
    Array<Float> data;
    Array<Bool> mask;
    IPosition axes(2); axes(0) = 1; axes(1) = 2;

// Unmasked input

    {       
       cerr << "  Unmasked" << endl;
       SubLattice<Float> mLatIn(latIn);
//
       LatticeMathUtil::collapse (data, axes, mLatIn, True);
       AlwaysAssert(data.ndim()==1, AipsError);       
       AlwaysAssert(data.shape()(0)==shape(0), AipsError);
       AlwaysAssert(allNear(data, Float(1.0), 1.0e-6), AipsError);
//
       LatticeMathUtil::collapse (data, mask, axes, mLatIn, True, True, True);
       AlwaysAssert(data.ndim()==1, AipsError);       
       AlwaysAssert(mask.ndim()==1, AipsError);
       AlwaysAssert(data.shape()(0)==shape(0), AipsError);
       AlwaysAssert(mask.shape()(0)==shape(0), AipsError);
       AlwaysAssert(allNear(data, Float(1.0), 1.0e-6), AipsError);
       AlwaysAssert(allEQ(mask, True), AipsError);
    }

// Masked Input

    {       
       cerr << "  Masked" << endl;
       SubLattice<Float> mLatIn(latIn);
//
       ArrayLattice<Bool> maskLat(shape);
       maskLat.set(True);       
       mLatIn.setPixelMask(maskLat, True);
//
       LatticeMathUtil::collapse (data, axes, mLatIn, True);
       AlwaysAssert(data.ndim()==1, AipsError);       
       AlwaysAssert(data.shape()(0)==shape(0), AipsError);
       AlwaysAssert(allNear(data, Float(1.0), 1.0e-6), AipsError);
//
       LatticeMathUtil::collapse (data, mask, axes, mLatIn, True, True, True);
       AlwaysAssert(data.ndim()==1, AipsError);       
       AlwaysAssert(mask.ndim()==1, AipsError);
       AlwaysAssert(data.shape()(0)==shape(0), AipsError);
       AlwaysAssert(mask.shape()(0)==shape(0), AipsError);
       AlwaysAssert(allNear(data, Float(1.0), 1.0e-6), AipsError);
       AlwaysAssert(allEQ(mask, True), AipsError);
    }
}
