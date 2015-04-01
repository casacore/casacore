//# tWCLELMask.cc:  mechanical test of the WCLELMask class
//# Copyright (C) 2000,2001,2003
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

#include <casacore/casa/aips.h>
#include <casacore/images/Regions/WCLELMask.h>
#include <casacore/images/Images/PagedImage.h>
#include <casacore/images/Images/ImageExprParse.h>
#include <casacore/coordinates/Coordinates/CoordinateUtil.h>
#include <casacore/coordinates/Coordinates/CoordinateSystem.h>
#include <casacore/lattices/LRegions/LCLELMask.h>
#include <casacore/lattices/LEL/LatticeExpr.h>
#include <casacore/lattices/Lattices/LatticeStepper.h>
#include <casacore/lattices/Lattices/LatticeIterator.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
void testVectorROIter (const Lattice<Bool>& lattice, Bool firstValue,
		       Bool alternates)
{
    Int nstep;
    const IPosition latticeShape(lattice.shape());
    const IPosition cursorShape(1,latticeShape(0));
    LatticeStepper step(latticeShape, cursorShape);
    RO_LatticeIterator<Bool>  iter(lattice, step);
    Bool value = firstValue;
    for (iter.reset(); !iter.atEnd(); iter++){
        AlwaysAssert(allEQ(iter.vectorCursor(), value), AipsError);
	if (alternates) {
	    value = (!value);
	}
    }
    nstep = iter.nsteps();
    AlwaysAssert(nstep == latticeShape.product()/latticeShape(0), AipsError);
    IPosition expectedPos(latticeShape-1);
    AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
    expectedPos(0) = 0;
    AlwaysAssert(iter.position() == expectedPos, AipsError);
}


int main ()
{
  try {
    CoordinateSystem cSys = CoordinateUtil::defaultCoords3D();
    IPosition latticeShape(3, 4, 8, 11);
    Array<Float> arr(latticeShape);
    indgen (arr);
    PagedImage<Float> image(latticeShape, cSys, "tWCLELMask_tmp.img");
    image.put (arr);
    image.flush();
    ArrayLattice<Float> arrlat(arr);
    {
      WCLELMask mask(String("fmod(floor(tWCLELMask_tmp.img / 4), 2) == 0"));
      AlwaysAssertExit (mask.ndim() == latticeShape.nelements());
      LCRegion* lc = mask.toLCRegion (cSys, latticeShape);
      AlwaysAssertExit (lc->hasMask());
      AlwaysAssertExit (! lc->isWritable());
      AlwaysAssertExit (lc->shape() == latticeShape);
      // Check the mask values using the iterator.
      testVectorROIter (*lc, True, True);
      delete lc;
    }
    {
      WCLELMask mask(ImageExprParse::command
                       ("fmod(floor(tWCLELMask_tmp.img / 4), 2) == 0"));
      AlwaysAssertExit (mask.ndim() == latticeShape.nelements());
      LCRegion* lc = mask.toLCRegion (cSys, latticeShape);
      AlwaysAssertExit (lc->hasMask());
      AlwaysAssertExit (! lc->isWritable());
      AlwaysAssertExit (lc->shape() == latticeShape);
      // Check the mask values using the iterator.
      testVectorROIter (*lc, True, True);
      delete lc;
    }
    {
      // Test if the auto-extend works fine.
      CoordinateSystem cSys2 = CoordinateUtil::defaultCoords2D();
      IPosition shape2(2, 4, 8);
      Array<Float> arr2(shape2);
      indgen (arr2);
      PagedImage<Float> image2(shape2, cSys2, "tWCLELMask_tmp.img2");
      image2.put (arr2);
      image2.flush();
      WCLELMask mask("fmod(floor(tWCLELMask_tmp.img2 / 4), 2) == 0");
      AlwaysAssertExit (mask.ndim() == shape2.nelements());
      LCRegion* lc = mask.toLCRegion (cSys, latticeShape);
      AlwaysAssertExit (lc->hasMask());
      AlwaysAssertExit (! lc->isWritable());
      AlwaysAssertExit (lc->shape() == latticeShape);
      // Check the mask values using the iterator.
      testVectorROIter (*lc, True, True);
      delete lc;
      // Should get exception for incorrect shape.
      try {
	LCRegion* lc = mask.toLCRegion (cSys, latticeShape-1);
	delete lc;
      } catch (AipsError x) {
	cout << "Expected exception: " << x.getMesg() << endl;
      }
    }
    {
      // Test if it works fine for an expression without coordinates.
      WCLELMask mask(fmod(floor(arrlat / 4), 2) == 0);
      AlwaysAssertExit (mask.ndim() == latticeShape.nelements());
      LCRegion* lc = mask.toLCRegion (cSys, latticeShape);
      AlwaysAssertExit (lc->hasMask());
      AlwaysAssertExit (! lc->isWritable());
      AlwaysAssertExit (lc->shape() == latticeShape);
      // Check the mask values using the iterator.
      testVectorROIter (*lc, True, True);
      delete lc;
      // Should get exception for incorrect shape.
      try {
	LCRegion* lc = mask.toLCRegion (cSys, latticeShape-1);
	delete lc;
      } catch (AipsError x) {
	cout << "Expected exception: " << x.getMesg() << endl;
      }
    }
    {
      // Test if it works fine for an expression without shape.
      WCLELMask mask("index0 in [0:3]");
      AlwaysAssertExit (mask.ndim() == 0);
      LCRegion* lc = mask.toLCRegion (cSys, latticeShape);
      AlwaysAssertExit (lc->hasMask());
      AlwaysAssertExit (! lc->isWritable());
      AlwaysAssertExit (lc->shape() == latticeShape);
      // Check the mask values using the iterator.
      testVectorROIter (*lc, True, False);
      delete lc;
    }
    {
      // Test if it works fine for an expression without shape.
      WCLELMask mask("indexnotin(1,[1:7:2])");
      AlwaysAssertExit (mask.ndim() == 0);
      LCRegion* lc = mask.toLCRegion (cSys, latticeShape);
      AlwaysAssertExit (lc->hasMask());
      AlwaysAssertExit (! lc->isWritable());
      AlwaysAssertExit (lc->shape() == latticeShape);
      // Check the mask values using the iterator.
      testVectorROIter (*lc, True, True);
      delete lc;
    }
  } catch (AipsError x) {
    cerr << "Caught exception: " << x.getMesg() << endl;
    return 1;
  } 

   cout << "OK" << endl;
   return 0;
}
