//# tWCLELMask.cc:  mechanical test of the WCLELMask class
//# Copyright (C) 2000
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

#include <aips/aips.h>
#include <trial/Images/WCLELMask.h>
#include <trial/Images/PagedImage.h>
#include <trial/Coordinates/CoordinateUtil.h>
#include <trial/Coordinates/CoordinateSystem.h>
#include <trial/Lattices/LCLELMask.h>
#include <trial/Lattices/LatticeExpr.h>
#include <aips/Lattices/LatticeStepper.h>
#include <aips/Lattices/LatticeIterator.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Exceptions/Error.h>
#include <aips/Arrays/IPosition.h>
#include <aips/Utilities/Assert.h>
#include <iostream.h>


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
	    value = ToBool(!value);
	}
    }
    nstep = iter.nsteps();
    AlwaysAssert(nstep == latticeShape.product()/latticeShape(0), AipsError);
    IPosition expectedPos(latticeShape-1);
    AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
    expectedPos(0) = 0;
    AlwaysAssert(iter.position() == expectedPos, AipsError);
}


main ()
{
  try {
    CoordinateSystem cSys = CoordinateUtil::defaultCoords3D();
    IPosition latticeShape(3, 4, 8, 11);
    Array<Float> arr(latticeShape);
    indgen (arr);
    PagedImage<Float> image(latticeShape, cSys, "tWCLELMask_tmp.img");
    image.put (arr);
    ArrayLattice<Float> arrlat(arr);
    {
      WCLELMask mask("fmod(floor(tWCLELMask_tmp.img / 4), 2) == 0");
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
  } catch (AipsError x) {
    cerr << "Caught exception: " << x.getMesg() << endl;
    exit(1);
  } 

   cout << "OK" << endl;
   exit(0);
}
