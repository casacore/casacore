//# tLCMask.cc:  mechanical test of the LCMask class
//# Copyright (C) 1998
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
#include <trial/Lattices/LCMask.h>
#include <trial/Lattices/LatticeStepper.h>
#include <trial/Lattices/LatticeIterator.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Exceptions/Error.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Utilities/Assert.h>
#include <iostream.h>


void testVectorROIter (const Lattice<Bool>& lattice, Bool firstValue,
		       Bool changes)
{
    Int nstep;
    const IPosition latticeShape(lattice.shape());
    const IPosition cursorShape(1,latticeShape(0));
    LatticeStepper step(latticeShape, cursorShape);
    RO_LatticeIterator<Bool>  iter(lattice, step);
    Bool value = firstValue;
    for (iter.reset(); !iter.atEnd(); iter++){
        AlwaysAssert(allEQ(iter.vectorCursor().ac(), value), AipsError);
	if (changes) {
	    value = ToBool(!value);
	}
    }
    nstep = iter.nsteps();
    AlwaysAssert(nstep == latticeShape.product()/latticeShape(0) - 1,
		 AipsError);
    IPosition expectedPos(latticeShape-1);
    AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
    expectedPos(0) = 0;
    AlwaysAssert(iter.position() == expectedPos, AipsError);
}


void testArrayRWIter (Lattice<Bool>& lattice)
{
    const IPosition latticeShape(lattice.shape());
    const IPosition cursorShape(latticeShape);
    LatticeIterator<Bool>  iter(lattice, cursorShape);
    for (iter.reset(); !iter.atEnd(); ++iter){
        iter.rwCursor() = !(iter.cursor());
    }
}


main ()

{
  try {
    {
        IPosition latticeShape(2, 4, 8);
        Array<Bool> arr(latticeShape);
        arr.set(True);
        arr(IPosition(2,0,0)) = False;
        LCMask mask(IPosition(2,0), arr, latticeShape);
        cout << mask.hasMask() << mask.maskArray().ac() << endl;
    }
    {
      IPosition latticeShape(4, 16, 12, 4, 32);
      Array<Bool> arr(latticeShape);
      arr(IPosition(4,0,0,0,0), latticeShape-1, IPosition(4,1,2,1,1)) = True;
      arr(IPosition(4,0,1,0,0), latticeShape-1, IPosition(4,1,2,1,1)) = False;
      LCMask mask(IPosition(4,0), arr, latticeShape);
      AlwaysAssertExit (mask.isWritable());
      AlwaysAssertExit (mask.hasMask());
      AlwaysAssertExit (mask.shape() == latticeShape);
      // Check the mask functions using the iterator.
      testVectorROIter (mask, True, True);
      testArrayRWIter (mask);
      testVectorROIter (mask, False, True);
      TableRecord rec = mask.toRecord("");
      LCRegion* copmask = LCRegion::fromRecord (rec, "");
      AlwaysAssertExit (copmask->isWritable());
      AlwaysAssertExit (copmask->hasMask());
      AlwaysAssertExit (copmask->shape() == latticeShape);
      testVectorROIter (*copmask, False, True);
      LCRegion* trmask = copmask->translate (IPosition(4,2,0,0,0));
      AlwaysAssertExit (trmask->isWritable());
      AlwaysAssertExit (trmask->hasMask());
      AlwaysAssertExit (trmask->latticeShape() == latticeShape);
      latticeShape(0) -= 2;
      AlwaysAssertExit (trmask->shape() == latticeShape);
      AlwaysAssertExit (trmask->box().start() == IPosition(4,2,0,0,0));
      testVectorROIter (*trmask, False, True);
      delete copmask;
      delete trmask;
    }
    {
      const IPosition latticeShape(4, 16, 12, 4, 32);
      // Construct with a trc which is 1 too high. The ctor corrects it.
      LCBox region(IPosition(4,0), latticeShape, latticeShape);
      AlwaysAssertExit (! region.isWritable());
      AlwaysAssertExit (! region.hasMask());
      AlwaysAssertExit (region.shape() == latticeShape);
      // Check the region functions using the iterator.
      testVectorROIter (region, True, False);
    }
    {
      IPosition latticeShape(2, 4, 8);
      Array<Bool> arr(latticeShape);
      arr.set(True);
      arr(IPosition(2,0)) = False;
      arr(latticeShape-1) = False;
      LCMask mask1(IPosition(2,0), arr, latticeShape);
      LCMask mask2(mask1);
      AlwaysAssertExit (mask2 == mask1);

      arr(latticeShape-1) = True;
      LCMask mask3(IPosition(2,0), arr, latticeShape);
      AlwaysAssertExit (mask3 != mask1);
    }
  } catch (AipsError x) {
    cerr << "Caught exception: " << x.getMesg() << endl;
    cout << "FAIL" << endl;
    exit(1);
  } end_try;
  cout << "OK" << endl;
  exit(0);
}
