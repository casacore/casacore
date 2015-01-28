//# tLCPixelSet.cc:  mechanical test of the LCPixelSet class
//# Copyright (C) 1998,1999,2000,2001
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
#include <casacore/lattices/LRegions/LCPixelSet.h>
#include <casacore/lattices/Lattices/LatticeStepper.h>
#include <casacore/lattices/Lattices/LatticeIterator.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayIO.h>
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
        // static_cast is a workaround for an SGI compiler bug
        AlwaysAssert(allEQ(static_cast<Vector<Bool> >(iter.vectorCursor()), value), AipsError);
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


void testArrayRWIter (Lattice<Bool>& lattice)
{
    const IPosition latticeShape(lattice.shape());
    const IPosition cursorShape(latticeShape);
    LatticeIterator<Bool>  iter(lattice, cursorShape);
    for (iter.reset(); !iter.atEnd(); ++iter){
        iter.rwCursor() = !(iter.cursor());
    }
}


int main ()

{
  try {
    {
        IPosition latticeShape(2, 4, 8);
        Array<Bool> arr(latticeShape);
        arr.set(True);
        arr(IPosition(2,0,0)) = False;
        LCPixelSet mask(arr, LCBox(IPosition(2,0),
				   latticeShape-1, latticeShape));
        cout << mask.hasMask() << mask.maskArray() << endl;
    }
    {
      IPosition latticeShape(4, 16, 12, 4, 32);
      Array<Bool> arr(latticeShape);
      arr(IPosition(4,0,0,0,0), latticeShape-1, IPosition(4,1,2,1,1)) = True;
      arr(IPosition(4,0,1,0,0), latticeShape-1, IPosition(4,1,2,1,1)) = False;
      LCPixelSet mask(arr, LCBox (IPosition(4,0),
				  latticeShape-1, latticeShape));
      AlwaysAssertExit (! mask.isWritable());
      AlwaysAssertExit (mask.hasMask());
      AlwaysAssertExit (mask.shape() == latticeShape);
      // Check the mask functions using the iterator.
      testVectorROIter (mask, True, True);
///      testArrayRWIter (mask);
///      testVectorROIter (mask, False, True);
      TableRecord rec = mask.toRecord("");
      LCRegion* copmask = LCRegion::fromRecord (rec, "");
      AlwaysAssertExit (! copmask->isWritable());
      AlwaysAssertExit (copmask->hasMask());
      AlwaysAssertExit (copmask->shape() == latticeShape);
      testVectorROIter (*copmask, True, True);
///      LCRegion* trmask = copmask->translate (IPosition(4,2,0,0,0));
///      AlwaysAssertExit (trmask->isWritable());
///      AlwaysAssertExit (trmask->hasMask());
///      AlwaysAssertExit (trmask->latticeShape() == latticeShape);
///      latticeShape(0) -= 2;
///      AlwaysAssertExit (trmask->shape() == latticeShape);
///      AlwaysAssertExit (trmask->boundingBox().start()
///                        == IPosition(4,2,0,0,0));
///      testVectorROIter (*trmask, False, True);
      delete copmask;
///      delete trmask;
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
      LCPixelSet mask1(arr, LCBox(IPosition(2,0),
				  latticeShape-1, latticeShape));
      LCPixelSet mask2(mask1);
      AlwaysAssertExit (mask2 == mask1);
      arr(latticeShape-1) = True;
      LCPixelSet mask3(arr, LCBox(IPosition(2,0),
				  latticeShape-1, latticeShape));
      AlwaysAssertExit (mask3 != mask1);
    }
  } catch (AipsError x) {
    cerr << "Caught exception: " << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  } 
  cout << "OK" << endl;
  return 0;
}
