//# tSubLattice.cc: Test program for class SubLattice
//# Copyright (C) 1998,1999,2000
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

#include <trial/Lattices/SubLattice.h>
#include <trial/Lattices/ArrayLattice.h>
#include <trial/Lattices/PagedArray.h>
#include <trial/Lattices/LatticeIterator.h>
#include <trial/Lattices/LatticeStepper.h>
#include <trial/Lattices/LCPagedMask.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/IPosition.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>
#include <iostream.h>


void testVectorROIter (const Lattice<Int>& sublat,
		       const Lattice<Int>& lattice,
		       const Slicer& slicer)
{
    Int nstep;
    const IPosition latticeShape(sublat.shape());
    const IPosition cursorShape(1,latticeShape(0));
    LatticeStepper step(latticeShape, cursorShape);
    RO_LatticeIterator<Int>  iter(sublat, step);
    LatticeStepper step2(lattice.shape(), cursorShape);
    step2.subSection (slicer.start(), slicer.end(), slicer.stride());
    RO_LatticeIterator<Int>  iter2(lattice, step2);
    for (iter.reset(); !iter.atEnd(); iter++, iter2++){
        AlwaysAssert(allEQ(iter.vectorCursor(),
			   iter2.vectorCursor()), AipsError);
    }
    nstep = iter.nsteps();
    AlwaysAssert(nstep == latticeShape.product()/latticeShape(0),
		 AipsError);
    IPosition expectedPos(latticeShape-1);
    AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
    expectedPos(0) = 0;
    AlwaysAssert(iter.position() == expectedPos, AipsError);
}


void testRest()
{
    PagedArray<Int> pa(IPosition(1,10), "tSubLattice_tmp.pa");
    AlwaysAssertExit (pa.isPaged());
    AlwaysAssertExit (pa.isPersistent());
    AlwaysAssertExit (pa.isWritable());
    AlwaysAssertExit (pa.name(True) == "tSubLattice_tmp.pa");
    LCPagedMask mask(IPosition(1,10), "tSubLattice_tmp.pa/mask");
    Slicer slicer(IPosition(1,1), IPosition(1,3));
    Slicer slfull(IPosition(1,0), IPosition(1,10));
    {
        // A SubLattice as a Lattice copy (RO).
	SubLattice<Int> sl(pa);
	AlwaysAssertExit (!sl.isMasked());
	AlwaysAssertExit (!sl.hasPixelMask());
	AlwaysAssertExit (sl.isPaged());
	AlwaysAssertExit (sl.isPersistent());
	AlwaysAssertExit (!sl.isWritable());
	AlwaysAssertExit (!sl.isMaskWritable());
	// A copy of the SubLattice.
	SubLattice<Int> sl1(sl, True);
	AlwaysAssertExit (!sl1.isMasked());
	AlwaysAssertExit (!sl1.hasPixelMask());
	AlwaysAssertExit (sl1.isPaged());
	AlwaysAssertExit (sl1.isPersistent());
	AlwaysAssertExit (!sl1.isWritable());
	AlwaysAssertExit (!sl1.isMaskWritable());
	AlwaysAssertExit (sl1.name(True) == "tSubLattice_tmp.pa");
    }
    {
        // A SubLattice as a Lattice copy (RW).
	SubLattice<Int> sl(pa, True);
	AlwaysAssertExit (!sl.isMasked());
	AlwaysAssertExit (!sl.hasPixelMask());
	AlwaysAssertExit (sl.isPaged());
	AlwaysAssertExit (sl.isPersistent());
	AlwaysAssertExit (sl.isWritable());
	AlwaysAssertExit (!sl.isMaskWritable());
	// A RW copy of the SubLattice.
	SubLattice<Int> sl1(sl, True);
	AlwaysAssertExit (!sl1.isMasked());
	AlwaysAssertExit (!sl1.hasPixelMask());
	AlwaysAssertExit (sl1.isPaged());
	AlwaysAssertExit (sl1.isPersistent());
	AlwaysAssertExit (sl1.isWritable());
	AlwaysAssertExit (!sl1.isMaskWritable());
	// A RO copy of the SubLattice.
	SubLattice<Int> sl2(sl, False);
	AlwaysAssertExit (!sl2.isMasked());
	AlwaysAssertExit (!sl2.hasPixelMask());
	AlwaysAssertExit (sl2.isPaged());
	AlwaysAssertExit (sl2.isPersistent());
	AlwaysAssertExit (!sl2.isWritable());
	AlwaysAssertExit (!sl2.isMaskWritable());
	AlwaysAssertExit (sl2.name(True) == "tSubLattice_tmp.pa");
    }
    {
        // A RO SubLattice as a masked Lattice.
	SubLattice<Int> sl(pa, mask);
	AlwaysAssertExit (sl.isMasked());
	AlwaysAssertExit (!sl.hasPixelMask());
	AlwaysAssertExit (sl.isPaged());
	AlwaysAssertExit (!sl.isPersistent());
	AlwaysAssertExit (!sl.isWritable());
	AlwaysAssertExit (!sl.isMaskWritable());
	// A copy of the SubLattice.
	SubLattice<Int> sl1(sl, True);
	AlwaysAssertExit (sl1.isMasked());
	AlwaysAssertExit (!sl1.hasPixelMask());
	AlwaysAssertExit (sl1.isPaged());
	AlwaysAssertExit (!sl1.isPersistent());
	AlwaysAssertExit (!sl1.isWritable());
	AlwaysAssertExit (!sl1.isMaskWritable());
	AlwaysAssertExit (sl1.name(True) == "tSubLattice_tmp.pa");
    }
    {
        // A RW SubLattice as a masked Lattice.
	SubLattice<Int> sl(pa, mask, True);
	AlwaysAssertExit (sl.isMasked());
	AlwaysAssertExit (!sl.hasPixelMask());
	AlwaysAssertExit (sl.isPaged());
	AlwaysAssertExit (!sl.isPersistent());
	AlwaysAssertExit (sl.isWritable());
	AlwaysAssertExit (sl.isMaskWritable());
	// A RW copy of the SubLattice.
	SubLattice<Int> sl1(sl, True);
	AlwaysAssertExit (sl1.isMasked());
	AlwaysAssertExit (!sl1.hasPixelMask());
	AlwaysAssertExit (sl1.isPaged());
	AlwaysAssertExit (!sl1.isPersistent());
	AlwaysAssertExit (sl1.isWritable());
	AlwaysAssertExit (!sl1.isMaskWritable());
	// A RO copy of the SubLattice.
	SubLattice<Int> sl2(sl, False);
	AlwaysAssertExit (sl2.isMasked());
	AlwaysAssertExit (!sl2.hasPixelMask());
	AlwaysAssertExit (sl2.isPaged());
	AlwaysAssertExit (!sl2.isPersistent());
	AlwaysAssertExit (!sl2.isWritable());
	AlwaysAssertExit (!sl2.isMaskWritable());
    }
    {
        // A small region of a lattice.
	SubLattice<Int> sl(pa, slicer, True);
	AlwaysAssertExit (!sl.isMasked());
	AlwaysAssertExit (!sl.hasPixelMask());
	AlwaysAssertExit (sl.isPaged());
	AlwaysAssertExit (!sl.isPersistent());
	AlwaysAssertExit (sl.isWritable());
	AlwaysAssertExit (!sl.isMaskWritable());
	// A RW copy of the SubLattice.
	SubLattice<Int> sl1(sl, True);
	AlwaysAssertExit (!sl1.isMasked());
	AlwaysAssertExit (!sl1.hasPixelMask());
	AlwaysAssertExit (sl1.isPaged());
	AlwaysAssertExit (!sl1.isPersistent());
	AlwaysAssertExit (sl1.isWritable());
	AlwaysAssertExit (!sl1.isMaskWritable());
	// A RO copy of the SubLattice.
	SubLattice<Int> sl2(sl, False);
	AlwaysAssertExit (!sl2.isMasked());
	AlwaysAssertExit (!sl2.hasPixelMask());
	AlwaysAssertExit (sl2.isPaged());
	AlwaysAssertExit (!sl2.isPersistent());
	AlwaysAssertExit (!sl2.isWritable());
	AlwaysAssertExit (!sl2.isMaskWritable());
    }
    {
        // A full region of a lattice.
	SubLattice<Int> sl(pa, slfull, True);
	AlwaysAssertExit (!sl.isMasked());
	AlwaysAssertExit (!sl.hasPixelMask());
	AlwaysAssertExit (sl.isPaged());
	AlwaysAssertExit (sl.isPersistent());
	AlwaysAssertExit (sl.isWritable());
	AlwaysAssertExit (!sl.isMaskWritable());
	// A RW copy of the SubLattice.
	SubLattice<Int> sl1(sl, True);
	AlwaysAssertExit (!sl1.isMasked());
	AlwaysAssertExit (!sl1.hasPixelMask());
	AlwaysAssertExit (sl1.isPaged());
	AlwaysAssertExit (sl1.isPersistent());
	AlwaysAssertExit (sl1.isWritable());
	AlwaysAssertExit (!sl1.isMaskWritable());
	// A RO copy of the SubLattice.
	SubLattice<Int> sl2(sl, False);
	AlwaysAssertExit (!sl2.isMasked());
	AlwaysAssertExit (!sl2.hasPixelMask());
	AlwaysAssertExit (sl2.isPaged());
	AlwaysAssertExit (sl2.isPersistent());
	AlwaysAssertExit (!sl2.isWritable());
	AlwaysAssertExit (!sl2.isMaskWritable());
    }
}


main ()
{
  try {
    {
      const IPosition latticeShape(4, 16, 12, 4, 32);
      Array<Int> arr(latticeShape);
      indgen(arr);
      ArrayLattice<Int> lattice(arr);
      Slicer slicer(IPosition(4,4,2,1,3), IPosition(4,14,10,3,23),
		    IPosition(4,2,3,1,4), Slicer::endIsLast);
      SubLattice<Int> sublat (lattice, slicer, True);
      AlwaysAssertExit (!sublat.isPaged());
      AlwaysAssertExit (!sublat.isPersistent());
      AlwaysAssertExit (!sublat.isMasked());
      AlwaysAssertExit (sublat.isWritable());
      AlwaysAssertExit (sublat.shape() == slicer.length());
      Array<Int> arr1, arr2;
      sublat.getSlice (arr1, IPosition(4,0), sublat.shape(),
		       IPosition(4,1));
      lattice.getSlice (arr2, slicer);
      AlwaysAssertExit (allEQ(arr1, arr2));
      AlwaysAssertExit (allEQ(arr1,
			      arr(slicer.start(), slicer.end(),
				  slicer.stride())));
      testVectorROIter (sublat, lattice, slicer);

      // Test some other SubLattice functions.
      testRest();
    }
  } catch (AipsError x) {
    cerr << "Caught exception: " << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  } end_try;

  cout << "OK" << endl;
  return 0;
}
