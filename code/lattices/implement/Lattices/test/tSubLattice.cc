//# tSubLattice.cc: Test program for class SubLattice
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

#include <trial/Lattices/SubLattice.h>
#include <trial/Lattices/ArrayLattice.h>
#include <trial/Lattices/LatticeIterator.h>
#include <trial/Lattices/LatticeStepper.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Lattices/IPosition.h>
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
        AlwaysAssert(allEQ(iter.vectorCursor().ac(),
			   iter2.vectorCursor().ac()), AipsError);
    }
    nstep = iter.nsteps();
    AlwaysAssert(nstep == latticeShape.product()/latticeShape(0),
		 AipsError);
    IPosition expectedPos(latticeShape-1);
    AlwaysAssert(iter.endPosition() == expectedPos, AipsError);
    expectedPos(0) = 0;
    AlwaysAssert(iter.position() == expectedPos, AipsError);
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
    }
  } catch (AipsError x) {
    cerr << "Caught exception: " << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  } end_try;

  cout << "OK" << endl;
  return 0;
}
