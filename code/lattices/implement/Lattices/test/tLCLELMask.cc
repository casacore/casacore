//# tLCLELMask.cc:  mechanical test of the LCLELMask class
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
#include <trial/Lattices/LCLELMask.h>
#include <trial/Lattices/LatticeExpr.h>
#include <aips/Lattices/LatticeStepper.h>
#include <aips/Lattices/LatticeIterator.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/IPosition.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>


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
    IPosition latticeShape(2, 4, 8);
    Array<Float> arr(latticeShape);
    indgen (arr);
    ArrayLattice<Float> arrlat(arr);
    {
      LCLELMask mask(fmod(floor(arrlat/4), 2) == 0);
      AlwaysAssertExit (mask.hasMask());
      AlwaysAssertExit (! mask.isWritable());
      AlwaysAssertExit (mask.shape() == latticeShape);
      // Check the mask values using the iterator.
      testVectorROIter (mask, True, True);

      LCLELMask mask1(fmod(floor(arrlat/4), 2) != 0);
      LCLELMask mask2(mask1);
      AlwaysAssertExit (mask2.hasMask());
      AlwaysAssertExit (! mask2.isWritable());
      AlwaysAssertExit (mask2.shape() == latticeShape);
      testVectorROIter (mask2, False, True);

      mask1 = mask;
      AlwaysAssertExit (mask1.hasMask());
      AlwaysAssertExit (! mask1.isWritable());
      AlwaysAssertExit (mask1.shape() == latticeShape);
      testVectorROIter (mask, True, True);

      AlwaysAssertExit (mask1 != mask2);
    }
  } catch (AipsError x) {
    cerr << "Caught exception: " << x.getMesg() << endl;
    exit(1);
  } 

   cout << "OK" << endl;
   exit(0);
}
