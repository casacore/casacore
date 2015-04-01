//# tLCLELMask.cc:  mechanical test of the LCLELMask class
//# Copyright (C) 2000,2001,2002
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
#include <casacore/lattices/LRegions/LCLELMask.h>
#include <casacore/lattices/LEL/LatticeExpr.h>
#include <casacore/lattices/Lattices/LatticeStepper.h>
#include <casacore/lattices/Lattices/LatticeIterator.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
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
        // Static cast avoids an SGI compiler bug.
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


int main ()
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
    return 1;
  } 

   cout << "OK" << endl;
   return 0;
}
