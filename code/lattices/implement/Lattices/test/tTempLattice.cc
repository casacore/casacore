//# tTempLattice.cc
//# Copyright (C) 1996,1997,1998
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


#include <trial/Lattices/TempLattice.h>
#include <trial/Lattices/LatticeIterator.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Utilities/COWPtr.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>
#include <iostream.h>


void doIt (TempLattice<Int>& scratch)
{
    IPosition shape(3,1);    
    shape(2) = scratch.shape()(2);
    AlwaysAssertExit (scratch.isWritable());
    LatticeIterator<Int> li(scratch, shape);
    Int i = 0;
    for (li.reset(); !li.atEnd(); li++, i++) {
	li.woCursor() = i;
    }
    shape = scratch.shape();
    shape(2) = 1;
    COWPtr<Array<Int> > ptrM;
    scratch.getSlice(ptrM, IPosition(3,0), shape, IPosition(3,1), False);
    AlwaysAssert(ptrM->shape().isEqual(shape), AipsError);
    Array<Int> expectedResult(shape);
    indgen(expectedResult.ac());
    AlwaysAssert(allEQ(*ptrM, expectedResult), AipsError);
    ptrM.rwRef() = 0;
    AlwaysAssert(allEQ(*ptrM, 0), AipsError);
    Slicer sl(IPosition(3,0,0,5), shape, IPosition(3,1));
    scratch.getSlice(ptrM, sl, False);
    AlwaysAssert(allEQ(*ptrM, expectedResult), AipsError);
    scratch.set(0);
    scratch.putAt (7, IPosition(3,7));
    AlwaysAssert(scratch.getAt(IPosition(3,0)) == 0, AipsError);
    AlwaysAssert(scratch.getAt(IPosition(3,7)) == 7, AipsError);
}

int main() {
  try {
    {
      TempLattice<Int> scratch(IPosition(3,64,64,257), 1);
      AlwaysAssertExit (scratch.isPaged());
      doIt (scratch);
    }
    {
      TempLattice<Int> small(IPosition(3,64,64,16), 1);
      AlwaysAssertExit (small.ok());
      AlwaysAssertExit (! small.isPaged());
      doIt (small);
    }
  } catch (AipsError x) {
    cerr << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  } end_try;
  cout << "OK" << endl;
  return 0;
}
