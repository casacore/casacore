//# tTempLattice.cc
//# Copyright (C) 1996,1997,1998,1999,2000,2001
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


#include <casacore/lattices/Lattices/TempLattice.h>
#include <casacore/lattices/Lattices/LatticeIterator.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Utilities/COWPtr.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
void doIt (TempLattice<Int>& scratch)
{
    scratch.tempClose();
    IPosition shape(3,1);    
    shape(2) = scratch.shape()(2);
    AlwaysAssertExit (scratch.isWritable());
    scratch.tempClose();
    LatticeIterator<Int> li(scratch, shape);
    scratch.tempClose();
    Int i = 0;
    for (li.reset(); !li.atEnd(); li++, i++) {
	li.woCursor() = i;
    }
    shape = scratch.shape();
    shape(2) = 1;
    COWPtr<Array<Int> > ptrM;
    scratch.tempClose();
    scratch.getSlice(ptrM, IPosition(3,0), shape, IPosition(3,1), False);
    scratch.reopen();
    AlwaysAssert(ptrM->shape().isEqual(shape), AipsError);
    Array<Int> expectedResult(shape);
    indgen(expectedResult);
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
  } 
  cout << "OK" << endl;
  return 0;
}
