//# tExtendLattice.cc: Test program for class ExtendLattice
//# Copyright (C) 2001
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

#include <casacore/lattices/Lattices/ExtendLattice.h>
#include <casacore/lattices/Lattices/SubLattice.h>
#include <casacore/lattices/Lattices/ArrayLattice.h>
#include <casacore/lattices/Lattices/PagedArray.h>
#include <casacore/lattices/Lattices/LatticeIterator.h>
#include <casacore/lattices/Lattices/LatticeStepper.h>
#include <casacore/lattices/LRegions/LCBox.h>
#include <casacore/lattices/LRegions/LCPixelSet.h>
#include <casacore/lattices/LRegions/LCPagedMask.h>
#include <casacore/casa/Arrays/AxesSpecifier.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
void testVectorROIter (const Lattice<Int>& extendlat,
		       const Lattice<Int>& lattice,
		       Int nnew)
{
  Int nstep;
  const IPosition latticeShape(extendlat.shape());
  const IPosition cursorShape(1,latticeShape(0));
  LatticeStepper step(latticeShape, cursorShape);
  RO_LatticeIterator<Int>  iter(extendlat, step);
  LatticeStepper step2(lattice.shape(), cursorShape);
  RO_LatticeIterator<Int>  iter2(lattice, step2);
  // static_cast's added for a workaround for an SGI compiler bug.
  for (iter2.reset(); !iter2.atEnd(); iter2++) {
    for (Int i=0; i<nnew; i++) {
      AlwaysAssert(allEQ(static_cast<Vector<Int> >(iter.vectorCursor()),
			 static_cast<Vector<Int> >(iter2.vectorCursor())),
		   AipsError);
      iter++;
    }
  }
  AlwaysAssert(iter.atEnd(), AipsError);
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
  PagedArray<Int> pa(IPosition(1,10), "tExtendLattice_tmp.pa");
  AlwaysAssertExit (pa.isPaged());
  AlwaysAssertExit (pa.isPersistent());
  AlwaysAssertExit (pa.isWritable());
  AlwaysAssertExit (pa.name(True) == "tExtendLattice_tmp.pa");
  LCPagedMask mask(IPosition(1,10), "tExtendLattice_tmp.pa/mask");
  {
    // Make an ExtendLattice.
    ExtendLattice<Int> sl(pa, IPosition(2,10,5), IPosition(1,1), IPosition());
    AlwaysAssertExit (!sl.isMasked());
    AlwaysAssertExit (!sl.hasPixelMask());
    AlwaysAssertExit (sl.isPaged());
    AlwaysAssertExit (!sl.isPersistent());
    AlwaysAssertExit (!sl.isWritable());
    AlwaysAssertExit (sl.name(True) == "tExtendLattice_tmp.pa");
    AlwaysAssertExit (sl.ndim() == 2);
    AlwaysAssertExit (sl.shape() == IPosition(2,10,5));
    AlwaysAssertExit (sl.niceCursorShape() == IPosition(2,10,1));
  }
  {
    // A RO ExtendLattice as a masked Lattice.
    SubLattice<Int> sp(pa, mask);
    ExtendLattice<Int> sl(sp, IPosition(2,10,5), IPosition(1,1), IPosition());
    AlwaysAssertExit (sl.isMasked());
    AlwaysAssertExit (!sl.hasPixelMask());
    AlwaysAssertExit (sl.isPaged());
    AlwaysAssertExit (!sl.isPersistent());
    AlwaysAssertExit (!sl.isWritable());
    AlwaysAssertExit (sl.name(True) == "tExtendLattice_tmp.pa");
    AlwaysAssertExit (sl.ndim() == 2);
    AlwaysAssertExit (sl.shape() == IPosition(2,10,5));
    AlwaysAssertExit (sl.niceCursorShape() == IPosition(2,10,1));
  }
}

void testMask()
{
  IPosition latticeShape(3,10,11,12);
  PagedArray<Int> pa(latticeShape, "tExtendLattice_tmp.pa");
  LCPagedMask mask(latticeShape, "tExtendLattice_tmp.pa/mask");
  Array<Int> arr(pa.shape());
  indgen(arr);
  pa.put (arr);
  Array<Bool> arrm(pa.shape());
  arrm = True;
  arrm(IPosition(3,0,0,0), IPosition(3,9,10,11), IPosition(3,2,1,1)) = False;
  mask.put (arrm);
  SubLattice<Int> lattice(pa, mask);
  ExtendLattice<Int> extendlat (lattice, IPosition(4,10,5,11,12),
				IPosition(1,1), IPosition());
  Array<Int> arr1 = extendlat.get();
  Array<Bool> arrm1 = extendlat.getMask();
  AlwaysAssertExit (arr1.shape() == extendlat.shape());
  AlwaysAssertExit (arrm1.shape() == extendlat.shape());
  for (Int i=0; i<5; i++) {
    Array<Int> parr = arr1(IPosition(4,0,i,0,0),
			   IPosition(4,10-1,i,11-1,12-1));
    AlwaysAssertExit (allEQ(parr.reform(latticeShape), arr));
    Array<Bool> parrm = arrm1(IPosition(4,0,i,0,0),
			      IPosition(4,10-1,i,11-1,12-1));
    AlwaysAssertExit (allEQ(parrm.reform(latticeShape), arrm));
  }
  for (Int i=0; i<5; i++) {
    Array<Int> parr = extendlat.getSlice (IPosition(4,0,i,0,0),
					  IPosition(4,10,1,11,12));
    AlwaysAssertExit (allEQ(parr.reform(latticeShape), arr));
    Array<Bool> parrm = extendlat.getMaskSlice (IPosition(4,0,i,0,0),
						IPosition(4,10,1,11,12));
    AlwaysAssertExit (allEQ(parrm.reform(latticeShape), arrm));
  }
}


int main ()
{
  try {
    {
      const IPosition latticeShape(4, 12, 1, 4, 32);
      Array<Int> arr(latticeShape);
      indgen(arr);
      ArrayLattice<Int> lattice(arr);
      {
	ExtendLattice<Int> extendlat (lattice, IPosition(5,12,3,4,4,32),
				      IPosition(1,1), IPosition(1,2));
	Array<Int> arr1 = extendlat.get();
	AlwaysAssertExit (arr1.shape() == extendlat.shape());
	for (Int i=0; i<3; i++) {
	  for (Int j=0; j<4; j++) {
	    Array<Int> parr = arr1(IPosition(5,0,i,j,0,0),
				   IPosition(5,12-1,i,j,4-1,32-1));
	    AlwaysAssertExit (allEQ(parr.reform(latticeShape), arr));
	  }
	}
	testVectorROIter (extendlat, lattice, 3*4);
      }
      {
	ExtendLattice<Int> extendlat (lattice, IPosition(5,12,3,4,4,32),
				      IPosition(1,2), IPosition(1,1));
	Array<Int> arr1 = extendlat.get();
	AlwaysAssertExit (arr1.shape() == extendlat.shape());
	for (Int i=0; i<3; i++) {
	  for (Int j=0; j<4; j++) {
	    Array<Int> parr = arr1(IPosition(5,0,i,j,0,0),
				   IPosition(5,12-1,i,j,4-1,32-1));
	    AlwaysAssertExit (allEQ(parr.reform(latticeShape), arr));
	  }
	}
	testVectorROIter (extendlat, lattice, 3*4);
      }
      {
	ExtendLattice<Int> extendlat (lattice, IPosition(5,12,1,4,4,32),
				      IPosition(1,2), IPosition());
	Array<Int> arr1 = extendlat.get();
	AlwaysAssertExit (arr1.shape() == extendlat.shape());
	for (Int i=0; i<1; i++) {
	  for (Int j=0; j<4; j++) {
	    Array<Int> parr = arr1(IPosition(5,0,i,j,0,0),
				   IPosition(5,12-1,i,j,4-1,32-1));
	    AlwaysAssertExit (allEQ(parr.reform(latticeShape), arr));
	  }
	}
	testVectorROIter (extendlat, lattice, 1*4);
      }
      {
	ExtendLattice<Int> extendlat (lattice, IPosition(4,12,6,4,32),
				      IPosition(), IPosition(1,1));
	Array<Int> arr1 = extendlat.get();
	AlwaysAssertExit (arr1.shape() == extendlat.shape());
	for (Int i=0; i<6; i++) {
	  Array<Int> parr = arr1(IPosition(4,0,i,0,0),
				 IPosition(4,12-1,i,4-1,32-1));
	  AlwaysAssertExit (allEQ(parr.reform(latticeShape), arr));
	}
	testVectorROIter (extendlat, lattice, 6);
      }
      {
	ExtendLattice<Int> extendlat (lattice, IPosition(6,12,3,4,6,32,5),
				      IPosition(2,3,5), IPosition(1,1));
	Array<Int> arr1 = extendlat.get();
	AlwaysAssertExit (arr1.shape() == extendlat.shape());
	for (Int i=0; i<3; i++) {
	  for (Int j=0; j<6; j++) {
	    for (Int k=0; k<5; k++) {
	      Array<Int> parr = arr1(IPosition(6,0,i,0,j,0,k),
				     IPosition(6,12-1,i,4-1,j,32-1,k));
	      AlwaysAssertExit (allEQ(parr.reform(latticeShape), arr));
	    }
	  }
	}
      }
    }
    // Test some other ExtendLattice functions.
    testRest();
    // Test mask handling
    testMask();
  } catch (AipsError x) {
    cerr << "Caught exception: " << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  } 

  cout << "OK" << endl;
  return 0;
}
