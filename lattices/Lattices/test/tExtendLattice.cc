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
void testVectorROIter (const Lattice<int32_t>& extendlat,
		       const Lattice<int32_t>& lattice,
		       int32_t nnew)
{
  int32_t nstep;
  const IPosition latticeShape(extendlat.shape());
  const IPosition cursorShape(1,latticeShape(0));
  LatticeStepper step(latticeShape, cursorShape);
  RO_LatticeIterator<int32_t>  iter(extendlat, step);
  LatticeStepper step2(lattice.shape(), cursorShape);
  RO_LatticeIterator<int32_t>  iter2(lattice, step2);
  // static_cast's added for a workaround for an SGI compiler bug.
  for (iter2.reset(); !iter2.atEnd(); iter2++) {
    for (int32_t i=0; i<nnew; i++) {
      AlwaysAssert(allEQ(static_cast<Vector<int32_t> >(iter.vectorCursor()),
			 static_cast<Vector<int32_t> >(iter2.vectorCursor())),
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
  PagedArray<int32_t> pa(IPosition(1,10), "tExtendLattice_tmp.pa");
  AlwaysAssertExit (pa.isPaged());
  AlwaysAssertExit (pa.isPersistent());
  AlwaysAssertExit (pa.isWritable());
  AlwaysAssertExit (pa.name(true) == "tExtendLattice_tmp.pa");
  LCPagedMask mask(IPosition(1,10), "tExtendLattice_tmp.pa/mask");
  {
    // Make an ExtendLattice.
    ExtendLattice<int32_t> sl(pa, IPosition(2,10,5), IPosition(1,1), IPosition());
    AlwaysAssertExit (!sl.isMasked());
    AlwaysAssertExit (!sl.hasPixelMask());
    AlwaysAssertExit (sl.isPaged());
    AlwaysAssertExit (!sl.isPersistent());
    AlwaysAssertExit (!sl.isWritable());
    AlwaysAssertExit (sl.name(true) == "tExtendLattice_tmp.pa");
    AlwaysAssertExit (sl.ndim() == 2);
    AlwaysAssertExit (sl.shape() == IPosition(2,10,5));
    AlwaysAssertExit (sl.niceCursorShape() == IPosition(2,10,1));
  }
  {
    // A RO ExtendLattice as a masked Lattice.
    SubLattice<int32_t> sp(pa, mask);
    ExtendLattice<int32_t> sl(sp, IPosition(2,10,5), IPosition(1,1), IPosition());
    AlwaysAssertExit (sl.isMasked());
    AlwaysAssertExit (!sl.hasPixelMask());
    AlwaysAssertExit (sl.isPaged());
    AlwaysAssertExit (!sl.isPersistent());
    AlwaysAssertExit (!sl.isWritable());
    AlwaysAssertExit (sl.name(true) == "tExtendLattice_tmp.pa");
    AlwaysAssertExit (sl.ndim() == 2);
    AlwaysAssertExit (sl.shape() == IPosition(2,10,5));
    AlwaysAssertExit (sl.niceCursorShape() == IPosition(2,10,1));
  }
}

void testMask()
{
  IPosition latticeShape(3,10,11,12);
  PagedArray<int32_t> pa(latticeShape, "tExtendLattice_tmp.pa");
  LCPagedMask mask(latticeShape, "tExtendLattice_tmp.pa/mask");
  Array<int32_t> arr(pa.shape());
  indgen(arr);
  pa.put (arr);
  Array<bool> arrm(pa.shape());
  arrm = true;
  arrm(IPosition(3,0,0,0), IPosition(3,9,10,11), IPosition(3,2,1,1)) = false;
  mask.put (arrm);
  SubLattice<int32_t> lattice(pa, mask);
  ExtendLattice<int32_t> extendlat (lattice, IPosition(4,10,5,11,12),
				IPosition(1,1), IPosition());
  Array<int32_t> arr1 = extendlat.get();
  Array<bool> arrm1 = extendlat.getMask();
  AlwaysAssertExit (arr1.shape() == extendlat.shape());
  AlwaysAssertExit (arrm1.shape() == extendlat.shape());
  for (int32_t i=0; i<5; i++) {
    Array<int32_t> parr = arr1(IPosition(4,0,i,0,0),
			   IPosition(4,10-1,i,11-1,12-1));
    AlwaysAssertExit (allEQ(parr.reform(latticeShape), arr));
    Array<bool> parrm = arrm1(IPosition(4,0,i,0,0),
			      IPosition(4,10-1,i,11-1,12-1));
    AlwaysAssertExit (allEQ(parrm.reform(latticeShape), arrm));
  }
  for (int32_t i=0; i<5; i++) {
    Array<int32_t> parr = extendlat.getSlice (IPosition(4,0,i,0,0),
					  IPosition(4,10,1,11,12));
    AlwaysAssertExit (allEQ(parr.reform(latticeShape), arr));
    Array<bool> parrm = extendlat.getMaskSlice (IPosition(4,0,i,0,0),
						IPosition(4,10,1,11,12));
    AlwaysAssertExit (allEQ(parrm.reform(latticeShape), arrm));
  }
}


int main ()
{
  try {
    {
      const IPosition latticeShape(4, 12, 1, 4, 32);
      Array<int32_t> arr(latticeShape);
      indgen(arr);
      ArrayLattice<int32_t> lattice(arr);
      {
	ExtendLattice<int32_t> extendlat (lattice, IPosition(5,12,3,4,4,32),
				      IPosition(1,1), IPosition(1,2));
	Array<int32_t> arr1 = extendlat.get();
	AlwaysAssertExit (arr1.shape() == extendlat.shape());
	for (int32_t i=0; i<3; i++) {
	  for (int32_t j=0; j<4; j++) {
	    Array<int32_t> parr = arr1(IPosition(5,0,i,j,0,0),
				   IPosition(5,12-1,i,j,4-1,32-1));
	    AlwaysAssertExit (allEQ(parr.reform(latticeShape), arr));
	  }
	}
	testVectorROIter (extendlat, lattice, 3*4);
      }
      {
	ExtendLattice<int32_t> extendlat (lattice, IPosition(5,12,3,4,4,32),
				      IPosition(1,2), IPosition(1,1));
	Array<int32_t> arr1 = extendlat.get();
	AlwaysAssertExit (arr1.shape() == extendlat.shape());
	for (int32_t i=0; i<3; i++) {
	  for (int32_t j=0; j<4; j++) {
	    Array<int32_t> parr = arr1(IPosition(5,0,i,j,0,0),
				   IPosition(5,12-1,i,j,4-1,32-1));
	    AlwaysAssertExit (allEQ(parr.reform(latticeShape), arr));
	  }
	}
	testVectorROIter (extendlat, lattice, 3*4);
      }
      {
	ExtendLattice<int32_t> extendlat (lattice, IPosition(5,12,1,4,4,32),
				      IPosition(1,2), IPosition());
	Array<int32_t> arr1 = extendlat.get();
	AlwaysAssertExit (arr1.shape() == extendlat.shape());
	for (int32_t i=0; i<1; i++) {
	  for (int32_t j=0; j<4; j++) {
	    Array<int32_t> parr = arr1(IPosition(5,0,i,j,0,0),
				   IPosition(5,12-1,i,j,4-1,32-1));
	    AlwaysAssertExit (allEQ(parr.reform(latticeShape), arr));
	  }
	}
	testVectorROIter (extendlat, lattice, 1*4);
      }
      {
	ExtendLattice<int32_t> extendlat (lattice, IPosition(4,12,6,4,32),
				      IPosition(), IPosition(1,1));
	Array<int32_t> arr1 = extendlat.get();
	AlwaysAssertExit (arr1.shape() == extendlat.shape());
	for (int32_t i=0; i<6; i++) {
	  Array<int32_t> parr = arr1(IPosition(4,0,i,0,0),
				 IPosition(4,12-1,i,4-1,32-1));
	  AlwaysAssertExit (allEQ(parr.reform(latticeShape), arr));
	}
	testVectorROIter (extendlat, lattice, 6);
      }
      {
	ExtendLattice<int32_t> extendlat (lattice, IPosition(6,12,3,4,6,32,5),
				      IPosition(2,3,5), IPosition(1,1));
	Array<int32_t> arr1 = extendlat.get();
	AlwaysAssertExit (arr1.shape() == extendlat.shape());
	for (int32_t i=0; i<3; i++) {
	  for (int32_t j=0; j<6; j++) {
	    for (int32_t k=0; k<5; k++) {
	      Array<int32_t> parr = arr1(IPosition(6,0,i,0,j,0,k),
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
  } catch (std::exception& x) {
    cerr << "Caught exception: " << x.what() << endl;
    cout << "FAIL" << endl;
    return 1;
  } 

  cout << "OK" << endl;
  return 0;
}
