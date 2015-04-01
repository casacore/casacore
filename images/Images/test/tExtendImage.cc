//# tExtendImage.cc: Test program for class ExtendImage
//# Copyright (C) 2001,2003
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

#include <casacore/images/Images/ExtendImage.h>
#include <casacore/images/Images/PagedImage.h>
#include <casacore/images/Images/SubImage.h>
#include <casacore/images/Regions/ImageRegion.h>
#include <casacore/coordinates/Coordinates/CoordinateSystem.h>
#include <casacore/coordinates/Coordinates/CoordinateUtil.h>
#include <casacore/lattices/Lattices/LatticeIterator.h>
#include <casacore/lattices/Lattices/LatticeStepper.h>
#include <casacore/lattices/LRegions/LCBox.h>
#include <casacore/lattices/LRegions/LCPixelSet.h>
#include <casacore/lattices/LRegions/LCPagedMask.h>
#include <casacore/casa/Arrays/AxesSpecifier.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
void testVectorROIter (const Lattice<Float>& extendlat,
		       const Lattice<Float>& lattice,
		       Int nnew)
{
  Int nstep;
  const IPosition latticeShape(extendlat.shape());
  const IPosition cursorShape(1,latticeShape(0));
  LatticeStepper step(latticeShape, cursorShape);
  RO_LatticeIterator<Float>  iter(extendlat, step);
  LatticeStepper step2(lattice.shape(), cursorShape);
  RO_LatticeIterator<Float>  iter2(lattice, step2);
  // static_cast's added for a workaround for an SGI compiler bug.
  for (iter2.reset(); !iter2.atEnd(); iter2++) {
    for (Int i=0; i<nnew; i++) {
      AlwaysAssert(allEQ(static_cast<Vector<Float> >(iter.vectorCursor()),
			 static_cast<Vector<Float> >(iter2.vectorCursor())),
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
  CoordinateSystem cSys = CoordinateUtil::defaultCoords2D();
  CoordinateSystem cSys2 = CoordinateUtil::defaultCoords3D();
  PagedImage<Float> pa(IPosition(2,10,10), cSys, "tExtendImage_tmp.pa");
  AlwaysAssertExit (pa.isPaged());
  AlwaysAssertExit (pa.isPersistent());
  AlwaysAssertExit (pa.isWritable());
  AlwaysAssertExit (pa.name(True) == "tExtendImage_tmp.pa");
  LCPagedMask lcmask(IPosition(2,10,10), "tExtendImage_tmp.pa/mask");
  ImageRegion mask(lcmask);
  {
    // Make an ExtendImage.
    ExtendImage<Float> sl(pa, IPosition(3,10,10,5), cSys2);
    AlwaysAssertExit (!sl.isMasked());
    AlwaysAssertExit (!sl.hasPixelMask());
    AlwaysAssertExit (sl.isPaged());
    AlwaysAssertExit (!sl.isPersistent());
    // A copy of the ExtendImage.
    ExtendImage<Float> sl1(sl);
    AlwaysAssertExit (!sl1.isMasked());
    AlwaysAssertExit (!sl1.hasPixelMask());
    AlwaysAssertExit (sl1.isPaged());
    AlwaysAssertExit (!sl1.isPersistent());
    AlwaysAssertExit (!sl1.isWritable());
    AlwaysAssertExit (sl1.name(True) == "tExtendImage_tmp.pa");
  }
  {
    // An ExtendImage as a masked Lattice.
    SubImage<Float> sls(pa, mask);
    ExtendImage<Float> sl(sls, IPosition(3,10,10,1), cSys2);
    AlwaysAssertExit (sl.isMasked());
    AlwaysAssertExit (!sl.hasPixelMask());
    AlwaysAssertExit (sl.isPaged());
    AlwaysAssertExit (!sl.isPersistent());
    AlwaysAssertExit (!sl.isWritable());
    // A copy of the ExtendImage.
    ExtendImage<Float> sl1(sl);
    AlwaysAssertExit (sl1.isMasked());
    AlwaysAssertExit (!sl1.hasPixelMask());
    AlwaysAssertExit (sl1.isPaged());
    AlwaysAssertExit (!sl1.isPersistent());
    AlwaysAssertExit (!sl1.isWritable());
    AlwaysAssertExit (sl1.name(True) == "tExtendImage_tmp.pa");
  }
  {
    // An ExtendImage with an image mask.
    pa.defineRegion ("mask1", mask, RegionHandler::Masks);
    pa.setDefaultMask ("mask1");
    ExtendImage<Float> sl(pa, IPosition(3,10,10,2), cSys2);
    AlwaysAssertExit (sl.isMasked());
    AlwaysAssertExit (sl.hasPixelMask());
    AlwaysAssertExit (sl.isPaged());
    AlwaysAssertExit (!sl.isPersistent());
    AlwaysAssertExit (!sl.isWritable());
    // A copy of the ExtendImage.
    ExtendImage<Float> sl1(sl);
    AlwaysAssertExit (sl1.isMasked());
    AlwaysAssertExit (sl1.hasPixelMask());
    AlwaysAssertExit (sl1.isPaged());
    AlwaysAssertExit (!sl1.isPersistent());
    AlwaysAssertExit (!sl1.isWritable());
  }
}

void testMask()
{
  IPosition latticeShape(3,10,11,12);
  CoordinateSystem cSys2 = CoordinateUtil::defaultCoords3D();
  CoordinateSystem cSys3 = CoordinateUtil::defaultCoords4D();
  PagedImage<Float> pa(latticeShape, cSys2, "tExtendImage_tmp.pa");
  LCPagedMask mask(latticeShape, "tExtendImage_tmp.pa/mask");
  Array<Float> arr(pa.shape());
  indgen(arr);
  pa.put (arr);
  Array<Bool> arrm(pa.shape());
  arrm = True;
  arrm(IPosition(3,0,0,0), IPosition(3,9,10,11), IPosition(3,2,1,1)) = False;
  mask.put (arrm);
  pa.defineRegion ("mask1", mask, RegionHandler::Masks);
  pa.setDefaultMask ("mask1");
  ExtendImage<Float> extendlat(pa, IPosition(4,10,11,5,12), cSys3);
  Array<Float> arr1 = extendlat.get();
  Array<Bool> arrm1 = extendlat.getMask();
  AlwaysAssertExit (arr1.shape() == extendlat.shape());
  AlwaysAssertExit (arrm1.shape() == extendlat.shape());
  for (Int i=0; i<5; i++) {
    Array<Float> parr = arr1(IPosition(4,0,0,i,0),
			   IPosition(4,10-1,11-1,i,12-1));
    AlwaysAssertExit (allEQ(parr.reform(latticeShape), arr));
    Array<Bool> parrm = arrm1(IPosition(4,0,0,i,0),
			      IPosition(4,10-1,11-1,i,12-1));
    AlwaysAssertExit (allEQ(parrm.reform(latticeShape), arrm));
  }
  for (Int i=0; i<5; i++) {
    Array<Float> parr = extendlat.getSlice (IPosition(4,0,0,i,0),
					  IPosition(4,10,11,1,12));
    AlwaysAssertExit (allEQ(parr.reform(latticeShape), arr));
    Array<Bool> parrm = extendlat.getMaskSlice (IPosition(4,0,0,i,0),
						IPosition(4,10,11,1,12));
    AlwaysAssertExit (allEQ(parrm.reform(latticeShape), arrm));
  }
}


int main ()
{
  try {
    {
      const IPosition latticeShape(3, 16, 1, 6);
      CoordinateSystem cSys = CoordinateUtil::defaultCoords3D();
      const IPosition newShape(4, 16, 4, 3, 6);
      CoordinateSystem cSys2 = CoordinateUtil::defaultCoords4D();
      Array<Float> arr(latticeShape);
      indgen(arr);
      PagedImage<Float> lattice(latticeShape, cSys, "tExtendImage_tmp.pa");
      lattice.put (arr);
      ExtendImage<Float> extimg (lattice, newShape, cSys2);
      AlwaysAssertExit (extimg.isPaged());
      AlwaysAssertExit (!extimg.isPersistent());
      AlwaysAssertExit (!extimg.isMasked());
      AlwaysAssertExit (!extimg.isWritable());
      AlwaysAssertExit (extimg.shape() == newShape);
      Array<Float> arr1 = extimg.get();
      AlwaysAssertExit (arr1.shape() == extimg.shape());
      for (Int i=0; i<4; i++) {
	for (Int j=0; j<3; j++) {
	  Array<Float> parr = arr1(IPosition(4,0,i,j,0),
				 IPosition(4,16-1,i,j,6-1));
	  AlwaysAssertExit (allEQ(parr.reform(latticeShape), arr));
	}
      }
      testVectorROIter (extimg, lattice, 4*3);
    }
    // Test some other ExtendImage functions.
    testRest();
    // Test the axes removal..
    testMask();
  } catch (AipsError x) {
    cerr << "Caught exception: " << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  } 

  cout << "OK" << endl;
  return 0;
}
