//# tTempImage.cc: Test program for the TempImage class
//# Copyright (C) 1998,1999,2000
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or(at your option)
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

#include <trial/Images/TempImage.h>
#include <trial/Images/ImageInfo.h>
#include <trial/Coordinates/CoordinateUtil.h>
#include <aips/Lattices/TempLattice.h>
#include <aips/Lattices/LatticeIterator.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/IPosition.h>
#include <aips/Quanta/QLogical.h>
#include <aips/Utilities/COWPtr.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>
#include <iostream.h>


void doIt (TempImage<Int>& scratch)
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
  AlwaysAssertExit (ptrM->shape().isEqual(shape));
  Array<Int> expectedResult(shape);
  indgen(expectedResult);
  AlwaysAssertExit (allEQ(*ptrM, expectedResult));
  ptrM.rwRef() = 0;
  AlwaysAssertExit (allEQ(*ptrM, 0));
  Slicer sl(IPosition(3,0,0,5), shape, IPosition(3,1));
  scratch.getSlice(ptrM, sl, False);
  AlwaysAssertExit (allEQ(*ptrM, expectedResult));
  scratch.set(0);
  scratch.putAt (7, IPosition(3,7));
  AlwaysAssertExit (scratch.getAt(IPosition(3,0)) == 0);
  AlwaysAssertExit (scratch.getAt(IPosition(3,7)) == 7);

  // Check if masking works fine.
  // To start with there should be no mask.
  TempLattice<Bool> mask(scratch.shape());
  mask.set (True);
  mask.putAt (False, IPosition(3,7));
  AlwaysAssertExit (! scratch.isMasked());
  AlwaysAssertExit (! scratch.hasPixelMask());
  AlwaysAssertExit (! scratch.isMaskWritable());
  AlwaysAssertExit (scratch.getRegionPtr() == 0);
  Array<Bool> tm;
  scratch.getMaskSlice (tm, IPosition(3,1), IPosition(3,6));
  AlwaysAssertExit (allEQ (tm, True));

  // Now attach a mask and see if it is fine.
  scratch.attachMask (mask);
  AlwaysAssertExit (scratch.isMasked());
  AlwaysAssertExit (scratch.hasPixelMask());
  AlwaysAssertExit (scratch.isMaskWritable());
  AlwaysAssertExit (scratch.getRegionPtr() == 0);
  Array<Bool> tm1;
  scratch.getMaskSlice (tm1, IPosition(3,1), IPosition(3,6));
  AlwaysAssertExit (allEQ (tm1, True));

  // Change the mask and see if it is reflected in the image's mask.
  mask.putAt (False, IPosition(3,7));
  tm1(IPosition(3,6)) = False;
  Array<Bool> tm2;
  scratch.getMaskSlice (tm2, IPosition(3,1), IPosition(3,6));
  AlwaysAssertExit (allEQ (tm2, tm1));

  // Change the image mask directly and see if it is fine.
  scratch.putMaskSlice (tm2, IPosition(3,0));
  Array<Bool> tm3(IPosition(3,7));
  tm3 = True;
  tm1(IPosition(3,6)) = False;
  tm1(IPosition(3,7)) = False;
  Array<Bool> tm3a;
  scratch.getMaskSlice (tm3a, IPosition(3,0), IPosition(3,7));
  AlwaysAssertExit (allEQ (tm3a, tm3));

  // Test unit handling.
  scratch.setUnits (Unit("Jy"));
  AlwaysAssertExit (scratch.units() == Unit("Jy"));
  // Test info handling.
  ImageInfo info = scratch.imageInfo();
  AlwaysAssertExit (info.restoringBeam().nelements()==0);
  Quantum<Double> a1(10.0,Unit("arcsec"));
  Quantum<Double> a2(8.0,Unit("arcsec"));
  Quantum<Double> a3(-45.0,Unit("deg"));
  info.setRestoringBeam(a1, a2, a3);
  scratch.setImageInfo(info);
  info = scratch.imageInfo();
  AlwaysAssertExit (info.restoringBeam()(0)==a1);
  AlwaysAssertExit (info.restoringBeam()(1)==a2);
  AlwaysAssertExit (info.restoringBeam()(2)==a3);
}

main()
{
  try {
    {
      TempImage<Int> scratch(TiledShape(IPosition(3,64,64,257)),
			     CoordinateUtil::defaultCoords3D(),
			     1);
      AlwaysAssertExit (scratch.isPaged());
      doIt (scratch);
    }
    {
      TempImage<Int> small(TiledShape(IPosition(3,64,64,16)),
			   CoordinateUtil::defaultCoords3D(),
			   1);
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
