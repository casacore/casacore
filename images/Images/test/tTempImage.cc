//# tTempImage.cc: Test program for the TempImage class
//# Copyright (C) 1998,1999,2000,2001
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

#include <casacore/images/Images/TempImage.h>
#include <casacore/images/Images/SubImage.h>
#include <casacore/images/Images/ImageInfo.h>
#include <casacore/coordinates/Coordinates/CoordinateUtil.h>
#include <casacore/lattices/Lattices/TempLattice.h>
#include <casacore/lattices/Lattices/LatticeIterator.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/IO/ArrayIO.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Quanta/QLogical.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/IO/CanonicalIO.h>
#include <casacore/casa/IO/MemoryIO.h>
#include <casacore/casa/Utilities/COWPtr.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>

// Write and check the image.
void doIt (TempImage<int32_t>& scratch)
{
  IPosition shape(3,1);    
  shape(2) = scratch.shape()(2);
  AlwaysAssertExit (scratch.isWritable());
  LatticeIterator<int32_t> li(scratch, shape);
  int32_t i = 0;
  for (li.reset(); !li.atEnd(); li++, i++) {
    li.woCursor() = i;
  }
  shape = scratch.shape();
  shape(2) = 1;
  COWPtr<Array<int32_t> > ptrM;
  scratch.getSlice(ptrM, IPosition(3,0), shape, IPosition(3,1), false);
  AlwaysAssertExit (ptrM->shape().isEqual(shape));
  Array<int32_t> expectedResult(shape);
  indgen(expectedResult);
  AlwaysAssertExit (allEQ(*ptrM, expectedResult));
  ptrM.rwRef() = 0;
  AlwaysAssertExit (allEQ(*ptrM, 0));
  Slicer sl(IPosition(3,0,0,5), shape, IPosition(3,1));
  scratch.getSlice(ptrM, sl, false);
  AlwaysAssertExit (allEQ(*ptrM, expectedResult));
  scratch.set(0);
  scratch.putAt (7, IPosition(3,7));
  AlwaysAssertExit (scratch.getAt(IPosition(3,0)) == 0);
  AlwaysAssertExit (scratch.getAt(IPosition(3,7)) == 7);

  // Check if masking works fine.
  // To start with there should be no mask.
  TempLattice<bool> mask(scratch.shape());
  mask.set (true);
  mask.putAt (false, IPosition(3,7));
  AlwaysAssertExit (! scratch.isMasked());
  AlwaysAssertExit (! scratch.hasPixelMask());
  Array<bool> tm;
  scratch.getMaskSlice (tm, IPosition(3,1), IPosition(3,6));
  AlwaysAssertExit (allEQ (tm, true));

  // Now attach a mask and see if it is fine.
  scratch.attachMask (mask);
  AlwaysAssertExit (scratch.isMasked());
  AlwaysAssertExit (scratch.hasPixelMask());
  AlwaysAssertExit (scratch.pixelMask().isWritable());
  Array<bool> tm1;
  scratch.getMaskSlice (tm1, IPosition(3,1), IPosition(3,6));
  AlwaysAssertExit (allEQ (tm1, true));

  // Change the mask and see if it is reflected in the image's mask.
  mask.putAt (false, IPosition(3,7));
  tm1(IPosition(3,6)) = false;
  Array<bool> tm2;
  scratch.getMaskSlice (tm2, IPosition(3,1), IPosition(3,6));
  AlwaysAssertExit (allEQ (tm2, tm1));

  // Change the image mask directly and see if it is fine.
  scratch.pixelMask().putSlice (tm2, IPosition(3,0));
  Array<bool> tm3(IPosition(3,7));
  tm3 = true;
  tm1(IPosition(3,6)) = false;
  tm1(IPosition(3,7)) = false;
  Array<bool> tm3a;
  scratch.getMaskSlice (tm3a, IPosition(3,0), IPosition(3,7));
  AlwaysAssertExit (allEQ (tm3a, tm3));

  // Delete the mask
  scratch.removeMask();
  AlwaysAssertExit (!scratch.isMasked());
  AlwaysAssertExit (!scratch.hasPixelMask());

  // Test unit handling.
  scratch.setUnits (Unit("Jy"));
  AlwaysAssertExit (scratch.units() == Unit("Jy"));
  // Test info handling.
  ImageInfo info = scratch.imageInfo();
  AlwaysAssertExit (info.restoringBeam().isNull());
  Quantity a1(10.0,Unit("arcsec"));
  Quantity a2(8.0,Unit("arcsec"));
  Quantity a3(-45.0,Unit("deg"));
  info.setRestoringBeam(GaussianBeam(a1, a2, a3));
  scratch.setImageInfo(info);
  info = scratch.imageInfo();
  AlwaysAssertExit (info.restoringBeam().getMajor()==a1);
  AlwaysAssertExit (info.restoringBeam().getMinor()==a2);
  AlwaysAssertExit (info.restoringBeam().getPA()==a3);
}

// Stream, unstream, and check the image.
void streamImage (ImageInterface<int32_t>& img)
{
  MemoryIO membuf;
  CanonicalIO canio (&membuf);
  AipsIO os (&canio);
  // Write the image.
  os.putstart("Image", 1);
  {
    Record rec;
    String msg;
    AlwaysAssertExit (img.toRecord(msg, rec));
    os <<  rec;
                      }
  os << img.get();
  os << img.isMasked();
  if (img.isMasked()) {
    os << img.getMask();
  }
  os.putend();
  // Get the image back.
  TempImage<int32_t> scratch;
  os.setpos (0);
  AlwaysAssertExit (os.getstart("Image") == 0);
  {
    Record rec;
    String msg;
    os >> rec;
    AlwaysAssertExit (scratch.fromRecord(msg, rec));
  }
  {
    Array<int32_t> arr;
    os >> arr;
    scratch.put (arr);
  }
  bool isMasked;
  os >> isMasked;
  if (isMasked) {
    Array<bool> mask;
    os >> mask;
    scratch.attachMask (ArrayLattice<bool>(mask));
  }
  // Check the result.
  AlwaysAssertExit (scratch.getAt(IPosition(3,7)) == 7);
  scratch.putAt (0, IPosition(3,7));
  AlwaysAssertExit (allEQ(scratch.get(), 0));
  /*  // Check the mask.
  AlwaysAssertExit (scratch.isMasked());
  AlwaysAssertExit (scratch.hasPixelMask());
  AlwaysAssertExit (scratch.pixelMask().isWritable());
  Array<bool> tm1;
  scratch.getMaskSlice (tm1, IPosition(3,1), IPosition(3,6));
  tm1(IPosition(3,6)) = false;
  AlwaysAssertExit (allEQ (tm1, true));
  */
  // Test other info.
  AlwaysAssertExit (scratch.units() == Unit("Jy"));
  // Test info handling.
  Quantity a1(10.0,Unit("arcsec"));
  Quantity a2(8.0,Unit("arcsec"));
  Quantity a3(-45.0,Unit("deg"));
  ImageInfo info = scratch.imageInfo();
  AlwaysAssertExit (info.restoringBeam().getMajor()==a1);
  AlwaysAssertExit (info.restoringBeam().getMinor()==a2);
  AlwaysAssertExit (info.restoringBeam().getPA()==a3);
}

void testTempCloseDelete()
{
  int32_t nchan= 10;
  int32_t nx=1000;
  int32_t ny=1000;

  TempImage<float> tIm((TiledShape(IPosition(4,nx,ny,1,nchan))),
                       CoordinateUtil::defaultCoords4D(),
                       0);
  cerr <<"isPaged " << tIm.isPaged() << endl;
  tIm.set(0.0);
  /////////Comment the tempClose it works fine
  tIm.tempClose();
  IPosition blc(4,0 , 0, 0, nchan);
  IPosition trc(4, nx-1, ny-1, 0, nchan);
  Array<float> goodplane(IPosition(4, nx,ny,1,1), 0.0f);
  for (int32_t k=0; k < nchan ; ++k){
    blc(3)=k; trc(3)=k;
    Slicer sl(blc, trc, Slicer::endIsLast);
    SubImage<float> imSub(tIm, sl, true);
    goodplane += float(k);
    imSub.put(goodplane);
  }

  LatticeExprNode LEN = max( tIm );
  cerr << "max " << LEN.getFloat() << endl;
}

int main()
{
  try {
    {
      TempImage<int32_t> scratch((TiledShape(IPosition(3,64,64,257))),
			     CoordinateUtil::defaultCoords3D(),
			     1);
      AlwaysAssertExit (scratch.isPaged());
      doIt (scratch);
    }
    {
      TempImage<int32_t> small((TiledShape(IPosition(3,64,64,16))),
			   CoordinateUtil::defaultCoords3D(),
			   1);
      AlwaysAssertExit (small.ok());
      AlwaysAssertExit (! small.isPaged());
      doIt (small);
    }
    {
    	// per hyper-plane beam support
    	TempImage<int32_t> temp((
    		TiledShape(IPosition(4,64, 64, 4, 16))),
    		CoordinateUtil::defaultCoords4D()
    	);
    	ImageInfo info = temp.imageInfo();
    	Quantity maj(5, "arcsec");
    	Quantity min(3, "arcsec");
    	Quantity pa(30, "deg");
    	info.setAllBeams(16, 4, GaussianBeam());
    	try {
    		temp.setImageInfo(info);
    	}
    	catch (std::exception& x) {
    		cout << "Exception thrown as expected: "
    			<< x.what() << endl;
    	}
    	info.setBeam(0, 0, maj, min, pa);
    	try {
    		temp.setImageInfo(info);
    	}
    	catch (std::exception& x) {}
    	for (uint32_t i=0; i<4; i++) {
    		for (uint32_t j=0; j<16; j++) {
    			info.setBeam(j, i, maj, min, pa);
    		}
    	}
    	AlwaysAssert(temp.setImageInfo(info), AipsError);
    }
    testTempCloseDelete();
  } catch (std::exception& x) {
    cerr << x.what() << endl;
    cout << "FAIL" << endl;
    return 1;
  } 
  cout << "OK" << endl;
  return 0;
}
