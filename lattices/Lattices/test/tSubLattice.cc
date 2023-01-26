//# tSubLattice.cc: Test program for class SubLattice
//# Copyright (C) 1998,1999,2000,2001,2003
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
#include <casacore/casa/Inputs/Input.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
void testVectorROIter (const Lattice<int32_t>& sublat,
		       const Lattice<int32_t>& lattice,
		       const Slicer& slicer)
{
    int32_t nstep;
    const IPosition latticeShape(sublat.shape());
    const IPosition cursorShape(1,latticeShape(0));
    LatticeStepper step(latticeShape, cursorShape);
    RO_LatticeIterator<int32_t>  iter(sublat, step);
    LatticeStepper step2(lattice.shape(), cursorShape);
    step2.subSection (slicer.start(), slicer.end(), slicer.stride());
    RO_LatticeIterator<int32_t>  iter2(lattice, step2);
      // static_cast's added for a workaround for an SGI compiler bug.
    for (iter.reset(); !iter.atEnd(); iter++, iter2++){
        AlwaysAssert(allEQ(static_cast<Vector<int32_t> >(iter.vectorCursor()),
			   static_cast<Vector<int32_t> >(iter2.vectorCursor())), AipsError);
    }
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
    PagedArray<int32_t> pa(IPosition(1,10), "tSubLattice_tmp.pa");
    AlwaysAssertExit (pa.isPaged());
    AlwaysAssertExit (pa.isPersistent());
    AlwaysAssertExit (pa.isWritable());
    AlwaysAssertExit (pa.name(true) == "tSubLattice_tmp.pa");
    LCPagedMask mask(IPosition(1,10), "tSubLattice_tmp.pa/mask");
    Slicer slicer(IPosition(1,1), IPosition(1,3));
    Slicer slfull(IPosition(1,0), IPosition(1,10));
    {
        // A SubLattice as a Lattice copy (RO).
	SubLattice<int32_t> sl(pa);
	AlwaysAssertExit (!sl.isMasked());
	AlwaysAssertExit (!sl.hasPixelMask());
	AlwaysAssertExit (sl.isPaged());
	AlwaysAssertExit (sl.isPersistent());
	AlwaysAssertExit (!sl.isWritable());
	// A copy of the SubLattice.
	SubLattice<int32_t> sl1(sl, true);
	AlwaysAssertExit (!sl1.isMasked());
	AlwaysAssertExit (!sl1.hasPixelMask());
	AlwaysAssertExit (sl1.isPaged());
	AlwaysAssertExit (sl1.isPersistent());
	AlwaysAssertExit (!sl1.isWritable());
	AlwaysAssertExit (sl1.name(true) == "tSubLattice_tmp.pa");
    }
    {
        // A SubLattice as a Lattice copy (RW).
	SubLattice<int32_t> sl(pa, true);
	AlwaysAssertExit (!sl.isMasked());
	AlwaysAssertExit (!sl.hasPixelMask());
	AlwaysAssertExit (sl.isPaged());
	AlwaysAssertExit (sl.isPersistent());
	AlwaysAssertExit (sl.isWritable());
	// A RW copy of the SubLattice.
	SubLattice<int32_t> sl1(sl, true);
	AlwaysAssertExit (!sl1.isMasked());
	AlwaysAssertExit (!sl1.hasPixelMask());
	AlwaysAssertExit (sl1.isPaged());
	AlwaysAssertExit (sl1.isPersistent());
	AlwaysAssertExit (sl1.isWritable());
	// A RO copy of the SubLattice.
	SubLattice<int32_t> sl2(sl, false);
	AlwaysAssertExit (!sl2.isMasked());
	AlwaysAssertExit (!sl2.hasPixelMask());
	AlwaysAssertExit (sl2.isPaged());
	AlwaysAssertExit (sl2.isPersistent());
	AlwaysAssertExit (!sl2.isWritable());
	AlwaysAssertExit (sl2.name(true) == "tSubLattice_tmp.pa");
    }
    {
        // A RO SubLattice as a masked Lattice.
	SubLattice<int32_t> sl(pa, mask);
	AlwaysAssertExit (sl.isMasked());
	AlwaysAssertExit (!sl.hasPixelMask());
	AlwaysAssertExit (sl.isPaged());
	AlwaysAssertExit (!sl.isPersistent());
	AlwaysAssertExit (!sl.isWritable());
	// A copy of the SubLattice.
	SubLattice<int32_t> sl1(sl, true);
	AlwaysAssertExit (sl1.isMasked());
	AlwaysAssertExit (!sl1.hasPixelMask());
	AlwaysAssertExit (sl1.isPaged());
	AlwaysAssertExit (!sl1.isPersistent());
	AlwaysAssertExit (!sl1.isWritable());
	AlwaysAssertExit (sl1.name(true) == "tSubLattice_tmp.pa");
    }
    {
        // A RW SubLattice as a masked Lattice.
	SubLattice<int32_t> sl(pa, mask, true);
	AlwaysAssertExit (sl.isMasked());
	AlwaysAssertExit (!sl.hasPixelMask());
	AlwaysAssertExit (sl.isPaged());
	AlwaysAssertExit (!sl.isPersistent());
	AlwaysAssertExit (sl.isWritable());
	// A RW copy of the SubLattice.
	SubLattice<int32_t> sl1(sl, true);
	AlwaysAssertExit (sl1.isMasked());
	AlwaysAssertExit (!sl1.hasPixelMask());
	AlwaysAssertExit (sl1.isPaged());
	AlwaysAssertExit (!sl1.isPersistent());
	AlwaysAssertExit (sl1.isWritable());
	// A RO copy of the SubLattice.
	SubLattice<int32_t> sl2(sl, false);
	AlwaysAssertExit (sl2.isMasked());
	AlwaysAssertExit (!sl2.hasPixelMask());
	AlwaysAssertExit (sl2.isPaged());
	AlwaysAssertExit (!sl2.isPersistent());
	AlwaysAssertExit (!sl2.isWritable());
    }
    {
        // A small region of a lattice.
	SubLattice<int32_t> sl(pa, slicer, true);
	AlwaysAssertExit (!sl.isMasked());
	AlwaysAssertExit (!sl.hasPixelMask());
	AlwaysAssertExit (sl.isPaged());
	AlwaysAssertExit (!sl.isPersistent());
	AlwaysAssertExit (sl.isWritable());
	// A RW copy of the SubLattice.
	SubLattice<int32_t> sl1(sl, true);
	AlwaysAssertExit (!sl1.isMasked());
	AlwaysAssertExit (!sl1.hasPixelMask());
	AlwaysAssertExit (sl1.isPaged());
	AlwaysAssertExit (!sl1.isPersistent());
	AlwaysAssertExit (sl1.isWritable());
	// A RO copy of the SubLattice.
	SubLattice<int32_t> sl2(sl, false);
	AlwaysAssertExit (!sl2.isMasked());
	AlwaysAssertExit (!sl2.hasPixelMask());
	AlwaysAssertExit (sl2.isPaged());
	AlwaysAssertExit (!sl2.isPersistent());
	AlwaysAssertExit (!sl2.isWritable());
    }
    {
        // A full region of a lattice.
	SubLattice<int32_t> sl(pa, slfull, true);
	AlwaysAssertExit (!sl.isMasked());
	AlwaysAssertExit (!sl.hasPixelMask());
	AlwaysAssertExit (sl.isPaged());
	AlwaysAssertExit (sl.isPersistent());
	AlwaysAssertExit (sl.isWritable());
	// A RW copy of the SubLattice.
	SubLattice<int32_t> sl1(sl, true);
	AlwaysAssertExit (!sl1.isMasked());
	AlwaysAssertExit (!sl1.hasPixelMask());
	AlwaysAssertExit (sl1.isPaged());
	AlwaysAssertExit (sl1.isPersistent());
	AlwaysAssertExit (sl1.isWritable());
	// A RO copy of the SubLattice.
	SubLattice<int32_t> sl2(sl, false);
	AlwaysAssertExit (!sl2.isMasked());
	AlwaysAssertExit (!sl2.hasPixelMask());
	AlwaysAssertExit (sl2.isPaged());
	AlwaysAssertExit (sl2.isPersistent());
	AlwaysAssertExit (!sl2.isWritable());
    }
}

void testAxes()
{
    PagedArray<int32_t> pa(IPosition(3,10,11,12), "tSubLattice_tmp.pa");
    LCPagedMask mask(IPosition(3,10,11,12), "tSubLattice_tmp.pa/mask");
    Array<int32_t> arr(pa.shape());
    indgen(arr);
    pa.put (arr);
    Array<bool> m(pa.shape());
    m = true;
    m(IPosition(3,0,0,0), IPosition(3,9,10,11), IPosition(3,2,1,1)) = false;
    mask.put (m);
    {
      // Create a sublattice with the mask and assign a pixelmask to it.
      SubLattice<int32_t> ml(pa, mask);
      Array<bool> pm(pa.shape());
      pm = true;
      pm(IPosition(3,0,0,0), IPosition(3,9,10,11), IPosition(3,3,2,1)) = false;
      ml.setPixelMask (ArrayLattice<bool>(pm), false);
      AlwaysAssertExit (ml.hasPixelMask());
      // Test if the mask read is correct.
      AlwaysAssertExit (allEQ (ml.getMask(), m&&pm));
      AlwaysAssertExit (allEQ (ml.pixelMask().get(), pm));
      // Copy constructor.
      SubLattice<int32_t> ml2(ml);
      AlwaysAssertExit (allEQ (ml2.pixelMask().get(), pm));
      AlwaysAssertExit (allEQ (ml2.getMask(), m&&pm));
      // Assign another pixelmask.
      pm = true;
      pm(IPosition(3,0,0,0), IPosition(3,9,10,11), IPosition(3,5,1,2)) = false;
      ml2.setPixelMask (ArrayLattice<bool>(pm), false);
      AlwaysAssertExit (allEQ (ml2.getMask(), m&&pm));
      AlwaysAssertExit (allEQ (ml2.pixelMask().get(), pm));
      // Now make a sublattice from a MaskedLattice.
      ml2 = SubLattice<int32_t> (ml2, AxesSpecifier());
      AlwaysAssertExit (allEQ (ml2.getMask(), m&&pm));
      AlwaysAssertExit (allEQ (ml2.pixelMask().get(), pm));
      // Assign another pixelmask.
      Array<bool> pm2(pa.shape());
      pm2 = false;
      pm2(IPosition(3,0,0,0), IPosition(3,9,10,11), IPosition(3,7,1,1)) = true;
      // The first one should fail.
      bool exc = false;
      try {
	ml2.setPixelMask (ArrayLattice<bool>(pm2), false);
      } catch (std::exception& x) {
	exc = true;
      }
      AlwaysAssertExit (exc);
      ml2.setPixelMask (ArrayLattice<bool>(pm2), true);
      AlwaysAssertExit (allEQ (ml2.pixelMask().get(), pm&&pm2));
      AlwaysAssertExit (allEQ (ml2.getMask(), m&&pm&&pm2));
    }
    Array<int32_t> arrs1 = arr(IPosition(3,3,1,2), IPosition(3,8,1,9));
    Array<int32_t> arrsub = arrs1.reform(IPosition(2,6,8));
    Array<bool> ms1 = m(IPosition(3,3,1,2), IPosition(3,8,1,9));
    Array<bool> msub = ms1.reform(IPosition(2,6,8));
    // Make sublattice with a removed axis 1.
    SubLattice<int32_t> ml(pa, mask, true);
    Array<bool> pixmask(IPosition(3,6,1,8));
    pixmask = true;
    pixmask (IPosition(3,0,0,0)) = !msub(IPosition(2,0,0));
    LCPixelSet pixset (pixmask, LCBox(IPosition(3,3,1,2),
				      IPosition(3,8,1,9),
				      m.shape()));
    SubLattice<int32_t> sl(ml, pixset, true, AxesSpecifier(false));

    IPosition ncs (pa.niceCursorShape());
    // Test if shape and niceCursorShape remove the axis.
    AlwaysAssertExit (sl.shape() == IPosition(2,6,8));
    AlwaysAssertExit (sl.niceCursorShape() == IPosition(2, min(6,ncs(0)),
							min(8,ncs(2))));
    // Test the getting functions.
    Array<int32_t> arrsl = sl.get();
    AlwaysAssertExit (allEQ (arrsl, arrsub));
    AlwaysAssertExit (sl.getAt(IPosition(2,1,3)) == arrsub(IPosition(2,1,3)));
    arrsub(IPosition(2,1,3)) += 10;
    // Test the put function and see if the result matches.
    sl.putAt(arrsub(IPosition(2,1,3)), IPosition(2,1,3));
    AlwaysAssertExit (sl.getAt(IPosition(2,1,3)) == arrsub(IPosition(2,1,3)));
    AlwaysAssertExit (allEQ (sl.get(), arrsub));
    arrsub(IPosition(2,1,3)) += 10;
    sl.put (arrsub);
    AlwaysAssertExit (allEQ (sl.get(), arrsub));
    AlwaysAssertExit (sl.getAt(IPosition(2,1,3)) == arrsub(IPosition(2,1,3)));
    // Now get and put a slice.
    Array<int32_t> arrsubsub = arrsub(IPosition(2,1,2),
				  IPosition(2,5,7),
				  IPosition(2,3,2));
    AlwaysAssertExit (allEQ (sl.getSlice(Slicer(IPosition(2,1,2),
						IPosition(2,5,7),
						IPosition(2,3,2),
						Slicer::endIsLast)),
			     arrsubsub));
    arrsubsub += 20;
    sl.putSlice (arrsubsub, IPosition(2,1,2), IPosition(2,3,2));
    AlwaysAssertExit (allEQ (sl.get(), arrsub));
    // Test the get mask functions.
    AlwaysAssertExit (allEQ (ml.getMask(), m));
    msub(IPosition(2,0,0)) = !msub(IPosition(2,0,0));
    AlwaysAssertExit (allEQ (sl.getMask(), msub));
    Array<bool> msubsub = msub(IPosition(2,1,2),
			       IPosition(2,5,7),
			       IPosition(2,3,2));
    AlwaysAssertExit (allEQ (sl.getMaskSlice(Slicer(IPosition(2,1,2),
						    IPosition(2,5,7),
						    IPosition(2,3,2),
						    Slicer::endIsLast)),
			     msubsub));
    // Assign a pixelmask to this sublattice.
    Array<bool> slm = sl.getMask();
    Array<bool> pm(sl.shape());
    pm = true;
    pm(IPosition(2,0,0), IPosition(2,1,2), IPosition(2,2,4)) = false;
    sl.setPixelMask (ArrayLattice<bool>(pm), false);
    AlwaysAssertExit (allEQ (sl.getMask(), slm&&pm));
}


void testAdd (Lattice<int32_t>& lat1, Lattice<int32_t>& lat2, bool useRef)
{
  {
    PagedArray<int32_t>* pa1 = dynamic_cast<PagedArray<int32_t>*> (&lat1);
    if (pa1) pa1->clearCache();
    PagedArray<int32_t>* pa2 = dynamic_cast<PagedArray<int32_t>*> (&lat2);
    if (pa2) pa2->clearCache();
    Timer timer;
    LatticeIterator<int32_t> lat1Iter (lat1, useRef);
    // Create dummy lat2Iter to setup cache correctly.
    // It may not be necessary, because the Table getSlice function
    // will setup the cache on its first access.
    RO_LatticeIterator<int32_t> lat2Iter (lat2, lat1.niceCursorShape(), useRef);
    Array<int32_t> lat2Buffer;
    while (! lat1Iter.atEnd()) {
      // Do separate getSlice to use reference semantics if
      // lat2 is an ArrayLattice.
      // Note that it requires lat2 to be non-const.
      lat2.getSlice (lat2Buffer, lat1Iter.position(),
		     lat1Iter.cursorShape());
      lat1Iter.rwCursor() += lat2Buffer;
      lat1Iter++;
    }
    timer.show ();
    ///if (pa1) pa1->showCacheStatistics (cout);
    ///if (pa2) pa2->showCacheStatistics (cout);
  }
}

int main (int argc, const char* argv[])
{
  try {
    {
      const IPosition latticeShape(4, 16, 12, 4, 32);
      Array<int32_t> arr(latticeShape);
      indgen(arr);
      ArrayLattice<int32_t> lattice(arr);
      Slicer slicer(IPosition(4,4,2,1,3), IPosition(4,14,10,3,23),
		    IPosition(4,2,3,1,4), Slicer::endIsLast);
      SubLattice<int32_t> sublat (lattice, slicer, true);
      AlwaysAssertExit (!sublat.isPaged());
      AlwaysAssertExit (!sublat.isPersistent());
      AlwaysAssertExit (!sublat.isMasked());
      AlwaysAssertExit (sublat.isWritable());
      AlwaysAssertExit (sublat.shape() == slicer.length());
      Array<int32_t> arr1, arr2;
      sublat.getSlice (arr1, IPosition(4,0), sublat.shape(),
		       IPosition(4,1));
      lattice.getSlice (arr2, slicer);
      AlwaysAssertExit (allEQ(arr1, arr2));
      AlwaysAssertExit (allEQ(arr1,
			      arr(slicer.start(), slicer.end(),
				  slicer.stride())));
      testVectorROIter (sublat, lattice, slicer);
    }
    // Test some other SubLattice functions.
    testRest();
    // Test the axes removal.
    testAxes();

    {
      // Test performance.
      Input inp(1);
      inp.version(" ");
      inp.create("nx", "512", "Number of pixels along the x-axis", "int");
      inp.create("ny", "512", "Number of pixels along the y-axis", "int");
      inp.readArguments(argc, argv);
      const uint32_t nx=inp.getInt("nx");
      const uint32_t ny=inp.getInt("ny");
      IPosition shape(2,nx,ny);
      ArrayLattice<int32_t> latArr1(shape);
      ArrayLattice<int32_t> latArr2(shape);

      Array<int32_t> arr(latArr1.shape());
      indgen(arr);
      latArr1.put (arr);
      latArr2.put (arr);
      cout << "Shape " << shape << endl;

      {
	SubLattice<int32_t> slatArr1(latArr1, true);
	SubLattice<int32_t> slatArr2(latArr2);
	cout << "subarray+=subarray useRef=false" << endl;
	testAdd (slatArr1, slatArr2, false);
	AlwaysAssert (allEQ(latArr1.get(), 2*arr), AipsError);
	cout << "subarray+=subarray useRef=true" << endl;
	testAdd (slatArr1, slatArr2, true);
	AlwaysAssert (allEQ(latArr1.get(), 3*arr), AipsError);
	cout << "array+=subarray useRef=false" << endl;
	testAdd (latArr1, slatArr2, false);
	AlwaysAssert (allEQ(latArr1.get(), 4*arr), AipsError);
	cout << "array+=subarray useRef=true" << endl;
	testAdd (latArr1, slatArr2, true);
	AlwaysAssert (allEQ(latArr1.get(), 5*arr), AipsError);
	cout << "subarray+=array useRef=false" << endl;
	testAdd (slatArr1, latArr2, false);
	AlwaysAssert (allEQ(latArr1.get(), 6*arr), AipsError);
	cout << "subarray+=array useRef=true" << endl;
	testAdd (slatArr1, latArr2, true);
	AlwaysAssert (allEQ(latArr1.get(), 7*arr), AipsError);
	cout << "array+=array useRef=false" << endl;
	testAdd (latArr1, latArr2, false);
	AlwaysAssert (allEQ(latArr1.get(), 8*arr), AipsError);
	cout << "array+=array useRef=true" << endl;
	testAdd (latArr1, latArr2, true);
	AlwaysAssert (allEQ(latArr1.get(), 9*arr), AipsError);
      }
    }
    {
      // test position in parent
      ArrayLattice<int32_t> parent((IPosition(3, 20, 20, 20)));
      Slicer slice(IPosition(3, 1, 1, 1), IPosition(3, 16, 17, 18),
                   IPosition(3, 1, 1, 2), Slicer::endIsLast);
      SubLattice<int32_t> sub(parent, slice);
      AlwaysAssert(sub.positionInParent(IPosition(3, 4, 5, 6)) ==
                   IPosition(3, 5, 6, 13),
                   AipsError);
      Slicer slice2(IPosition(3, 1, 1, 1), IPosition(3, 16, 17, 18),
                    IPosition(3, 16, 1, 2), Slicer::endIsLast);
      SubLattice<int32_t> sub2(parent, slice2, AxesSpecifier(false));
      AlwaysAssert(sub2.positionInParent(IPosition(2, 4, 5)) ==
                   IPosition(3, 1, 5, 11),
                   AipsError);
    }
  } catch (const std::exception& x) {
    cerr << "Caught exception: " << x.what() << endl;
    cout << "FAIL" << endl;
    return 1;
  } 

  cout << "OK" << endl;
  return 0;
}
