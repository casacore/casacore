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
//#
//# $Id$

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
void testVectorROIter (const Lattice<Int>& sublat,
		       const Lattice<Int>& lattice,
		       const Slicer& slicer)
{
    Int nstep;
    const IPosition latticeShape(sublat.shape());
    const IPosition cursorShape(1,latticeShape(0));
    LatticeStepper step(latticeShape, cursorShape);
    RO_LatticeIterator<Int>  iter(sublat, step);
    LatticeStepper step2(lattice.shape(), cursorShape);
    step2.subSection (slicer.start(), slicer.end(), slicer.stride());
    RO_LatticeIterator<Int>  iter2(lattice, step2);
      // static_cast's added for a workaround for an SGI compiler bug.
    for (iter.reset(); !iter.atEnd(); iter++, iter2++){
        AlwaysAssert(allEQ(static_cast<Vector<Int> >(iter.vectorCursor()),
			   static_cast<Vector<Int> >(iter2.vectorCursor())), AipsError);
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
    PagedArray<Int> pa(IPosition(1,10), "tSubLattice_tmp.pa");
    AlwaysAssertExit (pa.isPaged());
    AlwaysAssertExit (pa.isPersistent());
    AlwaysAssertExit (pa.isWritable());
    AlwaysAssertExit (pa.name(True) == "tSubLattice_tmp.pa");
    LCPagedMask mask(IPosition(1,10), "tSubLattice_tmp.pa/mask");
    Slicer slicer(IPosition(1,1), IPosition(1,3));
    Slicer slfull(IPosition(1,0), IPosition(1,10));
    {
        // A SubLattice as a Lattice copy (RO).
	SubLattice<Int> sl(pa);
	AlwaysAssertExit (!sl.isMasked());
	AlwaysAssertExit (!sl.hasPixelMask());
	AlwaysAssertExit (sl.isPaged());
	AlwaysAssertExit (sl.isPersistent());
	AlwaysAssertExit (!sl.isWritable());
	// A copy of the SubLattice.
	SubLattice<Int> sl1(sl, True);
	AlwaysAssertExit (!sl1.isMasked());
	AlwaysAssertExit (!sl1.hasPixelMask());
	AlwaysAssertExit (sl1.isPaged());
	AlwaysAssertExit (sl1.isPersistent());
	AlwaysAssertExit (!sl1.isWritable());
	AlwaysAssertExit (sl1.name(True) == "tSubLattice_tmp.pa");
    }
    {
        // A SubLattice as a Lattice copy (RW).
	SubLattice<Int> sl(pa, True);
	AlwaysAssertExit (!sl.isMasked());
	AlwaysAssertExit (!sl.hasPixelMask());
	AlwaysAssertExit (sl.isPaged());
	AlwaysAssertExit (sl.isPersistent());
	AlwaysAssertExit (sl.isWritable());
	// A RW copy of the SubLattice.
	SubLattice<Int> sl1(sl, True);
	AlwaysAssertExit (!sl1.isMasked());
	AlwaysAssertExit (!sl1.hasPixelMask());
	AlwaysAssertExit (sl1.isPaged());
	AlwaysAssertExit (sl1.isPersistent());
	AlwaysAssertExit (sl1.isWritable());
	// A RO copy of the SubLattice.
	SubLattice<Int> sl2(sl, False);
	AlwaysAssertExit (!sl2.isMasked());
	AlwaysAssertExit (!sl2.hasPixelMask());
	AlwaysAssertExit (sl2.isPaged());
	AlwaysAssertExit (sl2.isPersistent());
	AlwaysAssertExit (!sl2.isWritable());
	AlwaysAssertExit (sl2.name(True) == "tSubLattice_tmp.pa");
    }
    {
        // A RO SubLattice as a masked Lattice.
	SubLattice<Int> sl(pa, mask);
	AlwaysAssertExit (sl.isMasked());
	AlwaysAssertExit (!sl.hasPixelMask());
	AlwaysAssertExit (sl.isPaged());
	AlwaysAssertExit (!sl.isPersistent());
	AlwaysAssertExit (!sl.isWritable());
	// A copy of the SubLattice.
	SubLattice<Int> sl1(sl, True);
	AlwaysAssertExit (sl1.isMasked());
	AlwaysAssertExit (!sl1.hasPixelMask());
	AlwaysAssertExit (sl1.isPaged());
	AlwaysAssertExit (!sl1.isPersistent());
	AlwaysAssertExit (!sl1.isWritable());
	AlwaysAssertExit (sl1.name(True) == "tSubLattice_tmp.pa");
    }
    {
        // A RW SubLattice as a masked Lattice.
	SubLattice<Int> sl(pa, mask, True);
	AlwaysAssertExit (sl.isMasked());
	AlwaysAssertExit (!sl.hasPixelMask());
	AlwaysAssertExit (sl.isPaged());
	AlwaysAssertExit (!sl.isPersistent());
	AlwaysAssertExit (sl.isWritable());
	// A RW copy of the SubLattice.
	SubLattice<Int> sl1(sl, True);
	AlwaysAssertExit (sl1.isMasked());
	AlwaysAssertExit (!sl1.hasPixelMask());
	AlwaysAssertExit (sl1.isPaged());
	AlwaysAssertExit (!sl1.isPersistent());
	AlwaysAssertExit (sl1.isWritable());
	// A RO copy of the SubLattice.
	SubLattice<Int> sl2(sl, False);
	AlwaysAssertExit (sl2.isMasked());
	AlwaysAssertExit (!sl2.hasPixelMask());
	AlwaysAssertExit (sl2.isPaged());
	AlwaysAssertExit (!sl2.isPersistent());
	AlwaysAssertExit (!sl2.isWritable());
    }
    {
        // A small region of a lattice.
	SubLattice<Int> sl(pa, slicer, True);
	AlwaysAssertExit (!sl.isMasked());
	AlwaysAssertExit (!sl.hasPixelMask());
	AlwaysAssertExit (sl.isPaged());
	AlwaysAssertExit (!sl.isPersistent());
	AlwaysAssertExit (sl.isWritable());
	// A RW copy of the SubLattice.
	SubLattice<Int> sl1(sl, True);
	AlwaysAssertExit (!sl1.isMasked());
	AlwaysAssertExit (!sl1.hasPixelMask());
	AlwaysAssertExit (sl1.isPaged());
	AlwaysAssertExit (!sl1.isPersistent());
	AlwaysAssertExit (sl1.isWritable());
	// A RO copy of the SubLattice.
	SubLattice<Int> sl2(sl, False);
	AlwaysAssertExit (!sl2.isMasked());
	AlwaysAssertExit (!sl2.hasPixelMask());
	AlwaysAssertExit (sl2.isPaged());
	AlwaysAssertExit (!sl2.isPersistent());
	AlwaysAssertExit (!sl2.isWritable());
    }
    {
        // A full region of a lattice.
	SubLattice<Int> sl(pa, slfull, True);
	AlwaysAssertExit (!sl.isMasked());
	AlwaysAssertExit (!sl.hasPixelMask());
	AlwaysAssertExit (sl.isPaged());
	AlwaysAssertExit (sl.isPersistent());
	AlwaysAssertExit (sl.isWritable());
	// A RW copy of the SubLattice.
	SubLattice<Int> sl1(sl, True);
	AlwaysAssertExit (!sl1.isMasked());
	AlwaysAssertExit (!sl1.hasPixelMask());
	AlwaysAssertExit (sl1.isPaged());
	AlwaysAssertExit (sl1.isPersistent());
	AlwaysAssertExit (sl1.isWritable());
	// A RO copy of the SubLattice.
	SubLattice<Int> sl2(sl, False);
	AlwaysAssertExit (!sl2.isMasked());
	AlwaysAssertExit (!sl2.hasPixelMask());
	AlwaysAssertExit (sl2.isPaged());
	AlwaysAssertExit (sl2.isPersistent());
	AlwaysAssertExit (!sl2.isWritable());
    }
}

void testAxes()
{
    PagedArray<Int> pa(IPosition(3,10,11,12), "tSubLattice_tmp.pa");
    LCPagedMask mask(IPosition(3,10,11,12), "tSubLattice_tmp.pa/mask");
    Array<Int> arr(pa.shape());
    indgen(arr);
    pa.put (arr);
    Array<Bool> m(pa.shape());
    m = True;
    m(IPosition(3,0,0,0), IPosition(3,9,10,11), IPosition(3,2,1,1)) = False;
    mask.put (m);
    {
      // Create a sublattice with the mask and assign a pixelmask to it.
      SubLattice<Int> ml(pa, mask);
      Array<Bool> pm(pa.shape());
      pm = True;
      pm(IPosition(3,0,0,0), IPosition(3,9,10,11), IPosition(3,3,2,1)) = False;
      ml.setPixelMask (ArrayLattice<Bool>(pm), False);
      AlwaysAssertExit (ml.hasPixelMask());
      // Test if the mask read is correct.
      AlwaysAssertExit (allEQ (ml.getMask(), m&&pm));
      AlwaysAssertExit (allEQ (ml.pixelMask().get(), pm));
      // Copy constructor.
      SubLattice<Int> ml2(ml);
      AlwaysAssertExit (allEQ (ml2.pixelMask().get(), pm));
      AlwaysAssertExit (allEQ (ml2.getMask(), m&&pm));
      // Assign another pixelmask.
      pm = True;
      pm(IPosition(3,0,0,0), IPosition(3,9,10,11), IPosition(3,5,1,2)) = False;
      ml2.setPixelMask (ArrayLattice<Bool>(pm), False);
      AlwaysAssertExit (allEQ (ml2.getMask(), m&&pm));
      AlwaysAssertExit (allEQ (ml2.pixelMask().get(), pm));
      // Now make a sublattice from a MaskedLattice.
      ml2 = SubLattice<Int> (ml2, AxesSpecifier());
      AlwaysAssertExit (allEQ (ml2.getMask(), m&&pm));
      AlwaysAssertExit (allEQ (ml2.pixelMask().get(), pm));
      // Assign another pixelmask.
      Array<Bool> pm2(pa.shape());
      pm2 = False;
      pm2(IPosition(3,0,0,0), IPosition(3,9,10,11), IPosition(3,7,1,1)) = True;
      // The first one should fail.
      Bool exc = False;
      try {
	ml2.setPixelMask (ArrayLattice<Bool>(pm2), False);
      } catch (AipsError& x) {
	exc = True;
      }
      AlwaysAssertExit (exc);
      ml2.setPixelMask (ArrayLattice<Bool>(pm2), True);
      AlwaysAssertExit (allEQ (ml2.pixelMask().get(), pm&&pm2));
      AlwaysAssertExit (allEQ (ml2.getMask(), m&&pm&&pm2));
    }
    Array<Int> arrs1 = arr(IPosition(3,3,1,2), IPosition(3,8,1,9));
    Array<Int> arrsub = arrs1.reform(IPosition(2,6,8));
    Array<Bool> ms1 = m(IPosition(3,3,1,2), IPosition(3,8,1,9));
    Array<Bool> msub = ms1.reform(IPosition(2,6,8));
    // Make sublattice with a removed axis 1.
    SubLattice<Int> ml(pa, mask, True);
    Array<Bool> pixmask(IPosition(3,6,1,8));
    pixmask = True;
    pixmask (IPosition(3,0,0,0)) = !msub(IPosition(2,0,0));
    LCPixelSet pixset (pixmask, LCBox(IPosition(3,3,1,2),
				      IPosition(3,8,1,9),
				      m.shape()));
    SubLattice<Int> sl(ml, pixset, True, AxesSpecifier(False));

    IPosition ncs (pa.niceCursorShape());
    // Test if shape and niceCursorShape remove the axis.
    AlwaysAssertExit (sl.shape() == IPosition(2,6,8));
    AlwaysAssertExit (sl.niceCursorShape() == IPosition(2, min(6,ncs(0)),
							min(8,ncs(2))));
    // Test the getting functions.
    Array<Int> arrsl = sl.get();
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
    Array<Int> arrsubsub = arrsub(IPosition(2,1,2),
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
    Array<Bool> msubsub = msub(IPosition(2,1,2),
			       IPosition(2,5,7),
			       IPosition(2,3,2));
    AlwaysAssertExit (allEQ (sl.getMaskSlice(Slicer(IPosition(2,1,2),
						    IPosition(2,5,7),
						    IPosition(2,3,2),
						    Slicer::endIsLast)),
			     msubsub));
    // Assign a pixelmask to this sublattice.
    Array<Bool> slm = sl.getMask();
    Array<Bool> pm(sl.shape());
    pm = True;
    pm(IPosition(2,0,0), IPosition(2,1,2), IPosition(2,2,4)) = False;
    sl.setPixelMask (ArrayLattice<Bool>(pm), False);
    AlwaysAssertExit (allEQ (sl.getMask(), slm&&pm));
}


void testAdd (Lattice<Int>& lat1, Lattice<Int>& lat2, Bool useRef)
{
  {
    PagedArray<Int>* pa1 = dynamic_cast<PagedArray<Int>*> (&lat1);
    if (pa1) pa1->clearCache();
    PagedArray<Int>* pa2 = dynamic_cast<PagedArray<Int>*> (&lat2);
    if (pa2) pa2->clearCache();
    Timer timer;
    LatticeIterator<Int> lat1Iter (lat1, useRef);
    // Create dummy lat2Iter to setup cache correctly.
    // It may not be necessary, because the Table getSlice function
    // will setup the cache on its first access.
    RO_LatticeIterator<Int> lat2Iter (lat2, lat1.niceCursorShape(), useRef);
    Array<Int> lat2Buffer;
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
      Array<Int> arr(latticeShape);
      indgen(arr);
      ArrayLattice<Int> lattice(arr);
      Slicer slicer(IPosition(4,4,2,1,3), IPosition(4,14,10,3,23),
		    IPosition(4,2,3,1,4), Slicer::endIsLast);
      SubLattice<Int> sublat (lattice, slicer, True);
      AlwaysAssertExit (!sublat.isPaged());
      AlwaysAssertExit (!sublat.isPersistent());
      AlwaysAssertExit (!sublat.isMasked());
      AlwaysAssertExit (sublat.isWritable());
      AlwaysAssertExit (sublat.shape() == slicer.length());
      Array<Int> arr1, arr2;
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
      const uInt nx=inp.getInt("nx");
      const uInt ny=inp.getInt("ny");
      IPosition shape(2,nx,ny);
      ArrayLattice<Int> latArr1(shape);
      ArrayLattice<Int> latArr2(shape);

      Array<Int> arr(latArr1.shape());
      indgen(arr);
      latArr1.put (arr);
      latArr2.put (arr);
      cout << "Shape " << shape << endl;

      {
	SubLattice<Int> slatArr1(latArr1, True);
	SubLattice<Int> slatArr2(latArr2);
	cout << "subarray+=subarray useRef=False" << endl;
	testAdd (slatArr1, slatArr2, False);
	AlwaysAssert (allEQ(latArr1.get(), 2*arr), AipsError);
	cout << "subarray+=subarray useRef=True" << endl;
	testAdd (slatArr1, slatArr2, True);
	AlwaysAssert (allEQ(latArr1.get(), 3*arr), AipsError);
	cout << "array+=subarray useRef=False" << endl;
	testAdd (latArr1, slatArr2, False);
	AlwaysAssert (allEQ(latArr1.get(), 4*arr), AipsError);
	cout << "array+=subarray useRef=True" << endl;
	testAdd (latArr1, slatArr2, True);
	AlwaysAssert (allEQ(latArr1.get(), 5*arr), AipsError);
	cout << "subarray+=array useRef=False" << endl;
	testAdd (slatArr1, latArr2, False);
	AlwaysAssert (allEQ(latArr1.get(), 6*arr), AipsError);
	cout << "subarray+=array useRef=True" << endl;
	testAdd (slatArr1, latArr2, True);
	AlwaysAssert (allEQ(latArr1.get(), 7*arr), AipsError);
	cout << "array+=array useRef=False" << endl;
	testAdd (latArr1, latArr2, False);
	AlwaysAssert (allEQ(latArr1.get(), 8*arr), AipsError);
	cout << "array+=array useRef=True" << endl;
	testAdd (latArr1, latArr2, True);
	AlwaysAssert (allEQ(latArr1.get(), 9*arr), AipsError);
      }
    }
    {
      // test position in parent
      ArrayLattice<Int> parent((IPosition(3, 20, 20, 20)));
      Slicer slice(IPosition(3, 1, 1, 1), IPosition(3, 16, 17, 18),
                   IPosition(3, 1, 1, 2), Slicer::endIsLast);
      SubLattice<Int> sub(parent, slice);
      AlwaysAssert(sub.positionInParent(IPosition(3, 4, 5, 6)) ==
                   IPosition(3, 5, 6, 13),
                   AipsError);
      Slicer slice2(IPosition(3, 1, 1, 1), IPosition(3, 16, 17, 18),
                    IPosition(3, 16, 1, 2), Slicer::endIsLast);
      SubLattice<Int> sub2(parent, slice2, AxesSpecifier(False));
      AlwaysAssert(sub2.positionInParent(IPosition(2, 4, 5)) ==
                   IPosition(3, 1, 5, 11),
                   AipsError);
    }
  } catch (const AipsError& x) {
    cerr << "Caught exception: " << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  } 

  cout << "OK" << endl;
  return 0;
}
