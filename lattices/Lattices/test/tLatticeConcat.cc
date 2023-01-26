//# tLatticeConcat.cc: This program tests the LatticeConcat class
//# Copyright (C) 1996,1997,1999,2000,2001
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


#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/IO/FileLocker.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/lattices/Lattices/ArrayLattice.h>
#include <casacore/lattices/LRegions/LCBox.h>
#include <casacore/lattices/Lattices/LatticeConcat.h>
#include <casacore/lattices/Lattices/SubLattice.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
void check (uint32_t axis, MaskedLattice<float>& ml,
            MaskedLattice<float>& ml1, MaskedLattice<float>& ml2);
void check2 (MaskedLattice<float>& ml,
             MaskedLattice<float>& ml1, MaskedLattice<float>& ml2);
void check3 (const Slicer& sl, MaskedLattice<float>& ml1, 
             MaskedLattice<float>& ml2);
void check4 (const Slicer& sl, MaskedLattice<float>& ml1, 
             Array<float>& ml2);
void check5 (const Slicer& sl, MaskedLattice<float>& ml1, 
             Array<bool>& ml2);
void check6 (uint32_t axis, Lattice<bool>& ml,
             Lattice<bool>& ml1, Lattice<bool>& ml2);
void check7 (const Slicer& sl, LatticeConcat<float>& lc, float val, bool valMask);


int main() {
  try {

// Make some ArrayLattices

      IPosition shape(2,64,128);
      Array<float> a1(shape);
      Array<float> a2(shape);
      int32_t i, j;
      for (i=0; i<shape(0); i++) {
	for (j=0; j<shape(1); j++) {
	    a1(IPosition(2,i,j)) = i + j;
	    a2(IPosition(2,i,j)) = -i - j;
        }
      }
      ArrayLattice<float> l1(a1);
      ArrayLattice<float> l2(a2);
      ArrayLattice<float> l3(shape);
      l3.set(1.0);

// Make MaskedLattices with no mask

      SubLattice<float> ml1(l1, true);
      SubLattice<float> ml2(l2, true);
      SubLattice<float> ml3(l3, true);

// Make some MaskedLattices and give them a mask

      SubLattice<float> im1(l1,true);
      SubLattice<float> im2(l2, true);
      SubLattice<float> im3(l3, true);
//
      ArrayLattice<bool> mask1(shape);
      mask1.set(true);
      ArrayLattice<bool> mask2(shape);
      mask2.set(false);
      ArrayLattice<bool> mask3(shape);
      mask3.set(true);
      im1.setPixelMask(mask1,false);
      im2.setPixelMask(mask2,false);
      im3.setPixelMask(mask3,false);
//
      {
         cout << "tempClose/reopen/resync/flush" << endl;
         LatticeConcat<float> lc(0, true);
         lc.setLattice(ml1);
         lc.setLattice(im1);
         lc.reopen();
         lc.tempClose();
         lc.tempClose(0);
         lc.tempClose(1);
         lc.reopen(0);
         lc.reopen(1);
         lc.tempClose();
//
         lc.resync();
         lc.flush();
//
         IPosition outShape = lc.shape();
         AlwaysAssert(outShape.nelements()==2, AipsError);
         AlwaysAssert(outShape(0)==shape(0)+shape(0), AipsError);
         AlwaysAssert(outShape(1)==shape(1), AipsError);
         AlwaysAssert(lc.isMasked()==true, AipsError);
         AlwaysAssert(lc.hasPixelMask()==true, AipsError);
         AlwaysAssert(lc.pixelMask().isWritable()==false, AipsError);
         AlwaysAssert(lc.pixelMask().shape()==outShape, AipsError);
         AlwaysAssert(lc.axis()==0, AipsError);
         AlwaysAssert(lc.nlattices()==2, AipsError);
         Lattice<bool>& pixelMask = lc.pixelMask();
         check6(0, pixelMask, mask1, mask1);
      }
//
      {
         cout << "partly pixelMask" << endl;
         LatticeConcat<float> lc(0, true);
         lc.setLattice(im2);
         lc.setLattice(ml1);
//
         IPosition outShape = lc.shape();
         AlwaysAssert(outShape.nelements()==2, AipsError);
         AlwaysAssert(outShape(0)==shape(0)+shape(0), AipsError);
         AlwaysAssert(outShape(1)==shape(1), AipsError);
         AlwaysAssert(lc.isMasked()==true, AipsError);
         AlwaysAssert(lc.hasPixelMask()==true, AipsError);
         AlwaysAssert(lc.pixelMask().isWritable()==false, AipsError);
         AlwaysAssert(lc.pixelMask().shape()==outShape, AipsError);
         AlwaysAssert(lc.axis()==0, AipsError);
         AlwaysAssert(lc.nlattices()==2, AipsError);
         Lattice<bool>& pixelMask = lc.pixelMask();
         check6(0, pixelMask, mask2, mask1);
      }
//
      {
         cout << "Axis 0, ArrayLattices, no masks" << endl;

// Concatenate along axis 0

         LatticeConcat<float> lc(0, false);
         lc.setLattice(ml1);
         lc.setLattice(ml2);

// Find output shape

         IPosition outShape = lc.shape();
         AlwaysAssert(outShape.nelements()==2, AipsError);
         AlwaysAssert(outShape(0)==shape(0)+shape(0), AipsError);
         AlwaysAssert(outShape(1)==shape(1), AipsError);
         AlwaysAssert(lc.isMasked()==false, AipsError);
         AlwaysAssert(lc.hasPixelMask()==false, AipsError);
         AlwaysAssert(lc.axis()==0, AipsError);
         AlwaysAssert(lc.nlattices()==2, AipsError);
//
         check (0, lc, ml1, ml2);
      }

      {

         cout << "Axis 1, ArrayLattices, no masks" << endl;

// Concatenate along axis 1

         LatticeConcat<float> lc (1, true);
         lc.setLattice(ml1);
         lc.setLattice(ml2);

// Find output shape

         IPosition outShape = lc.shape();
         AlwaysAssert(outShape.nelements()==2, AipsError);
         AlwaysAssert(outShape(0)==shape(0), AipsError);
         AlwaysAssert(outShape(1)==shape(1)+shape(1), AipsError);
         AlwaysAssert(lc.isMasked()==false, AipsError);
         AlwaysAssert(lc.hasPixelMask()==false, AipsError);
         AlwaysAssert(lc.axis()==1, AipsError);
         AlwaysAssert(lc.nlattices()==2, AipsError);
//        
         check (1, lc, ml1, ml2);
      }
//
      {
         cout << "Increase dimensionality by 1, ArrayLattices, no masks" << endl;

// Create axis 2

         LatticeConcat<float> lc (2);
         lc.setLattice(ml1);
         lc.setLattice(ml2);

// Find output shape

         IPosition outShape = lc.shape();
         AlwaysAssert(outShape.nelements()==3, AipsError);
         AlwaysAssert(outShape(0)==shape(0), AipsError);
         AlwaysAssert(outShape(1)==shape(1), AipsError);
         AlwaysAssert(outShape(2)=2, AipsError);
         AlwaysAssert(lc.isMasked()==false, AipsError);
         AlwaysAssert(lc.hasPixelMask()==false, AipsError);
         AlwaysAssert(lc.axis()==2, AipsError);
         AlwaysAssert(lc.nlattices()==2, AipsError);
//        
         check2 (lc, ml1, ml2);
      }

      {
         cout << "Increase dimensionality by 1, masks" << endl;

// Create axis 2

         LatticeConcat<float> lc (2);
         lc.setLattice(im1);
         lc.setLattice(im2);

// Find output shape

         IPosition outShape = lc.shape();
         AlwaysAssert(outShape.nelements()==3, AipsError);
         AlwaysAssert(outShape(0)==shape(0), AipsError);
         AlwaysAssert(outShape(1)==shape(1), AipsError);
         AlwaysAssert(outShape(2)=2, AipsError);
         AlwaysAssert(lc.isMasked()==true, AipsError);
         AlwaysAssert(lc.hasPixelMask()==true, AipsError);
         AlwaysAssert(lc.pixelMask().isWritable()==true, AipsError);
         AlwaysAssert(lc.pixelMask().shape()==outShape, AipsError);
         AlwaysAssert(lc.axis()==2, AipsError);
         AlwaysAssert(lc.nlattices()==2, AipsError);
//        
         check2 (lc, im1, im2);
      }
      {
         cout << "Increase dimensionality by 1,  masks, various getslices" << endl;

// Create axis 2

         LatticeConcat<float> lc (2);
         lc.setLattice(im3);
         lc.setLattice(im3);
         lc.setLattice(im3);
         lc.setLattice(im3);
         lc.setLattice(im3);
         lc.setLattice(im3);
         lc.setLattice(im3);
         lc.setLattice(im3);

// Find output shape

         IPosition outShape = lc.shape();
         AlwaysAssert(outShape.nelements()==3, AipsError);
         AlwaysAssert(outShape(0)==shape(0), AipsError);
         AlwaysAssert(outShape(1)==shape(1), AipsError);
         AlwaysAssert(outShape(2)=8, AipsError);
         AlwaysAssert(lc.isMasked()==true, AipsError);
         AlwaysAssert(lc.axis()==2, AipsError);
         AlwaysAssert(lc.nlattices()==8, AipsError);

// Now look at funny slices

         {
            cout << "  All in lattice 1" << endl;
            IPosition blc(outShape.nelements(),0);
            blc(0) = 5; blc(1) = 10; blc(2) = 0;      
            IPosition trc(outShape-10);
            trc(2) = 0;
            IPosition stride(outShape.nelements(),1);
            Slicer sl(blc, trc, stride, Slicer::endIsLast);
            check7(sl, lc, 1.0, true);
         }
         {
            cout << "  All in lattice 1 + non-unit strides" << endl;
            IPosition blc(outShape.nelements());
            blc(0) = 5; blc(1) = 10; blc(2) = 0;      
            IPosition trc(outShape-10);
            trc(2) = 0;
            IPosition stride(outShape.nelements(),1);
            stride(0) = 2; stride(1) = 3;
            Slicer sl(blc, trc, stride, Slicer::endIsLast);
            check7(sl, lc, 1.0, true);
         }
         {
            cout << "  Many lattices" << endl;
            IPosition blc(outShape.nelements(),0);
            blc(0) = 5; blc(1) = 10; blc(2) = 2;
            IPosition trc(outShape-10);
            trc(2) = 6;
            IPosition stride(outShape.nelements(),1);
            Slicer sl(blc, trc, stride, Slicer::endIsLast);
            check7(sl, lc, 1.0, true);
         }
         {
            cout << "  Many lattices + non-unit strides" << endl;
            IPosition blc(outShape.nelements());
            blc(0) = 5; blc(1) = 10; blc(2) = 1;
            IPosition trc(outShape-10);
            trc(2) = 7;
            IPosition stride(outShape.nelements(),1);
            stride(0) = 2; stride(1) = 3; stride(2) = 2;
            Slicer sl(blc, trc, stride, Slicer::endIsLast);
            check7(sl, lc, 1.0, true);
         }
      }
      {
         cout << "Increase dimensionality by 1,  masks, various putslices" << endl;

// Create axis 2

         LatticeConcat<float> lc (2);
         lc.setLattice(im3);
         lc.setLattice(im3);
         lc.setLattice(im3);
         lc.setLattice(im3);
         lc.setLattice(im3);
         lc.setLattice(im3);
         lc.setLattice(im3);
         lc.setLattice(im3);

// Find output shape

         IPosition outShape = lc.shape();
         AlwaysAssert(outShape.nelements()==3, AipsError);
         AlwaysAssert(outShape(0)==shape(0), AipsError);
         AlwaysAssert(outShape(1)==shape(1), AipsError);
         AlwaysAssert(outShape(2)=8, AipsError);
         AlwaysAssert(lc.isMasked()==true, AipsError);
         AlwaysAssert(lc.axis()==2, AipsError);
         AlwaysAssert(lc.nlattices()==8, AipsError);
         AlwaysAssert(lc.isWritable(), AipsError);


// Now look at funny slices

         {
            cout << "  All in lattice 1" << endl;
            IPosition blc(outShape.nelements(),0);
            blc(0) = 5; blc(1) = 10; blc(2) = 0;      
            IPosition trc(outShape-10);
            trc(2) = 0;
            IPosition stride(outShape.nelements(),1);
            Slicer sl(blc, trc, stride, Slicer::endIsLast);
//
            Array<float> tmp0(sl.length()); tmp0.set(1.0);
            lc.putSlice(tmp0, sl.start(), sl.stride());
            check4(sl, lc, tmp0);
//
            Array<bool> btmp0(sl.length()); btmp0.set(false);
            lc.pixelMask().putSlice(btmp0, sl.start(), sl.stride());
            check5(sl, lc, btmp0);
         }
         {
            cout << "  All in lattice 1 + non-unit strides" << endl;
            IPosition blc(outShape.nelements());
            blc(0) = 5; blc(1) = 10; blc(2) = 0;      
            IPosition trc(outShape-10);
            trc(2) = 0;
            IPosition stride(outShape.nelements(),1);
            stride(0) = 2; stride(1) = 3;
            Slicer sl(blc, trc, stride, Slicer::endIsLast);
//
            Array<float> tmp0(sl.length()); tmp0.set(1.0);
            lc.putSlice(tmp0, sl.start(), sl.stride());
            check4(sl, lc, tmp0);
//
            Array<bool> btmp0(sl.length()); btmp0.set(false);
            lc.pixelMask().putSlice(btmp0, sl.start(), sl.stride());
            check5(sl, lc, btmp0);
         }
         {
            cout << "  Many lattices" << endl;
            IPosition blc(outShape.nelements(),0);
            blc(0) = 5; blc(1) = 10; blc(2) = 2;
            IPosition trc(outShape-10);
            trc(2) = 6;
            IPosition stride(outShape.nelements(),1);
            Slicer sl(blc, trc, stride, Slicer::endIsLast);
//
            Array<float> tmp0(sl.length()); tmp0.set(1.0);
            lc.putSlice(tmp0, sl.start(), sl.stride());
            check4(sl, lc, tmp0);
//
            Array<bool> btmp0(sl.length()); btmp0.set(false);
            lc.pixelMask().putSlice(btmp0, sl.start(), sl.stride());
            check5(sl, lc, btmp0);
         }
         {
            cout << "  Many lattices + non-unit strides" << endl;
            IPosition blc(outShape.nelements());
            blc(0) = 5; blc(1) = 10; blc(2) = 1;
            IPosition trc(outShape-10);
            trc(2) = 7;
            IPosition stride(outShape.nelements(),1);
            stride(0) = 2; stride(1) = 3; stride(2) = 2;
            Slicer sl(blc, trc, stride, Slicer::endIsLast);
//
            Array<float> tmp0(sl.length()); tmp0.set(1.0);
            lc.putSlice(tmp0, sl.start(), sl.stride());
            check4(sl, lc, tmp0);
//
            Array<bool> btmp0(sl.length()); btmp0.set(false);
            lc.pixelMask().putSlice(btmp0, sl.start(), sl.stride());
            check5(sl, lc, btmp0);
         }
      }
//
      {
         cout << "Axis 0, masks" << endl;

// Concatenate along axis 0

         LatticeConcat<float> lc (0);
         lc.setLattice(im1);
         lc.setLattice(im2);

// Find output shape

         IPosition outShape = lc.shape();
         AlwaysAssert(outShape.nelements()==2, AipsError);
         AlwaysAssert(outShape(0)==shape(0)+shape(0), AipsError);
         AlwaysAssert(outShape(1)==shape(1), AipsError);
         AlwaysAssert(lc.isMasked()==true, AipsError);
         AlwaysAssert(lc.axis()==0, AipsError);
         AlwaysAssert(lc.nlattices()==2, AipsError);
//
         check (0, lc, im1, im2);
      }

      {
//
// Now, having convinced ourselves that the lattices are
// concatenated properly, when we look at the whole thing,
// make sure slices are correct when straddling lattice
// boundaries etc.
//
         cout << "Axis 0, masks, various getslices" << endl;

// Concatenate along axis 0

         LatticeConcat<float> lc (0);
         im3.set(1.0);
         Lattice<bool>& pixelMask = im3.pixelMask();
         pixelMask.set(true);
         lc.setLattice(im3);
         lc.setLattice(im3);

// Find output shape

         IPosition outShape = lc.shape();
         AlwaysAssert(outShape.nelements()==2, AipsError);
         AlwaysAssert(outShape(0)==shape(0)+shape(0), AipsError);
         AlwaysAssert(outShape(1)==shape(1), AipsError);
         AlwaysAssert(lc.isMasked()==true, AipsError);
         AlwaysAssert(lc.axis()==0, AipsError);
         AlwaysAssert(lc.nlattices()==2, AipsError);

// Now look at funny slices

         {
            cout << "  All in lattice 1" << endl;
            IPosition blc(outShape.nelements(),0);
            IPosition trc(shape-1);
            IPosition stride(outShape.nelements(),1);
            Slicer sl(blc, trc, stride, Slicer::endIsLast);
            check7(sl, lc, 1.0, true);
         }
         {
            cout << "  All in lattice 1 + non-unit strides" << endl;
            IPosition blc(outShape.nelements(),0);
            IPosition trc(shape-1);
            IPosition stride(outShape.nelements(),1);
            stride(0) = 2; stride(1) = 3;
            Slicer sl(blc, trc, stride, Slicer::endIsLast);
            check7(sl, lc, 1.0, true);
         }
         {
            cout << "  Straddle boundary" << endl;
            IPosition blc(outShape.nelements(),5);
            IPosition trc(shape-10);
            trc(0) = shape(0) + 30;
            IPosition stride(outShape.nelements(),1);
            Slicer sl(blc, trc, stride, Slicer::endIsLast);
            check7(sl, lc, 1.0, true);
         }
         {
            cout << "  Straddle boundary and non-unit strides" << endl;
            IPosition blc(outShape.nelements(),5);
            IPosition trc(shape-10);
            trc(0) = shape(0) + 30;
            IPosition stride(outShape.nelements(),1);
            stride(0) = 2; stride(1) = 3;
            Slicer sl(blc, trc, stride, Slicer::endIsLast);
            check7(sl, lc, 1.0, true);
         }
         {
            cout << "  All in lattice 2" << endl;
            IPosition blc(shape-1);
            blc(0) = shape(0) + 10;
            blc(1) = 10;
            IPosition trc(blc+20);
            IPosition stride(outShape.nelements(),1);
            Slicer sl(blc, trc, stride, Slicer::endIsLast);
            check7(sl, lc, 1.0, true);
         }
         {
            cout << "  All in lattice 2 and non-unit strides" << endl;
            IPosition blc(shape-1);
            blc(0) = shape(0) + 10;
            blc(1) = 10;
            IPosition trc(blc+20);
            IPosition stride(outShape.nelements(),1);
            stride(0) = 2; stride(1) = 3;
            Slicer sl(blc, trc, stride, Slicer::endIsLast);
            check7(sl, lc, 1.0, true);
         }
      }


// Putslices

      {
         cout << "Axis 0, ArrayLattices, various putslices" << endl;

         Array<float> aa1 = ml1.get();         
         Array<float> aa2 = ml2.get();
         ArrayLattice<float> x1(aa1);
         ArrayLattice<float> x2(aa2);
         SubLattice<float> m1(x1,true);
         SubLattice<float> m2(x2,true);
//
         LatticeConcat<float> lc (0);
         lc.setLattice(m1);
         lc.setLattice(m2);
         IPosition outShape = lc.shape();
         AlwaysAssert(lc.isWritable(),AipsError);
//
         {
            cout << "  All in lattice 1" << endl;
            IPosition blc(outShape.nelements(),0);
            IPosition trc(shape-1);
            IPosition stride(outShape.nelements(),1);
            Slicer sl(blc, trc, stride, Slicer::endIsLast);
//
            Array<float> tmp0(sl.length()); tmp0.set(1.0);
            lc.putSlice(tmp0, sl.start(), sl.stride());
            check4(sl, lc, tmp0);
         }
         {
            cout << "  All in lattice 1 + non-unit strides" << endl;
            IPosition blc(outShape.nelements(),0);
            IPosition trc(shape-1);
            IPosition stride(outShape.nelements(),1);
            stride(0) = 2; stride(1) = 3;
            Slicer sl(blc, trc, stride, Slicer::endIsLast);
//
            Array<float> tmp0(sl.length()); tmp0.set(1.0);
            lc.putSlice(tmp0, sl.start(), sl.stride());
            check4(sl, lc, tmp0);
         }
         {
            cout << "  Straddle boundary" << endl;
            IPosition blc(outShape.nelements(),5);
            IPosition trc(shape-10);
            trc(0) = shape(0) + 30;
            IPosition stride(outShape.nelements(),1);
            Slicer sl(blc, trc, stride, Slicer::endIsLast);
//          
            Array<float> tmp0(sl.length()); tmp0.set(1.0);
            lc.putSlice(tmp0, sl.start(), sl.stride());
            check4(sl, lc, tmp0);   
         }
         {
            cout << "  Straddle boundary and non-unit strides" << endl;
            IPosition blc(outShape.nelements(),5);
            IPosition trc(shape-10);
            trc(0) = shape(0) + 30;
            IPosition stride(outShape.nelements(),1);
            stride(0) = 2; stride(1) = 3;
            Slicer sl(blc, trc, stride, Slicer::endIsLast);
//          
            Array<float> tmp0(sl.length()); tmp0.set(1.0);
            lc.putSlice(tmp0, sl.start(), sl.stride());
            check4(sl, lc, tmp0);   
         }
         {
            cout << "  All in lattice 2" << endl;
            IPosition blc(shape-1);
            blc(0) = shape(0) + 10;
            blc(1) = 10;
            IPosition trc(blc+20);
            IPosition stride(outShape.nelements(),1);
            Slicer sl(blc, trc, stride, Slicer::endIsLast);
//          
            Array<float> tmp0(sl.length()); tmp0.set(1.0);
            lc.putSlice(tmp0, sl.start(), sl.stride());
            check4(sl, lc, tmp0);   
         }
         {
            cout << "  All in lattice 2 and non-unit strides" << endl;
            IPosition blc(shape-1);
            blc(0) = shape(0) + 10;
            blc(1) = 10;
            IPosition trc(blc+20);
            IPosition stride(outShape.nelements(),1);
            stride(0) = 2; stride(1) = 3;
            Slicer sl(blc, trc, stride, Slicer::endIsLast);
//          
            Array<float> tmp0(sl.length()); tmp0.set(1.0);
            lc.putSlice(tmp0, sl.start(), sl.stride());
            check4(sl, lc, tmp0);   
         }
     }


// pixelMask tests

     {
         cout << "Testing pixelMask" << endl;
         LatticeConcat<float> lc (0);
         lc.setLattice(ml1);
         lc.setLattice(ml2);
         AlwaysAssert(lc.hasPixelMask()==false, AipsError);
         bool ok;
         try {
            lc.pixelMask();
            ok = false;
         } catch (std::exception& x) {
            ok = true;
         } 
         if (!ok) {
            throw (AipsError("pixelMask forced failure did not work - this was unexpected"));
         }
     }
     {
         LatticeConcat<float> lc (0);
         lc.setLattice(im1);
         lc.setLattice(im2);
//
         AlwaysAssert(lc.hasPixelMask(), AipsError);
         Lattice<bool>& pixelMask = lc.pixelMask();
         check6(0, pixelMask, mask1, mask2);
     }

// Test lock etc

     {
         cout << "Testing locking" << endl;
         LatticeConcat<float> lc (0);
         lc.setLattice(ml1);
         lc.setLattice(ml2);
	 AlwaysAssert(lc.lock(FileLocker::Read, 1), AipsError);
	 AlwaysAssert(lc.hasLock(FileLocker::Read), AipsError);
	 AlwaysAssert(lc.lock(FileLocker::Write, 1), AipsError);
	 AlwaysAssert(lc.hasLock(FileLocker::Write), AipsError);

// ArrayLattices will always return true for hasLock

         lc.unlock();
         AlwaysAssert(lc.hasLock(FileLocker::Read), AipsError);
         AlwaysAssert(lc.hasLock(FileLocker::Write), AipsError);
     }



// Test copy constructor

     {
         cout << "Testing copy constructor" << endl;
         LatticeConcat<float> lc (0);
         lc.setLattice(ml1);
         lc.setLattice(ml2);
         LatticeConcat<float> lc2(lc);

// Find output shape

         AlwaysAssert(lc.shape().isEqual(lc2.shape()), AipsError);
         AlwaysAssert(lc.isMasked()==lc2.isMasked(), AipsError);
         AlwaysAssert(lc2.axis()==0, AipsError);
         AlwaysAssert(lc.nlattices()==2, AipsError);
//
         check (0, lc, ml1, ml2);
     }

// Test assignment 

     {
         cout << "Testing assignment " << endl;
         LatticeConcat<float> lc (0);
         lc.setLattice(ml1);
         lc.setLattice(ml2);
         LatticeConcat<float> lc2;
         lc2 = lc;

// Find output shape

         AlwaysAssert(lc.shape().isEqual(lc2.shape()), AipsError);
         AlwaysAssert(lc.isMasked()==lc2.isMasked(), AipsError);
         AlwaysAssert(lc2.axis()==0, AipsError);
         AlwaysAssert(lc.nlattices()==2, AipsError);
//
         check (0, lc, ml1, ml2);
     }

// Some forced errors

      {
         cout << "Forced errors" << endl;

// Concatenate along axis 0

         LatticeConcat<float> lc (10);
         bool ok = true;
         try {
            lc.setLattice(ml1);
            ok = false;
         } catch (std::exception&x) {
         } 
         if (!ok) {
            throw (AipsError("setLattice forced failure did not work - this was unexpected"));  
         }
//
         ok = true;
         try {
            ArrayLattice<float> l4(IPosition(3,2,2,2));
            SubLattice<float> ml4(l4, true);
            lc.setLattice(ml4);
            ok = false;
         } catch (std::exception& x) {;} 
         if (!ok) {
            throw (AipsError("setLattice forced failure did not work - this was unexpected"));  
         }
      }

  } catch(std::exception& x) {
    cerr << x.what() << endl;
    return 1;
  } 
  cout << "OK" << endl;
  return 0;
}


void check (uint32_t axis, MaskedLattice<float>& ml,
            MaskedLattice<float>& ml1, MaskedLattice<float>& ml2)
{
   IPosition shape1 = ml1.shape();
   IPosition shape2 = ml2.shape();
//
   IPosition blc(2,0,0);
   AlwaysAssert(allEQ(ml1.get(), ml.getSlice(blc,shape1)), AipsError);
   AlwaysAssert(allEQ(ml1.getMask(), ml.getMaskSlice(blc,shape1)), AipsError);
//
   if (axis==0) {
      blc(0) += shape1(0);
      AlwaysAssert(allEQ(ml2.get(), ml.getSlice(blc,shape2)), AipsError);      
      AlwaysAssert(allEQ(ml2.getMask(), ml.getMaskSlice(blc,shape2)), AipsError);      
   } else if (axis==1) {
      blc(1) += shape1(1);
      AlwaysAssert(allEQ(ml2.get(), ml.getSlice(blc,shape2)), AipsError);
      AlwaysAssert(allEQ(ml2.getMask(), ml.getMaskSlice(blc,shape2)), AipsError);
   } else {
      AlwaysAssert(axis==0||axis==1, AipsError);
   }
}



void check2 (MaskedLattice<float>& ml,
             MaskedLattice<float>& ml1, MaskedLattice<float>& ml2)
{
   IPosition shape1 = ml1.shape();
   IPosition shape2 = ml2.shape();
   IPosition sliceShape(3,shape1(0), shape1(1), 1);
//
   IPosition blc(3,0,0,0);
   AlwaysAssert(allEQ(ml1.get(), ml.getSlice(blc,sliceShape,true)), AipsError);
   AlwaysAssert(allEQ(ml1.getMask(), ml.getMaskSlice(blc,sliceShape,true)), AipsError);
//
   blc(2) = 1;
   AlwaysAssert(allEQ(ml2.get(), ml.getSlice(blc,sliceShape,true)), AipsError);
   AlwaysAssert(allEQ(ml2.getMask(), ml.getMaskSlice(blc,sliceShape,true)), AipsError);
}

void check3 (const Slicer& sl, MaskedLattice<float>& ml1, 
             MaskedLattice<float>& ml2) 
{
   AlwaysAssert(allEQ(ml1.getSlice(sl), ml2.getSlice(sl)), AipsError);
   AlwaysAssert(allEQ(ml1.getMaskSlice(sl), ml2.getMaskSlice(sl)), AipsError);
}

void check4 (const Slicer& sl, MaskedLattice<float>& ml1, 
             Array<float>& ml2) 
{
   AlwaysAssert(allEQ(ml1.getSlice(sl), ml2), AipsError);
}

void check5 (const Slicer& sl, MaskedLattice<float>& ml1, 
             Array<bool>& ml2) 
{
   AlwaysAssert(allEQ(ml1.getMaskSlice(sl), ml2), AipsError);
}

void check6 (uint32_t axis, Lattice<bool>& ml,
             Lattice<bool>& ml1, Lattice<bool>& ml2)
{
   IPosition shape1 = ml1.shape();
   IPosition shape2 = ml2.shape();
//
   IPosition blc(2,0,0);
   AlwaysAssert(allEQ(ml1.get(), ml.getSlice(blc,shape1)), AipsError);
//
   if (axis==0) {
      blc(0) += shape1(0);
      AlwaysAssert(allEQ(ml2.get(), ml.getSlice(blc,shape2)), AipsError);      
   } else if (axis==1) {
      blc(1) += shape1(1);
      AlwaysAssert(allEQ(ml2.get(), ml.getSlice(blc,shape2)), AipsError);
   } else {
      AlwaysAssert(axis==0||axis==1, AipsError);
   }
}

void check7 (const Slicer& sl, LatticeConcat<float>& lc, float val, bool valMask)
{
   double tol(1.0e-6);
   AlwaysAssert(allNear(lc.getSlice(sl),val,tol),AipsError);
   AlwaysAssert(allEQ(lc.getMaskSlice(sl),valMask),AipsError);
}
