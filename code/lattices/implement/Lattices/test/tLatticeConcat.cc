//# tLatticeConcat.cc: This program tests the LatticeConcat class
//# Copyright (C) 1996,1997,1999
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
//# $Id: 

#include <aips/aips.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Exceptions/Error.h>
#include <aips/Exceptions/Excp.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Mathematics/Math.h>
#include <aips/Utilities/Assert.h>

#include <trial/Coordinates/CoordinateUtil.h>
#include <trial/Lattices/ArrayLattice.h>
#include <trial/Lattices/LCBox.h>
#include <trial/Lattices/LatticeConcat.h>
#include <trial/Lattices/LCPagedMask.h>
#include <trial/Lattices/SubLattice.h>
#include <trial/Images/PagedImage.h>
#include <trial/Images/ImageRegion.h>
#include <iostream.h>


void check (uInt axis, MaskedLattice<Float>& ml,
            MaskedLattice<Float>& ml1, MaskedLattice<Float>& ml2);

void fillImage (ImageInterface<Float>& im, Array<Float>& a, Bool maskValue);

int main() {
  try {

// Make some ArrayLattices

      IPosition shape(2,5,10);
      Array<Float> a1(shape);
      Array<Float> a2(shape);
      Int i, j;
      for (i=0; i<shape(0); i++) {
	for (j=0; j<shape(1); j++) {
	    a1(IPosition(2,i,j)) = i + j;
	    a2(IPosition(2,i,j)) = -i - j;
        }
      }
      ArrayLattice<Float> l1(a1);
      ArrayLattice<Float> l2(a2);

// Make MaskedLattices

      SubLattice<Float> ml1(l1, True);
      SubLattice<Float> ml2(l2, True);

      {
         cout << "Axis 0, ArrayLattices, no masks" << endl;

// Concatenate along axis 0

         LatticeConcat<Float> lc (0, False);
         lc.setLattice(ml1);
         lc.setLattice(ml2);

// Find output shape

         IPosition outShape = lc.shape();
         AlwaysAssert(outShape.nelements()==2, AipsError);
         AlwaysAssert(outShape(0)==shape(0)+shape(0), AipsError);
         AlwaysAssert(outShape(1)==shape(1), AipsError);
         AlwaysAssert(lc.isMasked()==False, AipsError);
         AlwaysAssert(lc.axis()==0, AipsError);

// Make output
   
         ArrayLattice<Float> l3(outShape);
         SubLattice<Float> ml3(l3, True);

// Do it

         lc.copyData(ml3);

// Check values

         check (0, ml3, ml1, ml2);
      }

      {

         cout << "Axis 1, ArrayLattices, no masks" << endl;

// Concatenate along axis 1

         LatticeConcat<Float> lc (1, False);
         lc.setLattice(ml1);
         lc.setLattice(ml2);

// Find output shape

         IPosition outShape = lc.shape();
         AlwaysAssert(outShape.nelements()==2, AipsError);
         AlwaysAssert(outShape(0)==shape(0), AipsError);
         AlwaysAssert(outShape(1)==shape(1)+shape(1), AipsError);
         AlwaysAssert(lc.isMasked()==False, AipsError);
         AlwaysAssert(lc.axis()==1, AipsError);

// Make output
   
         ArrayLattice<Float> l3(outShape);
         SubLattice<Float> ml3(l3, True);

// Do it

         lc.copyData(ml3);

// Check values
        
         check (1, ml3, ml1, ml2);
      }


// Make some PagedImages and give them a mask

      PagedImage<Float> im1(shape, CoordinateUtil::defaultCoords2D(),
                            "tLatticeConcat_tmp1.img");
      PagedImage<Float> im2(shape, CoordinateUtil::defaultCoords2D(),
                            "tLatticeConcat_tmp2.img");
      fillImage(im1, a1, True);
      fillImage(im2, a2, False);
      {
         cout << "Axis 0, PagedImages, masks" << endl;

// Concatenate along axis 0

         LatticeConcat<Float> lc (0, False);
         lc.setLattice(im1);
         lc.setLattice(im2);

// Find output shape

         IPosition outShape = lc.shape();
         AlwaysAssert(outShape.nelements()==2, AipsError);
         AlwaysAssert(outShape(0)==shape(0)+shape(0), AipsError);
         AlwaysAssert(outShape(1)==shape(1), AipsError);
         AlwaysAssert(lc.isMasked()==True, AipsError);
         AlwaysAssert(lc.axis()==0, AipsError);

// Make output

         PagedImage<Float> ml3(outShape, CoordinateUtil::defaultCoords2D(),
                               "tLatticeConcat_tmp3.img");
         fillImage(ml3, a1, True);

// Do it

         lc.copyData(ml3);

// Check values

         check (0, ml3, im1, im2);
      }


// Test setAxis

      {
         cout << "Test setAxis" << endl;

// Concatenate along axis 0

         LatticeConcat<Float> lc (0, False);
         lc.setLattice(ml1);
         lc.setLattice(ml2);
         lc.setAxis(1);

// Find output shape

         IPosition outShape = lc.shape();
         AlwaysAssert(outShape.nelements()==2, AipsError);
         AlwaysAssert(outShape(0)==shape(0), AipsError);
         AlwaysAssert(outShape(1)==shape(1)+shape(1), AipsError);
         AlwaysAssert(lc.axis()==1, AipsError);

// Make output
   
         ArrayLattice<Float> l3(outShape);
         SubLattice<Float> ml3(l3, True);

// Do it

         lc.copyData(ml3);

// Check values

         check (1, ml3, ml1, ml2);
      }


// Test reset

      {
         cout << "Test reset" << endl;

// Concatenate along axis 0

         LatticeConcat<Float> lc (1, False);
         lc.setLattice(ml1);
         lc.setLattice(ml2);
         lc.reset();

// Find output shape

         IPosition outShape = lc.shape();
         AlwaysAssert(outShape.nelements()==0, AipsError);
//
         lc.setAxis(1);
         lc.setLattice(ml1);
         lc.setLattice(ml2);
//
         outShape.resize(0);
         outShape = lc.shape();
         AlwaysAssert(outShape.nelements()==2, AipsError);
         AlwaysAssert(outShape(0)==shape(0), AipsError);
         AlwaysAssert(outShape(1)==shape(1)+shape(1), AipsError);
         AlwaysAssert(lc.axis()==1, AipsError);

// Make output
   
         ArrayLattice<Float> l3(outShape);
         SubLattice<Float> ml3(l3, True);

// Do it

         lc.copyData(ml3);

// Check values

         check (1, ml3, ml1, ml2);


      }


// Test copy constructor

     {
         cout << "Testing copy constructor" << endl;
         LatticeConcat<Float> lc (0, False);
         lc.setLattice(ml1);
         lc.setLattice(ml2);
         LatticeConcat<Float> lc2(lc);

// Find output shape

         AlwaysAssert(lc.shape().isEqual(lc2.shape()), AipsError);
         AlwaysAssert(lc.isMasked()==lc2.isMasked(), AipsError);
         AlwaysAssert(lc2.axis()==0, AipsError);

// Make output
   
         ArrayLattice<Float> l3(lc2.shape());
         SubLattice<Float> ml3(l3, True);

// Do it

         lc2.copyData(ml3);

// Check values

         check (0, ml3, ml1, ml2);
     }

// Test assignment 

     {
         cout << "Testing assignment " << endl;
         LatticeConcat<Float> lc (0, False);
         lc.setLattice(ml1);
         lc.setLattice(ml2);
         LatticeConcat<Float> lc2;
         lc2 = lc;

// Find output shape

         AlwaysAssert(lc.shape().isEqual(lc2.shape()), AipsError);
         AlwaysAssert(lc.isMasked()==lc2.isMasked(), AipsError);
         AlwaysAssert(lc2.axis()==0, AipsError);

// Make output
   
         ArrayLattice<Float> l3(lc2.shape());
         SubLattice<Float> ml3(l3, True);

// Do it

         lc2.copyData(ml3);

// Check values

         check (0, ml3, ml1, ml2);
     }

// Some forced errors

      {
         cout << "Forced errors" << endl;

// Concatenate along axis 0

         LatticeConcat<Float> lc (0, False);
         lc.setAxis(10);
         Bool ok = True;
         try {
            lc.setLattice(ml1);
            ok = False;
         } catch (AipsError x) {
         } end_try;
         if (!ok) {
            throw (AipsError("set forced failure did not work - this was unexpected"));  
         }
//
         ok = True;
         lc.setAxis(0);     
         lc.setLattice(ml1);
         lc.setLattice(ml2);
         try {
            lc.setAxis(20);
            ok = False;
         } catch (AipsError x) {;} end_try;
         if (!ok) {
            throw (AipsError("setAxis forced failure did not work - this was unexpected"));  
         }
//
         try {
            ArrayLattice<Float> l4(IPosition(3,2,2,2));
            SubLattice<Float> ml4(l4, True);
            lc.setLattice(ml4);
            ok = False;
         } catch (AipsError x) {;} end_try;
         if (!ok) {
            throw (AipsError("set forced failure did not work - this was unexpected"));  
         }
      }

  } catch(AipsError x) {
    cerr << x.getMesg() << endl;
    exit(1);
  } end_try;
  cout << "OK" << endl;
  exit(0);
};


void check (uInt axis, MaskedLattice<Float>& ml,
            MaskedLattice<Float>& ml1, MaskedLattice<Float>& ml2)
{
   IPosition shape1 = ml1.shape();
   IPosition shape2 = ml2.shape();
//
   IPosition blc(2,0,0);
   IPosition sliceShape(2,shape1(0), shape1(1));
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


void fillImage (ImageInterface<Float>& im, Array<Float>& a, Bool maskValue)
{
   im.put(a);
   LCPagedMask mask = LCPagedMask(RegionHandler::makeMask (im, "mask0"));
   mask.set(maskValue);
   im.defineRegion ("mask0", ImageRegion(mask), RegionHandler::Masks);
   im.setDefaultMask("mask0");
}

