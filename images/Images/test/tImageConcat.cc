//# tImageConcat.cc: This program tests the ImageConcat class
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
//#
//# $Id$


#include <casa/aips.h>
#include <casa/Arrays/Array.h>
#include <casa/Arrays/ArrayLogical.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Exceptions/Error.h>
#include <casa/IO/FileLocker.h>
#include <casa/Arrays/Slicer.h>
#include <casa/Arrays/IPosition.h>
#include <casa/BasicMath/Math.h>
#include <casa/Utilities/Assert.h>

#include <coordinates/Coordinates/CoordinateUtil.h>
#include <images/Images/ImageConcat.h>
#include <images/Images/PagedImage.h>
#include <images/Images/SubImage.h>
#include <images/Images/ImageInterface.h>
#include <images/Regions/ImageRegion.h>
#include <lattices/Lattices/LCPagedMask.h>
#include <lattices/Lattices/ArrayLattice.h>
#include <lattices/Lattices/SubLattice.h>
#include <casa/iostream.h>



#include <casa/namespace.h>
void check (uInt axis, MaskedLattice<Float>& ml,
            MaskedLattice<Float>& ml1, 
            MaskedLattice<Float>& ml2);
void check (uInt axis, MaskedLattice<Float>& ml,
            MaskedLattice<Float>& ml1, 
            MaskedLattice<Float>& ml2,
            MaskedLattice<Float>& ml3);
void makeMask (ImageInterface<Float>& im, Bool maskValue, Bool set);
void testLogger();

int main() {
  try {

// Make some Arrays

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

// Make some PagedImages and give them a mask

      PagedImage<Float> im1(shape, CoordinateUtil::defaultCoords2D(),
                            "tImageConcat_tmp1.img");
      PagedImage<Float> im2(shape, CoordinateUtil::defaultCoords2D(),
                            "tImageConcat_tmp2.img");
      makeMask(im1, True, True); 
      makeMask(im2, False, True);
      im1.put(a1); 
      im2.put(a2);

// Make a MaskedLattice as well

     ArrayLattice<Float> al1(a1);
     SubLattice<Float> ml1(al1);
 
//
      {
         cout << "Axis 0, PagedImages (masks)" << endl;

// Concatenate along axis 0

         ImageConcat<Float> lc (0, True);
         lc.setImage(im1, True);
         lc.setImage(im2, True);

// Find output shape

         IPosition outShape = lc.shape();
         AlwaysAssert(outShape.nelements()==2, AipsError);
         AlwaysAssert(outShape(0)==shape(0)+shape(0), AipsError);
         AlwaysAssert(outShape(1)==shape(1), AipsError);
         AlwaysAssert(lc.isMasked()==True, AipsError);
         AlwaysAssert(lc.hasPixelMask()==True, AipsError);

// Make output

         PagedImage<Float> ml3(outShape, CoordinateUtil::defaultCoords2D(),
                               "tImageConcat_tmp3.img");
         makeMask(ml3, True, False);

// Copy to output

         ml3.copyData(lc);
         ml3.pixelMask().put(lc.getMask());

// Check values

         check (0, ml3, im1, im2);
      }

//
      {
         cout << "Axis 1, PagedImages (masks)" << endl;

// Concatenate along axis 1

         ImageConcat<Float> lc (1, False);
         lc.setImage(im1, True);
         lc.setImage(im2, True);

// Find output shape

         IPosition outShape = lc.shape();
         AlwaysAssert(outShape.nelements()==2, AipsError);
         AlwaysAssert(outShape(0)==shape(0), AipsError);
         AlwaysAssert(outShape(1)==shape(1)+shape(1), AipsError);
         AlwaysAssert(lc.isMasked()==True, AipsError);
         AlwaysAssert(lc.hasPixelMask()==True, AipsError);

// Make output

         PagedImage<Float> ml3(outShape, CoordinateUtil::defaultCoords2D(),
                               "tImageConcat_tmp3.img");
         makeMask(ml3, True, False);

// Copy to output

         ml3.copyData(lc);
         ml3.pixelMask().put(lc.getMask());

// Check values

         check (1, ml3, im1, im2);
      }


      {
         cout << "Axis 0, PagedImages (masks) + MaskedLattice (no mask)" << endl;

// Concatenate along axis 0

         ImageConcat<Float> lc (0);
         lc.setImage(im1, True);
         lc.setImage(im2, True);
         lc.setLattice(ml1);

// Find output shape

         IPosition outShape = lc.shape();
         AlwaysAssert(outShape.nelements()==2, AipsError);
         AlwaysAssert(outShape(0)==3*shape(0), AipsError);
         AlwaysAssert(outShape(1)==shape(1), AipsError);
         AlwaysAssert(lc.isMasked()==True, AipsError);
         AlwaysAssert(lc.hasPixelMask()==True, AipsError);
         AlwaysAssert(lc.pixelMask().isWritable()==False, AipsError);

// Make output

         PagedImage<Float> ml3(outShape, CoordinateUtil::defaultCoords2D(),
                               "tImageConcat_tmp3.img");
         makeMask(ml3, True, False);

// Copy to output

         ml3.copyData(lc);
         ml3.pixelMask().put(lc.getMask());

// Check values

         check (0, ml3, im1, im2, ml1);
      }



// Contiguity test

      {
         cout << "Contiguity, axis 0, PagedImages, no masks" << endl;

// Make an image and then chop it up and glue it back together
// Thus we can test the coordinate contiguity demands


         IPosition shape2(2, 30, 10);
         PagedImage<Float> ml3(shape2, CoordinateUtil::defaultCoords2D(),
                               "tImageConcat_tmp3.img");
//
         Slicer sl1(IPosition(2,0,0), IPosition(2,9,9), Slicer::endIsLast);
         Slicer sl2(IPosition(2,10,0), IPosition(2,19,9), Slicer::endIsLast);         
         Slicer sl3(IPosition(2,20,0), IPosition(2,29,9), Slicer::endIsLast);
//
         SubImage<Float> si1(ml3, sl1, True); si1.set(1.0);
         SubImage<Float> si2(ml3, sl2, True); si2.set(2.0);
         SubImage<Float> si3(ml3, sl3, True); si3.set(3.0);

// Concatenate along axis 0

         ImageConcat<Float> lc (0);
         lc.setImage(si1, False);
         lc.setImage(si2, False);
         lc.setImage(si3, False);

// Find output shape

         IPosition outShape = lc.shape();
         AlwaysAssert(outShape.nelements()==2, AipsError);
         AlwaysAssert(shape2.isEqual(outShape), AipsError);
         AlwaysAssert(lc.isMasked()==False, AipsError);
         AlwaysAssert(lc.hasPixelMask()==False, AipsError);

// Make output

         PagedImage<Float> ml4(outShape, CoordinateUtil::defaultCoords2D(),
                               "tImageConcat_tmp4.img");

// Copy to output

         ml4.copyData(lc);

// Check values

         check (0, ml4, si1, si2, si3);
      }

// Test lock etc
            
     {
         cout << "Testing locking" << endl;
         ImageConcat<Float> lc2 (0, False);
         lc2.setImage(im1, True);
         lc2.setImage(im2, True);
         AlwaysAssert(lc2.lock(FileLocker::Read, 1), AipsError);
         AlwaysAssert(lc2.hasLock(FileLocker::Read), AipsError);
         AlwaysAssert(lc2.lock(FileLocker::Write, 1), AipsError);
         AlwaysAssert(lc2.hasLock(FileLocker::Write), AipsError);
         lc2.unlock();
         AlwaysAssert(!lc2.hasLock(FileLocker::Read), AipsError);
         AlwaysAssert(!lc2.hasLock(FileLocker::Write), AipsError);
     }


// Test copy constructor

     {
         cout << "Testing copy constructor" << endl;
         ImageConcat<Float> lc (0);
         lc.setImage(im1, True);
         lc.setImage(im2, True);
         ImageConcat<Float> lc2(lc);

// Find output shape

         AlwaysAssert(lc.shape().isEqual(lc2.shape()), AipsError);
         AlwaysAssert(lc.isMasked()==lc2.isMasked(), AipsError);
         AlwaysAssert(lc.hasPixelMask()==lc2.hasPixelMask(), AipsError);

// Make output
   
         PagedImage<Float> ml3(lc2.shape(), CoordinateUtil::defaultCoords2D(),
                               "tImageConcat_tmp3.img");
         makeMask(ml3, True, False);

// Copy to output

         ml3.copyData(lc);
         ml3.pixelMask().put(lc.getMask());

// Check values

         check (0, ml3, im1, im2);
     }

// Test assignment 

     {
         cout << "Testing assignment " << endl;
         ImageConcat<Float> lc (0);
         lc.setImage(im1, True);
         lc.setImage(im2, True);
         ImageConcat<Float> lc2;
         lc2 = lc;

// Find output shape

         AlwaysAssert(lc.shape().isEqual(lc2.shape()), AipsError);
         AlwaysAssert(lc.isMasked()==lc2.isMasked(), AipsError);
         AlwaysAssert(lc.hasPixelMask()==lc2.hasPixelMask(), AipsError);

// Make output
   
         PagedImage<Float> ml3(lc2.shape(), CoordinateUtil::defaultCoords2D(),
                               "tImageConcat_tmp3.img");
         makeMask(ml3, True, False);

// Copy to output

         ml3.copyData(lc);
         ml3.pixelMask().put(lc.getMask());

// Check values

         check (0, ml3, im1, im2);
     }

// Some forced errors

      {
         cout << "Forced errors" << endl;

         ImageConcat<Float> lc (10);
         Bool ok = True;
         try {
            lc.setImage(im1, True);
            ok = False;
         } catch (AipsError x) {
         } 
         if (!ok) {
            throw (AipsError("set forced failure did not work - this was unexpected"));  
         }
//
         try {
             PagedImage<Float> ml4(IPosition(3,10,10,10),
                                   CoordinateUtil::defaultCoords3D(),
                               "tImageConcat_tmp3.img");
            lc.setImage(ml4, True);
            ok = False;
         } catch (AipsError x) {;} 
         if (!ok) {
            throw (AipsError("set forced failure did not work - this was unexpected"));  
         }
      }

      // Check if the LoggerHolder works fine for concatenated images.
      testLogger();

  } catch(AipsError x) {
    cerr << x.getMesg() << endl;
    return 1;
  } 
  cout << "OK" << endl;
  return 0;
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


void check (uInt axis, MaskedLattice<Float>& ml,
            MaskedLattice<Float>& ml1, 
            MaskedLattice<Float>& ml2,
            MaskedLattice<Float>& ml3)
{
   IPosition shape1 = ml1.shape();
   IPosition shape2 = ml2.shape();
   IPosition shape3 = ml3.shape();
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
//
      blc(0) += shape2(0);
      AlwaysAssert(allEQ(ml3.get(), ml.getSlice(blc,shape3)), AipsError);      
      AlwaysAssert(allEQ(ml3.getMask(), ml.getMaskSlice(blc,shape3)), AipsError);      
   } else if (axis==1) {
      blc(1) += shape1(1);
      AlwaysAssert(allEQ(ml2.get(), ml.getSlice(blc,shape2)), AipsError);
      AlwaysAssert(allEQ(ml2.getMask(), ml.getMaskSlice(blc,shape2)), AipsError);
//
      blc(1) += shape2(1);
      AlwaysAssert(allEQ(ml3.get(), ml.getSlice(blc,shape3)), AipsError);      
      AlwaysAssert(allEQ(ml3.getMask(), ml.getMaskSlice(blc,shape3)), AipsError);      
   } else {
      AlwaysAssert(axis==0||axis==1, AipsError);
   }
}



void makeMask (ImageInterface<Float>& im, Bool maskValue, Bool set)
{
   im.makeMask ("mask0", True, True, set, maskValue);
}


void testLogger()
{
  // Make a concatenated image and make sure the image objects are gone.
   ImageConcat<Float> lc (0, True);
   ImageConcat<Float> lc2 (0, True);
   {
// Make some Arrays

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

// Make some PagedImages and add messages to their logger.
// Add the images to the Concat.

      PagedImage<Float> im1(shape, CoordinateUtil::defaultCoords2D(),
                            "tImageConcat_tmp1.imga");
      PagedImage<Float> im2(shape, CoordinateUtil::defaultCoords2D(),
                            "tImageConcat_tmp2.imga");
      im1.put(a1); 
      im2.put(a2);
      lc.setImage(im1, True);
      lc.setImage(im2, True);
      lc2.setImage(im2, True);
      lc2.setImage(im1, True);
      lc2.setImage(im2, True);
      im1.logger().logio() << "message1a" << LogIO::POST;
      im1.logger().logio() << "message1b" << LogIO::POST;
      im2.logger().logio() << "message2" << LogIO::POST;
   }
   // Add a message and check if the concatenation has 4 messages.
   LoggerHolder& logger = lc.logger();
   logger.logio() << "message_conc" << LogIO::POST;
   uInt nmsg=0;
   for (LoggerHolder::const_iterator iter = logger.begin(); iter != logger.end(); iter++) {
      cout << iter->message() << endl;
      nmsg++;
   }
   AlwaysAssertExit (nmsg == 4);

   // If it also works well with the copy ctor and the assignent operator.
   {
     ImageConcat<Float> ic2(lc2);
     {
       LoggerHolder& logger = ic2.logger();
       logger.logio() << "message_conc2" << LogIO::POST;
       uInt nmsg=0;
       for (LoggerHolder::const_iterator iter = logger.begin(); iter != logger.end(); iter++) {
	 cout << iter->message() << endl;
	 nmsg++;
       }
       AlwaysAssertExit (nmsg == 5);
     }
     ic2 = lc;
     {
       LoggerHolder& logger = ic2.logger();
       uInt nmsg=0;
       for (LoggerHolder::const_iterator iter = logger.begin(); iter != logger.end(); iter++) {
	 cout << iter->message() << endl;
	 nmsg++;
       }
       AlwaysAssertExit (nmsg == 4);
     }
   }
}
