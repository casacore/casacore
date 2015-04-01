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


#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/IO/FileLocker.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Utilities/Assert.h>

#include <casacore/coordinates/Coordinates/CoordinateUtil.h>
#include <casacore/images/Images/ImageConcat.h>
#include <casacore/images/Images/PagedImage.h>
#include <casacore/images/Images/TempImage.h>
#include <casacore/images/Images/ImageOpener.h>

#include <casacore/images/Images/SubImage.h>
#include <casacore/images/Images/ImageInterface.h>
#include <casacore/images/Regions/ImageRegion.h>
#include <casacore/lattices/LRegions/LCPagedMask.h>
#include <casacore/lattices/Lattices/ArrayLattice.h>
#include <casacore/lattices/Lattices/SubLattice.h>
#include <casacore/casa/iostream.h>



#include <casacore/casa/namespace.h>
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

// Save the concatenated image and read it back.
         AlwaysAssertExit (!lc.isPersistent());
         lc.save ("tImageConcat_tmp.imgconc");
         AlwaysAssertExit (lc.isPersistent());
         LatticeBase* latt = ImageOpener::openImage ("tImageConcat_tmp.imgconc");
         ImageConcat<Float>* lc3 = dynamic_cast<ImageConcat<Float>*>(latt);
         AlwaysAssertExit (lc3 != 0);
         AlwaysAssertExit (allEQ(lc3->get(), lc.get()));
         AlwaysAssertExit (allEQ(lc3->getMask(), lc.getMask()));
         delete lc3;
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
#ifndef AIPS_TABLE_NOLOCKING
         AlwaysAssert(!lc2.hasLock(FileLocker::Read), AipsError);
         AlwaysAssert(!lc2.hasLock(FileLocker::Write), AipsError);
#endif
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

    {
    	  cout << "Noncontiguous spectral axis test - CAS-4317" << endl;
    	  CoordinateSystem csys = CoordinateUtil::defaultCoords3D();
    	  Vector<Double> freqs(4);
    	  freqs[0] = 1.41e9;
    	  freqs[1] = 1.42e9;
    	  freqs[2] = 1.44e9;
    	  freqs[3] = 1.47e9;
    	  Double restfreq1 = 1.40e9;
    	  SpectralCoordinate sp1(MFrequency::LSRK, freqs, restfreq1);
    	  csys.replaceCoordinate(sp1, 1);
    	  TempImage<Float> t1(TiledShape(IPosition(3, 1, 1, 4)), csys);
    	  ImageInfo info1 = t1.imageInfo();
    	  GaussianBeam beam1(Quantity(2, "arcsec"), Quantity(1, "arcsec"), Quantity(2, "deg"));
    	  info1.setRestoringBeam(beam1);
    	  t1.setImageInfo(info1);
    	  Vector<Double> gfreqs(3);
    	  gfreqs[0] = 1.52e9;
    	  gfreqs[1] = 1.53e9;
    	  gfreqs[2] = 1.54e9;
    	  Double restfreq2 = 1.5e9;
    	  SpectralCoordinate sp2(MFrequency::LSRK, gfreqs, restfreq2);
    	  csys.replaceCoordinate(sp2, 1);
    	  TempImage<Float> t2(TiledShape(IPosition(3, 1, 1, 3)), csys);
    	  ImageInfo info2 = t2.imageInfo();
    	  GaussianBeam beam2(Quantity(4, "arcsec"), Quantity(3, "arcsec"), Quantity(20, "deg"));
    	  info2.setRestoringBeam(beam2);
    	  t2.setImageInfo(info2);
    	  ImageConcat<Float> concat(2);
    	  concat.setImage(t1, True);
    	  concat.setImage(t2, True);
    	  SpectralCoordinate newsp = concat.coordinates().spectralCoordinate();
    	  Double world;
    	  for (uInt i=0; i<7; i++) {
			  newsp.toWorld(world, i);
			  GaussianBeam beam = concat.imageInfo().restoringBeam(i, -1);
    		  if (i < 4) {
    			  AlwaysAssert(world == freqs[i], AipsError);
    			  AlwaysAssert(beam == beam1, AipsError);
    		  }
    		  else {
    			  cout << beam<<endl;
    			  cout <<beam2<<endl;
    			  AlwaysAssert(world == gfreqs[i-4], AipsError);
    			  AlwaysAssert(beam == beam2, AipsError);
    		  }
    	  }
		  AlwaysAssert(newsp.restFrequency() == restfreq1, AipsError);


    	  cout << "Noncontiguous spectral axis test concating 3 images - CAS-4319" << endl;
		  Vector<Double> hfreqs(3);
		  hfreqs[0] = 1.61e9;
		  hfreqs[1] = 1.62e9;
		  hfreqs[2] = 1.64e9;
		  Double restfreq3 = 1.6e9;
		  SpectralCoordinate sp3(MFrequency::LSRK, hfreqs, restfreq3);
		  csys.replaceCoordinate(sp3, 1);
    	  TempImage<Float> t3(TiledShape(IPosition(3, 1, 1, 3)), csys);
    	  ImageInfo info3 = t3.imageInfo();
    	  GaussianBeam beam3(Quantity(10, "arcsec"), Quantity(7, "arcsec"), Quantity(80, "deg"));
    	  info3.setRestoringBeam(beam3);
    	  t3.setImageInfo(info3);
    	  concat.setImage(t3, True);
    	  newsp = concat.coordinates().spectralCoordinate();
    	  for (uInt i=0; i<10; i++) {
    		  newsp.toWorld(world, i);
    		  GaussianBeam beam = concat.imageInfo().restoringBeam(i, -1);
    		  if (i < 4) {
    			  AlwaysAssert(world == freqs[i], AipsError);
    			  AlwaysAssert(beam == beam1, AipsError);
    		  }
    		  else if (i < 7) {
    			  AlwaysAssert(world == gfreqs[i-4], AipsError);
    			  AlwaysAssert(beam == beam2, AipsError);
    		  }
    		  else {
    			  AlwaysAssert(world == hfreqs[i-7], AipsError);
    			  AlwaysAssert(beam == beam3, AipsError);
    		  }
    	  }
    	  AlwaysAssert(newsp.restFrequency() == restfreq1, AipsError);
    }
    {
    	cout << "*** single beams concat test with stokes, CAS-4423" << endl;
    	CoordinateSystem csys = CoordinateUtil::defaultCoords4D();
    	TempImage<Float> i0(TiledShape(IPosition(4, 10, 10, 1, 8)), csys);
    	ImageInfo ii = i0.imageInfo();
    	ii.setAllBeams(
    		8, 1, GaussianBeam(
    			Quantity(4, "arcsec"), Quantity(3, "arcsec"), Quantity(0, "deg")
    		)
    	);
    	i0.setImageInfo(ii);

    	Vector<Double> refVal = csys.referenceValue();
    	refVal[3] += 1e9;
    	csys.setReferenceValue(refVal);
    	TempImage<Float> i1(TiledShape(IPosition(4, 10, 10, 1, 27)), csys);
    	ii = i1.imageInfo();
    	ii.setAllBeams(
    		27, 1, GaussianBeam(
    			Quantity(8, "arcsec"), Quantity(6, "arcsec"), Quantity(0, "deg")
    		)
  	    );
    	i1.setImageInfo(ii);
    	ImageConcat<Float> concat(3, False);
    	concat.setImage(i0, True);
    	concat.setImage(i1, True);



    }
    {
    	cout << "*** Stokes concatenation" << endl;
    	CoordinateSystem csys = CoordinateUtil::defaultCoords4D();
    	TempImage<Float> i0(TiledShape(IPosition(4, 5, 5, 4, 5)), csys);
    	TempImage<Float> i1(TiledShape(IPosition(4, 5, 5, 4, 5)), csys);
    	SubImage<Float> s0(i0, Slicer(
    		IPosition(4, 0), IPosition(4, 4, 4, 0, 4),
    		Slicer::endIsLast)
    	);
    	SubImage<Float> s1(i0, Slicer(
    		IPosition(4, 0, 0, 2, 0), IPosition(4, 4, 4, 2, 4),
    		Slicer::endIsLast)
    	);
    	cout << "first " << s0.coordinates().stokesCoordinate().stokes() << endl;
    	cout << "second " << s1.coordinates().stokesCoordinate().stokes() << endl;
    	ImageConcat<Float> concat(2);
    	concat.setImage(s0, False);
    	concat.setImage(s1, False);
        Vector<Int> outStokes = concat.coordinates().stokesCoordinate().stokes();
        AlwaysAssert(outStokes.size() == 2, AipsError);
        AlwaysAssert(outStokes[0] == 1, AipsError);
        AlwaysAssert(outStokes[1] == 3, AipsError);
    }
    {
        cout << "Test adding first image contiguous, next not produces expected world values" << endl;
    	CoordinateSystem csys = CoordinateUtil::defaultCoords4D();
        TempImage<Float> i0(TiledShape(IPosition(4, 5, 5, 4, 5)), csys);
    	SubImage<Float> s0(i0, Slicer(
    		IPosition(4, 0), IPosition(4, 4, 4, 3, 0),
    		Slicer::endIsLast)
    	);
    	SubImage<Float> s1(i0, Slicer(
    		IPosition(4, 0, 0, 0, 1), IPosition(4, 4, 4, 3, 1),
    		Slicer::endIsLast)
    	);
    	SubImage<Float> s3(i0, Slicer(
    		IPosition(4, 0, 0, 0, 3), IPosition(4, 4, 4, 3, 3),
    		Slicer::endIsLast)
    	);
        ImageConcat<Float> concat(3);
        concat.setImage(s0, False);
        Vector<Int> v0(4, 0);
        Vector<Int> v1(4, 0);
        v1[3] = 1;
        Vector<Int> v2(4, 0);
        v2[3] = 2;
        AlwaysAssert(
            concat.coordinates().toWorld(v0)[3] == s0.coordinates().toWorld(v0)[3],
            AipsError
        );
        concat.setImage(s1, False);
        AlwaysAssert(
            concat.coordinates().toWorld(v0)[3] == s0.coordinates().toWorld(v0)[3],
            AipsError
        );
        AlwaysAssert(
            concat.coordinates().toWorld(v1)[3] == s1.coordinates().toWorld(v0)[3],
            AipsError
        );
        concat.setImage(s3, True);
        cout << "get " << std::setprecision(10) << concat.coordinates().toWorld(v0)[3] << endl;
        cout << "exp " << std::setprecision(10) << s0.coordinates().toWorld(v0)[3] << endl;
        AlwaysAssert(
            concat.coordinates().toWorld(v0)[3] == s0.coordinates().toWorld(v0)[3],
            AipsError
        );
        cout << "get " << std::setprecision(10) << concat.coordinates().toWorld(v1)[3] << endl;
        cout << "exp " << std::setprecision(10) << s1.coordinates().toWorld(v0)[3] << endl;
        AlwaysAssert(
            concat.coordinates().toWorld(v1)[3] == s1.coordinates().toWorld(v0)[3],
            AipsError
        );
        cout << "get " << std::setprecision(10) << concat.coordinates().toWorld(v2)[3] << endl;
        cout << "exp " << std::setprecision(10) << s3.coordinates().toWorld(v0)[3] << endl;
        cout << "spec values " << std::setprecision(10) << concat.coordinates().spectralCoordinate().worldValues() << endl;
        AlwaysAssert(
            concat.coordinates().toWorld(v2)[3] == s3.coordinates().toWorld(v0)[3],
            AipsError
        );
  
    }
  } catch(const AipsError& x) {
    cerr << x.getMesg() << endl;
    return 1;
  } 
  cout << "OK" << endl;
  return 0;
}


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
   {
         cout << "Noncontiguous spectral axis test - CAS-4317" << endl;
         CoordinateSystem csys = CoordinateUtil::defaultCoords3D();
         Vector<Double> freqs(4);
         freqs[0] = 1.41e9;
         freqs[1] = 1.42e9;
         freqs[2] = 1.44e9;
         freqs[3] = 1.47e9;
         Double restfreq1 = 1.40e9;
         SpectralCoordinate sp1(MFrequency::LSRK, freqs, restfreq1);
         csys.replaceCoordinate(sp1, 1);
         TempImage<Float> t1(TiledShape(IPosition(3, 1, 1, 4)), csys);
         ImageInfo info1 = t1.imageInfo();
         GaussianBeam beam1(Quantity(2, "arcsec"), Quantity(1, "arcsec"), Quantity(2, "deg"));
         info1.setRestoringBeam(beam1);
         t1.setImageInfo(info1);
         Vector<Double> gfreqs(3);
         gfreqs[0] = 1.52e9;
         gfreqs[1] = 1.53e9;
         gfreqs[2] = 1.54e9;
         Double restfreq2 = 1.5e9;
         SpectralCoordinate sp2(MFrequency::LSRK, gfreqs, restfreq2);
         csys.replaceCoordinate(sp2, 1);
         TempImage<Float> t2(TiledShape(IPosition(3, 1, 1, 3)), csys);
         ImageInfo info2 = t2.imageInfo();
         GaussianBeam beam2(Quantity(4, "arcsec"), Quantity(3, "arcsec"), Quantity(20, "deg"));
         info2.setRestoringBeam(beam2);
         t2.setImageInfo(info2);
         ImageConcat<Float> concat(2);
         concat.setImage(t1, True);
         AlwaysAssert(concat.shape() == t1.shape(), AipsError);
         concat.setImage(t2, True);
         SpectralCoordinate newsp = concat.coordinates().spectralCoordinate();
         Double world;
         for (uInt i=0; i<7; i++) {
             newsp.toWorld(world, i);
             GaussianBeam beam = concat.imageInfo().restoringBeam(i, -1);
             if (i < 4) {
                 AlwaysAssert(world == freqs[i], AipsError);
                 AlwaysAssert(beam == beam1, AipsError);
             }
             else {
                 AlwaysAssert(world == gfreqs[i-4], AipsError);
                 AlwaysAssert(beam == beam2, AipsError);
             }
         }
         AlwaysAssert(newsp.restFrequency() == restfreq1, AipsError);
         cout << "Noncontiguous spectral axis test concating 3 images - CAS-4319" << endl;
         Vector<Double> hfreqs(3);
         hfreqs[0] = 1.61e9;
         hfreqs[1] = 1.62e9;
         hfreqs[2] = 1.64e9;
         Double restfreq3 = 1.6e9;
         SpectralCoordinate sp3(MFrequency::LSRK, hfreqs, restfreq3);
         csys.replaceCoordinate(sp3, 1);
         TempImage<Float> t3(TiledShape(IPosition(3, 1, 1, 3)), csys);
         ImageInfo info3 = t3.imageInfo();
         GaussianBeam beam3(Quantity(10, "arcsec"), Quantity(7, "arcsec"), Quantity(80, "deg"));
         info3.setRestoringBeam(beam3);
         t3.setImageInfo(info3);
         concat.setImage(t3, True);
         newsp = concat.coordinates().spectralCoordinate();
         for (uInt i=0; i<10; i++) {
             newsp.toWorld(world, i);
             GaussianBeam beam = concat.imageInfo().restoringBeam(i, -1);
             if (i < 4) {
                 AlwaysAssert(world == freqs[i], AipsError);
                 AlwaysAssert(beam == beam1, AipsError);
             }
             else if (i < 7) {
                 AlwaysAssert(world == gfreqs[i-4], AipsError);
                 AlwaysAssert(beam == beam2, AipsError);
             }
             else {
                 AlwaysAssert(world == hfreqs[i-7], AipsError);
                 AlwaysAssert(beam == beam3, AipsError);
             }
         }
         AlwaysAssert(newsp.restFrequency() == restfreq1, AipsError);
   }

}
