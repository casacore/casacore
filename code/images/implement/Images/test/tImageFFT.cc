//# tImageFFT.cc: test ImageFFT class
//# Copyright (C) 1996,1997,1998,1999,2000
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This library is free software; you can redistribute it and/or modify it
//# under the terms of the GNU Library General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or (at your
//# option) any later version.
//#
//# This library is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
//# License for more details.
//#
//# You should have received a copy of the GNU Library General Public License
//# along with this library; if not, write to the Free Software Foundation,
//# Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$
// 
//
#include <aips/aips.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Mathematics/FFTServer.h>
#include <aips/Mathematics/Math.h>
#include <aips/Tasking/Aipsrc.h>
#include <aips/Exceptions/Error.h>
#include <aips/Inputs/Input.h>
#include <aips/Logging.h>
#include <aips/Utilities/String.h>
  
#include <trial/Images/ImageFFT.h>
#include <trial/Images/ImageRegion.h>
#include <trial/Images/PagedImage.h>
#include <trial/Lattices/LCPagedMask.h>

#include <iostream.h>

void checkNumbers (const ImageInterface<Float>& rIn,
                   const ImageInterface<Float>& rOut,
                   const ImageInterface<Float>& iOut,
                   const ImageInterface<Float>& aOut,
                   const ImageInterface<Float>& pOut,
                   const ImageInterface<Complex>& cOut);

void checkNumbers (const ImageInterface<Float>& rIn,
                   const ImageInterface<Float>& rOut,
                   const ImageInterface<Float>& iOut,
                   const ImageInterface<Float>& aOut,
                   const ImageInterface<Float>& pOut,
                   const ImageInterface<Complex>& cOut,
                   const Vector<Bool>& axes);

void makeMask(ImageInterface<Float>& out);
void makeMask(ImageInterface<Complex>& out);

main (int argc, char **argv)
{
try {

   Input inputs(1);
   inputs.version ("$Revision$");


// Get inputs

   String root = Aipsrc::aipsRoot();
   String name = root + "/data/demo/Images/test_image";
   inputs.create("in", name, "Input file name");
   inputs.create("axes", "-10", "axes");
   inputs.readArguments(argc, argv);

   const String in = inputs.getString("in");
   const Block<Int> axes = inputs.getIntArray("axes");
   LogOrigin or("tImageFFT", "main()", WHERE);
   LogIO os(or);
 

// Check image name and get image data type. 

   if (in.empty()) {
      os << LogIO::NORMAL << "You must specify the image file name" << LogIO::POST;
      return 1;
   }
//
   DataType imageType = imagePixelType(in);
   if (imageType==TpFloat) {
      os << LogIO::NORMAL << "Create images" << LogIO::POST;

// Make images

      PagedImage<Float> inImage(in, True);
//
      IPosition outShape(inImage.shape());
      PagedImage<Float> outReal(outShape, inImage.coordinates(), "tImageFFT_real.img");
      if (inImage.isMasked()) makeMask(outReal);
      PagedImage<Float> outImag(outShape, inImage.coordinates(), "tImageFFT_imag.img");
      if (inImage.isMasked()) makeMask(outImag);
      PagedImage<Float> outAmp(outShape, inImage.coordinates(), "tImageFFT_amp.img");
      if (inImage.isMasked()) makeMask(outAmp);
      PagedImage<Float> outPhase(outShape, inImage.coordinates(), "tImageFFT_phase.img");
      if (inImage.isMasked()) makeMask(outPhase);
      PagedImage<Complex> outComplex(outShape, inImage.coordinates(), "tImageFFT_complex.img");
      if (inImage.isMasked()) makeMask(outComplex);

// FFT the sky only

      {
         os << LogIO::NORMAL << "FFT the sky" << LogIO::POST;
         ImageFFT fft;
         fft.fftsky(inImage);
         Array<Float> rArray0 = inImage.get();
//
         os << LogIO::NORMAL << "Get FFT and check values" << LogIO::POST;
         fft.getReal(outReal);
         fft.getImaginary(outImag);
         fft.getAmplitude(outAmp);
         fft.getPhase(outPhase);
         fft.getComplex(outComplex);
//
         checkNumbers(inImage, outReal, outImag, outAmp, outPhase, 
                      outComplex);

// Copy constructor

         os << LogIO::NORMAL << "Copy constructor, get FFT and check values" << LogIO::POST;
         ImageFFT fft2(fft);
         fft2.getReal(outReal);
         fft2.getImaginary(outImag);
         fft2.getAmplitude(outAmp);
         fft2.getPhase(outPhase);
         fft2.getComplex(outComplex);
//
         checkNumbers(inImage, outReal, outImag, outAmp, outPhase, 
                      outComplex);

// Assignment operator

         os << LogIO::NORMAL << "Assignment operator, get FFT and check values" << LogIO::POST;
         ImageFFT fft3;
         fft3 = fft2;
         fft3.getReal(outReal);
         fft3.getImaginary(outImag);
         fft3.getAmplitude(outAmp);
         fft3.getPhase(outPhase);
         fft3.getComplex(outComplex);
//
         checkNumbers(inImage, outReal, outImag, outAmp, outPhase, 
                      outComplex);
      }


// Multi dimensional FFT.  

      {
         os << LogIO::NORMAL << "FFT all dimensions" << LogIO::POST;
         IPosition outShape(inImage.shape());
         PagedImage<Float> outReal2(outShape, inImage.coordinates(), "tImageFFT_real2.img");
         if (inImage.isMasked()) makeMask(outReal2);
         PagedImage<Float> outImag2(outShape, inImage.coordinates(), "tImageFFT_imag2.img");
         if (inImage.isMasked()) makeMask(outImag2);
         PagedImage<Float> outAmp2(outShape, inImage.coordinates(), "tImageFFT_amp2.img");
         if (inImage.isMasked()) makeMask(outAmp2);
         PagedImage<Float> outPhase2(outShape, inImage.coordinates(), "tImageFFT_phase2.img");
         if (inImage.isMasked()) makeMask(outPhase2);
         PagedImage<Complex> outComplex2(outShape, inImage.coordinates(), "tImageFFT_complex2.img");
         if (inImage.isMasked()) makeMask(outComplex2);
//
         Vector<Bool> which(inImage.ndim(), True);
         if (axes.nelements()==1 && axes[0]==-10) {
            ;
         } else {
            for (uInt i=0; i<inImage.ndim(); i++) {
               Bool found = False;
               for (uInt j=0; j<axes.nelements(); j++) {
                 if (axes[j]==Int(i)) found = True;
               }
               which(i) = found;
            }
         }
//
         ImageFFT fft;
         fft.fft(inImage, which);
         fft.getReal(outReal2);
         fft.getImaginary(outImag2);
         fft.getAmplitude(outAmp2);
         fft.getPhase(outPhase2);
         fft.getComplex(outComplex2);
//
         checkNumbers (inImage, outReal2, outImag2, outAmp2, outPhase2, 
                       outComplex2, which);

      }
   } else {
      os << LogIO::NORMAL << "images of type " << Int(imageType)
	 << " not yet supported" << LogIO::POST;
      exit(1);
   }
}

  catch (AipsError x) {
     cerr << "aipserror: error " << x.getMesg() << endl;
     exit(1);
  } 

  exit(0);
}



void checkNumbers (const ImageInterface<Float>& rIn,
                   const ImageInterface<Float>& rOut,
                   const ImageInterface<Float>& iOut,
                   const ImageInterface<Float>& aOut,
                   const ImageInterface<Float>& pOut,
                   const ImageInterface<Complex>& cOut)
//
// Make tests on first plane of image only as ImageFFT
// only FFTs the sky plane whereas FFTServer does a
// multi-dimensional FFT
//
{
// Fill a complex array with the real input
// image.  Replace masked values by zero
      
      IPosition blc(rIn.ndim(),0);
      IPosition shape(rIn.shape());
      for (uInt i=0; i<rIn.ndim(); i++) {
         if (i>1) shape(i) = 1;
      }
      Array<Float> r0 = rIn.getSlice(blc, shape, True);
      Array<Bool> m0 = rIn.getMaskSlice(blc, shape, True);
      if (rIn.isMasked()) {
         Bool deleteIt1, deleteIt2;
         Float* pP = r0.getStorage(deleteIt1);
         const Bool*  pM = m0.getStorage(deleteIt2);
         for (Int i=0; i<r0.shape().product(); i++) {
            if (!pM[i]) pP[i] = 0.0;
         }
         r0.putStorage(pP, deleteIt1);
         m0.freeStorage(pM, deleteIt2);
      }
//
      Array<Complex> c(r0.shape());
      convertArray(c, r0);

// Make FFT server and FFT

      FFTServer<Float,Complex> fftServer(c.shape(),FFTEnums::COMPLEX);
      fftServer.fft(c, True);

// Check numbers

      AlwaysAssert(allNear(rOut.getSlice(blc,shape,True), real(c), 1e-6), AipsError);
      AlwaysAssert(allNear(iOut.getSlice(blc,shape,True), imag(c), 1e-6), AipsError);
      AlwaysAssert(allNear(aOut.getSlice(blc,shape,True), amplitude(c), 1e-6), AipsError);
      AlwaysAssert(allNear(pOut.getSlice(blc,shape,True), phase(c), 1e-6), AipsError);
      AlwaysAssert(allNear(cOut.getSlice(blc,shape,True), c, 1e-6), AipsError);
}


void checkNumbers (const ImageInterface<Float>& rIn,
                   const ImageInterface<Float>& rOut,
                   const ImageInterface<Float>& iOut,
                   const ImageInterface<Float>& aOut,
                   const ImageInterface<Float>& pOut,
                   const ImageInterface<Complex>& cOut,
                   const Vector<Bool>& axes)
{

// Can only check if all axes transformed

   for (uInt i=0; i<axes.nelements(); i++) {
      if (!axes(i)) return;
   }
      
 
// Fill a complex array with the real input
// image.  Replace masked values by zero
      
      Array<Float> r0 = rIn.get();
      Array<Bool> m0 = rIn.getMask();
      if (rIn.isMasked()) {
         Bool deleteIt1, deleteIt2;
         Float* pP = r0.getStorage(deleteIt1);
         const Bool*  pM = m0.getStorage(deleteIt2);
         for (Int i=0; i<r0.shape().product(); i++) {
            if (!pM[i]) pP[i] = 0.0;
         }
         r0.putStorage(pP, deleteIt1);
         m0.freeStorage(pM, deleteIt2);
      }
//
      Array<Complex> c(r0.shape());
      convertArray(c, r0);

// Make FFT server and FFT

      FFTServer<Float,Complex> fftServer(c.shape(),FFTEnums::COMPLEX);
      fftServer.fft(c, True);

// Check numbers

      AlwaysAssert(allNear(rOut.get(), real(c), 1e-6), AipsError);
      AlwaysAssert(allNear(iOut.get(), imag(c), 1e-6), AipsError);
      AlwaysAssert(allNear(aOut.get(), amplitude(c), 1e-6), AipsError);
      AlwaysAssert(allNear(pOut.get(), phase(c), 1e-6), AipsError);
      AlwaysAssert(allNear(cOut.get(), c, 1e-6), AipsError);
}



void makeMask(ImageInterface<Float>& out)
{
   out.makeMask ("mask0", True, True);
}

void makeMask(ImageInterface<Complex>& out)
{
   out.makeMask ("mask0", True, True);
}
