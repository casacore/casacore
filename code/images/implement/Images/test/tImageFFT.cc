//# tImageFFT.cc: test ImageFFT class
//# Copyright (C) 1996,1997,1998,1999
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
#include <aips/Tasking/Aipsrc.h>
#include <aips/Exceptions/Error.h>
#include <aips/Inputs/Input.h>
#include <aips/Logging.h>
#include <aips/Utilities/String.h>
  
#include <trial/Images/ImageFFT.h>
#include <trial/Images/PagedImage.h>

#include <iostream.h>



main (int argc, char **argv)
{
try {

   Input inputs(1);
   inputs.Version ("$Revision$");


// Get inputs

   String root = Aipsrc::aipsRoot();
   String name = root + "/code/trial/implement/Images/test/test_image";
   inputs.Create("in", name, "Input file name");
   inputs.ReadArguments(argc, argv);

   const String in = inputs.GetString("in");
   LogOrigin or("tImageFFT", "main()", WHERE);
   LogIO os(or);
 

// Check image name and get image data type. 

   if (in.empty()) {
      os << LogIO::NORMAL << "You must specify the image file name" << LogIO::POST;
      return 1;
   }

// Testing does not really test the functionality, as I would
// have to write the Fourier Transform by hand !

   DataType imageType = imagePixelType(in);
   if (imageType==TpFloat) {
      PagedImage<Float> inImage(in, True);
      IPosition outShape(inImage.shape());
      PagedImage<Float> outReal(outShape, inImage.coordinates(), "tImageFFT_real.img");
      PagedImage<Float> outImag(outShape, inImage.coordinates(), "tImageFFT_imag.img");
      PagedImage<Float> outAmp(outShape, inImage.coordinates(), "tImageFFT_amp.img");
      PagedImage<Float> outPhase(outShape, inImage.coordinates(), "tImageFFT_phase.img");
      PagedImage<Complex> outComplex(outShape, inImage.coordinates(), "tImageFFT_complex.img");
//
      ImageFFT fft;
      fft.fftsky(inImage);
//
      fft.getReal(outReal);
      fft.getImaginary(outImag);
      fft.getAmplitude(outAmp);
      fft.getPhase(outPhase);
      fft.getComplex(outComplex);
      const ImageInterface<Complex>& outComplex2 = fft.getComplex();
//
      Array<Float> rArray1 = outReal.get();
      Array<Float> iArray1 = outImag.get();
      Array<Float> aArray1 = outAmp.get();
      Array<Float> pArray1 = outPhase.get();
      Array<Complex> cArray1 = outComplex.get();
      Array<Complex> c2Array1 = outComplex2.get();
      AlwaysAssert(allEQ(cArray1,c2Array1), AipsError);

// Copy constructor

      ImageFFT fft2(fft);
      fft2.getReal(outReal);
      fft2.getImaginary(outImag);
      fft2.getAmplitude(outAmp);
      fft2.getPhase(outPhase);
      fft2.getComplex(outComplex);
      const ImageInterface<Complex>& outComplex3 = fft2.getComplex();
//
      Array<Float> rArray2 = outReal.get();
      Array<Float> iArray2 = outImag.get();
      Array<Float> aArray2 = outAmp.get();
      Array<Float> pArray2 = outPhase.get();
      Array<Complex> cArray2 = outComplex.get();
      Array<Complex> c2Array2 = outComplex3.get();
      AlwaysAssert(allEQ(cArray2,c2Array2), AipsError);
//
      AlwaysAssert(allEQ(rArray1,rArray2), AipsError);
      AlwaysAssert(allEQ(iArray1,iArray2), AipsError);
      AlwaysAssert(allEQ(aArray1,aArray2), AipsError);
      AlwaysAssert(allEQ(pArray1,pArray2), AipsError);
      AlwaysAssert(allEQ(cArray1,cArray2), AipsError);

// Assignment operator

      ImageFFT fft3;
      fft3 = fft2;
      fft3.getReal(outReal);
      fft3.getImaginary(outImag);
      fft3.getAmplitude(outAmp);
      fft3.getPhase(outPhase);
      fft3.getComplex(outComplex);
      const ImageInterface<Complex>& outComplex4 = fft3.getComplex();
//
      Array<Float> rArray3 = outReal.get();
      Array<Float> iArray3 = outImag.get();
      Array<Float> aArray3 = outAmp.get();
      Array<Float> pArray3 = outPhase.get();
      Array<Complex> cArray3 = outComplex.get();
      Array<Complex> c2Array3 = outComplex4.get();
      AlwaysAssert(allEQ(cArray3,c2Array3), AipsError);
//
      AlwaysAssert(allEQ(rArray1,rArray3), AipsError);
      AlwaysAssert(allEQ(iArray1,iArray3), AipsError);
      AlwaysAssert(allEQ(aArray1,aArray3), AipsError);
      AlwaysAssert(allEQ(pArray1,pArray3), AipsError);
      AlwaysAssert(allEQ(cArray1,cArray3), AipsError);
   } else {
      os << LogIO::NORMAL << "images of type " << imageType << " not yet supported" << LogIO::POST;
      exit(1);
   }
}

  catch (AipsError x) {
     cerr << "aipserror: error " << x.getMesg() << endl;
     exit(1);
  } end_try;

  exit(0);
}

