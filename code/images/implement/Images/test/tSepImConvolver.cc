//# tSepImConvolver.cc: 
//# Copyright (C) 1996,1997,1999,2000
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
#include <trial/Images/SepImageConvolver.h>

#include <aips/aips.h>
#include <aips/Functionals/Gaussian1D.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Exceptions/Error.h>
#include <aips/Inputs/Input.h>
#include <aips/Logging.h>
#include <aips/Mathematics/Constants.h>
#include <trial/Mathematics/VectorKernel.h>
#include <aips/Tasking/Aipsrc.h>
#include <aips/Utilities/String.h>


#include <trial/Images/PagedImage.h>
#include <iostream.h>



main (int argc, char **argv)
{

try {

   Input inputs(1);

// Get inputs

   String root = Aipsrc::aipsRoot();
   String name = root + "/code/trial/implement/Images/test/test_image";
   inputs.create("in", name, "Input file name");
   inputs.create("out", "tSepImageConv_image", "Output root image name");
   inputs.create("axis", "-1", "axis to convolve (default is all)");
   inputs.create("width", "3", "Width in pixels");
   inputs.create("type", "gauss", "Kernel type (box, gauss, hann");
   inputs.create("autoscale", "T", "Normalize so volume is unity");
   inputs.create("show", "F", "Show kernel");
   inputs.readArguments(argc, argv);

   const String in = inputs.getString("in");
   const String out = inputs.getString("out");
   const String kernelType = inputs.getString("type");
   Int axis = inputs.getInt("axis");
   Double fwhm = inputs.getDouble("width");
   Bool autoScale = inputs.getBool("autoscale");
   Bool show = inputs.getBool("show");
//
   if (in.empty()) {
      cout << "You must give an input image name" << endl;
      exit(1);
   }
   if (out.empty()) {
      cout << "You must give an output image name" << endl;
      exit(1);
   }
   if (fwhm <= 0.0) {
      cout << "FWHM must be positive" << endl;
      exit(1);
   }
 

   DataType imageType = imagePixelType(in);
   if (imageType!=TpFloat) {
      cout << "The image must be of type Float" << endl;
      exit(1);
   }

// Construct images

   PagedImage<Float> inImage(in);
   if (axis > Int(inImage.ndim())) {
      cout << "Axis must be <=" << inImage.ndim() << endl;
      exit(1);
   }
   PagedImage<Float> outImage1(TiledShape(inImage.shape()),
                              inImage.coordinates(), out+String("_a"));

   PagedImage<Float> outImage2(TiledShape(inImage.shape()),
                              inImage.coordinates(), out+String("_b"));

// Make a Gaussian kernel parameters

   IPosition shape = inImage.shape();
   const Double sigma = fwhm / sqrt(Double(8.0) * C::ln2);
   Double norm = 1.0 / (sigma * sqrt(2.0 * C::pi));
   if (!autoScale) norm = 1.0;
   LogOrigin or("tSeparableImageConvolver", "main()", WHERE);
   LogIO os(or);
   VectorKernel::KernelTypes type = VectorKernel::toKernelType(kernelType);


// Give kernel with vectors directly

   {

// Make convolver

      SepImageConvolver<Float> sic(inImage, os, True);
//
// Set the state of the convolver

      if (axis<=0) {
         for (uInt j=0; j<shape.nelements(); j++) {
            const Double refPix = shape(j)/2;
            const Gaussian1D<Double> gauss(norm, refPix, fwhm);
            Vector<Float> kernel(shape(j));
            for (Int i=0; i<shape(j); i++) kernel(i) = gauss(Double(i));
            sic.setKernel(j, kernel);
         }
      } else {
         axis--;
         const Double refPix = shape(axis)/2;
         const Gaussian1D<Double> gauss(norm, refPix, fwhm);
         Vector<Float> kernel(shape(axis));
         for (Int i=0; i<shape(axis); i++) kernel(i) = gauss(Double(i));
         sic.setKernel(axis, kernel);
      }
      sic.convolve(outImage1);
   }
//
// Give kernel parameters
//

   {

// Make convolver

      SepImageConvolver<Float> sic(inImage, os, True);
//
// Set the state of the convolver

      if (axis<=0) {
         for (uInt j=0; j<shape.nelements(); j++) {
            sic.setKernel(j, type, fwhm, autoScale);
         }
      } else {
         axis--;
         sic.setKernel(axis, type, fwhm, autoScale);
      }
      sic.convolve(outImage2);
   }

// Test some other things

   {
      SepImageConvolver<Float> sic(inImage, os, True);

// Copy constructor
      cout << "Copy constructor" << endl;
      SepImageConvolver<Float> sic2(sic);

// Assignment
      cout << "Assignment" << endl;
      sic2 = sic;

// Set/Get a kernel
      
      sic.setKernel(0, type, fwhm, autoScale);
      Vector<Float> kernel = sic.getKernel(uInt(0));
      if (show) {
         cout << "Kernel = " << kernel << endl;
         cout << "Sum of kernel = " << sum(kernel) << endl;
      }
   }
}

  catch (AipsError x) {
     cerr << "aipserror: error " << x.getMesg() << endl;
     return 1;
  } 

   exit(0);
}

