//# tSepImConvolver.cc: 
//# Copyright (C) 1996,1997,1999,2000,2001,2002,2003,2004
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
#include <images/Images/SepImageConvolver.h>

#include <casa/aips.h>
#include <scimath/Functionals/Gaussian1D.h>
#include <casa/Arrays/Vector.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/ArrayIO.h>
#include <casa/Exceptions/Error.h>
#include <casa/Inputs/Input.h>
#include <casa/Logging.h>
#include <casa/BasicSL/Constants.h>
#include <scimath/Mathematics/VectorKernel.h>
#include <tables/Tables/Table.h>
#include <casa/BasicSL/String.h>


#include <images/Images/PagedImage.h>
#include <casa/iostream.h>



#include <casa/namespace.h>
int main (int argc, const char* argv[])
{

try {

   Input inputs(1);

// Get inputs

   String name = "test_image.im";
   inputs.create("in", name, "Input file name");
   inputs.create("out", "tSepImConvolver_tmp", "Output root image name");
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
   Bool useShapeExactly = False;

//
   if (in.empty()) {
      cout << "You must give an input image name" << endl;
      return 1;
   }
   if (out.empty()) {
      cout << "You must give an output image name" << endl;
      return 1;
   }
   if (fwhm <= 0.0) {
      cout << "FWHM must be positive" << endl;
      return 1;
   }
 

   DataType imageType = imagePixelType(in);
   if (imageType!=TpFloat) {
      cout << "The image must be of type Float" << endl;
      return 1;
   }

// Construct images

   PagedImage<Float> inImage(in);
   if (axis > Int(inImage.ndim())) {
      cout << "Axis must be <=" << inImage.ndim() << endl;
      return 1;
   }
//
   String out1(out+String("_a"));
   String out2(out+String("_b"));
   {
     PagedImage<Float> outImage1(TiledShape(inImage.shape()),
                                inImage.coordinates(), out1);

     PagedImage<Float> outImage2(TiledShape(inImage.shape()),
                                inImage.coordinates(), out2);

// Make a Gaussian kernel parameters

     IPosition shape = inImage.shape();
     const Double sigma = fwhm / sqrt(Double(8.0) * C::ln2);
     Double norm = 1.0 / (sigma * sqrt(2.0 * C::pi));
     if (!autoScale) norm = 1.0;
     LogOrigin lor("tSeparableImageConvolver", "main()", WHERE);
     LogIO os(lor);
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
              sic.setKernel(j, type, fwhm, autoScale, useShapeExactly);
           }
        } else {
           axis--;
           sic.setKernel(axis, type, fwhm, autoScale, useShapeExactly);
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
      
        sic.setKernel(0, type, fwhm, autoScale, useShapeExactly);
        Vector<Float> kernel = sic.getKernel(uInt(0));
        if (show) {
           cout << "Kernel = " << kernel << endl;
           cout << "Sum of kernel = " << sum(kernel) << endl;
        }
     }
  }

// Clean up

  Table::deleteTable(out1, True);
  Table::deleteTable(out2, True);
}

  catch (AipsError x) {
     cerr << "aipserror: error " << x.getMesg() << endl;
     return 1;
  } 

   return 0;
}

