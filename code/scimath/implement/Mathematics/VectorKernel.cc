//# VectorKernel.cc:  generate moments from an image
//# Copyright (C) 1995,1996,1997,1998,1999,2000
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

#include <trial/Mathematics/VectorKernel.h>

#include <aips/aips.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/ArrayUtil.h>
#include <aips/Exceptions/Error.h>
#include <aips/Functionals/Gaussian1D.h>
#include <aips/Logging.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Mathematics/Math.h>
#include <aips/Utilities/Regex.h>


Vector<Double> VectorKernel::make(KernelTypes kernelType, Double width, 
                                  uInt shape, Bool peakIsUnity)
{
   LogIO os(LogOrigin("VectorKernel", "make(Double)"));
   if (shape <=1) {
      os << "Shape must be > 1" << LogIO::EXCEPTION;
   }
//
   Vector<Double> kernel;
   uInt nPixels = 0;
   if (kernelType == GAUSSIAN) { 

// Gaussian. The volume error is less than 6e-5% for +/- 5 sigma limits
// width is FWHM

      const Double sigma = width / sqrt(Double(8.0) * C::ln2);
      nPixels = (Int(5*sigma + 0.5) + 1) * 2;
      kernel.resize(min(shape,nPixels));
      nPixels = kernel.nelements();
//
      const Double refPix = Double(nPixels)/2;
      Double norm;
      if (peakIsUnity)  {
         norm = 1.0;
      } else {
         norm = 1.0 / (sigma * sqrt(2.0 * C::pi));
      }
      const Gaussian1D<Double> gauss(norm, refPix, Double(width));
      for (uInt j=0; j<nPixels; j++) kernel(j) = gauss(Double(j));
   } else if (kernelType == BOXCAR) {
      Int intWidth = Int(width+0.5);
      nPixels = intWidth;  
      const uInt n = min(shape,nPixels);
      kernel.resize(n);
      Double norm;
      if (peakIsUnity)  {
         norm = 1.0;
      } else {
         norm = Double(intWidth);
      }
//
      for (uInt i=0; i<n; i++) {
         kernel(i) = 1.0 / norm;
      }
   } else if (kernelType == HANNING) {

// shape is at least 2

      nPixels = min(uInt(3),shape);
      kernel.resize(nPixels);
      if (peakIsUnity)  {
         kernel(0) = 0.5;
         kernel(1) = 1.0;
         if (nPixels==3) kernel(2) = 0.5;
      } else {
         kernel(0) = 0.25;
         kernel(1) = 0.5;
         if (nPixels==3) kernel(2) = 0.25;
      }
   }
   return kernel;
}



Vector<Float> VectorKernel::make(KernelTypes kernelType, Float width, 
                                 uInt shape, Bool peakIsUnity)
{
   Vector<Double> tmp = make(kernelType, Double(width), shape, peakIsUnity);
   Vector<Float> kernel(tmp.nelements());
   for (uInt i=0; i<tmp.nelements(); i++) kernel(i) = Float(tmp(i));
   return kernel;
}



Vector<Int> VectorKernel::toKernelTypes (const String& kernels,
                                         const Regex& delimiter)
{
   const Vector<String> kernelStrings = stringToVector(kernels, delimiter);
   return VectorKernel::toKernelTypes(kernelStrings);
}
 

Vector<Int> VectorKernel::toKernelTypes (const Vector<String>& kernels)
{
   const uInt n = kernels.nelements();
   Vector<Int> kernelTypes(n);
   for (uInt i=0; i<n; i++) {
      kernelTypes(i) = VectorKernel::toKernelType(kernels(i));
   }
   return kernelTypes;
}
 


VectorKernel::KernelTypes VectorKernel::toKernelType (const String& kernel)

{
   String kernel2 = upcase(kernel);
   String kernel3(kernel2.at(0,1));
//               
   if (kernel3==String("B")) {
      return VectorKernel::BOXCAR;
   } else if (kernel3==String("G")) {
      return VectorKernel::GAUSSIAN;
   } else if (kernel3==String("H")) {
      return VectorKernel::HANNING;
   }
   LogIO os(LogOrigin("VectorKernel", "toKernelType"));
   os << "Illegal kernel type" << LogIO::EXCEPTION;
   return VectorKernel::BOXCAR;
}

