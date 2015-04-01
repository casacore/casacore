//# VectorKernel.cc:  generate moments from an image
//# Copyright (C) 1995,1996,1997,1998,1999,2000,2001,2002
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

#include <casacore/scimath/Mathematics/VectorKernel.h>

#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/scimath/Functionals/Gaussian1D.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Utilities/Regex.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

Vector<Double> VectorKernel::make(KernelTypes kernelType, Double width, 
                                  uInt shape, Bool useShapeExactly, Bool peakIsUnity)
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
      if (useShapeExactly) {
         nPixels = shape;
      } else {
         nPixels = max(shape,(uInt(5*sigma + 0.5) + 1) * 2);
      }
      kernel.resize(nPixels);
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
      uInt iWidth = uInt(width+0.5);
      if (useShapeExactly) {
         nPixels =  shape;
      } else {
         nPixels = max(shape,iWidth+1);
      }
      kernel.resize(nPixels);

// Try and center kernel

      uInt startPix = max(0u,(nPixels - iWidth)/2);
      uInt endPix = min(nPixels,startPix+iWidth-1);
//
      Double norm;
      if (peakIsUnity)  {
         norm = 1.0;
      } else {
         norm = Double(iWidth);
      }
//
      kernel = 0.0;
      for (uInt i=startPix; i<=endPix; i++) {
         kernel(i) = 1.0 / norm;
      }
   } else if (kernelType == HANNING) {

// kernel always shape 3

      /*nPixels = 3;
      kernel.resize(nPixels);
      if (peakIsUnity)  {
         kernel(0) = 0.5;
         kernel(1) = 1.0;
         kernel(2) = 0.5;
      } else {
         kernel(0) = 0.25;
         kernel(1) = 0.5;
         kernel(2) = 0.25;
      }*/
	   nPixels = shape;
	   kernel.resize( nPixels );
	   int nextIndex = shape + 1;
	   Double normalizer = 1.0 / ( nextIndex );
	   if ( peakIsUnity ){
		   normalizer = 0.5;
	   }

	   double piValue = 4 * atan( 1 );
	   int middle = (shape-1)/2;
	   int endIndex = nextIndex / 2;
	   for ( int i = 0; i < endIndex; i++ ){
		   Double xValue = endIndex - i;
		   Double angleValue = ( 2 * piValue * xValue) / nextIndex;
		   double value = 1-cos( angleValue);
		   value = value * normalizer;
		   kernel[middle - i] = value;
		   kernel[middle + i] = value;
	   }
   }
//
   return kernel;
}



Vector<Float> VectorKernel::make(KernelTypes kernelType, Float width, 
                                 uInt shape, Bool useShapeExactly, Bool peakIsUnity)
{
   Double tw = width;
   Vector<Double> tmp = make(kernelType, tw, shape, useShapeExactly, peakIsUnity);
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

   if (kernel3==String("B")) {
      return VectorKernel::BOXCAR;
   } else if (kernel3==String("G")) {
      return VectorKernel::GAUSSIAN;
   } else if (kernel3==String("H")) {
      return VectorKernel::HANNING;
   } else {
     ThrowIf (True, "Illegal kernel type " + kernel);
   }
   return VectorKernel::BOXCAR;    //# to satisfy compiler
}

String VectorKernel::fromKernelType (KernelTypes kernelType)
{
   String type;
   if (kernelType==BOXCAR) {
      type = String("BOXCAR");
   } else if (kernelType==GAUSSIAN) {
      type = String("GAUSSIAN");
   } else if (kernelType==HANNING) {
      type = String("HANNING");
   }
   return type;
}



} //# NAMESPACE CASACORE - END

