//# Image2DConvolver.cc:  convolution of an image by given Array
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
#include <images/Images/Image2DConvolver.h>
//
#include <casa/aips.h>
#include <casa/Arrays/IPosition.h>
#include <casa/Arrays/Array.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/Vector.h>
#include <casa/Arrays/Matrix.h>
#include <casa/Exceptions/Error.h>
#include <components/ComponentModels/GaussianShape.h>
#include <coordinates/Coordinates/CoordinateUtil.h>
#include <coordinates/Coordinates/CoordinateSystem.h>
#include <coordinates/Coordinates/DirectionCoordinate.h>
#include <lattices/LatticeMath/Fit2D.h>
#include <scimath/Functionals/Gaussian2D.h>
#include <images/Images/ImageConvolver.h>
#include <images/Images/PagedImage.h>
#include <images/Images/TempImage.h>
#include <images/Images/ImageInterface.h>
#include <images/Images/ImageInfo.h>
#include <images/Images/ImageUtilities.h>
#include <casa/Logging/LogIO.h>
#include <scimath/Mathematics/Convolver.h>
#include <casa/Quanta/Quantum.h>
#include <casa/Quanta/MVAngle.h>
#include <casa/Quanta/Unit.h>
#include <casa/BasicSL/String.h>
#include <casa/iostream.h>

namespace casa { //# NAMESPACE CASA - BEGIN

template <class T> 
Image2DConvolver<T>::Image2DConvolver ()
{}

template <class T>
Image2DConvolver<T>::Image2DConvolver(const Image2DConvolver<T> &other)
{
   operator=(other);
}

template <class T> 
Image2DConvolver<T>::~Image2DConvolver ()
{}


template <class T>
Image2DConvolver<T> &Image2DConvolver<T>::operator=(const Image2DConvolver<T> &other)
{
   if (this != &other) {
   }
   return *this;
}


template <class T>
void Image2DConvolver<T>::convolve(LogIO& os,  
                                   ImageInterface<T>& imageOut,
                                   ImageInterface<T>& imageIn,
                                   VectorKernel::KernelTypes kernelType,
                                   const IPosition& pixelAxes,
                                   const Vector<Quantum<Double> >& parameters,   
                                   Bool autoScale, Double scale,
                                   Bool copyMiscellaneous)
{
// Checks

   if (parameters.nelements() != 3) {
      os << "The world parameters vector must be of length 3" << LogIO::EXCEPTION;
   }                        
   if (pixelAxes.nelements() != 2) {
      os << "You must give two pixel axes to convolve" << LogIO::EXCEPTION;
   }                        
   const Int nDim = imageIn.ndim();
   if (pixelAxes(0)<0 || pixelAxes(0)>=nDim ||
       pixelAxes(1)<0 || pixelAxes(1)>=nDim) {
      os << "The pixel axes " << pixelAxes << " are illegal" << LogIO::EXCEPTION;
   }
//
   if (nDim < 2) {
      os << "The image axes must have at least 2 pixel axes" << LogIO::EXCEPTION;
   }
//
   const IPosition& inShape = imageIn.shape();
   const IPosition& outShape = imageOut.shape();
   if (!inShape.isEqual(outShape)) {
      os << "Input and output images must have the same shape" << LogIO::EXCEPTION;
   }

// Generate Kernel Array (height unity)

   Array<T> kernel;
   T kernelVolume = makeKernel(os, kernel, kernelType, parameters, pixelAxes, imageIn);

// Figure out output image restoring beam (if any), output units and scale
// factor for convolution kernel array

   Vector<Quantum<Double> > beamOut;
   const CoordinateSystem& cSys = imageIn.coordinates();
   const ImageInfo& imageInfo = imageIn.imageInfo();
   const Unit& brightnessUnit = imageIn.units();
   String brightnessUnitOut;
//
  dealWithRestoringBeam (os, brightnessUnitOut, beamOut, kernel, kernelVolume, 
                          kernelType, parameters,
                          pixelAxes, cSys, imageInfo, brightnessUnit,
                          autoScale, scale);

// Convolve.  We have already scaled the convolution kernel (with some 
// trickery cleverer than what ImageConvolver can do) so no more scaling

   Double scale2 = 1.0;
   ImageConvolver<T> aic;
   aic.convolve (os, imageOut, imageIn, kernel, ImageConvolver<T>::NONE, 
                 scale2, copyMiscellaneous);

// Overwrite some bits and pieces in the output image to do with the
// restoring beam  and image units

   imageOut.setUnits(brightnessUnitOut);
   ImageInfo iiOut = imageOut.imageInfo();
//
   Bool holdsOneSkyAxis;
   Bool hasSky = CoordinateUtil::holdsSky (holdsOneSkyAxis, cSys, pixelAxes.asVector());
   if (hasSky && beamOut.nelements()==3) {
      iiOut.setRestoringBeam(beamOut);
      imageOut.setImageInfo(iiOut);
   } else {
    
// If one of the axes is in the sky plane, we must
// delete the restoring beam as it is no longer meaningful

      if (holdsOneSkyAxis) {
         os << LogIO::WARN << "Because you convolved just one of the sky axes" << endl;
         os << "The output image does not have a valid spatial restoring beam" << LogIO::POST; 
         iiOut.removeRestoringBeam();
         imageOut.setImageInfo(iiOut);
      }
   }
}


// Private functions

template <class T>
T Image2DConvolver<T>::makeKernel(LogIO& os, Array<T>& kernelArray, 
                                  VectorKernel::KernelTypes kernelType,
                                  const Vector<Quantum<Double> >& parameters,
                                  const IPosition& pixelAxes,
                                  const ImageInterface<T>& imageIn) const
{

// Check number of parameters

   checkKernelParameters(os, kernelType, parameters);

// Convert kernel widths to pixels from world.  Demands major and minor
// both in pixels or both in world, else exception

   Vector<Double> dParameters;
   const CoordinateSystem cSys = imageIn.coordinates();

// Use the reference value for the shape conversion direction

   Vector<Quantum<Double> > wParameters(5);
   for (uInt i=0; i<3; i++) {
      wParameters(i+2) = parameters(i);
   }
//
   const Vector<Double> refVal = cSys.referenceValue();
   const Vector<String> units = cSys.worldAxisUnits();
   Int wAxis = cSys.pixelAxisToWorldAxis(pixelAxes(0));
   wParameters(0) = Quantum<Double>(refVal(wAxis), units(wAxis));
   wAxis = cSys.pixelAxisToWorldAxis(pixelAxes(1));
   wParameters(1) = Quantum<Double>(refVal(wAxis), units(wAxis));
   ImageUtilities::worldWidthsToPixel (os, dParameters, wParameters, cSys, pixelAxes, False);

// Create n-Dim kernel array shape

   IPosition kernelShape = shapeOfKernel (kernelType, dParameters, imageIn.ndim(), pixelAxes);

// Create kernel array. We will fill the n-Dim array (shape non-unity
// only for pixelAxes) through its 2D Matrix incarnation. Aren't we clever.
      
   kernelArray.resize(kernelShape);
   Array<T> kernelArray2 = kernelArray.nonDegenerate (pixelAxes);
   Matrix<T> kernelMatrix = static_cast<Matrix<T> >(kernelArray2);

// Fill kernel Matrix with functional (height unity)

   return fillKernel (kernelMatrix, kernelType, kernelShape, pixelAxes, dParameters);
}


template <class T>
void Image2DConvolver<T>::dealWithRestoringBeam (LogIO& os, 
                                                 String& brightnessUnitOut,
                                                 Vector<Quantum<Double> >& beamOut,
                                                 Array<T>& kernelArray, 
                                                 T kernelVolume,
                                                 VectorKernel::KernelTypes,
                                                 const Vector<Quantum<Double> >& parameters,
                                                 const IPosition& pixelAxes,
                                                 const CoordinateSystem& cSys,
                                                 const ImageInfo& imageInfo,
                                                 const Unit& brightnessUnitIn,
                                                 Bool autoScale, Double scale) const
{
// Find out if convolution axes hold the sky.  Scaling from
// Jy/beam and Jy/pixel only really makes sense if this is True

   Bool holdsOneSkyAxis;
   Bool hasSky = CoordinateUtil::holdsSky (holdsOneSkyAxis, cSys, pixelAxes.asVector());
   if (hasSky) {
      os << "You are convolving the sky" << LogIO::POST;
   } else {
      os << "You are not convolving the sky" << LogIO::POST;
   }

// Generate an array holding the restoring beam if needed
         
   Vector<Quantum<Double> > beamIn = imageInfo.restoringBeam();
   beamOut.resize(0);
                             
// Get brightness units
         
   String bUnitIn = upcase(brightnessUnitIn.getName());
//
   const Vector<Double>& refPix = cSys.referencePixel();
   if (hasSky && bUnitIn==String("JY/PIXEL")) {

// Easy case.  Peak of convolution kernel must be unity
// and output units are Jy/beam.  All other cases require
// numerical convolution of beams

      brightnessUnitOut = String("Jy/beam");
      beamOut.resize(3);

// Exception already generated if only one of major and minor in pixel units

      if (parameters(0).getFullUnit().getName()==String("pix")) {
         Vector<Double> pixelParameters(5);
         pixelParameters(0) = refPix(pixelAxes(0));
         pixelParameters(1) = refPix(pixelAxes(1));
         pixelParameters(2) = parameters(0).getValue();
         pixelParameters(3) = parameters(1).getValue();
         pixelParameters(4) = parameters(2).getValue(Unit("rad"));
         Vector<Quantum<Double> > worldParameters;
//
         ImageUtilities::pixelWidthsToWorld (os, worldParameters, pixelParameters, 
                                             cSys, pixelAxes, False);
//
         beamOut(0) = worldParameters(0);
         beamOut(1) = worldParameters(1);
      } else {
         beamOut(0) = parameters(0);
         beamOut(1) = parameters(1);
      }
      beamOut(2) = parameters(2);                // Input p.a. is positive N->E
//
      if (!autoScale) {
         T t = static_cast<T>(scale);
         kernelArray *= t;
         os << LogIO::WARN << "Autoscaling is recommended for Jy/pixel convolution" << LogIO::POST;
      }
   } else {
         
// Is there an input restoring beam and are we convolving the sky to which it
// pertains ?  If not, all we can do is use user scaling or normalize the convolution
// kernel to unit volume.  There is no point to convolving the input beam either as it pertains
// only to the sky
          
      if (hasSky && bUnitIn==String("JY/BEAM") && beamIn.nelements()==3) {

// Convert restoring beam parameters to pixels.  Output pa is pos +x -> +y in pixel frame.

         Vector<Quantum<Double> > wParameters(5);
         const Vector<Double> refVal = cSys.referenceValue();
         const Vector<String> units = cSys.worldAxisUnits();
         Int wAxis = cSys.pixelAxisToWorldAxis(pixelAxes(0));
         wParameters(0) = Quantum<Double>(refVal(wAxis), units(wAxis));
         wAxis = cSys.pixelAxisToWorldAxis(pixelAxes(1));
         wParameters(1) = Quantum<Double>(refVal(wAxis), units(wAxis));
         for (uInt i=0; i<3; i++) {
            wParameters(i+2) = beamIn(i);
         }
         Vector<Double> dParameters;
         ImageUtilities::worldWidthsToPixel (os, dParameters, wParameters, cSys, pixelAxes, False);
         
// Create 2-D beam array shape

         IPosition dummyAxes(2, 0, 1);
         IPosition beamShape = shapeOfKernel (VectorKernel::GAUSSIAN,
                                              dParameters, 2, dummyAxes);

// Create beam Matrix and fill with height unity
   
         Matrix<T> beamMatrixIn(beamShape(0), beamShape(1));
         fillKernel (beamMatrixIn, VectorKernel::GAUSSIAN, beamShape, 
                     dummyAxes, dParameters);

// Get 2-D version of convolution kenrel

         Array<T> kernelArray2 = kernelArray.nonDegenerate (pixelAxes);
         Matrix<T> kernelMatrix = static_cast<Matrix<T> >(kernelArray2);
         
// Convolve input restoring beam array by convolution kernel array

         Matrix<T> beamMatrixOut;
         Convolver<T> conv(beamMatrixIn, kernelMatrix.shape());   // matrixIn     = input restoring beam
         conv.linearConv(beamMatrixOut, kernelMatrix);                
            
// Scale kernel

         T maxValOut = max(beamMatrixOut);
         if (autoScale) {
            brightnessUnitOut = String("Jy/beam");
            kernelArray /= maxValOut;
         } else {
            T t = static_cast<T>(scale);
            kernelArray *= t;
         }

// Fit output beam matrix with a Gaussian, for better or worse
// Fit2D is not templated.  So all our templating is useless
// other than for Float until I template Fit2D

         Fit2D fitter(os);
         const uInt n = beamMatrixOut.shape()(0);
//
         Vector<Double> bParameters = fitter.estimate(Fit2D::GAUSSIAN, beamMatrixOut);
         Vector<Bool> bParameterMask(bParameters.nelements(), True);
         bParameters(1) = (n-1)/2;          // x centre
         bParameters(2) = bParameters(1);    // y centre
/*
         bParameterMask(1) = False;         // dont allow centre to float in fit
         bParameterMask(2) = False;
*/

// Set range so we don't include too many pixels in fit which will make it very slow

         fitter.addModel (Fit2D::GAUSSIAN, bParameters, bParameterMask);
         Array<Float> sigma;
         fitter.setIncludeRange(maxValOut/10.0, maxValOut+0.1);
         Fit2D::ErrorTypes error = fitter.fit (beamMatrixOut, sigma);
         if (error==Fit2D::NOCONVERGE ||
             error==Fit2D::FAILED ||
             error==Fit2D::NOGOOD) {
            os << "Failed to fit the output beam" << LogIO::EXCEPTION;
         }
         Vector<Double> bSolution = fitter.availableSolution();

// Convert to world units. Ho hum.
                            
         Vector<Double> pixelParameters(5);
         pixelParameters(0) = refPix(pixelAxes(0));
         pixelParameters(1) = refPix(pixelAxes(1));
         pixelParameters(2) = bSolution(3);
         pixelParameters(3) = bSolution(4);
         pixelParameters(4) = bSolution(5);
//
         ImageUtilities::pixelWidthsToWorld (os, beamOut, pixelParameters, cSys, pixelAxes, False);
      } else {
         if (autoScale) {
    
// Conserve flux is the best we can do
            
            kernelArray /= kernelVolume;
         } else {
            T t = static_cast<T>(scale);
            kernelArray *= t;
         }
      }
   }

// Put beam position angle into range +/- 180 in case it has eluded us so far

    if (beamOut.nelements()==3) {
       MVAngle pa(beamOut(2).getValue(Unit("rad")));
       pa();
       beamOut(2) = Quantum<Double>(pa.degree(), Unit("deg"));
    }
}


template <class T>
void Image2DConvolver<T>::checkKernelParameters(LogIO& os, 
                                                VectorKernel::KernelTypes kernelType,
                                                const Vector<Quantum<Double> >& parameters) const
{
   if (kernelType==VectorKernel::BOXCAR) {
      os << "Boxcar kernel not yet implemented" << LogIO::EXCEPTION;
//
      if (parameters.nelements() != 3) {
         os << "Boxcar kernels require 3 parameters" << LogIO::EXCEPTION;
      }
   } else if (kernelType==VectorKernel::GAUSSIAN) {
      if (parameters.nelements() != 3) {
         os << "Gaussian kernels require 3 parameters" << LogIO::EXCEPTION;
      }
   } else {
      os << "The kernel type " << VectorKernel::fromKernelType(kernelType) << " is not allowed" << LogIO::EXCEPTION;
   }
}


template <class T>
IPosition Image2DConvolver<T>::shapeOfKernel (VectorKernel::KernelTypes kernelType,
                                              const Vector<Double>& parameters,
                                              uInt ndim,
                                              const IPosition& axes) const
//
// Work out how big the array holding the kernel should be.
// Simplest algorithm possible. Shape is presently square.
//
{

// Find 2D shape

   uInt n;
   if (kernelType==VectorKernel::GAUSSIAN) {
      uInt n1 = sizeOfGaussian (parameters(0), 5.0);
      uInt n2 = sizeOfGaussian (parameters(1), 5.0);
      n = max(n1,n2);
      if (n%2==0) n++;                                     // Make shape odd so centres well
   } else if (kernelType==VectorKernel::BOXCAR) {
      n = 2 * Int(max(parameters(0), parameters(1))+0.5);
      if (n%2==0) n++;                                     // Make shape odd so centres well
   } else {
     throw(AipsError("Unrecognized kernel type"));        // Earlier checking should prevent this
   }

// Now find the shape for the image and slot the 2D shape in
// in the correct axis locations

   IPosition shape(ndim,1);
   shape(axes(0)) = n; 
   shape(axes(1)) = n;
//
   return shape;
}
   
template <class T>
uInt Image2DConvolver<T>::sizeOfGaussian (Double width, Double nSigma) const
{
// +/- 5sigma is a volume error of less than 6e-5%

   Double sigma = width / sqrt(Double(8.0) * C::ln2);
   return  (Int(nSigma*sigma + 0.5) + 1) * 2;
}


template <class T>
T Image2DConvolver<T>::fillKernel (Matrix<T>& kernelMatrix, 
                                   VectorKernel::KernelTypes kernelType,
                                   const IPosition& kernelShape,
                                   const IPosition& axes,
                                   const Vector<Double>& parameters) const
{

// Centre functional in array (shape is odd)
// Need to think about these T castes for Complex images

   T xCentre = static_cast<T>((kernelShape(axes(0)) - 1) / 2.0);
   T yCentre = static_cast<T>((kernelShape(axes(1)) - 1) / 2.0);
   T height = static_cast<T>(1.0);

// Create functional.  We only have gaussian2d functionals
// at this point.  Later the filling code can be moved out
// of the if statement

   T maxValKernel, volumeKernel;  
   T pa = static_cast<T>(parameters(2));
   T ratio = static_cast<T>(parameters(1) / parameters(0));
   T major = static_cast<T>(parameters(0));
   if (kernelType==VectorKernel::GAUSSIAN) {
       fillGaussian (maxValKernel, volumeKernel, kernelMatrix, height,
                     xCentre, yCentre, major, ratio, pa);
   } else if (kernelType==VectorKernel::BOXCAR) {
/*
      fillBoxcar (maxValKernel, volumeKernel, kernelMatrix, height,
                  xCentre, yCentre, major, ratio, pa);
*/
   } else {
     throw(AipsError("Unrecognized kernel type"));        // Earlier checking should prevent this
   }
//
   return volumeKernel;
}         

template <class T>
void Image2DConvolver<T>::fillGaussian (T& maxVal, T& volume,
                                        Matrix<T>& pixels, T height, T xCentre,
                                        T yCentre, T majorAxis, T ratio,
                                        T positionAngle) const
// 
// pa positive in +x ->+y pixel coordinate frame
//
{
   uInt n1 = pixels.shape()(0);
   uInt n2 = pixels.shape()(1);
   AlwaysAssert(n1==n2,AipsError);
   positionAngle += C::pi_2;        // +y -> -x
   Gaussian2D<T> g2d(height, xCentre, yCentre, majorAxis,
		       ratio, positionAngle);
   maxVal = -1.0e30;
   volume = 0.0;
   Vector<T> pos(2);
   for (uInt j=0; j<n1; j++) {
      pos(1) = static_cast<T>(j);
      for (uInt i=0; i<n1; i++) {
         pos(0) = static_cast<T>(i);
         T val = g2d(pos);
         pixels(i,j) = val;
//
         maxVal = max(val, maxVal);
         volume += val;
      }
   } 
}


} //# NAMESPACE CASA - END

