//# Image2DConvolver.cc:  convolution of an image by given Array
//# Copyright (C) 1995,1996,1997,1998,1999,2000,2001
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
#include <trial/Images/Image2DConvolver.h>
//
#include <aips/aips.h>
#include <aips/Arrays/IPosition.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Exceptions/Error.h>
#include <trial/ComponentModels/GaussianShape.h>
#include <trial/Coordinates/CoordinateUtil.h>
#include <trial/Coordinates/CoordinateSystem.h>
#include <trial/Coordinates/DirectionCoordinate.h>
#include <trial/Fitting/Fit2D.h>
#include <aips/Functionals/Gaussian2D.h>
#include <trial/Images/ArrayImageConvolver.h>
#include <trial/Images/PagedImage.h>
#include <trial/Images/TempImage.h>
#include <trial/Images/ImageInterface.h>
#include <trial/Images/ImageInfo.h>
#include <aips/Logging/LogIO.h>
#include <aips/Mathematics/Convolver.h>
#include <aips/Quanta/Quantum.h>
#include <aips/Quanta/Unit.h>
#include <trial/Tasking/NewFile.h>
#include <aips/Utilities/String.h>

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
ImageInterface<T>* Image2DConvolver<T>::convolve(LogIO& os,  
                                                 const String& outFile,
                                                 ImageInterface<T>& imageIn,
                                                 VectorKernel::KernelTypes kernelType,
                                                 const IPosition& pixelAxes,
                                                 const Vector<Quantum<Double> >& parameters,   
                                                 Bool autoScale, Double scale,
                                                 Bool askRemoveOut)
{

// Make output image.  TempImage or PagedImage

   ImageInterface<Float>* pImOut = 0;
   if (outFile.empty()) {
      pImOut = new TempImage<Float>(imageIn.shape(), imageIn.coordinates());
   } else {

// This should only be used in the Tasking environment

       if (askRemoveOut) {
          NewFile validfile;
          String errmsg;
          if (!validfile.valueOK(outFile, errmsg)) {
              os << errmsg << LogIO::EXCEPTION;
          }
       }
//
      pImOut = new PagedImage<Float>(imageIn.shape(), imageIn.coordinates(), outFile);
   }

// Convolve. Clean up pointer if trouble.

   try {
      convolve(os, *pImOut, imageIn, kernelType, pixelAxes, parameters,
               autoScale, scale, True);
   } catch (AipsError x) {
      delete pImOut; pImOut = 0;
      os << x.getMesg() << LogIO::EXCEPTION;
   }

// Return pointer

   return pImOut;
}


template <class T>
void Image2DConvolver<T>::convolve(LogIO& os,  
                                   ImageInterface<T>& imageOut,
                                   ImageInterface<T>& imageIn,
                                   VectorKernel::KernelTypes kernelType,
                                   const IPosition& pixelAxes,
                                   const Vector<Quantum<Double> >& parameters,   
                                   Bool autoScale, Double scale,
                                   Bool copyMiscellaneous=True)
{

// Check convolution axes

   if (pixelAxes.nelements() != 2) {
      os << "You must give two pixel axes to convolve" << LogIO::EXCEPTION;
   }                        
   const Int nDim = imageIn.ndim();
   if (pixelAxes(0)<0 || pixelAxes(0)>=nDim ||
       pixelAxes(1)<0 || pixelAxes(1)>=nDim) {
      os << "The pixel axes " << pixelAxes << " are illegal" << LogIO::EXCEPTION;
   }

// Check shapes

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
// Convolve

   ArrayImageConvolver<T> aic;
   aic.convolve (os, imageOut, imageIn, kernel, copyMiscellaneous);

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
                                  const IPosition& axes,
                                  const ImageInterface<T>& imageIn) const
{

// Check number of parameters

   checkKernelParameters(os, kernelType, parameters);

// Convert kernel widths to pixels from world.  Demands major and minor
// both in pixels or both in world, else exception

   Vector<Double> dParameters;
   const CoordinateSystem cSys = imageIn.coordinates();
   worldWidthsToPixel (os, dParameters, parameters, cSys, axes);

// Create n-Dim kernel array shape

   IPosition kernelShape = shapeOfKernel (kernelType, dParameters, imageIn.ndim(), axes);

// Create kernel array. We will fill the n-Dim array (shape non-unity
// only for pixelAxes) through its 2D Matrix incarnation. Aren't we clever.
      
   kernelArray.resize(kernelShape);
   Array<T> kernelArray2 = kernelArray.nonDegenerate (axes);
   Matrix<T> kernelMatrix = static_cast<Matrix<T> >(kernelArray2);

// Fill kernel Matrix with functional (height unity)

   return fillKernel (kernelMatrix, kernelType, kernelShape, axes, dParameters);
}


template <class T>
void Image2DConvolver<T>::dealWithRestoringBeam (LogIO& os, 
                                                 String& brightnessUnitOut,
                                                 Vector<Quantum<Double> >& beamOut,
                                                 Array<T>& kernelArray, 
                                                 T kernelVolume,
                                                 VectorKernel::KernelTypes kernelType,
                                                 const Vector<Quantum<Double> >& parameters,
                                                 const IPosition& axes,
                                                 const CoordinateSystem& cSys,
                                                 const ImageInfo& imageInfo,
                                                 const Unit& brightnessUnitIn,
                                                 Bool autoScale, Double scale) const
{

// Find out if convolution axes hold the sky.  Scaling from
// Jy/beam and Jy/pixel only really makes sense if this is True

   Bool holdsOneSkyAxis;
   Bool hasSky = CoordinateUtil::holdsSky (holdsOneSkyAxis, cSys, axes.asVector());
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
   if (hasSky && bUnitIn==String("JY/PIXEL")) {

// Easy case.  Peak of convolution kernel must be unity
// and output units are Jy/beam.  All other cases require
// numerical convolution of beams

      brightnessUnitOut = String("Jy/beam");
      beamOut.resize(3);

// Exception already generated if only one of major and minor in pixel units

      if (parameters(0).getFullUnit().getName()==String("pix")) {
         Vector<Double> pixelParameters(3);
         pixelParameters(0) = parameters(0).getValue();
         pixelParameters(1) = parameters(1).getValue();
         pixelParameters(2) = parameters(2).getValue(Unit("rad"));
         Vector<Quantum<Double> > worldParameters;
//
         skyWidthsPixelToWorld (os, worldParameters, cSys, pixelParameters, axes);
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
         
         Vector<Double> dParameters;
         worldWidthsToPixel (os, dParameters, beamIn, cSys, axes);
         
// Create 2-D beam array shape

         IPosition dummyAxes(2, 0, 1);
         IPosition beamShape = shapeOfKernel (VectorKernel::GAUSSIAN,
                                              dParameters, 2, dummyAxes);

// Create beam Matrix and fill with height unity
   
         Matrix<T> beamMatrixIn(beamShape(0), beamShape(1));
         fillKernel (beamMatrixIn, VectorKernel::GAUSSIAN, beamShape, 
                     dummyAxes, dParameters);

// Get 2-D version of convolution kenrel

         Array<T> kernelArray2 = kernelArray.nonDegenerate (axes);
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
         Fit2D::ErrorTypes error = fitter.fit (beamMatrixOut, sigma, False);
         if (error==Fit2D::NOCONVERGE ||
             error==Fit2D::FAILED ||
             error==Fit2D::NOGOOD) {
            os << "Failed to fit the output beam" << LogIO::EXCEPTION;
         }
         Vector<Double> bSolution = fitter.availableSolution();

// Convert to world units. Ho hum.
                            
         Vector<Double> pixelParameters(3);
         pixelParameters(0) = bSolution(3);
         pixelParameters(1) = bSolution(4);
         pixelParameters(2) = bSolution(5);
//
         skyWidthsPixelToWorld (os, beamOut, cSys, pixelParameters, axes);
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
void Image2DConvolver<T>::worldWidthsToPixel (LogIO& os,
                                              Vector<Double>& dParameters,
                                              const Vector<Quantum<Double> >& parameters,
                                              const CoordinateSystem& cSys,
                                              const IPosition& pixelAxes) const
//
// Convert 2d functional axis widths from world to pixel
//
// If one width is in pixels both must be pixels.
// If axes are not from the same coordinate type units must be pixels.
//
// On input, pa is N->E (at ref pix) for celestial planes.
// Otherwise pa is in pixel coordinate system +x -> +y  
// On output, pa is positive +x -> +y in pixel frame
// I hate position angles
//
// parameters = major, minor, pa
//
{
   dParameters.resize(parameters.nelements());
   Int c0, c1, axisInCoordinate0, axisInCoordinate1;
   cSys.findPixelAxis(c0, axisInCoordinate0, pixelAxes(0));
   cSys.findPixelAxis(c1, axisInCoordinate1, pixelAxes(1));
      
// Find units
   
   String majorUnit = parameters(0).getFullUnit().getName();
   String minorUnit = parameters(1).getFullUnit().getName();
        
// This saves me trying to handle mixed pixel/world units which is a pain for coupled coordinates
    
   if ( (majorUnit==String("pix") && minorUnit!=String("pix"))  ||
        (majorUnit!=String("pix") && minorUnit==String("pix")) ) {
         os << "If pixel units are used, both major and minor axes must have pixel units" << LogIO::EXCEPTION;
   }
         
// Some checks
      
   Coordinate::Type type0 = cSys.type(c0);
   Coordinate::Type type1 = cSys.type(c1);
   if (type0 != type1) {
      if (majorUnit!=String("pix") || minorUnit!=String("pix")) {
         os << "The coordinate types for the convolution axes are different" << endl;
         os << "Therefore the units of the major and minor axes of " << endl;
         os << "the convolution kernel widths must both be pixels" << LogIO::EXCEPTION;
      }
   }
   if (type0==Coordinate::DIRECTION && type1==Coordinate::DIRECTION &&  c0!=c1) {
      os << "The given axes do not come from the same Direction coordinate" << endl;
      os << "This situation requires further code development" << LogIO::EXCEPTION;
   }
   if (type0==Coordinate::STOKES || type1==Coordinate::STOKES) {
         os << "Cannot convolve Stokes axes" << LogIO::EXCEPTION;
   }
      
// Deal with pixel units separately.    Both are in pixels if either is in pixels.

   if (majorUnit==String("pix")) {
      dParameters(0) = parameters(0).getValue();                      // Major
      dParameters(1) = parameters(1).getValue();                      // Minor
// 
      if (type0==Coordinate::DIRECTION && type1==Coordinate::DIRECTION) {
         const DirectionCoordinate& dCoord = cSys.directionCoordinate (c0);
         MDirection world;
         dCoord.toWorld(world, dCoord.referencePixel());               // Can't fail

// Use GaussianShape to get the position angle right

         Quantum<Double> tmpMin(1.0, Unit("arcsec"));
         Quantum<Double> tmpMaj(dParameters(0)/dParameters(1), Unit("arcsec"));
         GaussianShape gaussShape(world, tmpMaj, tmpMin, parameters(2));        // pa is N->E
         Vector<Double> pars = gaussShape.toPixel (dCoord);
         dParameters(2) = pars(4);                                              // pa: +x -> +y
       } else {
      
// Some 'mixed' plane; the pa is already +x -> +y
   
         dParameters(2) = parameters(2).getValue(Unit("rad"));                  // pa
       }
       return;
   }

// Continue on if non-pixel units

   if (type0==Coordinate::DIRECTION && type1==Coordinate::DIRECTION) {
      
// Check units are angular
      
      Unit rad(String("rad"));
      if (!parameters(0).check(rad.getValue())) {
         os << "The units of the major axis must be angular" << LogIO::EXCEPTION;
      }
      if (!parameters(1).check(rad.getValue())) {
         os << "The units of the minor axis must be angular" << LogIO::EXCEPTION;
      }
                                   
// Make a Gaussian shape to convert to pixels at reference pixel
  
      const DirectionCoordinate& dCoord = cSys.directionCoordinate (c0);
      MDirection world;
      dCoord.toWorld(world, dCoord.referencePixel());               // Can't fail
// 
      GaussianShape gaussShape(world, parameters(0), parameters(1), parameters(2));
      Vector<Double> pars = gaussShape.toPixel (dCoord);
      dParameters(0) = pars(2);
      dParameters(1) = pars(3);
      dParameters(2) = pars(4);      // radians; +x -> +y
   } else {

// The only other coordinates currently available are non-coupled
// ones and linear except for Tabular, which can be non-regular.
// Urk.
   
      Vector<Double> refVal = cSys.referenceValue();
      Vector<Double> refPix = cSys.referencePixel();
// 
      Vector<String> units = cSys.worldAxisUnits();
      Int worldAxis0 = cSys.pixelAxisToWorldAxis(pixelAxes(0));
      Int worldAxis1 = cSys.pixelAxisToWorldAxis(pixelAxes(1));
      String unit0 = units(worldAxis0);   
      String unit1 = units(worldAxis1);
      
// I will be able to relax this criterion when I get the time
   
      if (unit0 != unit1) {
         os << "Units of the convolution axes must be the same" << LogIO::EXCEPTION;
      }
    
// Find major and minor axes in pixels

      Unit unit(unit0);
      dParameters(0) = worldWidthToPixel (os, dParameters(2), parameters(0), 
                                          cSys, refVal, refPix, unit, pixelAxes);
      dParameters(1) = worldWidthToPixel (os, dParameters(2), parameters(1), 
                                          cSys, refVal, refPix, unit, pixelAxes);
      dParameters(2) = parameters(2).getValue(Unit("rad"));                // radians; +x -> +y
   }
}   

template <class T>
Double Image2DConvolver<T>::worldWidthToPixel (LogIO& os, Double positionAngle, 
                                               const Quantum<Double>& length,
                                               const CoordinateSystem& cSys,
                                               const Vector<Double>& refVal,
                                               const Vector<Double>& refPix,
                                               const Unit& unit,
                                               const IPosition& pixelAxes) const
//
// convert a world length for a linear (non Direction) coordinate
// to a length in pixels
// 
{
// Check units are ok

   if (!length.check(unit.getValue())) {
      ostrstream oss;
      oss << "The units of the world length (" << length.getFullUnit().getName()
          << ") are not consistent with those of Coordinate System ("
          << unit.getName() << ")" << ends;
      String s(oss);
      os << s << LogIO::EXCEPTION;
   }
// 
   Int worldAxis0 = cSys.pixelAxisToWorldAxis(pixelAxes(0));
   Int worldAxis1 = cSys.pixelAxisToWorldAxis(pixelAxes(1));
//
   Double w0 = cos(positionAngle) * length.getValue(unit) / 2.0;
   Double w1 = sin(positionAngle) * length.getValue(unit) / 2.0;

// Find pixel coordinate of tip of axis  relative to reference pixel

   Vector<Double> world = refVal.copy();
   world(worldAxis0) = world(worldAxis0) + w0;
   world(worldAxis1) = world(worldAxis1) + w1;
// 
   Vector<Double> pixel;
   if (!cSys.toPixel (pixel, world)) {
      os << cSys.errorMessage() << LogIO::EXCEPTION;
   }
//  
   pixel -= refPix;
   Double lengthInPixels = 2.0 * hypot(pixel(pixelAxes(0)), pixel(pixelAxes(1)));
   return lengthInPixels;
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
   Gaussian2D<T> g2d(height, xCentre, yCentre, majorAxis, ratio, positionAngle);
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

template <class T>
Bool Image2DConvolver<T>::skyWidthsPixelToWorld (LogIO& os, 
                                                 Vector<Quantum<Double> >& wParameters,
                                                 const CoordinateSystem& cSys, 
                                                 const Vector<Double>& pParameters,
                                                 const IPosition& pixelAxes) const
//
// Convert 2d functional axis sizes from pixel to world at the reference pixel
// Only called if the pixelAxes axes hold the sky
//
// On input pa is positive for +x -> +y in pixel frame
// On output pa is positive N->E
//
{

// What coordinates are these axes ?

   Int c0, c1, axisInCoordinate0, axisInCoordinate1;
   cSys.findPixelAxis(c0, axisInCoordinate0, pixelAxes(0));
   cSys.findPixelAxis(c1, axisInCoordinate1, pixelAxes(1));   
                                 
// See what sort of coordinates we have. Make sure it is called
// only for the Sky.  More development needed otherwise.
                                 
   Coordinate::Type type0 = cSys.type(c0);
   Coordinate::Type type1 = cSys.type(c1);
   if (type0!=Coordinate::DIRECTION || type1!=Coordinate::DIRECTION) {
      os << "Can only be called for axes holding the sky" << LogIO::EXCEPTION;
   }
   if (c0!=c1) {
      os << "The given axes do not come from the same Direction coordinate" << endl;
      os << "This situation requires further code development" << LogIO::EXCEPTION;
   }
      
// Is the 'x' (first axis) the Longitude or Latitude ?
          
   Vector<Int> dirPixelAxes = cSys.pixelAxes(c0);
   Bool xIsLong = dirPixelAxes(0)==pixelAxes(0) && dirPixelAxes(1)==pixelAxes(1);
   uInt whereIsX = 0;
   uInt whereIsY = 1;
   if (!xIsLong) {
      whereIsX = 1;
      whereIsY = 0;
   }
   
// Encode a pretend GaussianShape from these values as a means     
// of converting to world.         

   const DirectionCoordinate& dCoord = cSys.directionCoordinate(c0);
   GaussianShape gaussShape;
   Vector<Double> cParameters(5);
   cParameters(0) = dCoord.referencePixel()(whereIsX);     // x centre
   cParameters(1) = dCoord.referencePixel()(whereIsY);     // y centre
   cParameters(2) = pParameters(0);
   cParameters(3) = pParameters(1);
   cParameters(4) = pParameters(2);
//  
   Bool flipped = gaussShape.fromPixel (cParameters, dCoord);
   wParameters.resize(3);
   wParameters(0) = gaussShape.majorAxis();
   wParameters(1) = gaussShape.minorAxis();
   wParameters(2) = gaussShape.positionAngle();
//
   return flipped;
}  
   
   


