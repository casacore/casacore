//# Image2DConvolver.h: 2D convolution of an image
//# Copyright (C) 1996,1997,1998,1999,2000,2001
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

#if !defined(AIPS_Image2DConvolver_H)
#define AIPS_Image2DConvolver_H


//# Includes
#include <aips/aips.h>
#include <aips/Logging/LogIO.h>
#include <aips/Arrays/Array.h>
#include <trial/Mathematics/VectorKernel.h>

//# Forward Declarations
template <class T> class ImageInterface;
template <class T> class Matrix;
template <class T> class Quantum;
template <class T> class Vector;
class String;
class IPosition;
class CoordinateSystem;
class ImageInfo;
class Unit;


// <summary>
// This class does 2D convolution of an image by a functional form
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="ImageInterface">ImageInterface</linkto>
//   <li> <linkto class="Convolver">Convolver</linkto>
// </prerequisite>

// <etymology>
// This class handles 2D convolution of images 
// </etymology>

// <synopsis>
// This class convolves an image by a specified 2D function.
// </synopsis>

// <example>
// <srcBlock>
// </srcBlock>
// </example>

// <motivation>
// Convolution is a standard image processing requirement.  The
// class object has no state.  The functions could be static.
// The convolution is done via FFT.  Thus input pixels which
// are masked are set to 0 before the convolution.  The mask
// is transferred to the output image.  No additional scaling
// of the output image values is done.
// 
// </motivation>

// <todo asof="2001/08/28">
//   <li> 
// </todo>
 

template <class T> class Image2DConvolver
{
public:

// Constructor 
   Image2DConvolver ();

// Copy constructor.  Uses reference semantics.
   Image2DConvolver(const Image2DConvolver<T> &other);

// Destructor
  ~Image2DConvolver();

// Assignment operator. Uses reference semantics.
   Image2DConvolver &operator=(const Image2DConvolver<T> &other);

// Convolve.   If the output image needs a mask and doesn't have one,
// it will be given one if possible.  The miscInfo, imageInfo,
// units and logger will be copied from the input to the output
// unless you indicate not to (copyMiscellaneous).  
   void convolve(LogIO& os, 
                 ImageInterface<T>& imageOut, 
                 ImageInterface<T>& imageIn, 
                 VectorKernel::KernelTypes kernelType,
                 const IPosition& pixelAxes,
                 const Vector<Quantum<Double> >& parameters,
                 Bool autoScale, Double scale,
                 Bool copyMiscellaneous=True);
//
// Convolve.   You specify the output image name.  If empty 
// String, the ImageInterface object will be a TempImage.
// If the output image needs a mask and doesn't have one,
// it will be given one if possible.  The miscInfo, imageInfo,
// units and logger will be copied from the input to the output.
// Only set askRemoveOut to True IF you are calling from the tasking
// environment.  You will then get the choice GUI prompt to remove the
// output file if it already exists.  The calling routine must
// delete the returned pointer.
   ImageInterface<T>* convolve(LogIO& os, const String& outFile,
                               ImageInterface<T>& imageIn,
                               VectorKernel::KernelTypes kernelType,
                               const IPosition& pixelAxes,
                               const Vector<Quantum<Double> >& parameters,
                               Bool autoScale, Double scale, 
                               Bool askRemoveOut);

private:

// Check kernel parameters
   void checkKernelParameters(LogIO& os, VectorKernel::KernelTypes kernelType,
                              const Vector<Quantum<Double> >& parameters) const;
//
   void dealWithRestoringBeam (LogIO& os, 
                               String& brightnessUnitOut,
                               Vector<Quantum<Double> >& beamOut,
                               Array<T>& kernelArray,
                               T kernelVolume,
                               VectorKernel::KernelTypes kernelType,
                               const Vector<Quantum<Double> >& parameters,
                               const IPosition& axes,
                               const CoordinateSystem& cSys,
                               const ImageInfo& imageInfo,
                               const Unit& brightnessUnit,
                               Bool autoscale, Double scale) const;
//
   T fillKernel (Matrix<T>& kernelMatrix,
                 VectorKernel::KernelTypes kernelType,
                 const IPosition& kernelShape,
                 const IPosition& axes,
                 const Vector<Double>& parameters) const;
//
   void fillGaussian (T& maxVal, T& volume,
                      Matrix<T>& pixels, T height, T xCentre,
                      T yCentre, T majorAxis, T ratio,
                      T positionAngle) const;
//
   T makeKernel(LogIO& os, Array<T>& kernel,
                   VectorKernel::KernelTypes kernelType,
                   const Vector<Quantum<Double> >& parameters,
                   const IPosition& axes,
                   const ImageInterface<T>& inImage) const;
//
   IPosition shapeOfKernel (VectorKernel::KernelTypes kernelType,
                            const Vector<Double>& parameters,
                            uInt ndim,
                            const IPosition& axes) const;
//
   uInt sizeOfGaussian (Double width, Double nSigma) const;
//
   Bool skyWidthsPixelToWorld (LogIO& os,
                               Vector<Quantum<Double> >& wParameters,
                               const CoordinateSystem& cSys,
                               const Vector<Double>& pParameters,
                               const IPosition& pixelAxes) const;
//
   void worldWidthsToPixel (LogIO& os, Vector<Double>& dParameters,
                            const Vector<Quantum<Double> >& parameters,
                            const CoordinateSystem& cSys,
                            const IPosition& pixelAxes) const;
//
   Double worldWidthToPixel (LogIO& os, Double positionAngle,  
                             const Quantum<Double>& length,
                             const CoordinateSystem& cSys,
                             const Vector<Double>& refVal,
                             const Vector<Double>& refPix,
                             const Unit& unit,
                             const IPosition& pixelAxes) const;
};


#endif



