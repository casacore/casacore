//# Image2DConvolver.h: 2D convolution of an image
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002,2003
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

#ifndef IMAGES_IMAGE2DCONVOLVER_H
#define IMAGES_IMAGE2DCONVOLVER_H


//# Includes
#include <casa/aips.h>
#include <casa/Logging/LogIO.h>
#include <casa/Arrays/Array.h>
#include <scimath/Mathematics/VectorKernel.h>

namespace casa { //# NAMESPACE CASA - BEGIN

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
};



} //# NAMESPACE CASA - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <images/Images/Image2DConvolver.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif



