//# SepImageConvolver.h: separable convolution of an image
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

#ifndef IMAGES_SEPIMAGECONVOLVER_H
#define IMAGES_SEPIMAGECONVOLVER_H


//# Includes
#include <casa/aips.h>
#include <casa/Logging/LogIO.h>
#include <casa/Arrays/Vector.h>
#include <casa/Containers/Block.h>
#include <images/Images/ImageInterface.h>
#include <scimath/Mathematics/VectorKernel.h>

namespace casa { //# NAMESPACE CASA - BEGIN

//# Forward Declarations
template <class T> class Quantum;
class String;



// <summary>
// This class does separable convolution of an image
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="ImageInterface">ImageInterface</linkto>
//   <li> <linkto class="Convolver">Convolver</linkto>
// </prerequisite>

// <etymology>
// This class handles convolution of images by separable kernels.
// </etymology>

// <synopsis>
// Convolution kernels can be separable or not.  For example,
// convolution of an image by a 2-D gaussian when the position angle
// of the Gaussian is along one of the axes is separable. If the
// position angle is otherwise, it is not separable.   When the
// kernel is separable, an N-dimensional specification of the
// convolution kernel is straightforward.
//
// Although this class is templated, it will only work
// for Float and Double types.
// </synopsis>

// <example>
// <srcBlock>
// </srcBlock>
// </example>

// <motivation>
// Separable and non-separable convolution are standard requirements.
// </motivation>

// <todo asof="1999/03/31">
//   <li> 
// </todo>
 

template <class T> class SepImageConvolver
{
public:


// Constructor 
   SepImageConvolver (ImageInterface<T>& image, LogIO &os, Bool showProgress);

// Copy constructor.  Uses reference semantics.
   SepImageConvolver(const SepImageConvolver<T> &other);

// Destructor
  ~SepImageConvolver();

// Assignment operator. Uses reference semantics.
   SepImageConvolver &operator=(const SepImageConvolver<T> &other);

// Set convolution kernel vector.  The specified axis is convolved
// by the given kernel.
   void setKernel(uInt axis, const Vector<T>& kernel);

// Set convolution kernel.  The specified axis is convolved
// by the given kernel.  If autoScale is True then kernel volume is unity,
// else kernel peak is 1 * scale. If useImageShapeExactly is True, the kernel
// will be the shape of the axis, else it will be big enough 
// to accomodate the kernel width (e.g. +/- 5sigma for Gaussian)
// <group>
   void setKernel(uInt axis, VectorKernel::KernelTypes kernelType,
                  const Quantum<Double>& width, Bool autoScale,
                  Bool useImageShapeExactly=True, Double scale=1.0);
   void setKernel(uInt axis, VectorKernel::KernelTypes kernelType,
                  Double width, Bool autoScale, 
                  Bool useImageShapeExactly=True, Double scale=1.0);
// </group>

// Get the convolution kernel for the specified axis
   Vector<T> getKernel(uInt axis);

// Get the convolution kernel shape for the specified axis
   uInt  getKernelShape(uInt axis);

// Perform the convolution either outputting to a new image
// or in-situ.    The error checking for the
// convolution parameters is done when you call this
// function.  If outputting a new image, and it needs a mask and doesn't have one,
// the it will be given one if possible and the input
// mask will be transferred to the output.  Masked pixels
// are zeroed before convolving
// <group>
   void convolve(ImageInterface<T>& imageOut);
   void convolve();
// </group>


private:
   ImageInterface<T>* itsImagePtr;
   LogIO itsOs;
   Vector<uInt> itsAxes;
   PtrBlock<Vector<T>* > itsVectorKernels;
   Bool itsShowProgress;

//

   void checkAxis(uInt axis);
   Bool isTempImage (const ImageInterface<Float>* pIm) const;
   void zero();
   void smoothProfiles (ImageInterface<T>& in,
                        const Int& axis,
                        const Vector<T>& psf);
};



} //# NAMESPACE CASA - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <images/Images/SepImageConvolver.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
