//# ImageConvolver.h: convolution of an image by array
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2003
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

#if !defined(AIPS_IMAGECONVOLVER_H)
#define AIPS_IMAGECONVOLVER_H


//# Includes
#include <aips/aips.h>
#include <aips/Logging/LogIO.h>
#include <aips/Arrays/Array.h>

//# Forward Declarations
template <class T> class ImageInterface;
template <class T> class Lattice;
class String;



// <summary>
// This class does convolution of an image by an Array or Lattice
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="ImageInterface">ImageInterface</linkto>
//   <li> <linkto class="LatticeConvolver">LatticeConvolver</linkto>
// </prerequisite>

// <etymology>
// This class handles convolution of images by an Array or Lattice
// </etymology>

// <synopsis>
// This class convolves an image by a specified kernel (Array or 
// Lattice).  If the kernel does not have enough dimensions, degenerate
// ones are added.The class object has no state.  The functions could be static.
// The convolution is done via FFT.  Thus input pixels which
// are masked are set to 0 before the convolution.  The mask
// is transferred to the output image.  No additional scaling
// of the output image values is done.
// 

// </synopsis>

// <example>
// <srcBlock>
// </srcBlock>
// </example>

// <motivation>
// Convolution is a standard image processing requirement.  
// </motivation>

// <todo asof="2001/08/28">
//   <li> 
// </todo>
 

template <class T> class ImageConvolver
{
public:

enum ScaleTypes {

// None; neither autoscaling nor direct scaling
   NONE,

// Autoscale (normalize kernel to unit sum)
   AUTOSCALE,

// SCALE (apply given scale factor)
   SCALE,

// Number
   NTypes};

// Constructor 
   ImageConvolver ();

// Copy constructor.  Uses reference semantics.
   ImageConvolver(const ImageConvolver<T> &other);

// Destructor
  ~ImageConvolver();

// Assignment operator. Uses reference semantics.
   ImageConvolver &operator=(const ImageConvolver<T> &other);

// Convolve.   If the output image needs a mask and doesn't have one,
// it will be given one if possible. The input mask is transferred to
// the output. The miscInfo, imageInfo, units and logger will be copied 
// from the input to the output unless you indicate not 
// to (copyMiscellaneous).  Any restoring beam is deleted from the
// output image ImageInfo object.  Degenerate  axes are added 
// to the kernel if it does not have enough dimensions.   If <src>autoScale</src>
// is true, the kernel is normalized to have unit sum.  Otherwise,
// the kernel is scaled (multiplied) by the value <src>scale</src>
// <group>
   void convolve(LogIO& os, 
                 ImageInterface<T>& imageOut, 
                 ImageInterface<T>& imageIn, 
                 const Array<T>& kernel,
                 ScaleTypes scaleType,
                 Double scale,
                 Bool copyMiscellaneous=True);
   void convolve(LogIO& os, 
                 ImageInterface<T>& imageOut, 
                 ImageInterface<T>& imageIn, 
                 const Lattice<T>& kernel,
                 ScaleTypes scaleType,
                 Double scale,
                 Bool copyMiscellaneous=True);
// </group>

private:

// Make mask for image
   void makeMask(ImageInterface<T>& out, LogIO& os) const;
};


#endif
