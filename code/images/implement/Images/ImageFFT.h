//# ImageFFT.h: FFT an image
//# Copyright (C) 1996,1997,1998,1999
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

#if !defined(AIPS_IMAGEFFT_H)
#define AIPS_IMAGEFFT_H

#include <aips/aips.h>
#include <trial/Images/ImageInterface.h>
class CoordinateSystem;

// <summary>
// FFT an image
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=LatticeFFT>LatticeFFT</linkto> 
//   <li> <linkto class=ImageInterface>ImageInterface</linkto> 
// </prerequisite>

// <etymology>
// Take the fast Fourier Transform of an image.
// </etymology>

// <synopsis>
// This class takes the FFT of an image.  Currently, its functionality
// is limited to taking the FFT of the sky plane of an image.  All
// other axes are iterated through.  
// </synopsis>
//
// <example>
// <srcblock>
//
//// Make a constant image
//
//      IPosition shape(2, 10, 20);
//      PagedImage<Float> im1(shape, CoordinateUtil::defaultCoords2D(), im1");
//      im1.set(1.0);
//
//// FFT
//
//      ImageFFT fft;
//      fft.fftsky(im1);
//      
//// Recover real and imaginary. The coordinates will be overwritten
//
//      PagedImage<Float> r1(shape, CoordinateUtil::defaultCoords2D(), real1");
//      PagedImage<Float> i1(shape, CoordinateUtil::defaultCoords2D(), imag1");
//      fft.getReal(r1);
//      fft.getImag(i1);
// </srcblock>
// </example>


// <motivation>
// Taking the Fourier Transform of an image is a basic part of image analysis
// </motivation>

// <todo asof="1999/09/23">
//   <li> Handle masks
//   <li> FT axes other than the sky axes
//   <li> reverse transformations
// </todo>


class ImageFFT 
{
public:

// Default constructor
   ImageFFT ();

// Copy constructor (reference semantics)
   ImageFFT(const ImageFFT& other);

// Assignment (reference semantics)
   ImageFFT& operator=(const ImageFFT& other);

// Destructor
   ~ImageFFT();

// Do the FFT of the sky plane to the uv plane
   void fftsky (const ImageInterface<Float>& in);

// Return the FFT in the desired form.    The CoordinateSystem of
// the image is overwritten by these functions to reflect the 
// Fourier axes.  
// <group>
   const ImageInterface<Complex>& getComplex () const;
   void getComplex (ImageInterface<Complex>& out) const;
   void getReal (ImageInterface<Float>& out) const;
   void getImaginary (ImageInterface<Float>& out) const;
   void getAmplitude (ImageInterface<Float>& out) const;
   void getPhase (ImageInterface<Float>& out) const;
// </group>

private:

   ImageInterface<Complex>* itsTempImagePtr;
   ImageInterface<Float>* itsInImagePtr;
   Bool itsDone;

// FFT the sky
   void fftsky2 (ImageInterface<Complex>& out,
                 const ImageInterface<Float>& in);

// Overwrite the coordinates for the sky axes
// with Fourier coordinates
   void fiddleCoordinates (LogIO& os);

// Copy some bits and pieces from the input to the output
// images
   void copyMiscellaneous (ImageInterface<Float>& out,   
                           const ImageInterface<Float>& in) const;
   void copyMiscellaneous (ImageInterface<Complex>& out,   
                           const ImageInterface<Float>& in) const;

// Find the sky axes in this CoordinateSystem
   void findSky(LogIO& os, Int& dC, Vector<Int>& pixelAxes,
                Vector<Int>& worldAxes, const CoordinateSystem& cSys);
};
#endif
