//# ImageTwoPtCorr.h: Compute two point correlation function of an image
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

#ifndef IMAGES_IMAGETWOPTCORR_H
#define IMAGES_IMAGETWOPTCORR_H

#include <casa/aips.h>
#include <images/Images/ImageInterface.h>
#include <lattices/Lattices/LatticeTwoPtCorr.h>

namespace casa { //# NAMESPACE CASA - BEGIN

class IPosition;

// <summary>
// Compute two point correlation functions from images
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=LatticeTwoPtCorr>LatticeTwoPtCorr</linkto> 
//   <li> <linkto class=ImageInterface>ImageInterface</linkto> 
//   <li> <linkto class=TempImage>TempImage</linkto> 
// </prerequisite>

// <etymology>
// Compute the two point correlation function of an image
// </etymology>

// <synopsis>
// This class allows you to compute two point correlation functions
// from an image  over planes of the specified two axes. Presently
// only autocorrelations and in particualt the structure function are implemented.  
//
// The structure function is  <src>S(x,y) = < [image(i,j) - image(i+x,j+y)]**2 ></src>
// where x and y are absolute integer shifts (or lags) and the
// ensemble average is for each lag pair, x&y.
// </synopsis>
//
// <example>
// <srcblock>
// </srcblock>
// </example>


// <motivation>
// Taking the Structure Function of an image is a basic part of image analysis
// </motivation>

// <todo asof="2003/12/13">
//   <li> Add more types
//   <li> Add crosscorrelation
// </todo>


template <class T> class ImageTwoPtCorr 
{
public:

// Default constructor
   ImageTwoPtCorr ();

// Copy constructor (reference semantics)
   ImageTwoPtCorr(const ImageTwoPtCorr& other);

// Assignment (reference semantics)
   ImageTwoPtCorr& operator=(const ImageTwoPtCorr& other);

// Destructor
   ~ImageTwoPtCorr();

// Compute the desired autocorrelation function for the specified plane.  You specify 
// which two axes to compute the structure function over.  If the 
// axes array is empty (or not an argument), then the Sky plane 
// is selected if it exists, otherwise 
// the first two axes are selected.  The CoordinateSystem of the output image 
// is overwritten.  The miscellaneous items (ImageInfo, MiscInfo, Logger)
// are copied from the input image to the output.
// <group>
   void autoCorrelation (ImageInterface<T>& out,
                         const ImageInterface<T>& in,
                         const IPosition& axes, 
                         typename LatticeTwoPtCorr<T>::Method method,
                         Bool progress=True) const;
   void autoCorrelation (ImageInterface<T>& out,
                         const ImageInterface<T>& in,
                         typename LatticeTwoPtCorr<T>::Method method,
                         Bool progress=True) const;
// </group>

// Helper function to set up the axes vector.  If axes is of length 0,
// it looks for the Sky (DirectionCoordinate). If that's not there,
// you get the first two axes in the image.
   static IPosition setUpAxes (const IPosition& axes,
                               const CoordinateSystem& cSys);

// Helper function to provide output image shape given the input shape
// and the axes to find the structure function over.
   static IPosition setUpShape (const IPosition& inShape, const IPosition& axes);



private:


// Copy MiscInfo, ImageInfo, and logSInk to output
   void copyMiscellaneous (ImageInterface<T>& out,
                           const ImageInterface<T>& in) const;

// Overwrite the CoordinateSystem
   void setCoordinateSystem (ImageInterface<T>& out,
                             const ImageInterface<T>& in,
                             const IPosition& axes) const;

// Set the brightness unit
   void setUnit (ImageInterface<T>& out) const;

};

} //# NAMESPACE CASA - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <images/Images/ImageTwoPtCorr.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
