//# ImageUtilities.h: Some utility functions handy for accessing images
//# Copyright (C) 1996,1997,1999,2000,2001,2002
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
//#
//# $Id$
#ifndef IMAGES_IMAGEUTILITIES_H
#define IMAGES_IMAGEUTILITIES_H


#include <casacore/casa/aips.h>
#include <casacore/scimath/Mathematics/GaussianBeam.h>
#include <casacore/lattices/Lattices/TiledShape.h>
#include <casacore/casa/Utilities/PtrHolder.h>
#include <casacore/casa/Utilities/CountedPtr.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
template <class T> class ImageInterface;
template <class T> class Vector;
template <class T> class Quantum;
template <class T> class MaskedArray;
template <class T> class PtrHolder;
class CoordinateSystem;
class Coordinate;
class String;
class IPosition;
class LogIO;
class AxesSpecifier;
class ImageAttrHandler;

//
// <summary>
// Utility functions for Image manipulation
// </summary>
//   
//
// <use visibility=export>
//
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
// 
// <prerequisite>
//   <li> IPosition
//   <li> Arrays
//   <li> Lattice
// </prerequisite>
//
// <synopsis>
// Some helpful static functions that are common to some of my image
// analysis application  classes.
// </synopsis>
//
// <motivation>
// I needed some bits and pieces.  My goal isto move this rag-tag bunch
// out of here into other classes as time goes on.  So far
// I have eliminated 80% of the original !
// </motivation>
//
// <todo asof="1996/11/27">
//  <li>  
// </todo>
 

class ImageUtilities
{
public:
  // Open disk image (can be any registered image).  Exception
  // if fileName empty or file does not exist or file is not
  // of legal image type.   For Casacore images, the default mask is
  // applied.
  //  <group>
  template<class T>
  static void openImage (PtrHolder<ImageInterface<T> >& image,
                         const String& fileName);

  template<class T>
  static void openImage (ImageInterface<T>*& image,
                         const String& fileName);

  template<class T>
  static SHARED_PTR<ImageInterface<T> > openImage (const String& fileName);
//  </group>

// Copy MiscInfo, ImageInfo, brightness unit and logger (history) from in to out
   template <typename T, typename U>
   static void copyMiscellaneous (ImageInterface<T>& out,
                                  const ImageInterface<U>& in,
                                  Bool copyImageInfo = True);

// Copy a mask from one image to another
   template <typename T, typename U>
   static void copyMask (ImageInterface<T>& out,
                         const ImageInterface<U>& in,
                         const String& maskOut, const String& maskIn,
                         AxesSpecifier axesSpecifier);

  // Copy the attributes from one image to another.
  static void copyAttributes (ImageAttrHandler& out,
                              ImageAttrHandler& in);

// Add one degenerate axis for each of the specified coordinate types.
// If the outfile string is given the new image is a PagedImage.
// If the outfile string is empty, the new image is a TempImage.
   template <typename T>
   static void addDegenerateAxes (
		   LogIO& os,
		   PtrHolder<ImageInterface<T> >& outImage,
		   const ImageInterface<T>& inImage,
		   const String& outFile, Bool direction,
		   Bool spectral, const String& stokes,
		   Bool linear, Bool tabular, Bool overwrite,
                   Bool silent=False
   );

// Function to bin up (average data) one axis of an N-D MaskedArray. The interface
// is pretty specific to a particular application. It's here because
// its implemented with ImageRebin.  On input, the output MA *must*
// have zero shape.   The input and output Coordinates must have the
// same type and have only one axis (Linear,Spectral & Tabular).
// The output coordinate is adjusted for the binning.   The binning
// factor does not have to fit integrally into the shape of the specified
// axis.
   template <typename T>
   static void bin (MaskedArray<T>& out, Coordinate& coordOut,
                    const MaskedArray<T>& in, const Coordinate& coordIn,
                    uInt axis, uInt bin);

// This function converts pixel coordinates to world coordinates. You
// specify a vector of pixel coordinates (<src>pixels</src>) for only one 
// axis, the <src>pixelAxis</src>.    For the other pixel axes in the
// <src>CoordinateSystem</src>, if a pixel axis "i" is  found in the 
// <src>CursorAxes</src> vector, its pixel coordinate is set to 
// the average of the selected region from the image for that axis
// (<src>(blc(i)+trc(i))/2)</src>), otherwise it is set to the reference pixel.   
// The vector of world coordinates for <src>pixelAxis</src> is returned as formatted 
// Strings.  If for some reason it can't make the conversion, the element
// element is returned as "?"    Returns <src>False</src> if the lengths of
// <<src>blc</src> and <src>trc</src> are not equal to the number of pixel axes
// in the coordinate system.
   static Bool pixToWorld (
		   Vector<String>& sWorld,
		   const CoordinateSystem& cSys,
		   const Int& pixelAxis,
		   const Vector<Int>& cursorAxes,
		   const IPosition& blc,
		   const IPosition& trc,
		   const Vector<Double>& pixels,
		   const Int& prec,
		   const Bool usePrecForMixed=False
   );

// Convert long axis names "Right Ascension", "Declination", "Frequency" and
// "Velocity" to "RA", "Dec", "Freq", "Vel" respectively.  Unknown strings
// are returned as given.
   static String shortAxisName (const String& axisName);

   // write the specified image and add the specified pixels to it.
   // Currently no checks are done to ensure the pixel array size and
   // mapShape are compatible; the caller is responsible for this check.
   static void writeImage(
   		const TiledShape& mapShape,
   		const CoordinateSystem& coordinateInfo,
   		const String& imageName,
   		const Array<Float>& pixels, LogIO& log,
   		const Array<Bool>& pixelMask = Array<Bool>()
   );

   static GaussianBeam makeFakeBeam(
		   LogIO& logIO, const CoordinateSystem& csys,
		   Bool suppressWarnings = False
   );

   static void getUnitAndDoppler(
	   String& xUnit, String& doppler,
	   const uInt axis, const CoordinateSystem& csys
   );
};



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/images/Images/ImageUtilities2.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
