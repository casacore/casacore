//# ImageConcat.h: concatenate images along an axis
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

#if !defined(AIPS_IMAGECONCAT_H)
#define AIPS_IMAGECONCAT_H


#include <aips/aips.h>
#include <trial/Lattices/LatticeConcat.h>
#include <trial/Images/ImageInterface.h>
#include <trial/Coordinates/CoordinateSystem.h>

class CoordinateSystem;
template <class T> class ImageSummary;


// <summary>
// Concatenates images along a specified axis
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=LatticeConcat>LatticeConcat</linkto> 
//   <li> <linkto class=ImageInterface>ImageInterface</linkto> 
// </prerequisite>

// <etymology>
// This is a class designed to concatenate images along a specified axis
// </etymology>

// <synopsis>
// This is a class designed to concatenate images along a specified 
// pre-existing axis.  This means you can join them together.  E.g., join 
// images of shape  [10,20,30] and [10,20,40] into an image of shape [10,20,70]
//
// You can also concatenate  a lattice to an image.  
// </synopsis>
//
// <example>
// <srcblock>
//// Make some PagedImages and give them a mask each
//
//      IPosition shape(2, 10, 20);
//      PagedImage<Float> im1(shape, CoordinateUtil::defaultCoords2D(),
//                            "tImageConcat_tmp1.img");
//      im1.set(1.0);
//      LCPagedMask mask1 = LCPagedMask(RegionHandler::makeMask (im1, "mask0"));
//      mask1.set(True);
//      im1.defineRegion ("mask0", ImageRegion(mask1), RegionHandler::Masks);
//      im1.setDefaultMask("mask0"); 
//
//      PagedImage<Float> im2(shape, CoordinateUtil::defaultCoords2D(),
//                            "tImageConcat_tmp2.img");
//      im2.set(2.0);
//      LCPagedMask mask2 = LCPagedMask(RegionHandler::makeMask (im2, "mask0"));
//      mask2.set(False);
//      im2.defineRegion ("mask0", ImageRegion(mask2), RegionHandler::Masks);
//      im2.setDefaultMask("mask0"); 
//
//// Make concatenator for axis 0
//
//      ImageConcat<Float> concat(0, False);
//
//// Relax coordinate constraints
//
//      concat.setImage(im1, True);
//      concat.setImage(im2, True);
//
//// Make output image  and mask (if required, which it will be in this case)
//
//      PagedImage<Float> im3(concat.shape(), CoordinateUtil::defaultCoords2D(),
//                            "tImageConcat_tmp3.img");
//      if (concat.isMasked()) {
//        LCPagedMask mask3 = LCPagedMask(RegionHandler::makeMask (im3, "mask0"));
//        im3.defineRegion ("mask0", ImageRegion(mask3), RegionHandler::Masks);
//        im3.setDefaultMask("mask0"); 
//      }
//
//// Do it.   
//
//      concat.copyData(im3);
//
// </srcblock>
// See tImageConcat.cc for more examples.
// </example>


// <motivation>
// Image concatentation is a useful enduser requirement.  
// </motivation>

// <todo asof="1999/09/23">
//   <li> Offer the ability to increase the dimensionality of
//        the output image
//   <li> This class is probably better to be derived from ImageInterface
//        like the LEL classes.    Then it would contain a LatticeConcat
//        object instead of deriving from it
// </todo>


template <class T> class ImageConcat : public LatticeConcat<Float>
{
public:

// Constructor. Specify the pixel axis for concatenation and
// whether you would like to see a progress meters
   ImageConcat (uInt axis, Bool showProgress);

// Default constructor, Sets the concatenation axis to 0
// and showProgress to False
   ImageConcat ();

// Copy constructor (reference semantics)
   ImageConcat(const ImageConcat<T> &other);

// Destructor
   ~ImageConcat ();

// Assignment operator (reference semantics)
   ImageConcat<T> &operator=(const ImageConcat<T> &other);

// Set concatenation pixel axis.  You can call this more than once
// and at any time - the state of the object changes as needed.
// If relax is False, throws an exception if the coordinates
// of the images are not consistent.  If True, just issues
// a warning.  
   void setAxis(uInt axis, Bool relax);

// Sets a new image into the list to be concatenated.  
// If relax is False, throws an exception if the coordinates
// of the images are not consistent.  If True, just issues
// a warning.  
   void setImage (ImageInterface<T>& image, Bool relax);

// Sets a new lattice into the list to be concatenated.  
// You can only concatenate a lattice with an image if
// you have first used setImage to set an image (this
// provides the CooridinateSystem information)
   void setLattice (MaskedLattice<T>& lattice);

// Resets the state of the object to axis=0, no lattices,
// and no progress meter
   void reset();

// Fill the supplied Lattice with the input images 
// so as to effectively concatenate them.    The images are 
// concatenated in the order in which you give them in
// the setLattice function.   Coordinates, history, etc
// are updated in the output image.  Throws
// exceptions for incompatibilities.
   void copyData(ImageInterface<T>& image);

private:

   Bool itsWarnAxisNames, itsWarnAxisUnits, itsWarnImageUnits;
   Bool itsWarnContig, itsWarnRefPix, itsWarnRefVal, itsWarnInc;
//
   Double coordConvert(Int& worldAxis, LogIO& os,
                       const CoordinateSystem& cSys,
                       uInt axis, Double pixelCoord) const;
   void checkContiguous (Bool& warnContig, const IPosition& shape1,
                         const CoordinateSystem& cSys1,
                         const CoordinateSystem& cSys2,
                         LogIO& os, uInt axis, Bool relax);
   void checkCoordinates (Bool& warnRefPix, Bool& warnRefVal,
                          Bool& warnInc, LogIO& os,
                          const ImageSummary<T>& sum1,
                          const ImageSummary<T>& sum2,
                          uInt axis, Bool relax);

};
#endif
