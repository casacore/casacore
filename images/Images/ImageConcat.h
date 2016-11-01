//# ImageConcat.h: concatenate images along an axis
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

#ifndef IMAGES_IMAGECONCAT_H
#define IMAGES_IMAGECONCAT_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/lattices/Lattices/Lattice.h>
#include <casacore/lattices/Lattices/LatticeConcat.h>
#include <casacore/images/Images/ImageInterface.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class CoordinateSystem;
template <class T> class ImageSummary;
template <class T> class MaskedLattice;


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
// axis. This means you can join them together.  E.g.,
// join images of shape  [10,20,30] and [10,20,40] into a lattice
// of shape [10,20,70].
//
// The ImageConcat object does not copy the input images, it
// just references them.  You can use the Lattice<T>::copyData(Lattice<T>)
// function to fill an output image with the concatenated input images
//
// If you use the putSlice function, be aware that it will change the
// underlying images if they are writable.
//
// You can also concatenate  a lattice to an image.  
// </synopsis>
//
// <example>
// <srcblock>
//      IPosition shape(2, 10, 20);
//      PagedImage<Float> im1(shape, CoordinateUtil::defaultCoords2D(),
//                            "tImageConcat_tmp1.img");
//      im1.set(1.0);
//      PagedImage<Float> im2(shape, CoordinateUtil::defaultCoords2D(),
//                            "tImageConcat_tmp2.img");
//      im2.set(2.0);
//
//// Make concatenator for axis 0
//
//      ImageConcat<Float> concat(0);
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
//
//// Copy to output
//
//      im3.copyData(concat);
//
// </srcblock>
// See tImageConcat.cc for more examples.
// </example>


// <motivation>
// Image concatentation is a useful enduser requirement.  
// </motivation>

// <todo asof="1999/10/23">
//   <li> Offer the ability to increase the dimensionality of
//        the output image
// </todo>


template <class T> class ImageConcat : public ImageInterface<T>
{
public:

// Constructor. Specify the pixel axis for concatenation
   explicit ImageConcat (uInt axis, Bool tempClose=True);

// Construct the object from an AipsIO file with the given name.
   ImageConcat (AipsIO& aio, const String& fileName);

// Default constructor, Sets the concatenation axis to 0
   ImageConcat();

// Copy constructor (reference semantics)
   ImageConcat (const ImageConcat<T> &other);

// Destructor
   virtual ~ImageConcat();

// Assignment operator (reference semantics)
   ImageConcat<T> &operator= (const ImageConcat<T> &other);

// Make a copy of the object (reference semantics).
   virtual ImageInterface<T>* cloneII() const;

// Save the image in an AipsIO file with the given name.
// It can be opened by the constructor taking a file name.
   virtual void save (const String& fileName) const;

// Get the image type (returns name of derived class).
   virtual String imageType() const;

// Is the lattice persistent and can it be loaded by other processes as well?
   virtual Bool isPersistent() const;

// Sets a new image into the list to be concatenated.  
// If relax is False, throws an exception if the images
// are not contiguous along the concatenation axis.
// If relax is True, it will create a non-regular TabularCoordinate
// for non-contiguous images if the coordinates are monotonic.
// Otherwise, it just uses the coordinates of the image
   void setImage (ImageInterface<T>& image, Bool relax);

// Add a clone of the lattice to the list to be concatenated.  
// You can only concatenate a lattice with an image if
// you have first used setImage to set an image (this
// provides the CooridinateSystem information)
   void setLattice (MaskedLattice<T>& lattice);

// Return the number of images/lattices set so far
   uInt nimages() const
     { return latticeConcat_p.nlattices(); }

// Returns the current concatenation axis (0 relative)
   uInt axis () const
     { return latticeConcat_p.axis(); }

// Returns the number of dimensions of the *input* images/lattices
// Returns 0 if none yet set. 
   uInt imageDim() const
     { return latticeConcat_p.latticeDim(); }

// Handle the (un)locking and syncing, etc.
// <group>
   virtual Bool lock (FileLocker::LockType, uInt nattempts);
   virtual void unlock();
   virtual Bool hasLock (FileLocker::LockType) const;
   virtual void resync();
   virtual void flush();
   virtual void tempClose();
   virtual void reopen();
// </group>

// Return the name of the current ImageInterface object.
// If the object is persistent, it returns its file name.
// Otherwise it returns the string "Concatenation :"
   virtual String name (Bool stripPath=False) const;

// Has the object really a mask?
   virtual Bool isMasked() const;

// Does the image have a pixelmask?
   virtual Bool hasPixelMask() const;

// Get access to the pixelmask.
// An exception is thrown if the image does not have a pixelmask
// <group>
   virtual const Lattice<Bool>& pixelMask() const;
   virtual Lattice<Bool>& pixelMask();
  // </group>
  
// Get the region used (always returns 0)
   virtual const LatticeRegion* getRegionPtr() const;

// If all of the underlying lattices are writable returns True
   virtual Bool isWritable() const;
   
// Return the shape of the concatenated image
   virtual IPosition shape() const;

  
// Return the best cursor shape.  This isn't very meaningful for an ImageConcat
// Image since it isn't on disk !  But if you do copy it out, this is
// what you should use.  The maxPixels aregument is ignored.   
   virtual IPosition doNiceCursorShape (uInt maxPixels) const;

// Do the actual get of the data.
// The return value is always False, thus the buffer does not reference
// another array.  Generally the user should use function getSlice
   virtual Bool doGetSlice (Array<T>& buffer, const Slicer& section);
   
// Do the actual get of the mask data.
// The return value is always False, thus the buffer does not reference
// another array. Generally the user should use function getMaskSlice
   virtual Bool doGetMaskSlice (Array<Bool>& buffer, const Slicer& section);

// Do the actual put of the data into the Lattice.  This will change the
// underlying images (if they are writable) that were used to create the
// ImageConcat object. It throws an exception if not writable.
// Generally the user should use function putSlice
   virtual void doPutSlice (const Array<T>& sourceBuffer,
                            const IPosition& where,
                            const IPosition& stride);      

// Throws an excpetion as you cannot reshape an ImageConcat object
   virtual void resize(const TiledShape&);

// Check class invariants.
  virtual Bool ok() const;
  
// These are the implementations of the LatticeIterator letters.
// <note> not for public use </note>
  virtual LatticeIterInterface<T> *makeIter
                               (const LatticeNavigator &navigator,
				Bool useRef) const;


private:
   LatticeConcat<T> latticeConcat_p;
   Bool warnAxisNames_p, warnAxisUnits_p, warnImageUnits_p;
   Bool warnContig_p, warnRefPix_p, warnRefVal_p, warnInc_p, warnTab_p;
   Bool isContig_p;
   mutable String fileName_p;     // Empty if not persistent
   Vector<Bool> isImage_p;
   Vector<Double> pixelValues_p;
   Vector<Double> worldValues_p;
   Coordinate::Type originalAxisType_p;

   Double coordConvert(Int& worldAxis, LogIO& os,
                       const CoordinateSystem& cSys,
                       uInt axis, Double pixelCoord) const;

   void _checkContiguous(const IPosition& shape1,
                            const CoordinateSystem& cSys1,
                            const CoordinateSystem& cSys2,
                            LogIO& os, uInt axis, Bool relax);

   void checkNonConcatAxisCoordinates (LogIO& os,
                                       const ImageInterface<T>& image,
                                       Bool relax);

   Vector<Int> makeNewStokes(const Vector<Int>& stokes1,
                             const Vector<Int>& stokes2);

   // Updates the CoordinateSystem in the ImageConcat image. The first lattice must
   // be an image.  The first lattice is contiguous by definition.  The Coordinate
   // System for the first image must be set before calling this function. For
   // the first image, this function just sets up worldValues and pixelValues
   void setCoordinates();

   void _updatePixelAndWorldValues(uInt iIm);

  //# Make members of parent class known.
public:
  using ImageInterface<T>::logger;
  using ImageInterface<T>::coordinates;
  using ImageInterface<T>::units;
  using ImageInterface<T>::miscInfo;
protected:
  using ImageInterface<T>::setCoordsMember;
  using ImageInterface<T>::setMiscInfoMember;
};



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/images/Images/ImageConcat.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
