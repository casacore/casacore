//# ExtendImage.h: An extension of an ImageInterface object
//# Copyright (C) 2001,2003
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

#ifndef IMAGES_EXTENDIMAGE_H
#define IMAGES_EXTENDIMAGE_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/images/Images/ImageInterface.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
template <class T> class ExtendLattice;


// <summary>
// An extension of an ImageInterface object.
// </summary>
//
// <use visibility=export>
//
// <reviewed reviewer="" date="" tests="tExtendImage.cc">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class=ImageInterface>ImageInterface</linkto>
//   <li> <linkto class=ExtendLattice>ExtendLattice</linkto>
// </prerequisite>
//
// <synopsis> 
// Class ExtendImage can be used to (virtually) extend an image
// along axes with length 1 and/or to add new axes. In this way such
// an image can be made conformant with another image.
// E.g. it can be used to extend the continuum channel to
// subtract it from each channel in an image cube.
// </synopsis> 
//
// <example>
// <srcblock>
// </srcblock>
// </example>
//
// <motivation>
// Used by LEL to handle images with different dimensionalities.
// </motivation>
//
//# <todo asof="1998/02/09">
//# </todo>


template <class T> class ExtendImage: public ImageInterface<T>
{
public: 
  // The default constructor
  ExtendImage();

  // Create a ExtendImage from a Image.
  // The coordinate system of the given image should be a subset of the
  // new coordinate system. The same is true for the shape.
  ExtendImage (const ImageInterface<T>& image,
	       const IPosition& newShape,
	       const CoordinateSystem& newCsys);
  
  // Copy constructor (reference semantics).
  ExtendImage (const ExtendImage<T>& other);
    
  virtual ~ExtendImage();

  // Assignment (reference semantics).
  ExtendImage<T>& operator= (const ExtendImage<T>& other);

  // Make a copy of the object (reference semantics).
  // <group>
  virtual ImageInterface<T>* cloneII() const;
  // </group>

  // Get the image type (returns name of derived class).
  virtual String imageType() const;

  // Is the ExtendImage masked?
  // It is if its parent image is masked.
  virtual Bool isMasked() const;

  // Does the image object have a pixelmask?
  // It does if its parent has a pixelmask.
  virtual Bool hasPixelMask() const;

  // Get access to the pixelmask in use (thus to the pixelmask of the parent).
  // An exception is thrown if the parent does not have a pixelmask.
  // <group>
  virtual const Lattice<Bool>& pixelMask() const;
  virtual Lattice<Bool>& pixelMask();
  // </group>

  // Get the region used (always returns 0).
  virtual const LatticeRegion* getRegionPtr() const;

  // A ExtendImage is not persistent.
  virtual Bool isPersistent() const;

  // Is the ExtendImage paged to disk?
  virtual Bool isPaged() const;

  // An ExtendImage is not writable
  virtual Bool isWritable() const;

  // Returns the shape of the ExtendImage
  virtual IPosition shape() const;
  
  // This function returns the recommended maximum number of pixels to
  // include in the cursor of an iterator.
  virtual uInt advisedMaxPixels() const;

  // Function which changes the shape of the ExtendImage.
  // Throws an exception as resizing an ExtendImage is not possible.
  virtual void resize(const TiledShape& newShape);

  // Return the name of the parent ImageInterface object. 
  virtual String name (Bool stripPath=False) const;
  
  // Check class invariants.
  virtual Bool ok() const;

  // Get access to the attribute handler (of the parent image).
  // If a handler keyword does not exist yet, it is created if
  // <src>createHandler</src> is set.
  // Otherwise the handler is empty and no groups can be created for it.
  virtual ImageAttrHandler& attrHandler (Bool createHandler=False);

  // Do the actual getting of an array of values.
  virtual Bool doGetSlice (Array<T>& buffer, const Slicer& section);

  // Putting data is not possible.
  virtual void doPutSlice (const Array<T>& sourceBuffer,
			   const IPosition& where,
			   const IPosition& stride);
  
  // Get a section of the mask.
  virtual Bool doGetMaskSlice (Array<Bool>& buffer, const Slicer& section);

  // This function is used by the LatticeIterator class to generate an
  // iterator of the correct type for this Lattice. Not recommended
  // for general use. 
  virtual LatticeIterInterface<T>* makeIter
                            (const LatticeNavigator& navigator,
			     Bool useRef) const;

  // Get the best cursor shape.
  virtual IPosition doNiceCursorShape (uInt maxPixels) const;

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

private:
  //# itsImagePtr points to the parent image.
  ImageInterface<T>* itsImagePtr;
  ExtendLattice<T>*  itsExtLatPtr;

  //# Make members of parent class known.
public:
  using ImageInterface<T>::logger;
protected:
  using ImageInterface<T>::setCoordsMember;
};



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/images/Images/ExtendImage.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
