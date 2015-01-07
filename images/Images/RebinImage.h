//# RebinImage.h: rebin an image
//# Copyright (C) 2003
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

#ifndef IMAGES_REBINIMAGE_H
#define IMAGES_REBINIMAGE_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/images/Images/ImageInterface.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
template <class T> class RebinLattice;
class LogIO;

// <summary>
// Rebin an image
// </summary>
//
// <use visibility=export>
//
// <reviewed reviewer="" date="" tests="tRebinImage.cc">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class=ImageInterface>ImageInterface</linkto>
//   <li> <linkto class=RebinLattice>RebinLattice</linkto>
// </prerequisite>
//
// <synopsis> 
// Class RebinImage can be used to rebin (data averaged over bin) an image 
// by integer amounts per axis.
// </synopsis> 
//
// <example>
// <srcblock>
//    IPosition factors(2,2,2);
//    PagedImage<Float> imageIn(String("myImage")):
//    RebinLattice<Float> rb(imageIn, factors);
//    IPosition shapeOut = rb.shape();
//    TiledShape tShapeOut(shapeOut);
//    TempImage<Float> imageOut(tShapeOut, rb.coordinates());
//    LatticeUtilities::copyDataAndMask(os, imageOut, rb);
//    ImageUtilities::copyMiscellaneous (imageOut, imageIn);
// </srcblock>
// </example>
//
// <motivation>
// Users like to rebin images...
// </motivation>
//
// <todo asof="2004/04/07">
// </todo>


template <class T> class RebinImage: public ImageInterface<T>
{
public: 

  // Default constructor (object useless)
  RebinImage ();

  // Constructor. The bin factors don't have to be integral. Anything left over
  // at the end is treated as a full bin.
  RebinImage (const ImageInterface<T>&, const IPosition& factors);

  // Copy constructor (reference semantics).
  RebinImage (const RebinImage<T>& other);
    
  virtual ~RebinImage();

  // Assignment (reference semantics).
  RebinImage<T>& operator= (const RebinImage<T>& other);

  // Make a copy of the object (reference semantics).
  // <group>
  virtual ImageInterface<T>* cloneII() const;
  // </group>

  // Get the image type (returns name of derived class).
  virtual String imageType() const;

  // Is the RebinImage masked?
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

  // A RebinImage is not persistent.
  virtual Bool isPersistent() const;

  // Is the RebinImage paged to disk?
  virtual Bool isPaged() const;

  // An RebinImage is not writable
  virtual Bool isWritable() const;

  // Returns the shape of the RebinImage
  virtual IPosition shape() const;
  
  // This function returns the recommended maximum number of pixels to
  // include in the cursor of an iterator.
  virtual uInt advisedMaxPixels() const;

  // Function which changes the shape of the RebinImage.
  // Throws an exception as resizing an RebinImage is not possible.
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
  // Non-unit strides are not yet supported.
  virtual Bool doGetSlice (Array<T>& buffer, const Slicer& section);

  // Putting data is not possible as the lattice is not writable.
  virtual void doPutSlice (const Array<T>& sourceBuffer,
			   const IPosition& where,
			   const IPosition& stride);
  
  // Get a section of the mask.
  // Non-unit strides are not yet supported.
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
  RebinLattice<T>*   itsRebinPtr;

  //# Make members of parent class known.
public:
  using ImageInterface<T>::logger;
protected:
  using ImageInterface<T>::setCoordsMember;
};



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/images/Images/RebinImage.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
