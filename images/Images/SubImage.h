//# SubImage.h: A (masked) subset of an ImageInterface object
//# Copyright (C) 1998,1999,2000,2001,2003
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

#ifndef IMAGES_SUBIMAGE_H
#define IMAGES_SUBIMAGE_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/ArrayFwd.h>
#include <casacore/images/Images/ImageInterface.h>
#include <casacore/casa/Arrays/AxesSpecifier.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class IPosition;
class LattRegionHolder;
class Slicer;
template <class T> class SubLattice;
class LatticeNavigator;
template <class T> class LatticeIterInterface;
class String;


// <summary>
// A (masked) subset of an ImageInterface object.
// </summary>
//
// <use visibility=export>
//
// <reviewed reviewer="" date="" tests="tSubImage.cc">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class=ImageInterface>ImageInterface</linkto>
//   <li> <linkto class=SubLattice>SubLattice</linkto>
// </prerequisite>
//
// <synopsis> 
// Class SubImage has to be used to apply a region or mask to an image.
// Several functions are inherited from SubLattice and not declared
// in this class.
// <p>
// Using an <linkto class=AxesSpecifier>AxesSpecifier</linkto> object
// it is possible to remove some or all degenerate axes (i.e. axes
// with length 1) to get an image with a lower dimensionality.
// </synopsis> 
//
// <example>
// <srcblock>
// </srcblock>
// </example>
//
// <motivation>
// </motivation>
//
// <todo asof="1998/02/09">
// </todo>


template <class T> class SubImage: public ImageInterface<T>
{
public: 
  // The default constructor
  SubImage();

  // Create a SubImage from a Image.
  // This results in a SubImage without a real mask.
  // <br>The "const Image" version yields a non-writable SubImage,
  // while for the non-const version one has to specify if the SubImage
  // should be writable (if the original image is non-writable, the
  // SubImage is always set to non-writable).
  // <br>If preserveAxesOrder is true, the axes order will be preserved. This
  // is only important in cases where pixel axes are to be dropped, if not
  // the axes order will be preserved. If false and pixel axes are dropped,
  // the order of the coordinates will be preserved, but not necessarily
  // the axes.
  // <group>
  SubImage (const ImageInterface<T>& image,
	    AxesSpecifier=AxesSpecifier(), bool preserveAxesOrder=false);
  SubImage (ImageInterface<T>& image, bool writableIfPossible,
	    AxesSpecifier=AxesSpecifier(), bool preserveAxesOrder=false);
  // </group>

  // Create a SubImage from the given Image and region.
  // <br>An exception is thrown if the image shape used in the region
  // differs from the shape of the image.
  // <group>
  SubImage (const ImageInterface<T>& image, const LattRegionHolder& region,
	    AxesSpecifier=AxesSpecifier(), bool preserveAxesOrder=false);
  SubImage (ImageInterface<T>& image, const LattRegionHolder& region,
	    bool writableIfPossible,
	    AxesSpecifier=AxesSpecifier(), bool preserveAxesOrder=false);
  // </group>
  
  // Create a SubImage from the given Image and slicer.
  // The slicer can be strided.
  // <br>An exception is thrown if the slicer exceeds the image shape.
  // <group>
  SubImage (const ImageInterface<T>& image, const Slicer& slicer,
	    AxesSpecifier=AxesSpecifier(), bool preserveAxesOrder=false);
  SubImage (ImageInterface<T>& image, const Slicer& slicer,
	    bool writableIfPossible,
	    AxesSpecifier=AxesSpecifier(), bool preserveAxesOrder=false);
  // </group>
  
  // Copy constructor (reference semantics).
  SubImage (const SubImage<T>& other);
    
  virtual ~SubImage();

  // Assignment (reference semantics).
  SubImage<T>& operator= (const SubImage<T>& other);

  // Make a copy of the object (reference semantics).
  // <group>
  virtual ImageInterface<T>* cloneII() const;
  // </group>

  // Get the image type (returns name of derived class).
  virtual String imageType() const;

  // Is the SubImage masked?
  // It is if its parent image or its region is masked.
  virtual bool isMasked() const;

  // Does the image object have a pixelmask?
  // It does if its parent has a pixelmask.
  virtual bool hasPixelMask() const;

  // Get access to the pixelmask in use (thus to the pixelmask of the parent).
  // An exception is thrown if the parent does not have a pixelmask.
  // <group>
  virtual const Lattice<bool>& pixelMask() const;
  virtual Lattice<bool>& pixelMask();
  // </group>

  // A SubImage is persistent if no region is applied to the parent image.
  // That is true if the region has the same shape as the parent image
  // and the region has no mask.
  virtual bool isPersistent() const;

  // Is the SubImage paged to disk?
  virtual bool isPaged() const;

  // Can the lattice data be referenced as an array section?
  virtual bool canReferenceArray() const;

  // Is the SubImage writable?
  virtual bool isWritable() const;

  // Get the region/mask object describing this subImage.
  virtual const LatticeRegion* getRegionPtr() const;

  // Returns the shape of the SubImage including all degenerate axes
  // (i.e. axes with a length of one).
  virtual IPosition shape() const;
  
  // Returns the number of axes in this SubImage. This includes all
  // degenerate axes.
  virtual uint32_t ndim() const;
  
  // Returns the total number of elements in this SubImage.
  virtual size_t nelements() const;
  
  // returns a value of "True" if this instance of Lattice and 'other' have 
  // the same shape, otherwise returns a value of "False".
  virtual bool conform (const Lattice<T>& other) const;
  
  // This function returns the recommended maximum number of pixels to
  // include in the cursor of an iterator.
  virtual uint32_t advisedMaxPixels() const;

  // Get access to the attribute handler (of the parent image).
  // If a handler keyword does not exist yet, it is created if
  // <src>createHandler</src> is set.
  // Otherwise the handler is empty and no groups can be created for it.
  virtual ImageAttrHandler& attrHandler (bool createHandler=false);

  // Get or put a single element in the lattice.
  // <group>
  virtual T getAt (const IPosition& where) const;
  virtual void putAt (const T& value, const IPosition& where);
  // </group>
  
  // Function which changes the shape of the SubImage.
  // Throws an exception as resizing a SubImage is not possible.
  virtual void resize(const TiledShape& newShape);

  // Return the name of the parent ImageInterface object. 
  virtual String name (bool stripPath=false) const;
  
  // Check class invariants.
  virtual bool ok() const;

  // Do the actual getting of an array of values.
  virtual bool doGetSlice (Array<T>& buffer, const Slicer& section);

  // Do the actual getting of an array of values.
  virtual void doPutSlice (const Array<T>& sourceBuffer,
			   const IPosition& where,
			   const IPosition& stride);
  
  // Get a section of the mask.
  virtual bool doGetMaskSlice (Array<bool>& buffer, const Slicer& section);

  // This function is used by the LatticeIterator class to generate an
  // iterator of the correct type for this Lattice. Not recommended
  // for general use. 
  virtual LatticeIterInterface<T>* makeIter
                               (const LatticeNavigator& navigator,
				bool useRef) const;

  // Get the best cursor shape.
  virtual IPosition doNiceCursorShape (uint32_t maxPixels) const;

  // Handle the (un)locking and syncing, etc.
  // <group>
  virtual bool lock (FileLocker::LockType, uint32_t nattempts);
  virtual void unlock();
  virtual bool hasLock (FileLocker::LockType) const;
  virtual void resync();
  virtual void flush();
  virtual void tempClose();
  virtual void reopen();
  // </group>

private:
  // Set the coordinates.
  // It removes world axes if the subimage has axes removed.
  // <br>If preserveAxesOrder is true and axes are dropped, it will preserve
  // the order of the axes as well as the order of the coordinates.
  void setCoords (const CoordinateSystem& coords, bool preserveAxesOrder);
  void setCoords (const CoordinateSystem& coords);

  // Set the other members to the one in itsImagePtr.
  void setMembers();

  // Set the members to the subset (in particular, the beamset).
  void setMembers (const Slicer& slicer);

  // Helper
   void convertIPosition(Vector<float>& x, const IPosition& pos) const;


  //# itsImagePtr points to the parent image.
  ImageInterface<T>* itsImagePtr;
  SubLattice<T>*     itsSubLatPtr;

  //# Make members of parent class known.
public:
  using ImageInterface<T>::logger;
protected:
  using ImageInterface<T>::setCoordsMember;
};

//# Declare extern templates for often used types.
  extern template class SubImage<float>;
  extern template class SubImage<Complex>;


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/images/Images/SubImage.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
