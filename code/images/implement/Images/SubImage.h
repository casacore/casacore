//# SubImage.h: A (masked) subset of an ImageInterface object
//# Copyright (C) 1998
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

#if !defined(AIPS_SUBIMAGE_H)
#define AIPS_SUBIMAGE_H


//# Includes
#include <trial/Images/MaskedImage.h>

//# Forward Declarations
class IPosition;
class ImageRegion;
class Slicer;
template <class T> class SubLattice;
template <class T> class Array;
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
// <list>
//   <item> <linkto class=ImageInterface>ImageInterface</linkto>
//   <item> <linkto class=SubLattice>SubLattice</linkto>
// </list>
// </prerequisite>
//
// <synopsis> 
// Class SubImage has to be used to apply a region or mask to an image.
// Several functions are inherited from SubLattice and not declared
// in this class.
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


template <class T> class SubImage: public MaskedImage<T>
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
  // <group>
  SubImage (const ImageInterface<T>& image);
  SubImage (ImageInterface<T>& image, Bool writableIfPossible);
  // </group>

  // Create a SubImage from the given (Masked)Image and region.
  // <br>An exception is thrown if the image shape used in the region
  // differs from the shape of the image.
  // <group>
  SubImage (const ImageInterface<T>& image, const ImageRegion& region);
  SubImage (ImageInterface<T>& image, const ImageRegion& region,
	    Bool writableIfPossible);
  SubImage (const MaskedImage<T>& image, const ImageRegion& region);
  SubImage (MaskedImage<T>& image, const ImageRegion& region,
	    Bool writableIfPossible);
  // </group>
  
  // Create a SubImage from the given (Masked)Image and slicer.
  // The slicer can be strided.
  // <br>An exception is thrown if the slicer exceeds the image shape.
  // <group>
  SubImage (const ImageInterface<T>& image, const Slicer& slicer);
  SubImage (ImageInterface<T>& image, const Slicer& slicer,
	    Bool writableIfPossible);
  SubImage (const MaskedImage<T>& image, const Slicer& slicer);
  SubImage (MaskedImage<T>& image, const Slicer& slicer,
	    Bool writableIfPossible);
  // </group>
  
  // Copy constructor (reference semantics).
  SubImage (const SubImage<T>& other);
    
  virtual ~SubImage();

  // Assignment (reference semantics).
  SubImage<T>& operator= (const SubImage<T>& other);

  // Make a copy of the object (reference semantics).
  // <group>
  virtual Lattice<T>* clone() const;
  virtual MaskedImage<T>* cloneMI() const;
  // </group>


  // Is the SubImage writable?
  virtual Bool isWritable() const;

  // Is the SubImage really masked?
  // False means that it is only a rectangular box and that it is
  // not needed to look at the mask.
  virtual Bool isMasked() const;

  // Get the region/mask object describing this subImage.
  virtual const LatticeRegion& region() const;

  // Returns the shape of the SubImage including all degenerate axes
  // (i.e. axes with a length of one).
  virtual IPosition shape() const;
  
  // Returns the number of axes in this SubImage. This includes all
  // degenerate axes.
  virtual uInt ndim() const;
  
  // Returns the total number of elements in this SubImage.
  virtual uInt nelements() const;
  
  // returns a value of "True" if this instance of Lattice and 'other' have 
  // the same shape, otherwise returns a value of "False".
  virtual Bool conform (const Lattice<T>& other) const;
  
  // This function returns the recommended maximum number of pixels to
  // include in the cursor of an iterator.
  virtual uInt maxPixels() const;

  // Get or put a single element in the lattice.
  // <group>
  virtual T getAt (const IPosition& where) const;
  virtual void putAt (const T& value, const IPosition& where);
  // </group>
  
  // Function which changes the shape of the SubImage.
  // Throws an exception as resizing a SubImage is not possible.
  virtual void resize(const TiledShape& newShape);

  // Function which get and set the units associated with the image
  // pixels (i.e. the "brightness" unit). 
  // It returns the unit of the parent object.
  // The set function returns False as the units of a SubImage
  // cannot be changed.
  // <group>   
  virtual Bool setUnits(const Unit& newUnits);
  virtual Unit units() const;
  // </group>

  // Return the name of the current ImageInterface object. 
  // Returns a blank string.
  virtual String name(const Bool stripPath=False) const;
  
  // Functions to set or replace the coordinate information.
  // The function returns False as the coordinates of a SubImage
  // cannot be changed.
  virtual Bool setCoordinateInfo(const CoordinateSystem& coords);
  
  // Often we have miscellaneous information we want to attach to an image.
  // It returns the info of the parent object.
  // The set function returns False as the miscInfo of a SubImage
  // cannot be changed.
  // <group>
  virtual const RecordInterface &miscInfo() const;
  virtual Bool setMiscInfo(const RecordInterface &newInfo);
  // </group>
  
  // Check class invariants.
  virtual Bool ok() const;

  // Do the actual get of the mask data.
  virtual Bool doGetMaskSlice (Array<Bool>& buffer, const Slicer& section);

  // Do the actual getting of an array of values.
  virtual Bool doGetSlice (Array<T>& buffer, const Slicer& section);

  // Do the actual getting of an array of values.
  virtual void doPutSlice (const Array<T>& sourceBuffer,
			   const IPosition& where,
			   const IPosition& stride);
  
  // This function is used by the LatticeIterator class to generate an
  // iterator of the correct type for this Lattice. Not recommended
  // for general use. 
  virtual LatticeIterInterface<T>*
                      makeIter (const LatticeNavigator& navigator) const;

  // Get the best cursor shape.
  virtual IPosition doNiceCursorShape (uInt maxPixels) const;

private:
  //# Both pointers point to the same object.
  //# However, itsSubImagePtr is 0 if the parent image was not a SubImage.
  ImageInterface<T>* itsImagePtr;
  SubLattice<T>*     itsSubLatPtr;
};


#endif
