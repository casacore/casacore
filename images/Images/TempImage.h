//# TempImage.h: Temporary astronomical images
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
//#
//# $Id$

#ifndef IMAGES_TEMPIMAGE_H
#define IMAGES_TEMPIMAGE_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/images/Images/ImageInterface.h>
#include <casacore/lattices/Lattices/TiledShape.h>
#include <casacore/lattices/Lattices/TempLattice.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Temporary astronomical images.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tTempImage.cc" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=CoordinateSystem>CoordinateSystem</linkto>
//   <li> <linkto class=ImageInterface>ImageInterface</linkto>
//   <li> <linkto class=TempLattice>TempLattice</linkto>
// </prerequisite>

// <etymology>
// The TempImage name comes from its role as the Image class for temporary
// storage.
// </etymology>

// <synopsis> 
// The class <src>TempImage</src> is useful for storing temporary images
// for which it is not known whether they can be held in memory.
// It uses class <linkto class=TempLattice>TempLattice</linkto> to
// hold the image in memory when it is small enough. Otherwise it is
// held in a temporary file. Similarly to <src>TempLattice</src>
// one can give the maximum memory to use to control when the image
// can be held in memory.
// <br>
// The other Image information like coordinates, units, and miscinfo
// is held in member variables and disappears when the TempImage object
// is destructed.
// <p>
// It is possibly to temporarily close a TempImage, which only takes effect
// when it is created as a PagedArray. In this way it is possible to reduce
// the number of open files in case a lot of TempImage objects are used.
// A temporarily closed TempImage will be reopened automatically when needed.
// It can also be reopened explicitly.
// </synopsis> 

// <example>
// <srcblock>
// </srcblock>
// </example>

// <motivation>
// The size of astronomical data can be very large.  The ability to fit an 
// entire image into random access memory cannot be guaranteed.  Paging from 
// disk pieces of the image appeared to be the way to deal with this problem.
// </motivation>

//# <todo asof="1998/10/27">
//#  <li> Maybe move applyMask, maskPtr_p, etc to base class ImageInterface
//# </todo>


template<class T> class TempImage: public ImageInterface<T>
{
public: 
  // The default constructor creates an empty image.
  TempImage();

  // Construct a temporary Image from shape and coordinate information.
  // If the image is sufficiently small, it is kept in memory.
  // Otherwise it is kept in a temporary disk table. It can
  // be forced to disk by setting maxMemoryinMB=0.
  // The algorithm is the same as in class
  // <linkto class=TempLattice>TempLattice</linkto>.
  TempImage (const TiledShape& mapShape,
	     const CoordinateSystem& coordinateInfo,
	     Int maxMemoryInMB=-1);

  TempImage (const TiledShape& mapShape,
	     const CoordinateSystem& coordinateInfo,
	     Double maxMemoryInMB);

  // Copy constructor (reference semantics).
  TempImage (const TempImage<T>& other);

  // Destructor
  ~TempImage();

  // Assignment operator (reference semantics).
  TempImage<T>& operator= (const TempImage<T>& other);

  // Make a copy of the object (reference semantics).
  virtual ImageInterface<T>* cloneII() const;

  // Get the image type (returns name of derived class).
  virtual String imageType() const;

  // Is the TempImage paged to disk?
  virtual Bool isPaged() const;

  // Can the lattice data be referenced as an array section?
  virtual Bool canReferenceArray() const;

  // Is the TempImage writable?
  virtual Bool isWritable() const;

  // Set the default pixelmask to the mask with the given name
  // (which has to exist in the "masks" group).
  // If the image table is writable, the setting is persistent by writing
  // the name as a keyword.
  // If the given regionName is the empty string,
  // the default pixelmask is unset.
  virtual void setDefaultMask (const String& maskName);

  // Delete the pixel mask attached to the TempImage.
  // Does nothing if there isn't one
  void removeMask()
    { setDefaultMask (""); }

  // Use the mask as specified.
  // If a mask was already in use, it is replaced by the new one.
  virtual void useMask (MaskSpecifier = MaskSpecifier());

  // Remove a region/mask belonging to the image from the given group
  // (which can be Any).
  // If a mask removed is the default mask, the image gets unmasked.
  // <br>Optionally an exception is thrown if the region does not exist.
  virtual void removeRegion (const String& name,
			     RegionHandler::GroupType = RegionHandler::Any,
			     Bool throwIfUnknown = True);

  // Attach a mask to the TempImage.
  // It replaces a probably already attached mask.
  // It has to have the same shape as the image.
  virtual void attachMask (const Lattice<Bool>& mask);

  // It a mask attached to the image?
  virtual Bool isMasked() const;

  // Does the image object use a pixelmask?
  // This is similar to <src>isMasked()</src>.
  virtual Bool hasPixelMask() const;

  // Get access to the pixelmask used.
  // An exception is thrown if the image does not use a pixelmask.
  // <group>
  virtual const Lattice<Bool>& pixelMask() const;
  virtual Lattice<Bool>& pixelMask();
  // </group>

  // Get a section of the mask.
  // It throws an exception if there is no mask.
  virtual Bool doGetMaskSlice (Array<Bool>& buffer, const Slicer& section);

  // Flush the data.
  virtual void flush();

  // Close the TempImage temporarily (if it is paged to disk).
  // Note that a possible mask is not closed.
  // It'll be reopened automatically when needed or when
  // <src>reopen</src> is called explicitly.
  virtual void tempClose();

  // If needed, reopen a temporarily closed TempLattice.
  virtual void reopen();

  // Function which changes the shape of the image (N.B. the data is thrown 
  // away - the Image will be filled with nonsense afterwards)
  virtual void resize (const TiledShape& newShape);
  
  // Return the name of the current TempImage object.
  // It is always "Temporary_Image"
  virtual String name (Bool stripPath=False) const;

  // Return the shape of the image
  virtual IPosition shape() const;

  // Function which sets all of the elements in the Lattice to a value.
  virtual void set (const T& value);

  // Replace every element, x, of the lattice with the result of f(x).
  // You must pass in the address of the function -- so the function
  // must be declared and defined in the scope of your program.  
  // Both versions of apply require a function that accepts a single 
  // argument of type T (the Lattice template actual type) and returns
  // a result of the same type.  The first apply expects a function with
  // an argument passed by value; the second expects the argument to
  // be passed by const reference.  The first form ought to run faster
  // for the built-in types, which may be an issue for large images
  // stored in memory, where disk access is not an issue.
  // <group>
  virtual void apply (T (*function)(T));
  virtual void apply (T (*function)(const T&));
  virtual void apply (const Functional<T,T>& function);
  // </group>
    
  // Get or put a single pixel.
  // Note that the function operator () can also be used to get a pixel.
  // <group>
  virtual T getAt (const IPosition& where) const;
  virtual void putAt (const T& value, const IPosition& where);
  // </group>

  // This is the implementations of the letters for the envelope Iterator
  // class <note> Not for public use </note>
  virtual LatticeIterInterface<T>* makeIter
                           (const LatticeNavigator& navigator,
			    Bool useRef) const;

  // Returns the maximum recommended number of pixels for a cursor.
  // This is the number of pixels in a tile. 
  virtual uInt advisedMaxPixels() const;

  // Help the user pick a cursor for most efficient access.
  virtual IPosition doNiceCursorShape (uInt maxPixels) const;

  // Maximum size - not necessarily all used. In pixels.
  virtual uInt maximumCacheSize() const;

  // Set the maximum (allowed) cache size as indicated.
  virtual void setMaximumCacheSize (uInt howManyPixels);

  // Set the cache size as to "fit" the indicated path.
  virtual void setCacheSizeFromPath (const IPosition& sliceShape,
  			             const IPosition& windowStart,
			             const IPosition& windowLength,
			             const IPosition& axisPath);
    
  // Set the actual cache size for this Array to be be big enough for the
  // indicated number of tiles. This cache is not shared with PagedArrays
  // in other rows and is always clipped to be less than the maximum value
  // set using the setMaximumCacheSize member function.
  // tiles. Tiles are cached using a first in first out algorithm. 
  virtual void setCacheSizeInTiles (uInt howManyTiles);

  // Clears and frees up the caches, but the maximum allowed cache size is 
  // unchanged from when setCacheSize was called
  virtual void clearCache();

  // Report on cache success.
  virtual void showCacheStatistics (ostream& os) const;

  // Check for symmetry in data members.
  virtual Bool ok() const;

protected:
  // Get the region used (it always returns 0).
  virtual const LatticeRegion* getRegionPtr() const;

  // Function which extracts an array from the map.
  virtual Bool doGetSlice (Array<T>& buffer, const Slicer& theSlice);
  
  // Function to replace the values in the map with soureBuffer.
  virtual void doPutSlice (const Array<T>& sourceBuffer,
			   const IPosition& where,
			   const IPosition& stride);


private:  
  void applyMaskSpecifier (const MaskSpecifier&);
  void applyMask (const String& maskName);

  TempLattice<T>* mapPtr_p;
  Lattice<Bool>*  maskPtr_p;

  //# Make members of parent class known.
public:
  using ImageInterface<T>::logger;
  using ImageInterface<T>::coordinates;
  using ImageInterface<T>::getDefaultMask;
  using ImageInterface<T>::hasRegion;
  using ImageInterface<T>::getImageRegionPtr;
  using ImageInterface<T>::setCoordinateInfo;
protected:
  using ImageInterface<T>::setCoordsMember;
};



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/images/Images/TempImage.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
