//# FITSImage.h: Class providing native access to FITS images
//# Copyright (C) 2001,2002
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

#ifndef IMAGES_FITSIMAGE_H
#define IMAGES_FITSIMAGE_H


//# Includes
#include <images/Images/ImageInterface.h>
#include <images/Images/MaskSpecifier.h>
#include <tables/Tables/TiledFileAccess.h>
#include <lattices/Lattices/TiledShape.h>
#include <fits/FITS/fits.h>
#include <casa/BasicSL/String.h>
#include <casa/Utilities/DataType.h>

#define WCSLIB_GETWCSTAB

namespace casa { //# NAMESPACE CASA - BEGIN

//# Forward Declarations
template <class T> class Array;
template <class T> class Lattice;
//
class MaskSpecifier;
class IPosition;
class Slicer;
class CoordinateSystem;
class FITSMask;
class FitsInput;


// <summary>
// Class providing native access to FITS images.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tFITSImage.cc">
// </reviewed>

// <prerequisite>
// <list>
//   <item> <linkto class=ImageInterface>ImageInterface</linkto>
//   <item> <linkto class=FITSMask>FITSMask</linkto>
// </list>
// </prerequisite>

// <etymology>
//  This class provides native access to FITS images. 
//  64bit, 32bit floating point, 32 bit and 16bit integer FITS images are 
//  presently supported.
// </etymology>

// <synopsis> 
//  A FITSImage provides native access to FITS images by accessing them
//  with the TiledFileAccess class.  The FITSImage is read only.
//  We could implement a writable FITSImage but putting the mask
//  would lose data values (uses magic blanking) and FITS is really
//  meant as an interchange medium, not an internal format.
//
//  Because FITS uses magic value blanking, the mask is generated
//  on the fly as needed.
// </synopsis> 

// <example>
// <srcblock>
//    FITSImage im("in.fits"); 
//    LogIO logger(or);
//    ImageStatistics<Float> stats(im, logger);
//    Bool ok = stats.display();                              // Display statistics
// </srcblock>
// </example>

// <motivation>
// This provides native access to FITS images.
// </motivation>

//# <todo asof="2001/02/09">
//# </todo>


class FITSImage: public ImageInterface<Float>
{
public: 
  // Construct a FITSImage from the disk FITS file name and apply mask.
  explicit FITSImage(const String& name, uInt whichRep=0);

  // Construct a FITSImage from the disk FITS file name and apply mask or not.
  FITSImage(const String& name, const MaskSpecifier& mask, uInt whichRep=0);

  // Copy constructor (reference semantics)
  FITSImage(const FITSImage& other);

  // Destructor does nothing
  ~FITSImage();

  // Assignment (reference semantics)
  FITSImage& operator=(const FITSImage& other);

  // Function to open a FITS image (new parser)
  static LatticeBase* openFITSImage (const String& name,
				     const MaskSpecifier&);

  // Register the open function.
  static void registerOpenFunction();

  //# ImageInterface virtual functions
  
  // Make a copy of the object with new (reference semantics).
  virtual ImageInterface<Float>* cloneII() const;

  // Get the image type (returns FITSImage).
  virtual String imageType() const;

  // Function which changes the shape of the FITSImage.
  // Throws an exception as FITSImage is not writable.
  virtual void resize(const TiledShape& newShape);

  //# MaskedLattice virtual functions

  // Has the object really a mask?  The FITSImage always
  // has a pixel mask and never has a region mask so this
  // always returns True
  virtual Bool isMasked() const;

  // FITSimage always has a pixel mask so returns True
  virtual Bool hasPixelMask() const;

  // Get access to the pixelmask.  FITSImage always has a pixel mask.
  // <group>
  virtual const Lattice<Bool>& pixelMask() const;
  virtual Lattice<Bool>& pixelMask();
  // </group>

  // Do the actual get of the mask data.   The return value is always 
  // False, thus the buffer does not reference another array.
  virtual Bool doGetMaskSlice (Array<Bool>& buffer, const Slicer& section);

  // Get the region used.  There is no region. 
  // Always returns 0.
  virtual const LatticeRegion* getRegionPtr() const;

 
  //# Lattice virtual functions

  // Do the actual get of the data.
  // Returns False as the data do not reference another Array
  virtual Bool doGetSlice (Array<Float>& buffer, const Slicer& theSlice);

  // The FITSImage is not writable, so this throws an exception.
  virtual void doPutSlice (const Array<Float>& sourceBuffer,
			   const IPosition& where,
			   const IPosition& stride);

  //# LatticeBase virtual functions

  // The lattice is paged to disk.
  virtual Bool isPaged() const;

  // The lattice is persistent.
  virtual Bool isPersistent() const;

  // The FITSImage is not writable.
  virtual Bool isWritable() const;

  // Returns the name of the disk file.
  virtual String name (Bool stripPath=False) const;
  
  // return the shape of the FITSImage
  virtual IPosition shape() const;

  // Returns the maximum recommended number of pixels for a cursor. This is
  // the number of pixels in a tile. 
  virtual uInt advisedMaxPixels() const;

  // Help the user pick a cursor for most efficient access if they only want
  // pixel values and don't care about the order or dimension of the
  // cursor. 
  virtual IPosition doNiceCursorShape (uInt maxPixels) const;

  // Temporarily close the image.
  virtual void tempClose();

  // Reopen a temporarily closed image.
  virtual void reopen();

  // Check class invariants.
  virtual Bool ok() const;

  // Return the (internal) data type (TpFloat or TpShort).
  DataType dataType () const
    { return dataType_p; }

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

private:  
  String         name_p;
  MaskSpecifier  maskSpec_p;
  CountedPtr<TiledFileAccess> pTiledFile_p;
  Lattice<Bool>* pPixelMask_p;
  TiledShape     shape_p;
  Float          scale_p;
  Float          offset_p;
  Short          shortMagic_p;
  Int            longMagic_p;
  Bool           hasBlanks_p;
  DataType       dataType_p;
  Int64          fileOffset_p;
  Bool           isClosed_p;
  uInt           whichRep_p;

// Reopen the image if needed.
   void reopenIfNeeded() const
     { if (isClosed_p) const_cast<FITSImage*>(this)->reopen(); }

// Setup the object (used by constructors).
   void setup();

// Open the image (used by setup and reopen).
   void open();

// Fish things out of the FITS file
   void getImageAttributes (CoordinateSystem& cSys,
                            IPosition& shape, ImageInfo& info,
                            Unit& brightnessUnit, RecordInterface& miscInfo, 
                            Int& recsize, Int& recno,
			    FITS::ValueType& dataType, 
                            Float& scale, Float& offset, Short& shortMagic, 
                            Int& longMagic, Bool& hasBlanks, const String& name,
                            uInt whichRep);

// Crack the header
   template <typename T>
   void crackHeader (CoordinateSystem& cSys, IPosition& shape, ImageInfo& imageInfo,
                     Unit& brightnessUnit, RecordInterface& miscInfo,
                     Float& scale, Float& offset, Short& magicShort,
                     Int& magicLong, Bool& hasBlanks, LogIO& os, FitsInput& infile,
                     uInt whichRep);
		     
};



} //# NAMESPACE CASA - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <images/Images/FITS2Image.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES

#endif


