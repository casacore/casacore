//# FITSQualityImage.h: Class providing native access to FITS images
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

#ifndef IMAGES_FITSQUALITYIMAGE_H
#define IMAGES_FITSQUALITYIMAGE_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/images/Images/FITSErrorImage.h>
#include <casacore/images/Images/ImageInterface.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
template <class T> class Array;
template <class T> class Lattice;
//
class FITSImage;
class FITSQualityMask;
class IPosition;
class Slicer;

// <summary>
// Class providing native access to FITS Quality Images.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tFITSQualityImage.cc">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=FITSImage>FITSImage</linkto>
//   <li> <linkto class=FITSErrorImage>FITSErrorImage</linkto>
// </prerequisite>

// <etymology>
// The class provides access to a quality image via two extensions
// in the corresponding FITS file.
// </etymology>

// <synopsis>
//  A FITSQualityImage provides native access to FITS images by accessing
//  the data and the error values via the classes FITSImage and
//  FITSErrorImage, respectively. A QualityCoordinate connects these
//  two layers. The FITSQualityImage is read only.
// </synopsis>

// <example>
// <srcblock>
//	   FITSQualityImage fitsQIStat("im.fits", 1, 2);
//	   LogIO logger(or);
//	   ImageStatistics<Float> stats(fitsQIStat, logger);
//	   Bool ok = stats.display();
//    </srcblock>
// </example>

// <motivation>
// This provides access to FITS Quality Images
// </motivation>

//# <todo asof="2011/06/17">
//# </todo>

class FITSQualityImage: public ImageInterface<Float>
{
public: 
  // Construct a FITSQualityImage from the FITS file name and extensions
  // specified in the input.
  explicit FITSQualityImage(const String& name);

  // Construct a FITSQualityImage from the disk FITS file name and extensions.
  explicit FITSQualityImage(const String& name, uInt whichDataHDU, uInt whichErrorHDU);

  // Copy constructor (reference semantics)
  FITSQualityImage(const FITSQualityImage& other);

  // Destructor
  ~FITSQualityImage();

  // Assignment (reference semantics).
  FITSQualityImage& operator=(const FITSQualityImage& other);

  //# ImageInterface virtual functions
  
  // Make a copy of the object with new (reference semantics).
  virtual ImageInterface<Float>* cloneII() const;

  // Given the misc-info of a CASA image (with quality-axis)
  // the misc-info of the data sub-image and the error sub-image
  // are produced. This ensures that, if written to FITS, the
  // data and error extensions have the all necessary keywords.
  Bool static qualFITSInfo(String &error, TableRecord &dataExtMiscInfo, TableRecord &errorExtMiscInfo,
		  const TableRecord &miscInfo);

  // Get the FITS data
  FITSImage      *fitsData() const {return fitsdata_p;};

  // Get the FITS error
  FITSErrorImage *fitsError() const {return fitserror_p;};

  // Get the image type (returns FITSImage).
  virtual String imageType() const;

  // Function which changes the shape of the FITSQualityImage.
  // Throws an exception as FITSQualityImage is not writable.
  virtual void resize(const TiledShape& newShape);

  // Has the object really a mask?  The FITSQualityImage always
  // has a pixel mask and never has a region mask so this
  // always returns True
  virtual Bool isMasked() const;

  // FITSQualityImage always has a pixel mask so returns True
  virtual Bool hasPixelMask() const;

  // Get access to the pixelmask.  FITSQualityImage always has a pixel mask.
  // <group>
  virtual const Lattice<Bool>& pixelMask() const;
  virtual Lattice<Bool>& pixelMask();
  // </group>


  // Get the region used.  There is no region. 
  // Always returns 0.
  virtual const LatticeRegion* getRegionPtr() const;
 
  // Do the actual get of the data.
  // Returns False as the data do not reference another Array
  virtual Bool doGetSlice (Array<Float>& buffer, const Slicer& theSlice);

  // The FITSQualityImage is not writable, so this throws an exception.
  virtual void doPutSlice (const Array<Float>& sourceBuffer,
			   const IPosition& where,
			   const IPosition& stride);

  // Do the actual get of the mask data.   The return value is always
  // False, thus the buffer does not reference another array.
  virtual Bool doGetMaskSlice (Array<Bool>& buffer, const Slicer& section);

  //# LatticeBase virtual functions

  // The lattice is paged to disk.
  virtual Bool isPaged() const;

  // The lattice is persistent.
  virtual Bool isPersistent() const;

  // The FITSImage is not writable.
  virtual Bool isWritable() const;

  // Returns the name of the disk file.
  virtual String name (Bool stripPath=False) const;
  
  // Return the shape of the FITSImage.
  virtual IPosition shape() const;

  // Returns the maximum recommended number of pixels for a cursor. This is
  // the number of pixels in a tile. 
  virtual uInt advisedMaxPixels() const;

  // Help the user pick a cursor for most efficient access if they only want
  // pixel values and don't care about the order or dimension of the
  // cursor. 
  virtual IPosition doNiceCursorShape (uInt maxPixels) const;

  // Check class invariants.
  virtual Bool ok() const;

  // Temporarily close the image.
  virtual void tempClose();
  virtual void tempCloseData();
  virtual void tempCloseError();

  // Reopen a temporarily closed image.
  virtual void reopen();

  // Return the (internal) data type (TpFloat or TpShort).
  DataType dataType () const;

  // Return the data HDU number
  uInt whichDataHDU () const
    { return whichDataHDU_p; }

  // Return the error HDU number
  uInt whichErrorHDU () const
    { return whichErrorHDU_p; }

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
  String         fullname_p;
  FITSImage      *fitsdata_p;
  FITSErrorImage *fitserror_p;
  Lattice<Bool>  *pPixelMask_p;
  TiledShape     shape_p;
  uInt           whichDataHDU_p;
  uInt           whichErrorHDU_p;
  uInt           whichMaskHDU_p;
  FITSErrorImage::ErrorType errType_p;
  Bool           isClosed_p;
  Bool           isDataClosed_p;
  Bool           isErrorClosed_p;

  // Reopen the image if needed.
  void reopenIfNeeded() const;
  void reopenDataIfNeeded();
  void reopenErrorIfNeeded();

  // Get the extension indices from an
  // extension expression.
  void getExtInfo();

  // Setup the object (used by constructors).
   void setup();

   // Make sure the input is compatible.
   Bool checkInput();
};



} //# NAMESPACE CASACORE - END

#endif


