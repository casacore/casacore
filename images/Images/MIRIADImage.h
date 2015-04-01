//# MIRIADImage.h: Class providing native access to MIRIAD images
//# Copyright (C) 2001
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

#ifndef IMAGES_MIRIADIMAGE_H
#define IMAGES_MIRIADIMAGE_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/images/Images/ImageInterface.h>
#include <casacore/images/Images/MaskSpecifier.h>
#include <casacore/tables/DataMan/TiledFileAccess.h>
#include <casacore/lattices/Lattices/TiledShape.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/DataType.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

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
// Class providing native access to MIRIAD images.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tMIRIADImage.cc">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=ImageInterface>ImageInterface</linkto>
//   <li> <linkto class=FITSMask>FITSMask</linkto>
// </prerequisite>

// <etymology>
//  This class provides native access to MIRIAD images. 
// </etymology>

// <synopsis> 
//  A MIRIADImage provides native access to MIRIAD images by accessing them
//  with the TiledFileAccess class.  -- or -- the native miriad I/O routines.
//  The MIRIADImage is read only. -- really -- ??
//
// </synopsis> 

// <example>
// <srcblock>
//    MIRIADImage im("cube1"); 
//    LogIO logger(or);
//    ImageStatistics<Float> stats(im, logger);
//    Bool ok = stats.display();                              // Display statistics
// </srcblock>
// </example>

// <motivation>
// This provides native access to MIRIAD images.
// </motivation>

//# <todo asof="2001/09/10">
//# </todo>


class MIRIADImage: public ImageInterface<Float>
{
public: 
  // Construct a MIRIADImage from the disk MIRIAD dataset name and apply mask.
  explicit MIRIADImage(const String& name);

  // Construct a MIRIADImage from the disk MIRIAD file name and apply mask or not.
  MIRIADImage(const String& name, const MaskSpecifier&);

  // Copy constructor (reference semantics)
  MIRIADImage(const MIRIADImage& other);

  // Destructor does nothing
  ~MIRIADImage();

  // Assignment (reference semantics)
  MIRIADImage& operator=(const MIRIADImage& other);

  // Function to open a MIRIAD image.
  static LatticeBase* openMIRIADImage (const String& name,
				       const MaskSpecifier&);

  // Register the open function.
  static void registerOpenFunction();

  //# ImageInterface virtual functions
  
  // Make a copy of the object with new (reference semantics).
  virtual ImageInterface<Float>* cloneII() const;

  // Get the image type (returns MIRIADImage).
  virtual String imageType() const;

  // Function which changes the shape of the MIRIADImage.
  // Throws an exception as MIRIADImage is not writable.
  virtual void resize(const TiledShape& newShape);

  // Functions which get and set the units associated with the image
  // pixels (i.e. the "brightness" unit). Initially the unit is empty.
  // Although the MIRIADimage is not writable, you can change the
  // unit in the MIRIADImage object, but it will not be changed 
  // in the MIRIAD disk file.
  // <group>   
#if 0
  virtual Bool setUnits(const Unit& newUnits);
  virtual Unit units() const;
#endif
  // </group>

  // Often we have miscellaneous information we want to attach to an image.
  // Although MIRIADImage is not writable, you can set a new
  // MiscInfo record, but it will not be stored with the MIRIAD file
  // <group>
  virtual const RecordInterface &miscInfo() const;
  virtual Bool setMiscInfo(const RecordInterface &newInfo);
  // </group>

  //# MaskedLattice virtual functions

  // Has the object really a mask?  The MIRIADImage always
  // has a pixel mask and never has a region mask so this
  // should always return True
  virtual Bool isMasked() const;

  // MIRIADimage always has a pixel mask so should return True
  virtual Bool hasPixelMask() const;

  // Get access to the pixelmask.  MIRIADImage always has a pixel mask.
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

  // The MIRIADImage is not writable, so this throws an exception.
  virtual void doPutSlice (const Array<Float>& sourceBuffer,
			   const IPosition& where,
			   const IPosition& stride);

  //# LatticeBase virtual functions

  // The lattice is paged to disk.
  virtual Bool isPaged() const;

  // The lattice is persistent.
  virtual Bool isPersistent() const;

  // The MIRIADImage is not writable.
  virtual Bool isWritable() const;

  // Returns the name of the disk file.
  virtual String name (Bool stripPath=False) const;
  
  // return the shape of the MIRIADImage
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
  String         name_p;                          // filename, as given
  Int            tno_p;                           // miriad file handle
  MaskSpecifier  maskSpec_p;
  Unit           unit_p;
  Record         rec_p;
  CountedPtr<TiledFileAccess> pTiledFile_p;
  Lattice<Bool>* pPixelMask_p;
  //  Float          scale_p;
  //  Float          offset_p;
  //  Short          magic_p;
  TiledShape     shape_p;
  Bool           hasBlanks_p;
  DataType       dataType_p;                      // always float's for miriad
  Int64          fileOffset_p;                    // always 4 for direct (tiled) access
  Bool           isClosed_p;

// Reopen the image if needed.
   void reopenIfNeeded() const
     { if (isClosed_p) const_cast<MIRIADImage*>(this)->reopen(); }

// Setup the object (used by constructors).
   void setup();

// Open the image (used by setup and reopen).
   void open();

// Fish things out of the MIRIAD file
   void getImageAttributes (CoordinateSystem& cSys,
                            IPosition& shape, ImageInfo& info,
                            Unit& brightnessUnit, Record& miscInfo, 
                            Bool& hasBlanks, const String& name);

// <group>
   void crackHeader (CoordinateSystem& cSys,
                     IPosition& shape, ImageInfo& imageInfo,
                     Unit& brightnessUnit, Record& miscInfo,
                     LogIO&os);

// </group>
};



} //# NAMESPACE CASACORE - END

#endif
