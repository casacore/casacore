//# FITSImage.h: provides native access to FITS images
//# Copyright (C) 1994,1995,1996,1997,1999,2000,2001
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

#if !defined(AIPS_FITSIMAGE_H)
#define AIPS_FITSIMAGE_H


//# Includes
#include <trial/Images/ImageInterface.h>
#include <aips/Containers/Record.h>
#include <aips/Utilities/String.h>


//# Forward Declarations
template <class T> class Array;
template <class T> class Lattice;
//
class IPosition;
class Slicer;
class CoordinateSystem;
class FITSMask;
class TiledFileAccess;


// <summary>
// Provides native access to FITS images
// </summary>
//
// <use visibility=export>
//
// <reviewed reviewer="" date="" tests="tFITSImage.cc">
// </reviewed>
//
// <prerequisite>
// <list>
//   <item> ImageInterface
//   <item> FITSMask
// </list>
// </prerequisite>
//
// <etymology>
//  This class provides native access to FITS images.  Only
//  floating point FITS images are presently supported.
// </etymology>
//
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
//
// <example>
// <srcblock>
//    FITSImage im("in.fits"); 
//    LogIO logger(or);
//    ImageStatistics<Float> stats(im, logger);
//    Bool ok = stats.display();                              // Display statistics
// </srcblock>
// </example>
//
// <motivation>
// This provides native access to FITS images.
// </motivation>
//
// <todo asof="2001/02/09">
// </todo>


class FITSImage: public ImageInterface<Float>
{
public: 
  // Construct a FITSImage from the disk FITS file name
  FITSImage(const String& name);

  // Copy constructor (reference semantics)
  FITSImage(const FITSImage& other);

  // Destructor does nothing
  ~FITSImage();

  // Assignment (reference semantics)
  FITSImage& operator=(const FITSImage& other);

  // ImageInterface virtual functions
  
  // Make a copy of the object with new (reference semantics).
  virtual ImageInterface<Float>* cloneII() const;

  // Get the image type (returns FITSImage).
  virtual String imageType() const;

  // Function which changes the shape of the FITSImage.
  // Throws an exception as FITSImage is not writable.
  virtual void resize(const TiledShape& newShape);

  // Functions which get and set the units associated with the image
  // pixels (i.e. the "brightness" unit). Initially the unit is empty.
  // Although the FITSimage is not writable, you can change the
  // unit in the FITSImage object, but it will not be changed 
  // in the FITS disk file.
  // <group>   
  virtual Bool setUnits(const Unit& newUnits);
  virtual Unit units() const;
  // </group>

  // Often we have miscellaneous information we want to attach to an image.
  // Although FITSImage is not writable, you can set a new
  // MiscInfo record, but it will not be stored with the FITS file
  // <group>
  virtual const RecordInterface &miscInfo() const;
  virtual Bool setMiscInfo(const RecordInterface &newInfo);
  // </group>

  // MaskedLattice virtual functions

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

 
  // Lattice virtual functions

  // Do the actual get of the data.  Returns False as the data do not reference another Array
  virtual Bool doGetSlice (Array<Float>& buffer, const Slicer& theSlice);

  // The FITSImage is not writable, so this throws an exception.
  virtual void doPutSlice (const Array<Float>& sourceBuffer,
			   const IPosition& where,
			   const IPosition& stride);

  // LatticeBase virtual functions

  // Returns False, as the FITSImage is not writable.
   virtual Bool isWritable() const;

  // Returns the name of the disk file.
  virtual String name (Bool stripPath=False) const;
  
  // return the shape of the FITSImage
  virtual IPosition shape() const;

  // Check class invariants.
  virtual Bool ok() const;
  
private:  

  String name_p;
  Unit unit_p;
  Record rec_p;
  IPosition tileShape_p;
  CountedPtr<TiledFileAccess> pTiledFile_p;
  Lattice<Bool>* pPixelMask_p;

// Fish things out of the FITS file
   void getImageAttributes (CoordinateSystem& cSys,
                            IPosition& shape, ImageInfo& info,
                            Unit& brightnessUnit, Record& miscInfo, 
                            Int& recsize, Int& recno, const String& name);

 // The default constructor
  FITSImage();
};


#endif
