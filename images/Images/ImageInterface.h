//# ImageInterface.h: a base class for astronomical images
//# Copyright (C) 1996,1997,1998,1999,2000,2001
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

#ifndef IMAGES_IMAGEINTERFACE_H
#define IMAGES_IMAGEINTERFACE_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/images/Regions/RegionHandler.h>
#include <casacore/images/Images/MaskSpecifier.h>
#include <casacore/images/Images/ImageInfo.h>
#include <casacore/images/Images/ImageAttrHandler.h>
#include <casacore/lattices/Lattices/MaskedLattice.h>
#include <casacore/coordinates/Coordinates/CoordinateSystem.h>
#include <casacore/tables/LogTables/LoggerHolder.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Quanta/Unit.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
template <class T> class LatticeIterInterface;
template <class T> class Vector;
template <class T> class COWPtr;
class ImageRegion;
class IPosition;
class TiledShape;


// <summary>
// A base class for astronomical images.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Lattice>Lattices</linkto>
//   <li> <linkto class=CoordinateSystem>CoordinateSystem</linkto>
// </prerequisite>

// <etymology>
// The ImageInterface class name is derived from its role as the cookie cutter
// Interface base class for Images.  
// </etymology>

// <synopsis> 
// The ImageInterface class is an abstract base class. All Image classes
// should derive from this class to ensure functions which operate on Images
// will work for all Image derivations.
// 
// An Image is currently defined as an Array of pixels, a Boolean mask,
// defining which pixels are valid and coordinates to define the reference
// frame. The only concrete class currently derived from this Interface is
// PagedImage, which allows the image to be stored on disk, and only reads
// specified portions of the image into memory.  
// </synopsis>

// <example>
// As this is an abstract base class it is not possible to construct an
// instance of this object.  It can however be used as a function argument.<br>
// eg 1. (used in dImageInterface.cc)
// <srcblock>
// Float sumPixels(const ImageInterface<Float>& image){
//   uInt rowLength = image.shape()(0);
//   IPosition rowShape(image.ndim());
//   rowShape = 1; rowShape(0) = rowLength;
//   Float sumPix = 0;
//   RO_LatticeIterator<Float> iter(image, rowShape);
//   while(!iter.atEnd()){
//     sumPix += sum(iter.vectorCursor());
//     iter++;
//   }
//   return sumPix;
// }
// </srcblock>
//
// The main purpose of this class is for programming objects, the following
// example is of how one would derive from ImageInterface: <br>
// eg 2.
// <srcblock>
// template <class T> class myNewImage : public ImageInterface<T>
// {
// public:
//   // default constructor
//   myNewImage();
//
//   // argumented constructor
//   myNewImage(...);
//
//   // destructor
//   ~myNewImage
//   
//   // the shape function is forced upon us by the Lattice base class
//   IPosition shape() const;
//   
//   // doGetSlice is another function required of all Lattice objects.
//   Bool doGetSlice(<Array<T>& buffer, const Slicer& section);
//
//  // etc...
// private:
//  // put the actual map data down here.
//  // etc...
// };
// </srcblock>
// </example>

// <motivation> 
// The use of abstract base classes to guide inheritance seemed appropriate
// for Images to ensure that CoordinateSystems and masking get handled
// uniformly.
// </motivation>

// <todo asof="1995/04/25">
//   <li> replace ImageCoordinates
// </todo>


template <class T> class ImageInterface: public MaskedLattice<T>
{
  //# Make members of parent class known.
public:
  using MaskedLattice<T>::shape;

public: 
  ImageInterface();

  // Construct for a specific region handler object.
  ImageInterface (const RegionHandler& regionHandler);

  // Copy constructor (copy semantics).
  ImageInterface (const ImageInterface& other);

  virtual ~ImageInterface();

  // Make a copy of the derived object (reference semantics).
  // <group>
  virtual MaskedLattice<T>* cloneML() const;
  virtual ImageInterface<T>* cloneII() const = 0;
  // </group>

  // Get the image type (returns name of derived class).
  virtual String imageType() const = 0;

  // Function which changes the shape of the image (N.B. the data is thrown 
  // away - the Image will be filled with nonsense afterwards)
  virtual void resize (const TiledShape& newShape) = 0;
  
  // Function which get and set the units associated with the image
  // pixels (i.e. the "brightness" unit). <src>setUnits()</src> returns
  // False if it cannot set the unit for some reason (e.g. the underlying
  // file is not writable).
  // <group>
  virtual Bool setUnits (const Unit& newUnits);
  virtual const Unit& units() const
    { return unit_p; }
  // </group>

  // Return the name of the current ImageInterface object. This will generally 
  // be a file name for images that have a persistent form.  Any path
  // before the actual file name can be optionally stripped off.
  virtual String name (Bool stripPath=False) const = 0;

  // Functions to set or replace the coordinate information in the Image
  // Returns False on failure, e.g. if the number of axes do not match.
  // <group>
  virtual Bool setCoordinateInfo (const CoordinateSystem& coords);
  const CoordinateSystem& coordinates() const
    { return coords_p; }
  // </group>

  // Function to get a LELCoordinate object containing the coordinates.
  virtual LELCoordinates lelCoordinates() const;

  // Get access to the LoggerHolder.
  // <group>
  LoggerHolder& logger()
    { return log_p; }
  const LoggerHolder& logger() const
    { return log_p; }
  // </group>

  // Allow messages to be logged to this ImageInterface.
  // <group>
  LogIO& logSink()
    { return logger().logio(); }
  const LogIO& logSink() const
    { return const_cast<ImageInterface<T>*>(this)->logSink(); }
  // </group>
  
  // Add the messages from the other image logger to this one.
  void appendLog (const LoggerHolder& other)
    { log_p.append (other); }

  // Often we have miscellaneous information we want to attach to an image.
  // This is where it goes.  
  // <br>
  // Note that setMiscInfo REPLACES the information with the new information.
  // It can fail if, e.g., the underlying table is not writable.
  // <group>
  const TableRecord& miscInfo() const
    { return miscInfo_p; }
  virtual Bool setMiscInfo (const RecordInterface& newInfo);
  // </group>

  // The ImageInfo object contains some miscellaneous information about the image
  // which unlike that stored in MiscInfo, has a standard list of things,
  // such as the restoring beam.
  //
  // Note that setImageInfo REPLACES the information with the new information.
  // It is up to the derived class to make the ImageInfo permanent.
  // <group>
  const ImageInfo& imageInfo() const
    { return imageInfo_p; }
  virtual Bool setImageInfo (const ImageInfo& info);
  // </group>

  // Get access to the attribute handler.
  // By default an empty handler is returned where no groups can be added to.
  // <group>
  virtual ImageAttrHandler& attrHandler (Bool createHandler=False);
  ImageAttrHandler& roAttrHandler() const
    { return const_cast<ImageInterface<T>*>(this)->attrHandler(False); }
  // </group>

  // Can the image handle region definition?
  Bool canDefineRegion() const
    { return regHandPtr_p->canDefineRegion(); }

  // Make a mask which is suitable for the type of image.
  // Optionally the mask can be initialized with the given value
  // (by default it will not).
  // <br>Optionally the mask can be defined as an image region/mask
  // and turned in the default mask for the image. By default it will.
  virtual ImageRegion makeMask (const String& name,
				Bool defineAsRegion = True,
				Bool setAsDefaultMask = True,
				Bool initialize = False,
				Bool value = True);

  // Define a region/mask belonging to the image.
  // The group type determines if it stored as a region or mask.
  // If overwrite=False, an exception will be thrown if the region
  // already exists.
  // <br>An exception is thrown if canDefineRegion is False.
  virtual void defineRegion (const String& name, const ImageRegion& region,
			     RegionHandler::GroupType,
			     Bool overwrite = False);

  // Does the image have a region with the given name?
  virtual Bool hasRegion (const String& regionName,
			  RegionHandler::GroupType = RegionHandler::Any) const;

  // Get a region/mask belonging to the image from the given group
  // (which can be Any).
  // <br>Optionally an exception is thrown if the region does not exist.
  // A zero pointer is returned if the region does not exist.
  // The caller has to delete the <src>ImageRegion</src> object created.
  virtual ImageRegion* getImageRegionPtr
                            (const String& name,
			     RegionHandler::GroupType = RegionHandler::Any,
			     Bool throwIfUnknown = True) const;

  // Rename a region.
  // If a region with the new name already exists, it is deleted or
  // an exception is thrown (depending on <src>overwrite</src>).
  // The region name is looked up in the given group(s).
  // <br>An exception is thrown if the old region name does not exist.
  virtual void renameRegion (const String& newName,
			     const String& oldName,
			     RegionHandler::GroupType = RegionHandler::Any,
			     Bool overwrite = False);

  // Remove a region/mask belonging to the image from the given group
  // (which can be Any).
  // <br>Optionally an exception is thrown if the region does not exist.
  virtual void removeRegion (const String& name,
			     RegionHandler::GroupType = RegionHandler::Any,
			     Bool throwIfUnknown = True);

  // Get the names of all regions/masks.
  virtual Vector<String> regionNames
                   (RegionHandler::GroupType = RegionHandler::Any) const;

  // Use the mask as specified.
  // If a mask was already in use, it is replaced by the new one.
  virtual void useMask (MaskSpecifier = MaskSpecifier());

  // Set the default pixelmask to the mask with the given name
  // (which has to exist in the "masks" group).
  // If the image table is writable, the setting is persistent by writing
  // the name as a keyword.
  // If the given regionName is the empty string,
  // the default pixelmask is unset.
  virtual void setDefaultMask (const String& regionName);

  // Get the name of the default pixelmask.
  // An empty string is returned if no default pixelmask.
  virtual String getDefaultMask() const;

  // Get a region belonging to the image.
  // An exception is thrown if the region does not exist.
  ImageRegion getRegion (const String& regionName,
			 RegionHandler::GroupType = RegionHandler::Any) const;

  // Make a unique region name from the given root name, thus make it such
  // that the name is not already in use for a region or mask.
  // The root name is returned if it is already unique.
  // Otherwise a number is appended to the root name to make it unique.
  // The number starts at the given number and is incremented until the name
  // is unique.
  String makeUniqueRegionName (const String& rootName,
			       uInt startNumber = 1) const;

  // Check class invariants. 
  virtual Bool ok() const = 0;

  // Save and restore an ImageInterface object to or from a state Record
  Bool toRecord (String& error, RecordInterface& outRec);
  Bool fromRecord (String& error, const RecordInterface& inRec);
 
protected:
  // Assignment (copy semantics) is only useful for derived classes.
  ImageInterface& operator= (const ImageInterface& other);

  // Restore the image info from the record.
  Bool restoreImageInfo (const RecordInterface& rec);

  // Set the image logger variable.
  void setLogMember (const LoggerHolder& logger)
    { log_p = logger; }

  // Set the image info variable.
  void setImageInfoMember (const ImageInfo& imageInfo);

  // Set the coordinate system variable.
  void setCoordsMember (const CoordinateSystem& coords)
    { coords_p = coords; }

  // Set the unit variable.
  void setUnitMember (const Unit& unit)
    { unit_p = unit; }

  // Set the miscinfo variable.
  void setMiscInfoMember (const RecordInterface& rec)
    { miscInfo_p.assign (rec); }

  // Get access to the region handler.
  RegionHandler* getRegionHandler()
    { return regHandPtr_p; }

  // Get non-const access to the ImageInfo.
  ImageInfo& rwImageInfo()
    { return imageInfo_p; }

private:
  // It is the job of the derived class to make these variables valid.
  CoordinateSystem coords_p;
  LoggerHolder     log_p;
  ImageInfo        imageInfo_p;
  Unit             unit_p;
  TableRecord      miscInfo_p;

  // The region handling object.
  RegionHandler* regHandPtr_p;

  // The attribute handling object.
  ImageAttrHandler itsBaseAttrHandler;
};



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/images/Images/ImageInterface.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
