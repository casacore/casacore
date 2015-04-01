//# HDF5Image.h: astronomical image in HDF5 format
//# Copyright (C) 2008
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

#ifndef IMAGES_HDF5IMAGE_H
#define IMAGES_HDF5IMAGE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/images/Images/ImageInterface.h>
#include <casacore/images/Images/ImageAttrHandlerHDF5.h>
#include <casacore/lattices/Lattices/HDF5Lattice.h>

//# Forward Declarations
#include <casacore/casa/iosfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  // <summary>
  // Read, store, and manipulate astronomical images in HDF5 format.
  // </summary>

  // <use visibility=export>

  // <reviewed reviewer="" date="" tests="tHDF5Image.cc" demos="dHDF5Image.cc">
  // </reviewed>

  // <prerequisite>
  //   <li> <linkto class=CoordinateSystem>CoordinateSystem</linkto>
  //   <li> <linkto class=ImageInterface>ImageInterface</linkto>
  //   <li> <linkto class=Lattice>Lattice</linkto>
  //   <li> <linkto class=LatticeIterator>LatticeIterator</linkto>
  //   <li> <linkto class=LatticeNavigator>LatticeNavigator</linkto>
  //   <li> <linkto class=ImageRegion>ImageRegion</linkto>
  // </prerequisite>

  // <etymology>
  // The HDF5Image name comes from its role as the Image class using HDF5.
  // </etymology>

  // <synopsis> 
  // All Casacore Images are Lattices.  They may be treated like any other Lattice;
  // getSlice(...), putSlice(...), LatticeIterator for iterating, etc...
  // ArrayImages contain a map, a mask for that map, and coordinate 
  // information.  This provides a Lattice interface for images and their 
  // respective coordinates.  Additional functionality is defined by the 
  // ImageInterface class. 
  //
  // You can use the global function <src>imagePixelType</src> to determine
  // what the pixel type of an image is before you open the image if your
  // code can work with Images of many possible types, or for error checking.
  //
  // </synopsis> 

  // <example>
  // This example shows how to create a mask for an image, fill it, and
  // make it known to the image.
  // <srcblock>
  //   // Open the image (as readonly for the moment).
  //   HDF5Image<Float> myimage ("image.name");
  //   // Create a mask for the image.
  //   // The mask will be stored in a subtable of the image.
  //   LCPagedMask mask (RegionHandler::makeMask (myimage, "mask.name"));
  //   // Fill the mask with whatever values (e.g. all True).
  //   mask.set (True);
  //   // Make the mask known to the image (with name mask1).
  //   myimage.defineRegion ("mask1", mask, RegionHandler::Masks);
  //   // Make the mask the default mask for this image.
  //   myimage.setDefaultMask ("mask1");
  // </srcblock>
  // It is possible to create as many masks as one likes. They can all
  // be defined as masks for the image (with different names, of course).
  // However, only one of them can be the default mask (the mask used
  // by default when the image is opened). When another mask has to be
  // used, one can do two things:
  // <ul>
  //  <li> Use setDefaultMask to make the other mask the default mask.
  //   This is advisable when the change should be more or less permanent.
  //  <li> Open the HDF5Image without using a default mask. Thereafter
  //   a <linkto class=SubImage>SubImage</linkto> object can be created
  //   from the HDF5Image and the mask. This is advisable when it the
  //   mask has to be used only one time.
  // </ul>
  // </example>

  // <motivation>
  // The size of astronomical data can be very large.  The ability to fit an 
  // entire image into random access memory cannot be guaranteed.  Paging from 
  // disk pieces of the image appeared to be the way to deal with this problem.
  // </motivation>

  // <note>
  //  When you make a new HDF5Image, and you are transferring
  //  information from some other HDF5Image, be aware that you
  //  must copy, manually, things like miscInfo, imageInfo, units,
  //  logSink (history) to the new file.
  // </note>

  template <class T> class HDF5Image: public ImageInterface<T>
  {
  public: 
    // Construct a new Image from shape and coordinate information. The image
    // will be stored in the named file.
    HDF5Image (const TiledShape& mapShape,
	       const CoordinateSystem& coordinateInfo,
	       const String& nameOfNewFile);
  
    // Reconstruct an image from a pre-existing file.
    // By default the default pixelmask (if available) is used.
    explicit HDF5Image (const String& fileName, MaskSpecifier = MaskSpecifier());
  
    // Copy constructor (reference semantics).
    HDF5Image (const HDF5Image<T>& other);

    ~HDF5Image();
  
    // Assignment operator (reference semantics).
    HDF5Image<T>& operator= (const HDF5Image<T>& other);
  
    // Make a copy of the object (reference semantics).
    virtual ImageInterface<T>* cloneII() const;

    // Get the image type (returns name of derived class).
    virtual String imageType() const;

    // Return the current HDF5 file name. By default this includes the full path. 
    // The path preceding the file name can be stripped off on request.
    virtual String name (Bool stripPath=False) const;

    // Function which changes the shape of the ImageExpr.
    // Throws an exception as an HDF5Image cannot be resized.
    virtual void resize(const TiledShape& newShape);

    // Check for symmetry in data members.
    virtual Bool ok() const;

    // Return the shape of the image.
    virtual IPosition shape() const;

    // Function which extracts an array from the map.
    virtual Bool doGetSlice (Array<T>& buffer, const Slicer& theSlice);
  
    // Function to replace the values in the map with soureBuffer.
    virtual void doPutSlice (const Array<T>& sourceBuffer,
			     const IPosition& where,
			     const IPosition& stride);

    // Get a pointer the default pixelmask object used with this image.
    // It returns 0 if no default pixelmask is used.
    virtual const LatticeRegion* getRegionPtr() const;

    // An HDF5Image is always persistent.
    virtual Bool isPersistent() const;

    // An HDF5Image is always paged to disk.
    virtual Bool isPaged() const;

    // Is the HDF5Image writable?
    virtual Bool isWritable() const;

    // Does the image object use a pixelmask?
    virtual Bool hasPixelMask() const;

    // Get access to the pixelmask used.
    // An exception is thrown if the image does not use a pixelmask.
    // <group>
    virtual const Lattice<Bool>& pixelMask() const;
    virtual Lattice<Bool>& pixelMask();
    // </group>

    // Set the default pixelmask to the mask with the given name
    // (which has to exist in the "masks" group).
    // If the image file is writable, the setting is persistent by writing
    // the name as a keyword.
    // If the given mask name is the empty string,
    // the default pixelmask is unset.
    virtual void setDefaultMask (const String& maskName);

    // Use the mask as specified.
    // If a mask was already in use, it is replaced by the new one.
    virtual void useMask (MaskSpecifier = MaskSpecifier());

    // Replace every element, x, of the lattice with the result of f(x).
    // you must pass in the address of the function -- so the function
    // must be declared and defined in the scope of your program.  
    // Both versions of apply require a function that accepts a single 
    // argument of type T (the Lattice template actual type) and returns
    // a result of the same type.  The first apply expects a function with
    // an argument passed by value; the second expects the argument to
    // be passed by const reference.  The first form ought to run faster
    // for the built-in types, which may be an issue for large Lattices
    // stored in memory, where disk access is not an issue.
    // <group>
    virtual void apply (T (*function)(T));
    virtual void apply (T (*function)(const T& ));
    virtual void apply (const Functional<T,T>& function);
    // </group>

    // Add a lattice to this image.
    HDF5Image<T>& operator+= (const Lattice<T>& other);

    // Function which sets the units associated with the image
    // pixels (i.e. the "brightness" unit). <src>setUnits()</src> returns
    // False if it cannot set the unit for some reason (e.g. the underlying
    // file is not writable).
    virtual Bool setUnits (const Unit& newUnits);

    // Flushes the new coordinate system to disk if the file is writable.
    virtual Bool setCoordinateInfo (const CoordinateSystem& coords);

    // These are the true implementations of the paran operator.
    // <note> Not for public use </note>
    // <group>
    virtual T getAt (const IPosition& where) const;
    virtual void putAt (const T& value, const IPosition& where);
    // </group>

    // Replace the miscinfo in the HDF5Image.
    // It can fail if, e.g., the underlying file is not writable.
    virtual Bool setMiscInfo (const RecordInterface& newInfo);

    // The ImageInfo object contains some miscellaneous information about the
    // image, which unlike that stored in MiscInfo, has a standard list of
    // things, such as the restoring beam.
    // Note that setImageInfo REPLACES the information with the new information.
    // It can fail if, e.g., the underlying file is not writable.
    virtual Bool setImageInfo(const ImageInfo& info);

    // Get access to the attribute handler.
    // If a handler keyword does not exist yet, it is created if
    // <src>createHandler</src> is set.
    // Otherwise the handler is empty and no groups can be created for it.
    virtual ImageAttrHandler& attrHandler (Bool createHandler=False);

    // Remove a region/mask belonging to the image from the given group
    // (which can be Any).
    // If a mask removed is the default mask, the image gets unmasked.
    // <br>Optionally an exception is thrown if the region does not exist.
    virtual void removeRegion (const String& name,
			       RegionHandler::GroupType = RegionHandler::Any,
			       Bool throwIfUnknown = True);

    // This is the implementation of the letter for the envelope Iterator
    // class. <note> Not for public use </note>.
    virtual LatticeIterInterface<T>* makeIter
    (const LatticeNavigator& navigator,
     Bool useRef) const;

    // Returns the maximum recommended number of pixels for a cursor. This is
    // the number of pixels in a tile. 
    virtual uInt advisedMaxPixels() const;

    // Help the user pick a cursor for most efficient access.
    virtual IPosition doNiceCursorShape (uInt maxPixels) const;

    // Flush the data.
    virtual void flush();


  private:
    // Function to return the internal HDF5File object to the RegionHandler.
    static const CountedPtr<HDF5File>& getFile (void* imagePtr);

    // This must be called in every constructor and place where the image
    // is attached to a new image.
    void attach_logtable();
    void open_logtable();
    void restoreUnits (const RecordInterface& rec);
    void restoreMiscInfo (const RecordInterface& rec);
    void restoreImageInfo (const RecordInterface& rec);
    void restoreAll();

    void check_conformance (const Lattice<T>& other);
    void applyMaskSpecifier (const MaskSpecifier&);
    void applyMask (const String& maskName);

    //# Data members.
    HDF5Lattice<T> map_p;
    LatticeRegion* regionPtr_p;
    ImageAttrHandlerHDF5 itsAttrHandler;

    //# Make members of parent class known.
  public:
    using ImageInterface<T>::logSink;
    using ImageInterface<T>::logger;
    using ImageInterface<T>::imageInfo;
    using ImageInterface<T>::coordinates;
    using ImageInterface<T>::getDefaultMask;
    using ImageInterface<T>::hasRegion;
    using ImageInterface<T>::getImageRegionPtr;
  protected:
    using ImageInterface<T>::setCoordsMember;
    using ImageInterface<T>::setMiscInfoMember;
    using ImageInterface<T>::setLogMember;
    using ImageInterface<T>::setUnitMember;
    using ImageInterface<T>::setImageInfoMember;
  };


  // Tell if HDF5 images can be used.
  inline Bool canUseHDF5Image()
    { return HDF5Object::hasHDF5Support(); }

  // Determine the pixel type in the HDF5Image contained in
  // <src>fileName</src>.  If the file doesn't appear to be HDF5 or cannot
  // be opened, TpOther is returned.
  // <group name="pixeltype")
  DataType hdf5imagePixelType (const String& fileName);
  // Check if this HDF5 file is an HDF5 image.
  Bool isHDF5Image (const String& fileName);
  // </group>


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/images/Images/HDF5Image.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
