//# ImageProxy.h:  Proxy interface to images
//# Copyright (C) 1997-2007
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

#ifndef IMAGES_IMAGEPROXY_H
#define IMAGES_IMAGEPROXY_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/images/Images/MaskSpecifier.h>
#include <casacore/lattices/Lattices/LatticeBase.h>
#include <casacore/lattices/Lattices/TiledShape.h>
#include <casacore/casa/Utilities/CountedPtr.h>
#include <casacore/casa/Containers/ValueHolder.h>
#include <casacore/casa/Containers/Record.h>

namespace casacore {

  //# Forward Declarations.
  class ImageInfo;
  template<typename T> class ImageInterface;
  class LatticeExprNode;
  class CoordinateSystem;
  class ImageAttrHandler;

  // <synopsis>
  // ImageProxy is a proxy to an image having data type float, double,
  // Complex, or DComplex. Its primary purpose is to be bind the images
  // module to Python through pyrap.images. However, it can also be used
  // directly in C++.
  //
  // An ImageProxy object can be constructed for an image stored on disk in
  // Casacore, FITS, HDF5, or Miriad format. It can also be constructed given
  // a shape or an N-dim array with values.
  // Furthermore it can be constructed from a LEL expression (see class ImageExpr)
  // or a vector of images to be concatenated (see class ImageConcat).
  //
  // Many functions exist to operate on an ImageProxy object. For example:
  // <ul>
  //  <li> get meta info (shape, data type, coordinates, etc.)
  //  <li> save in Casacore, HDF5, or FITS format.
  //  <li> regrid.
  //  <li> get statistics.
  //  <li> form a subimage (which is done in a virtual way).
  // </ul>
  // Functions regrid and statistics can only be used for float images.
  // They throw an exception for images with other data types.
  // Note that using a LEL expression it is possible to (virtually) convert an
  // image with another type to a float image.
  // </synopsis>

  class ImageProxy
  {
  public:
    // Default constructor is needed for Boost-Python.
    ImageProxy();

    // Construct from the concatenation of the images along the axis.
    // The axis must be given as a Fortran-array axis.
    // All images must be of the same data type.
    ImageProxy (const Vector<String>& names, int32_t axis);

    // Construct from a string that contains an image name or image expression.
    // It is opened using ImageOpener.
    ImageProxy (const String& name, const String& mask,
                const std::vector<ImageProxy>& images);

    // Construct from the concatenation of the image objects along the axis.
    // The axis must be given as a Fortran-array axis.
    // All images must be of the same data type.
    //# The dummy arguments are needed to let all constructors have a
    //# different number of arguments (for Boost-Python).
    ImageProxy (const std::vector<ImageProxy>& images, int32_t axis,
                int32_t dummy1=0, int32_t dummy2=0);

    // Construct from a ValueHolder containing an Array of float or Complex.
    // If the name is empty it is created as a temporary image, otherwise
    // as a PagedImage or HDF5Image.
    // If the coordinates record is empty, default coordinates are used.
    // A mask is created if the mask name or mask value is not empty.
    ImageProxy (const ValueHolder& values,
                const ValueHolder& mask,
                const Record& coordinates,
                const String& imageName = String(),
                bool overwrite = true,
                bool asHDF5 = false,
                const String& maskName = String(),
                const IPosition& tileShape = IPosition());

    // Construct from a shape.
    // If the name is empty it is created as a temporary image, otherwise
    // as a PagedImage or HDF5Image.
    // If the coordinates record is empty, default coordinates are used.
    // A mask is created if the mask name is not empty.
    ImageProxy (const IPosition& shape,
                const ValueHolder& value,
                const Record& coordinates,
                const String& imageName = String(),
                bool overwrite = true,
                bool asHDF5 = false,
                const String& maskName = String(),
                const IPosition& tileShape = IPosition(),
                int32_t dummy=0);

    // Construct from an existing image object.
    ImageProxy (const CountedPtr<LatticeBase>&);

    // Copy constructor (reference semantics).
    ImageProxy (const ImageProxy&);

    // Assignment (reference semantics).
    ImageProxy& operator= (const ImageProxy&);

     ~ImageProxy();

    // Open the image (which can also be an expression).
    // It throws an exception if not succeeded.
    static LatticeBase* openImage (const String& name,
                                   const String& mask = String(),
                                   const std::vector<ImageProxy>& images = std::vector<ImageProxy>());

    // Open an image in the file/table with the given name.
    // The specified mask will be applied (default is default mask).
    // A null pointer is returned for an unknown image type.
    // Non-Casacore image types must have been registered to be known.
    // If not successful, try to open it as an image expression.
    static LatticeBase* openImageOrExpr (const String& str,
                                         const MaskSpecifier&,
                                         const Block<LatticeExprNode>& nodes);

    // Close the image by setting all pointers to 0.
    void close();

    // Turn the ImageProxy into a LatticeExprNode.
    LatticeExprNode makeNode() const;

    // Is the image persistent or temporary.
    bool isPersistent() const;

    // Get the name of the image.
    String name (bool stripPath=false) const;

    // Get the shape of the image.
    IPosition shape() const;

    // Get the dimensionality of the image.
    uint32_t ndim() const;

    // Get the size of the image (nr of pixels).
    uint32_t size() const;

    // Get the data type of the image.
    String dataType() const;

    DataType type() const;

    // Get the image type (PagedImage, HDF5Image, etc.)
    String imageType() const;

    // Get a chunk of data.
    ValueHolder getData (const IPosition& blc,
                         const IPosition& trc, 
                         const IPosition& inc);

    // Get a chunk of the mask.
    ValueHolder getMask (const IPosition& blc,
                         const IPosition& trc, 
                         const IPosition& inc);

    // Put a chunk of data.
    void putData (const ValueHolder&,
                  const IPosition& blc,
                  const IPosition& inc);

    // Put a chunk of the mask.
    // The mask will be created if not present yet.
    // That will not be done if the entire mask is true.
    void putMask (const ValueHolder& value,
                  const IPosition& blc,
                  const IPosition& inc);

    // Does the image have a read or write lock?
    bool hasLock (bool writeLock = false);

    // Try to acquire a read or write lock.
    // nattempts=0 means wait until acquired. Otherwise every second an
    // attempt is done.
    void lock (bool writeLock=false, int32_t nattempts=0);

    // Release the lock acquired by lock().
    void unlock();

    // Get the names of the attribute groups.
    Vector<String> attrGroupNames() const;

    // Create a new attribute group.
    void createAttrGroup (const String& groupName);

    // Get the names of all attributes in a group.
    Vector<String> attrNames (const String& groupName) const;

    // Get the number of rows in an attribute group.
    uint32_t attrNrows (const String& groupName) const;

    // Get the value of an attribute in a group row.
    ValueHolder getAttr (const String& groupName,
                         const String& attrName,
                         uint32_t rownr) const;

    // Get all attributes in a group row.
    Record getAttrRow (const String& groupName,
                       uint32_t rownr) const;

    // Get the unit(s) of an attribute in a group.
    Vector<String> getAttrUnit(const String& groupName,
                               const String& attrName) const;

    // Get the measinfo of an attribute in a group.
    Vector<String> getAttrMeas(const String& groupName,
                               const String& attrName) const;

    // Put the value, unit, and measinfo of an attribute in a group row.
    // The attribute or row is added if new.
    void putAttr (const String& groupName, const String& attrName, uint32_t rownr,
                  const ValueHolder& value,
                  const Vector<String>& units,
                  const Vector<String>& measInfo);

    // Form a new (virtual) image being a subset of the image.
    // It uses preserveAxesOrder=false.
    ImageProxy subImage (const IPosition& blc,
                         const IPosition& trc, 
                         const IPosition& inc,
                         bool dropDegenerate=true);
    // Same with a new function name for backward compatibility with old pyrap.
    ImageProxy subImage2 (const IPosition& blc,
                          const IPosition& trc, 
                          const IPosition& inc,
                          bool dropDegenerate,
                          bool preserveAxesOrder);

    // Get the brightness unit.
    String unit() const;

    // Get the coordinate system.
    Record coordSys() const;

    const CoordinateSystem& coordSysObject() const;

    // Convert a pixel coordinate to world coordinate.
    // if <src>reverseAxes=true</src> the input and output vector will be
    // reversed (as needed for pyrap).
    Vector<double> toWorld (const Vector<double>& pixel,
                            bool reverseAxes);

    // Convert a world coordinate to pixel coordinate.
    // if <src>reverseAxes=true</src> the input and output vector will be
    // reversed (as needed for pyrap).
    Vector<double> toPixel (const Vector<double>& world,
                            bool reverseAxes);

    // Get the image info.
    Record imageInfo() const;

    const ImageInfo& imageInfoObject() const;

    // Get the miscellaneous info.
    Record miscInfo() const;

    // Get the history.
    Vector<String> history() const;

    // Write the image in FITS format.
    // See class ImageFITSConverter for a description of the arguments.
    // Currently only a float image can be written to FITS.
    void toFits (const String& fitsfile, bool overwrite=true,
                 bool velocity=true, bool optical=true, int32_t bitpix=-32,
                 double minpix=1, double maxpix=-1) const;

    // Write the image to an image file with the given name.
    // An exception is thrown if the name is the name of an already open image.
    void saveAs (const String& fileName, bool overwrite=true,
                 bool hdf5=false,
                 bool copyMask=true, const String& newMaskName=String(),
                 const IPosition& newTileShape=IPosition()) const;

    // Return the statistics for the given axes.
    // E.g., if axes 0,1 is given in a 3-dim image, the statistics are
    // calculated for each plane along the 3rd axis.
    // MinMaxValues can be given to include or exclude (4th argument) pixels
    // with values in the given range. If only one value is given, min=-abs(val)
    // and max=abs(val).
    // Robust statistics (Median, MedAbsDevMed, and Quartile) can be returned
    // too.
    Record statistics (const Vector<int32_t>& axes,
                       const String& mask,
                       const ValueHolder& minMaxValues,
                       bool exclude = false,
                       bool robust = false) const;

    // Regrid the image on the given axes to the given coordinate system.
    // The output is stored in the given file; it no file name is given a
    // temporary image is made.
    // If the output shape is empty, the old shape is used.
    // <src>replicate=true</src> means replication rather than regridding.
    ImageProxy regrid (const Vector<int32_t>& axes = Vector<int32_t>(),
                       const String& outfile = String(),
                       bool overwriteOutFile = true,
                       const IPosition& outShape = IPosition(),
                       const Record& coordSys = Record(),
                       const String& method = "linear",
                       int32_t decimate = 10,
                       bool replicate = false,
                       bool doRefChange = true,
                       bool forceRegrid = false);

    // Check and adjust blc, trc, or inc using the shape.
    // <group>
    static IPosition adjustBlc (const IPosition& blc, const IPosition& shp);
    static IPosition adjustTrc (const IPosition& trc, const IPosition& shp);
    static IPosition adjustInc (const IPosition& inc, const IPosition& shp);
    // </group>


    /*

    ImageProxy rotate(const String& outfile, 
                      const IPosition& shape, 
                      const Quantity& pa, Record& region, 
                      const String& mask, 
                      const String& method = "cubic", 
                      int32_t decimate = 0, 
                      bool replicate = false, 
                      bool dropdeg = false,
                      bool overwrite = false);


    bool setbrightnessunit (const String& unit);

    bool setcoordsys (const Record& csys);

    bool sethistory (const String& origin, const Vector<String>& history);

    bool setmiscinfo (const Record& info);

    ImageProxy subimage(const String& outfile, Record& region, 
                        const String& mask, 
                        bool dropdeg = false, 
                        bool overwrite = false, 
                        bool list = true);

    Vector<double> topixel(Record& value);

    */

    LatticeBase* getLattice() const
      { return itsLattice.operator->(); }

 private:
    // Form an ImageProxy object from an existing image object.
    explicit ImageProxy (LatticeBase*);

    // Throw an exception if the object is null.
    void checkNull() const;

    // Make an image from an array or shape.
    template<typename T>
    void makeImage (const Array<T>& array, const Array<bool>& mask,
                    const IPosition& shape, const Record& coordinates,
                    const String& fileName, bool asHDF5,
                    const String& maskName,
                    const IPosition& tileShape);

    // Form a concatenated image.
    // <group>
    void concatImages         (const std::vector<ImageProxy>& images, int32_t axis);
    void concatImagesFloat    (const std::vector<ImageProxy>& images, int32_t axis);
    void concatImagesDouble   (const std::vector<ImageProxy>& images, int32_t axis);
    void concatImagesComplex  (const std::vector<ImageProxy>& images, int32_t axis);
    void concatImagesDComplex (const std::vector<ImageProxy>& images, int32_t axis);
    // </group>

    // Setup the pointers for the various image data types.
    void setup();

    // Setup the pointers for the various image data types.
    // It takes over the lattice pointer.
    void setup (LatticeBase* lattice);

    // Centre all axes except the Stokes one.
    void centreRefPix (CoordinateSystem& cSys, 
                       const IPosition& shape) const;

    // Put the mask and create it if needed.
    template<typename T>
    void doPutMask (ImageInterface<T>& image,
                    const ValueHolder& value,
                    const IPosition& blc,
                    const IPosition& inc);

    // Copy the image to an image (PagedImage or HDF5Image) with the given name.
    // A new tile shape can be given.
    // If the image is masked, the mask can be copied as well.
    template <typename T>
    void saveImage (const String& fileName,
                    bool hdf5, bool copyMask,
                    const String& newMaskName, 
                    const IPosition& newTileShape,
                    const ImageInterface<T>& image) const;

    // Form a tiled shape from the current shape and a possible new tile shape.
    TiledShape makeTiledShape (const IPosition& newTileShape,
                               const IPosition& shape,
                               const IPosition& oldTileShape=IPosition()) const;

    // Calculate the statistics.
    template<typename T>
    Record makeStatistics (const ImageInterface<T>& image,
                           const Vector<int32_t>& axes,
                           const String& mask,
                           const ValueHolder& minMaxValues,
                           bool exclude,
                           bool robust) const;

    // Do the actual regridding.
    template<typename T>
    ImageProxy doRegrid (const ImageInterface<T>& image,
                         const Vector<int32_t>& axes,
                         const String& outfile,
                         const IPosition& shape,
                         const Record& coordSys,
                         const String& method,
                         int32_t decimate,
                         bool replicate, 
                         bool doRefChange,
                         bool force);

    // Make a coordinate system from the Record.
    // The cylindrical fix is applied if needed.
    CoordinateSystem makeCoordinateSystem (const Record& coordinates,
                                           const IPosition& shape) const;


    //# Data members.
    //# itsLattice is the real data; the pointers are for type convenience only.
    CountedPtr<LatticeBase>   itsLattice;
    ImageInterface<float>*    itsImageFloat;
    ImageInterface<double>*   itsImageDouble;
    ImageInterface<Complex>*  itsImageComplex;
    ImageInterface<DComplex>* itsImageDComplex;
    const CoordinateSystem*   itsCoordSys;
    ImageAttrHandler*         itsAttrHandler;
  };

} // end namespace casacore

#endif
