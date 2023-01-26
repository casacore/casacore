//# ImageProxy.cc:  Proxy interface to images
//# Copyright (C) 1995-2007
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

//# Do not use automatic template instantiation.
#define CACACORE_NO_AUTO_TEMPLATES

#include <casacore/casa/aips.h>
#include <casacore/images/Images/ImageProxy.h>
#include <casacore/images/Images/ImageInterface.h>
#include <casacore/images/Images/ImageInfo.h>
#include <casacore/images/Images/ImageConcat.h>
#include <casacore/images/Images/ImageFITSConverter.h>
#include <casacore/images/Images/ImageRegrid.h>
#include <casacore/images/Images/ImageSummary.h>
#include <casacore/images/Images/ImageStatistics.h>
#include <casacore/images/Images/ImageOpener.h>
#include <casacore/images/Images/TempImage.h>
#include <casacore/images/Images/ImageExpr.h>
#include <casacore/images/Images/PagedImage.h>
#include <casacore/images/Images/HDF5Image.h>
#include <casacore/images/Images/FITSImage.h>
#include <casacore/images/Images/MIRIADImage.h>
#include <casacore/images/Images/ImageUtilities.h>
#include <casacore/lattices/LEL/LatticeExprNode.h>
#include <casacore/coordinates/Coordinates/CoordinateSystem.h>
#include <casacore/coordinates/Coordinates/CoordinateUtil.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/sstream.h>
#include <vector>
#include <list>

using namespace std;

namespace casacore { //# name space casa begins

  ImageProxy::ImageProxy()
    : itsImageFloat    (0),
      itsImageDouble   (0),
      itsImageComplex  (0),
      itsImageDComplex (0),
      itsCoordSys      (0),
      itsAttrHandler   (0)
  {}

  ImageProxy::ImageProxy (LatticeBase* lattice)
    : itsImageFloat    (0),
      itsImageDouble   (0),
      itsImageComplex  (0),
      itsImageDComplex (0),
      itsCoordSys      (0),
      itsAttrHandler   (0)
  {
    setup (lattice);
  }

  ImageProxy::ImageProxy (const String& name, const String& mask,
                          const vector<ImageProxy>& images)
    : itsImageFloat    (0),
      itsImageDouble   (0),
      itsImageComplex  (0),
      itsImageDComplex (0),
      itsCoordSys      (0),
      itsAttrHandler   (0)
  {
    // Register the functions to create a FITSImage or MIRIADImage object.
    FITSImage::registerOpenFunction();
    MIRIADImage::registerOpenFunction();
    LatticeBase* lattice = openImage (name, mask, images);
    setup (lattice);
  }

  ImageProxy::ImageProxy (const ValueHolder& values, const ValueHolder& mask,
                          const Record& coordinates,
                          const String& fileName, bool overwrite, bool asHDF5,
                          const String& maskName,
                          const IPosition& tileShape)
    : itsImageFloat    (0),
      itsImageDouble   (0),
      itsImageComplex  (0),
      itsImageDComplex (0),
      itsCoordSys      (0),
      itsAttrHandler   (0)
  {
    if (!overwrite) {
      File file(fileName);
      if (file.exists()) {
        throw AipsError ("file " + fileName +
                         " already exists and should not be overwritten");
      }
    }
    switch (values.dataType()) {
    case TpArrayShort:
    case TpArrayUShort:
    case TpArrayInt:
    case TpArrayUInt:
    case TpArrayInt64:
    case TpArrayFloat:
      makeImage (values.asArrayFloat(), mask.asArrayBool(),
                 IPosition(), coordinates,
                 fileName, asHDF5, maskName, tileShape);
      break;
    case TpArrayDouble:
      makeImage (values.asArrayDouble(), mask.asArrayBool(),
                 IPosition(), coordinates,
                 fileName, asHDF5, maskName, tileShape);
      break;
    case TpArrayComplex:
      makeImage (values.asArrayComplex(), mask.asArrayBool(),
                 IPosition(), coordinates,
                 fileName, asHDF5, maskName, tileShape);
      break;
    case TpArrayDComplex:
      makeImage (values.asArrayDComplex(), mask.asArrayBool(),
                 IPosition(), coordinates,
                 fileName, asHDF5, maskName, tileShape);
      break;
    default:
      throw AipsError ("ImageProxy: invalid data type");
    }
  }

  ImageProxy::ImageProxy (const IPosition& shape, const ValueHolder& value,
                          const Record& coordinates,
                          const String& fileName, bool overwrite, bool asHDF5,
                          const String& maskName,
                          const IPosition& tileShape, int32_t)
    : itsImageFloat    (0),
      itsImageDouble   (0),
      itsImageComplex  (0),
      itsImageDComplex (0),
      itsCoordSys      (0),
      itsAttrHandler   (0)
  {
    if (!overwrite) {
      File file(fileName);
      if (file.exists()) {
        throw AipsError ("file " + fileName +
                         " already exists and should not be overwritten");
      }
    }
    switch (value.dataType()) {
    case TpUChar:
    case TpShort:
    case TpUShort:
    case TpInt:
    case TpUInt:
    case TpFloat:
      makeImage (Array<float>(), Array<bool>(), shape, coordinates,
                 fileName, asHDF5, maskName, tileShape);
      break;
    case TpDouble:
      makeImage (Array<double>(), Array<bool>(), shape, coordinates,
                 fileName, asHDF5, maskName, tileShape);
      break;
    case TpComplex:
      makeImage (Array<Complex>(), Array<bool>(), shape, coordinates,
                 fileName, asHDF5, maskName, tileShape);
      break;
    case TpDComplex:
      makeImage (Array<DComplex>(), Array<bool>(), shape, coordinates,
                 fileName, asHDF5, maskName, tileShape);
      break;
    default:
      throw AipsError ("ImageProxy: invalid data type");
    }
  }

  ImageProxy::ImageProxy (const Vector<String>& names, int32_t axis)
    : itsImageFloat    (0),
      itsImageDouble   (0),
      itsImageComplex  (0),
      itsImageDComplex (0),
      itsCoordSys      (0),
      itsAttrHandler   (0)
  {
    vector<ImageProxy> images;
    images.reserve (names.size());
    for (uint32_t i=0; i<names.size(); ++i) {
      images.push_back (ImageProxy(names[i], "", vector<ImageProxy>()));
    }
    concatImages (images, axis);
  }

  ImageProxy::ImageProxy (const vector<ImageProxy>& images, int32_t axis,
                          int32_t, int32_t)
    : itsImageFloat    (0),
      itsImageDouble   (0),
      itsImageComplex  (0),
      itsImageDComplex (0),
      itsCoordSys      (0),
      itsAttrHandler   (0)
  {
    concatImages (images, axis);
  }

  ImageProxy::ImageProxy (const CountedPtr<LatticeBase>& image)
    : itsLattice       (image),
      itsImageFloat    (0),
      itsImageDouble   (0),
      itsImageComplex  (0),
      itsImageDComplex (0),
      itsCoordSys      (0),
      itsAttrHandler   (0)
  {
    if (! itsLattice.null()) {
      setup();
    }
  }

  ImageProxy::ImageProxy (const ImageProxy& that)
    : itsLattice       (that.itsLattice),
      itsImageFloat    (0),
      itsImageDouble   (0),
      itsImageComplex  (0),
      itsImageDComplex (0),
      itsCoordSys      (0),
      itsAttrHandler   (0)
  {
    if (! itsLattice.null()) {
      setup();
    }
  }

  ImageProxy& ImageProxy::operator= (const ImageProxy& that)
  {
    if (this != &that) {
      close();
      itsLattice = that.itsLattice;
      if (! itsLattice.null()) {
        setup();
      }
    }
    return *this;
  }

  ImageProxy::~ImageProxy()
  {}

  LatticeBase* ImageProxy::openImage (const String& name, const String& mask,
                                      const vector<ImageProxy>& images)
  {
    MaskSpecifier maskSp;
    if (!mask.empty()) {
      if (mask == "nomask") {
        maskSp = MaskSpecifier ("");
      } else {
        maskSp = MaskSpecifier (mask);
      }
    }
    Block<LatticeExprNode> tempNodes(images.size());
    for (uint32_t i=0; i<images.size(); ++i) {
      tempNodes[i] = images[i].makeNode();
    }
    String msg;
    LatticeBase* lattice = 0;
    try {
      lattice = openImageOrExpr (name, maskSp, tempNodes);
    } catch (const std::exception& x) {
      msg = x.what();
      lattice = 0;
    }
    if (lattice == 0) {
      throw AipsError (name + " cannot be opened as image (expression): "
                       + msg);
    }
    return lattice;
  }

  LatticeBase* ImageProxy::openImageOrExpr (const String& str,
                                            const MaskSpecifier& spec,
                                            const Block<LatticeExprNode>& nodes)
  {
    LatticeBase* lattice = ImageOpener::openImage (str, spec);
    if (lattice == 0) {
      lattice = ImageOpener::openExpr (str, nodes);
    }
    return lattice;
  }

  void ImageProxy::close()
  {
    itsLattice       = CountedPtr<LatticeBase>();
    itsImageFloat    = 0;
    itsImageDouble   = 0;
    itsImageComplex  = 0;
    itsImageDComplex = 0;
    itsCoordSys      = 0;
    itsAttrHandler   = 0;
  }

  void ImageProxy::checkNull() const
  {
    if (itsLattice.null()) {
      throw AipsError ("ImageProxy does not contain an image object");
    }
  }

  LatticeExprNode ImageProxy::makeNode() const
  {
    if (itsImageFloat) {
      return LatticeExprNode (*itsImageFloat);
    } else if (itsImageDouble) {
      return LatticeExprNode (*itsImageDouble);
    } else if (itsImageComplex) {
      return LatticeExprNode (*itsImageComplex);
    } else if (itsImageDComplex) {
      return LatticeExprNode (*itsImageDComplex);
    }
    throw AipsError ("ImageProxy does not contain an image object");
  }

  template<typename T>
  void ImageProxy::makeImage (const Array<T>& array,
                              const Array<bool>& mask,
                              const IPosition& shape,
                              const Record& coordinates,
                              const String& name, bool asHDF5,
                              const String& maskName,
                              const IPosition& tileShape)
  {
    // Get shape and check arguments.
    IPosition shp(shape);
    if (array.size() == 0) {
      if (shape.size() == 0) {
        throw AipsError ("A value array or a shape has to be given");
      }
    } else {
      shp = array.shape();
      if (mask.size() > 0) {
        AlwaysAssert (array.shape().isEqual(mask.shape()), AipsError);
      }
    }
    CoordinateSystem cSys;
    if (coordinates.empty()) {
      cSys = CoordinateUtil::makeCoordinateSystem (shp, false);
      centreRefPix(cSys, shp);
    } else {
      cSys = makeCoordinateSystem (coordinates, shp);
    }
    ImageInterface<T>* image = 0;
    if (name.empty()) {
      image = new TempImage<T> (shp, cSys, 1000);
    } else if (asHDF5) {
      image = new HDF5Image<T>  (makeTiledShape(tileShape,shp), cSys, name);
    } else {
      image = new PagedImage<T> (makeTiledShape(tileShape,shp), cSys, name);
    }
    // Let ImageProxy take over the pointer.
    setup(image);
    // Write data if present.
    if (array.size() > 0) {
      image->put (array);
    }
    // Create and put mask if needed.
    // Use default 'mask0' if a mask is given.
    String mname(maskName);
    if (mname.empty()  &&  mask.size() > 0) {
      mname = "mask0";
    }
    if (!mname.empty()) {
      // Create a mask and make it the default mask.
      image->makeMask (mname, true, true);
    }
    // Put mask if present.
    if (mask.size() > 0) {
      image->pixelMask().put (mask);
    }
  }

  void ImageProxy::concatImages (const vector<ImageProxy>& images, int32_t axis)
  {
    if (images.size() == 0) {
      throw AipsError ("ImageProxy: no images given in vector");
    }
    switch (images[0].getLattice()->dataType()) {
    case TpFloat:
      concatImagesFloat (images, axis);
      break;
    case TpDouble:
      concatImagesDouble (images, axis);
      break;
    case TpComplex:
      concatImagesComplex (images, axis);
      break;
    case TpDComplex:
      concatImagesDComplex (images, axis);
      break;
    default:
      throw AipsError ("Image has an invalid data type");
    }
  }

  void ImageProxy::concatImagesFloat (const vector<ImageProxy>& images,
                                      int32_t axis)
  {
    ImageConcat<float>* concat = new ImageConcat<float>(axis);
    for (uint32_t i=0; i<images.size(); ++i) {
      LatticeBase* lattice = images[i].getLattice();
      if (lattice->dataType() != TpFloat) {
        throw AipsError ("Not all images to concatenate have type float");
      }
      // Note this cast is fully safe.
      concat->setImage (*(ImageInterface<float>*)(lattice), true);
    }
    setup (concat);
  }

  void ImageProxy::concatImagesDouble (const vector<ImageProxy>& images,
                                       int32_t axis)
  {
    ImageConcat<double>* concat = new ImageConcat<double>(axis);
    for (uint32_t i=0; i<images.size(); ++i) {
      LatticeBase* lattice = images[i].getLattice();
      if (lattice->dataType() != TpDouble) {
        throw AipsError ("Not all images to concatenate have type double");
      }
      // Note this cast is fully safe.
      concat->setImage (*(ImageInterface<double>*)(lattice), true);
    }
    setup (concat);
  }

  void ImageProxy::concatImagesComplex (const vector<ImageProxy>& images,
                                        int32_t axis)
  {
    ImageConcat<Complex>* concat = new ImageConcat<Complex>(axis);
    for (uint32_t i=0; i<images.size(); ++i) {
      LatticeBase* lattice = images[i].getLattice();
      if (lattice->dataType() != TpComplex) {
        throw AipsError ("Not all images to concatenate have type Complex");
      }
      // Note this cast is fully safe.
      concat->setImage (*(ImageInterface<Complex>*)(lattice), true);
    }
    setup (concat);
  }

  void ImageProxy::concatImagesDComplex (const vector<ImageProxy>& images,
                                         int32_t axis)
  {
    ImageConcat<DComplex>* concat = new ImageConcat<DComplex>(axis);
    for (uint32_t i=0; i<images.size(); ++i) {
      LatticeBase* lattice = images[i].getLattice();
      if (lattice->dataType() != TpDComplex) {
        throw AipsError ("Not all images to concatenate have type DComplex");
      }
      // Note this cast is fully safe.
      concat->setImage (*(ImageInterface<DComplex>*)(lattice), true);
    }
    setup (concat);
  }

  void ImageProxy::setup (LatticeBase* lattice)
  {
    itsLattice = lattice;
    setup();
  }

  void ImageProxy::setup()
  {
    // Get raw pointer from the CountedPtr.
    LatticeBase* lattice = &(*itsLattice);
    switch (lattice->dataType()) {
    case TpFloat:
      itsImageFloat = dynamic_cast<ImageInterface<float>*>(lattice);
      break;
    case TpDouble:
      itsImageDouble = dynamic_cast<ImageInterface<double>*>(lattice);
      break;
    case TpComplex:
      itsImageComplex = dynamic_cast<ImageInterface<Complex>*>(lattice);
      break;
    case TpDComplex:
      itsImageDComplex = dynamic_cast<ImageInterface<DComplex>*>(lattice);
      break;
    default:
      throw AipsError ("Image has an invalid data type");
    }
    if (itsImageFloat) {
      itsCoordSys    = &itsImageFloat->coordinates();
      itsAttrHandler = &itsImageFloat->attrHandler();
    } else if (itsImageDouble) {
      itsCoordSys    = &itsImageDouble->coordinates();
      itsAttrHandler = &itsImageDouble->attrHandler();
    } else if (itsImageComplex) {
      itsCoordSys    = &itsImageComplex->coordinates();
      itsAttrHandler = &itsImageComplex->attrHandler();
    } else if (itsImageDComplex) {
      itsCoordSys    = &itsImageDComplex->coordinates();
      itsAttrHandler = &itsImageDComplex->attrHandler();
    } else {
      throw AipsError ("The lattice does not appear to be an image");
    }
 }

  void ImageProxy::centreRefPix (CoordinateSystem& cSys, 
                                 const IPosition& shape) const
  {
    // Center all axes except Stokes.
    int32_t after = -1;
    int32_t iS = cSys.findCoordinate (Coordinate::STOKES, after);
    int32_t sP = -1;
    if (iS >= 0) {
      sP = cSys.pixelAxes(iS)[0];
    }
    Vector<double> refPix = cSys.referencePixel();
    for (uint32_t i=0; i<refPix.nelements(); ++i) {
      if (int32_t(i) != sP) {
        refPix[i] = double(shape[i] / 2);
      }
    }
    cSys.setReferencePixel (refPix);
  }

  bool ImageProxy::isPersistent() const
  {
    checkNull();
    return itsLattice->isPersistent();
  }

  String ImageProxy::name (bool stripPath) const
  {
    checkNull();
    return itsLattice->name (stripPath);
  }

  IPosition ImageProxy::shape() const
  {
    checkNull();
    return itsLattice->shape();
  }

  uint32_t ImageProxy::ndim() const
  {
    checkNull();
    return itsLattice->shape().size();
  }

  uint32_t ImageProxy::size() const
  {
    checkNull();
    return itsLattice->shape().product();
  }

  String ImageProxy::dataType() const
  {
    checkNull();
    ostringstream ostr;
    ostr << itsLattice->dataType();
    return ostr.str();
  }

  DataType ImageProxy::type() const {
      checkNull();
      return itsLattice->dataType();
  }

  String ImageProxy::imageType() const
  {
    // LatticeBase does not have a fileType function or so.
    // So alas we have to use the one in the Image object.
    if (itsImageFloat) {
      return itsImageFloat->imageType();
    } else if (itsImageDouble) {
      return itsImageDouble->imageType();
    } else if (itsImageComplex) {
      return itsImageComplex->imageType();
    } else if (itsImageDComplex) {
      return itsImageDComplex->imageType();
    }
    throw AipsError ("ImageProxy does not contain an image object");
  }

  Vector<String> ImageProxy::attrGroupNames() const
  {
    checkNull();
    return itsAttrHandler->groupNames();
  }

  void ImageProxy::createAttrGroup (const String& groupName)
  {
    checkNull();
    itsAttrHandler->createGroup (groupName);
  }

  Vector<String> ImageProxy::attrNames (const String& groupName) const
  {
    checkNull();
    return itsAttrHandler->openGroup(groupName).attrNames();
  }

  uint32_t ImageProxy::attrNrows (const String& groupName) const
  {
    checkNull();
    return itsAttrHandler->openGroup(groupName).nrows();
  }

  ValueHolder ImageProxy::getAttr (const String& groupName,
                                   const String& attrName,
                                   uint32_t rownr) const
  {
    checkNull();
    return itsAttrHandler->openGroup(groupName).getData (attrName, rownr);
  }

  Record ImageProxy::getAttrRow (const String& groupName,
                                 uint32_t rownr) const
  {
    checkNull();
    return itsAttrHandler->openGroup(groupName).getDataRow (rownr);
  }

  Vector<String> ImageProxy::getAttrUnit(const String& groupName,
                                         const String& attrName) const
  {
    checkNull();
    return itsAttrHandler->openGroup(groupName).getUnit (attrName);
  }

  Vector<String> ImageProxy::getAttrMeas(const String& groupName,
                                         const String& attrName) const
  {
    checkNull();
    return itsAttrHandler->openGroup(groupName).getMeasInfo (attrName);
  }

  void ImageProxy::putAttr (const String& groupName,
                            const String& attrName,
                            uint32_t rownr,
                            const ValueHolder& value,
                            const Vector<String>& units,
                            const Vector<String>& measInfo)
  {
    checkNull();
    itsAttrHandler->openGroup(groupName).putData (attrName, rownr, value,
                                                  units, measInfo);
  }

  ValueHolder ImageProxy::getData (const IPosition& blc,
                                   const IPosition& trc, 
                                   const IPosition& inc)
  {
    IPosition shp = shape();
    Slicer slicer(adjustBlc(blc, shp),
                  adjustTrc(trc, shp),
                  adjustInc(inc, shp),
                  Slicer::endIsLast);
    if (itsImageFloat) {
      return ValueHolder (itsImageFloat->getSlice (slicer));
    } else if (itsImageDouble) {
      return ValueHolder (itsImageDouble->getSlice (slicer));
    } else if (itsImageComplex) {
      return ValueHolder (itsImageComplex->getSlice (slicer));
    } else if (itsImageDComplex) {
      return ValueHolder (itsImageDComplex->getSlice (slicer));
    }
    throw AipsError ("ImageProxy does not contain an image object");
  }

  ValueHolder ImageProxy::getMask (const IPosition& blc,
                                   const IPosition& trc, 
                                   const IPosition& inc)
  {
    IPosition shp = shape();
    Slicer slicer(adjustBlc(blc, shp),
                  adjustTrc(trc, shp),
                  adjustInc(inc, shp),
                  Slicer::endIsLast);
    if (itsImageFloat) {
      return ValueHolder (itsImageFloat->getMaskSlice (slicer));
    } else if (itsImageDouble) {
      return ValueHolder (itsImageDouble->getMaskSlice (slicer));
    } else if (itsImageComplex) {
      return ValueHolder (itsImageComplex->getMaskSlice (slicer));
    } else if (itsImageDComplex) {
      return ValueHolder (itsImageDComplex->getMaskSlice (slicer));
    }
    throw AipsError ("ImageProxy does not contain an image object");
  }

  void ImageProxy::putData (const ValueHolder& value,
                            const IPosition& blc,
                            const IPosition& inc)
  {
    IPosition shp = shape();
    IPosition ablc = adjustBlc (blc, shp);
    IPosition ainc = adjustInc (inc, shp);
    if (itsImageFloat) {
      itsImageFloat->putSlice (value.asArrayFloat(), ablc, ainc);
    } else if (itsImageDouble) {
      itsImageDouble->putSlice (value.asArrayDouble(), ablc, ainc);
    } else if (itsImageComplex) {
      itsImageComplex->putSlice (value.asArrayComplex(), ablc, ainc);
    } else if (itsImageDComplex) {
      itsImageDComplex->putSlice (value.asArrayDComplex(), ablc, ainc);
    } else {
      throw AipsError ("ImageProxy does not contain an image object");
    }
  }

  void ImageProxy::putMask (const ValueHolder& value,
                            const IPosition& blc,
                            const IPosition& inc)
  {
    IPosition shp = shape();
    IPosition ablc = adjustBlc (blc, shp);
    IPosition ainc = adjustInc (inc, shp);
    if (itsImageFloat) {
      doPutMask (*itsImageFloat, value, ablc, ainc);
    } else if (itsImageDouble) {
      doPutMask (*itsImageDouble, value, ablc, ainc);
    } else if (itsImageComplex) {
      doPutMask (*itsImageComplex, value, ablc, ainc);
    } else if (itsImageDComplex) {
      doPutMask (*itsImageDComplex, value, ablc, ainc);
    } else {
      throw AipsError ("ImageProxy does not contain an image object");
    }
  }

  template<typename T>
  void ImageProxy::doPutMask (ImageInterface<T>& image,
                              const ValueHolder& value,
                              const IPosition& blc,
                              const IPosition& inc)
  {
    checkNull();
    Array<bool> maskArr = value.asArrayBool();
    if (! image.hasPixelMask()) {
      // No mask yet.
      // Do not put if the entire mask is true. This might reflect a get
      // where all true-s are filled in if there is no mask.
      if (anyEQ(maskArr, false)) {
        // Create a mask and make it the default mask.
        image.makeMask ("mask0", true, true);
        // Initialize the mask if only part of the mask will be put.
        if (! maskArr.shape().isEqual (image.shape())) {
          image.pixelMask().set (true);
        }
      }
    }
    if (image.hasPixelMask()) {
      image.pixelMask().putSlice (value.asArrayBool(), blc, inc);
    }
  }

  bool ImageProxy::hasLock (bool writeLock)
  {
    checkNull();
    return itsLattice->hasLock (writeLock ?
                                FileLocker::Write : FileLocker::Read);
  }
  
  void ImageProxy::lock (bool writeLock, int32_t nattempts)
  {
    checkNull();
    itsLattice->lock (writeLock ? FileLocker::Write : FileLocker::Read,
                      nattempts);
  }

  void ImageProxy::unlock()
  {
    checkNull();
    itsLattice->unlock();
  }

  ImageProxy ImageProxy::subImage (const IPosition& blc,
                                   const IPosition& trc, 
                                   const IPosition& inc,
                                   bool dropDegenerate)
  {
    return subImage2 (blc, trc, inc, dropDegenerate, false);
  }
  ImageProxy ImageProxy::subImage2 (const IPosition& blc,
                                    const IPosition& trc, 
                                    const IPosition& inc,
                                    bool dropDegenerate,
                                    bool preserveAxesOrder)
  {
    AxesSpecifier axesSpec(!dropDegenerate);
    IPosition shp = shape();
    Slicer slicer(adjustBlc(blc, shp),
                  adjustTrc(trc, shp),
                  adjustInc(inc, shp),
                  Slicer::endIsLast);
    if (itsImageFloat) {
      return ImageProxy(new SubImage<float>(*itsImageFloat, slicer,
                                            true, axesSpec,
                                            preserveAxesOrder));
    } else if (itsImageDouble) {
      return ImageProxy(new SubImage<double>(*itsImageDouble, slicer,
                                             true, axesSpec,
                                             preserveAxesOrder));
    } else if (itsImageComplex) {
      return ImageProxy(new SubImage<Complex>(*itsImageComplex, slicer,
                                              true, axesSpec,
                                              preserveAxesOrder));
    } else if (itsImageDComplex) {
      return ImageProxy(new SubImage<DComplex>(*itsImageDComplex, slicer,
                                               true, axesSpec,
                                               preserveAxesOrder));
    }
    throw AipsError ("ImageProxy does not contain an image object");
  }

  IPosition ImageProxy::adjustBlc (const IPosition& blc, const IPosition& shp)
  {
    if (blc.size() > shp.size()) {
      throw AipsError ("blc length exceeds dimensionality of image");
    }
    // Initialize possibly missing elements to 0.
    IPosition res(shp.size(), 0);
    for (uint32_t i=0; i<blc.size(); ++i) {
      if (blc[i] >= shp[i]) {
        throw AipsError ("blc value exceeds shape of image");
      } else if (blc[i] > 0) {
        res[i] = blc[i];
      }
    }
    return res;
  }

  IPosition ImageProxy::adjustTrc (const IPosition& trc, const IPosition& shp)
  {
    if (trc.size() > shp.size()) {
      throw AipsError ("trc length exceeds dimensionality of image");
    }
    // Initialize possibly missing elements to shape.
    IPosition res(shp-1);
    for (uint32_t i=0; i<trc.size(); ++i) {
      if (trc[i] > 0  ||  trc[i] < shp[i]) {
        res[i] = trc[i];
      }
    }
    return res;
  }

  IPosition ImageProxy::adjustInc (const IPosition& inc, const IPosition& shp)
  {
    if (inc.size() > shp.size()) {
      throw AipsError ("inc length exceeds dimensionality of image");
    }
    // Initialize possibly missing elements to 1.
    IPosition res(shp.size(), 1);
    for (uint32_t i=0; i<inc.size(); ++i) {
      if (inc[i] > 1) {
        res[i] = inc[i];
      }
    }
    return res;
  }

  String ImageProxy::unit()const
  {
    if (itsImageFloat) {
      return itsImageFloat->units().getName();
    } else if (itsImageDouble) {
      return itsImageDouble->units().getName();
    } else if (itsImageComplex) {
      return itsImageComplex->units().getName();
    } else if (itsImageDComplex) {
      return itsImageDComplex->units().getName();
    }
    throw AipsError ("ImageProxy does not contain an image object");
  }

  Record ImageProxy::coordSys() const
  {
    checkNull();
    Record rec;
    itsCoordSys->save (rec, "x");
    Record& coord = rec.rwSubRecord("x");
    // Add the pixel axes info, so it can be used in coordinates.py.
    // Give the info in C-order (thus reverse values).
    // Also add the axes lengths.
    IPosition shape = itsLattice->shape();
    for (uint32_t i=0; i<itsCoordSys->nCoordinates(); ++i) {
      Vector<int32_t> paxes = itsCoordSys->pixelAxes(i);
      Vector<int32_t> axes(paxes.size());
      Vector<int32_t> axshp(paxes.size());
      for (uint32_t j=0; j<paxes.size(); ++j) {
        axes[j]  = shape.size() - paxes[paxes.size()-j-1] - 1;
        axshp[j] = shape[paxes[j]];
      }
      Record& coordRec = coord.rwSubRecord (itsCoordSys->coordRecordName(i));
      coordRec.define ("_image_axes", axes);
      coordRec.define ("_axes_sizes", axshp);
    }
    return coord;
  }

    const CoordinateSystem& ImageProxy::coordSysObject() const {
        checkNull();
        return *itsCoordSys;
    }

  Vector<double> ImageProxy::toWorld (const Vector<double>& pixel,
                                      bool reverseAxes)
  {
    checkNull();
    Vector<double> coord(pixel.size());
    if (!reverseAxes) {
      coord = pixel;
    } else {
      for (uint32_t i=0; i<pixel.size(); ++i) {
        coord[i] = pixel[pixel.size()-i-1];
      }
    }
    Vector<double> world;
    if (! itsCoordSys->toWorld (world, coord)) {
      throw AipsError (itsCoordSys->errorMessage());
    }
    if (!reverseAxes) {
      return world;
    }
    for (uint32_t i=0; i<world.size(); ++i) {
      coord[i] = world[world.size()-i-1];
    }
    return coord;  
  }

  Vector<double> ImageProxy::toPixel (const Vector<double>& world,
                                      bool reverseAxes)           
  {                                                               
    checkNull();
    Vector<double> coord(world.size());                           
    if (!reverseAxes) {                                           
      coord = world;                                              
    } else {                                                      
      for (uint32_t i=0; i<world.size(); ++i) {                       
        coord[i] = world[world.size()-i-1];
      }
    }
    Vector<double> pixel;
    if (! itsCoordSys->toPixel (pixel, coord)) {
      throw AipsError (itsCoordSys->errorMessage());
    }
    if (!reverseAxes) {
      return pixel;
    }
    for (uint32_t i=0; i<pixel.size(); ++i) {
      coord[i] = pixel[pixel.size()-i-1];
    }
    return coord;
  }

  Record ImageProxy::imageInfo() const
  {
    Record rec;
    String error;
    imageInfoObject().toRecord(error, rec);
    return rec;
  }

  const ImageInfo& ImageProxy::imageInfoObject() const {
    if (itsImageFloat) {
      return itsImageFloat->imageInfo();
    }
    else if (itsImageDouble) {
      return itsImageDouble->imageInfo();
    }
    else if (itsImageComplex) {
      return itsImageComplex->imageInfo();
    }
    else if (itsImageDComplex) {
      return itsImageDComplex->imageInfo();
    }
    else {
      throw AipsError ("ImageProxy does not contain an image object");
    }
  }

  Record ImageProxy::miscInfo() const
  {
    TableRecord rec;
    if (itsImageFloat) {
      rec = itsImageFloat->miscInfo();
    } else if (itsImageDouble) {
      rec = itsImageDouble->miscInfo();
    } else if (itsImageComplex) {
      rec = itsImageComplex->miscInfo();
    } else if (itsImageDComplex) {
      rec = itsImageDComplex->miscInfo();
    } else {
      throw AipsError ("ImageProxy does not contain an image object");
    }
    return Record(rec);
  }

  void ImageProxy::toFits (const String& fileName, bool overwrite,
                           bool velocity, bool optical, int32_t bitpix,
                           double minpix, double maxpix) const
  {
    checkNull();
    bool ok = false;
    String error ("Currently only float images can be converted to FITS");
    if (itsImageFloat) {
      ok = ImageFITSConverter::ImageToFITS (error, *itsImageFloat, fileName,
                                            HostInfo::memoryFree()/1024,
                                            velocity, optical,
                                            bitpix, minpix, maxpix,
                                            overwrite, false, false);
    }
    if (!ok) {
      throw AipsError (error);
    }
  }

  Vector<String> ImageProxy::history() const
  {
    const LoggerHolder* logger;
    if (itsImageFloat) {
      logger = &itsImageFloat->logger();
    } else if (itsImageDouble) {
      logger = &itsImageDouble->logger();
    } else if (itsImageComplex) {
      logger = &itsImageComplex->logger();
    } else if (itsImageDComplex) {
      logger = &itsImageDComplex->logger();
    } else {
      throw AipsError ("ImageProxy does not contain an image object");
    }
    list<string> l;
    for (LoggerHolder::const_iterator iter = logger->begin();
	 iter != logger->end();
	 iter++) {
      l.push_back (iter->message());
    }
    Vector<String> vec(l.size());
    String* vecd = vec.data();
    for (list<string>::const_iterator iter=l.begin(); iter!=l.end(); ++iter) {
      *vecd++ = *iter;
    }
    return vec;
  }

  void ImageProxy::saveAs (const String& fileName, bool overwrite,
                           bool hdf5,
                           bool copyMask, const String& newMaskName,
                           const IPosition& newTileShape) const
  {
    if (!overwrite) {
      File file(fileName);
      if (file.exists()) {
        throw AipsError ("file " + fileName +
                         " already exists and should not be overwritten");
      }
    }
    if (itsImageFloat) {
      saveImage (fileName, hdf5, copyMask, newMaskName,
                 newTileShape, *itsImageFloat);
    } else if (itsImageDouble) {
      saveImage (fileName, hdf5, copyMask, newMaskName,
                 newTileShape, *itsImageDouble);
    } else if (itsImageComplex) {
      saveImage (fileName, hdf5, copyMask, newMaskName,
                 newTileShape, *itsImageComplex);
    } else if (itsImageDComplex) {
      saveImage (fileName, hdf5, copyMask, newMaskName,
                 newTileShape, *itsImageDComplex);
    } else {
      throw AipsError ("ImageProxy does not contain an image object");
    }
  }

  TiledShape ImageProxy::makeTiledShape (const IPosition& newTileShape,
                                         const IPosition& shape,
                                         const IPosition& oldTileShape) const
  {
    if (! newTileShape.empty()) {
      return TiledShape (shape, newTileShape);
    }
    if (oldTileShape.empty()) {
      return TiledShape (shape);
    }
    return TiledShape (shape, oldTileShape);
  }

  template <typename T>
  void ImageProxy::saveImage (const String& fileName,
                              bool hdf5, bool copyMask,
                              const String& newMaskName, 
                              const IPosition& newTileShape,
                              const ImageInterface<T>& image) const
  {
    checkNull();
    ImageInterface<T>* newImage;
    TiledShape tiledShape (makeTiledShape (newTileShape,
                                           image.shape(),
                                           image.niceCursorShape()));
    if (hdf5) {
      newImage = new HDF5Image<T>  (tiledShape, image.coordinates(), fileName);
    } else {
      newImage = new PagedImage<T> (tiledShape, image.coordinates(), fileName);
    }
    newImage->copyData (image);
    ImageUtilities::copyMiscellaneous (*newImage, image);
    if (copyMask  &&  image.isMasked()) {
      // Generate mask name if not given
      String maskName = newMaskName;
      if (maskName.empty()) {
        maskName = image.getDefaultMask();
        if (maskName.empty()) {
          maskName = newImage->makeUniqueRegionName ("mask", 0);
        }
      }
      // Create a mask and make it the default mask.
      // Copy the image mask.
      newImage->makeMask (maskName, true, true);
      Lattice<bool>& pixelMaskOut = newImage->pixelMask();
      LatticeIterator<bool> maskIter(pixelMaskOut);
      for (maskIter.reset(); !maskIter.atEnd(); maskIter++) {
	maskIter.rwCursor() = image.getMaskSlice(maskIter.position(),
                                                 maskIter.cursorShape());
      }
    }
    delete newImage;
  }

  Record ImageProxy::statistics (const Vector<int32_t>& axes,
                                 const String& mask,
                                 const ValueHolder& minMaxValues,
                                 bool exclude,
                                 bool robust) const
  {
    checkNull();
    // Default for cursor is all axes.
    Vector<int32_t> axesc(axes);
    if (axesc.empty()) {
      axesc.resize (ndim());
      indgen (axesc);
    }
    if (itsImageFloat) {
      return makeStatistics (*itsImageFloat, axesc, mask,
                             minMaxValues, exclude, robust);
    } else if (itsImageDouble) {
      throw AipsError("No statistics possible yet on double precision images");
    } else if (itsImageComplex) {
      throw AipsError("No statistics possible on complex images");
    } else if (itsImageDComplex) {
      throw AipsError("No statistics possible on dcomplex images");
    } else {
      throw AipsError ("ImageProxy does not contain an image object");
    }
  }

  template<typename T>
  Record ImageProxy::makeStatistics (const ImageInterface<T>& image,
                                     const Vector<int32_t>& axes,
                                     const String&,
                                     const ValueHolder& minMaxValues,
                                     bool exclude,
                                     bool robust) const
  {
    checkNull();
    ImageStatistics<T> stats(image, false, false);
    // Set cursor axes.
    if (!stats.setAxes(axes)) {
      throw AipsError (stats.errorMessage());
    }
    // Set pixel include/exclude ranges.
    Vector<T> minMax;
    minMaxValues.getValue (minMax);
    if (minMax.size() > 0) {
      if (exclude) {
        stats.setInExCludeRange (Vector<T>(), minMax, false);
      } else {
        stats.setInExCludeRange (minMax, Vector<T>(), false);
      }
    }
    // Get statistics.
    Array<double> npts, sum, sumsquared, min, max, mean, sigma;
    Array<double> rms, fluxDensity, med, medAbsDevMed, quartile;
    if (robust) {
      stats.getStatistic (med, LatticeStatsBase::MEDIAN);
      stats.getStatistic (medAbsDevMed, LatticeStatsBase::MEDABSDEVMED);
      stats.getStatistic (quartile, LatticeStatsBase::QUARTILE);
    }
    stats.getStatistic (npts, LatticeStatsBase::NPTS);
    stats.getStatistic (sum, LatticeStatsBase::SUM);
    stats.getStatistic (sumsquared, LatticeStatsBase::SUMSQ);
    stats.getStatistic (min, LatticeStatsBase::MIN);
    stats.getStatistic (max, LatticeStatsBase::MAX);
    stats.getStatistic (mean, LatticeStatsBase::MEAN);
    stats.getStatistic (sigma, LatticeStatsBase::SIGMA);
    stats.getStatistic (rms, LatticeStatsBase::RMS);
    stats.getStatistic (fluxDensity, LatticeStatsBase::FLUX);
    Record retval;
    retval.define ("npts", npts);
    retval.define ("sum", sum);
    retval.define ("sumsq", sumsquared);
    retval.define ("min", min);
    retval.define ("max", max);
    retval.define ("mean", mean);
    if (robust) {
      retval.define ("median", med);
      retval.define ("medabsdevmed", medAbsDevMed);
      retval.define ("quartile", quartile);
    }
    retval.define ("sigma", sigma);
    retval.define ("rms", rms);
    ////retval.define ("flux", fluxDensity);
    IPosition minPos, maxPos;
    if (stats.getMinMaxPos(minPos, maxPos)) {
      if (minPos.nelements() > 0  &&  maxPos.nelements() > 0) {
        retval.define ("minpos", minPos.asVector());
        retval.define ("maxpos", maxPos.asVector());
      }
    }
    return retval;
  }

  ImageProxy ImageProxy::regrid (const Vector<int32_t>& axes,
                                 const String& outFile,
                                 bool overwrite,
                                 const IPosition& shape,
                                 const Record& coordSys,
                                 const String& method,
                                 int32_t decimate,
                                 bool replicate,
                                 bool doRefChange,
                                 bool forceRegrid)
  {
    if (!overwrite) {
      File file(outFile);
      if (file.exists()) {
        throw AipsError ("file " + outFile +
                         " already exists and should not be overwritten");
      }
    }
    if (itsImageFloat) {
      return doRegrid (*itsImageFloat, axes, outFile,
                       shape, coordSys, method,
                       decimate, replicate, doRefChange, forceRegrid);
    } else if (itsImageDouble) {
      throw AipsError("No regrid possible yet on double precision images");
    } else if (itsImageComplex) {
      throw AipsError("No regrid possible on complex images");
    } else if (itsImageDComplex) {
      throw AipsError("No regrid possible on dcomplex images");
    } else {
      throw AipsError ("ImageProxy does not contain an image object");
    }
  }

  template<typename T>
  ImageProxy ImageProxy::doRegrid (const ImageInterface<T>& image,
                                   const Vector<int32_t>& axes,
                                   const String& outFile,
                                   const IPosition& shape,
                                   const Record& coordSys,
                                   const String& method,
                                   int32_t decimate,
                                   bool replicate,
                                   bool doRefChange,
                                   bool forceRegrid)
  {
    String method2 = method;
    method2.upcase();
    IPosition outShape;
    if (shape.size() == 0  ||  shape[0] == -1) {
      outShape = image.shape();
    } else {
      outShape = shape;
    }
    // Deal with axes
    IPosition axes2(axes);
    // Make CoordinateSystem from user given.
    CoordinateSystem cSysTo   = makeCoordinateSystem (coordSys, outShape);
    CoordinateSystem cSysFrom = image.coordinates();
    if(cSysTo.nCoordinates() == 0){
      cSysTo = cSysFrom;
    }
    cSysTo.setObsInfo (cSysFrom.obsInfo());
    // Now build a CS which copies the user specified Coordinate for
    // axes to be regridded and the input image Coordinate for axes not
    // to be regridded
    LogIO log;
    set<Coordinate::Type> regridCoords;
    CoordinateSystem cSys =
      ImageRegrid<T>::makeCoordinateSystem (log, regridCoords,
                                            cSysTo, cSysFrom, axes2);
    if (cSys.nPixelAxes() != outShape.nelements()) {
      throw AipsError("The number of pixel axes in the output shape and "
                      "Coordinate System must be the same");
    }
    // Create the image and mask
    ImageInterface<float>* pImOut;
    if (outFile.empty()) {
      pImOut = new TempImage<float>(outShape, cSys);
    } else {
      pImOut = new PagedImage<float>(outShape, cSys, outFile);
      /// make hdf5 if image is hdf5
    }
    // Create proxy from it, so it gets deleted in case of an exception.
    ImageProxy proxy(pImOut);
    pImOut->set(0.0);
    ImageUtilities::copyMiscellaneous (*pImOut, image);
    Interpolate2D::Method imethod = Interpolate2D::stringToMethod(method);
    IPosition dummy;
    ImageRegrid<T> ir;
    ir.disableReferenceConversions (!doRefChange);
    ir.regrid (*pImOut, imethod, axes2, image, replicate, decimate,
               true, forceRegrid);
    return proxy;
  }

  CoordinateSystem
  ImageProxy::makeCoordinateSystem (const Record& coordinates,
                                    const IPosition& shape) const
  {
    CoordinateSystem* csp;
    if (coordinates.nfields() == 1) {
      // Must be a record as an element
      Record tmp(coordinates.asRecord(RecordFieldId(0)));
      csp = CoordinateSystem::restore (tmp, "");
    } else {
      csp = CoordinateSystem::restore (coordinates, "");
    }
    CoordinateSystem cs(*csp);
    delete csp;
    // Fix up any body longitude ranges.
    String errMsg;
    if (!CoordinateUtil::cylindricalFix (cs, errMsg, shape)) {
      throw AipsError (errMsg);
    }
    return cs;
  }


} // end of casa namespace
