//# HDF5Image.tcc: defines the HDF5Image class
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

#ifndef IMAGES_HDF5IMAGE_TCC
#define IMAGES_HDF5IMAGE_TCC


#include <casacore/images/Images/HDF5Image.h>
#include <casacore/images/Regions/ImageRegion.h>
#include <casacore/images/Regions/RegionHandlerHDF5.h>
#include <casacore/images/Images/ImageInfo.h>
#include <casacore/lattices/Lattices/ArrayLattice.h>
#include <casacore/lattices/Lattices/LatticeNavigator.h>
#include <casacore/lattices/Lattices/LatticeStepper.h>
#include <casacore/lattices/Lattices/LatticeIterator.h>
#include <casacore/lattices/LEL/LatticeExprNode.h>
#include <casacore/lattices/LEL/LatticeExpr.h>
#include <casacore/lattices/LRegions/LatticeRegion.h>
#include <casacore/casa/HDF5/HDF5Record.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/Logging/LogMessage.h>

#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/LogiArray.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/OS/File.h>
#include <casacore/casa/OS/Path.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Quanta/UnitMap.h>

#include <casacore/casa/iostream.h>
#include <casacore/casa/sstream.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

template <class T> 
HDF5Image<T>::HDF5Image (const TiledShape& shape, 
			 const CoordinateSystem& coordinateInfo, 
			 const String& fileName)
: ImageInterface<T>(RegionHandlerHDF5(getFile, this)),
  regionPtr_p      (0)
{
  map_p = HDF5Lattice<T>(shape, fileName, "map", "/");
  attach_logtable();
  AlwaysAssert(setCoordinateInfo(coordinateInfo), AipsError);
}

template <class T> 
HDF5Image<T>::HDF5Image (const String& fileName,
			 MaskSpecifier spec)
: ImageInterface<T>(RegionHandlerHDF5(getFile, this)),
  regionPtr_p      (0)
{
  map_p = HDF5Lattice<T>(fileName, "map", "/");
  attach_logtable();
  restoreAll();
  applyMaskSpecifier (spec);
}

template <class T> 
HDF5Image<T>::HDF5Image (const HDF5Image<T>& other)
: ImageInterface<T>(other),
  map_p            (other.map_p),
  regionPtr_p      (0)
{
  if (other.regionPtr_p != 0) {
    regionPtr_p = new LatticeRegion (*other.regionPtr_p);
  }
}

template <class T> 
HDF5Image<T>::~HDF5Image()
{
  flush();
  delete regionPtr_p;
}

template <class T> 
HDF5Image<T>& HDF5Image<T>::operator=(const HDF5Image<T>& other)
{
  if (this != &other) {
    ImageInterface<T>::operator= (other);
    map_p = other.map_p;
    delete regionPtr_p;
    regionPtr_p = 0;
    if (other.regionPtr_p != 0) {
      regionPtr_p = new LatticeRegion (*other.regionPtr_p);
    }
  } 
  return *this;
}

template <class T> 
ImageInterface<T>* HDF5Image<T>::cloneII() const
{
  return new HDF5Image<T> (*this);
}

template<class T>
String HDF5Image<T>::imageType() const
{
  return "HDF5Image";
}

template<class T>
Bool HDF5Image<T>::isPersistent() const
{
  return True;
}

template<class T>
Bool HDF5Image<T>::isPaged() const
{
  return True;
}

template <class T> 
Bool HDF5Image<T>::isWritable() const
{
  return map_p.isWritable();
}

template<class T>
Bool HDF5Image<T>::hasPixelMask() const
{
  return (regionPtr_p != 0  &&  regionPtr_p->hasMask());
}

template<class T>
const Lattice<Bool>& HDF5Image<T>::pixelMask() const
{
  if (regionPtr_p == 0) {
    throw (AipsError ("HDF5Image::pixelMask - no pixelmask used"));
  }
  return *regionPtr_p;
}
template<class T>
Lattice<Bool>& HDF5Image<T>::pixelMask()
{
  if (regionPtr_p == 0) {
    throw (AipsError ("HDF5Image::pixelMask - no pixelmask used"));
  }
  return *regionPtr_p;
}

template<class T>
const LatticeRegion* HDF5Image<T>::getRegionPtr() const
{
  return regionPtr_p;
}

template<class T>
void HDF5Image<T>::setDefaultMask (const String& regionName)
{
  // Use the new region as the image's mask.
  applyMask (regionName);
  // Store the new default name.
  ImageInterface<T>::setDefaultMask (regionName);
}

template <class T> 
void HDF5Image<T>::useMask (MaskSpecifier spec)
{
  applyMaskSpecifier (spec);
}

template<class T>
void HDF5Image<T>::applyMaskSpecifier (const MaskSpecifier& spec)
{
  // Use default mask if told to do so.
  // If there is no default, use no mask.
  String name = spec.name();
  if (spec.useDefault()) {
    name = getDefaultMask();
    if (! hasRegion (name, RegionHandler::Masks)) {
      name = String();
    }
  }
  applyMask (name);
}

template<class T>
void HDF5Image<T>::applyMask (const String& maskName)
{
  // No region if no mask name is given.
  if (maskName.empty()) {
    delete regionPtr_p;
    regionPtr_p = 0;
    return;
  }
  // Reconstruct the ImageRegion object.
  // Turn the region into lattice coordinates.
  ImageRegion* regPtr = getImageRegionPtr (maskName, RegionHandler::Masks);
  LatticeRegion* latReg = new LatticeRegion
                          (regPtr->toLatticeRegion (coordinates(), shape()));
  delete regPtr;
  // The mask has to cover the entire image.
  if (latReg->shape() != shape()) {
    delete latReg;
    throw (AipsError ("HDF5Image::setDefaultMask - region " + maskName +
		      " does not cover the full image"));
  }
  // Replace current by new mask.
  delete regionPtr_p;
  regionPtr_p = latReg;
}


template <class T> 
String HDF5Image<T>::name (Bool stripPath) const 
{
  return map_p.name (stripPath);
}

template <class T> 
IPosition HDF5Image<T>::shape() const
{
  return map_p.shape();
}

template <class T>
void HDF5Image<T>::resize (const TiledShape&)
{
  throw (AipsError ("HDF5Image::resize - an HDF5Image cannot be resized"));
}

template <class T> 
Bool HDF5Image<T>::doGetSlice(Array<T>& buffer, const Slicer& theSlice)
{
  return map_p.doGetSlice(buffer, theSlice);
}

template <class T> 
void HDF5Image<T>::doPutSlice(const Array<T>& sourceBuffer, 
                               const IPosition& where, 
                               const IPosition& stride)
{
    //  if (throughmask_p || !mask_p) {
  map_p.putSlice(sourceBuffer,where,stride);
    //  } else if (mask_p) {
    //    Array<T> map;
    //Array<Bool> mask;
    //IPosition shape(sourceBuffer.shape());
    //mask_p->getSlice(mask, where, shape, stride, True);
    //map_p.getSlice(map, where, shape, stride, True);
    // use maskedarrays to do all the work.
    //map(mask==False) = sourceBuffer;
    //map_p.putSlice(map,where,stride);
    //  } else {
    //    throw(AipsError("HDF5Image<T>::putSlice - throughmask==False but no "
    //		    "mask exists."));
    //  }
}


// apply a function to all elements of the map
template <class T> 
void HDF5Image<T>::apply(T (*function)(T)) 
{
    map_p.apply(function);
}

// apply a function to all elements of a const map;
template <class T> 
void HDF5Image<T>::apply(T (*function)(const T&))
{
    map_p.apply(function);
}

template <class T> 
void HDF5Image<T>::apply(const Functional<T,T>& function)
{
    map_p.apply(function);
}


template <class T> 
T HDF5Image<T>::getAt(const IPosition& where) const
{
   return map_p(where);
}

template <class T> 
void HDF5Image<T>::putAt(const T& value, const IPosition& where) {
    map_p.putAt (value, where);
}

template <class T> 
LatticeIterInterface<T>* HDF5Image<T>::makeIter
                                   (const LatticeNavigator& navigator,
				    Bool useRef) const
{
  return map_p.makeIter (navigator, useRef);
}

template <class T> 
Bool HDF5Image<T>::ok() const
{
  return (map_p.ndim() == coordinates().nPixelAxes());
}


template <class T> 
HDF5Image<T>& HDF5Image<T>::operator+= (const Lattice<T>& other)
{
  check_conformance(other);
  LatticeExpr<T> expr(*this + other);
  this->copyData (expr);
  return *this;
}

template<class T>
const CountedPtr<HDF5File>& HDF5Image<T>::getFile (void* imagePtr)
{
  HDF5Image<T>* im = static_cast<HDF5Image<T>*>(imagePtr);
  return im->map_p.file();
}


template<class T> 
void HDF5Image<T>::attach_logtable()
{
  open_logtable();
}

template<class T> 
void HDF5Image<T>::open_logtable()
{
  // No log table (yet?).
}

template <class T>
void HDF5Image<T>::restoreAll()
{
  // Restore the coordinates.
  Record rec = HDF5Record::readRecord (*map_p.group(), "coordinfo");
  CoordinateSystem* restoredCoords = CoordinateSystem::restore(rec, "coords");
  AlwaysAssert(restoredCoords != 0, AipsError);
  setCoordsMember (*restoredCoords);
  delete restoredCoords;
  // Restore the image info.
  rec = HDF5Record::readRecord (*map_p.group(), "imageinfo");
  restoreImageInfo (rec);
  // Restore the units.
  rec = HDF5Record::readRecord (*map_p.group(), "unitinfo");
  restoreUnits (rec);
  // Restore the miscinfo.
  rec = HDF5Record::readRecord (*map_p.group(), "miscinfo");
  restoreMiscInfo (rec);
  // Restore the mask/region info.
  dynamic_cast<RegionHandlerHDF5*>(this->getRegionHandler())->restore();
}

template <class T> 
Bool HDF5Image<T>::setCoordinateInfo (const CoordinateSystem& coords)
{
  Bool ok = ImageInterface<T>::setCoordinateInfo(coords);
  if (ok) {
    Record rec;
    AlwaysAssert (coordinates().save(rec, "coords"), AipsError);
    HDF5Record::writeRecord (*map_p.group(), "coordinfo", rec);
  }
  return ok;
}
  
template<class T> 
void HDF5Image<T>::restoreMiscInfo (const RecordInterface& rec)
{
  setMiscInfoMember (rec);
}

template<class T> 
Bool HDF5Image<T>::setMiscInfo (const RecordInterface& newInfo)
{
  setMiscInfoMember (newInfo);
  HDF5Record::writeRecord (*map_p.group(), "miscinfo", newInfo);
  return True;
}

template<class T> 
Bool HDF5Image<T>::setUnits(const Unit& newUnits) 
{
  setUnitMember (newUnits);
  Record rec;
  rec.define("units", newUnits.getName());
  HDF5Record::writeRecord (*map_p.group(), "unitinfo", rec);
  return True;
}

template<class T> 
void HDF5Image<T>::restoreUnits (const RecordInterface& rec)
{
  Unit retval;
  String unitName;
  if (rec.isDefined("units")) {
    if (rec.dataType("units") != TpString) {
      LogIO os;
      os << LogOrigin("HDF5Image<T>", "units()", WHERE)
	 << "'units' keyword in hdf5image is not a string! Units not restored." 
         << LogIO::SEVERE << LogIO::POST;
    } else {
      rec.get("units", unitName);
    }
  }
  if (! unitName.empty()) {
    // OK, non-empty unit, see if it's valid, if not try some known things to
    // make a valid unit out of it.
    if (! UnitVal::check(unitName)) {
      // Beam and Pixel are the most common undefined units
      UnitMap::putUser("Pixel",UnitVal(1.0),"Pixel unit");
      UnitMap::putUser("Beam",UnitVal(1.0),"Beam area");
    }
    if (! UnitVal::check(unitName)) {
      // OK, maybe we need FITS
      UnitMap::addFITS();
    }
    if (!UnitVal::check(unitName)) {
      LogIO os;
      os << LogOrigin("HDF5Image<T>", "units()", WHERE)
         << LogIO::SEVERE << "Unit '" << unitName
         << "' is unknown. Not restoring units" << LogIO::POST;
    } else {
      retval = Unit(unitName);
    }
  }
  setUnitMember (retval);
}


template<class T>
Bool HDF5Image<T>::setImageInfo (const ImageInfo& info) 
{
  Bool ok = ImageInterface<T>::setImageInfo(info);
  if (ok) {
    // Update the ImageInfo
    Record rec;
    String error;
    if (imageInfo().toRecord(error, rec)) {
      HDF5Record::writeRecord (*map_p.group(), "imageinfo", rec);
    } else {
      LogIO os;
      os << LogIO::SEVERE << "Error saving ImageInfo in record because " 
         << error << LogIO::POST;
      ok = False;
    }
  }
  return ok;
}

template<class T>
void HDF5Image<T>::restoreImageInfo (const RecordInterface& rec)
{
  String error;
  ImageInfo info;
  Bool ok = info.fromRecord (error, rec);
  if (!ok) {
    LogIO os;
    os << LogIO::WARN << "Failed to restore the ImageInfo because " 
       << error << LogIO::POST;
  } else {
    setImageInfoMember (info);
  }
}

template<class T> 
void HDF5Image<T>::removeRegion (const String& name,
				 RegionHandler::GroupType type,
				 Bool throwIfUnknown)
{
  // Remove the default mask if it is the region to be removed.
  if (name == getDefaultMask()) {
    setDefaultMask (String());
  }
  ImageInterface<T>::removeRegion (name, type, throwIfUnknown);
}


template<class T> 
void HDF5Image<T>::check_conformance(const Lattice<T>& other)
{
  if (! this->conform(other)) {
    throw AipsError("Shapes of image " + name() + " and other lattice do not conform");
  }
}

template<class T> 
uInt HDF5Image<T>::advisedMaxPixels() const
{
  return map_p.advisedMaxPixels();
}

template<class T> 
IPosition HDF5Image<T>::doNiceCursorShape(uInt maxPixels) const
{
  return map_p.niceCursorShape(maxPixels);
}

template<class T>
void HDF5Image<T>::flush()
{
  map_p.flush();
  logger().flush();
  if (regionPtr_p != 0) {
    regionPtr_p->flush();
  }
  itsAttrHandler.flush();
  // Save the mask/region info.
  dynamic_cast<RegionHandlerHDF5*>(this->getRegionHandler())->save();
}

template<class T>
ImageAttrHandler& HDF5Image<T>::attrHandler (Bool createHandler)
{
  return itsAttrHandler.attachHid (*map_p.group(), createHandler,
                                   map_p.isWritable());
}

} //# NAMESPACE CASACORE - END

#endif
