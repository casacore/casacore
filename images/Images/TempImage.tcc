//# TempImage.cc: defines the TempImage class
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

#ifndef IMAGES_TEMPIMAGE_TCC
#define IMAGES_TEMPIMAGE_TCC

#include <casacore/images/Images/TempImage.h>
#include <casacore/images/Regions/RegionHandlerMemory.h>
#include <casacore/images/Regions/ImageRegion.h>
#include <casacore/lattices/Lattices/TempLattice.h>
#include <casacore/lattices/LRegions/LatticeRegion.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

template <class T>
TempImage<T>::TempImage()
: ImageInterface<T> (RegionHandlerMemory()),
  mapPtr_p          (new TempLattice<T>),
  maskPtr_p         (0)
{} 
 
template <class T>
TempImage<T>::TempImage (const TiledShape& mapShape,
			 const CoordinateSystem& coordinateInfo,
			 Double maxMemoryInMb)
: ImageInterface<T> (RegionHandlerMemory()),
  mapPtr_p          (new TempLattice<T> (mapShape, maxMemoryInMb)),
  maskPtr_p         (0)
{
    AlwaysAssert(setCoordinateInfo (coordinateInfo), AipsError);
}

template <class T>
TempImage<T>::TempImage (const TiledShape& mapShape,
			 const CoordinateSystem& coordinateInfo,
			 Int maxMemoryInMb)
: ImageInterface<T> (RegionHandlerMemory()),
  mapPtr_p          (new TempLattice<T> (mapShape, maxMemoryInMb)),
  maskPtr_p         (0)
{
    AlwaysAssert(setCoordinateInfo (coordinateInfo), AipsError);
}

template <class T>
TempImage<T>::TempImage (const TempImage<T>& other)
: ImageInterface<T> (other),
  mapPtr_p          (new TempLattice<T> (*other.mapPtr_p)),
  maskPtr_p         (0)
{
  if (other.maskPtr_p != 0) {
    maskPtr_p = other.maskPtr_p->clone();
  }
}
 
template <class T>
TempImage<T>& TempImage<T>::operator= (const TempImage<T>& other)
{
  if (this != &other) {
    delete mapPtr_p;
    mapPtr_p = 0;
    delete maskPtr_p;
    maskPtr_p = 0;
    ImageInterface<T>::operator= (other);
    mapPtr_p = new TempLattice<T> (*other.mapPtr_p);
    if (other.maskPtr_p != 0) {
      maskPtr_p = other.maskPtr_p->clone();
    }
  }
  return *this;
} 
 
template <class T>
TempImage<T>::~TempImage()
{
  delete mapPtr_p;
  delete maskPtr_p;
}


template <class T>
ImageInterface<T>* TempImage<T>::cloneII() const
{
  return new TempImage (*this);
}


template <class T>
String TempImage<T>::imageType() const
{
  return "TempImage";
}

template<class T>
Bool TempImage<T>::isPaged() const
{
  return mapPtr_p->isPaged();
}

template<class T>
Bool TempImage<T>::canReferenceArray() const
{
  return mapPtr_p->canReferenceArray();
}

template <class T>
Bool TempImage<T>::isWritable() const
{  
  return mapPtr_p->isWritable();
}


template<class T>
void TempImage<T>::flush()
{
  mapPtr_p->flush();
}

template<class T>
void TempImage<T>::tempClose()
{
  mapPtr_p->tempClose();
}

template<class T>
void TempImage<T>::reopen()
{
  mapPtr_p->reopen();
}


template<class T>
void TempImage<T>::setDefaultMask (const String& regionName)
{
  // Use the new region as the image's mask.
  applyMask (regionName);
  // Store the new default name.
  ImageInterface<T>::setDefaultMask (regionName);
}

template<class T>
void TempImage<T>::useMask (MaskSpecifier spec)
{
  applyMaskSpecifier (spec);
}

template<class T>
void TempImage<T>::applyMaskSpecifier (const MaskSpecifier& spec)
{
  // Use default mask if told to do so.
  // If it does not exist, use no mask.
  String name = spec.name();
  if (spec.useDefault()) {
    name = getDefaultMask();
    if (! hasRegion (name, RegionHandler::Masks)) {
      name = "";
    }
  }
  applyMask (name);
}

template<class T>
void TempImage<T>::applyMask (const String& maskName)
{
  // No region if no mask name is given.
  if (maskName.empty()) {
    delete maskPtr_p;
    maskPtr_p = 0;
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
    throw (AipsError ("TempImage::setDefaultMask - region " + maskName +
		      " does not cover the full image"));
  }
  // Replace current by new mask.
  delete maskPtr_p;
  maskPtr_p = latReg;
}

template<class T>
void TempImage<T>::attachMask (const Lattice<Bool>& mask)
{
  if (! shape().isEqual (mask.shape())) {
    throw (AipsError ("TempImage::attachMask - "
		      "shapes of lattice and mask mismatch"));
  }
  if (maskPtr_p) {
     delete maskPtr_p;
     maskPtr_p = 0;
  }
  maskPtr_p = mask.clone();
}

template<class T> 
void TempImage<T>::removeRegion (const String& name,
				 RegionHandler::GroupType type,
				 Bool throwIfUnknown)
{
  // Remove the default mask if it is the region to be removed.
  if (name == getDefaultMask()) {
    setDefaultMask ("");
  }
  ImageInterface<T>::removeRegion (name, type, throwIfUnknown);
}

template<class T>
Bool TempImage<T>::isMasked() const
{
  return (maskPtr_p != 0);
}

template<class T>
Bool TempImage<T>::hasPixelMask() const
{
  return (maskPtr_p != 0);
}

template<class T>
const Lattice<Bool>& TempImage<T>::pixelMask() const
{
  if (maskPtr_p == 0) {
    throw (AipsError ("TempImage::pixelMask - no mask attached"));
  }
  return *maskPtr_p;
}

template<class T>
Lattice<Bool>& TempImage<T>::pixelMask()
{
  if (maskPtr_p == 0) {
    throw (AipsError ("TempImage::pixelMask - no mask attached"));
  }
  return *maskPtr_p;
}


template<class T>
Bool TempImage<T>::doGetMaskSlice (Array<Bool>& buffer, const Slicer& section)
{
  // If no mask, base implementation returns a True mask.
  if (maskPtr_p == 0) {
    return MaskedLattice<T>::doGetMaskSlice (buffer, section);
  }
  return maskPtr_p->doGetSlice (buffer, section);
}


template<class T>
const LatticeRegion* TempImage<T>::getRegionPtr() const
{
  return 0;
}

template <class T>
IPosition TempImage<T>::shape() const  
{ 
  return mapPtr_p->shape();
}

template <class T>
void TempImage<T>::resize (const TiledShape& newShape)
{
  delete mapPtr_p;
  mapPtr_p = new TempLattice<T> (newShape);
}

template <class T>
Bool TempImage<T>::doGetSlice (Array<T>& buffer,
			       const Slicer& section)
{
  return mapPtr_p->doGetSlice (buffer, section);
} 
   

template <class T>
void TempImage<T>::doPutSlice (const Array<T>& buffer,
			       const IPosition& where,
			       const IPosition& stride)
{
  mapPtr_p->doPutSlice (buffer, where, stride);
}


template <class T> 
String TempImage<T>::name (Bool) const
{
  return String ("Temporary_Image");
}


template<class T>
void TempImage<T>::set (const T& value)
{
  mapPtr_p->set (value);
}

template<class T>
void TempImage<T>::apply (T (*function)(T))
{
  mapPtr_p->apply (function);
}

template<class T>
void TempImage<T>::apply (T (*function)(const T&))
{
  mapPtr_p->apply (function);
}

template<class T>
void TempImage<T>::apply (const Functional<T,T>& function)
{
  mapPtr_p->apply (function);
}

template<class T>
uInt TempImage<T>::advisedMaxPixels() const
{
  return mapPtr_p->advisedMaxPixels();
}

template<class T>
IPosition TempImage<T>::doNiceCursorShape (uInt maxPixels) const
{
  return mapPtr_p->niceCursorShape (maxPixels);
}

template<class T>
uInt TempImage<T>::maximumCacheSize() const
{
  return mapPtr_p->maximumCacheSize();
}

template<class T>
void TempImage<T>::setMaximumCacheSize (uInt howManyPixels)
{
  mapPtr_p->setMaximumCacheSize (howManyPixels);
}

template<class T>
void TempImage<T>::setCacheSizeFromPath (const IPosition& sliceShape,
					 const IPosition& windowStart,
					 const IPosition& windowLength,
					 const IPosition& axisPath)
{
  mapPtr_p->setCacheSizeFromPath (sliceShape, windowStart, windowLength,
				  axisPath);
}

template<class T>
void TempImage<T>::setCacheSizeInTiles (uInt howManyTiles)
{
  mapPtr_p->setCacheSizeInTiles (howManyTiles);
}

template<class T>
void TempImage<T>::clearCache()
{
  mapPtr_p->clearCache();
}

template<class T>
void TempImage<T>::showCacheStatistics (ostream& os) const
{
  mapPtr_p->showCacheStatistics (os);
}


template<class T>
T TempImage<T>::getAt (const IPosition& where) const
{
  return mapPtr_p->getAt (where);
}

template<class T>
void TempImage<T>::putAt (const T& value, const IPosition& where)
{
  mapPtr_p->putAt (value, where);
}


template <class T>
Bool TempImage<T>::ok() const
{
  return mapPtr_p->ok();
}  


template <class T>
LatticeIterInterface<T>* TempImage<T>::makeIter
                                (const LatticeNavigator& navigator,
				 Bool useRef) const
{
  return mapPtr_p->makeIter (navigator, useRef);
}


} //# NAMESPACE CASACORE - END


#endif
