//# ExtendImage.cc: An extension of an ImageInterface object
//# Copyright (C) 2001,2003
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

#ifndef IMAGES_EXTENDIMAGE_TCC
#define IMAGES_EXTENDIMAGE_TCC

#include <casacore/images/Images/ExtendImage.h>
#include <casacore/lattices/Lattices/ExtendLattice.h>
#include <casacore/lattices/LRegions/LatticeRegion.h>
#include <casacore/coordinates/Coordinates/CoordinateUtil.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T>
ExtendImage<T>::ExtendImage()
: itsImagePtr  (0),
  itsExtLatPtr (0)
{}

template<class T>
ExtendImage<T>::ExtendImage (const ImageInterface<T>& image,
			     const IPosition& newShape,
			     const CoordinateSystem& newCsys)
: itsImagePtr (image.cloneII())
{
  IPosition newAxes, stretchAxes;
  if (! CoordinateUtil::findExtendAxes (newAxes, stretchAxes,
					newShape, image.shape(),
					newCsys, image.coordinates())) {
    throw AipsError ("ExtendImage - "
		     "new csys or shape incompatible with old ones");
  }
  itsExtLatPtr = new ExtendLattice<T> (image, newShape, newAxes, stretchAxes);
  setCoordsMember (newCsys);
  this->setImageInfoMember (itsImagePtr->imageInfo());
  this->setMiscInfoMember (itsImagePtr->miscInfo());
  this->setUnitMember (itsImagePtr->units());
  logger().addParent (itsImagePtr->logger());
}

template<class T>
ExtendImage<T>::ExtendImage (const ExtendImage<T>& other)
: ImageInterface<T> (other),
  itsImagePtr (other.itsImagePtr->cloneII())
{
  itsExtLatPtr = new ExtendLattice<T> (*other.itsExtLatPtr);
}

template<class T>
ExtendImage<T>::~ExtendImage()
{
  delete itsImagePtr;
  delete itsExtLatPtr;
}

template<class T>
ExtendImage<T>& ExtendImage<T>::operator= (const ExtendImage<T>& other)
{
  if (this != &other) {
    ImageInterface<T>::operator= (other);
    delete itsImagePtr;
    itsImagePtr = other.itsImagePtr->cloneII();
    delete itsExtLatPtr;
    itsExtLatPtr = new ExtendLattice<T> (*other.itsExtLatPtr);
  }
  return *this;
}

template<class T>
ImageInterface<T>* ExtendImage<T>::cloneII() const
{
  return new ExtendImage<T> (*this);
}

template<class T>
String ExtendImage<T>::imageType() const
{
  return "ExtendImage";
}


template <class T>
Bool ExtendImage<T>::ok() const
{
  return itsExtLatPtr->ok();
}

template<class T>
Bool ExtendImage<T>::isMasked() const
{
  return itsExtLatPtr->isMasked();
}

template<class T>
Bool ExtendImage<T>::isPersistent() const
{
  return itsExtLatPtr->isPersistent();
}

template<class T>
Bool ExtendImage<T>::isPaged() const
{
  return itsExtLatPtr->isPaged();
}

template<class T>
Bool ExtendImage<T>::isWritable() const
{
  return itsExtLatPtr->isWritable();
}

template<class T>
Bool ExtendImage<T>::hasPixelMask() const
{
  return itsExtLatPtr->hasPixelMask();
}

template<class T>
const Lattice<Bool>& ExtendImage<T>::pixelMask() const
{
  return itsExtLatPtr->pixelMask();
}
template<class T>
Lattice<Bool>& ExtendImage<T>::pixelMask()
{
  return itsExtLatPtr->pixelMask();
}

template<class T>
const LatticeRegion* ExtendImage<T>::getRegionPtr() const
{
  return itsExtLatPtr->getRegionPtr();
}

template<class T>
IPosition ExtendImage<T>::shape() const
{
  return itsExtLatPtr->shape();
}

template<class T>
void ExtendImage<T>::resize (const TiledShape&)
{
  throw (AipsError ("ExtendImage::resize is not possible"));
}

template<class T>
String ExtendImage<T>::name (Bool stripPath) const
{
  return itsImagePtr->name (stripPath);
}
  
template<class T>
ImageAttrHandler& ExtendImage<T>::attrHandler (Bool createHandler)
{
  return itsImagePtr->attrHandler (createHandler);
}

template<class T>
Bool ExtendImage<T>::doGetSlice (Array<T>& buffer,
			      const Slicer& section)
{
  return itsExtLatPtr->doGetSlice (buffer, section);
}

template<class T>
void ExtendImage<T>::doPutSlice (const Array<T>& sourceBuffer,
				 const IPosition& where, 
				 const IPosition& stride)
{
  itsExtLatPtr->doPutSlice (sourceBuffer, where, stride);
}

template<class T>
Bool ExtendImage<T>::doGetMaskSlice (Array<Bool>& buffer,
				     const Slicer& section)
{
  return itsExtLatPtr->doGetMaskSlice (buffer, section);
}

template<class T>
uInt ExtendImage<T>::advisedMaxPixels() const
{
  return itsExtLatPtr->advisedMaxPixels();
}

template<class T>
IPosition ExtendImage<T>::doNiceCursorShape (uInt maxPixels) const
{
  return itsExtLatPtr->niceCursorShape (maxPixels);
}

template<class T>
LatticeIterInterface<T>* ExtendImage<T>::makeIter
                               (const LatticeNavigator& navigator,
				Bool useRef) const
{
  return itsExtLatPtr->makeIter (navigator, useRef);
}

template<class T>
Bool ExtendImage<T>::lock (FileLocker::LockType type, uInt nattempts)
{
  return itsExtLatPtr->lock (type, nattempts);
}
template<class T>
void ExtendImage<T>::unlock()
{
  itsExtLatPtr->unlock();
  itsImagePtr->unlock();
}
template<class T>
Bool ExtendImage<T>::hasLock (FileLocker::LockType type) const
{
  return itsExtLatPtr->hasLock (type);
}
template<class T>
void ExtendImage<T>::resync()
{
  itsExtLatPtr->resync();
  itsImagePtr->resync();
}
template<class T>
void ExtendImage<T>::flush()
{
  itsImagePtr->flush();
}
template<class T>
void ExtendImage<T>::tempClose()
{
  itsExtLatPtr->tempClose();
  itsImagePtr->tempClose();
  logger().tempClose();
}
template<class T>
void ExtendImage<T>::reopen()
{
  itsImagePtr->reopen();
}

} //# NAMESPACE CASACORE - END


#endif
