//# RebinImage.cc: rebin an image
//# Copyright (C) 2003
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

#ifndef IMAGES_REBINIMAGE_TCC
#define IMAGES_REBINIMAGE_TCC

#include <casacore/images/Images/RebinImage.h>
#include <casacore/lattices/Lattices/RebinLattice.h>
#include <casacore/lattices/LRegions/LatticeRegion.h>
#include <casacore/coordinates/Coordinates/CoordinateUtil.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T>
RebinImage<T>::RebinImage ()
: itsImagePtr (0),
  itsRebinPtr (0)
{}

template<class T>
RebinImage<T>::RebinImage (const ImageInterface<T>& image,
                           const IPosition& factors)
: itsImagePtr (image.cloneII())
{
  ThrowIf (
           image.imageInfo().hasMultipleBeams()
           && image.coordinates().hasSpectralAxis()
           && factors[image.coordinates().spectralAxisNumber()] != 1,
           "This image has multiple beams. The spectral axis cannot be rebinned"
           );
  itsRebinPtr = new RebinLattice<T>(image, factors);
//
  CoordinateSystem cSys = 
     CoordinateUtil::makeBinnedCoordinateSystem (factors, image.coordinates(), true);
  setCoordsMember (cSys);
//
  this->setImageInfoMember (itsImagePtr->imageInfo());
  this->setMiscInfoMember (itsImagePtr->miscInfo());
  this->setUnitMember (itsImagePtr->units());
  logger().addParent (itsImagePtr->logger());
}

template<class T>
RebinImage<T>::RebinImage (const RebinImage<T>& other)
: ImageInterface<T> (other),
  itsImagePtr (other.itsImagePtr->cloneII())
{
  itsRebinPtr = new RebinLattice<T> (*other.itsRebinPtr);
}

template<class T>
RebinImage<T>::~RebinImage()
{
  delete itsImagePtr;
  delete itsRebinPtr;
}

template<class T>
RebinImage<T>& RebinImage<T>::operator= (const RebinImage<T>& other)
{
  if (this != &other) {
    delete itsImagePtr;
    itsImagePtr = 0;
    delete itsRebinPtr;
    itsRebinPtr = 0;
    ImageInterface<T>::operator= (other);
    itsImagePtr = other.itsImagePtr->cloneII();
    itsRebinPtr = new RebinLattice<T> (*other.itsRebinPtr);
  }
  return *this;
}

template<class T>
ImageInterface<T>* RebinImage<T>::cloneII() const
{
  return new RebinImage<T> (*this);
}

template<class T>
String RebinImage<T>::imageType() const
{
  return "RebinImage";
}


template <class T>
bool RebinImage<T>::ok() const
{
  return itsRebinPtr->ok();
}

template<class T>
bool RebinImage<T>::isMasked() const
{
  return itsRebinPtr->isMasked();
}

template<class T>
bool RebinImage<T>::isPersistent() const
{
  return itsRebinPtr->isPersistent();
}

template<class T>
bool RebinImage<T>::isPaged() const
{
  return itsRebinPtr->isPaged();
}

template<class T>
bool RebinImage<T>::isWritable() const
{
  return itsRebinPtr->isWritable();
}

template<class T>
bool RebinImage<T>::hasPixelMask() const
{
  return itsRebinPtr->hasPixelMask();
}

template<class T>
const Lattice<bool>& RebinImage<T>::pixelMask() const
{
  return itsRebinPtr->pixelMask();
}

template<class T>
Lattice<bool>& RebinImage<T>::pixelMask()
{
  return itsRebinPtr->pixelMask();
}

template<class T>
const LatticeRegion* RebinImage<T>::getRegionPtr() const
{
  return itsRebinPtr->getRegionPtr();
}

template<class T>
IPosition RebinImage<T>::shape() const
{
  return itsRebinPtr->shape();
}

template<class T>
void RebinImage<T>::resize (const TiledShape&)
{
  throw (AipsError ("RebinImage::resize is not possible"));
}

template<class T>
String RebinImage<T>::name (bool stripPath) const
{
  return itsImagePtr->name (stripPath);
}
  
template<class T>
ImageAttrHandler& RebinImage<T>::attrHandler (bool createHandler)
{
  return itsImagePtr->attrHandler (createHandler);
}

template<class T>
bool RebinImage<T>::doGetSlice (Array<T>& buffer,
    			        const Slicer& section)
{
  return itsRebinPtr->doGetSlice (buffer, section);
}

template<class T>
void RebinImage<T>::doPutSlice (const Array<T>& sourceBuffer,
   			        const IPosition& where, 
				const IPosition& stride)
{
  itsRebinPtr->doPutSlice (sourceBuffer, where, stride);
}

template<class T>
bool RebinImage<T>::doGetMaskSlice (Array<bool>& buffer,
				       const Slicer& section)
{
  return itsRebinPtr->doGetMaskSlice (buffer, section);
}

template<class T>
uint32_t RebinImage<T>::advisedMaxPixels() const
{
  return itsRebinPtr->advisedMaxPixels();
}

template<class T>
IPosition RebinImage<T>::doNiceCursorShape (uint32_t maxPixels) const
{
  return itsRebinPtr->niceCursorShape (maxPixels);
}

template<class T>
LatticeIterInterface<T>* RebinImage<T>::makeIter
                               (const LatticeNavigator& navigator,
				bool useRef) const
{
  return itsRebinPtr->makeIter (navigator, useRef);
}

template<class T>
bool RebinImage<T>::lock (FileLocker::LockType type, uint32_t nattempts)
{
  return itsRebinPtr->lock (type, nattempts);
}
template<class T>
void RebinImage<T>::unlock()
{
  itsRebinPtr->unlock();
  itsImagePtr->unlock();
}
template<class T>
bool RebinImage<T>::hasLock (FileLocker::LockType type) const
{
  return itsRebinPtr->hasLock (type);
}
template<class T>
void RebinImage<T>::resync()
{
  itsRebinPtr->resync();
  itsImagePtr->resync();
}
template<class T>
void RebinImage<T>::flush()
{
  itsImagePtr->flush();
}
template<class T>
void RebinImage<T>::tempClose()
{
  itsRebinPtr->tempClose();
  itsImagePtr->tempClose();
  logger().tempClose();
}
template<class T>
void RebinImage<T>::reopen()
{
  itsImagePtr->reopen();
}

} //# NAMESPACE CASACORE - END


#endif
