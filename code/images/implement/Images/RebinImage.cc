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
//#
//# $Id$

#include <trial/Images/RebinImage.h>
#include <trial/Lattices/RebinLattice.h>
#include <trial/Lattices/LatticeRegion.h>
#include <trial/Coordinates/CoordinateUtil.h>
#include <aips/Arrays/IPosition.h>
#include <aips/Arrays/Vector.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>

template<class T>
RebinImage<T>::RebinImage ()
: itsImagePtr (0)
{}

template<class T>
RebinImage<T>::RebinImage (const ImageInterface<T>& image,
                           const Vector<uInt>& factors, Bool doCache)
: itsImagePtr (image.cloneII())
{
  itsRebinPtr = new RebinLattice<T>(image, factors, doCache);
//
  CoordinateSystem cSys = makeOutputCoordinates (factors, image.coordinates(),
                                                 image.shape(), itsRebinPtr->shape());
  setCoordsMember (cSys);
//
  setImageInfoMember (itsImagePtr->imageInfo());
  setMiscInfoMember (itsImagePtr->miscInfo());
  setUnitMember (itsImagePtr->units());
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
    ImageInterface<T>::operator= (other);
    delete itsImagePtr;
    itsImagePtr = other.itsImagePtr->cloneII();
    delete itsRebinPtr;
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
Bool RebinImage<T>::ok() const
{
  return itsRebinPtr->ok();
}

template<class T>
Bool RebinImage<T>::isMasked() const
{
  return itsRebinPtr->isMasked();
}

template<class T>
Bool RebinImage<T>::isPersistent() const
{
  return itsRebinPtr->isPersistent();
}

template<class T>
Bool RebinImage<T>::isPaged() const
{
  return itsRebinPtr->isPaged();
}

template<class T>
Bool RebinImage<T>::isWritable() const
{
  return itsRebinPtr->isWritable();
}

template<class T>
Bool RebinImage<T>::hasPixelMask() const
{
  return itsRebinPtr->hasPixelMask();
}

template<class T>
const Lattice<Bool>& RebinImage<T>::pixelMask() const
{
  return itsRebinPtr->pixelMask();
}

template<class T>
Lattice<Bool>& RebinImage<T>::pixelMask()
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
String RebinImage<T>::name (Bool stripPath) const
{
  return itsImagePtr->name (stripPath);
}
  
template<class T>
Bool RebinImage<T>::doGetSlice (Array<T>& buffer,
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
Bool RebinImage<T>::doGetMaskSlice (Array<Bool>& buffer,
				       const Slicer& section)
{
  return itsRebinPtr->doGetMaskSlice (buffer, section);
}

template<class T>
uInt RebinImage<T>::advisedMaxPixels() const
{
  return itsRebinPtr->advisedMaxPixels();
}

template<class T>
IPosition RebinImage<T>::doNiceCursorShape (uInt maxPixels) const
{
  return itsRebinPtr->niceCursorShape (maxPixels);
}

template<class T>
LatticeIterInterface<T>* RebinImage<T>::makeIter
                               (const LatticeNavigator& navigator,
				Bool useRef) const
{
  return itsRebinPtr->makeIter (navigator, useRef);
}

template<class T>
Bool RebinImage<T>::lock (FileLocker::LockType type, uInt nattempts)
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
Bool RebinImage<T>::hasLock (FileLocker::LockType type) const
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


template<class T>
CoordinateSystem RebinImage<T>::makeOutputCoordinates (const Vector<uInt>& factors,
                                                       const CoordinateSystem& cSysIn,
                                                       const IPosition& shapeIn,
                                                       const IPosition& shapeOut) const
{
// Check Stokes.

   const uInt nDim = shapeIn.nelements();
   Int coord, axisInCoord;
//
   for (uInt i=0; i<cSysIn.nPixelAxes(); i++) {
      cSysIn.findPixelAxis(coord, axisInCoord, i);
      if (cSysIn.type(coord) == Coordinate::STOKES) {
         if (shapeIn(i) != shapeOut(i)) {
            throw (AipsError ("You cannot rebin a Stokes axis"));
         }
      }
   }

// Set output values

   Vector<Double> incrIn(cSysIn.increment().copy());
   Vector<Double> incrOut(incrIn.copy());
   Vector<Double> refPixIn(cSysIn.referencePixel().copy());
   Vector<Double> refPixOut(refPixIn.copy());

// Loop over pixel axes

   for (uInt pA=0; pA<nDim; pA++) { 
     uInt wA = cSysIn.pixelAxisToWorldAxis(pA);
//
     incrOut(wA) *= Double(factors[pA]);
     refPixOut(pA) = (refPixIn(pA) + 0.5) / factors[pA] - 0.5;
   } 
//
    CoordinateSystem cSysOut(cSysIn);
    cSysOut.setReferencePixel(refPixOut);
    cSysOut.setIncrement(incrOut);
//
    return cSysOut;
}
 
