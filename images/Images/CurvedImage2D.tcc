//# CurvedImage2D.cc: An image crosscut based on a curve in a plane
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

#ifndef IMAGES_CURVEDIMAGE2D_TCC
#define IMAGES_CURVEDIMAGE2D_TCC

#include <casacore/images/Images/CurvedImage2D.h>
#include <casacore/lattices/Lattices/CurvedLattice2D.h>
#include <casacore/lattices/LRegions/LatticeRegion.h>
#include <casacore/coordinates/Coordinates/CoordinateUtil.h>
#include <casacore/coordinates/Coordinates/CoordinateSystem.h>
#include <casacore/coordinates/Coordinates/LinearCoordinate.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T>
CurvedImage2D<T>::CurvedImage2D()
: itsImagePtr  (0),
  itsCurLatPtr (0)
{}

template<class T>
CurvedImage2D<T>::CurvedImage2D (const ImageInterface<T>& image,
				 const CLInterpolator2D<T>& interpolator,
				 const PixelCurve1D& curve,
				 uint32_t axis1, uint32_t axis2,
				 int32_t curveAxis)
: itsImagePtr (image.cloneII())
{
  itsCurLatPtr = new CurvedLattice2D<T> (image, interpolator, curve,
					 axis1, axis2, curveAxis);

// Currently the output CS is an arbitrary Linear system
// A correct CS needs to be set. Probably some new Coordinates
// need to be derived to do this.  

  CoordinateSystem cSysOut;
  LinearCoordinate c(itsCurLatPtr->ndim());
  cSysOut.addCoordinate(c);  
  cSysOut.setObsInfo(itsImagePtr->coordinates().obsInfo());
//
  setCoordsMember (cSysOut);
  setImageInfoMember (itsImagePtr->imageInfo());
  setMiscInfoMember (itsImagePtr->miscInfo());
  setUnitMember (itsImagePtr->units());
  logger().addParent (itsImagePtr->logger());
}

template<class T>
CurvedImage2D<T>::CurvedImage2D (const CurvedImage2D<T>& other)
: ImageInterface<T> (other),
  itsImagePtr (other.itsImagePtr->cloneII())
{
  itsCurLatPtr = new CurvedLattice2D<T> (*other.itsCurLatPtr);
}

template<class T>
CurvedImage2D<T>::~CurvedImage2D()
{
  delete itsImagePtr;
  delete itsCurLatPtr;
}

template<class T>
CurvedImage2D<T>& CurvedImage2D<T>::operator= (const CurvedImage2D<T>& other)
{
  if (this != &other) {
    ImageInterface<T>::operator= (other);
    delete itsImagePtr;
    itsImagePtr = other.itsImagePtr->cloneII();
    delete itsCurLatPtr;
    itsCurLatPtr = new CurvedLattice2D<T> (*other.itsCurLatPtr);
  }
  return *this;
}

template<class T>
ImageInterface<T>* CurvedImage2D<T>::cloneII() const
{
  return new CurvedImage2D<T> (*this);
}

template<class T>
String CurvedImage2D<T>::imageType() const
{
  return "CurvedImage2D";
}


template <class T>
bool CurvedImage2D<T>::ok() const
{
  return itsCurLatPtr->ok();
}

template<class T>
bool CurvedImage2D<T>::isMasked() const
{
  return itsCurLatPtr->isMasked();
}

template<class T>
bool CurvedImage2D<T>::isPersistent() const
{
  return itsCurLatPtr->isPersistent();
}

template<class T>
bool CurvedImage2D<T>::isPaged() const
{
  return itsCurLatPtr->isPaged();
}

template<class T>
bool CurvedImage2D<T>::isWritable() const
{
  return itsCurLatPtr->isWritable();
}

template<class T>
bool CurvedImage2D<T>::hasPixelMask() const
{
  return itsCurLatPtr->hasPixelMask();
}

template<class T>
const Lattice<bool>& CurvedImage2D<T>::pixelMask() const
{
  return itsCurLatPtr->pixelMask();
}
template<class T>
Lattice<bool>& CurvedImage2D<T>::pixelMask()
{
  return itsCurLatPtr->pixelMask();
}

template<class T>
const LatticeRegion* CurvedImage2D<T>::getRegionPtr() const
{
  return itsCurLatPtr->getRegionPtr();
}

template<class T>
IPosition CurvedImage2D<T>::shape() const
{
  return itsCurLatPtr->shape();
}

template<class T>
void CurvedImage2D<T>::resize (const TiledShape&)
{
  throw (AipsError ("CurvedImage2D::resize is not possible"));
}

template<class T>
String CurvedImage2D<T>::name (bool stripPath) const
{
  return itsImagePtr->name (stripPath);
}
  
template<class T>
ImageAttrHandler& CurvedImage2D<T>::attrHandler (bool createHandler)
{
  return itsImagePtr->attrHandler (createHandler);
}

template<class T>
bool CurvedImage2D<T>::doGetSlice (Array<T>& buffer,
				   const Slicer& section)
{
  return itsCurLatPtr->doGetSlice (buffer, section);
}

template<class T>
void CurvedImage2D<T>::doPutSlice (const Array<T>& sourceBuffer,
				   const IPosition& where, 
				   const IPosition& stride)
{
  itsCurLatPtr->doPutSlice (sourceBuffer, where, stride);
}

template<class T>
bool CurvedImage2D<T>::doGetMaskSlice (Array<bool>& buffer,
				       const Slicer& section)
{
  return itsCurLatPtr->doGetMaskSlice (buffer, section);
}

template<class T>
uint32_t CurvedImage2D<T>::advisedMaxPixels() const
{
  return itsCurLatPtr->advisedMaxPixels();
}

template<class T>
IPosition CurvedImage2D<T>::doNiceCursorShape (uint32_t maxPixels) const
{
  return itsCurLatPtr->niceCursorShape (maxPixels);
}

template<class T>
LatticeIterInterface<T>* CurvedImage2D<T>::makeIter
                               (const LatticeNavigator& navigator,
				bool useRef) const
{
  return itsCurLatPtr->makeIter (navigator, useRef);
}

template<class T>
bool CurvedImage2D<T>::lock (FileLocker::LockType type, uint32_t nattempts)
{
  return itsCurLatPtr->lock (type, nattempts);
}
template<class T>
void CurvedImage2D<T>::unlock()
{
  itsCurLatPtr->unlock();
  itsImagePtr->unlock();
}
template<class T>
bool CurvedImage2D<T>::hasLock (FileLocker::LockType type) const
{
  return itsCurLatPtr->hasLock (type);
}
template<class T>
void CurvedImage2D<T>::resync()
{
  itsCurLatPtr->resync();
  itsImagePtr->resync();
}
template<class T>
void CurvedImage2D<T>::flush()
{
  itsImagePtr->flush();
}
template<class T>
void CurvedImage2D<T>::tempClose()
{
  itsCurLatPtr->tempClose();
  itsImagePtr->tempClose();
  logger().tempClose();
}
template<class T>
void CurvedImage2D<T>::reopen()
{
  itsImagePtr->reopen();
}

} //# NAMESPACE CASACORE - END


#endif
