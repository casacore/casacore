//# SubImage.cc: A subset of a Image
//# Copyright (C) 1998
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

#include <trial/Images/SubImage.h>
#include <trial/Images/ImageRegion.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Arrays/Vector.h>
#include <aips/Measures/UnitMap.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>

typedef Vector<Int> gppbug_subimage;


template<class T>
SubImage<T>::SubImage()
: ImageInterface<T> (),
  SubLattice<T>     (),
  itsImagePtr       (0),
  itsSubImagePtr    (0)
{}

template<class T>
SubImage<T>::SubImage (const ImageInterface<T>& image)
: ImageInterface<T> (),
  itsImagePtr (image.cloneII()),
  itsSubImagePtr    (0)
{
  setPtr (itsImagePtr, 0, False);
  setRegion();
  coords_p = image.coordinates();
}

template<class T>
SubImage<T>::SubImage (ImageInterface<T>& image,
		       Bool writableIfPossible)
: ImageInterface<T> (),
  itsImagePtr (image.cloneII()),
  itsSubImagePtr    (0)
{
  setPtr (itsImagePtr, 0, writableIfPossible);
  setRegion();
  coords_p = image.coordinates();
}

template<class T>
SubImage<T>::SubImage (const ImageInterface<T>& image,
		       const ImageRegion& imageRegion)
: ImageInterface<T> (),
  itsImagePtr (image.cloneII()),
  itsSubImagePtr    (0)
{
  setPtr (itsImagePtr, 0, False);
  setRegion (imageRegion.toLCRegion (image.coordinates()));
  const Slicer& slicer = region().slicer();
  coords_p = image.coordinates().subImage (slicer.start().asVector(),
					   slicer.stride().asVector());
}

template<class T>
SubImage<T>::SubImage (ImageInterface<T>& image,
		       const ImageRegion& imageRegion,
		       Bool writableIfPossible)
: ImageInterface<T> (),
  itsImagePtr (image.cloneII()),
  itsSubImagePtr    (0)
{
  setPtr (itsImagePtr, 0, writableIfPossible);
  setRegion (imageRegion.toLCRegion (image.coordinates()));
  const Slicer& slicer = region().slicer();
  coords_p = image.coordinates().subImage (slicer.start().asVector(),
					   slicer.stride().asVector());
}

template<class T>
SubImage<T>::SubImage (const SubImage<T>& image,
		       const ImageRegion& imageRegion)
: ImageInterface<T> (),
  itsSubImagePtr    (new SubImage<T>(image))
{
  itsImagePtr = itsSubImagePtr;
  setPtr (0, itsSubImagePtr, False);
  setRegion (imageRegion.toLCRegion (image.coordinates()));
  const Slicer& slicer = region().slicer();
  coords_p = image.coordinates().subImage (slicer.start().asVector(),
					   slicer.stride().asVector());
}

template<class T>
SubImage<T>::SubImage (SubImage<T>& image,
		       const ImageRegion& imageRegion,
		       Bool writableIfPossible)
: ImageInterface<T> (),
  itsSubImagePtr    (new SubImage<T>(image))
{
  itsImagePtr = itsSubImagePtr;
  setPtr (0, itsSubImagePtr, writableIfPossible);
  setRegion (imageRegion.toLCRegion (image.coordinates()));
  const Slicer& slicer = region().slicer();
  coords_p = image.coordinates().subImage (slicer.start().asVector(),
					   slicer.stride().asVector());
}

template<class T>
SubImage<T>::SubImage (const ImageInterface<T>& image,
		       const Slicer& slicer)
: ImageInterface<T> (),
  itsImagePtr       (image.cloneII()),
  itsSubImagePtr    (0)
{
  setPtr (itsImagePtr, 0, False);
  setRegion (slicer);
  coords_p = image.coordinates().subImage (slicer.start().asVector(),
					   slicer.stride().asVector());
}

template<class T>
SubImage<T>::SubImage (ImageInterface<T>& image,
		       const Slicer& slicer,
		       Bool writableIfPossible)
: ImageInterface<T> (),
  itsImagePtr       (image.cloneII()),
  itsSubImagePtr    (0)
{
  setPtr (itsImagePtr, 0, writableIfPossible);
  setRegion (slicer);
  coords_p = image.coordinates().subImage (slicer.start().asVector(),
					   slicer.stride().asVector());
}

template<class T>
SubImage<T>::SubImage (const SubImage<T>& image,
		       const Slicer& slicer)
: ImageInterface<T> (),
  itsSubImagePtr    (new SubImage<T>(image))
{
  itsImagePtr = itsSubImagePtr;
  setPtr (0, itsSubImagePtr, False);
  setRegion (slicer);
  coords_p = image.coordinates().subImage (slicer.start().asVector(),
					   slicer.stride().asVector());
}

template<class T>
SubImage<T>::SubImage (SubImage<T>& image,
		       const Slicer& slicer,
		       Bool writableIfPossible)
: ImageInterface<T> (),
  itsSubImagePtr    (new SubImage<T>(image))
{
  itsImagePtr = itsSubImagePtr;
  setPtr (0, itsSubImagePtr, writableIfPossible);
  setRegion (slicer);
  coords_p = image.coordinates().subImage (slicer.start().asVector(),
					   slicer.stride().asVector());
}

template<class T>
SubImage<T>::SubImage (const SubImage<T>& other)
: ImageInterface<T> (),
  itsImagePtr       (0),
  itsSubImagePtr    (0)
{
  operator= (other);
}

template<class T>
SubImage<T>::~SubImage()
{
  // Note that itsImagePtr is the same as itsLatticePtr in the base class.
  // So the object gets already deleted in ~SubLattice.
}

template<class T>
SubImage<T>& SubImage<T>::operator= (const SubImage<T>& other)
{
  if (this != &other) {
    ImageInterface<T>::operator= (other);
    itsSubImagePtr = other.itsSubImagePtr;
    if (itsSubImagePtr == 0) {
      itsImagePtr = other.itsImagePtr->cloneII();
    } else {
      itsSubImagePtr = new SubImage<T>(*itsSubImagePtr);
      itsImagePtr    = itsSubImagePtr;
    }
    setPtr (itsImagePtr, itsSubImagePtr, other.isWritable());
    setRegion (other.region());
  }
  return *this;
}

template<class T>
Lattice<T>* SubImage<T>::clone() const
{
  return new SubImage<T> (*this);
}
template<class T>
MaskedLattice<T>* SubImage<T>::cloneML() const
{
  return new SubImage<T> (*this);
}
template<class T>
ImageInterface<T>* SubImage<T>::cloneII() const
{
  return new SubImage<T> (*this);
}


template <class T>
Bool SubImage<T>::ok() const
{
  return SubLattice<T>::ok();
}

template<class T>
IPosition SubImage<T>::shape() const
{
  return SubLattice<T>::shape();
}

template<class T>
void SubImage<T>::resize(const TiledShape&)
{
  throw (AipsError ("SubImage::resize is not possible"));
}

template<class T>
Bool SubImage<T>::setUnits(const Unit&)
{
  return False;
}

template<class T>
Unit SubImage<T>::units() const
{
  return itsImagePtr->units();
}

template<class T>
String SubImage<T>::name(const Bool) const
{
  return "";
}
  
template<class T>
Bool SubImage<T>::setCoordinateInfo(const CoordinateSystem&)
{
  return False;
}
  
template<class T>
const RecordInterface& SubImage<T>::miscInfo() const
{
  return itsImagePtr->miscInfo();
}

template<class T>
Bool SubImage<T>::setMiscInfo(const RecordInterface&)
{
  return False;
}

template<class T>
Bool SubImage<T>::doGetSlice (Array<T>& buffer,
				const Slicer& section)
{
  return SubLattice<T>::doGetSlice (buffer, section);
}

template<class T>
void SubImage<T>::doPutSlice (const Array<T>& sourceBuffer,
				const IPosition& where, 
				const IPosition& stride)
{
  SubLattice<T>::doPutSlice (sourceBuffer, where, stride);
}
