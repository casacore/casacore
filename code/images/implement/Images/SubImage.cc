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
#include <trial/Lattices/SubLattice.h>
#include <trial/Lattices/LatticeRegion.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Arrays/Vector.h>
#include <aips/Quanta/UnitMap.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>

typedef Vector<Int> gppbug1_subimage;
typedef Array<Bool> gppbug2_subimage;


template<class T>
SubImage<T>::SubImage()
: MaskedImage<T> (),
  itsImagePtr    (0),
  itsSubLatPtr   (0)
{}

template<class T>
SubImage<T>::SubImage (const ImageInterface<T>& image)
: MaskedImage<T> (),
  itsImagePtr (image.cloneII())
{
  itsSubLatPtr = new SubLattice<T> (image);
  coords_p = image.coordinates();
}

template<class T>
SubImage<T>::SubImage (ImageInterface<T>& image,
		       Bool writableIfPossible)
: MaskedImage<T> (),
  itsImagePtr (image.cloneII())
{
  itsSubLatPtr = new SubLattice<T> (image, writableIfPossible);
  coords_p = image.coordinates();
}

template<class T>
SubImage<T>::SubImage (const ImageInterface<T>& image,
		       const ImageRegion& region)
: MaskedImage<T> (),
  itsImagePtr (image.cloneII())
{
  itsSubLatPtr = new SubLattice<T> (image,
				    region.toLatticeRegion (image.coordinates(),
							    image.shape()));
  const Slicer& slicer = itsSubLatPtr->region().slicer();
  coords_p = image.coordinates().subImage (slicer.start().asVector(),
					   slicer.stride().asVector());
}

template<class T>
SubImage<T>::SubImage (ImageInterface<T>& image,
		       const ImageRegion& region,
		       Bool writableIfPossible)
: MaskedImage<T> (),
  itsImagePtr (image.cloneII())
{

  itsSubLatPtr = new SubLattice<T> (image, 
				    region.toLatticeRegion (image.coordinates(),
							    image.shape()),
                                    writableIfPossible);
  const Slicer& slicer = itsSubLatPtr->region().slicer();
  coords_p = image.coordinates().subImage (slicer.start().asVector(),
					   slicer.stride().asVector());
}

template<class T>
SubImage<T>::SubImage (const MaskedImage<T>& image,
		       const ImageRegion& region)
: MaskedImage<T> (),
  itsImagePtr (image.cloneII())
{
  itsSubLatPtr = new SubLattice<T> (image,
				    region.toLatticeRegion (image.coordinates(),
							    image.shape()));
  const Slicer& slicer = itsSubLatPtr->region().slicer();
  coords_p = image.coordinates().subImage (slicer.start().asVector(),
					   slicer.stride().asVector());
}

template<class T>
SubImage<T>::SubImage (MaskedImage<T>& image,
		       const ImageRegion& region,
		       Bool writableIfPossible)
: MaskedImage<T> (),
  itsImagePtr (image.cloneII())
{
  itsSubLatPtr = new SubLattice<T> (image,
				    region.toLatticeRegion (image.coordinates(),
							    image.shape()),
				    writableIfPossible);
  const Slicer& slicer = itsSubLatPtr->region().slicer();
  coords_p = image.coordinates().subImage (slicer.start().asVector(),
					   slicer.stride().asVector());
}

template<class T>
SubImage<T>::SubImage (const ImageInterface<T>& image,
		       const Slicer& slicer)
: MaskedImage<T> (),
  itsImagePtr (image.cloneII())
{
  itsSubLatPtr = new SubLattice<T> (image, slicer);
  const Slicer& refslicer = itsSubLatPtr->region().slicer();
  coords_p = image.coordinates().subImage (refslicer.start().asVector(),
					   refslicer.stride().asVector());
}

template<class T>
SubImage<T>::SubImage (ImageInterface<T>& image,
		       const Slicer& slicer,
		       Bool writableIfPossible)
: MaskedImage<T> (),
  itsImagePtr (image.cloneII())
{
  itsSubLatPtr = new SubLattice<T> (image, slicer, writableIfPossible);
  const Slicer& refslicer = itsSubLatPtr->region().slicer();
  coords_p = image.coordinates().subImage (refslicer.start().asVector(),
					   refslicer.stride().asVector());
}

template<class T>
SubImage<T>::SubImage (const MaskedImage<T>& image,
		       const Slicer& slicer)
: MaskedImage<T> (),
  itsImagePtr (image.cloneII())
{
  itsSubLatPtr = new SubLattice<T> (image, slicer);
  const Slicer& refslicer = itsSubLatPtr->region().slicer();
  coords_p = image.coordinates().subImage (refslicer.start().asVector(),
					   refslicer.stride().asVector());
}

template<class T>
SubImage<T>::SubImage (MaskedImage<T>& image,
		       const Slicer& slicer,
		       Bool writableIfPossible)
: MaskedImage<T> (),
  itsImagePtr (image.cloneII())
{
  itsSubLatPtr = new SubLattice<T> (image, slicer, writableIfPossible);
  const Slicer& refslicer = itsSubLatPtr->region().slicer();
  coords_p = image.coordinates().subImage (refslicer.start().asVector(),
					   refslicer.stride().asVector());
}

template<class T>
SubImage<T>::SubImage (const SubImage<T>& other)
: MaskedImage<T> (other),
  itsImagePtr  (other.itsImagePtr->cloneII())
{
  itsSubLatPtr = new SubLattice<T> (*other.itsSubLatPtr);
}

template<class T>
SubImage<T>::~SubImage()
{
  delete itsImagePtr;
  delete itsSubLatPtr;
}

template<class T>
SubImage<T>& SubImage<T>::operator= (const SubImage<T>& other)
{
  if (this != &other) {
    MaskedImage<T>::operator= (other);
    delete itsImagePtr;
    itsImagePtr = other.itsImagePtr->cloneII();
    delete itsSubLatPtr;
    itsSubLatPtr = new SubLattice<T> (*other.itsSubLatPtr);
  }
  return *this;
}

template<class T>
Lattice<T>* SubImage<T>::clone() const
{
  return new SubImage<T> (*this);
}
template<class T>
MaskedImage<T>* SubImage<T>::cloneMI() const
{
  return new SubImage<T> (*this);
}


template <class T>
Bool SubImage<T>::ok() const
{
  return itsSubLatPtr->ok();
}

template<class T>
Bool SubImage<T>::isPaged() const
{
  return itsSubLatPtr->isPaged();
}

template<class T>
Bool SubImage<T>::isWritable() const
{
  return itsSubLatPtr->isWritable();
}

template<class T>
Bool SubImage<T>::isMasked() const
{
  return itsSubLatPtr->isMasked();
}

template<class T>
const LatticeRegion& SubImage<T>::region() const
{
    return itsSubLatPtr->region();
}

template<class T>
IPosition SubImage<T>::shape() const
{
  return itsSubLatPtr->shape();
}

template<class T>
uInt SubImage<T>::ndim() const
{
  return itsSubLatPtr->ndim();
}

template<class T>
uInt SubImage<T>::nelements() const
{
  return itsSubLatPtr->nelements();
}

template<class T>
Bool SubImage<T>::conform (const Lattice<T>& other) const
{
  return shape().isEqual (other.shape());
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
  return itsImagePtr->name();
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
Bool SubImage<T>::doGetMaskSlice (Array<Bool>& buffer,
				  const Slicer& section)
{
  return itsSubLatPtr->doGetMaskSlice (buffer, section);
}

template<class T>
Bool SubImage<T>::doGetSlice (Array<T>& buffer,
			      const Slicer& section)
{
  return itsSubLatPtr->doGetSlice (buffer, section);
}

template<class T>
void SubImage<T>::doPutSlice (const Array<T>& sourceBuffer,
			      const IPosition& where, 
			      const IPosition& stride)
{
  itsSubLatPtr->doPutSlice (sourceBuffer, where, stride);
}

template<class T>
uInt SubImage<T>::maxPixels() const
{
  return itsSubLatPtr->maxPixels();
}

template<class T>
IPosition SubImage<T>::doNiceCursorShape (uInt maxPixels) const
{
  return itsSubLatPtr->niceCursorShape (maxPixels);
}

template<class T>
T SubImage<T>::getAt (const IPosition& where) const
{
  return itsSubLatPtr->getAt (where);
}

template<class T>
void SubImage<T>::putAt (const T& value, const IPosition& where)
{
  itsSubLatPtr->putAt (value, where);
}

template<class T>
LatticeIterInterface<T>* SubImage<T>::makeIter
                               (const LatticeNavigator& navigator) const
{
  return itsSubLatPtr->makeIter (navigator);
}
