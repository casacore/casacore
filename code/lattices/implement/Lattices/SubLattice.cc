//# SubLattice.cc: A subset of a Lattice
//# Copyright (C) 1997
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

#include <trial/Lattices/SubLattice.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Lattices/Slicer.h>
#include <aips/Exceptions/Error.h>


template<class T>
SubLattice<T>::SubLattice()
: itsLatticePtr (0),
  itsRegionPtr  (0),
  itsWritable   (False)
{}

template<class T>
SubLattice<T>::SubLattice (const Lattice<T>& lattice,
			   const PixelRegion& region)
: itsLatticePtr (lattice.clone()),
  itsRegionPtr  (region.clone()),
  itsWritable   (False)
{
  if (region.latticeShape() != lattice.shape()) {
    throw (AipsError ("SubLattice::SubLattice - "
		      "lattice shape in region mismatches lattice"));
  }
}

template<class T>
SubLattice<T>::SubLattice (Lattice<T>& lattice,
			   const PixelRegion& region)
: itsLatticePtr (lattice.clone()),
  itsRegionPtr  (region.clone()),
  itsWritable   (lattice.isWritable())
{
  if (region.latticeShape() != lattice.shape()) {
    throw (AipsError ("SubLattice::SubLattice - "
		      "lattice shape in region mismatches lattice"));
  }
}

template<class T>
SubLattice<T>::SubLattice (const SubLattice<T>& other)
: itsLatticePtr (other.itsLatticePtr->clone()),
  itsRegionPtr  (other.itsRegionPtr->clone()),
  itsWritable   (other.itsWritable)
{}

template<class T>
SubLattice<T>::~SubLattice()
{
  delete itsLatticePtr;
  delete itsRegionPtr;
}

template<class T>
SubLattice<T>& SubLattice<T>::operator= (const SubLattice<T>& other)
{
  if (this != &other) {
    itsLatticePtr = other.itsLatticePtr->clone();
    itsRegionPtr  = other.itsRegionPtr->clone();
    itsWritable   = other.itsWritable;
  }
  return *this;
}

template<class T>
Lattice<T>* SubLattice<T>::clone() const
{
  return new SubLattice<T> (*this);
}

template<class T>
Bool SubLattice<T>::isWritable() const
{
  return itsWritable;
}

template<class T>
IPosition SubLattice<T>::shape() const
{
  return itsRegionPtr->box().length();
}

template<class T>
uInt SubLattice<T>::ndim() const
{
  return itsLatticePtr->ndim();
}

template<class T>
uInt SubLattice<T>::nelements() const
{
  return itsRegionPtr->box().length().product();
}

template<class T>
Bool SubLattice<T>::conform (const Lattice<T>& other) const
{
  return shape().isEqual (other.shape());
}

template<class T>
Bool SubLattice<T>::getSlice (COWPtr<Array<T> >& buffer, const IPosition& start,
			      const IPosition& shape, const IPosition& stride,
			      Bool removeDegenerateAxes) const
{

  return itsLatticePtr->getSlice (buffer,
				  itsRegionPtr->expand (Slicer(start, shape,
							       stride)),
				  removeDegenerateAxes);
}

template<class T>
Bool SubLattice<T>::getSlice (COWPtr<Array<T> >& buffer, const Slicer& section,
			      Bool removeDegenerateAxes) const
{
  return itsLatticePtr->getSlice (buffer,
				  itsRegionPtr->expand (section),
				  removeDegenerateAxes);
}

template<class T>
Bool SubLattice<T>::getSlice (Array<T>& buffer, const IPosition& start, 
			      const IPosition& shape, const IPosition& stride, 
			      Bool removeDegenerateAxes)
{
  return itsLatticePtr->getSlice (buffer,
				  itsRegionPtr->expand (Slicer(start, shape,
							       stride)),
				  removeDegenerateAxes);
}

template<class T>
Bool SubLattice<T>::getSlice (Array<T>& buffer, const Slicer& section,
			      Bool removeDegenerateAxes)
{
  return itsLatticePtr->getSlice (buffer,
				  itsRegionPtr->expand (section),
				  removeDegenerateAxes);
}

template<class T>
void SubLattice<T>::putSlice (const Array<T>& sourceBuffer,
			      const IPosition& where, 
			      const IPosition& stride)
{
  if (!itsWritable) {
      throw (AipsError ("SubLattice::putSlice - non-writable lattice"));
  }
  itsLatticePtr->putSlice (sourceBuffer,
			   itsRegionPtr->expand (where),
			   stride * itsRegionPtr->box().stride());
}

template<class T>
void SubLattice<T>::putSlice (const Array<T>& sourceBuffer,
			      const IPosition& where)
{
  if (!itsWritable) {
      throw (AipsError ("SubLattice::putSlice - non-writable lattice"));
  }
  itsLatticePtr->putSlice (sourceBuffer,
			   itsRegionPtr->expand (where));
}

template<class T>
uInt SubLattice<T>::maxPixels() const
{
  return itsLatticePtr->maxPixels();
}

template<class T>
IPosition SubLattice<T>::niceCursorShape (uInt maxPixels) const
{
  return itsLatticePtr->niceCursorShape (maxPixels);
}

template<class T>
T SubLattice<T>::getAt (const IPosition& where) const
{
  return itsLatticePtr->getAt (itsRegionPtr->expand (where));
}

template<class T>
void SubLattice<T>::putAt (const T& value, const IPosition& where)
{
  if (!itsWritable) {
      throw (AipsError ("SubLattice::putSlice - non-writable lattice"));
  }
  itsLatticePtr->putAt (value, itsRegionPtr->expand (where));
}

template <class T>
Bool SubLattice<T>::ok() const
{
  return itsLatticePtr->ok();
}

template<class T>
LatticeIterInterface<T>* SubLattice<T>::makeIter
                               (const LatticeNavigator& navigator) const
{
  return new SubLatticeIter<T> (navigator);
  // Make a clone of the navigator to be able to apply our region.
  LatticeNavigator* navPtr = navigator.clone();
  const Slicer& section = itsRegionPtr->box();
  navPtr->subSection (section.start(), section.end(), section.stride());
  delete navPtr;
  return iterPtr;
}
