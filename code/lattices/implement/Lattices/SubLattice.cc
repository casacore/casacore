//# SubLattice.cc: A subset of a Lattice
//# Copyright (C) 1997,1998
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
#include <trial/Lattices/LatticeIterInterface.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>


typedef Array<Bool> sublattice_gppbug1;


template<class T>
SubLattice<T>::SubLattice()
: itsLatticePtr (0),
  itsMaskLatPtr (0),
  itsWritable   (False)
{}

template<class T>
SubLattice<T>::SubLattice (const Lattice<T>& lattice)
{
  setPtr (lattice.clone(), 0, False);
  setRegion();
}

template<class T>
SubLattice<T>::SubLattice (Lattice<T>& lattice,
			   Bool writableIfPossible)
{
  setPtr (lattice.clone(), 0, writableIfPossible);
  setRegion();
}

template<class T>
SubLattice<T>::SubLattice (const Lattice<T>& lattice,
			   const LatticeRegion& region)
{
  setPtr (lattice.clone(), 0, False);
  setRegion (region);
}

template<class T>
SubLattice<T>::SubLattice (Lattice<T>& lattice,
			   const LatticeRegion& region,
			   Bool writableIfPossible)
{
  setPtr (lattice.clone(), 0, writableIfPossible);
  setRegion (region);
}

template<class T>
SubLattice<T>::SubLattice (const MaskedLattice<T>& lattice,
			   const LatticeRegion& region)
{
  setPtr (0, lattice.cloneML(), False);
  setRegion (region);
}

template<class T>
SubLattice<T>::SubLattice (MaskedLattice<T>& lattice,
			   const LatticeRegion& region,
			   Bool writableIfPossible)
{
  setPtr (0, lattice.cloneML(), writableIfPossible);
  setRegion (region);
}

template<class T>
SubLattice<T>::SubLattice (const Lattice<T>& lattice,
			   const Slicer& slicer)
{
  setPtr (lattice.clone(), 0, False);
  setRegion (slicer);
}

template<class T>
SubLattice<T>::SubLattice (Lattice<T>& lattice,
			   const Slicer& slicer,
			   Bool writableIfPossible)
{
  setPtr (lattice.clone(), 0, writableIfPossible);
  setRegion (slicer);
}

template<class T>
SubLattice<T>::SubLattice (const MaskedLattice<T>& lattice,
			   const Slicer& slicer)
{
  setPtr (0, lattice.cloneML(), False);
  setRegion (slicer);
}

template<class T>
SubLattice<T>::SubLattice (MaskedLattice<T>& lattice,
			   const Slicer& slicer,
			   Bool writableIfPossible)
{
  setPtr (0, lattice.cloneML(), writableIfPossible);
  setRegion (slicer);
}

template<class T>
SubLattice<T>::SubLattice (const SubLattice<T>& other)
: itsLatticePtr (0),
  itsMaskLatPtr (0)
{
  operator= (other);
}

template<class T>
SubLattice<T>::~SubLattice()
{
  // Note that itsMaskLatPtr (if filled in) always points to the same
  // object as itsLatticePtr.
  delete itsLatticePtr;
}

template<class T>
SubLattice<T>& SubLattice<T>::operator= (const SubLattice<T>& other)
{
  if (this != &other) {
    itsRegion = other.itsRegion;
    delete itsLatticePtr;
    itsLatticePtr = other.itsLatticePtr;
    itsMaskLatPtr = other.itsMaskLatPtr;
    if (itsMaskLatPtr != 0) {
      itsMaskLatPtr = itsMaskLatPtr->cloneML();
      itsLatticePtr = itsMaskLatPtr;
      itsRegion.setParent (&itsMaskLatPtr->region());
    } else if (itsLatticePtr != 0) {
      itsLatticePtr = itsLatticePtr->clone();
    }
    itsWritable = other.itsWritable;
  }
  return *this;
}

template<class T>
Lattice<T>* SubLattice<T>::clone() const
{
  return new SubLattice<T> (*this);
}
template<class T>
MaskedLattice<T>* SubLattice<T>::cloneML() const
{
  return new SubLattice<T> (*this);
}

template<class T>
void SubLattice<T>::setPtr (Lattice<T>* latticePtr,
			    MaskedLattice<T>* maskLatPtr,
			    Bool writableIfPossible)
{
  if (maskLatPtr == 0) {
    itsLatticePtr = latticePtr;
    itsMaskLatPtr = 0;
  } else {
    itsLatticePtr = maskLatPtr;
    itsMaskLatPtr = maskLatPtr;
  }
  itsWritable = writableIfPossible;
  if (itsWritable  &&  latticePtr->isWritable()) {
    itsWritable = True;
  }
}

template<class T>
void SubLattice<T>::setRegion (const LatticeRegion& region)
{
  if (itsLatticePtr->shape() != region.region().latticeShape()) {
    throw (AipsError ("SubLattice::SubLattice - "
		      "shape of lattice mismatches lattice shape in region"));
  }
  itsRegion = region;
  if (itsMaskLatPtr != 0) {
    itsRegion.setParent (&itsMaskLatPtr->region());
  }
}
template<class T>
void SubLattice<T>::setRegion (const Slicer& slicer)
{
  setRegion (LatticeRegion (slicer, itsLatticePtr->shape()));
}
template<class T>
void SubLattice<T>::setRegion()
{
  IPosition shape = itsLatticePtr->shape();
  setRegion (LatticeRegion (Slicer(IPosition(shape.nelements(),0), shape),
			    shape));
}

template<class T>
Bool SubLattice<T>::isPaged() const
{
  return itsLatticePtr->isPaged();
}

template<class T>
Bool SubLattice<T>::isWritable() const
{
  return itsWritable;
}

template<class T>
Bool SubLattice<T>::isMasked() const
{
  return itsRegion.hasMask();
}

template<class T>
const LatticeRegion& SubLattice<T>::region() const
{
    return itsRegion;
}

template<class T>
IPosition SubLattice<T>::shape() const
{
  return itsRegion.slicer().length();
}

template<class T>
uInt SubLattice<T>::ndim() const
{
  return itsRegion.slicer().ndim();
}

template<class T>
uInt SubLattice<T>::nelements() const
{
  return itsRegion.nelements();
}

template<class T>
Bool SubLattice<T>::conform (const Lattice<T>& other) const
{
  return shape().isEqual (other.shape());
}

template<class T>
Bool SubLattice<T>::doGetMaskSlice (Array<Bool>& buffer,
				    const Slicer& section)
{
  return itsRegion.getSlice (buffer, section);
}

template<class T>
Bool SubLattice<T>::doGetSlice (Array<T>& buffer,
				const Slicer& section)
{
  return itsLatticePtr->getSlice (buffer, itsRegion.convert (section));
}

template<class T>
void SubLattice<T>::doPutSlice (const Array<T>& sourceBuffer,
				const IPosition& where, 
				const IPosition& stride)
{
  if (!itsWritable) {
      throw (AipsError ("SubLattice::putSlice - non-writable lattice"));
  }
  if (itsMaskLatPtr != 0) {
    itsMaskLatPtr->putSlice (sourceBuffer,
			     itsRegion.convert (where),
			     stride * itsRegion.slicer().stride());
  } else {
    itsLatticePtr->putSlice (sourceBuffer,
			     itsRegion.convert (where),
			     stride * itsRegion.slicer().stride());
  }
}


template<class T>
uInt SubLattice<T>::maxPixels() const
{
  return itsLatticePtr->maxPixels();
}

template<class T>
IPosition SubLattice<T>::doNiceCursorShape (uInt maxPixels) const
{
  IPosition cursorShape (itsLatticePtr->niceCursorShape (maxPixels));
  const IPosition& shape = itsRegion.slicer().length();
  for (uInt i=0; i<shape.nelements(); i++) {
    if (cursorShape(i) > shape(i)) {
      cursorShape(i) = shape(i);
    }
  }
  return cursorShape;
}

template<class T>
T SubLattice<T>::getAt (const IPosition& where) const
{
  return itsLatticePtr->getAt (itsRegion.convert (where));
}

template<class T>
void SubLattice<T>::putAt (const T& value, const IPosition& where)
{
  if (!itsWritable) {
      throw (AipsError ("SubLattice::putAt - non-writable lattice"));
  }
  if (itsMaskLatPtr != 0) {
    itsMaskLatPtr->putAt (value, itsRegion.convert (where));
  } else {
    itsLatticePtr->putAt (value, itsRegion.convert (where));
  }
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
  return new LatticeIterInterface<T> (*this, navigator);
  // Make a clone of the navigator to be able to apply our region.
///  LatticeNavigator* navPtr = navigator.clone();
///  const Slicer& section = itsRegionPtr->box();
///  navPtr->subSection (section.start(), section.end(), section.stride());
///  delete navPtr;
///  return iterPtr;
}
