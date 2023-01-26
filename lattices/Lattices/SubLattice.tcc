//# SubLattice.cc: A subset of a Lattice
//# Copyright (C) 1997,1998,1999,2000,2001,2003
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

#ifndef LATTICES_SUBLATTICE_TCC
#define LATTICES_SUBLATTICE_TCC

#include <casacore/lattices/Lattices/SubLattice.h>
#include <casacore/lattices/Lattices/LatticeIterInterface.h>
#include <casacore/lattices/LEL/LatticeExpr.h>
#include <casacore/lattices/LRegions/LCRegion.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T>
SubLattice<T>::SubLattice()
: itsLatticePtr   (0),
  itsMaskLatPtr   (0),
  itsWritable     (false),
  itsHasLattPMask (false),
  itsPixelMask    (0),
  itsOwnPixelMask (0)
{}

template<class T>
SubLattice<T>::SubLattice (const Lattice<T>& lattice,
			   AxesSpecifier axesSpec)
{
  setPtr (lattice.clone(), 0, false);
  setRegion();
  setAxesMap (axesSpec);
}

template<class T>
SubLattice<T>::SubLattice (Lattice<T>& lattice,
			   bool writableIfPossible,
			   AxesSpecifier axesSpec)
{
  setPtr (lattice.clone(), 0, writableIfPossible);
  setRegion();
  setAxesMap (axesSpec);
}

template<class T>
SubLattice<T>::SubLattice (const MaskedLattice<T>& lattice,
			   AxesSpecifier axesSpec)
{
  setPtr (0, lattice.cloneML(), false);
  setRegion();
  setAxesMap (axesSpec);
}

template<class T>
SubLattice<T>::SubLattice (MaskedLattice<T>& lattice,
			   bool writableIfPossible,
			   AxesSpecifier axesSpec)
{
  setPtr (0, lattice.cloneML(), writableIfPossible);
  setRegion();
  setAxesMap (axesSpec);
}

template<class T>
SubLattice<T>::SubLattice (const Lattice<T>& lattice,
			   const LatticeRegion& region,
			   AxesSpecifier axesSpec)
{
  setPtr (lattice.clone(), 0, false);
  setRegion (region);
  setAxesMap (axesSpec);
}

template<class T>
SubLattice<T>::SubLattice (Lattice<T>& lattice,
			   const LatticeRegion& region,
			   bool writableIfPossible,
			   AxesSpecifier axesSpec)
{
  setPtr (lattice.clone(), 0, writableIfPossible);
  setRegion (region);
  setAxesMap (axesSpec);
}

template<class T>
SubLattice<T>::SubLattice (const MaskedLattice<T>& lattice,
			   const LatticeRegion& region,
			   AxesSpecifier axesSpec)
{
  setPtr (0, lattice.cloneML(), false);
  setRegion (region);
  setAxesMap (axesSpec);
}

template<class T>
SubLattice<T>::SubLattice (MaskedLattice<T>& lattice,
			   const LatticeRegion& region,
			   bool writableIfPossible,
			   AxesSpecifier axesSpec)
{
  setPtr (0, lattice.cloneML(), writableIfPossible);
  setRegion (region);
  setAxesMap (axesSpec);
}

template<class T>
SubLattice<T>::SubLattice (const Lattice<T>& lattice,
			   const Slicer& slicer,
			   AxesSpecifier axesSpec)
{
  setPtr (lattice.clone(), 0, false);
  setRegion (slicer);
  setAxesMap (axesSpec);
}

template<class T>
SubLattice<T>::SubLattice (Lattice<T>& lattice,
			   const Slicer& slicer,
			   bool writableIfPossible,
			   AxesSpecifier axesSpec)
{
  setPtr (lattice.clone(), 0, writableIfPossible);
  setRegion (slicer);
  setAxesMap (axesSpec);
}

template<class T>
SubLattice<T>::SubLattice (const MaskedLattice<T>& lattice,
			   const Slicer& slicer,
			   AxesSpecifier axesSpec)
{
  setPtr (0, lattice.cloneML(), false);
  setRegion (slicer);
  setAxesMap (axesSpec);
}

template<class T>
SubLattice<T>::SubLattice (MaskedLattice<T>& lattice,
			   const Slicer& slicer,
			   bool writableIfPossible,
			   AxesSpecifier axesSpec)
{
  setPtr (0, lattice.cloneML(), writableIfPossible);
  setRegion (slicer);
  setAxesMap (axesSpec);
}

template<class T>
SubLattice<T>::SubLattice (const SubLattice<T>& other)
: MaskedLattice<T>(),
  itsLatticePtr   (0),
  itsMaskLatPtr   (0),
  itsPixelMask    (0),
  itsOwnPixelMask (0)
{
  operator= (other);
}

template<class T>
SubLattice<T>::~SubLattice()
{
  // Note that itsMaskLatPtr (if filled in) always points to the same
  // object as itsLatticePtr, so it does not need to be deleted.
  delete itsLatticePtr;
  delete itsPixelMask;
  delete itsOwnPixelMask;
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
    } else if (itsLatticePtr != 0) {
      itsLatticePtr = itsLatticePtr->clone();
    }
    itsWritable = other.itsWritable;
    delete itsPixelMask;
    itsPixelMask = 0;
    delete itsOwnPixelMask;
    itsOwnPixelMask = 0;
    if (other.itsOwnPixelMask != 0) {
      itsOwnPixelMask = other.itsOwnPixelMask->clone();
    }
    itsHasLattPMask = other.itsHasLattPMask;
    itsAxesMap = other.itsAxesMap;
  }
  return *this;
}

template<class T>
MaskedLattice<T>* SubLattice<T>::cloneML() const
{
  return new SubLattice<T> (*this);
}

template<class T>
void SubLattice<T>::setPtr (Lattice<T>* latticePtr,
			    MaskedLattice<T>* maskLatPtr,
			    bool writableIfPossible)
{
  itsHasLattPMask = false;
  itsPixelMask    = 0;
  itsOwnPixelMask = 0;
  if (maskLatPtr == 0) {
    itsLatticePtr = latticePtr;
    itsMaskLatPtr = 0;
  } else {
    itsLatticePtr = maskLatPtr;
    if (! maskLatPtr->isMasked()) {
      itsMaskLatPtr = 0;
    } else {
      itsMaskLatPtr = maskLatPtr;
      itsHasLattPMask = itsMaskLatPtr->hasPixelMask();
    }
  }
  itsWritable = false;
  if (writableIfPossible  &&  itsLatticePtr->isWritable()) {
    itsWritable = true;
  }
}

template<class T>
void SubLattice<T>::setRegion (const LatticeRegion& region)
{
    ThrowIf(
        ! (itsLatticePtr->shape().isEqual(region.region().latticeShape())),
        "shape of lattice " + itsLatticePtr->shape().toString()
        + " mismatches lattice shape in region " + region.region().latticeShape().toString()
    );
  itsRegion = region;
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
void SubLattice<T>::setAxesMap (const AxesSpecifier& axesSpec)
{
  itsAxesMap = axesSpec.apply (itsRegion.slicer().length());
  if (itsAxesMap.isReordered()) {
    throw AipsError ("SubLattice does not support axes reordering");
  }
  itsAxesSpec = axesSpec;
}

template<class T>
bool SubLattice<T>::isMasked() const
{
  return  (itsMaskLatPtr != 0  ||  itsRegion.hasMask()
	   ||  itsOwnPixelMask != 0);
}

template<class T>
bool SubLattice<T>::isPersistent() const
{
  return itsLatticePtr->isPersistent()
     &&  !isMasked()
     &&  !itsAxesMap.isRemoved()
     &&  shape().isEqual (itsLatticePtr->shape());
}

template<class T>
bool SubLattice<T>::isPaged() const
{
  return itsLatticePtr->isPaged();
}

template<class T>
bool SubLattice<T>::canReferenceArray() const
{
  return itsLatticePtr->canReferenceArray();
}

template<class T>
bool SubLattice<T>::isWritable() const
{
  return itsWritable;
}

template<class T>
bool SubLattice<T>::lock (FileLocker::LockType type, uint32_t nattempts)
{
  return itsLatticePtr->lock (type, nattempts);
}
template<class T>
void SubLattice<T>::unlock()
{
  itsLatticePtr->unlock();
}
template<class T>
bool SubLattice<T>::hasLock (FileLocker::LockType type) const
{
  return itsLatticePtr->hasLock (type);
}
template<class T>
void SubLattice<T>::resync()
{
  itsLatticePtr->resync();
}

template<class T>
void SubLattice<T>::flush()
{
  itsLatticePtr->flush();
}

template<class T>
void SubLattice<T>::tempClose()
{
  itsLatticePtr->tempClose();
}

template<class T>
void SubLattice<T>::reopen()
{
  itsLatticePtr->reopen();
}

template<class T>
bool SubLattice<T>::hasPixelMask() const
{
  return itsHasLattPMask  ||  itsOwnPixelMask != 0;
}

template<class T>
const Lattice<bool>& SubLattice<T>::pixelMask() const
{
  return ((const SubLattice<T>*)this)->pixelMask();
}
template<class T>
Lattice<bool>& SubLattice<T>::pixelMask()
{
  if (itsPixelMask == 0) {
    if (!hasPixelMask()) {
      throw (AipsError ("SubLattice::pixelMask - no pixelmask available"));
    }
    if (itsHasLattPMask) {
      // Construct the pixelmask (as a subset of the parent pixelmask).
      Lattice<bool>& fullMask = itsMaskLatPtr->pixelMask();
      itsPixelMask = new SubLattice<bool> (fullMask, itsRegion, itsWritable,
					   itsAxesSpec);
      // If there is an own pixelmask, and them.
      if (itsOwnPixelMask != 0) {
	Lattice<bool>* pmask = itsPixelMask;
	itsPixelMask = new LatticeExpr<bool> (*pmask && *itsOwnPixelMask);
	delete pmask;
      }
    } else {
      itsPixelMask = itsOwnPixelMask->clone();
    }
  }
  return *itsPixelMask;
}

template<class T>
void SubLattice<T>::setPixelMask (const Lattice<bool>& pixelMask,
				  bool mayExist)
{
  if (!mayExist  &&  itsHasLattPMask) {
    throw (AipsError ("SubLattice::setPixelMask - "
		      "underlying lattice has a pixelmask already"));
  }
  if (!(shape().isEqual(pixelMask.shape()))) {
    throw (AipsError ("SubLattice::setPixelMask - "
		      "shape of pixel mask mismatches sublattice"));
  }
  delete itsPixelMask;
  itsPixelMask = 0;
  delete itsOwnPixelMask;
  itsOwnPixelMask = 0;
  itsOwnPixelMask = pixelMask.clone();
}

template<class T>
const LatticeRegion* SubLattice<T>::getRegionPtr() const
{
  return &itsRegion;
}

template<class T>
IPosition SubLattice<T>::shape() const
{
  return itsAxesMap.shapeToNew (itsRegion.slicer().length());
}

template<class T>
String SubLattice<T>::name (bool stripPath) const
{
  return itsLatticePtr->name(stripPath);
}

template<class T>
bool SubLattice<T>::doGetSlice (Array<T>& buffer,
				const Slicer& section)
{
  if (! itsAxesMap.isRemoved()) {
    return itsLatticePtr->getSlice (buffer, itsRegion.convert (section));
  }
  // Axes have been removed, so we have to reform the array buffer
  // to be able to get data from the original lattice.
  // Get the section shape in the original lattice.
  Slicer latSect = itsRegion.convert (itsAxesMap.slicerToOld (section));
  Array<T> tmp;
  bool reformed = false;
  if (buffer.shape().isEqual (section.length())) {
    // Use (in principle) the same buffer storage if its shape is correct.
    // This is needed for LatticeIterator to work correctly, because it
    // expects to get the data in the buffer it provides
    // (unless ref=true is returned).
    Array<T> tmp2 = buffer.reform (latSect.length());
    tmp.reference (tmp2);
    reformed = true;
  }
  bool ref = itsLatticePtr->getSlice (tmp, latSect);
  // Reform (i.e. remove axes) if the buffer did not have the correct shape
  // or if the lattice data are referenced.
  if (!reformed  ||  ref) {
    Array<T> tmp2 = tmp.reform (section.length());
    buffer.reference (tmp2);
  }
  return ref;
}

template<class T>
void SubLattice<T>::doPutSlice (const Array<T>& sourceBuffer,
				const IPosition& where, 
				const IPosition& stride)
{
  if (!itsWritable) {
      throw (AipsError ("SubLattice::putSlice - non-writable lattice"));
  }
  if (! itsAxesMap.isRemoved()) {
    itsLatticePtr->putSlice (sourceBuffer, itsRegion.convert (where),
			     stride * itsRegion.slicer().stride());
  } else {
    Array<T> tmp = sourceBuffer.reform
                            (itsAxesMap.shapeToOld (sourceBuffer.shape()));
    itsLatticePtr->putSlice (tmp, 
			     itsRegion.convert (itsAxesMap.posToOld (where)),
			     itsAxesMap.shapeToOld (stride) *
                                                itsRegion.slicer().stride());
  }
}


template<class T>
uint32_t SubLattice<T>::advisedMaxPixels() const
{
  return itsLatticePtr->advisedMaxPixels();
}

template<class T>
IPosition SubLattice<T>::doNiceCursorShape (uint32_t maxPixels) const
{
  IPosition cursorShape (itsLatticePtr->niceCursorShape (maxPixels));
  const IPosition& shape = itsRegion.slicer().length();
  for (uint32_t i=0; i<shape.nelements(); i++) {
    if (cursorShape(i) > shape(i)) {
      cursorShape(i) = shape(i);
    }
  }
  return itsAxesMap.shapeToNew (cursorShape);
}
template<class T>
T SubLattice<T>::getAt (const IPosition& where) const
{
	return itsLatticePtr->getAt (positionInParent(where));
	/*
  if (! itsAxesMap.isRemoved()) {
    return itsLatticePtr->getAt (itsRegion.convert (where));
  }
  return itsLatticePtr->getAt (itsRegion.convert(itsAxesMap.posToOld (where)));
  */
}

template<class T>
void SubLattice<T>::putAt (const T& value, const IPosition& where)
{
	ThrowIf(! itsWritable, "SubLattice::putAt - non-writable lattice");
	itsLatticePtr->putAt (value, positionInParent(where));
  /*
  if (! itsAxesMap.isRemoved()) {
    itsLatticePtr->putAt (value, itsRegion.convert (where));
  } else {
    itsLatticePtr->putAt (value, itsRegion.convert (itsAxesMap.posToOld
						                 (where)));
  }
  */
}

template<class T>
bool SubLattice<T>::doGetMaskSlice (Array<bool>& buffer,
				    const Slicer& section)
{
  // If the lattice has no mask, we can return the region and/or pixel mask.
  if (itsMaskLatPtr == 0) {
    if (itsOwnPixelMask == 0) {
      // Note that if the region has no mask, it will return all true.
      return getRegionDataSlice (buffer, section);
    }
    if (! itsRegion.hasMask()) {
      return itsOwnPixelMask->getSlice (buffer, section);
    }
    // Return AND of region and pixel mask.
    bool ref = getRegionDataSlice (buffer, section);
    andMask (buffer, ref, itsOwnPixelMask->getSlice (section));
    return false;
  }
  // The lattice has a mask.
  // If there are no other masks, we can return the lattice's mask.
  if (! itsRegion.hasMask()) {
    if (itsOwnPixelMask == 0) {
      return getMaskDataSlice (buffer, section);
    }
    // Return AND of lattice and pixel mask.
    bool ref = getMaskDataSlice (buffer, section);
    andMask (buffer, ref, itsOwnPixelMask->getSlice (section));
    return false;
  }
  // Lattice and region have a mask, so they have to be ANDed.
  bool ref = getMaskDataSlice (buffer, section);
  Array<bool> tmpbuf;
  getRegionDataSlice (tmpbuf, section);
  andMask (buffer, ref, tmpbuf);
  if (itsOwnPixelMask != 0) {
    andMask (buffer, false, itsOwnPixelMask->getSlice (section));
  }
  return false;
}

template<class T>
void SubLattice<T>::andMask (Array<bool>& buffer, bool ref,
			     const Array<bool>& tmpbuf) const
{
  // Make a copy if the array is referenced.
  if (ref) {
    Array<bool> mask;
    mask = buffer;
    buffer.reference (mask);
  }
  // And the masks.
  bool deleteBuf, deleteTmp;
  const bool* tmpptr = tmpbuf.getStorage (deleteTmp);
  bool* bufptr = buffer.getStorage (deleteBuf);
  uint32_t n = buffer.nelements();
  for (uint32_t i=0; i<n; i++) {
    if (!tmpptr[i]) {
      bufptr[i] = tmpptr[i];
    }
  }
  tmpbuf.freeStorage (tmpptr, deleteTmp);
  buffer.putStorage (bufptr, deleteBuf);
}


template<class T>
bool SubLattice<T>::getRegionDataSlice (Array<bool>& buffer,
					const Slicer& section)
{
  if (! itsAxesMap.isRemoved()) {
    return itsRegion.getSlice (buffer, section);
  }
  bool ref = itsRegion.getSlice (buffer, itsAxesMap.slicerToOld (section));
  Array<bool> tmp = buffer.reform (section.length());
  buffer.reference (tmp);
  return ref;
}

template<class T>
bool SubLattice<T>::getMaskDataSlice (Array<bool>& buffer,
				      const Slicer& section)
{
  if (! itsAxesMap.isRemoved()) {
    return itsMaskLatPtr->doGetMaskSlice (buffer, itsRegion.convert (section));
  }
  bool ref = itsMaskLatPtr->doGetMaskSlice (buffer, itsRegion.convert
                                          (itsAxesMap.slicerToOld (section)));
  Array<bool> tmp = buffer.reform (section.length());
  buffer.reference (tmp);
  return ref;
}


template <class T>
bool SubLattice<T>::ok() const
{
  return itsLatticePtr->ok();
}

template<class T>
LatticeIterInterface<T>* SubLattice<T>::makeIter (const LatticeNavigator& nav,
						  bool useRef) const
{
  return new LatticeIterInterface<T> (*this, nav, useRef);
  // Make a clone of the navigator to be able to apply our region.
///  LatticeNavigator* navPtr = navigator.clone();
///  const Slicer& section = itsRegionPtr->box();
///  navPtr->subSection (section.start(), section.end(), section.stride());
///  delete navPtr;
///  return iterPtr;
}

} //# NAMESPACE CASACORE - END


#endif
