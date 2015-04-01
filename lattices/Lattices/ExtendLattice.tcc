//# ExtendLattice.cc: A subset of a Lattice
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

#ifndef LATTICES_EXTENDLATTICE_TCC
#define LATTICES_EXTENDLATTICE_TCC

#include <casacore/lattices/Lattices/ExtendLattice.h>
#include <casacore/lattices/Lattices/LatticeIterInterface.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T>
ExtendLattice<T>::ExtendLattice()
: itsLatticePtr   (0),
  itsMaskLatPtr   (0),
  itsHasPixelMask (False),
  itsPixelMask    (0)
{}

template<class T>
ExtendLattice<T>::ExtendLattice (const Lattice<T>& lattice,
				 const IPosition& newShape,
				 const IPosition& newAxes,
				 const IPosition& stretchAxes)
: itsExtendSpec (lattice.shape(), newShape, newAxes, stretchAxes)
{
  setPtr (lattice.clone(), 0);
}

template<class T>
ExtendLattice<T>::ExtendLattice (const MaskedLattice<T>& lattice,
				 const IPosition& newShape,
				 const IPosition& newAxes,
				 const IPosition& stretchAxes)
: itsExtendSpec (lattice.shape(), newShape, newAxes, stretchAxes)
{
  setPtr (0, lattice.cloneML());
}

template<class T>
ExtendLattice<T>::ExtendLattice (const ExtendLattice<T>& other)
: MaskedLattice<T>(),
  itsLatticePtr (0),
  itsMaskLatPtr (0),
  itsPixelMask  (0)
{
  operator= (other);
}

template<class T>
ExtendLattice<T>::~ExtendLattice()
{
  // Note that itsMaskLatPtr (if filled in) always points to the same
  // object as itsLatticePtr, so it does not need to be deleted.
  delete itsLatticePtr;
  delete itsPixelMask;
}

template<class T>
ExtendLattice<T>& ExtendLattice<T>::operator= (const ExtendLattice<T>& other)
{
  if (this != &other) {
    delete itsLatticePtr;
    itsLatticePtr = other.itsLatticePtr;
    itsMaskLatPtr = other.itsMaskLatPtr;
    if (itsMaskLatPtr != 0) {
      itsMaskLatPtr = itsMaskLatPtr->cloneML();
      itsLatticePtr = itsMaskLatPtr;
    } else if (itsLatticePtr != 0) {
      itsLatticePtr = itsLatticePtr->clone();
    }
    delete itsPixelMask;
    itsHasPixelMask = other.itsHasPixelMask;
    itsExtendSpec = other.itsExtendSpec;
  }
  return *this;
}

template<class T>
MaskedLattice<T>* ExtendLattice<T>::cloneML() const
{
  return new ExtendLattice<T> (*this);
}

template<class T>
void ExtendLattice<T>::setPtr (Lattice<T>* latticePtr,
			       MaskedLattice<T>* maskLatPtr)
{
  itsHasPixelMask = False;
  itsPixelMask = 0;
  if (maskLatPtr == 0) {
    itsLatticePtr = latticePtr;
    itsMaskLatPtr = 0;
  } else {
    itsLatticePtr = maskLatPtr;
    if (! maskLatPtr->isMasked()) {
      itsMaskLatPtr = 0;
    } else {
      itsMaskLatPtr = maskLatPtr;
      itsHasPixelMask = itsMaskLatPtr->hasPixelMask();
    }
  }
}

template<class T>
Bool ExtendLattice<T>::isMasked() const
{
  return  (itsMaskLatPtr != 0);
}

template<class T>
Bool ExtendLattice<T>::isPersistent() const
{
  return False;
}

template<class T>
Bool ExtendLattice<T>::isPaged() const
{
  return itsLatticePtr->isPaged();
}

template<class T>
Bool ExtendLattice<T>::isWritable() const
{
  return False;
}

template<class T>
Bool ExtendLattice<T>::lock (FileLocker::LockType type, uInt nattempts)
{
  return itsLatticePtr->lock (type, nattempts);
}
template<class T>
void ExtendLattice<T>::unlock()
{
  itsLatticePtr->unlock();
}
template<class T>
Bool ExtendLattice<T>::hasLock (FileLocker::LockType type) const
{
  return itsLatticePtr->hasLock (type);
}
template<class T>
void ExtendLattice<T>::resync()
{
  itsLatticePtr->resync();
}

template<class T>
void ExtendLattice<T>::flush()
{
  itsLatticePtr->flush();
}

template<class T>
void ExtendLattice<T>::tempClose()
{
  itsLatticePtr->tempClose();
}

template<class T>
void ExtendLattice<T>::reopen()
{
  itsLatticePtr->reopen();
}

template<class T>
Bool ExtendLattice<T>::hasPixelMask() const
{
  return itsHasPixelMask;
}

template<class T>
const Lattice<Bool>& ExtendLattice<T>::pixelMask() const
{
  return ((const ExtendLattice<T>*)this)->pixelMask();
}
template<class T>
Lattice<Bool>& ExtendLattice<T>::pixelMask()
{
  if (!itsHasPixelMask) {
    throw (AipsError ("ExtendLattice::pixelMask - no pixelmask available"));
  }
  // Construct the pixelmask (as an extension of the parent pixelmask)
  // if that is not done yet.
  if (itsPixelMask == 0) {
    Lattice<Bool>& fullMask = itsMaskLatPtr->pixelMask();
    itsPixelMask = new ExtendLattice<Bool> (fullMask,
					    itsExtendSpec.newShape(),
					    itsExtendSpec.newAxes(),
					    itsExtendSpec.stretchAxes());
  }
  return *itsPixelMask;
}

template <class T>
const LatticeRegion* ExtendLattice<T>::getRegionPtr() const
{
   return 0;
}


template<class T>
IPosition ExtendLattice<T>::shape() const
{
  return itsExtendSpec.newShape();
}

template<class T>
String ExtendLattice<T>::name (Bool stripPath) const
{
  return itsLatticePtr->name(stripPath);
}

template<class T>
Bool ExtendLattice<T>::doGetSlice (Array<T>& buffer,
				   const Slicer& section)
{
  IPosition shape;
  Slicer newSect = itsExtendSpec.convert (shape, section);
  Array<T> tmpbuf(newSect.length());
  itsLatticePtr->doGetSlice (tmpbuf, newSect);
  // Reform tmpbuf, so it has the same dimensionality as buffer.
  Array<T> data = tmpbuf.reform (shape);
  // Now we have to extend tmpbuf along all extend axes.
  const IPosition& length = section.length();
  buffer.resize (length);
  IPosition pos (buffer.ndim(), 0);
  IPosition end (buffer.shape() - 1);
  //# Iterate along the extendAxes through the buffer.
  const IPosition extendAxes = itsExtendSpec.extendAxes();
  uInt nre = extendAxes.nelements();
  for (;;) {
    uInt i;
    for (i=0; i<nre; i++) {
      end(extendAxes(i)) = pos(extendAxes(i));
    }
    //# Set each section of the buffer to the data.
    buffer(pos,end) = data;
    //# Go to the next section.
    for (i=0; i<nre; i++) {
      if (++pos(extendAxes(i)) < length(extendAxes(i))) {
	break;
      }
      // This dimension is done. Reset it and continue with the next.
      pos(extendAxes(i)) = 0;
    }
    //# End the iteration when all dimensions are done.
    if (i == nre) {
      break;
    }
  }
  return False;
}

template<class T>
void ExtendLattice<T>::doPutSlice (const Array<T>&,
				   const IPosition&, 
				   const IPosition&)
{
  throw (AipsError ("ExtendLattice::putSlice - non-writable lattice"));
}


template<class T>
uInt ExtendLattice<T>::advisedMaxPixels() const
{
  return itsLatticePtr->advisedMaxPixels();
}

template<class T>
IPosition ExtendLattice<T>::doNiceCursorShape (uInt maxPixels) const
{
  IPosition cursorShape (itsLatticePtr->niceCursorShape (maxPixels));
  return itsExtendSpec.convertNew (cursorShape);
}


template<class T>
Bool ExtendLattice<T>::doGetMaskSlice (Array<Bool>& buffer,
				       const Slicer& section)
{
  // When lattice has no mask, set mask to True.
  if (itsMaskLatPtr == 0) {
    buffer = True;
    return False;
  }
  IPosition shape;
  Slicer newSect = itsExtendSpec.convert (shape, section);
  Array<Bool> tmpbuf(newSect.length());
  itsMaskLatPtr->doGetMaskSlice (tmpbuf, newSect);
  // Reform tmpbuf, so it has the same dimensionality as buffer.
  Array<Bool> data = tmpbuf.reform (shape);
  // Now we have to extend tmpbuf along all extend axes.
  const IPosition& length = section.length();
  buffer.resize (length);
  IPosition pos (buffer.ndim(), 0);
  IPosition end (buffer.shape() - 1);
  //# Iterate along the extendAxes through the buffer.
  const IPosition extendAxes = itsExtendSpec.extendAxes();
  uInt nre = extendAxes.nelements();
  for (;;) {
    uInt i;
    for (i=0; i<nre; i++) {
      end(extendAxes(i)) = pos(extendAxes(i));
    }
    //# Set each section of the buffer to the data.
    buffer(pos,end) = data;
    //# Go to the next section.
    for (i=0; i<nre; i++) {
      if (++pos(extendAxes(i)) < length(extendAxes(i))) {
	break;
      }
      // This dimension is done. Reset it and continue with the next.
      pos(extendAxes(i)) = 0;
    }
    //# End the iteration when all dimensions are done.
    if (i == nre) {
      break;
    }
  }
  return False;
}


template <class T>
Bool ExtendLattice<T>::ok() const
{
  return itsLatticePtr->ok();
}

} //# NAMESPACE CASACORE - END


#endif
