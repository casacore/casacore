//# MaskedLattice.cc: Abstract base class for array-like classes with masks
//# Copyright (C) 1998,1999,2000
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

#ifndef LATTICES_MASKEDLATTICE_TCC
#define LATTICES_MASKEDLATTICE_TCC


#include <casacore/lattices/Lattices/MaskedLattice.h>
#include <casacore/lattices/LRegions/LatticeRegion.h>
#include <casacore/lattices/LRegions/LCBox.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Utilities/COWPtr.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

template <class T>
MaskedLattice<T>::MaskedLattice (const MaskedLattice<T>& that)
: Lattice<T>(),
  itsDefRegPtr (0)
{
  if (that.itsDefRegPtr != 0) {
    itsDefRegPtr = new LatticeRegion (*that.itsDefRegPtr);
  }
}

template <class T>
MaskedLattice<T>::~MaskedLattice()
{
  delete itsDefRegPtr;
}

template <class T>
MaskedLattice<T>& MaskedLattice<T>::operator= (const MaskedLattice<T>& that)
{
  if (this != &that) {
    delete itsDefRegPtr;
    itsDefRegPtr = 0;
    if (that.itsDefRegPtr != 0) {
      itsDefRegPtr = new LatticeRegion (*that.itsDefRegPtr);
    }
  }
  return *this;
}

template<class T>
Lattice<T>* MaskedLattice<T>::clone() const
{
    return cloneML();
}

template<class T>
Bool MaskedLattice<T>::isMasked() const
{
  const LatticeRegion* ptr = getRegionPtr();
  if (ptr == 0) {
    return False;
  }
  return ptr->hasMask();
}


template<class T>
Bool MaskedLattice<T>::hasPixelMask() const
{
  return False;
}

template<class T>
const Lattice<Bool>& MaskedLattice<T>::pixelMask() const
{
  throw (AipsError ("MaskedLattice::pixelMask - no pixelmask available"));
  //# Make the compiler happy.
  return *itsDefRegPtr;
}
template<class T>
Lattice<Bool>& MaskedLattice<T>::pixelMask()
{
  throw (AipsError ("MaskedLattice::pixelMask - no pixelmask available"));
  //# Make the compiler happy.
  return *itsDefRegPtr;
}

template<class T>
const LatticeRegion& MaskedLattice<T>::region() const
{
  // If there is a region, return it.
  const LatticeRegion* ptr = getRegionPtr();
  if (ptr != 0) {
    return *ptr;
  }
  // No region, so use the one in the MaskedLattice itself which
  // describes the entire lattice. Create it if it does not exist yet.
  // Check if its shape still matches.
  if (itsDefRegPtr != 0) {
    if (itsDefRegPtr->slicer().length().isEqual (shape())) {
      return *itsDefRegPtr;
    }
    delete itsDefRegPtr;
    itsDefRegPtr = 0;
  }
  itsDefRegPtr = new LatticeRegion (LCBox(shape()));
  return *itsDefRegPtr;
}


template<class T>
Bool MaskedLattice<T>::getMask (COWPtr<Array<Bool> >& buffer,
				Bool removeDegenerateAxes) const
{
  uInt nd = ndim();
  return getMaskSlice (buffer, Slicer(IPosition(nd,0), shape()),
		       removeDegenerateAxes);
}

template<class T>
Bool MaskedLattice<T>::getMask (Array<Bool>& buffer,
				Bool removeDegenerateAxes)
{
  uInt nd = ndim();
  return getMaskSlice (buffer, Slicer(IPosition(nd,0), shape()),
		       removeDegenerateAxes);
}

template<class T>
Array<Bool> MaskedLattice<T>::getMask (Bool removeDegenerateAxes) const
{
  uInt nd = ndim();
  return getMaskSlice (Slicer(IPosition(nd,0), shape()),
		       removeDegenerateAxes);
}

template<class T>
Bool MaskedLattice<T>::getMaskSlice (COWPtr<Array<Bool> >& buffer,
				     const IPosition& start, 
				     const IPosition& shape,
				     Bool removeDegenerateAxes) const
{
  return getMaskSlice (buffer, Slicer(start, shape),
		       removeDegenerateAxes);
}

template<class T>
Bool MaskedLattice<T>::getMaskSlice (COWPtr<Array<Bool> >& buffer,
				     const IPosition& start, 
				     const IPosition& shape,
				     const IPosition& stride,
				     Bool removeDegenerateAxes) const
{
  return getMaskSlice (buffer, Slicer(start, shape, stride),
		       removeDegenerateAxes);
}

template<class T>
Bool MaskedLattice<T>::getMaskSlice (COWPtr<Array<Bool> >& buffer,
				     const Slicer& section,
				     Bool removeDegenerateAxes) const
{
  // Cast pointer to non-const.
  // This is safe, since the array is copied when needed by COWptr.
  MaskedLattice<T>* This = (MaskedLattice<T>*)this;
  // The COWPtr takes over the pointer to the array.
  Array<Bool>* arr = new Array<Bool>;
  Bool isARef = This->getMaskSlice (*arr, section, removeDegenerateAxes);
  buffer = COWPtr<Array<Bool> > (arr, True, isARef);
  return False;
}

template<class T>
Bool MaskedLattice<T>::getMaskSlice (Array<Bool>& buffer,
				     const IPosition& start,
				     const IPosition& shape,
				     Bool removeDegenerateAxes)
{
  return getMaskSlice (buffer, Slicer(start, shape), removeDegenerateAxes);
}

template<class T>
Bool MaskedLattice<T>::getMaskSlice (Array<Bool>& buffer,
				     const IPosition& start,
				     const IPosition& shape,
				     const IPosition& stride,
				     Bool removeDegenerateAxes)
{
  return getMaskSlice (buffer, Slicer(start, shape, stride),
		       removeDegenerateAxes);
}

template<class T>
Bool MaskedLattice<T>::getMaskSlice (Array<Bool>& buffer,
				     const Slicer& section,
				     Bool removeDegenerateAxes)
{
  Bool isARef;
  // When the slicer is fixed, it can be used immediately.
  // Otherwise unspecified values are to be filled in.
  if (section.isFixed()) {
    isARef = doGetMaskSlice (buffer, section);
  } else {
    IPosition blc,trc,inc;
    section.inferShapeFromSource (shape(), blc, trc, inc);
    isARef = doGetMaskSlice (buffer, Slicer(blc,trc,inc,Slicer::endIsLast));
  }
  if (removeDegenerateAxes) {
    Array<Bool> tmp = buffer.nonDegenerate();
    buffer.reference (tmp);
  }
  return isARef;
}

template<class T>
Array<Bool> MaskedLattice<T>::getMaskSlice (const IPosition& start,
					    const IPosition& shape,
					    Bool removeDegenerateAxes) const
{
  return getMaskSlice (Slicer(start,shape), removeDegenerateAxes);
}

template<class T>
Array<Bool> MaskedLattice<T>::getMaskSlice (const IPosition& start,
					    const IPosition& shape,
					    const IPosition& stride,
					    Bool removeDegenerateAxes) const
{
  return getMaskSlice (Slicer(start,shape,stride), removeDegenerateAxes);
}

template<class T>
Array<Bool> MaskedLattice<T>::getMaskSlice (const Slicer& section,
					    Bool removeDegenerateAxes) const
{
  // Cast pointer to non-const.
  // This is safe, since the array is copied when needed.
  MaskedLattice<T>* This = (MaskedLattice<T>*)this;
  // Note that getMaskSlice is used to be sure that section getMasks filled
  // when needed.
  Array<Bool> arr;
  Bool isARef = This->getMaskSlice (arr, section, removeDegenerateAxes);
  // When not referenced, return it as such.
  // Otherwise make a copy.
  if (!isARef) {
    return arr;
  }
  Array<Bool> tmp;
  tmp = arr;
  return tmp;
}


template<class T>
Bool MaskedLattice<T>::doGetMaskSlice (Array<Bool>& buffer,
				       const Slicer& section)
{
  // Note that Slicer::inferShapeFromSource has already been called
  // by getMaskSlice.
  const LatticeRegion* ptr = getRegionPtr();
  if (ptr == 0) {
    buffer.resize (section.length());
    buffer = True;
    return False;
  }
  return const_cast<LatticeRegion*>(ptr)->doGetSlice (buffer, section);
}

} //# NAMESPACE CASACORE - END


#endif
