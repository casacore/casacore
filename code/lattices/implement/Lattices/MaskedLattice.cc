//# MaskedLattice.cc: Abstract base class for array-like classes with masks
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


#include <trial/Lattices/MaskedLattice.h>
#include <aips/Arrays/Array.h>
#include <aips/Lattices/Slicer.h>
#include <aips/Utilities/COWPtr.h>


typedef Array<Bool> gppbug1_maskedlattice;
typedef COWPtr<Array<Bool> > gppbug2_maskedlattice;


template <class T>
MaskedLattice<T>::~MaskedLattice()
{}


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
Bool MaskedLattice<T>::getMaskSlice (Array<Bool>& buffer, const Slicer& section,
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
