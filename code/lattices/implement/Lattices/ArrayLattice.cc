//# ArrayLattice.cc: this defines the Lattice wrapper class for Arrays.
//# Copyright (C) 1995,1997
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

#include <trial/Lattices/ArrayLattice.h>
#include <trial/Lattices/ArrLatticeIter.h>
#include <trial/Lattices/LatticeIterInterface.h>
#include <trial/Lattices/LatticeNavigator.h>
#include <aips/Exceptions/Error.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Lattices/Slicer.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/COWPtr.h>

template<class T>
ArrayLattice<T>::ArrayLattice()
: itsWritable (False)
{}

template<class T>
ArrayLattice<T>::ArrayLattice (const IPosition& shape) 
: itsData     (shape),
  itsWritable (True)
{}

template<class T>
ArrayLattice<T>::ArrayLattice (Array<T>& array) 
: itsData     (array),
  itsWritable (True)
{}

template<class T>
ArrayLattice<T>::ArrayLattice (const Array<T>& array) 
: itsData     (array),
  itsWritable (False)
{}

template<class T>
ArrayLattice<T>::ArrayLattice (const ArrayLattice<T>&other) 
: itsData     (other.itsData),
  itsWritable (other.itsWritable)
{
    // Nothing
}

template<class T>
ArrayLattice<T>::~ArrayLattice()
{
  // Nothing
}

template<class T>
ArrayLattice<T>& ArrayLattice<T>::operator= (const ArrayLattice<T>& other)
{
  if (this != &other) {
    itsData     = other.itsData;
    itsWritable = other.itsWritable;
  }
  return *this;
}

template<class T>
Lattice<T>* ArrayLattice<T>::clone() const
{
  return new ArrayLattice<T> (*this);
}

template <class T>
Bool ArrayLattice<T>::isWritable() const
{
  return itsWritable;
}

template<class T>
IPosition ArrayLattice<T>::shape() const
{
  return itsData.shape();
} 

template<class T>
Bool ArrayLattice<T>::getSlice (COWPtr<Array<T> >& bufPtr,
				const IPosition& start, 
				const IPosition& shape,
				const IPosition& stride,
				Bool removeDegenerateAxes) const
{
  return getSlice (bufPtr, Slicer(start, shape, stride), removeDegenerateAxes);
}

template<class T>
Bool ArrayLattice<T>::getSlice (COWPtr<Array<T> >& bufPtr,
				const Slicer& section, 
				Bool removeDegenerateAxes) const
{
  // cast away the constness of the ArrayLattice using a pointer copy. This can
  // be done as the COWPtr will be set to be "readonly" and hence the
  // ArrayLattice cannot be modified without the COWPtr making a copy of the
  // cursor
  // The COWPtr takes over the pointer to the array.
  ArrayLattice<T>* This = (ArrayLattice<T>*) this;
  Array<T>* arr = new Array<T>;
  Bool isARef = This->getSlice(*arr, section, removeDegenerateAxes);
  bufPtr = COWPtr<Array<T> > (arr, True, isARef);
  // While the returned array is normally a reference return "False" indicating
  // a copy as any attempt to modify the Array will result in a copy.
  return False;
}

template<class T>
Bool ArrayLattice<T>::getSlice (Array<T>& buffer,
				const IPosition& start,
				const IPosition& shape, 
				const IPosition& stride,
				Bool removeDegenerateAxes)
{
  return getSlice (buffer, Slicer(start, shape, stride), removeDegenerateAxes);
}

template<class T>
Bool ArrayLattice<T>::getSlice (Array<T>& buffer,
				const Slicer& section, 
				Bool removeDegenerateAxes)
{
  // The following block checks that the supplied buffer is the right size or
  // empty. These restrictions are not required for the rest of this function
  // to work. I impose them because they are required by the corresponding
  // functions in the PagedArray class. If these restrictions are a performance
  // bottleneck they can by removed (only for optimised code please).
  if (buffer.nelements() != 0) {
    const IPosition& shape = buffer.shape();
    if (removeDegenerateAxes) {
      AlwaysAssert (shape.isEqual (section.length().nonDegenerate()),
		    AipsError);
    } else {
      AlwaysAssert (shape.isEqual (section.length()), AipsError);
    }
  }
  Array<T> cursor = itsData(section.start(), section.end(), section.stride());
  if (removeDegenerateAxes) {
    buffer.nonDegenerate(cursor);
  } else {
    buffer.reference(cursor);
  }
  return True;
}

template<class T>
void ArrayLattice<T>::putSlice (const Array<T>& sourceBuffer,
				const IPosition& where, 
				const IPosition& stride)
{
  if (!itsWritable) {
      throw (AipsError ("ArrayLattice::putSlice - non-writable lattice"));
  }
  const uInt sdim = sourceBuffer.ndim();
  const uInt ldim = ndim();
  DebugAssert(ldim == where.nelements(), AipsError);
  DebugAssert(ldim == stride.nelements(), AipsError);
  if (sdim == ldim) {
    itsData(where, 
	    where + (sourceBuffer.shape()-1)*stride, 
	    stride) = sourceBuffer;
  } else {
    Array<T> allAxes(sourceBuffer.addDegenerate(ldim-sdim));
    itsData(where, 
	    where + (allAxes.shape()-1)*stride, 
	    stride) = allAxes;
  }
}

template<class T>
void ArrayLattice<T>::putSlice (const Array<T>& sourceBuffer,
				const IPosition& where)
{
  if (!itsWritable) {
      throw (AipsError ("ArrayLattice::putSlice - non-writable lattice"));
  }
  const uInt sdim = sourceBuffer.ndim();
  const uInt ldim = ndim();
  DebugAssert(ldim == where.nelements(), AipsError);
  if (sdim == ldim) {
    itsData(where, 
	    where + (sourceBuffer.shape()-1)) = sourceBuffer;
  } else {
    Array<T> allAxes(sourceBuffer.addDegenerate(ldim-sdim));
    itsData(where, 
	    where + (allAxes.shape()-1)) = allAxes;
  }
}

template<class T>
void ArrayLattice<T>::getIterSlice (Array<T>& buffer, const IPosition& start,
				    const IPosition& end, const IPosition& incr)
{
    Array<T> tmp (itsData(start, end, incr));
    buffer.reference (tmp);
}


template<class T>
void ArrayLattice<T>::set (const T& value)
{
  if (!itsWritable) {
      throw (AipsError ("ArrayLattice::set - non-writable lattice"));
  }
  itsData.set(value);
}

template<class T>
T ArrayLattice<T>::getAt (const IPosition& where) const
{
  return itsData(where);
}

template<class T>
void ArrayLattice<T>::putAt (const T& value, const IPosition& where)
{
  if (!itsWritable) {
      throw (AipsError ("ArrayLattice::putAt - non-writable lattice"));
  }
  itsData(where) = value;
}

template<class T>
LatticeIterInterface<T>* ArrayLattice<T>::makeIter
                                       (const LatticeNavigator& nav) const
{
  return new ArrLatticeIter<T>(*this, nav);
}

template<class T>
Array<T>& ArrayLattice<T>::asArray()
{
  if (!itsWritable) {
      throw (AipsError ("ArrayLattice::asArray - non-writable lattice"));
  }
  return itsData;
}

template<class T>
const Array<T>& ArrayLattice<T>::asArray() const
{
  return itsData;
}

// Check class invariants. 
template<class T>
Bool ArrayLattice<T>::ok() const
{
  return itsData.ok();
}
